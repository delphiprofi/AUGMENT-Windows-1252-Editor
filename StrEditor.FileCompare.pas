Unit StrEditor.FileCompare;

interface

Uses
  System.SysUtils
, System.Classes
, System.Math
, System.Generics.Collections
, System.IOUtils
, StrEditor.Encoding
;

Type
  TCompareExitCode = (
    ceOK               = 0,
    ceEncodingMismatch = 1,
    ceCharsBroken      = 2,
    ceLineNotFound     = 3,
    ceBOMDifference    = 4
  );

  TLineCharInfo = record
    MasterLineIndex : Integer;
    Fragments       : TArray<string>;   // Textfragmente zwischen Sonderzeichen
    SpecialChars    : TArray<Char>;     // Sonderzeichen zwischen den Fragmenten
  end;

  TCompareResult = record
    ExitCode      : TCompareExitCode;
    Message       : string;
    Details       : TStringList;
    TotalChecked  : Integer;
    BrokenCount   : Integer;
    NotFoundCount : Integer;
  end;

  TFileCompare = class
    public
      class function Compare( const aFile1    : string;
                              const aFile2    : string;
                              const aVerbose  : Boolean ) : TCompareResult;
    strict private
      const SPECIAL_CHARS : array[ 0..10 ] of Char = ( #$00F6, #$00E4, #$00FC, #$00D6, #$00C4, #$00DC, #$00DF, #$00E9, #$00A7, #$2022, #$20AC );
      const MIN_FRAGMENT_LEN = 3;

      class function IsSpecialChar( aChar : Char ) : Boolean;
      class function SplitLineInfo( const aLine : string; aLineIndex : Integer ) : TLineCharInfo;
      class function ExtractLineInfos( const aLines : TStringList ) : TList<TLineCharInfo>;
      class function HasUsableFragments( const aInfo : TLineCharInfo ) : Boolean;
      class function LineContainsSpecialChar( const aLine : string ) : Boolean;
      class function FindMatchingLine( const aLines       : TStringList;
                                       const aInfo        : TLineCharInfo;
                                       out   aFoundLine   : string;
                                       out   aFoundIndex  : Integer ) : Boolean;
      class function FindMatchingLineByContext( const aLines       : TStringList;
                                                const aMasterLines : TStringList;
                                                const aInfo        : TLineCharInfo;
                                                out   aFoundLine   : string;
                                                out   aFoundIndex  : Integer ) : Boolean;
      class function CheckSpecialChars( const aFoundLine : string;
                                        const aInfo      : TLineCharInfo;
                                        out   aBrokenChars : string ) : Boolean;
      class function LineSimilarity( const aLine1 : string; const aLine2 : string ) : Double;
      class function StripSpecialChars( const aLine : string ) : string;
      class function BuildLineMapping( const aMasterLines : TStringList;
                                       const aFileLines   : TStringList;
                                       const aVerbose     : Boolean ) : TDictionary<Integer, Integer>;
      class procedure RunCheckPass( const aLines1      : TStringList;
                                    const aLines2      : TStringList;
                                    const aInfos       : TList<TLineCharInfo>;
                                    const aVerbose     : Boolean;
                                    var   aResult      : TCompareResult );
  end;

implementation

function EncodingToStr( aEncoding : TEncodingType ) : string;
begin
  case aEncoding of
    etUTF8        : Result := 'UTF-8';
    etWindows1252 : Result := 'Windows-1252';
    else            Result := 'Unknown';
  end; // of case
end;

{ TFileCompare }

class function TFileCompare.IsSpecialChar( aChar : Char ) : Boolean;
Var
  lSpc : Char;
begin
  Result := false;

  for lSpc in SPECIAL_CHARS do
    if aChar = lSpc then
      begin
        Result := true;
        Exit;
      end;
end;

class function TFileCompare.SplitLineInfo( const aLine : string; aLineIndex : Integer ) : TLineCharInfo;
Var
  lFragments : TList<string>;
  lChars     : TList<Char>;
  lStart     : Integer;
  i          : Integer;
begin
  lFragments := TList<string>.Create;
  lChars     := TList<Char>.Create;

  try
    lStart := 1;

    for i := 1 to Length( aLine ) do
      begin
        if IsSpecialChar( aLine[ i ] ) then
          begin
            lFragments.Add( Copy( aLine, lStart, i - lStart ) );
            lChars.Add( aLine[ i ] );
            lStart := i + 1;
          end;
      end;

    lFragments.Add( Copy( aLine, lStart, Length( aLine ) - lStart + 1 ) );

    Result.MasterLineIndex := aLineIndex;
    Result.Fragments       := lFragments.ToArray;
    Result.SpecialChars    := lChars.ToArray;
  finally
    lChars.Free;
    lFragments.Free;
  end;
end;

class function TFileCompare.ExtractLineInfos( const aLines : TStringList ) : TList<TLineCharInfo>;
Var
  i    : Integer;
  lLine : string;
  lInfo : TLineCharInfo;
begin
  Result := TList<TLineCharInfo>.Create;

  for i := 0 to aLines.Count - 1 do
    begin
      lLine := aLines[ i ];
      lInfo := SplitLineInfo( lLine, i );

      if Length( lInfo.SpecialChars ) > 0 then
        Result.Add( lInfo );
    end;
end;

class function TFileCompare.HasUsableFragments( const aInfo : TLineCharInfo ) : Boolean;
Var
  k : Integer;
begin
  Result := false;

  for k := 0 to Length( aInfo.Fragments ) - 1 do
    if Length( Trim( aInfo.Fragments[ k ] ) ) >= MIN_FRAGMENT_LEN then
      begin
        Result := true;
        Exit;
      end;
end;

class function TFileCompare.FindMatchingLine( const aLines     : TStringList;
                                               const aInfo      : TLineCharInfo;
                                               out   aFoundLine  : string;
                                               out   aFoundIndex : Integer ) : Boolean;

  function FragmentsMatchLine( const aLine : string ) : Boolean;
  Var
    k            : Integer;
    lPos         : Integer;
    lSearchStart : Integer;
    lFrag        : string;
    lUsableCount : Integer;
  begin
    Result       := false;
    lSearchStart := 1;
    lUsableCount := 0;

    for k := 0 to Length( aInfo.Fragments ) - 1 do
      begin
        lFrag := aInfo.Fragments[ k ];

        if Length( Trim( lFrag ) ) < MIN_FRAGMENT_LEN then
          Continue;

        Inc( lUsableCount );
        lPos := Pos( lFrag, Copy( aLine, lSearchStart, Length( aLine ) - lSearchStart + 1 ) );

        if lPos <= 0 then
          Exit;

        lSearchStart := lSearchStart + lPos - 1 + Length( lFrag );
      end;

    Result := lUsableCount > 0;
  end;

Var
  i : Integer;
begin
  Result      := false;
  aFoundLine  := '';
  aFoundIndex := -1;

  // Priorität 1: Gleiche Zeilennummer prüfen (Dateien sind oft identisch/ähnlich)
  if ( aInfo.MasterLineIndex >= 0 ) and ( aInfo.MasterLineIndex < aLines.Count ) then
    if FragmentsMatchLine( aLines[ aInfo.MasterLineIndex ] ) then
      begin
        aFoundLine  := aLines[ aInfo.MasterLineIndex ];
        aFoundIndex := aInfo.MasterLineIndex;
        Result      := true;
        Exit;
      end;

  // Priorität 2: Alle anderen Zeilen durchsuchen
  for i := 0 to aLines.Count - 1 do
    begin
      if i = aInfo.MasterLineIndex then
        Continue;

      if FragmentsMatchLine( aLines[ i ] ) then
        begin
          aFoundLine  := aLines[ i ];
          aFoundIndex := i;
          Result      := true;
          Exit;
        end;
    end;
end;

class function TFileCompare.LineContainsSpecialChar( const aLine : string ) : Boolean;
Var
  i : Integer;
begin
  Result := false;

  for i := 1 to Length( aLine ) do
    if IsSpecialChar( aLine[ i ] ) then
      begin
        Result := true;
        Exit;
      end;
end;

class function TFileCompare.FindMatchingLineByContext( const aLines       : TStringList;
                                                        const aMasterLines : TStringList;
                                                        const aInfo        : TLineCharInfo;
                                                        out   aFoundLine   : string;
                                                        out   aFoundIndex  : Integer ) : Boolean;

  // Suche eine reine ASCII-Zeile (TrimLeft) in aLines per exaktem Vergleich
  // Sucht zuerst in der Nähe des erwarteten Index (±1..±5), dann alle anderen
  function FindContextLineInFile( const aContextLine : string;
                                  const aExpectedIdx : Integer;
                                  out   aFoundIdx    : Integer ) : Boolean;
  Var
    lTrimmedCtx : string;
    lDelta      : Integer;
    lCheckIdx   : Integer;
    k           : Integer;
  begin
    Result      := false;
    aFoundIdx   := -1;
    lTrimmedCtx := TrimLeft( aContextLine );

    if lTrimmedCtx = '' then
      Exit;

    // Priorität 1: Erwartete Zeilennummer und Nachbarn prüfen (±0..±5)
    for lDelta := 0 to 5 do
      begin
        // Exakt die erwartete Position
        lCheckIdx := aExpectedIdx - lDelta;

        if ( lCheckIdx >= 0 ) and ( lCheckIdx < aLines.Count ) then
          if TrimLeft( aLines[ lCheckIdx ] ) = lTrimmedCtx then
            begin
              aFoundIdx := lCheckIdx;
              Result    := true;
              Exit;
            end;

        // Nur bei Delta > 0 auch nach oben suchen
        if lDelta > 0 then
          begin
            lCheckIdx := aExpectedIdx + lDelta;

            if ( lCheckIdx >= 0 ) and ( lCheckIdx < aLines.Count ) then
              if TrimLeft( aLines[ lCheckIdx ] ) = lTrimmedCtx then
                begin
                  aFoundIdx := lCheckIdx;
                  Result    := true;
                  Exit;
                end;
          end;
      end;

    // Priorität 2: Alle restlichen Zeilen durchsuchen
    for k := 0 to aLines.Count - 1 do
      begin
        if ( k >= aExpectedIdx - 5 ) and ( k <= aExpectedIdx + 5 ) then
          Continue;

        if TrimLeft( aLines[ k ] ) = lTrimmedCtx then
          begin
            aFoundIdx := k;
            Result    := true;
            Exit;
          end;
      end;
  end;

Var
  lContextOffset : Integer;
  lContextIdx    : Integer;
  lContextLine   : string;
  lFoundCtxIdx   : Integer;
  lTargetIdx     : Integer;
begin
  Result      := false;
  aFoundLine  := '';
  aFoundIndex := -1;

  // Suche in Nachbarzeilen (1-3 Zeilen Abstand) nach einer reinen ASCII-Zeile als Anker
  for lContextOffset := 1 to 3 do
    begin
      // Zeile DAVOR prüfen
      lContextIdx := aInfo.MasterLineIndex - lContextOffset;

      if ( lContextIdx >= 0 ) and ( lContextIdx < aMasterLines.Count ) then
        begin
          lContextLine := aMasterLines[ lContextIdx ];

          // Nur Zeilen OHNE Sonderzeichen als Anker verwenden
          if not LineContainsSpecialChar( lContextLine ) then
            if FindContextLineInFile( lContextLine, lContextIdx, lFoundCtxIdx ) then
              begin
                lTargetIdx := lFoundCtxIdx + lContextOffset;

                if ( lTargetIdx >= 0 ) and ( lTargetIdx < aLines.Count ) then
                  begin
                    aFoundLine  := aLines[ lTargetIdx ];
                    aFoundIndex := lTargetIdx;
                    Result      := true;
                    Exit;
                  end;
              end;
        end;

      // Zeile DANACH prüfen
      lContextIdx := aInfo.MasterLineIndex + lContextOffset;

      if ( lContextIdx >= 0 ) and ( lContextIdx < aMasterLines.Count ) then
        begin
          lContextLine := aMasterLines[ lContextIdx ];

          // Nur Zeilen OHNE Sonderzeichen als Anker verwenden
          if not LineContainsSpecialChar( lContextLine ) then
            if FindContextLineInFile( lContextLine, lContextIdx, lFoundCtxIdx ) then
              begin
                lTargetIdx := lFoundCtxIdx - lContextOffset;

                if ( lTargetIdx >= 0 ) and ( lTargetIdx < aLines.Count ) then
                  begin
                    aFoundLine  := aLines[ lTargetIdx ];
                    aFoundIndex := lTargetIdx;
                    Result      := true;
                    Exit;
                  end;
              end;
        end;
    end;
end;

class function TFileCompare.StripSpecialChars( const aLine : string ) : string;
Var
  i  : Integer;
  lSB : TStringBuilder;
begin
  lSB := TStringBuilder.Create( Length( aLine ) );

  try
    for i := 1 to Length( aLine ) do
      if not IsSpecialChar( aLine[ i ] ) then
        lSB.Append( aLine[ i ] );

    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

class function TFileCompare.LineSimilarity( const aLine1 : string; const aLine2 : string ) : Double;
Var
  lLen1    : Integer;
  lLen2    : Integer;
  lMaxLen  : Integer;
  lMatches : Integer;
  i        : Integer;
  lMinLen  : Integer;
begin
  lLen1 := Length( aLine1 );
  lLen2 := Length( aLine2 );

  if ( lLen1 = 0 ) and ( lLen2 = 0 ) then
    begin
      Result := 1.0;
      Exit;
    end;

  lMaxLen := Max( lLen1, lLen2 );

  if lMaxLen = 0 then
    begin
      Result := 0.0;
      Exit;
    end;

  // Schneller Vorab-Check: Wenn Längenunterschied > 50% → nicht ähnlich
  if Min( lLen1, lLen2 ) < ( lMaxLen div 2 ) then
    begin
      Result := 0.0;
      Exit;
    end;

  // Zeichenweiser Vergleich von links
  lMatches := 0;
  lMinLen  := Min( lLen1, lLen2 );

  for i := 1 to lMinLen do
    if aLine1[ i ] = aLine2[ i ] then
      Inc( lMatches );

  Result := lMatches / lMaxLen;
end;

class function TFileCompare.BuildLineMapping( const aMasterLines : TStringList;
                                               const aFileLines   : TStringList;
                                               const aVerbose     : Boolean ) : TDictionary<Integer, Integer>;
Var
  lMapping     : TDictionary<Integer, Integer>;
  lMasterIdx   : Integer;
  lFileIdx     : Integer;
  lMasterLine  : string;
  lFileLine    : string;
  lMasterStrip : string;
  lFileStrip   : string;
  lSim         : Double;
  lLookAhead   : Integer;
  lFoundAhead  : Boolean;
  lAheadSim    : Double;
  lMaxLook     : Integer;
begin
  lMapping := TDictionary<Integer, Integer>.Create;

  lMasterIdx := 0;
  lFileIdx   := 0;
  lMaxLook   := 10;

  while ( lMasterIdx < aMasterLines.Count ) and ( lFileIdx < aFileLines.Count ) do
    begin
      lMasterLine := aMasterLines[ lMasterIdx ];
      lFileLine   := aFileLines[ lFileIdx ];

      // Zeilen mit Sonderzeichen — ASCII-Fragmente vergleichen
      if LineContainsSpecialChar( lMasterLine ) then
        begin
          if LineContainsSpecialChar( lFileLine ) then
            begin
              // Beide haben Sonderzeichen → gestrippte Versionen vergleichen
              lMasterStrip := TrimLeft( StripSpecialChars( lMasterLine ) );
              lFileStrip   := TrimLeft( StripSpecialChars( lFileLine ) );

              if ( lMasterStrip = lFileStrip ) or
                 ( ( Length( lMasterStrip ) > 0 ) and ( LineSimilarity( lMasterStrip, lFileStrip ) >= 0.6 ) ) then
                begin
                  // Passt zusammen
                  lMapping.AddOrSetValue( lMasterIdx, lFileIdx );
                  Inc( lMasterIdx );
                  Inc( lFileIdx );
                  Continue;
                end;

              // Nicht ähnlich → Lookahead in File nach passender SC-Zeile
              lFoundAhead := false;

              for lLookAhead := 1 to Min( lMaxLook, aFileLines.Count - lFileIdx - 1 ) do
                begin
                  if not LineContainsSpecialChar( aFileLines[ lFileIdx + lLookAhead ] ) then
                    Continue;

                  lFileStrip := TrimLeft( StripSpecialChars( aFileLines[ lFileIdx + lLookAhead ] ) );

                  if ( lMasterStrip = lFileStrip ) or
                     ( ( Length( lMasterStrip ) > 0 ) and ( LineSimilarity( lMasterStrip, lFileStrip ) >= 0.6 ) ) then
                    begin
                      lFileIdx    := lFileIdx + lLookAhead;
                      lFoundAhead := true;
                      Break;
                    end;
                end;

              if lFoundAhead then
                begin
                  lMapping.AddOrSetValue( lMasterIdx, lFileIdx );
                  Inc( lMasterIdx );
                  Inc( lFileIdx );
                  Continue;
                end;

              // Lookahead in Master nach passender SC-Zeile
              lFileStrip := TrimLeft( StripSpecialChars( lFileLine ) );

              for lLookAhead := 1 to Min( lMaxLook, aMasterLines.Count - lMasterIdx - 1 ) do
                begin
                  if not LineContainsSpecialChar( aMasterLines[ lMasterIdx + lLookAhead ] ) then
                    Continue;

                  lMasterStrip := TrimLeft( StripSpecialChars( aMasterLines[ lMasterIdx + lLookAhead ] ) );

                  if ( lFileStrip = lMasterStrip ) or
                     ( ( Length( lFileStrip ) > 0 ) and ( LineSimilarity( lFileStrip, lMasterStrip ) >= 0.6 ) ) then
                    begin
                      lMasterIdx  := lMasterIdx + lLookAhead;
                      lFoundAhead := true;
                      Break;
                    end;
                end;

              if lFoundAhead then
                begin
                  lMapping.AddOrSetValue( lMasterIdx, lFileIdx );
                  Inc( lMasterIdx );
                  Inc( lFileIdx );
                  Continue;
                end;

              // Keine Übereinstimmung → blind mappen und weiter
              lMapping.AddOrSetValue( lMasterIdx, lFileIdx );
              Inc( lMasterIdx );
              Inc( lFileIdx );
              Continue;
            end
          else begin
                 // Master hat SC, File nicht → Master hat Extra-SC-Zeile, File hat sie nicht
                 // Lookahead in File nach einer SC-Zeile die passt
                 lMasterStrip := TrimLeft( StripSpecialChars( lMasterLine ) );
                 lFoundAhead  := false;

                 for lLookAhead := 1 to Min( lMaxLook, aFileLines.Count - lFileIdx - 1 ) do
                   begin
                     if not LineContainsSpecialChar( aFileLines[ lFileIdx + lLookAhead ] ) then
                       Continue;

                     lFileStrip := TrimLeft( StripSpecialChars( aFileLines[ lFileIdx + lLookAhead ] ) );

                     if ( lMasterStrip = lFileStrip ) or
                        ( ( Length( lMasterStrip ) > 0 ) and ( LineSimilarity( lMasterStrip, lFileStrip ) >= 0.6 ) ) then
                       begin
                         lFileIdx    := lFileIdx + lLookAhead;
                         lFoundAhead := true;
                         Break;
                       end;
                   end;

                 if lFoundAhead then
                   begin
                     lMapping.AddOrSetValue( lMasterIdx, lFileIdx );
                     Inc( lMasterIdx );
                     Inc( lFileIdx );
                     Continue;
                   end;

                 // Nicht gefunden → Master-Zeile existiert nicht in File, weiter
                 Inc( lMasterIdx );
                 Continue;
               end;
        end;

      if LineContainsSpecialChar( lFileLine ) then
        begin
          // File hat Sonderzeichen, Master nicht → File hat Extra-Zeile
          Inc( lFileIdx );
          Continue;
        end;

      // Beide Zeilen sind sonderzeichenfrei → vergleichen
      lMasterStrip := TrimLeft( lMasterLine );
      lFileStrip   := TrimLeft( lFileLine );

      // Exakt gleich?
      if lMasterStrip = lFileStrip then
        begin
          lMapping.AddOrSetValue( lMasterIdx, lFileIdx );
          Inc( lMasterIdx );
          Inc( lFileIdx );
          Continue;
        end;

      // Ähnlichkeit prüfen
      lSim := LineSimilarity( lMasterStrip, lFileStrip );

      if lSim >= 0.6 then
        begin
          // Geänderte Zeile — gleiche Position
          lMapping.AddOrSetValue( lMasterIdx, lFileIdx );
          Inc( lMasterIdx );
          Inc( lFileIdx );
          Continue;
        end;

      // Nicht ähnlich → eingefügte oder gelöschte Zeile
      // Lookahead: Prüfe ob Master-Zeile weiter vorne in File vorkommt
      lFoundAhead := false;

      for lLookAhead := 1 to Min( lMaxLook, aFileLines.Count - lFileIdx - 1 ) do
        begin
          lFileStrip := TrimLeft( aFileLines[ lFileIdx + lLookAhead ] );

          if LineContainsSpecialChar( aFileLines[ lFileIdx + lLookAhead ] ) then
            Continue;

          lAheadSim := LineSimilarity( lMasterStrip, lFileStrip );

          if ( lMasterStrip = lFileStrip ) or ( lAheadSim >= 0.6 ) then
            begin
              // File hat eingefügte Zeilen → File-Cursor vorspringen
              lFileIdx    := lFileIdx + lLookAhead;
              lFoundAhead := true;
              Break;
            end;
        end;

      if lFoundAhead then
        Continue; // Nochmal von vorne mit neuer Position

      // Lookahead: Prüfe ob File-Zeile weiter vorne in Master vorkommt
      lFileStrip := TrimLeft( lFileLine );

      for lLookAhead := 1 to Min( lMaxLook, aMasterLines.Count - lMasterIdx - 1 ) do
        begin
          lMasterStrip := TrimLeft( aMasterLines[ lMasterIdx + lLookAhead ] );

          if LineContainsSpecialChar( aMasterLines[ lMasterIdx + lLookAhead ] ) then
            Continue;

          lAheadSim := LineSimilarity( lFileStrip, lMasterStrip );

          if ( lFileStrip = lMasterStrip ) or ( lAheadSim >= 0.6 ) then
            begin
              // Master hat gelöschte Zeilen → Master-Cursor vorspringen
              lMasterIdx  := lMasterIdx + lLookAhead;
              lFoundAhead := true;
              Break;
            end;
        end;

      if lFoundAhead then
        Continue; // Nochmal von vorne mit neuer Position

      // Keine Übereinstimmung gefunden → beide weiter
      lMapping.AddOrSetValue( lMasterIdx, lFileIdx );
      Inc( lMasterIdx );
      Inc( lFileIdx );
    end;

  // Restliche Master-Zeilen ohne Mapping
  // (File ist kürzer als Master — passiert bei gelöschten Zeilen am Ende)

  if aVerbose then
    begin
      WriteLn( Format( '  Pass 2: Line mapping created with %d entries (Master: %d, File: %d lines)',
        [ lMapping.Count, aMasterLines.Count, aFileLines.Count ] ) );

      // Debug: Zeige Mapping für Zeilen 745-760
      Var lDebugIdx : Integer;

      for lDebugIdx := 744 to Min( 764, aMasterLines.Count - 1 ) do
        begin
          Var lMappedTo : Integer;

          if lMapping.TryGetValue( lDebugIdx, lMappedTo ) then
            WriteLn( Format( '  DEBUG MAP: Master[%d] -> File[%d]  M="%s"  F="%s"',
              [ lDebugIdx + 1, lMappedTo + 1,
                Copy( aMasterLines[ lDebugIdx ], 1, 60 ),
                Copy( aFileLines[ Min( lMappedTo, aFileLines.Count - 1 ) ], 1, 60 ) ] ) )
          else
            WriteLn( Format( '  DEBUG MAP: Master[%d] -> NOT MAPPED  M="%s"',
              [ lDebugIdx + 1, Copy( aMasterLines[ lDebugIdx ], 1, 60 ) ] ) );
        end;
    end;

  Result := lMapping;
end;

class function TFileCompare.CheckSpecialChars( const aFoundLine : string;
                                                const aInfo      : TLineCharInfo;
                                                out   aBrokenChars : string ) : Boolean;
Var
  k            : Integer;
  lPos         : Integer;
  lSearchStart : Integer;
  lFrag        : string;
  lCharPos     : Integer;
begin
  Result      := true;
  aBrokenChars := '';
  lSearchStart := 1;

  for k := 0 to Length( aInfo.Fragments ) - 1 do
    begin
      lFrag := aInfo.Fragments[ k ];

      if Length( lFrag ) = 0 then
        begin
          // Leeres Fragment = Sonderzeichen am Anfang oder aufeinanderfolgend
          // Prüfe ob das Sonderzeichen an der aktuellen Position steht
          if k < Length( aInfo.SpecialChars ) then
            begin
              lCharPos := lSearchStart;

              if ( lCharPos >= 1 ) and ( lCharPos <= Length( aFoundLine ) ) then
                begin
                  if aFoundLine[ lCharPos ] <> aInfo.SpecialChars[ k ] then
                    begin
                      Result := false;
                      aBrokenChars := aBrokenChars + aInfo.SpecialChars[ k ];
                    end;

                  Inc( lSearchStart );
                end
              else begin
                     Result := false;
                     aBrokenChars := aBrokenChars + aInfo.SpecialChars[ k ];
                   end;
            end;

          Continue;
        end;

      lPos := Pos( lFrag, Copy( aFoundLine, lSearchStart, Length( aFoundLine ) - lSearchStart + 1 ) );

      if lPos > 0 then
        begin
          lSearchStart := lSearchStart + lPos - 1 + Length( lFrag );

          // Nach diesem Fragment kommt ein Sonderzeichen (wenn nicht letztes Fragment)
          if k < Length( aInfo.SpecialChars ) then
            begin
              if ( lSearchStart >= 1 ) and ( lSearchStart <= Length( aFoundLine ) ) then
                begin
                  if aFoundLine[ lSearchStart ] <> aInfo.SpecialChars[ k ] then
                    begin
                      Result := false;
                      aBrokenChars := aBrokenChars + aInfo.SpecialChars[ k ];
                    end;

                  Inc( lSearchStart );
                end
              else begin
                     Result := false;
                     aBrokenChars := aBrokenChars + aInfo.SpecialChars[ k ];
                   end;
            end;
        end;
    end;
end;

class procedure TFileCompare.RunCheckPass( const aLines1  : TStringList;
                                           const aLines2  : TStringList;
                                           const aInfos   : TList<TLineCharInfo>;
                                           const aVerbose : Boolean;
                                           var   aResult  : TCompareResult );
Var
  lInfo        : TLineCharInfo;
  lFoundLine   : string;
  lFoundIndex  : Integer;
  lBrokenChars : string;
  i            : Integer;
begin
  aResult.BrokenCount   := 0;
  aResult.NotFoundCount := 0;
  aResult.Details.Clear;

  for i := 0 to aInfos.Count - 1 do
    begin
      lInfo := aInfos[ i ];

      Var lFound := false;

      // Strategie 1: Direkte Fragment-Suche (nur wenn brauchbare Fragmente vorhanden)
      if HasUsableFragments( lInfo ) then
        lFound := FindMatchingLine( aLines1, lInfo, lFoundLine, lFoundIndex );

      // Strategie 2: Kontext-basierte Suche über Nachbarzeilen
      if not lFound then
        lFound := FindMatchingLineByContext( aLines1, aLines2, lInfo, lFoundLine, lFoundIndex );

      if lFound then
        begin
          // Zeile gefunden - prüfe alle Sonderzeichen
          if CheckSpecialChars( lFoundLine, lInfo, lBrokenChars ) then
            begin
              if aVerbose then
                aResult.Details.Add( Format( '  OK   Line %d: %d special chars OK',
                  [ lInfo.MasterLineIndex + 1, Length( lInfo.SpecialChars ) ] ) );
            end
          else begin
                 Inc( aResult.BrokenCount, Length( lBrokenChars ) );
                 aResult.Details.Add( Format( '  BROKEN Line %d: expected chars ''%s'' are corrupted',
                   [ lInfo.MasterLineIndex + 1, lBrokenChars ] ) );

                 if aVerbose then
                   aResult.Details.Add( Format( '         Found in file1 line %d: "%s"',
                     [ lFoundIndex + 1, Copy( lFoundLine, 1, 80 ) ] ) );
               end;
        end
      else begin
             Inc( aResult.NotFoundCount, Length( lInfo.SpecialChars ) );
             aResult.Details.Add( Format( '  NOT FOUND Line %d: %d special chars (%d fragments)',
               [ lInfo.MasterLineIndex + 1, Length( lInfo.SpecialChars ), Length( lInfo.Fragments ) ] ) );
           end;
    end;
end;

class function TFileCompare.Compare( const aFile1   : string;
                                     const aFile2   : string;
                                     const aVerbose : Boolean ) : TCompareResult;
Var
  lEncoding1 : TEncodingType;
  lEncoding2 : TEncodingType;
  lLines1    : TStringList;
  lLines2    : TStringList;
  lInfos     : TList<TLineCharInfo>;
  lCharCount : Integer;
  lLineCount : Integer;
  i          : Integer;
begin
  Result.ExitCode      := ceOK;
  Result.Message       := '';
  Result.Details       := TStringList.Create;
  Result.TotalChecked  := 0;
  Result.BrokenCount   := 0;
  Result.NotFoundCount := 0;

  // Schritt 1: Encoding prüfen
  lEncoding1 := TEncodingHelper.DetectEncoding( aFile1 );
  lEncoding2 := TEncodingHelper.DetectEncoding( aFile2 );

  if aVerbose then
    begin
      WriteLn( 'File1: ' + aFile1 );
      WriteLn( '  Encoding: ' + EncodingToStr( lEncoding1 ) );
      WriteLn( 'File2 (Master): ' + aFile2 );
      WriteLn( '  Encoding: ' + EncodingToStr( lEncoding2 ) );
    end;

  if lEncoding1 <> lEncoding2 then
    begin
      // Prüfe ob der Text-Inhalt trotz unterschiedlichem Encoding identisch ist
      // (z.B. UTF-8 mit BOM vs. Windows-1252 ohne BOM, aber gleicher Text)
      Var lCheckLines1 : TStringList := NIL;
      Var lCheckLines2 : TStringList := NIL;
      Var lCheckEnc1   : TEncodingType;
      Var lCheckEnc2   : TEncodingType;

      if TEncodingHelper.ReadFile( aFile1, lCheckLines1, lCheckEnc1 ) and
         TEncodingHelper.ReadFile( aFile2, lCheckLines2, lCheckEnc2 ) then
        begin
          if lCheckLines1.Text = lCheckLines2.Text then
            begin
              FreeAndNIL( lCheckLines1 );
              FreeAndNIL( lCheckLines2 );

              Result.ExitCode := ceBOMDifference;
              Result.Message  := 'Encoding differs but content is identical: File1=' + EncodingToStr( lEncoding1 ) +
                                  ' File2=' + EncodingToStr( lEncoding2 );
              Result.Details.Add( Result.Message );

              if aVerbose then
                WriteLn( '  Text content is identical - only encoding/BOM differs' );

              Exit;
            end;
        end;

      FreeAndNIL( lCheckLines1 );
      FreeAndNIL( lCheckLines2 );

      Result.ExitCode := ceEncodingMismatch;
      Result.Message  := 'Encoding mismatch: File1=' + EncodingToStr( lEncoding1 ) +
                          ' File2=' + EncodingToStr( lEncoding2 );
      Result.Details.Add( Result.Message );
      Exit;
    end;

  // Schritt 1b: Binär-Vergleich - identische Dateien brauchen keine Prüfung
  Var lBytes1 := TFile.ReadAllBytes( aFile1 );
  Var lBytes2 := TFile.ReadAllBytes( aFile2 );

  if ( Length( lBytes1 ) = Length( lBytes2 ) ) and
     ( Length( lBytes1 ) > 0 ) and
     CompareMem( @lBytes1[ 0 ], @lBytes2[ 0 ], Length( lBytes1 ) ) then
    begin
      Result.Message := 'OK: Files are binary identical';

      if aVerbose then
        WriteLn( 'Files are binary identical - no further check needed' );

      Exit;
    end;

  // Schritt 2: Master (file2) lesen
  lLines2 := NIL;

  if not TEncodingHelper.ReadFile( aFile2, lLines2, lEncoding2 ) then
    begin
      Result.ExitCode := ceLineNotFound;
      Result.Message  := 'Cannot read master file: ' + aFile2;
      Result.Details.Add( Result.Message );
      Exit;
    end;

  // Schritt 3: File1 lesen
  lLines1 := NIL;

  if not TEncodingHelper.ReadFile( aFile1, lLines1, lEncoding1 ) then
    begin
      Result.ExitCode := ceLineNotFound;
      Result.Message  := 'Cannot read file: ' + aFile1;
      Result.Details.Add( Result.Message );
      FreeAndNIL( lLines2 );
      Exit;
    end;

  try
    // Schritt 4: Zeilen mit Sonderzeichen aus Master extrahieren (zeilenbasiert)
    lInfos := ExtractLineInfos( lLines2 );

    try
      // Gesamtzahl der Sonderzeichen zählen
      lCharCount := 0;

      for i := 0 to lInfos.Count - 1 do
        lCharCount := lCharCount + Length( lInfos[ i ].SpecialChars );

      lLineCount := lInfos.Count;

      if aVerbose then
        begin
          WriteLn( 'Lines with special characters in master: ' + IntToStr( lLineCount ) );
          WriteLn( 'Total special characters to check: ' + IntToStr( lCharCount ) );
        end;

      Result.TotalChecked := lCharCount;

      // Schritt 5: Pass 1 — Jede Zeile in File1 prüfen
      if aVerbose then
        WriteLn( '--- Pass 1 ---' );

      RunCheckPass( lLines1, lLines2, lInfos, aVerbose, Result );

      // Schritt 6: Pass 2 — Wenn Pass 1 Fehler hat, Zeilen-Mapping erstellen und nochmal prüfen
      if ( Result.BrokenCount > 0 ) or ( Result.NotFoundCount > 0 ) then
        begin
          if aVerbose then
            WriteLn( Format( '--- Pass 2 (Pass 1 had %d broken, %d not found) ---',
              [ Result.BrokenCount, Result.NotFoundCount ] ) );

          Var lMapping := BuildLineMapping( lLines2, lLines1, aVerbose );

          try
            if lMapping.Count > 0 then
              begin
                // MasterLineIndex in lInfos durch gemappte Indices ersetzen
                Var lMappedInfos := TList<TLineCharInfo>.Create;

                try
                  for i := 0 to lInfos.Count - 1 do
                    begin
                      Var lInfo     := lInfos[ i ];
                      Var lMappedIdx : Integer;

                      if lMapping.TryGetValue( lInfo.MasterLineIndex, lMappedIdx ) then
                        lInfo.MasterLineIndex := lMappedIdx;

                      lMappedInfos.Add( lInfo );
                    end;

                  RunCheckPass( lLines1, lLines2, lMappedInfos, aVerbose, Result );
                finally
                  lMappedInfos.Free;
                end;
              end;
          finally
            lMapping.Free;
          end;
        end;
    finally
      lInfos.Free;
    end;
  finally
    FreeAndNIL( lLines1 );
    FreeAndNIL( lLines2 );
  end;

  // Ergebnis bestimmen
  if Result.BrokenCount > 0 then
    begin
      Result.ExitCode := ceCharsBroken;
      Result.Message  := Format( 'BROKEN: %d of %d special characters are corrupted',
        [ Result.BrokenCount, Result.TotalChecked ] );
    end
  else
  if Result.NotFoundCount > 0 then
    begin
      Result.ExitCode := ceLineNotFound;
      Result.Message  := Format( 'WARNING: %d of %d special chars in lines not found (manual review needed)',
        [ Result.NotFoundCount, Result.TotalChecked ] );
    end
  else begin
         Result.Message := Format( 'OK: All %d special characters in %d lines match',
           [ Result.TotalChecked, lLineCount ] );
       end;
end;

end.
