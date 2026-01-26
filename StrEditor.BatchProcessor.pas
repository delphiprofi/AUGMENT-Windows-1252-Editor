Unit StrEditor.BatchProcessor;

interface

Uses
  System.SysUtils
, System.Classes
, System.Generics.Collections
, System.Generics.Defaults
, StrEditor.CommandLine
, StrEditor.Operations
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Batch Processor für Line-Operationen.
  ///   WICHTIG: Alle Zeilennummern in den Operationen beziehen sich auf den
  ///   ORIGINAL-Zustand der Datei! Der BatchProcessor rechnet intern um.
  ///   Sortiert Operationen nach Zeilennummer (höchste zuerst) und führt sie aus.
  /// </summary>
  {$ENDREGION}
  TBatchProcessor = class
    private
      Type
        TLineOperation = record
          Command          : TCommandType;
          LineNumber       : Integer;   // Original-Zeilennummer
          LineNumbers      : string;
          StartLine        : Integer;   // Original-Startzeile
          EndLine          : Integer;   // Original-Endzeile
          InsertAfterLine  : Integer;   // Original-Zeile
          InsertBeforeLine : Integer;   // Original-Zeile
          Text             : string;
          TextIsBase64     : Boolean;
          IndentSpaces     : Integer;   // Anzahl Spaces für Indent/Unindent
          Backup           : Boolean;
          DryRun           : Boolean;
          Diff             : Boolean;
          Verbose          : Boolean;
        end;

        // Speichert Offset-Informationen nach einer Operation
        TLineOffset = record
          AfterOriginalLine : Integer;  // Offset gilt für Zeilen > dieser Original-Zeile
          Offset            : Integer;  // +N für Einfügungen, -N für Löschungen
        end;

      class function CalculateAdjustedLine( aOriginalLine : Integer; const aOffsets : TList<TLineOffset> ) : Integer;
      class function CountTextLines( const aText : string ) : Integer;

    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Prüft, ob die Operationen Line-Operationen enthalten
      /// </summary>
      {$ENDREGION}
      class function HasLineOperations( const aOperations : TArray<TCommandLineParams> ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Verarbeitet Line-Operationen in der richtigen Reihenfolge (höchste Zeile zuerst)
      /// </summary>
      {$ENDREGION}
      class function ProcessLineOperations( const aFilePath : string; const aOperations : TArray<TCommandLineParams> ) : Boolean;
  end;

implementation

class function TBatchProcessor.CalculateAdjustedLine( aOriginalLine : Integer; const aOffsets : TList<TLineOffset> ) : Integer;
Var
  lOffset : TLineOffset;
begin
  Result := aOriginalLine;

  for lOffset in aOffsets do
    if aOriginalLine > lOffset.AfterOriginalLine then
      Result := Result + lOffset.Offset;
end;

class function TBatchProcessor.CountTextLines( const aText : string ) : Integer;
Var
  i : Integer;
begin
  if aText = '' then
    begin
      Result := 0;
      Exit;
    end;

  Result := 1;
  i      := 1;

  while i <= Length( aText ) do
    begin
      if ( aText[ i ] = #13 ) and ( i < Length( aText ) ) and ( aText[ i + 1 ] = #10 ) then
        begin
          Inc( Result );
          Inc( i, 2 );
        end
      else
        Inc( i );
    end;
end;

class function TBatchProcessor.HasLineOperations( const aOperations : TArray<TCommandLineParams> ) : Boolean;
Var
  lOp : TCommandLineParams;
begin
  Result := false;

  for lOp in aOperations do
    begin
      if ( lOp.Command = ctDeleteLine ) or ( lOp.Command = ctDeleteLines ) or
         ( lOp.Command = ctReplaceLine ) or ( lOp.Command = ctInsert ) or
         ( lOp.Command = ctInsertBefore ) or ( lOp.Command = ctIndent ) or
         ( lOp.Command = ctUnindent ) then
        begin
          Result := true;
          Exit;
        end;
    end;
end;

class function TBatchProcessor.ProcessLineOperations( const aFilePath : string; const aOperations : TArray<TCommandLineParams> ) : Boolean;
Var
  lLineOps : TList<TLineOperation>;
  lOffsets : TList<TLineOffset>;
  lOp      : TCommandLineParams;
  lLineOp  : TLineOperation;
  lOffset  : TLineOffset;
  lResult  : TOperationResult;
  i        : Integer;
  lAdjustedLine      : Integer;
  lAdjustedStartLine : Integer;
  lAdjustedEndLine   : Integer;
  lTextLineCount     : Integer;
begin
  Result   := true;
  lLineOps := TList<TLineOperation>.Create;
  lOffsets := TList<TLineOffset>.Create;

  try
    // Sammle alle Line-Operationen mit ihren ORIGINAL-Zeilennummern
    for lOp in aOperations do
      begin
        if ( lOp.Command = ctDeleteLine ) or ( lOp.Command = ctDeleteLines ) or
           ( lOp.Command = ctReplaceLine ) or ( lOp.Command = ctInsert ) or
           ( lOp.Command = ctInsertBefore ) or ( lOp.Command = ctIndent ) or
           ( lOp.Command = ctUnindent ) then
          begin
            lLineOp.Command          := lOp.Command;
            lLineOp.LineNumber       := lOp.LineNumber;
            lLineOp.LineNumbers      := lOp.LineNumbers;
            lLineOp.StartLine        := lOp.StartLine;
            lLineOp.EndLine          := lOp.EndLine;
            lLineOp.InsertAfterLine  := lOp.InsertAfterLine;
            lLineOp.InsertBeforeLine := lOp.InsertBeforeLine;
            lLineOp.Text             := lOp.Text;
            lLineOp.TextIsBase64     := lOp.TextIsBase64;
            lLineOp.IndentSpaces     := lOp.IndentSpaces;
            lLineOp.Backup           := lOp.Backup;
            lLineOp.DryRun           := lOp.DryRun;
            lLineOp.Diff             := lOp.Diff;
            lLineOp.Verbose          := lOp.Verbose;

            lLineOps.Add( lLineOp );
          end;
      end;

    // Sortiere nach höchster Original-Zeilennummer (absteigend)
    lLineOps.Sort( TComparer<TLineOperation>.Construct(
      function( const Left, Right : TLineOperation ) : Integer
      Var
        lLeftMax  : Integer;
        lRightMax : Integer;
      begin
        // Bestimme die relevante Zeilennummer für Left
        if Left.Command = ctDeleteLine
          then lLeftMax := Left.LineNumber
          else
        if Left.Command = ctReplaceLine
          then lLeftMax := Left.LineNumber
          else
        if Left.Command = ctInsert
          then lLeftMax := Left.InsertAfterLine
          else
        if Left.Command = ctInsertBefore
          then lLeftMax := Left.InsertBeforeLine
          else
        if ( Left.StartLine > 0 ) and ( Left.EndLine > 0 )
          then lLeftMax := Left.EndLine
          else lLeftMax := 0;

        // Bestimme die relevante Zeilennummer für Right
        if Right.Command = ctDeleteLine
          then lRightMax := Right.LineNumber
          else
        if Right.Command = ctReplaceLine
          then lRightMax := Right.LineNumber
          else
        if Right.Command = ctInsert
          then lRightMax := Right.InsertAfterLine
          else
        if Right.Command = ctInsertBefore
          then lRightMax := Right.InsertBeforeLine
          else
        if ( Right.StartLine > 0 ) and ( Right.EndLine > 0 )
          then lRightMax := Right.EndLine
          else lRightMax := 0;

        // Sortiere absteigend: höchste Zeilennummer zuerst
        Result := lRightMax - lLeftMax;
      end ) );

    // Führe Operationen aus - von höchster zu niedrigster Original-Zeilennummer
    // Nach jeder Operation wird ein Offset gespeichert für nachfolgende Operationen
    for i := 0 to lLineOps.Count - 1 do
      begin
        lLineOp := lLineOps[ i ];

        case lLineOp.Command of
          ctDeleteLine:
            begin
              // Berechne angepasste Zeilennummer basierend auf bisherigen Offsets
              lAdjustedLine := CalculateAdjustedLine( lLineOp.LineNumber, lOffsets );

              lResult := TStringOperations.DeleteLine( aFilePath, lAdjustedLine, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;

              // Offset speichern: Zeilen nach dieser Original-Zeile verschieben sich um -1
              lOffset.AfterOriginalLine := lLineOp.LineNumber;
              lOffset.Offset            := -1;
              lOffsets.Add( lOffset );
            end;

          ctDeleteLines:
            begin
              // Berechne angepasste Zeilennummern
              lAdjustedStartLine := CalculateAdjustedLine( lLineOp.StartLine, lOffsets );
              lAdjustedEndLine   := CalculateAdjustedLine( lLineOp.EndLine, lOffsets );

              if lLineOp.LineNumbers <> ''
                then lResult := TStringOperations.DeleteLines( aFilePath, lLineOp.LineNumbers, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose )
                else lResult := TStringOperations.DeleteLines( aFilePath, lAdjustedStartLine, lAdjustedEndLine, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;

              // Offset speichern: Anzahl gelöschter Zeilen
              lOffset.AfterOriginalLine := lLineOp.EndLine;
              lOffset.Offset            := -( lLineOp.EndLine - lLineOp.StartLine + 1 );
              lOffsets.Add( lOffset );
            end;

          ctReplaceLine:
            begin
              // Berechne angepasste Zeilennummer
              lAdjustedLine := CalculateAdjustedLine( lLineOp.LineNumber, lOffsets );

              lResult := TStringOperations.ReplaceLine( aFilePath, lAdjustedLine, lLineOp.Text, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose, lLineOp.TextIsBase64 );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;

              // Offset speichern: Wenn Text mehrere Zeilen hat, verschieben sich nachfolgende Zeilen
              lTextLineCount := CountTextLines( lLineOp.Text );

              if lTextLineCount <> 1 then
                begin
                  lOffset.AfterOriginalLine := lLineOp.LineNumber;
                  lOffset.Offset            := lTextLineCount - 1;  // -1 weil eine Zeile ersetzt wird
                  lOffsets.Add( lOffset );
                end;
            end;

          ctInsert:
            begin
              // Berechne angepasste Zeilennummer
              lAdjustedLine := CalculateAdjustedLine( lLineOp.InsertAfterLine, lOffsets );

              lResult := TStringOperations.Insert( aFilePath, lLineOp.Text, lAdjustedLine, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.TextIsBase64 );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;

              // Offset speichern: Eingefügte Zeilen verschieben nachfolgende Zeilen
              lTextLineCount := CountTextLines( lLineOp.Text );

              lOffset.AfterOriginalLine := lLineOp.InsertAfterLine;
              lOffset.Offset            := lTextLineCount;
              lOffsets.Add( lOffset );
            end;

          ctInsertBefore:
            begin
              // Berechne angepasste Zeilennummer
              lAdjustedLine := CalculateAdjustedLine( lLineOp.InsertBeforeLine, lOffsets );

              lResult := TStringOperations.InsertBefore( aFilePath, lLineOp.Text, lAdjustedLine, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.TextIsBase64 );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;

              // Offset speichern: Eingefügte Zeilen verschieben nachfolgende Zeilen
              // InsertBefore fügt VOR der Zeile ein, also gilt der Offset für Zeilen >= InsertBeforeLine
              lTextLineCount := CountTextLines( lLineOp.Text );

              lOffset.AfterOriginalLine := lLineOp.InsertBeforeLine - 1;  // -1 weil VOR der Zeile
              lOffset.Offset            := lTextLineCount;
              lOffsets.Add( lOffset );
            end;

          ctIndent:
            begin
              // Berechne angepasste Zeilennummern
              lAdjustedStartLine := CalculateAdjustedLine( lLineOp.StartLine, lOffsets );
              lAdjustedEndLine   := CalculateAdjustedLine( lLineOp.EndLine, lOffsets );

              lResult := TStringOperations.IndentLines( aFilePath, lAdjustedStartLine, lAdjustedEndLine, lLineOp.IndentSpaces, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;

              // Indent ändert keine Zeilenanzahl - kein Offset nötig
            end;

          ctUnindent:
            begin
              // Berechne angepasste Zeilennummern
              lAdjustedStartLine := CalculateAdjustedLine( lLineOp.StartLine, lOffsets );
              lAdjustedEndLine   := CalculateAdjustedLine( lLineOp.EndLine, lOffsets );

              lResult := TStringOperations.UnindentLines( aFilePath, lAdjustedStartLine, lAdjustedEndLine, lLineOp.IndentSpaces, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;

              // Unindent ändert keine Zeilenanzahl - kein Offset nötig
            end;
        end; // of case
      end;
  finally
    lOffsets.Free;
    lLineOps.Free;
  end;
end;

end.

