Unit StrEditor.Repair;

interface

Uses
  System.SysUtils
, System.Classes
;

Type
  TVCSType = ( vcsAuto, vcsHg, vcsGit );

  TRepairResult = Record
    Success       : Boolean;
    ErrorMessage  : string;
    BytesRepaired : Integer;
    UmlautsFound  : Integer;
    VCSUsed       : TVCSType;
  end;

  {$REGION 'Documentation'}
  /// <summary>
  ///   Repariert kaputte Umlaute (9D-Bytes) durch Vergleich mit VCS-Original.
  ///   Unterstützt Mercurial (hg) und Git.
  /// </summary>
  {$ENDREGION}
  TUmlautRepair = class
    public
      /// <summary>
      ///   Prüft ob eine Datei kaputte Umlaute (9D-Bytes) enthält
      /// </summary>
      class function HasCorruptedUmlauts( const aFilePath : string ) : Boolean;

      /// <summary>
      ///   Zählt die Anzahl der kaputten Umlaute (9D-Bytes)
      /// </summary>
      class function CountCorruptedUmlauts( const aFilePath : string ) : Integer;

      /// <summary>
      ///   Repariert kaputte Umlaute mit Hilfe des VCS (Mercurial oder Git).
      ///   Bei vcsAuto wird automatisch erkannt welches VCS verwendet wird.
      /// </summary>
      class function RepairFromVCS( const aFilePath : string;
                                    const aVCS      : TVCSType = vcsAuto;
                                    const aRepoRoot : string = '';
                                    const aRevision : string = '';
                                    const aDryRun   : Boolean = false;
                                    const aVerbose  : Boolean = false;
                                    const aBackup   : Boolean = false ) : TRepairResult;

      /// <summary>
      ///   Repariert kaputte Umlaute durch Vergleich mit einer Referenz-Datei
      /// </summary>
      class function RepairFromReference( const aCorruptedPath : string;
                                          const aReferencePath : string;
                                          const aDryRun        : Boolean = false;
                                          const aVerbose       : Boolean = false;
                                          const aBackup        : Boolean = false ) : TRepairResult;

      /// <summary>
      ///   Erkennt automatisch welches VCS verwendet wird
      /// </summary>
      class function DetectVCS( const aFilePath : string; out aRepoRoot : string ) : TVCSType;

      /// <summary>
      ///   Konvertiert VCS-Typ zu String
      /// </summary>
      class function VCSToString( const aVCS : TVCSType ) : string;

      /// <summary>
      ///   Parst VCS-String zu Typ
      /// </summary>
      class function StringToVCS( const aValue : string ) : TVCSType;

    strict private
      const CORRUPTED_BYTE = $9D;  // Das kaputte Byte, das alle Umlaute ersetzt

      class function GetMercurialRoot( const aFilePath : string ) : string;
      class function GetGitRoot( const aFilePath : string ) : string;
      class function ExecuteVCSCommand( const aRepoRoot : string;
                                        const aCommand  : string;
                                        out   aContent  : TBytes ) : Boolean;
      class function GetDefaultRevision( const aVCS : TVCSType ) : string;
      class function RepairBytes( const aCorrupted  : TBytes;
                                  const aReference  : TBytes;
                                  out   aRepaired   : TBytes;
                                  out   aCount      : Integer;
                                  const aVerbose    : Boolean ) : Boolean;
  end;

implementation

Uses
  System.IOUtils
, Winapi.Windows
;

{ TUmlautRepair }

class function TUmlautRepair.HasCorruptedUmlauts( const aFilePath : string ) : Boolean;
Var
  lBytes : TBytes;
  lByte  : Byte;
begin
  Result := false;

  if not FileExists( aFilePath ) then
    Exit;

  lBytes := TFile.ReadAllBytes( aFilePath );

  for lByte in lBytes do
    begin
      if lByte = CORRUPTED_BYTE then
        begin
          Result := true;
          Exit;
        end;
    end;
end;

class function TUmlautRepair.CountCorruptedUmlauts( const aFilePath : string ) : Integer;
Var
  lBytes : TBytes;
  lByte  : Byte;
begin
  Result := 0;

  if not FileExists( aFilePath ) then
    Exit;

  lBytes := TFile.ReadAllBytes( aFilePath );

  for lByte in lBytes do
    begin
      if lByte = CORRUPTED_BYTE then
        Inc( Result );
    end;
end;

class function TUmlautRepair.VCSToString( const aVCS : TVCSType ) : string;
begin
  case aVCS of
    vcsAuto : Result := 'auto';
    vcsHg   : Result := 'hg';
    vcsGit  : Result := 'git';
    else Result := 'unknown';
  end; // of case
end;

class function TUmlautRepair.StringToVCS( const aValue : string ) : TVCSType;
begin
  if SameText( aValue, 'hg' ) or SameText( aValue, 'mercurial' )
    then Result := vcsHg
    else
  if SameText( aValue, 'git' )
    then Result := vcsGit
    else Result := vcsAuto;
end;

class function TUmlautRepair.GetDefaultRevision( const aVCS : TVCSType ) : string;
begin
  case aVCS of
    vcsHg  : Result := '.^';      // Mercurial: Parent der aktuellen Revision
    vcsGit : Result := 'HEAD~1';  // Git: Ein Commit zurück
    else Result := '';
  end; // of case
end;

class function TUmlautRepair.GetMercurialRoot( const aFilePath : string ) : string;
Var
  lDir : string;
begin
  Result := '';
  lDir   := TPath.GetDirectoryName( TPath.GetFullPath( aFilePath ) );

  while lDir <> '' do
    begin
      if DirectoryExists( TPath.Combine( lDir, '.hg' ) ) then
        begin
          Result := lDir;
          Exit;
        end;

      Var lParent := TPath.GetDirectoryName( lDir );

      if lParent = lDir then
        Break;

      lDir := lParent;
    end;
end;

class function TUmlautRepair.GetGitRoot( const aFilePath : string ) : string;
Var
  lDir : string;
begin
  Result := '';
  lDir   := TPath.GetDirectoryName( TPath.GetFullPath( aFilePath ) );

  while lDir <> '' do
    begin
      if DirectoryExists( TPath.Combine( lDir, '.git' ) ) then
        begin
          Result := lDir;
          Exit;
        end;

      Var lParent := TPath.GetDirectoryName( lDir );

      if lParent = lDir then
        Break;

      lDir := lParent;
    end;
end;

class function TUmlautRepair.DetectVCS( const aFilePath : string; out aRepoRoot : string ) : TVCSType;
begin
  // Mercurial zuerst prüfen
  aRepoRoot := GetMercurialRoot( aFilePath );

  if aRepoRoot <> '' then
    begin
      Result := vcsHg;
      Exit;
    end;

  // Git prüfen
  aRepoRoot := GetGitRoot( aFilePath );

  if aRepoRoot <> '' then
    begin
      Result := vcsGit;
      Exit;
    end;

  Result    := vcsAuto;
  aRepoRoot := '';
end;

class function TUmlautRepair.ExecuteVCSCommand( const aRepoRoot : string;
                                                const aCommand  : string;
                                                out   aContent  : TBytes ) : Boolean;
Var
  lSA          : TSecurityAttributes;
  lSI          : TStartupInfo;
  lPI          : TProcessInformation;
  lReadPipe    : THandle;
  lWritePipe   : THandle;
  lBytesRead   : DWORD;
  lBuffer      : array[ 0..4095 ] of Byte;
  lTotalBytes  : TBytes;
  lExitCode    : DWORD;
begin
  Result  := false;
  SetLength( aContent, 0 );

  // Pipe für stdout erstellen
  FillChar( lSA, SizeOf( lSA ), 0 );
  lSA.nLength        := SizeOf( lSA );
  lSA.bInheritHandle := true;

  if not CreatePipe( lReadPipe, lWritePipe, @lSA, 0 ) then
    Exit;

  try
    FillChar( lSI, SizeOf( lSI ), 0 );
    lSI.cb          := SizeOf( lSI );
    lSI.dwFlags     := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    lSI.hStdOutput  := lWritePipe;
    lSI.hStdError   := lWritePipe;
    lSI.wShowWindow := SW_HIDE;

    FillChar( lPI, SizeOf( lPI ), 0 );

    if not CreateProcess( NIL, PChar( aCommand ), NIL, NIL, true,
                          CREATE_NO_WINDOW, NIL, PChar( aRepoRoot ), lSI, lPI ) then
      Exit;

    try
      CloseHandle( lWritePipe );
      lWritePipe := 0;

      // Ausgabe lesen
      SetLength( lTotalBytes, 0 );

      while ReadFile( lReadPipe, lBuffer, SizeOf( lBuffer ), lBytesRead, NIL ) and ( lBytesRead > 0 ) do
        begin
          Var lOldLen := Length( lTotalBytes );
          SetLength( lTotalBytes, lOldLen + Integer( lBytesRead ) );
          Move( lBuffer, lTotalBytes[ lOldLen ], lBytesRead );
        end;

      WaitForSingleObject( lPI.hProcess, INFINITE );
      GetExitCodeProcess( lPI.hProcess, lExitCode );

      if lExitCode = 0 then
        begin
          aContent := lTotalBytes;
          Result   := true;
        end;
    finally
      CloseHandle( lPI.hProcess );
      CloseHandle( lPI.hThread );
    end;
  finally
    if lReadPipe <> 0 then
      CloseHandle( lReadPipe );

    if lWritePipe <> 0 then
      CloseHandle( lWritePipe );
  end;
end;

class function TUmlautRepair.RepairBytes( const aCorrupted : TBytes;
                                          const aReference : TBytes;
                                          out   aRepaired  : TBytes;
                                          out   aCount     : Integer;
                                          const aVerbose   : Boolean ) : Boolean;

  function GetUmlautName( aByte : Byte ) : string;
  begin
    case aByte of
      $E4 : Result := 'ä';
      $C4 : Result := 'Ä';
      $F6 : Result := 'ö';
      $D6 : Result := 'Ö';
      $FC : Result := 'ü';
      $DC : Result := 'Ü';
      $DF : Result := 'ß';
      else Result := '?';
    end; // of case
  end;

  // Findet das passende Umlaut-Byte aus der Referenz basierend auf Kontext
  function FindMatchingUmlaut( aCorruptedPos : Integer; const aContext : Integer = 10 ) : Byte;
  Var
    lCtxEnd : Integer;
    lRefPos, lBestMatch, lBestScore, lScore : Integer;
    k, lOffset : Integer;
  begin
    Result := CORRUPTED_BYTE;  // Fallback: nicht ändern

    // Kontext vor dem 9D-Byte prüfen
    lCtxEnd := aCorruptedPos - 1;

    if lCtxEnd < 0 then
      Exit;

    // Suche in der Referenz nach ähnlichem Kontext
    lBestMatch := -1;
    lBestScore := 0;

    for lRefPos := aContext to Length( aReference ) - 1 do
      begin
        // Prüfe ob an dieser Position ein Umlaut ist
        if not ( aReference[ lRefPos ] in [ $E4, $C4, $F6, $D6, $FC, $DC, $DF ] ) then
          Continue;

        // Vergleiche Kontext davor
        lScore := 0;

        for lOffset := 1 to aContext do
          begin
            k := aCorruptedPos - lOffset;

            if ( k >= 0 ) and ( lRefPos - lOffset >= 0 ) then
              begin
                if aCorrupted[ k ] = aReference[ lRefPos - lOffset ] then
                  Inc( lScore )
                else
                if aCorrupted[ k ] = CORRUPTED_BYTE then
                  Inc( lScore );  // 9D könnte auch hier passen
              end;
          end;

        if lScore > lBestScore then
          begin
            lBestScore := lScore;
            lBestMatch := lRefPos;
          end;
      end;

    if ( lBestMatch >= 0 ) and ( lBestScore >= aContext div 2 ) then
      Result := aReference[ lBestMatch ];
  end;

Var
  i            : Integer;
  lFoundByte   : Byte;
  lSameSize    : Boolean;
begin
  aCount    := 0;
  lSameSize := Length( aCorrupted ) = Length( aReference );

  if ( not lSameSize ) and aVerbose then
    WriteLn( 'WARNING: File sizes differ - Corrupted: ', Length( aCorrupted ),
             ', Reference: ', Length( aReference ), ' - using context matching' );

  SetLength( aRepaired, Length( aCorrupted ) );
  Move( aCorrupted[ 0 ], aRepaired[ 0 ], Length( aCorrupted ) );

  for i := 0 to Length( aCorrupted ) - 1 do
    begin
      if aCorrupted[ i ] = CORRUPTED_BYTE then
        begin
          if lSameSize and ( i < Length( aReference ) ) then
            begin
              // Einfacher Fall: gleiche Größe - direkte Position
              lFoundByte := aReference[ i ];
            end
          else begin
                 // Komplexer Fall: unterschiedliche Größe - Kontext-Matching
                 lFoundByte := FindMatchingUmlaut( i );
               end;

          // Nur ersetzen wenn ein gültiges Umlaut-Byte gefunden wurde
          if lFoundByte in [ $E4, $C4, $F6, $D6, $FC, $DC, $DF ] then
            begin
              aRepaired[ i ] := lFoundByte;
              Inc( aCount );

              if aVerbose then
                WriteLn( Format( '  Pos %d: 9D -> %s (%s)',
                         [ i, IntToHex( lFoundByte, 2 ), GetUmlautName( lFoundByte ) ] ) );
            end
          else begin
                 if aVerbose then
                   WriteLn( Format( '  Pos %d: 9D - no matching umlaut found!', [ i ] ) );
               end;
        end;
    end;

  Result := aCount > 0;
end;

class function TUmlautRepair.RepairFromVCS( const aFilePath : string;
                                            const aVCS      : TVCSType;
                                            const aRepoRoot : string;
                                            const aRevision : string;
                                            const aDryRun   : Boolean;
                                            const aVerbose  : Boolean;
                                            const aBackup   : Boolean ) : TRepairResult;
Var
  lVCS        : TVCSType;
  lRepoRoot   : string;
  lRelPath    : string;
  lRevision   : string;
  lCommand    : string;
  lCorrupted  : TBytes;
  lReference  : TBytes;
  lRepaired   : TBytes;
begin
  Result.Success       := false;
  Result.ErrorMessage  := '';
  Result.BytesRepaired := 0;
  Result.UmlautsFound  := 0;
  Result.VCSUsed       := vcsAuto;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  // VCS und Repo-Root bestimmen
  if aVCS = vcsAuto then
    begin
      lVCS := DetectVCS( aFilePath, lRepoRoot );

      if lVCS = vcsAuto then
        begin
          Result.ErrorMessage := 'No VCS repository (hg or git) found for: ' + aFilePath;
          Exit;
        end;
    end
  else begin
         lVCS := aVCS;

         if aRepoRoot <> ''
           then lRepoRoot := aRepoRoot
           else begin
                  case lVCS of
                    vcsHg  : lRepoRoot := GetMercurialRoot( aFilePath );
                    vcsGit : lRepoRoot := GetGitRoot( aFilePath );
                  end; // of case
                end;

         if lRepoRoot = '' then
           begin
             Result.ErrorMessage := 'No ' + VCSToString( lVCS ) + ' repository found for: ' + aFilePath;
             Exit;
           end;
       end;

  Result.VCSUsed := lVCS;

  // Revision bestimmen
  if aRevision <> ''
    then lRevision := aRevision
    else lRevision := GetDefaultRevision( lVCS );

  // Relativen Pfad berechnen
  lRelPath := ExtractRelativePath( IncludeTrailingPathDelimiter( lRepoRoot ),
                                   TPath.GetFullPath( aFilePath ) );

  if aVerbose then
    begin
      WriteLn( 'VCS: ', VCSToString( lVCS ) );
      WriteLn( 'Repository root: ', lRepoRoot );
      WriteLn( 'Relative path: ', lRelPath );
      WriteLn( 'Revision: ', lRevision );
    end;

  // Kaputte Datei lesen
  lCorrupted := TFile.ReadAllBytes( aFilePath );
  Result.UmlautsFound := CountCorruptedUmlauts( aFilePath );

  if Result.UmlautsFound = 0 then
    begin
      Result.Success      := true;
      Result.ErrorMessage := 'No corrupted umlauts found';
      Exit;
    end;

  if aVerbose then
    WriteLn( 'Found ', Result.UmlautsFound, ' corrupted umlauts (9D bytes)' );

  // VCS-Befehl erstellen
  case lVCS of
    vcsHg  : lCommand := Format( 'hg cat -r %s "%s"', [ lRevision, lRelPath ] );
    vcsGit : lCommand := Format( 'git show %s:"%s"', [ lRevision, StringReplace( lRelPath, '\', '/', [ rfReplaceAll ] ) ] );
  end; // of case

  if aVerbose then
    WriteLn( 'Command: ', lCommand );

  // Original aus VCS holen
  if not ExecuteVCSCommand( lRepoRoot, lCommand, lReference ) then
    begin
      Result.ErrorMessage := 'Failed to get file from ' + VCSToString( lVCS ) + ': ' + lCommand;
      Exit;
    end;

  if aVerbose then
    WriteLn( 'Got reference from ', VCSToString( lVCS ), ': ', Length( lReference ), ' bytes' );

  // Reparieren
  if not RepairBytes( lCorrupted, lReference, lRepaired, Result.BytesRepaired, aVerbose ) then
    begin
      Result.ErrorMessage := 'No bytes repaired';
      Exit;
    end;

  if aDryRun then
    begin
      Result.Success := true;
      WriteLn( 'DRY-RUN: Would repair ', Result.BytesRepaired, ' bytes' );
      Exit;
    end;

  // Backup erstellen wenn gewünscht
  if aBackup then
    begin
      Var lBackupPath := aFilePath + '.bak';

      TFile.Copy( aFilePath, lBackupPath, true );

      if aVerbose then
        WriteLn( 'Backup created: ', lBackupPath );
    end;

  // Speichern
  TFile.WriteAllBytes( aFilePath, lRepaired );
  Result.Success := true;

  if aVerbose then
    WriteLn( 'Repaired ', Result.BytesRepaired, ' bytes in ', aFilePath );
end;

class function TUmlautRepair.RepairFromReference( const aCorruptedPath : string;
                                                  const aReferencePath : string;
                                                  const aDryRun        : Boolean;
                                                  const aVerbose       : Boolean;
                                                  const aBackup        : Boolean ) : TRepairResult;
Var
  lCorrupted : TBytes;
  lReference : TBytes;
  lRepaired  : TBytes;
begin
  Result.Success       := false;
  Result.ErrorMessage  := '';
  Result.BytesRepaired := 0;
  Result.UmlautsFound  := 0;
  Result.VCSUsed       := vcsAuto;  // Kein VCS bei Referenz-Datei

  if not FileExists( aCorruptedPath ) then
    begin
      Result.ErrorMessage := 'Corrupted file not found: ' + aCorruptedPath;
      Exit;
    end;

  if not FileExists( aReferencePath ) then
    begin
      Result.ErrorMessage := 'Reference file not found: ' + aReferencePath;
      Exit;
    end;

  lCorrupted := TFile.ReadAllBytes( aCorruptedPath );
  lReference := TFile.ReadAllBytes( aReferencePath );

  Result.UmlautsFound := CountCorruptedUmlauts( aCorruptedPath );

  if Result.UmlautsFound = 0 then
    begin
      Result.Success      := true;
      Result.ErrorMessage := 'No corrupted umlauts found';
      Exit;
    end;

  if aVerbose then
    WriteLn( 'Found ', Result.UmlautsFound, ' corrupted umlauts (9D bytes)' );

  if not RepairBytes( lCorrupted, lReference, lRepaired, Result.BytesRepaired, aVerbose ) then
    begin
      Result.ErrorMessage := 'No bytes repaired';
      Exit;
    end;

  if aDryRun then
    begin
      Result.Success := true;
      WriteLn( 'DRY-RUN: Would repair ', Result.BytesRepaired, ' bytes' );
      Exit;
    end;

  // Backup erstellen wenn gewünscht
  if aBackup then
    begin
      Var lBackupPath := aCorruptedPath + '.bak';

      TFile.Copy( aCorruptedPath, lBackupPath, true );

      if aVerbose then
        WriteLn( 'Backup created: ', lBackupPath );
    end;

  TFile.WriteAllBytes( aCorruptedPath, lRepaired );
  Result.Success := true;

  if aVerbose then
    WriteLn( 'Repaired ', Result.BytesRepaired, ' bytes in ', aCorruptedPath );
end;

end.

