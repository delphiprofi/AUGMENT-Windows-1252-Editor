Unit TestStrEditor.Config;

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, System.IOUtils
, System.JSON
, Winapi.Windows
, StrEditor.Config
, StrEditor.CommandLine
, StrEditor.BatchProcessor
, StrEditor.Encoding
, StrEditor.Operations
;

Type
  [TestFixture]
  TTestConfig = class
    strict private
      fTestFilePath   : string;
      fTestConfigPath : string;

      procedure CreateTestFile( const aContent : string );
      procedure CreateTestConfig( const aJSON : string );
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestJSONConfig_DeleteLine;

      [Test]
      procedure TestJSONConfig_DeleteLines_CommaSeparated;

      [Test]
      procedure TestJSONConfig_DeleteLines_Range;

      [Test]
      procedure TestJSONConfig_ReplaceLine;

      [Test]
      procedure TestJSONConfig_ReplaceLine_Base64;

      [Test]
      procedure TestBatchProcessor_MultipleDeleteLines;

      [Test]
      procedure TestBatchProcessor_MixedOperations;

      [Test]
      procedure TestBatchProcessor_SortedExecution;

      [Test]
      procedure TestJSONConfig_UTF8_Umlaut_In_OldStr;

      [Test]
      procedure TestJSONConfig_UTF8_Umlaut_In_NewStr;

      [Test]
      procedure TestJSONConfig_ViaExe_RemoveLogBlocks;

      [Test]
      procedure TestJSONConfig_ViaExe_AddLogBlocks;

      [Test]
      procedure TestJSONConfig_ViaExe_FullRoundtrip;
  end;

implementation

procedure TTestConfig.Setup;
begin
  fTestFilePath   := TPath.Combine( TPath.GetTempPath, 'test_config_' + IntToStr( Random( 10000 ) ) + '.txt' );
  fTestConfigPath := TPath.Combine( TPath.GetTempPath, 'test_config_' + IntToStr( Random( 10000 ) ) + '.json' );
end;

procedure TTestConfig.TearDown;
begin
  if FileExists( fTestFilePath ) then
    System.SysUtils.DeleteFile( fTestFilePath );

  if FileExists( fTestConfigPath ) then
    System.SysUtils.DeleteFile( fTestConfigPath );

  if FileExists( fTestFilePath + '.bak' ) then
    System.SysUtils.DeleteFile( fTestFilePath + '.bak' );
end;

procedure TTestConfig.CreateTestFile( const aContent : string );
Var
  lFile : TStringList;
begin
  lFile := TStringList.Create;

  try
    lFile.Text := aContent;
    lFile.SaveToFile( fTestFilePath, TEncoding.UTF8 );
  finally
    lFile.Free;
  end;
end;

procedure TTestConfig.CreateTestConfig( const aJSON : string );
begin
  TFile.WriteAllText( fTestConfigPath, aJSON, TEncoding.UTF8 );
end;

procedure TTestConfig.TestJSONConfig_DeleteLine;
Var
  lParams : TCommandLineParams;
  lJSON   : string;
begin
  lJSON := '{ "command": "delete-line", "file": "' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '", "line": 2 }';
  CreateTestConfig( lJSON );

  Assert.IsTrue( TConfigHelper.LoadFromJSON( fTestConfigPath, lParams ), 'LoadFromJSON should succeed' );
  Assert.AreEqual( Ord( ctDeleteLine ), Ord( lParams.Command ), 'Command should be ctDeleteLine' );
  Assert.AreEqual( 2, lParams.LineNumber, 'LineNumber should be 2' );
end;

procedure TTestConfig.TestJSONConfig_DeleteLines_CommaSeparated;
Var
  lParams : TCommandLineParams;
  lJSON   : string;
begin
  lJSON := '{ "command": "delete-lines", "file": "' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '", "lines": "1,3,5" }';
  CreateTestConfig( lJSON );

  Assert.IsTrue( TConfigHelper.LoadFromJSON( fTestConfigPath, lParams ), 'LoadFromJSON should succeed' );
  Assert.AreEqual( Ord( ctDeleteLines ), Ord( lParams.Command ), 'Command should be ctDeleteLines' );
  Assert.AreEqual( '1,3,5', lParams.LineNumbers, 'LineNumbers should be "1,3,5"' );
end;

procedure TTestConfig.TestJSONConfig_DeleteLines_Range;
Var
  lParams : TCommandLineParams;
  lJSON   : string;
begin
  lJSON := '{ "command": "delete-lines", "file": "' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '", "start-line": 10, "end-line": 20 }';
  CreateTestConfig( lJSON );

  Assert.IsTrue( TConfigHelper.LoadFromJSON( fTestConfigPath, lParams ), 'LoadFromJSON should succeed' );
  Assert.AreEqual( Ord( ctDeleteLines ), Ord( lParams.Command ), 'Command should be ctDeleteLines' );
  Assert.AreEqual( 10, lParams.StartLine, 'StartLine should be 10' );
  Assert.AreEqual( 20, lParams.EndLine, 'EndLine should be 20' );
end;

procedure TTestConfig.TestJSONConfig_ReplaceLine;
Var
  lParams : TCommandLineParams;
  lJSON   : string;
begin
  lJSON := '{ "command": "replace-line", "file": "' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '", "line": 5, "text": "New Line" }';
  CreateTestConfig( lJSON );

  Assert.IsTrue( TConfigHelper.LoadFromJSON( fTestConfigPath, lParams ), 'LoadFromJSON should succeed' );
  Assert.AreEqual( Ord( ctReplaceLine ), Ord( lParams.Command ), 'Command should be ctReplaceLine' );
  Assert.AreEqual( 5, lParams.LineNumber, 'LineNumber should be 5' );
  Assert.AreEqual( 'New Line', lParams.Text, 'Text should be "New Line"' );
end;

procedure TTestConfig.TestJSONConfig_ReplaceLine_Base64;
Var
  lParams : TCommandLineParams;
  lJSON   : string;
begin
  lJSON := '{ "command": "replace-line", "file": "' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '", "line": 5, "text": "TmV3IExpbmU=", "text-base64-encoded": true }';
  CreateTestConfig( lJSON );

  Assert.IsTrue( TConfigHelper.LoadFromJSON( fTestConfigPath, lParams ), 'LoadFromJSON should succeed' );
  Assert.AreEqual( Ord( ctReplaceLine ), Ord( lParams.Command ), 'Command should be ctReplaceLine' );
  Assert.AreEqual( 5, lParams.LineNumber, 'LineNumber should be 5' );
  Assert.IsTrue( lParams.TextIsBase64, 'TextIsBase64 should be true' );
end;

procedure TTestConfig.TestBatchProcessor_MultipleDeleteLines;
Var
  lOperations : TArray<TCommandLineParams>;
  lResult     : string;
begin
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3'#13#10'Line 4'#13#10'Line 5' );

  SetLength( lOperations, 3 );
  lOperations[ 0 ].Command    := ctDeleteLine;
  lOperations[ 0 ].FilePath   := fTestFilePath;
  lOperations[ 0 ].LineNumber := 2;
  lOperations[ 0 ].Backup     := false;
  lOperations[ 0 ].DryRun     := false;
  lOperations[ 0 ].Diff       := false;
  lOperations[ 0 ].Verbose    := false;

  lOperations[ 1 ].Command    := ctDeleteLine;
  lOperations[ 1 ].FilePath   := fTestFilePath;
  lOperations[ 1 ].LineNumber := 4;
  lOperations[ 1 ].Backup     := false;
  lOperations[ 1 ].DryRun     := false;
  lOperations[ 1 ].Diff       := false;
  lOperations[ 1 ].Verbose    := false;

  lOperations[ 2 ].Command    := ctDeleteLine;
  lOperations[ 2 ].FilePath   := fTestFilePath;
  lOperations[ 2 ].LineNumber := 1;
  lOperations[ 2 ].Backup     := false;
  lOperations[ 2 ].DryRun     := false;
  lOperations[ 2 ].Diff       := false;
  lOperations[ 2 ].Verbose    := false;

  Assert.IsTrue( TBatchProcessor.ProcessLineOperations( fTestFilePath, lOperations ), 'ProcessLineOperations should succeed' );

  lResult := TFile.ReadAllText( fTestFilePath, TEncoding.UTF8 ).TrimRight;
  Assert.AreEqual( 'Line 3'#13#10'Line 5', lResult, 'Lines 1, 2, 4 should be deleted' );
end;

procedure TTestConfig.TestBatchProcessor_MixedOperations;
Var
  lOperations : TArray<TCommandLineParams>;
  lResult     : string;
begin
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3'#13#10'Line 4'#13#10'Line 5' );

  SetLength( lOperations, 2 );
  lOperations[ 0 ].Command    := ctDeleteLine;
  lOperations[ 0 ].FilePath   := fTestFilePath;
  lOperations[ 0 ].LineNumber := 2;
  lOperations[ 0 ].Backup     := false;
  lOperations[ 0 ].DryRun     := false;
  lOperations[ 0 ].Diff       := false;
  lOperations[ 0 ].Verbose    := false;

  lOperations[ 1 ].Command     := ctReplaceLine;
  lOperations[ 1 ].FilePath    := fTestFilePath;
  lOperations[ 1 ].LineNumber  := 4;
  lOperations[ 1 ].Text        := 'New Line 4';
  lOperations[ 1 ].TextIsBase64:= false;
  lOperations[ 1 ].Backup      := false;
  lOperations[ 1 ].DryRun      := false;
  lOperations[ 1 ].Diff        := false;
  lOperations[ 1 ].Verbose     := false;

  Assert.IsTrue( TBatchProcessor.ProcessLineOperations( fTestFilePath, lOperations ), 'ProcessLineOperations should succeed' );

  lResult := TFile.ReadAllText( fTestFilePath, TEncoding.UTF8 ).TrimRight;
  Assert.AreEqual( 'Line 1'#13#10'Line 3'#13#10'New Line 4'#13#10'Line 5', lResult, 'Line 2 deleted, Line 4 replaced' );
end;

procedure TTestConfig.TestBatchProcessor_SortedExecution;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3'#13#10'Line 4'#13#10'Line 5' );

  SetLength( lOperations, 2 );
  lOperations[ 0 ].Command    := ctDeleteLine;
  lOperations[ 0 ].FilePath   := fTestFilePath;
  lOperations[ 0 ].LineNumber := 2;
  lOperations[ 0 ].Backup     := false;
  lOperations[ 0 ].DryRun     := false;
  lOperations[ 0 ].Diff       := false;
  lOperations[ 0 ].Verbose    := false;

  lOperations[ 1 ].Command    := ctDeleteLine;
  lOperations[ 1 ].FilePath   := fTestFilePath;
  lOperations[ 1 ].LineNumber := 4;
  lOperations[ 1 ].Backup     := false;
  lOperations[ 1 ].DryRun     := false;
  lOperations[ 1 ].Diff       := false;
  lOperations[ 1 ].Verbose    := false;

  Assert.IsTrue( TBatchProcessor.HasLineOperations( lOperations ), 'HasLineOperations should return true' );
  Assert.IsTrue( TBatchProcessor.ProcessLineOperations( fTestFilePath, lOperations ), 'ProcessLineOperations should succeed' );

  Var lResult := TFile.ReadAllText( fTestFilePath, TEncoding.UTF8 ).TrimRight;
  Assert.AreEqual( 'Line 1'#13#10'Line 3'#13#10'Line 5', lResult, 'Lines 2 and 4 should be deleted (sorted execution)' );
end;

procedure TTestConfig.TestJSONConfig_UTF8_Umlaut_In_OldStr;
Var
  lParams        : TCommandLineParams;
  lResult        : TOperationResult;
  lContent       : string;
  lBytes         : TBytes;
  lJSONBytes     : TBytes;
  lGeloeschtW    : string;  // "Gelöscht" als Unicode-String
  lGeloeschtUTF8 : TBytes;  // "Gelöscht" als UTF-8 Bytes
begin
  // 1. Erstelle Windows-1252 kodierte Datei mit Umlauten
  //    "Gelöscht" in Windows-1252: G=47, e=65, l=6C, ö=F6, s=73, c=63, h=68, t=74
  lBytes := TBytes.Create( $47, $65, $6C, $F6, $73, $63, $68, $74 );  // "Gelöscht"
  lGeloeschtW := TEncodingHelper.Windows1252ToString( lBytes );  // Unicode "Gelöscht"
  lContent := 'Status: ' + lGeloeschtW;

  // Speichere als Windows-1252
  lBytes := TEncodingHelper.StringToWindows1252( lContent );
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  // 2. Erstelle UTF-8 kodierte JSON-Config mit Umlaut im old-str
  //    "Gelöscht" in UTF-8: G=47, e=65, l=6C, ö=C3 B6, s=73, c=63, h=68, t=74
  lGeloeschtUTF8 := TBytes.Create( $47, $65, $6C, $C3, $B6, $73, $63, $68, $74 );

  // Baue JSON als Bytes zusammen (vermeidet String-Literal-Encoding-Probleme)
  lJSONBytes := TEncoding.UTF8.GetBytes( '{ "command": "str-replace", "file": "' +
                StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) +
                '", "old-str": "' ) +
                lGeloeschtUTF8 +
                TEncoding.UTF8.GetBytes( '", "new-str": "Deleted" }' );

  // Speichere JSON als UTF-8 Bytes
  TFile.WriteAllBytes( fTestConfigPath, lJSONBytes );

  // 3. Lade Config und führe Replace aus
  Assert.IsTrue( TConfigHelper.LoadFromJSON( fTestConfigPath, lParams ), 'LoadFromJSON should succeed' );

  // 4. Prüfe ob der OldStr korrekt geladen wurde (sollte Unicode "ö" enthalten)
  //    Vergleiche mit dem Unicode-String den wir aus Windows-1252 erstellt haben
  Assert.AreEqual( lGeloeschtW, lParams.OldStr, 'OldStr should contain correct Umlaut' );

  // 5. Führe die Ersetzung aus
  lResult := TStringOperations.StrReplace( fTestFilePath, lParams.OldStr, lParams.NewStr, -1, -1, false, false, false );

  // 6. Prüfe ob die Ersetzung erfolgreich war
  Assert.IsTrue( lResult.Success, 'StrReplace should succeed - ' + lResult.ErrorMessage );

  // 7. Prüfe das Ergebnis
  lBytes := TFile.ReadAllBytes( fTestFilePath );
  lContent := TEncodingHelper.Windows1252ToString( lBytes );
  Assert.AreEqual( 'Status: Deleted', lContent.TrimRight, 'Content should be replaced' );
end;

procedure TTestConfig.TestJSONConfig_UTF8_Umlaut_In_NewStr;
Var
  lParams        : TCommandLineParams;
  lResult        : TOperationResult;
  lContent       : string;
  lBytes         : TBytes;
  lJSONBytes     : TBytes;
  lGeloeschtW    : string;  // "Gelöscht" als Unicode-String
  lGeloeschtUTF8 : TBytes;  // "Gelöscht" als UTF-8 Bytes
begin
  // 1. Erstelle Windows-1252 kodierte Datei
  lContent := 'Status: Deleted';
  lBytes := TEncodingHelper.StringToWindows1252( lContent );
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  // Erstelle Unicode-Referenzstring aus Windows-1252 Bytes
  lBytes := TBytes.Create( $47, $65, $6C, $F6, $73, $63, $68, $74 );  // "Gelöscht"
  lGeloeschtW := TEncodingHelper.Windows1252ToString( lBytes );  // Unicode "Gelöscht"

  // 2. Erstelle UTF-8 kodierte JSON-Config mit Umlaut im new-str
  //    "Gelöscht" in UTF-8: G=47, e=65, l=6C, ö=C3 B6, s=73, c=63, h=68, t=74
  lGeloeschtUTF8 := TBytes.Create( $47, $65, $6C, $C3, $B6, $73, $63, $68, $74 );

  // Baue JSON als Bytes zusammen (vermeidet String-Literal-Encoding-Probleme)
  lJSONBytes := TEncoding.UTF8.GetBytes( '{ "command": "str-replace", "file": "' +
                StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) +
                '", "old-str": "Deleted", "new-str": "' ) +
                lGeloeschtUTF8 +
                TEncoding.UTF8.GetBytes( '" }' );

  // Speichere JSON als UTF-8 Bytes
  TFile.WriteAllBytes( fTestConfigPath, lJSONBytes );

  // 3. Lade Config und führe Replace aus
  Assert.IsTrue( TConfigHelper.LoadFromJSON( fTestConfigPath, lParams ), 'LoadFromJSON should succeed' );

  // 4. Prüfe ob der NewStr korrekt geladen wurde
  //    Vergleiche mit dem Unicode-String den wir aus Windows-1252 erstellt haben
  Assert.AreEqual( lGeloeschtW, lParams.NewStr, 'NewStr should contain correct Umlaut' );

  // 5. Führe die Ersetzung aus
  lResult := TStringOperations.StrReplace( fTestFilePath, lParams.OldStr, lParams.NewStr, -1, -1, false, false, false );

  Assert.IsTrue( lResult.Success, 'StrReplace should succeed - ' + lResult.ErrorMessage );

  // 6. Prüfe das Ergebnis - muss als Windows-1252 gespeichert sein
  lBytes := TFile.ReadAllBytes( fTestFilePath );

  // Prüfe ob das ö als Windows-1252 Byte F6 gespeichert wurde
  lContent := TEncodingHelper.Windows1252ToString( lBytes );

  // Vergleiche mit unserem Unicode-Referenzstring
  Assert.AreEqual( 'Status: ' + lGeloeschtW, lContent.TrimRight, 'Content should contain Umlaut' );

  // Zusätzlich: Prüfe das tatsächliche Byte für ö
  // In "Status: Gelöscht" ist ö an Position 11 (0-basiert)
  // S=0, t=1, a=2, t=3, u=4, s=5, :=6, space=7, G=8, e=9, l=10, ö=11
  Assert.AreEqual( Byte( $F6 ), lBytes[ 11 ], 'Byte for ö should be F6 (Windows-1252)' );
end;

function RunStrEditor( const aConfigPath : string ) : Integer;
Var
  lStartupInfo  : TStartupInfo;
  lProcessInfo  : TProcessInformation;
  lCommandLine  : string;
  lExitCode     : DWORD;
begin
  Result := -1;

  lCommandLine := 'StrEditor.exe --config "' + aConfigPath + '"';

  FillChar( lStartupInfo, SizeOf( lStartupInfo ), 0 );
  lStartupInfo.cb          := SizeOf( lStartupInfo );
  lStartupInfo.dwFlags     := STARTF_USESHOWWINDOW;
  lStartupInfo.wShowWindow := SW_HIDE;

  if CreateProcess( NIL, PChar( lCommandLine ), NIL, NIL, false, CREATE_NO_WINDOW, NIL, NIL, lStartupInfo, lProcessInfo ) then
    begin
      WaitForSingleObject( lProcessInfo.hProcess, 30000 );
      GetExitCodeProcess( lProcessInfo.hProcess, lExitCode );
      Result := Integer( lExitCode );
      CloseHandle( lProcessInfo.hProcess );
      CloseHandle( lProcessInfo.hThread );
    end;
end;

procedure TTestConfig.TestJSONConfig_ViaExe_RemoveLogBlocks;
// Testet das Entfernen von Log-Bloecken via StrEditor.exe --config
// Alle Zeilennummern beziehen sich auf den ORIGINAL-Zustand!
const
  cMethodWithLogs =
    'procedure Test;'#13#10 +                    // 1
    'begin'#13#10 +                              // 2
    '  {$IFDEF FRANK_LOG}'#13#10 +               // 3
    '  TLog.Debug( ''Start'' );'#13#10 +         // 4
    '  {$ENDIF}'#13#10 +                         // 5
    '  if Node = NIL then'#13#10 +               // 6
    '    begin'#13#10 +                          // 7
    '      {$IFDEF FRANK_LOG}'#13#10 +           // 8
    '      TLog.Debug( ''NIL'' );'#13#10 +       // 9
    '      {$ENDIF}'#13#10 +                     // 10
    '      Exit;'#13#10 +                        // 11
    '    end;'#13#10 +                           // 12
    '  DoSomething;'#13#10 +                     // 13
    '  {$IFDEF FRANK_LOG}'#13#10 +               // 14
    '  TLog.Debug( ''End'' );'#13#10 +           // 15
    '  {$ENDIF}'#13#10 +                         // 16
    'end;';                                      // 17
Var
  lJSON      : string;
  lExitCode  : Integer;
  lContent   : string;
  lBytes     : TBytes;
  lEscPath   : string;
begin
  // 1. Erstelle Test-Datei als Windows-1252
  lBytes := TEncodingHelper.StringToWindows1252( cMethodWithLogs );
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  // 2. Erstelle JSON-Config - alle Zeilennummern = ORIGINAL
  lEscPath := StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] );
  lJSON :=
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    { "command": "delete-lines", "file": "' + lEscPath + '", "start-line": 3, "end-line": 5 },'#13#10 +
    '    { "command": "delete-lines", "file": "' + lEscPath + '", "start-line": 8, "end-line": 10 },'#13#10 +
    '    { "command": "delete-lines", "file": "' + lEscPath + '", "start-line": 14, "end-line": 16 }'#13#10 +
    '  ]'#13#10 +
    '}';
  TFile.WriteAllText( fTestConfigPath, lJSON, TEncoding.UTF8 );

  // 3. Fuehre StrEditor.exe aus
  lExitCode := RunStrEditor( fTestConfigPath );
  Assert.AreEqual( 0, lExitCode, 'StrEditor.exe should return 0' );

  // 4. Pruefe Ergebnis
  lBytes   := TFile.ReadAllBytes( fTestFilePath );
  lContent := TEncodingHelper.Windows1252ToString( lBytes );

  Assert.IsFalse( lContent.Contains( 'FRANK_LOG' ), 'All FRANK_LOG should be removed' );
  Assert.IsFalse( lContent.Contains( 'TLog.Debug' ), 'All TLog.Debug should be removed' );
  Assert.IsTrue( lContent.Contains( 'DoSomething' ), 'DoSomething should remain' );
  Assert.IsTrue( lContent.Contains( 'Exit' ), 'Exit should remain' );
end;

procedure TTestConfig.TestJSONConfig_ViaExe_AddLogBlocks;
// Testet das Einfuegen von Log-Bloecken via StrEditor.exe --config
// Alle Zeilennummern beziehen sich auf den ORIGINAL-Zustand (8 Zeilen)!
const
  cMethodWithoutLogs =
    'procedure Test;'#13#10 +                    // 1
    'begin'#13#10 +                              // 2
    '  if Node = NIL then'#13#10 +               // 3
    '    begin'#13#10 +                          // 4
    '      Exit;'#13#10 +                        // 5
    '    end;'#13#10 +                           // 6
    '  DoSomething;'#13#10 +                     // 7
    'end;';                                      // 8
Var
  lJSON      : string;
  lExitCode  : Integer;
  lContent   : string;
  lBytes     : TBytes;
  lEscPath   : string;
begin
  // 1. Erstelle Test-Datei als Windows-1252
  lBytes := TEncodingHelper.StringToWindows1252( cMethodWithoutLogs );
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  // 2. Erstelle JSON-Config - alle Zeilennummern = ORIGINAL (8 Zeilen)
  lEscPath := StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] );
  lJSON :=
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    { "command": "insert-after", "file": "' + lEscPath + '", "insert-after-line": 2, ' +
         '"text-lines": ["  {$IFDEF FRANK_LOG}", "  TLog.Debug( ''Start'' );", "  {$ENDIF}"] },'#13#10 +
    '    { "command": "insert-before", "file": "' + lEscPath + '", "insert-before-line": 5, ' +
         '"text-lines": ["      {$IFDEF FRANK_LOG}", "      TLog.Debug( ''NIL'' );", "      {$ENDIF}"] },'#13#10 +
    '    { "command": "insert-before", "file": "' + lEscPath + '", "insert-before-line": 8, ' +
         '"text-lines": ["  {$IFDEF FRANK_LOG}", "  TLog.Debug( ''End'' );", "  {$ENDIF}"] }'#13#10 +
    '  ]'#13#10 +
    '}';
  TFile.WriteAllText( fTestConfigPath, lJSON, TEncoding.UTF8 );

  // 3. Fuehre StrEditor.exe aus
  lExitCode := RunStrEditor( fTestConfigPath );
  Assert.AreEqual( 0, lExitCode, 'StrEditor.exe should return 0' );

  // 4. Pruefe Ergebnis
  lBytes   := TFile.ReadAllBytes( fTestFilePath );
  lContent := TEncodingHelper.Windows1252ToString( lBytes );

  Assert.IsTrue( lContent.Contains( 'FRANK_LOG' ), 'FRANK_LOG should be present' );
  Assert.IsTrue( lContent.Contains( 'TLog.Debug( ''Start'' )' ), 'Start log should be present' );
  Assert.IsTrue( lContent.Contains( 'TLog.Debug( ''NIL'' )' ), 'NIL log should be present' );
  Assert.IsTrue( lContent.Contains( 'TLog.Debug( ''End'' )' ), 'End log should be present' );
end;

procedure TTestConfig.TestJSONConfig_ViaExe_FullRoundtrip;
// Kompletter Roundtrip: Logs entfernen und wieder einfuegen
// Prueft ob das Ergebnis identisch mit dem Original ist
const
  cMethodWithLogs =
    'procedure Test;'#13#10 +
    'begin'#13#10 +
    '  {$IFDEF FRANK_LOG}'#13#10 +
    '  TLog.Debug( ''Start'' );'#13#10 +
    '  {$ENDIF}'#13#10 +
    '  if Node = NIL then'#13#10 +
    '    begin'#13#10 +
    '      {$IFDEF FRANK_LOG}'#13#10 +
    '      TLog.Debug( ''NIL'' );'#13#10 +
    '      {$ENDIF}'#13#10 +
    '      Exit;'#13#10 +
    '    end;'#13#10 +
    '  DoSomething;'#13#10 +
    '  {$IFDEF FRANK_LOG}'#13#10 +
    '  TLog.Debug( ''End'' );'#13#10 +
    '  {$ENDIF}'#13#10 +
    'end;';
Var
  lRemoveJSON   : string;
  lAddJSON      : string;
  lExitCode     : Integer;
  lOriginal     : TBytes;
  lAfterRemove  : TBytes;
  lAfterAdd     : TBytes;
  lEscPath      : string;
  lRemoveConfig : string;
  lAddConfig    : string;
  i             : Integer;
begin
  // Separate Config-Dateien fuer Remove und Add
  lRemoveConfig := TPath.Combine( TPath.GetTempPath, 'remove_logs_' + IntToStr( Random( 10000 ) ) + '.json' );
  lAddConfig    := TPath.Combine( TPath.GetTempPath, 'add_logs_' + IntToStr( Random( 10000 ) ) + '.json' );

  try
    // 1. Erstelle Original-Datei
    lOriginal := TEncodingHelper.StringToWindows1252( cMethodWithLogs );
    TFile.WriteAllBytes( fTestFilePath, lOriginal );

    lEscPath := StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] );

    // 2. SCHRITT 1: Logs entfernen
    lRemoveJSON :=
      '{'#13#10 +
      '  "operations": ['#13#10 +
      '    { "command": "delete-lines", "file": "' + lEscPath + '", "start-line": 3, "end-line": 5 },'#13#10 +
      '    { "command": "delete-lines", "file": "' + lEscPath + '", "start-line": 8, "end-line": 10 },'#13#10 +
      '    { "command": "delete-lines", "file": "' + lEscPath + '", "start-line": 14, "end-line": 16 }'#13#10 +
      '  ]'#13#10 +
      '}';
    TFile.WriteAllText( lRemoveConfig, lRemoveJSON, TEncoding.UTF8 );

    lExitCode := RunStrEditor( lRemoveConfig );
    Assert.AreEqual( 0, lExitCode, 'Remove logs should succeed' );

    lAfterRemove := TFile.ReadAllBytes( fTestFilePath );
    Assert.IsFalse( TEncodingHelper.Windows1252ToString( lAfterRemove ).Contains( 'FRANK_LOG' ),
      'After remove: No FRANK_LOG should be present' );

    // 3. SCHRITT 2: Logs wieder einfuegen (Zeilennummern = Zustand ohne Logs = 8 Zeilen)
    lAddJSON :=
      '{'#13#10 +
      '  "operations": ['#13#10 +
      '    { "command": "insert-after", "file": "' + lEscPath + '", "insert-after-line": 2, ' +
           '"text-lines": ["  {$IFDEF FRANK_LOG}", "  TLog.Debug( ''Start'' );", "  {$ENDIF}"] },'#13#10 +
      '    { "command": "insert-before", "file": "' + lEscPath + '", "insert-before-line": 5, ' +
           '"text-lines": ["      {$IFDEF FRANK_LOG}", "      TLog.Debug( ''NIL'' );", "      {$ENDIF}"] },'#13#10 +
      '    { "command": "insert-before", "file": "' + lEscPath + '", "insert-before-line": 8, ' +
           '"text-lines": ["  {$IFDEF FRANK_LOG}", "  TLog.Debug( ''End'' );", "  {$ENDIF}"] }'#13#10 +
      '  ]'#13#10 +
      '}';
    TFile.WriteAllText( lAddConfig, lAddJSON, TEncoding.UTF8 );

    lExitCode := RunStrEditor( lAddConfig );
    Assert.AreEqual( 0, lExitCode, 'Add logs should succeed' );

    // 4. Pruefe ob Ergebnis identisch mit Original ist
    lAfterAdd := TFile.ReadAllBytes( fTestFilePath );

    Assert.AreEqual( Length( lOriginal ), Length( lAfterAdd ),
      'After roundtrip: Length should match original' );

    for i := 0 to Length( lOriginal ) - 1 do
      Assert.AreEqual( lOriginal[ i ], lAfterAdd[ i ],
        'After roundtrip: Byte ' + IntToStr( i ) + ' should match original' );

  finally
    if FileExists( lRemoveConfig ) then
      System.SysUtils.DeleteFile( lRemoveConfig );

    if FileExists( lAddConfig ) then
      System.SysUtils.DeleteFile( lAddConfig );
  end;
end;

end.

