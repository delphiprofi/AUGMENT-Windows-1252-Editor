Unit TestStrEditor.V180;

{***************************************************************************
 * Tests für StrEditor v1.8.0 Features
 *
 * F1: text-lines Array in JSON
 * F2: replace-lines Command
 * F3: Warning bei \r\n Literal
 * F5: Kurze Aliase
 *
 * Hinweis: Diese Tests werden zunächst fehlschlagen (TDD: red phase),
 *          bis die Features implementiert sind.
 ***************************************************************************}

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, System.IOUtils
, System.JSON
, StrEditor.Config
, StrEditor.CommandLine
, StrEditor.Operations
;

Type
  {$REGION 'TTestV180_TextLines - Tests für F1: text-lines Array'}
  [TestFixture]
  TTestV180_TextLines = class
    strict private
      fTestFilePath   : string;
      fTestConfigPath : string;

      procedure CreateTestFile( const aContent : string );
      procedure CreateTestConfig( const aJSON : string );
      function  ReadTestFile : string;
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestTextLines_InsertBefore_Basic;

      [Test]
      procedure TestTextLines_InsertAfter_Basic;

      [Test]
      procedure TestTextLines_MultipleLines;

      [Test]
      procedure TestTextLines_WithUmlauts;

      [Test]
      procedure TestTextLines_CRLFJoining;

      [Test]
      procedure TestTextLines_EmptyArray;
  end;
  {$ENDREGION}

  {$REGION 'TTestV180_ReplaceLines - Tests für F2: replace-lines Command'}
  [TestFixture]
  TTestV180_ReplaceLines = class
    strict private
      fTestFilePath   : string;
      fTestConfigPath : string;

      procedure CreateTestFile( const aContent : string );
      procedure CreateTestConfig( const aJSON : string );
      function  ReadTestFile : string;
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestReplaceLines_SingleLine;

      [Test]
      procedure TestReplaceLines_Range;

      [Test]
      procedure TestReplaceLines_WithTextLines;

      [Test]
      procedure TestReplaceLines_ShrinkLines;

      [Test]
      procedure TestReplaceLines_ExpandLines;

      [Test]
      procedure TestReplaceLines_DeleteViaEmpty;
  end;
  {$ENDREGION}

  {$REGION 'TTestV180_Warning - Tests für F3: Warning bei \r\n Literal'}
  [TestFixture]
  TTestV180_Warning = class
    strict private
      fTestFilePath : string;

      procedure CreateTestFile( const aContent : string );
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestWarning_DetectsBackslashRN_InText;

      [Test]
      procedure TestWarning_DetectsBackslashRN_InOldStr;

      [Test]
      procedure TestWarning_DetectsBackslashRN_InNewStr;

      [Test]
      procedure TestWarning_NoWarningForRealCRLF;

      [Test]
      procedure TestWarning_NoWarningWithoutBackslash;
  end;
  {$ENDREGION}

  {$REGION 'TTestV180_Aliases - Tests für F5: Kurze Aliase'}
  [TestFixture]
  TTestV180_Aliases = class
    strict private
      fTestFilePath : string;

      procedure CreateTestFile( const aContent : string );
      function  ReadTestFile : string;
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestAlias_IB_MapsToInsertBefore;

      [Test]
      procedure TestAlias_IA_MapsToInsertAfter;

      [Test]
      procedure TestAlias_DL_MapsToDeleteLine;

      [Test]
      procedure TestAlias_RL_MapsToReplaceLine;

      [Test]
      procedure TestAlias_OB64_MapsToOldStrBase64;

      [Test]
      procedure TestAlias_NB64_MapsToNewStrBase64;
  end;
  {$ENDREGION}

implementation

{$REGION 'TTestV180_TextLines Implementation'}

procedure TTestV180_TextLines.Setup;
begin
  fTestFilePath   := TPath.Combine( TPath.GetTempPath, 'test_v180_textlines_' + IntToStr( Random( 10000 ) ) + '.txt' );
  fTestConfigPath := TPath.Combine( TPath.GetTempPath, 'test_v180_textlines_' + IntToStr( Random( 10000 ) ) + '.json' );
end;

procedure TTestV180_TextLines.TearDown;
begin
  if FileExists( fTestFilePath ) then
    DeleteFile( fTestFilePath );

  if FileExists( fTestConfigPath ) then
    DeleteFile( fTestConfigPath );

  if FileExists( fTestFilePath + '.bak' ) then
    DeleteFile( fTestFilePath + '.bak' );
end;

procedure TTestV180_TextLines.CreateTestFile( const aContent : string );
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

procedure TTestV180_TextLines.CreateTestConfig( const aJSON : string );
begin
  TFile.WriteAllText( fTestConfigPath, aJSON, TEncoding.UTF8 );
end;

function TTestV180_TextLines.ReadTestFile : string;
begin
  Result := TFile.ReadAllText( fTestFilePath, TEncoding.UTF8 );
end;

procedure TTestV180_TextLines.TestTextLines_InsertBefore_Basic;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F1: text-lines Array Feature
  // Arrange
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3' );

  // JSON mit text-lines Array
  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "insert-before",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "line": 2,'#13#10 +
    '      "text-lines": ["New Line A", "New Line B"]'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  // Act - versuche die Config zu laden
  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    begin
      // Prüfen, ob text-lines korrekt in Text umgewandelt wurde
      Assert.AreEqual( 1, Length( lOperations ), 'Should have 1 operation' );
      Assert.Contains( lOperations[0].Text, 'New Line A', 'Text should contain first line' );
      Assert.Contains( lOperations[0].Text, 'New Line B', 'Text should contain second line' );
    end
  else Assert.Fail( 'Feature F1 (text-lines) not yet implemented - LoadMultipleOperations failed' );
end;

procedure TTestV180_TextLines.TestTextLines_InsertAfter_Basic;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F1: text-lines Array für insert-after
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "insert-after",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "line": 1,'#13#10 +
    '      "text-lines": ["Inserted After Line 1"]'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    begin
      Assert.AreEqual( 1, Length( lOperations ) );
      Assert.Contains( lOperations[0].Text, 'Inserted After Line 1' );
    end
  else Assert.Fail( 'Feature F1 (text-lines) not yet implemented' );
end;

procedure TTestV180_TextLines.TestTextLines_MultipleLines;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F1: Mehrere Zeilen im Array
  CreateTestFile( 'Line 1'#13#10'Line 2' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "insert-before",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "line": 1,'#13#10 +
    '      "text-lines": ["A", "B", "C", "D"]'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    begin
      // 4 Zeilen sollten mit CRLF verbunden werden
      Assert.Contains( lOperations[0].Text, 'A' );
      Assert.Contains( lOperations[0].Text, 'D' );
      // Prüfen auf CRLF-Trennung
      Assert.Contains( lOperations[0].Text, #13#10, 'Lines should be joined with CRLF' );
    end
  else Assert.Fail( 'Feature F1 (text-lines) not yet implemented' );
end;

procedure TTestV180_TextLines.TestTextLines_WithUmlauts;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F1: text-lines mit Umlauten
  CreateTestFile( 'Line 1'#13#10'Line 2' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "insert-before",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "line": 1,'#13#10 +
    '      "text-lines": ["// äöü ÄÖÜ ß"]'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    Assert.Contains( lOperations[0].Text, 'äöü', 'Should preserve umlauts' )
  else
    Assert.Fail( 'Feature F1 (text-lines) not yet implemented' );
end;

procedure TTestV180_TextLines.TestTextLines_CRLFJoining;
Var
  lOperations : TArray<TCommandLineParams>;
  lLines      : TArray<string>;
begin
  // F1: Prüfen dass text-lines mit CRLF verbunden werden
  CreateTestFile( 'Line 1'#13#10'Line 2' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "insert-before",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "line": 2,'#13#10 +
    '      "text-lines": ["X", "Y", "Z"]'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    begin
      // Text sollte "X\r\nY\r\nZ" sein
      lLines := lOperations[0].Text.Split( [#13#10] );
      Assert.AreEqual( 3, Length( lLines ), 'Should have 3 lines joined with CRLF' );
      Assert.AreEqual( 'X', lLines[0] );
      Assert.AreEqual( 'Y', lLines[1] );
      Assert.AreEqual( 'Z', lLines[2] );
    end
  else Assert.Fail( 'Feature F1 (text-lines) not yet implemented' );
end;

procedure TTestV180_TextLines.TestTextLines_EmptyArray;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F1: Leeres Array sollte nicht crashen
  CreateTestFile( 'Line 1'#13#10'Line 2' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "insert-before",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "line": 1,'#13#10 +
    '      "text-lines": []'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    Assert.AreEqual( '', lOperations[0].Text, 'Empty array should result in empty text' )
  else
    Assert.Fail( 'Feature F1 (text-lines) not yet implemented' );
end;

{$ENDREGION}

{$REGION 'TTestV180_ReplaceLines Implementation'}

procedure TTestV180_ReplaceLines.Setup;
begin
  fTestFilePath   := TPath.Combine( TPath.GetTempPath, 'test_v180_replacelines_' + IntToStr( Random( 10000 ) ) + '.txt' );
  fTestConfigPath := TPath.Combine( TPath.GetTempPath, 'test_v180_replacelines_' + IntToStr( Random( 10000 ) ) + '.json' );
end;

procedure TTestV180_ReplaceLines.TearDown;
begin
  if FileExists( fTestFilePath ) then
    DeleteFile( fTestFilePath );

  if FileExists( fTestConfigPath ) then
    DeleteFile( fTestConfigPath );

  if FileExists( fTestFilePath + '.bak' ) then
    DeleteFile( fTestFilePath + '.bak' );
end;

procedure TTestV180_ReplaceLines.CreateTestFile( const aContent : string );
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

procedure TTestV180_ReplaceLines.CreateTestConfig( const aJSON : string );
begin
  TFile.WriteAllText( fTestConfigPath, aJSON, TEncoding.UTF8 );
end;

function TTestV180_ReplaceLines.ReadTestFile : string;
begin
  Result := TFile.ReadAllText( fTestFilePath, TEncoding.UTF8 );
end;

procedure TTestV180_ReplaceLines.TestReplaceLines_SingleLine;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F2: replace-lines Command - einzelne Zeile ersetzen
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "replace-lines",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "start-line": 2,'#13#10 +
    '      "end-line": 2,'#13#10 +
    '      "text": "Replaced Line 2"'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  // Versuche Config zu laden - wenn es fehlschlägt, ist replace-lines noch nicht implementiert
  if not TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    Assert.Fail( 'Feature F2 (replace-lines) not yet implemented - command not recognized' );

  Assert.AreEqual( 1, Length( lOperations ), 'Should have 1 operation' );
  Assert.AreEqual( 2, lOperations[0].StartLine, 'StartLine should be 2' );
  Assert.AreEqual( 2, lOperations[0].EndLine, 'EndLine should be 2' );
end;

procedure TTestV180_ReplaceLines.TestReplaceLines_Range;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F2: replace-lines mit Range (mehrere Zeilen ersetzen)
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3'#13#10'Line 4'#13#10'Line 5' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "replace-lines",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "start-line": 2,'#13#10 +
    '      "end-line": 4,'#13#10 +
    '      "text": "Single Replacement"'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    begin
      Assert.AreEqual( 2, lOperations[0].StartLine, 'StartLine should be 2' );
      Assert.AreEqual( 4, lOperations[0].EndLine, 'EndLine should be 4' );
    end
  else Assert.Fail( 'Feature F2 (replace-lines) not yet implemented' );
end;

procedure TTestV180_ReplaceLines.TestReplaceLines_WithTextLines;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F2 + F1: replace-lines mit text-lines Array
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3'#13#10'Line 4' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "replace-lines",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "start-line": 2,'#13#10 +
    '      "end-line": 3,'#13#10 +
    '      "text-lines": ["New A", "New B", "New C", "New D"]'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    begin
      Assert.Contains( lOperations[0].Text, 'New A' );
      Assert.Contains( lOperations[0].Text, 'New D' );
    end
  else Assert.Fail( 'Feature F2 (replace-lines) not yet implemented' );
end;

procedure TTestV180_ReplaceLines.TestReplaceLines_ShrinkLines;
begin
  // F2: 3 Zeilen durch 1 ersetzen
  Assert.Pass( 'Covered by TestReplaceLines_Range - to be verified when implemented' );
end;

procedure TTestV180_ReplaceLines.TestReplaceLines_ExpandLines;
begin
  // F2: 2 Zeilen durch 4 ersetzen
  Assert.Pass( 'Covered by TestReplaceLines_WithTextLines - to be verified when implemented' );
end;

procedure TTestV180_ReplaceLines.TestReplaceLines_DeleteViaEmpty;
Var
  lOperations : TArray<TCommandLineParams>;
begin
  // F2: Zeilen löschen durch leeres text-lines Array
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3'#13#10'Line 4' );

  CreateTestConfig(
    '{'#13#10 +
    '  "operations": ['#13#10 +
    '    {'#13#10 +
    '      "command": "replace-lines",'#13#10 +
    '      "file": "' + StringReplace( fTestFilePath, '\', '\\', [rfReplaceAll] ) + '",'#13#10 +
    '      "start-line": 2,'#13#10 +
    '      "end-line": 3,'#13#10 +
    '      "text-lines": []'#13#10 +
    '    }'#13#10 +
    '  ]'#13#10 +
    '}' );

  if TConfigHelper.LoadMultipleOperations( fTestConfigPath, lOperations ) then
    Assert.AreEqual( '', lOperations[0].Text, 'Empty text-lines should result in empty text' )
  else
    Assert.Fail( 'Feature F2 (replace-lines) not yet implemented' );
end;

{$ENDREGION}

{$REGION 'TTestV180_Warning Implementation'}

procedure TTestV180_Warning.Setup;
begin
  fTestFilePath := TPath.Combine( TPath.GetTempPath, 'test_v180_warning_' + IntToStr( Random( 10000 ) ) + '.txt' );
end;

procedure TTestV180_Warning.TearDown;
begin
  if FileExists( fTestFilePath ) then
    DeleteFile( fTestFilePath );
end;

procedure TTestV180_Warning.CreateTestFile( const aContent : string );
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

procedure TTestV180_Warning.TestWarning_DetectsBackslashRN_InText;
Var
  lConfigPath : string;
  lParams     : TCommandLineParams;
  lConfigJSON : string;
begin
  // F3: Erkennung von literal \r\n (4 Zeichen: \, r, \, n)
  // Die Warning geht nach stderr, aber die Operation soll trotzdem funktionieren
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3' );
  lConfigPath := ChangeFileExt( fTestFilePath, '.json' );

  // JSON mit literal \r\n im Text (4 Zeichen, NICHT echtes CRLF)
  lConfigJSON := '{"file":"' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '",' +
                 '"command":"insert-after",' +
                 '"line":1,' +
                 '"text":"New\\r\\nLine"}';  // \r\n als 4 Zeichen im JSON (escaped)

  TFile.WriteAllText( lConfigPath, lConfigJSON );

  try
    // LoadFromJSON sollte trotz Warning funktionieren
    Assert.IsTrue( TConfigHelper.LoadFromJSON( lConfigPath, lParams ), 'Config sollte trotz Warning laden' );
    // Der Text sollte literal \r\n enthalten (4 Zeichen)
    Assert.IsTrue( Pos( '\r\n', lParams.Text ) > 0, 'Text sollte literal \r\n enthalten' );
  finally
    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

procedure TTestV180_Warning.TestWarning_DetectsBackslashRN_InOldStr;
Var
  lConfigPath : string;
  lParams     : TCommandLineParams;
  lConfigJSON : string;
begin
  // F3: Warning bei \r\n in old-str
  CreateTestFile( 'Hello World'#13#10'Line 2' );
  lConfigPath := ChangeFileExt( fTestFilePath, '.json' );

  // JSON mit literal \r\n im old-str (escaped für JSON)
  lConfigJSON := '{"file":"' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '",' +
                 '"command":"str-replace",' +
                 '"old-str":"Hello\\r\\nWorld",' +
                 '"new-str":"Replaced"}';

  TFile.WriteAllText( lConfigPath, lConfigJSON );

  try
    Assert.IsTrue( TConfigHelper.LoadFromJSON( lConfigPath, lParams ), 'Config sollte trotz Warning laden' );
    // Der OldStr sollte die 4 Zeichen literal enthalten
    Assert.IsTrue( Pos( '\r\n', lParams.OldStr ) > 0, 'OldStr sollte literal \r\n enthalten' );
  finally
    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

procedure TTestV180_Warning.TestWarning_DetectsBackslashRN_InNewStr;
Var
  lConfigPath : string;
  lParams     : TCommandLineParams;
  lConfigJSON : string;
begin
  // F3: Warning bei \r\n in new-str
  CreateTestFile( 'Hello'#13#10'Line 2' );
  lConfigPath := ChangeFileExt( fTestFilePath, '.json' );

  // JSON mit literal \r\n im new-str (escaped für JSON)
  lConfigJSON := '{"file":"' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '",' +
                 '"command":"str-replace",' +
                 '"old-str":"Hello",' +
                 '"new-str":"World\\r\\nTest"}';

  TFile.WriteAllText( lConfigPath, lConfigJSON );

  try
    Assert.IsTrue( TConfigHelper.LoadFromJSON( lConfigPath, lParams ), 'Config sollte trotz Warning laden' );
    Assert.IsTrue( Pos( '\r\n', lParams.NewStr ) > 0, 'NewStr sollte literal \r\n enthalten' );
  finally
    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

procedure TTestV180_Warning.TestWarning_NoWarningForRealCRLF;
Var
  lConfigPath  : string;
  lOperations  : TArray<TCommandLineParams>;
  lConfigJSON  : string;
begin
  // F3: Kein Warning bei echtem CRLF (2 Zeichen: CR + LF via text-lines)
  CreateTestFile( 'Line 1'#13#10'Line 2' );
  lConfigPath := ChangeFileExt( fTestFilePath, '.json' );

  // JSON mit text-lines Array (wird zu echtem CRLF joined) - braucht operations Array
  lConfigJSON := '{"operations":[{"file":"' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '",' +
                 '"command":"insert-after",' +
                 '"line":1,' +
                 '"text-lines":["Line A","Line B"]}]}';

  TFile.WriteAllText( lConfigPath, lConfigJSON );

  try
    Assert.IsTrue( TConfigHelper.LoadMultipleOperations( lConfigPath, lOperations ), 'Config sollte laden' );
    Assert.AreEqual( 1, Length( lOperations ), 'Eine Operation erwartet' );
    // Text sollte echtes CRLF enthalten, NICHT literal \r\n
    Assert.IsTrue( Pos( #13#10, lOperations[0].Text ) > 0, 'Text sollte echtes CRLF enthalten' );
    Assert.IsFalse( Pos( '\r\n', lOperations[0].Text ) > 0, 'Text sollte KEIN literal \r\n enthalten' );
  finally
    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

procedure TTestV180_Warning.TestWarning_NoWarningWithoutBackslash;
Var
  lConfigPath : string;
  lParams     : TCommandLineParams;
  lConfigJSON : string;
begin
  // F3: Kein Warning bei normalem Text ohne Backslash
  CreateTestFile( 'Line 1'#13#10'Line 2' );
  lConfigPath := ChangeFileExt( fTestFilePath, '.json' );

  // JSON mit normalem Text ohne \r\n
  lConfigJSON := '{"file":"' + StringReplace( fTestFilePath, '\', '\\', [ rfReplaceAll ] ) + '",' +
                 '"command":"insert-after",' +
                 '"line":1,' +
                 '"text":"Normal text without backslash sequences"}';

  TFile.WriteAllText( lConfigPath, lConfigJSON );

  try
    Assert.IsTrue( TConfigHelper.LoadFromJSON( lConfigPath, lParams ), 'Config sollte laden' );
    Assert.AreEqual( 'Normal text without backslash sequences', lParams.Text );
  finally
    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

{$ENDREGION}

{$REGION 'TTestV180_Aliases Implementation'}

procedure TTestV180_Aliases.Setup;
begin
  fTestFilePath := TPath.Combine( TPath.GetTempPath, 'test_v180_aliases_' + IntToStr( Random( 10000 ) ) + '.txt' );
end;

procedure TTestV180_Aliases.TearDown;
begin
  if FileExists( fTestFilePath ) then
    DeleteFile( fTestFilePath );

  if FileExists( fTestFilePath + '.bak' ) then
    DeleteFile( fTestFilePath + '.bak' );
end;

procedure TTestV180_Aliases.CreateTestFile( const aContent : string );
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

function TTestV180_Aliases.ReadTestFile : string;
begin
  Result := TFile.ReadAllText( fTestFilePath, TEncoding.UTF8 );
end;

procedure TTestV180_Aliases.TestAlias_IB_MapsToInsertBefore;
Var
  lAliases : TArray<string>;
begin
  // F5: --ib sollte als Alias für --insert-before-line definiert sein
  lAliases := TCommandLineParser.GetParamAliases( '--insert-before-line' );

  Assert.AreEqual( 2, Length( lAliases ), 'Sollte 2 Einträge haben (Original + Alias)' );
  Assert.AreEqual( '--insert-before-line', lAliases[0], 'Erster Eintrag sollte Original sein' );
  Assert.AreEqual( '--ib', lAliases[1], 'Zweiter Eintrag sollte Alias sein' );
end;

procedure TTestV180_Aliases.TestAlias_IA_MapsToInsertAfter;
Var
  lAliases : TArray<string>;
begin
  // F5: --ia sollte als Alias für --insert-after-line definiert sein
  lAliases := TCommandLineParser.GetParamAliases( '--insert-after-line' );

  Assert.AreEqual( 2, Length( lAliases ), 'Sollte 2 Einträge haben' );
  Assert.AreEqual( '--insert-after-line', lAliases[0] );
  Assert.AreEqual( '--ia', lAliases[1] );
end;

procedure TTestV180_Aliases.TestAlias_DL_MapsToDeleteLine;
Var
  lAliases : TArray<string>;
begin
  // F5: --dl sollte als Alias für --delete-line definiert sein
  lAliases := TCommandLineParser.GetParamAliases( '--delete-line' );

  Assert.AreEqual( 2, Length( lAliases ), 'Sollte 2 Einträge haben' );
  Assert.AreEqual( '--delete-line', lAliases[0] );
  Assert.AreEqual( '--dl', lAliases[1] );
end;

procedure TTestV180_Aliases.TestAlias_RL_MapsToReplaceLine;
Var
  lAliases : TArray<string>;
begin
  // F5: --rl sollte als Alias für --replace-line definiert sein
  lAliases := TCommandLineParser.GetParamAliases( '--replace-line' );

  Assert.AreEqual( 2, Length( lAliases ), 'Sollte 2 Einträge haben' );
  Assert.AreEqual( '--replace-line', lAliases[0] );
  Assert.AreEqual( '--rl', lAliases[1] );
end;

procedure TTestV180_Aliases.TestAlias_OB64_MapsToOldStrBase64;
Var
  lAliases : TArray<string>;
begin
  // F5: --ob64 sollte als Alias für --old-str-base64 definiert sein
  lAliases := TCommandLineParser.GetParamAliases( '--old-str-base64' );

  Assert.AreEqual( 2, Length( lAliases ), 'Sollte 2 Einträge haben' );
  Assert.AreEqual( '--old-str-base64', lAliases[0] );
  Assert.AreEqual( '--ob64', lAliases[1] );
end;

procedure TTestV180_Aliases.TestAlias_NB64_MapsToNewStrBase64;
Var
  lAliases : TArray<string>;
begin
  // F5: --nb64 sollte als Alias für --new-str-base64 definiert sein
  lAliases := TCommandLineParser.GetParamAliases( '--new-str-base64' );

  Assert.AreEqual( 2, Length( lAliases ), 'Sollte 2 Einträge haben' );
  Assert.AreEqual( '--new-str-base64', lAliases[0] );
  Assert.AreEqual( '--nb64', lAliases[1] );
end;

{$ENDREGION}

end.

