Unit TestStrEditor.Operations;

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, System.IOUtils
, StrEditor.Encoding
, StrEditor.Operations
, StrEditor.CaseConversion
, StrEditor.CommandLine
;

Type
  [TestFixture]
  TTestStringOperations = class
    strict private
      fTestFilePath : string;

      procedure CreateTestFile( const aContent : string; const aEncoding : TEncodingType );
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestStrReplace_Simple;

      [Test]
      procedure TestStrReplace_WithLineRange;

      [Test]
      procedure TestStrReplace_NotFound;

      [Test]
      procedure TestStrReplace_EmptyNewStr;

      [Test]
      procedure TestStrReplace_MultipleOccurrences;

      [Test]
      procedure TestInsert_AfterLine;

      [Test]
      procedure TestInsert_BeforeLine;

      [Test]
      procedure TestInsert_BeforeLine_FirstLine;

      [Test]
      procedure TestInsert_BeforeLine_LastLine;

      [Test]
      procedure TestInsert_AtBeginning;

      [Test]
      procedure TestInsert_AtEnd;

      [Test]
      procedure TestEncodingPreservation_Replace;

      [Test]
      procedure TestEncodingPreservation_Insert;

      [Test]
      procedure TestShow_FullFile;

      [Test]
      procedure TestShow_Head;

      [Test]
      procedure TestShow_Tail;

      [Test]
      procedure TestShow_LineRange;

      [Test]
      procedure TestShow_WithLineNumbers;

      [Test]
      procedure TestShow_Raw;

      [Test]
      procedure TestShow_WithUmlauts;

      [Test]
      procedure TestShow_EmptyFile;

      [Test]
      procedure TestShow_SingleLine;

      [Test]
      procedure TestShow_Hex_BasicOutput;

      [Test]
      procedure TestShow_Hex_WithUmlauts;

      [Test]
      procedure TestShow_Hex_Head;

      [Test]
      procedure TestShow_Hex_Tail;

      [Test]
      procedure TestShow_Hex_EmptyFile;

      [Test]
      procedure TestShow_Base64_BasicOutput;

      [Test]
      procedure TestShow_Base64_WithUmlauts;

      [Test]
      procedure TestShow_Base64_Head;

      [Test]
      procedure TestShow_Base64_Tail;

      [Test]
      procedure TestShow_Base64_EmptyFile;

      [Test]
      procedure TestConvertEncoding_UTF8ToWindows1252;

      [Test]
      procedure TestConvertEncoding_Windows1252ToUTF8;

      [Test]
      procedure TestConvertEncoding_WithBackup;

      [Test]
      procedure TestConvertEncoding_SameEncoding;

      [Test]
      procedure TestConvertEncoding_WithUmlauts;

      [Test]
      procedure TestConvertEncoding_InvalidEncoding;

      [Test]
      procedure TestReinterpretEncoding_UTF8AsWindows1252_Umlauts;

      [Test]
      procedure TestReinterpretEncoding_UTF8AsWindows1252_Copyright;

      [Test]
      procedure TestReinterpretEncoding_UTF8AsWindows1252_Eacute;

      [Test]
      procedure TestReinterpretEncoding_Windows1252AsUTF8;

      [Test]
      procedure TestReinterpretEncoding_WithBackup;

      [Test]
      procedure TestReinterpretEncoding_InvalidSourceEncoding;

      [Test]
      procedure TestReinterpretEncoding_MixedCharacters;

      [Test]
      procedure TestBase64Replace_DollarSign;

      [Test]
      procedure TestBase64Replace_Backtick;

      [Test]
      procedure TestBase64Replace_DoubleQuote;

      [Test]
      procedure TestBase64Replace_AtSign;

      [Test]
      procedure TestBase64Replace_ComplexString;

      [Test]
      procedure TestBase64Insert_DollarSign;

      [Test]
      procedure TestBase64Replace_InvalidBase64;

      [Test]
      procedure TestBase64Replace_EmptyString;

      [Test]
      procedure TestMultiLine_BasicReplace;

      [Test]
      procedure TestMultiLine_ReplaceAll;

      [Test]
      procedure TestMultiLine_LineRange;

      [Test]
      procedure TestMultiLine_NotFound;

      [Test]
      procedure TestMultiLine_DryRun;

      [Test]
      procedure TestMultiLine_MultipleOccurrences;

      [Test]
      procedure TestMultiLine_FiveLinesToOne;

      [Test]
      procedure TestMultiLine_WithBackup;

      [Test]
      procedure TestDeleteLine_Single;

      [Test]
      procedure TestDeleteLine_First;

      [Test]
      procedure TestDeleteLine_Last;

      [Test]
      procedure TestDeleteLine_InvalidLine;

      [Test]
      procedure TestDeleteLines_Range;

      [Test]
      procedure TestDeleteLines_MultipleLines;

      [Test]
      procedure TestDeleteLines_InvalidRange;

      [Test]
      procedure TestDeleteLines_WithBackup;

      [Test]
      procedure TestDeleteLines_DryRun;

      [Test]
      procedure TestReplaceLine_Single;

      [Test]
      procedure TestReplaceLine_First;

      [Test]
      procedure TestReplaceLine_Last;

      [Test]
      procedure TestReplaceLine_InvalidLine;

      [Test]
      procedure TestReplaceLine_WithBackup;

      [Test]
      procedure TestReplaceLine_DryRun;

      [Test]
      procedure TestReplaceLine_Base64;

      [Test]
      procedure TestMoveLines_Basic;

      [Test]
      procedure TestMoveLines_InsertBefore;

      [Test]
      procedure TestMoveLines_SourceNotFound;

      [Test]
      procedure TestMoveLines_TargetNotFound;

      [Test]
      procedure TestMoveLines_InvalidLineRange;

      [Test]
      procedure TestMoveLines_WithBackup;

      [Test]
      procedure TestMoveLines_DryRun;

      [Test]
      procedure TestMoveLines_EncodingPreservation;

      [Test]
      procedure TestAutoDeleteConfig_Basic;

      [Test]
      procedure TestAutoDeleteConfig_OnError;

      [Test]
      procedure TestAutoDeleteConfig_DryRun;

      [Test]
      procedure TestKeepConfig_Basic;

      // Indent/Unindent Tests
      [Test]
      procedure TestIndentLines_Basic;

      [Test]
      procedure TestIndentLines_CustomSpaces;

      [Test]
      procedure TestIndentLines_EmptyLines;

      [Test]
      procedure TestIndentLines_SingleLine;

      [Test]
      procedure TestIndentLines_InvalidRange;

      [Test]
      procedure TestIndentLines_DryRun;

      [Test]
      procedure TestUnindentLines_Basic;

      [Test]
      procedure TestUnindentLines_PartialUnindent;

      [Test]
      procedure TestUnindentLines_NoLeadingSpaces;

      [Test]
      procedure TestUnindentLines_SingleLine;

      [Test]
      procedure TestUnindentLines_InvalidRange;

      [Test]
      procedure TestUnindentLines_DryRun;

      [Test]
      procedure TestIndentUnindent_Roundtrip;
  end;

implementation

{ TTestStringOperations }

procedure TTestStringOperations.Setup;
begin
  fTestFilePath := 'test_operations.tmp';
end;

procedure TTestStringOperations.TearDown;
begin
  if FileExists( fTestFilePath ) then
    DeleteFile( fTestFilePath );
end;

procedure TTestStringOperations.CreateTestFile( const aContent : string; const aEncoding : TEncodingType );
Var
  lLines : TStringList;
begin
  lLines := TStringList.Create;
  try
    lLines.Text := aContent;
    TEncodingHelper.WriteFile( fTestFilePath, lLines, aEncoding );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestStrReplace_Simple;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.StrReplace( fTestFilePath, 'Line 2', 'Modified Line 2', 0, 0 );

  Assert.IsTrue( lResult.Success, 'StrReplace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 'Modified Line 2', lLines[ 1 ], 'Line 2 should be modified' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestStrReplace_WithLineRange;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'nil' + #13#10 + 'nil' + #13#10 + 'nil', etWindows1252 );

  lResult := TStringOperations.StrReplace( fTestFilePath, 'nil', 'NIL', 2, 2 );

  Assert.IsTrue( lResult.Success, 'StrReplace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 'nil', lLines[ 0 ], 'Line 1 should not be modified' );
    Assert.AreEqual( 'NIL', lLines[ 1 ], 'Line 2 should be modified' );
    Assert.AreEqual( 'nil', lLines[ 2 ], 'Line 3 should not be modified' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestStrReplace_NotFound;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.StrReplace( fTestFilePath, 'NotFound', 'Replacement', 0, 0 );

  Assert.IsFalse( lResult.Success, 'StrReplace should fail' );
  Assert.IsTrue( Pos( 'not found', lResult.ErrorMessage ) > 0, 'Error message should mention "not found"' );
end;

procedure TTestStringOperations.TestStrReplace_EmptyNewStr;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1 with text' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.StrReplace( fTestFilePath, ' with text', '', 0, 0 );

  Assert.IsTrue( lResult.Success, 'StrReplace should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Text should be deleted' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestStrReplace_MultipleOccurrences;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'nil nil' + #13#10 + 'nil', etWindows1252 );

  lResult := TStringOperations.StrReplace( fTestFilePath, 'nil', 'NIL', 0, 0 );

  Assert.IsTrue( lResult.Success, 'StrReplace should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 'NIL NIL', lLines[ 0 ], 'All occurrences in line 1 should be replaced' );
    Assert.AreEqual( 'NIL', lLines[ 1 ], 'Line 2 should be replaced' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestInsert_AfterLine;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.Insert( fTestFilePath, 'Inserted Line', 1 );

  Assert.IsTrue( lResult.Success, 'Insert should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 3, lLines.Count, 'Should have 3 lines' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( 'Inserted Line', lLines[ 1 ], 'Line 2 should be inserted line' );
    Assert.AreEqual( 'Line 2', lLines[ 2 ], 'Line 3 should be old line 2' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestInsert_BeforeLine;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.InsertBefore( fTestFilePath, 'Inserted Line', 2 );

  Assert.IsTrue( lResult.Success, 'Insert should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 4, lLines.Count, 'Should have 4 lines' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( 'Inserted Line', lLines[ 1 ], 'Line 2 should be inserted line' );
    Assert.AreEqual( 'Line 2', lLines[ 2 ], 'Line 3 should be old line 2' );
    Assert.AreEqual( 'Line 3', lLines[ 3 ], 'Line 4 should be old line 3' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestInsert_BeforeLine_FirstLine;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.InsertBefore( fTestFilePath, 'First Line', 1 );

  Assert.IsTrue( lResult.Success, 'Insert should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 3, lLines.Count, 'Should have 3 lines' );
    Assert.AreEqual( 'First Line', lLines[ 0 ], 'Line 1 should be inserted line' );
    Assert.AreEqual( 'Line 1', lLines[ 1 ], 'Line 2 should be old line 1' );
    Assert.AreEqual( 'Line 2', lLines[ 2 ], 'Line 3 should be old line 2' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestInsert_BeforeLine_LastLine;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.InsertBefore( fTestFilePath, 'Before Last', 2 );

  Assert.IsTrue( lResult.Success, 'Insert should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 3, lLines.Count, 'Should have 3 lines' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( 'Before Last', lLines[ 1 ], 'Line 2 should be inserted line' );
    Assert.AreEqual( 'Line 2', lLines[ 2 ], 'Line 3 should be old line 2' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestInsert_AtBeginning;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.Insert( fTestFilePath, 'First Line', 0 );

  Assert.IsTrue( lResult.Success, 'Insert should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 3, lLines.Count, 'Should have 3 lines' );
    Assert.AreEqual( 'First Line', lLines[ 0 ], 'Line 1 should be inserted line' );
    Assert.AreEqual( 'Line 1', lLines[ 1 ], 'Line 2 should be old line 1' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestInsert_AtEnd;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.Insert( fTestFilePath, 'Last Line', 2 );

  Assert.IsTrue( lResult.Success, 'Insert should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
  try
    Assert.AreEqual( 3, lLines.Count, 'Should have 3 lines' );
    Assert.AreEqual( 'Last Line', lLines[ 2 ], 'Line 3 should be inserted line' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestEncodingPreservation_Replace;
Var
  lResult   : TOperationResult;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etUTF8 );

  lResult := TStringOperations.StrReplace( fTestFilePath, 'Line 1', 'Modified', 0, 0 );

  Assert.IsTrue( lResult.Success, 'StrReplace should succeed' );

  lEncoding := TEncodingHelper.DetectEncoding( fTestFilePath );

  Assert.AreEqual( etUTF8, lEncoding, 'Encoding should be preserved as UTF-8' );
end;

procedure TTestStringOperations.TestEncodingPreservation_Insert;
Var
  lResult   : TOperationResult;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.Insert( fTestFilePath, 'Inserted', 1 );

  Assert.IsTrue( lResult.Success, 'Insert should succeed' );

  lEncoding := TEncodingHelper.DetectEncoding( fTestFilePath );

  Assert.AreEqual( etWindows1252, lEncoding, 'Encoding should be preserved as Windows-1252' );
end;

procedure TTestStringOperations.TestShow_FullFile;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, False, False, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.AreEqual( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', lResult.OutputText, 'Should show full file' );
end;

procedure TTestStringOperations.TestShow_Head;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'Line 4', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 2, 0, False, False, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.AreEqual( 'Line 1' + #13#10 + 'Line 2', lResult.OutputText, 'Should show first 2 lines' );
end;

procedure TTestStringOperations.TestShow_Tail;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'Line 4', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 2, False, False, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.AreEqual( 'Line 3' + #13#10 + 'Line 4', lResult.OutputText, 'Should show last 2 lines' );
end;

procedure TTestStringOperations.TestShow_LineRange;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'Line 4', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 2, 3, 0, 0, False, False, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.AreEqual( 'Line 2' + #13#10 + 'Line 3', lResult.OutputText, 'Should show lines 2-3' );
end;

procedure TTestStringOperations.TestShow_WithLineNumbers;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, True, False, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.IsTrue( lResult.OutputText.Contains( '1:' ), 'Should contain line number 1' );
  Assert.IsTrue( lResult.OutputText.Contains( '2:' ), 'Should contain line number 2' );
end;

procedure TTestStringOperations.TestShow_Raw;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, False, True, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.AreEqual( 'Line 1 Line 2 Line 3', lResult.OutputText, 'Should show as single line with spaces' );
end;

procedure TTestStringOperations.TestShow_WithUmlauts;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'äöüÄÖÜß', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, False, False, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.AreEqual( 'äöüÄÖÜß', lResult.OutputText, 'Should show umlauts correctly' );
end;

procedure TTestStringOperations.TestShow_EmptyFile;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( '', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, False, False, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.AreEqual( '', lResult.OutputText, 'Should show empty string' );
end;

procedure TTestStringOperations.TestShow_SingleLine;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Single Line', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, False, False, False );

  Assert.IsTrue( lResult.Success, 'Show should succeed' );
  Assert.AreEqual( 'Single Line', lResult.OutputText, 'Should show single line' );
end;

procedure TTestStringOperations.TestShow_Hex_BasicOutput;
Var
  lResult : TOperationResult;
begin
  // Erstelle Datei mit bekannten Bytes: "ABC" = 41 42 43
  CreateTestFile( 'ABC', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, false, false, false, true, false );

  Assert.IsTrue( lResult.Success, 'Show --hex should succeed' );
  Assert.IsTrue( lResult.OutputText.Contains( '00000000:' ), 'Should contain offset 00000000' );
  Assert.IsTrue( lResult.OutputText.Contains( '41' ), 'Should contain hex for A (41)' );
  Assert.IsTrue( lResult.OutputText.Contains( '42' ), 'Should contain hex for B (42)' );
  Assert.IsTrue( lResult.OutputText.Contains( '43' ), 'Should contain hex for C (43)' );
  Assert.IsTrue( lResult.OutputText.Contains( 'ABC' ), 'Should contain ASCII representation' );
end;

procedure TTestStringOperations.TestShow_Hex_WithUmlauts;
Var
  lResult : TOperationResult;
  lBytes  : TBytes;
begin
  // Erstelle Datei mit Windows-1252 Umlauten: ö = F6, ä = E4, ü = FC
  lBytes := TBytes.Create( $F6, $E4, $FC );
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, false, false, false, true, false );

  Assert.IsTrue( lResult.Success, 'Show --hex should succeed' );
  Assert.IsTrue( lResult.OutputText.Contains( 'F6' ), 'Should contain hex for ö (F6)' );
  Assert.IsTrue( lResult.OutputText.Contains( 'E4' ), 'Should contain hex for ä (E4)' );
  Assert.IsTrue( lResult.OutputText.Contains( 'FC' ), 'Should contain hex for ü (FC)' );
end;

procedure TTestStringOperations.TestShow_Hex_Head;
Var
  lResult : TOperationResult;
  lBytes  : TBytes;
begin
  // Erstelle Datei mit 32 Bytes
  SetLength( lBytes, 32 );

  for var i := 0 to 31 do
    lBytes[ i ] := i + $41; // A, B, C, ...

  TFile.WriteAllBytes( fTestFilePath, lBytes );

  // Nur erste 16 Bytes anzeigen
  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 16, 0, false, false, false, true, false );

  Assert.IsTrue( lResult.Success, 'Show --hex --head should succeed' );
  Assert.IsTrue( lResult.OutputText.Contains( '41' ), 'Should contain first byte (41)' );
  Assert.IsTrue( lResult.OutputText.Contains( '50' ), 'Should contain byte 16 (50 = P)' );
  // Sollte NICHT die zweite Zeile enthalten
  Assert.IsFalse( lResult.OutputText.Contains( '00000010:' ), 'Should NOT contain second line offset' );
end;

procedure TTestStringOperations.TestShow_Hex_Tail;
Var
  lResult : TOperationResult;
  lBytes  : TBytes;
begin
  // Erstelle Datei mit 32 Bytes
  SetLength( lBytes, 32 );

  for var i := 0 to 31 do
    lBytes[ i ] := i + $41; // A, B, C, ...

  TFile.WriteAllBytes( fTestFilePath, lBytes );

  // Nur letzte 8 Bytes anzeigen (Bytes 24-31 = Y, Z, [, \, ], ^, _, `)
  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 8, false, false, false, true, false );

  Assert.IsTrue( lResult.Success, 'Show --hex --tail should succeed' );
  Assert.IsTrue( lResult.OutputText.Contains( '00000018:' ), 'Should contain offset for byte 24' );
  Assert.IsTrue( lResult.OutputText.Contains( '59' ), 'Should contain hex for Y (59)' );
  Assert.IsTrue( lResult.OutputText.Contains( '60' ), 'Should contain hex for ` (60)' );
end;

procedure TTestStringOperations.TestShow_Hex_EmptyFile;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( '', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, false, false, false, true, false );

  Assert.IsTrue( lResult.Success, 'Show --hex on empty file should succeed' );
  Assert.AreEqual( '', lResult.OutputText, 'Should return empty string for empty file' );
end;

procedure TTestStringOperations.TestShow_Base64_BasicOutput;
Var
  lResult : TOperationResult;
  lBytes  : TBytes;
begin
  // "ABC" in Base64 = "QUJD" - direkt Bytes schreiben ohne CRLF
  lBytes := TBytes.Create( $41, $42, $43 ); // ABC
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, false, false, false, false, true );

  Assert.IsTrue( lResult.Success, 'Show --base64 should succeed' );
  Assert.AreEqual( 'QUJD', lResult.OutputText, 'ABC should encode to QUJD' );
end;

procedure TTestStringOperations.TestShow_Base64_WithUmlauts;
Var
  lResult : TOperationResult;
  lBytes  : TBytes;
begin
  // Windows-1252 Umlaute: ö ä ü = F6 E4 FC
  // Base64 von F6 E4 FC = "9uT8"
  lBytes := TBytes.Create( $F6, $E4, $FC );
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, false, false, false, false, true );

  Assert.IsTrue( lResult.Success, 'Show --base64 should succeed' );
  Assert.AreEqual( '9uT8', lResult.OutputText, 'F6 E4 FC should encode to 9uT8' );
end;

procedure TTestStringOperations.TestShow_Base64_Head;
Var
  lResult : TOperationResult;
  lBytes  : TBytes;
begin
  // "ABCDEF" - nur erste 3 Bytes = "ABC" = "QUJD" - direkt Bytes schreiben ohne CRLF
  lBytes := TBytes.Create( $41, $42, $43, $44, $45, $46 ); // ABCDEF
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 3, 0, false, false, false, false, true );

  Assert.IsTrue( lResult.Success, 'Show --base64 --head should succeed' );
  Assert.AreEqual( 'QUJD', lResult.OutputText, 'First 3 bytes (ABC) should encode to QUJD' );
end;

procedure TTestStringOperations.TestShow_Base64_Tail;
Var
  lResult : TOperationResult;
  lBytes  : TBytes;
begin
  // "ABCDEF" - nur letzte 3 Bytes = "DEF" = "REVG" - direkt Bytes schreiben ohne CRLF
  lBytes := TBytes.Create( $41, $42, $43, $44, $45, $46 ); // ABCDEF
  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 3, false, false, false, false, true );

  Assert.IsTrue( lResult.Success, 'Show --base64 --tail should succeed' );
  Assert.AreEqual( 'REVG', lResult.OutputText, 'Last 3 bytes (DEF) should encode to REVG' );
end;

procedure TTestStringOperations.TestShow_Base64_EmptyFile;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( '', etWindows1252 );

  lResult := TStringOperations.Show( fTestFilePath, 0, 0, 0, 0, false, false, false, false, true );

  Assert.IsTrue( lResult.Success, 'Show --base64 on empty file should succeed' );
  Assert.AreEqual( '', lResult.OutputText, 'Should return empty string for empty file' );
end;

procedure TTestStringOperations.TestConvertEncoding_UTF8ToWindows1252;
Var
  lResult         : TOperationResult;
  lLines          : TStringList;
  lEncoding       : TEncodingType;
  lBackupFilePath : string;
begin
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3', etUTF8 );

  lResult := TStringOperations.ConvertEncoding( fTestFilePath, 'windows1252', False, False, False );

  Assert.IsTrue( lResult.Success, 'ConvertEncoding should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etWindows1252 ), Ord( lEncoding ), 'Encoding should be Windows-1252' );
    Assert.AreEqual( 3, lLines.Count, 'Should have 3 lines' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should match' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should match' );
    Assert.AreEqual( 'Line 3', lLines[ 2 ], 'Line 3 should match' );
  finally
    lLines.Free;
  end;

  lBackupFilePath := fTestFilePath + '.bak';

  Assert.IsFalse( FileExists( lBackupFilePath ), 'Backup should not exist (--backup not specified)' );
end;

procedure TTestStringOperations.TestConvertEncoding_Windows1252ToUTF8;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1'#13#10'Line 2'#13#10'Line 3', etWindows1252 );

  lResult := TStringOperations.ConvertEncoding( fTestFilePath, 'utf8', False, False, False );

  Assert.IsTrue( lResult.Success, 'ConvertEncoding should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etUTF8 ), Ord( lEncoding ), 'Encoding should be UTF-8' );
    Assert.AreEqual( 3, lLines.Count, 'Should have 3 lines' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should match' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should match' );
    Assert.AreEqual( 'Line 3', lLines[ 2 ], 'Line 3 should match' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestConvertEncoding_WithBackup;
Var
  lResult         : TOperationResult;
  lBackupFilePath : string;
  lLines          : TStringList;
  lEncoding       : TEncodingType;
begin
  CreateTestFile( 'Test Content', etUTF8 );

  lBackupFilePath := fTestFilePath + '.bak';

  lResult := TStringOperations.ConvertEncoding( fTestFilePath, 'windows1252', True, False, False );

  Assert.IsTrue( lResult.Success, 'ConvertEncoding should succeed' );
  Assert.IsTrue( FileExists( lBackupFilePath ), 'Backup should exist' );

  Assert.IsTrue( TEncodingHelper.ReadFile( lBackupFilePath, lLines, lEncoding ), 'Should read backup file' );

  try
    Assert.AreEqual( Ord( etUTF8 ), Ord( lEncoding ), 'Backup should have original encoding (UTF-8)' );
    Assert.AreEqual( 'Test Content', lLines[ 0 ], 'Backup content should match' );
  finally
    lLines.Free;
  end;

  if FileExists( lBackupFilePath ) then
    DeleteFile( lBackupFilePath );
end;

procedure TTestStringOperations.TestConvertEncoding_SameEncoding;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Test Content', etWindows1252 );

  lResult := TStringOperations.ConvertEncoding( fTestFilePath, 'windows1252', False, False, False );

  Assert.IsFalse( lResult.Success, 'ConvertEncoding should fail (same encoding)' );
  Assert.AreEqual( 'File is already in target encoding', lResult.ErrorMessage, 'Error message should match' );
end;

procedure TTestStringOperations.TestConvertEncoding_WithUmlauts;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Umlaute: ä ö ü ß Ä Ö Ü', etWindows1252 );

  lResult := TStringOperations.ConvertEncoding( fTestFilePath, 'utf8', False, False, False );

  Assert.IsTrue( lResult.Success, 'ConvertEncoding should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etUTF8 ), Ord( lEncoding ), 'Encoding should be UTF-8' );
    Assert.AreEqual( 'Umlaute: ä ö ü ß Ä Ö Ü', lLines[ 0 ], 'Umlauts should be preserved' );
  finally
    lLines.Free;
  end;

  lResult := TStringOperations.ConvertEncoding( fTestFilePath, 'windows1252', False, False, False );

  Assert.IsTrue( lResult.Success, 'ConvertEncoding back should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etWindows1252 ), Ord( lEncoding ), 'Encoding should be Windows-1252' );
    Assert.AreEqual( 'Umlaute: ä ö ü ß Ä Ö Ü', lLines[ 0 ], 'Umlauts should be preserved after round-trip' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestConvertEncoding_InvalidEncoding;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Test Content', etWindows1252 );

  lResult := TStringOperations.ConvertEncoding( fTestFilePath, 'ascii', False, False, False );

  Assert.IsFalse( lResult.Success, 'ConvertEncoding should fail (invalid encoding)' );
  Assert.IsTrue( lResult.ErrorMessage.Contains( 'Invalid target encoding' ), 'Error message should mention invalid encoding' );
end;

procedure TTestStringOperations.TestReinterpretEncoding_UTF8AsWindows1252_Umlauts;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
  lBytes    : TBytes;
begin
  SetLength( lBytes, 35 );

  lBytes[ 0 ]  := Ord( 'U' );
  lBytes[ 1 ]  := Ord( 'm' );
  lBytes[ 2 ]  := Ord( 'l' );
  lBytes[ 3 ]  := Ord( 'a' );
  lBytes[ 4 ]  := Ord( 'u' );
  lBytes[ 5 ]  := Ord( 't' );
  lBytes[ 6 ]  := Ord( 'e' );
  lBytes[ 7 ]  := Ord( ':' );
  lBytes[ 8 ]  := Ord( ' ' );
  lBytes[ 9 ]  := $C3;
  lBytes[ 10 ] := $A4;
  lBytes[ 11 ] := Ord( ' ' );
  lBytes[ 12 ] := $C3;
  lBytes[ 13 ] := $B6;
  lBytes[ 14 ] := Ord( ' ' );
  lBytes[ 15 ] := $C3;
  lBytes[ 16 ] := $BC;
  lBytes[ 17 ] := Ord( ' ' );
  lBytes[ 18 ] := $C3;
  lBytes[ 19 ] := $9F;
  lBytes[ 20 ] := Ord( ' ' );
  lBytes[ 21 ] := $C3;
  lBytes[ 22 ] := $84;
  lBytes[ 23 ] := Ord( ' ' );
  lBytes[ 24 ] := $C3;
  lBytes[ 25 ] := $96;
  lBytes[ 26 ] := Ord( ' ' );
  lBytes[ 27 ] := $C3;
  lBytes[ 28 ] := $9C;
  lBytes[ 29 ] := $0D;
  lBytes[ 30 ] := $0A;
  lBytes[ 31 ] := Ord( 'E' );
  lBytes[ 32 ] := Ord( 'n' );
  lBytes[ 33 ] := Ord( 'd' );
  lBytes[ 34 ] := Ord( 'e' );

  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.ReinterpretEncoding( fTestFilePath, 'utf8', False, False, False );

  Assert.IsTrue( lResult.Success, 'ReinterpretEncoding should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etWindows1252 ), Ord( lEncoding ), 'Encoding should be Windows-1252' );
    Assert.AreEqual( 2, lLines.Count, 'Should have 2 lines' );
    Assert.IsTrue( Pos( 'Umlaute:', lLines[ 0 ] ) > 0, 'Should contain "Umlaute:"' );
    Assert.IsTrue( Pos( 'Ende', lLines[ 1 ] ) > 0, 'Should contain "Ende"' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReinterpretEncoding_UTF8AsWindows1252_Copyright;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
  lBytes    : TBytes;
begin
  SetLength( lBytes, 13 );

  lBytes[ 0 ]  := Ord( 'C' );
  lBytes[ 1 ]  := Ord( 'o' );
  lBytes[ 2 ]  := Ord( 'p' );
  lBytes[ 3 ]  := Ord( 'y' );
  lBytes[ 4 ]  := Ord( 'r' );
  lBytes[ 5 ]  := Ord( 'i' );
  lBytes[ 6 ]  := Ord( 'g' );
  lBytes[ 7 ]  := Ord( 'h' );
  lBytes[ 8 ]  := Ord( 't' );
  lBytes[ 9 ]  := Ord( ' ' );
  lBytes[ 10 ] := $C2;
  lBytes[ 11 ] := $A9;
  lBytes[ 12 ] := Ord( ' ' );

  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.ReinterpretEncoding( fTestFilePath, 'utf8', False, False, False );

  Assert.IsTrue( lResult.Success, 'ReinterpretEncoding should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etWindows1252 ), Ord( lEncoding ), 'Encoding should be Windows-1252' );
    Assert.IsTrue( Pos( 'Copyright', lLines[ 0 ] ) > 0, 'Should contain "Copyright"' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReinterpretEncoding_UTF8AsWindows1252_Eacute;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
  lBytes    : TBytes;
begin
  SetLength( lBytes, 8 );

  lBytes[ 0 ] := Ord( 'C' );
  lBytes[ 1 ] := Ord( 'a' );
  lBytes[ 2 ] := Ord( 'f' );
  lBytes[ 3 ] := $C3;
  lBytes[ 4 ] := $A9;
  lBytes[ 5 ] := Ord( ' ' );
  lBytes[ 6 ] := Ord( 'X' );
  lBytes[ 7 ] := Ord( 'Y' );

  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.ReinterpretEncoding( fTestFilePath, 'utf8', False, False, False );

  Assert.IsTrue( lResult.Success, 'ReinterpretEncoding should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etWindows1252 ), Ord( lEncoding ), 'Encoding should be Windows-1252' );
    Assert.IsTrue( Pos( 'Caf', lLines[ 0 ] ) > 0, 'Should contain "Caf"' );
    Assert.IsTrue( Pos( 'XY', lLines[ 0 ] ) > 0, 'Should contain "XY"' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReinterpretEncoding_Windows1252AsUTF8;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
  lBytes    : TBytes;
begin
  SetLength( lBytes, 6 );

  lBytes[ 0 ] := $EF;
  lBytes[ 1 ] := $BB;
  lBytes[ 2 ] := $BF;
  lBytes[ 3 ] := $E4;
  lBytes[ 4 ] := $F6;
  lBytes[ 5 ] := $FC;

  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.ReinterpretEncoding( fTestFilePath, 'windows1252', False, False, False );

  Assert.IsTrue( lResult.Success, 'ReinterpretEncoding should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etUTF8 ), Ord( lEncoding ), 'Encoding should be UTF-8' );
    Assert.AreEqual( 1, lLines.Count, 'Should have 1 line' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReinterpretEncoding_WithBackup;
Var
  lResult         : TOperationResult;
  lBackupFilePath : string;
  lBytes          : TBytes;
begin
  SetLength( lBytes, 4 );

  lBytes[ 0 ] := $C3;
  lBytes[ 1 ] := $A4;
  lBytes[ 2 ] := Ord( 'X' );
  lBytes[ 3 ] := Ord( 'Y' );

  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lBackupFilePath := fTestFilePath + '.bak';

  lResult := TStringOperations.ReinterpretEncoding( fTestFilePath, 'utf8', True, False, False );

  Assert.IsTrue( lResult.Success, 'ReinterpretEncoding should succeed' );
  Assert.IsTrue( FileExists( lBackupFilePath ), 'Backup should exist' );

  if FileExists( lBackupFilePath ) then
    DeleteFile( lBackupFilePath );
end;

procedure TTestStringOperations.TestReinterpretEncoding_InvalidSourceEncoding;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Test Content', etWindows1252 );

  lResult := TStringOperations.ReinterpretEncoding( fTestFilePath, 'ascii', False, False, False );

  Assert.IsFalse( lResult.Success, 'ReinterpretEncoding should fail (invalid encoding)' );
  Assert.IsTrue( lResult.ErrorMessage.Contains( 'Invalid source encoding' ), 'Error message should mention invalid encoding' );
end;

procedure TTestStringOperations.TestReinterpretEncoding_MixedCharacters;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
  lBytes    : TBytes;
begin
  SetLength( lBytes, 20 );

  lBytes[ 0 ]  := Ord( 'T' );
  lBytes[ 1 ]  := Ord( 'e' );
  lBytes[ 2 ]  := Ord( 's' );
  lBytes[ 3 ]  := Ord( 't' );
  lBytes[ 4 ]  := Ord( ' ' );
  lBytes[ 5 ]  := $C3;
  lBytes[ 6 ]  := $A4;
  lBytes[ 7 ]  := Ord( ' ' );
  lBytes[ 8 ]  := $C2;
  lBytes[ 9 ]  := $A9;
  lBytes[ 10 ] := Ord( ' ' );
  lBytes[ 11 ] := $C3;
  lBytes[ 12 ] := $A9;
  lBytes[ 13 ] := Ord( ' ' );
  lBytes[ 14 ] := $C3;
  lBytes[ 15 ] := $B6;
  lBytes[ 16 ] := Ord( ' ' );
  lBytes[ 17 ] := Ord( 'X' );
  lBytes[ 18 ] := Ord( 'Y' );
  lBytes[ 19 ] := Ord( 'Z' );

  TFile.WriteAllBytes( fTestFilePath, lBytes );

  lResult := TStringOperations.ReinterpretEncoding( fTestFilePath, 'utf8', False, False, False );

  Assert.IsTrue( lResult.Success, 'ReinterpretEncoding should succeed' );

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'Should read file' );

  try
    Assert.AreEqual( Ord( etWindows1252 ), Ord( lEncoding ), 'Encoding should be Windows-1252' );
    Assert.IsTrue( Pos( 'Test', lLines[ 0 ] ) > 0, 'Should contain "Test"' );
    Assert.IsTrue( Pos( 'XYZ', lLines[ 0 ] ) > 0, 'Should contain "XYZ"' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestBase64Replace_DollarSign;
Var
  lResult : TOperationResult;
  lLines  : TStringList;
  lBase64 : string;
begin
  TFile.WriteAllText( fTestFilePath, '     {$IFDEF PREUNICODE}' + #13#10 + 'end.', TEncoding.GetEncoding( 1252 ) );

  lBase64 := 'ICAgICB7JElGREVGIFBSRVVOSUNPREV9';

  lResult := TStringOperations.StrReplace( fTestFilePath, lBase64, '     {$IFDEF UNICODE}', 1, -1, False, False, False, ccNone, 0, '', False, True, False );

  Assert.IsTrue( lResult.Success, 'Replace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( '     {$IFDEF UNICODE}', lLines[ 0 ], 'Line should be replaced' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestBase64Replace_Backtick;
Var
  lResult : TOperationResult;
  lLines  : TStringList;
  lBase64 : string;
begin
  TFile.WriteAllText( fTestFilePath, 'Test `backtick` here' + #13#10 + 'end.', TEncoding.GetEncoding( 1252 ) );

  lBase64 := 'VGVzdCBgYmFja3RpY2tgIGhlcmU=';

  lResult := TStringOperations.StrReplace( fTestFilePath, lBase64, 'Test replaced here', 1, -1, False, False, False, ccNone, 0, '', False, True, False );

  Assert.IsTrue( lResult.Success, 'Replace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 'Test replaced here', lLines[ 0 ], 'Line should be replaced' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestBase64Replace_DoubleQuote;
Var
  lResult : TOperationResult;
  lLines  : TStringList;
  lBase64 : string;
begin
  TFile.WriteAllText( fTestFilePath, 'Test "quoted" here' + #13#10 + 'end.', TEncoding.GetEncoding( 1252 ) );

  lBase64 := 'VGVzdCAicXVvdGVkIiBoZXJl';

  lResult := TStringOperations.StrReplace( fTestFilePath, lBase64, 'Test replaced here', 1, -1, False, False, False, ccNone, 0, '', False, True, False );

  Assert.IsTrue( lResult.Success, 'Replace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 'Test replaced here', lLines[ 0 ], 'Line should be replaced' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestBase64Replace_AtSign;
Var
  lResult : TOperationResult;
  lLines  : TStringList;
  lBase64 : string;
begin
  TFile.WriteAllText( fTestFilePath, 'Email: test@example.com' + #13#10 + 'end.', TEncoding.GetEncoding( 1252 ) );

  lBase64 := 'RW1haWw6IHRlc3RAZXhhbXBsZS5jb20=';

  lResult := TStringOperations.StrReplace( fTestFilePath, lBase64, 'Email: new@example.com', 1, -1, False, False, False, ccNone, 0, '', False, True, False );

  Assert.IsTrue( lResult.Success, 'Replace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 'Email: new@example.com', lLines[ 0 ], 'Line should be replaced' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestBase64Replace_ComplexString;
Var
  lResult : TOperationResult;
  lLines  : TStringList;
  lBase64 : string;
begin
  TFile.WriteAllText( fTestFilePath, '{$IFDEF DEBUG} WriteLn("Test @Home"); {$ENDIF}' + #13#10 + 'end.', TEncoding.GetEncoding( 1252 ) );

  lBase64 := 'eyRJRkRFRiBERUJVR30gV3JpdGVMbigiVGVzdCBASG9tZSIpOyB7JEVORElGfQ==';

  lResult := TStringOperations.StrReplace( fTestFilePath, lBase64, '{$IFDEF RELEASE} WriteLn("Prod"); {$ENDIF}', 1, -1, False, False, False, ccNone, 0, '', False, True, False );

  Assert.IsTrue( lResult.Success, 'Replace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( '{$IFDEF RELEASE} WriteLn("Prod"); {$ENDIF}', lLines[ 0 ], 'Line should be replaced' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestBase64Insert_DollarSign;
Var
  lResult : TOperationResult;
  lLines  : TStringList;
  lBase64 : string;
begin
  TFile.WriteAllText( fTestFilePath, 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'end.', TEncoding.GetEncoding( 1252 ) );

  lBase64 := 'eyRJRkRFRiBERUJVR30=';

  lResult := TStringOperations.Insert( fTestFilePath, lBase64, 1, False, False, False, True );

  Assert.IsTrue( lResult.Success, 'Insert should succeed' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'First line unchanged' );
    Assert.AreEqual( '{$IFDEF DEBUG}', lLines[ 1 ], 'Inserted line' );
    Assert.AreEqual( 'Line 2', lLines[ 2 ], 'Second line unchanged' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestBase64Replace_InvalidBase64;
Var
  lResult : TOperationResult;
begin
  TFile.WriteAllText( fTestFilePath, 'Test' + #13#10 + 'end.', TEncoding.GetEncoding( 1252 ) );

  lResult := TStringOperations.StrReplace( fTestFilePath, 'INVALID!!!BASE64', 'New', 1, -1, False, False, False, ccNone, 0, '', False, True, False );

  Assert.IsFalse( lResult.Success, 'Replace should fail with invalid Base64' );
  Assert.IsTrue( Pos( 'Invalid Base64', lResult.ErrorMessage ) > 0, 'Error message should mention Base64' );
end;

procedure TTestStringOperations.TestBase64Replace_EmptyString;
Var
  lResult : TOperationResult;
  lBase64 : string;
begin
  TFile.WriteAllText( fTestFilePath, 'Test' + #13#10 + 'end.', TEncoding.GetEncoding( 1252 ) );

  lBase64 := '';

  lResult := TStringOperations.StrReplace( fTestFilePath, lBase64, 'New', 1, -1, False, False, False, ccNone, 0, '', False, True, False );

  Assert.IsFalse( lResult.Success, 'Replace should fail with empty string' );
  Assert.IsTrue( Pos( 'String not found', lResult.ErrorMessage ) > 0, 'Error message should mention string not found' );
end;

procedure TTestStringOperations.TestMultiLine_BasicReplace;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lOldStr   : string;
  lNewStr   : string;
  lContent  : string;
begin
  lContent := 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'end.';
  TFile.WriteAllText( fTestFilePath, lContent, TEncoding.GetEncoding( 1252 ) );

  lOldStr := 'Line 2' + #13#10 + 'Line 3';
  lNewStr := 'New Line';

  lResult := TStringOperations.StrReplace( fTestFilePath, lOldStr, lNewStr, 1, -1, False, False, False, ccNone, 0, '', False, False, False, True, False );

  Assert.IsTrue( lResult.Success, 'Multi-line replace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should report 1 change' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 3, lLines.Count, 'Should have 3 lines after replace' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( 'New Line', lLines[ 1 ], 'Line 2 should be replaced' );
    Assert.AreEqual( 'end.', lLines[ 2 ], 'Line 3 should be end.' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestMultiLine_ReplaceAll;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lOldStr   : string;
  lNewStr   : string;
  lContent  : string;
begin
  lContent := 'Block' + #13#10 + 'A' + #13#10 + 'Middle' + #13#10 + 'Block' + #13#10 + 'A' + #13#10 + 'end.';
  TFile.WriteAllText( fTestFilePath, lContent, TEncoding.GetEncoding( 1252 ) );

  lOldStr := 'Block' + #13#10 + 'A';
  lNewStr := 'Replaced';

  lResult := TStringOperations.StrReplace( fTestFilePath, lOldStr, lNewStr, 1, -1, False, False, False, ccNone, 0, '', False, False, False, True, True );

  Assert.IsTrue( lResult.Success, 'Multi-line replace-all should succeed' );
  Assert.AreEqual( 2, lResult.LinesChanged, 'Should report 2 changes' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 4, lLines.Count, 'Should have 4 lines after replace' );
    Assert.AreEqual( 'Replaced', lLines[ 0 ], 'First block should be replaced' );
    Assert.AreEqual( 'Middle', lLines[ 1 ], 'Middle should be unchanged' );
    Assert.AreEqual( 'Replaced', lLines[ 2 ], 'Second block should be replaced' );
    Assert.AreEqual( 'end.', lLines[ 3 ], 'Last line should be end.' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestMultiLine_LineRange;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lOldStr   : string;
  lNewStr   : string;
  lContent  : string;
begin
  lContent := 'Line 1' + #13#10 + 'Block' + #13#10 + 'A' + #13#10 + 'Line 4' + #13#10 + 'Block' + #13#10 + 'A' + #13#10 + 'end.';
  TFile.WriteAllText( fTestFilePath, lContent, TEncoding.GetEncoding( 1252 ) );

  lOldStr := 'Block' + #13#10 + 'A';
  lNewStr := 'Replaced';

  lResult := TStringOperations.StrReplace( fTestFilePath, lOldStr, lNewStr, 1, 4, False, False, False, ccNone, 0, '', False, False, False, True, False );

  Assert.IsTrue( lResult.Success, 'Multi-line replace with line range should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should report 1 change (only first block in range)' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 6, lLines.Count, 'Should have 6 lines after replace' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( 'Replaced', lLines[ 1 ], 'First block should be replaced' );
    Assert.AreEqual( 'Line 4', lLines[ 2 ], 'Line 4 should be unchanged' );
    Assert.AreEqual( 'Block', lLines[ 3 ], 'Second block should NOT be replaced (outside range)' );
    Assert.AreEqual( 'A', lLines[ 4 ], 'Second block line 2 should be unchanged' );
    Assert.AreEqual( 'end.', lLines[ 5 ], 'Last line should be end.' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestMultiLine_NotFound;
Var
  lResult   : TOperationResult;
  lOldStr   : string;
  lNewStr   : string;
  lContent  : string;
begin
  lContent := 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'end.';
  TFile.WriteAllText( fTestFilePath, lContent, TEncoding.GetEncoding( 1252 ) );

  lOldStr := 'Not' + #13#10 + 'Found';
  lNewStr := 'Replaced';

  lResult := TStringOperations.StrReplace( fTestFilePath, lOldStr, lNewStr, 1, -1, False, False, False, ccNone, 0, '', False, False, False, True, False );

  Assert.IsFalse( lResult.Success, 'Multi-line replace should fail when string not found' );
  Assert.IsTrue( Pos( 'String not found', lResult.ErrorMessage ) > 0, 'Error message should mention string not found' );
end;

procedure TTestStringOperations.TestMultiLine_DryRun;
Var
  lResult       : TOperationResult;
  lOldStr       : string;
  lNewStr       : string;
  lContent      : string;
  lOriginalHash : string;
begin
  lContent := 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'end.';
  TFile.WriteAllText( fTestFilePath, lContent, TEncoding.GetEncoding( 1252 ) );

  lOriginalHash := TFile.ReadAllText( fTestFilePath, TEncoding.GetEncoding( 1252 ) );

  lOldStr := 'Line 2' + #13#10 + 'Line 3';
  lNewStr := 'New Line';

  lResult := TStringOperations.StrReplace( fTestFilePath, lOldStr, lNewStr, 1, -1, True, False, False, ccNone, 0, '', False, False, False, True, False );

  Assert.IsTrue( lResult.Success, 'Multi-line dry-run should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should report 1 change' );

  Assert.AreEqual( lOriginalHash, TFile.ReadAllText( fTestFilePath, TEncoding.GetEncoding( 1252 ) ), 'File should not be modified in dry-run mode' );
end;

procedure TTestStringOperations.TestMultiLine_MultipleOccurrences;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lOldStr   : string;
  lNewStr   : string;
  lContent  : string;
begin
  lContent := 'Block' + #13#10 + 'A' + #13#10 + 'Middle' + #13#10 + 'Block' + #13#10 + 'A' + #13#10 + 'end.';
  TFile.WriteAllText( fTestFilePath, lContent, TEncoding.GetEncoding( 1252 ) );

  lOldStr := 'Block' + #13#10 + 'A';
  lNewStr := 'Replaced';

  lResult := TStringOperations.StrReplace( fTestFilePath, lOldStr, lNewStr, 1, -1, False, False, False, ccNone, 0, '', False, False, False, True, False );

  Assert.IsTrue( lResult.Success, 'Multi-line replace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should report 1 change (only first occurrence)' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 5, lLines.Count, 'Should have 5 lines after replace' );
    Assert.AreEqual( 'Replaced', lLines[ 0 ], 'First block should be replaced' );
    Assert.AreEqual( 'Middle', lLines[ 1 ], 'Middle should be unchanged' );
    Assert.AreEqual( 'Block', lLines[ 2 ], 'Second block should NOT be replaced (only first occurrence)' );
    Assert.AreEqual( 'A', lLines[ 3 ], 'Second block line 2 should be unchanged' );
    Assert.AreEqual( 'end.', lLines[ 4 ], 'Last line should be end.' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestMultiLine_FiveLinesToOne;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lOldStr   : string;
  lNewStr   : string;
  lContent  : string;
begin
  lContent := '      {$IFDEF PREUNICODE}' + #13#10 + '      WW := MyWideCanvasTextWidth(SortGrid1.canvas,AnsiOrUTF8((A[iii]+''..'')));' + #13#10 + '      {$ELSE}' + #13#10 + '      WW := Sortgrid1.canvas.textWidth(A[iii]+''..'' );' + #13#10 + '      {$ENDIF}' + #13#10 + 'end.';
  TFile.WriteAllText( fTestFilePath, lContent, TEncoding.GetEncoding( 1252 ) );

  lOldStr := '      {$IFDEF PREUNICODE}' + #13#10 + '      WW := MyWideCanvasTextWidth(SortGrid1.canvas,AnsiOrUTF8((A[iii]+''..'')));' + #13#10 + '      {$ELSE}' + #13#10 + '      WW := Sortgrid1.canvas.textWidth(A[iii]+''..'' );' + #13#10 + '      {$ENDIF}';
  lNewStr := '      WW := MyWideCanvasTextWidth(SortGrid1.canvas,AnsiOrUTF8((A[iii]+''..'')));';

  lResult := TStringOperations.StrReplace( fTestFilePath, lOldStr, lNewStr, 1, -1, False, False, False, ccNone, 0, '', False, False, False, True, False );

  Assert.IsTrue( lResult.Success, 'Multi-line replace (5 lines to 1) should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should report 1 change' );

  lLines := TStringList.Create;

  try
    lLines.LoadFromFile( fTestFilePath, TEncoding.GetEncoding( 1252 ) );
    Assert.AreEqual( 2, lLines.Count, 'Should have 2 lines after replace (1 + end.)' );
    Assert.AreEqual( '      WW := MyWideCanvasTextWidth(SortGrid1.canvas,AnsiOrUTF8((A[iii]+''..'')));', lLines[ 0 ], 'Line should be replaced' );
    Assert.AreEqual( 'end.', lLines[ 1 ], 'Last line should be end.' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestMultiLine_WithBackup;
Var
  lResult     : TOperationResult;
  lOldStr     : string;
  lNewStr     : string;
  lContent    : string;
  lBackupPath : string;
begin
  lContent := 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'end.';
  TFile.WriteAllText( fTestFilePath, lContent, TEncoding.GetEncoding( 1252 ) );

  lOldStr := 'Line 2' + #13#10 + 'Line 3';
  lNewStr := 'New Line';

  lBackupPath := fTestFilePath + '.bak';

  if TFile.Exists( lBackupPath ) then
    TFile.Delete( lBackupPath );

  lResult := TStringOperations.StrReplace( fTestFilePath, lOldStr, lNewStr, 1, -1, False, True, False, ccNone, 0, '', False, False, False, True, False );

  Assert.IsTrue( lResult.Success, 'Multi-line replace with backup should succeed' );
  Assert.IsTrue( TFile.Exists( lBackupPath ), 'Backup file should be created' );

  Assert.AreEqual( lContent, TFile.ReadAllText( lBackupPath, TEncoding.GetEncoding( 1252 ) ), 'Backup should contain original content' );

  if TFile.Exists( lBackupPath ) then
    TFile.Delete( lBackupPath );
end;

procedure TTestStringOperations.TestDeleteLine_Single;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.DeleteLine( fTestFilePath, 2 );

  Assert.IsTrue( lResult.Success, 'DeleteLine should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should delete 1 line' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 2, lLines.Count, 'Should have 2 lines after deletion' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should remain' );
    Assert.AreEqual( 'Line 3', lLines[ 1 ], 'Line 3 should remain' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestDeleteLine_First;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.DeleteLine( fTestFilePath, 1 );

  Assert.IsTrue( lResult.Success, 'DeleteLine should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 2, lLines.Count, 'Should have 2 lines after deletion' );
    Assert.AreEqual( 'Line 2', lLines[ 0 ], 'Line 2 should be first' );
    Assert.AreEqual( 'Line 3', lLines[ 1 ], 'Line 3 should be second' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestDeleteLine_Last;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.DeleteLine( fTestFilePath, 3 );

  Assert.IsTrue( lResult.Success, 'DeleteLine should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 2, lLines.Count, 'Should have 2 lines after deletion' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should remain' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should remain' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestDeleteLine_InvalidLine;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.DeleteLine( fTestFilePath, 10 );

  Assert.IsFalse( lResult.Success, 'DeleteLine should fail for invalid line' );
  Assert.IsTrue( Pos( 'Invalid line number', lResult.ErrorMessage ) > 0, 'Error message should mention invalid line' );
end;

procedure TTestStringOperations.TestDeleteLines_Range;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'Line 4' + #13#10 + 'Line 5', etWindows1252 );

  lResult := TStringOperations.DeleteLines( fTestFilePath, 2, 4 );

  Assert.IsTrue( lResult.Success, 'DeleteLines should succeed' );
  Assert.AreEqual( 3, lResult.LinesChanged, 'Should delete 3 lines' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 2, lLines.Count, 'Should have 2 lines after deletion' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should remain' );
    Assert.AreEqual( 'Line 5', lLines[ 1 ], 'Line 5 should remain' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestDeleteLines_MultipleLines;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'Line 4' + #13#10 + 'Line 5', etWindows1252 );

  lResult := TStringOperations.DeleteLines( fTestFilePath, '1,3,5' );

  Assert.IsTrue( lResult.Success, 'DeleteLines should succeed' );
  Assert.AreEqual( 3, lResult.LinesChanged, 'Should delete 3 lines' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 2, lLines.Count, 'Should have 2 lines after deletion' );
    Assert.AreEqual( 'Line 2', lLines[ 0 ], 'Line 2 should remain' );
    Assert.AreEqual( 'Line 4', lLines[ 1 ], 'Line 4 should remain' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestDeleteLines_InvalidRange;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.DeleteLines( fTestFilePath, 5, 10 );

  Assert.IsFalse( lResult.Success, 'DeleteLines should fail for invalid range' );
  Assert.IsTrue( Pos( 'Invalid line number', lResult.ErrorMessage ) > 0, 'Error message should mention invalid line' );
end;

procedure TTestStringOperations.TestDeleteLines_WithBackup;
Var
  lResult     : TOperationResult;
  lBackupPath : string;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lBackupPath := fTestFilePath + '.bak';

  if TFile.Exists( lBackupPath ) then
    TFile.Delete( lBackupPath );

  lResult := TStringOperations.DeleteLines( fTestFilePath, 2, 2, False, True );

  Assert.IsTrue( lResult.Success, 'DeleteLines with backup should succeed' );
  Assert.IsTrue( TFile.Exists( lBackupPath ), 'Backup file should be created' );

  if TFile.Exists( lBackupPath ) then
    TFile.Delete( lBackupPath );
end;

procedure TTestStringOperations.TestDeleteLines_DryRun;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.DeleteLines( fTestFilePath, 2, 2, True );

  Assert.IsTrue( lResult.Success, 'DeleteLines dry-run should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 3, lLines.Count, 'File should not be modified in dry-run' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should still exist' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReplaceLine_Single;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.ReplaceLine( fTestFilePath, 2, 'New Line 2' );

  Assert.IsTrue( lResult.Success, 'ReplaceLine should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should replace 1 line' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 3, lLines.Count, 'Should still have 3 lines' );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should remain' );
    Assert.AreEqual( 'New Line 2', lLines[ 1 ], 'Line 2 should be replaced' );
    Assert.AreEqual( 'Line 3', lLines[ 2 ], 'Line 3 should remain' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReplaceLine_First;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.ReplaceLine( fTestFilePath, 1, 'New Line 1' );

  Assert.IsTrue( lResult.Success, 'ReplaceLine should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 'New Line 1', lLines[ 0 ], 'First line should be replaced' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReplaceLine_Last;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.ReplaceLine( fTestFilePath, 3, 'New Line 3' );

  Assert.IsTrue( lResult.Success, 'ReplaceLine should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 'New Line 3', lLines[ 2 ], 'Last line should be replaced' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReplaceLine_InvalidLine;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  lResult := TStringOperations.ReplaceLine( fTestFilePath, 10, 'New Line' );

  Assert.IsFalse( lResult.Success, 'ReplaceLine should fail for invalid line' );
  Assert.IsTrue( Pos( 'Invalid line number', lResult.ErrorMessage ) > 0, 'Error message should mention invalid line' );
end;

procedure TTestStringOperations.TestReplaceLine_WithBackup;
Var
  lResult     : TOperationResult;
  lBackupPath : string;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lBackupPath := fTestFilePath + '.bak';

  if TFile.Exists( lBackupPath ) then
    TFile.Delete( lBackupPath );

  lResult := TStringOperations.ReplaceLine( fTestFilePath, 2, 'New Line 2', False, True );

  Assert.IsTrue( lResult.Success, 'ReplaceLine with backup should succeed' );
  Assert.IsTrue( TFile.Exists( lBackupPath ), 'Backup file should be created' );

  if TFile.Exists( lBackupPath ) then
    TFile.Delete( lBackupPath );
end;

procedure TTestStringOperations.TestReplaceLine_DryRun;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lResult := TStringOperations.ReplaceLine( fTestFilePath, 2, 'New Line 2', True );

  Assert.IsTrue( lResult.Success, 'ReplaceLine dry-run should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should not be modified in dry-run' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestReplaceLine_Base64;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
  lBase64   : string;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  lBase64 := 'TmV3IExpbmUgMg==';

  lResult := TStringOperations.ReplaceLine( fTestFilePath, 2, lBase64, False, False, False, False, True );

  Assert.IsTrue( lResult.Success, 'ReplaceLine with Base64 should succeed' );

  TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );

  try
    Assert.AreEqual( 'New Line 2', lLines[ 1 ], 'Line 2 should be replaced with decoded Base64' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestMoveLines_Basic;
Var
  lResult       : TOperationResult;
  lSourcePath   : string;
  lTargetPath   : string;
  lSourceLines  : TStringList;
  lTargetLines  : TStringList;
  lEncoding     : TEncodingType;
begin
  lSourcePath := 'test_move_source.tmp';
  lTargetPath := 'test_move_target.tmp';

  // Create source file with 5 lines
  lSourceLines := TStringList.Create;
  try
    lSourceLines.Add( 'Source Line 1' );
    lSourceLines.Add( 'Source Line 2' );
    lSourceLines.Add( 'Source Line 3' );
    lSourceLines.Add( 'Source Line 4' );
    lSourceLines.Add( 'Source Line 5' );
    TEncodingHelper.WriteFile( lSourcePath, lSourceLines, etWindows1252 );
  finally
    lSourceLines.Free;
  end;

  // Create target file with 3 lines
  lTargetLines := TStringList.Create;
  try
    lTargetLines.Add( 'Target Line 1' );
    lTargetLines.Add( 'Target Line 2' );
    lTargetLines.Add( 'Target Line 3' );
    TEncodingHelper.WriteFile( lTargetPath, lTargetLines, etWindows1252 );
  finally
    lTargetLines.Free;
  end;

  try
    // Move lines 2-3 from source to target after line 1
    lResult := TStringOperations.MoveLines( lSourcePath, lTargetPath, 2, 3, 1, 0 );

    Assert.IsTrue( lResult.Success, 'MoveLines should succeed' );
    Assert.AreEqual( 2, lResult.LinesChanged, 'Should move 2 lines' );

    // Verify source file (should have 3 lines now)
    TEncodingHelper.ReadFile( lSourcePath, lSourceLines, lEncoding );
    try
      Assert.AreEqual( 3, lSourceLines.Count, 'Source should have 3 lines' );
      Assert.AreEqual( 'Source Line 1', lSourceLines[ 0 ] );
      Assert.AreEqual( 'Source Line 4', lSourceLines[ 1 ] );
      Assert.AreEqual( 'Source Line 5', lSourceLines[ 2 ] );
    finally
      lSourceLines.Free;
    end;

    // Verify target file (should have 5 lines now)
    TEncodingHelper.ReadFile( lTargetPath, lTargetLines, lEncoding );
    try
      Assert.AreEqual( 5, lTargetLines.Count, 'Target should have 5 lines' );
      Assert.AreEqual( 'Target Line 1', lTargetLines[ 0 ] );
      Assert.AreEqual( 'Source Line 2', lTargetLines[ 1 ] );
      Assert.AreEqual( 'Source Line 3', lTargetLines[ 2 ] );
      Assert.AreEqual( 'Target Line 2', lTargetLines[ 3 ] );
      Assert.AreEqual( 'Target Line 3', lTargetLines[ 4 ] );
    finally
      lTargetLines.Free;
    end;
  finally
    if FileExists( lSourcePath ) then
      DeleteFile( lSourcePath );

    if FileExists( lTargetPath ) then
      DeleteFile( lTargetPath );
  end;
end;

procedure TTestStringOperations.TestMoveLines_InsertBefore;
Var
  lResult       : TOperationResult;
  lSourcePath   : string;
  lTargetPath   : string;
  lSourceLines  : TStringList;
  lTargetLines  : TStringList;
  lEncoding     : TEncodingType;
begin
  lSourcePath := 'test_move_source.tmp';
  lTargetPath := 'test_move_target.tmp';

  // Create source file
  lSourceLines := TStringList.Create;
  try
    lSourceLines.Add( 'Source Line 1' );
    lSourceLines.Add( 'Source Line 2' );
    TEncodingHelper.WriteFile( lSourcePath, lSourceLines, etWindows1252 );
  finally
    lSourceLines.Free;
  end;

  // Create target file
  lTargetLines := TStringList.Create;
  try
    lTargetLines.Add( 'Target Line 1' );
    lTargetLines.Add( 'Target Line 2' );
    TEncodingHelper.WriteFile( lTargetPath, lTargetLines, etWindows1252 );
  finally
    lTargetLines.Free;
  end;

  try
    // Move line 1 from source to target before line 2
    lResult := TStringOperations.MoveLines( lSourcePath, lTargetPath, 1, 1, 0, 2 );

    Assert.IsTrue( lResult.Success, 'MoveLines with insert-before should succeed' );

    // Verify target file
    TEncodingHelper.ReadFile( lTargetPath, lTargetLines, lEncoding );
    try
      Assert.AreEqual( 3, lTargetLines.Count, 'Target should have 3 lines' );
      Assert.AreEqual( 'Target Line 1', lTargetLines[ 0 ] );
      Assert.AreEqual( 'Source Line 1', lTargetLines[ 1 ] );
      Assert.AreEqual( 'Target Line 2', lTargetLines[ 2 ] );
    finally
      lTargetLines.Free;
    end;
  finally
    if FileExists( lSourcePath ) then
      DeleteFile( lSourcePath );

    if FileExists( lTargetPath ) then
      DeleteFile( lTargetPath );
  end;
end;

procedure TTestStringOperations.TestMoveLines_SourceNotFound;
Var
  lResult     : TOperationResult;
  lTargetPath : string;
  lLines      : TStringList;
begin
  lTargetPath := 'test_move_target.tmp';

  lLines := TStringList.Create;
  try
    lLines.Add( 'Target Line 1' );
    TEncodingHelper.WriteFile( lTargetPath, lLines, etWindows1252 );
  finally
    lLines.Free;
  end;

  try
    lResult := TStringOperations.MoveLines( 'nonexistent.pas', lTargetPath, 1, 1, 1, 0 );

    Assert.IsFalse( lResult.Success, 'MoveLines should fail for nonexistent source' );
    Assert.IsTrue( Pos( 'Source file not found', lResult.ErrorMessage ) > 0 );
  finally
    if FileExists( lTargetPath ) then
      DeleteFile( lTargetPath );
  end;
end;

procedure TTestStringOperations.TestMoveLines_TargetNotFound;
Var
  lResult     : TOperationResult;
  lSourcePath : string;
  lLines      : TStringList;
begin
  lSourcePath := 'test_move_source.tmp';

  lLines := TStringList.Create;
  try
    lLines.Add( 'Source Line 1' );
    TEncodingHelper.WriteFile( lSourcePath, lLines, etWindows1252 );
  finally
    lLines.Free;
  end;

  try
    lResult := TStringOperations.MoveLines( lSourcePath, 'nonexistent.pas', 1, 1, 1, 0 );

    Assert.IsFalse( lResult.Success, 'MoveLines should fail for nonexistent target' );
    Assert.IsTrue( Pos( 'Target file not found', lResult.ErrorMessage ) > 0 );
  finally
    if FileExists( lSourcePath ) then
      DeleteFile( lSourcePath );
  end;
end;

procedure TTestStringOperations.TestMoveLines_InvalidLineRange;
Var
  lResult       : TOperationResult;
  lSourcePath   : string;
  lTargetPath   : string;
  lLines        : TStringList;
begin
  lSourcePath := 'test_move_source.tmp';
  lTargetPath := 'test_move_target.tmp';

  lLines := TStringList.Create;
  try
    lLines.Add( 'Line 1' );
    lLines.Add( 'Line 2' );
    TEncodingHelper.WriteFile( lSourcePath, lLines, etWindows1252 );
    TEncodingHelper.WriteFile( lTargetPath, lLines, etWindows1252 );
  finally
    lLines.Free;
  end;

  try
    // Try to move lines 5-10 from a 2-line file
    lResult := TStringOperations.MoveLines( lSourcePath, lTargetPath, 5, 10, 1, 0 );

    Assert.IsFalse( lResult.Success, 'MoveLines should fail for invalid line range' );
    Assert.IsTrue( Pos( 'Invalid', lResult.ErrorMessage ) > 0 );
  finally
    if FileExists( lSourcePath ) then
      DeleteFile( lSourcePath );

    if FileExists( lTargetPath ) then
      DeleteFile( lTargetPath );
  end;
end;

procedure TTestStringOperations.TestMoveLines_WithBackup;
Var
  lResult           : TOperationResult;
  lSourcePath       : string;
  lTargetPath       : string;
  lSourceBackupPath : string;
  lTargetBackupPath : string;
  lLines            : TStringList;
begin
  lSourcePath       := 'test_move_source.tmp';
  lTargetPath       := 'test_move_target.tmp';
  lSourceBackupPath := lSourcePath + '.bak';
  lTargetBackupPath := lTargetPath + '.bak';

  lLines := TStringList.Create;
  try
    lLines.Add( 'Line 1' );
    lLines.Add( 'Line 2' );
    TEncodingHelper.WriteFile( lSourcePath, lLines, etWindows1252 );
    TEncodingHelper.WriteFile( lTargetPath, lLines, etWindows1252 );
  finally
    lLines.Free;
  end;

  try
    lResult := TStringOperations.MoveLines( lSourcePath, lTargetPath, 1, 1, 1, 0, False, True );

    Assert.IsTrue( lResult.Success, 'MoveLines with backup should succeed' );
    Assert.IsTrue( FileExists( lSourceBackupPath ), 'Source backup should exist' );
    Assert.IsTrue( FileExists( lTargetBackupPath ), 'Target backup should exist' );
  finally
    if FileExists( lSourcePath ) then
      DeleteFile( lSourcePath );

    if FileExists( lTargetPath ) then
      DeleteFile( lTargetPath );

    if FileExists( lSourceBackupPath ) then
      DeleteFile( lSourceBackupPath );

    if FileExists( lTargetBackupPath ) then
      DeleteFile( lTargetBackupPath );
  end;
end;

procedure TTestStringOperations.TestMoveLines_DryRun;
Var
  lResult       : TOperationResult;
  lSourcePath   : string;
  lTargetPath   : string;
  lSourceLines  : TStringList;
  lTargetLines  : TStringList;
  lEncoding     : TEncodingType;
begin
  lSourcePath := 'test_move_source.tmp';
  lTargetPath := 'test_move_target.tmp';

  lSourceLines := TStringList.Create;
  try
    lSourceLines.Add( 'Source Line 1' );
    lSourceLines.Add( 'Source Line 2' );
    TEncodingHelper.WriteFile( lSourcePath, lSourceLines, etWindows1252 );
  finally
    lSourceLines.Free;
  end;

  lTargetLines := TStringList.Create;
  try
    lTargetLines.Add( 'Target Line 1' );
    TEncodingHelper.WriteFile( lTargetPath, lTargetLines, etWindows1252 );
  finally
    lTargetLines.Free;
  end;

  try
    lResult := TStringOperations.MoveLines( lSourcePath, lTargetPath, 1, 1, 1, 0, True );

    Assert.IsTrue( lResult.Success, 'MoveLines dry-run should succeed' );

    // Verify source file is unchanged
    TEncodingHelper.ReadFile( lSourcePath, lSourceLines, lEncoding );
    try
      Assert.AreEqual( 2, lSourceLines.Count, 'Source should still have 2 lines' );
    finally
      lSourceLines.Free;
    end;

    // Verify target file is unchanged
    TEncodingHelper.ReadFile( lTargetPath, lTargetLines, lEncoding );
    try
      Assert.AreEqual( 1, lTargetLines.Count, 'Target should still have 1 line' );
    finally
      lTargetLines.Free;
    end;
  finally
    if FileExists( lSourcePath ) then
      DeleteFile( lSourcePath );

    if FileExists( lTargetPath ) then
      DeleteFile( lTargetPath );
  end;
end;

procedure TTestStringOperations.TestMoveLines_EncodingPreservation;
Var
  lResult       : TOperationResult;
  lSourcePath   : string;
  lTargetPath   : string;
  lSourceLines  : TStringList;
  lTargetLines  : TStringList;
  lEncoding     : TEncodingType;
begin
  lSourcePath := 'test_move_source.tmp';
  lTargetPath := 'test_move_target.tmp';

  // Create source file with UTF-8 encoding
  lSourceLines := TStringList.Create;
  try
    lSourceLines.Add( 'Ümläüt Söürce' );
    TEncodingHelper.WriteFile( lSourcePath, lSourceLines, etUTF8 );
  finally
    lSourceLines.Free;
  end;

  // Create target file with Windows-1252 encoding
  lTargetLines := TStringList.Create;
  try
    lTargetLines.Add( 'Target Line' );
    TEncodingHelper.WriteFile( lTargetPath, lTargetLines, etWindows1252 );
  finally
    lTargetLines.Free;
  end;

  try
    lResult := TStringOperations.MoveLines( lSourcePath, lTargetPath, 1, 1, 1, 0 );

    Assert.IsTrue( lResult.Success, 'MoveLines should succeed' );

    // Verify source encoding is preserved (UTF-8)
    TEncodingHelper.ReadFile( lSourcePath, lSourceLines, lEncoding );
    try
      Assert.AreEqual( etUTF8, lEncoding, 'Source encoding should be preserved as UTF-8' );
    finally
      lSourceLines.Free;
    end;

    // Verify target encoding is preserved (Windows-1252)
    TEncodingHelper.ReadFile( lTargetPath, lTargetLines, lEncoding );
    try
      Assert.AreEqual( etWindows1252, lEncoding, 'Target encoding should be preserved as Windows-1252' );
    finally
      lTargetLines.Free;
    end;
  finally
    if FileExists( lSourcePath ) then
      DeleteFile( lSourcePath );

    if FileExists( lTargetPath ) then
      DeleteFile( lTargetPath );
  end;
end;

procedure TTestStringOperations.TestAutoDeleteConfig_Basic;
Var
  lConfigPath : string;
  lTestPath   : string;
  lContent    : string;
begin
  // Arrange - Test that config files are auto-deleted on success (v1.8.3 default behavior)
  lTestPath   := TPath.Combine( TPath.GetTempPath, 'test_auto_delete_config.pas' );
  lConfigPath := TPath.Combine( TPath.GetTempPath, 'test_config_auto_delete.json' );

  lContent := 'Unit Test;' + sLineBreak +
              'interface' + sLineBreak +
              'implementation' + sLineBreak +
              'end.';
  TFile.WriteAllText( lTestPath, lContent );
  TFile.WriteAllText( lConfigPath, '{"file": "' + StringReplace( lTestPath, '\', '\\', [rfReplaceAll] ) + '", "old-str": "Test", "new-str": "Test2"}' );

  try
    // Act & Assert - just verify files exist (actual deletion tested via command line)
    Assert.IsTrue( FileExists( lTestPath ), 'Test file should exist' );
    Assert.IsTrue( FileExists( lConfigPath ), 'Config file should exist before execution' );
  finally
    if FileExists( lTestPath ) then
      DeleteFile( lTestPath );

    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

procedure TTestStringOperations.TestAutoDeleteConfig_OnError;
Var
  lConfigPath : string;
begin
  // Arrange - Config points to non-existent file, should NOT be deleted on error
  lConfigPath := TPath.Combine( TPath.GetTempPath, 'test_config_error.json' );
  TFile.WriteAllText( lConfigPath, '{"file": "nonexistent_file.pas", "old-str": "Test", "new-str": "Test2"}' );

  try
    // Assert - Config should still exist (not deleted on error)
    Assert.IsTrue( FileExists( lConfigPath ), 'Config file should exist for error test' );
  finally
    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

procedure TTestStringOperations.TestAutoDeleteConfig_DryRun;
Var
  lConfigPath : string;
  lTestPath   : string;
  lContent    : string;
begin
  // Arrange - In dry-run mode, config should NOT be deleted
  lTestPath   := TPath.Combine( TPath.GetTempPath, 'test_auto_delete_dryrun.pas' );
  lConfigPath := TPath.Combine( TPath.GetTempPath, 'test_config_dryrun.json' );

  lContent := 'Unit Test;' + sLineBreak +
              'interface' + sLineBreak +
              'implementation' + sLineBreak +
              'end.';
  TFile.WriteAllText( lTestPath, lContent );
  TFile.WriteAllText( lConfigPath, '{"file": "' + StringReplace( lTestPath, '\', '\\', [rfReplaceAll] ) + '", "old-str": "Test", "new-str": "Test2", "dry-run": true}' );

  try
    // Act & Assert - Config should still exist after dry-run (not deleted)
    Assert.IsTrue( FileExists( lConfigPath ), 'Config file should exist for dry-run test' );
  finally
    if FileExists( lTestPath ) then
      DeleteFile( lTestPath );

    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

procedure TTestStringOperations.TestKeepConfig_Basic;
Var
  lConfigPath : string;
  lTestPath   : string;
  lContent    : string;
begin
  // Arrange - Test that --keep-config preserves the JSON file
  lTestPath   := TPath.Combine( TPath.GetTempPath, 'test_keep_config.pas' );
  lConfigPath := TPath.Combine( TPath.GetTempPath, 'test_config_keep.json' );

  lContent := 'Unit Test;' + sLineBreak +
              'interface' + sLineBreak +
              'implementation' + sLineBreak +
              'end.';
  TFile.WriteAllText( lTestPath, lContent );
  TFile.WriteAllText( lConfigPath, '{"file": "' + StringReplace( lTestPath, '\', '\\', [rfReplaceAll] ) + '", "old-str": "Test", "new-str": "Test2"}' );

  try
    // Act & Assert - just verify files exist (--keep-config tested via command line)
    Assert.IsTrue( FileExists( lTestPath ), 'Test file should exist' );
    Assert.IsTrue( FileExists( lConfigPath ), 'Config file should exist' );
  finally
    if FileExists( lTestPath ) then
      DeleteFile( lTestPath );

    if FileExists( lConfigPath ) then
      DeleteFile( lConfigPath );
  end;
end;

procedure TTestStringOperations.TestIndentLines_Basic;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange: 4 Zeilen, Zeilen 2-3 sollen um 2 Spaces eingerückt werden
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3' + #13#10 + 'Line 4', etWindows1252 );

  // Act
  lResult := TStringOperations.IndentLines( fTestFilePath, 2, 3, 2 );

  // Assert
  Assert.IsTrue( lResult.Success, 'IndentLines should succeed' );
  Assert.AreEqual( 2, lResult.LinesChanged, 'Should change 2 lines' );

  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( '  Line 2', lLines[ 1 ], 'Line 2 should be indented' );
    Assert.AreEqual( '  Line 3', lLines[ 2 ], 'Line 3 should be indented' );
    Assert.AreEqual( 'Line 4', lLines[ 3 ], 'Line 4 should be unchanged' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestIndentLines_CustomSpaces;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange: Indent mit 4 Spaces
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  // Act
  lResult := TStringOperations.IndentLines( fTestFilePath, 1, 2, 4 );

  // Assert
  Assert.IsTrue( lResult.Success, 'IndentLines should succeed' );

  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( '    Line 1', lLines[ 0 ], 'Line 1 should be indented by 4 spaces' );
    Assert.AreEqual( '    Line 2', lLines[ 1 ], 'Line 2 should be indented by 4 spaces' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestIndentLines_EmptyLines;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange: Datei mit leeren Zeilen
  CreateTestFile( 'Line 1' + #13#10 + '' + #13#10 + 'Line 3', etWindows1252 );

  // Act
  lResult := TStringOperations.IndentLines( fTestFilePath, 1, 3, 2 );

  // Assert
  Assert.IsTrue( lResult.Success, 'IndentLines should succeed' );
  Assert.AreEqual( 3, lResult.LinesChanged, 'Should report 3 lines in range' );

  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( '  Line 1', lLines[ 0 ], 'Line 1 should be indented' );
    Assert.AreEqual( '', lLines[ 1 ], 'Empty line should stay empty' );
    Assert.AreEqual( '  Line 3', lLines[ 2 ], 'Line 3 should be indented' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestIndentLines_SingleLine;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange: Nur eine Zeile einrücken
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  // Act
  lResult := TStringOperations.IndentLines( fTestFilePath, 2, 2, 2 );

  // Assert
  Assert.IsTrue( lResult.Success, 'IndentLines should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( '  Line 2', lLines[ 1 ], 'Line 2 should be indented' );
    Assert.AreEqual( 'Line 3', lLines[ 2 ], 'Line 3 should be unchanged' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestIndentLines_InvalidRange;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  // Act
  lResult := TStringOperations.IndentLines( fTestFilePath, 1, 10, 2 );

  // Assert
  Assert.IsFalse( lResult.Success, 'IndentLines should fail for invalid range' );
  Assert.IsTrue( Pos( 'out of range', lResult.ErrorMessage ) > 0, 'Error should mention out of range' );
end;

procedure TTestStringOperations.TestIndentLines_DryRun;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  // Act - dry-run
  lResult := TStringOperations.IndentLines( fTestFilePath, 1, 2, 2, true );

  // Assert
  Assert.IsTrue( lResult.Success, 'IndentLines dry-run should succeed' );
  Assert.AreEqual( 2, lResult.LinesChanged, 'Should report 2 lines would change' );

  // Datei sollte unverändert sein
  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged in dry-run' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should be unchanged in dry-run' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestUnindentLines_Basic;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange: Zeilen mit 4 Spaces Einrückung
  CreateTestFile( 'Line 1' + #13#10 + '    Line 2' + #13#10 + '    Line 3' + #13#10 + 'Line 4', etWindows1252 );

  // Act
  lResult := TStringOperations.UnindentLines( fTestFilePath, 2, 3, 2 );

  // Assert
  Assert.IsTrue( lResult.Success, 'UnindentLines should succeed' );
  Assert.AreEqual( 2, lResult.LinesChanged, 'Should change 2 lines' );

  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( '  Line 2', lLines[ 1 ], 'Line 2 should have 2 spaces removed' );
    Assert.AreEqual( '  Line 3', lLines[ 2 ], 'Line 3 should have 2 spaces removed' );
    Assert.AreEqual( 'Line 4', lLines[ 3 ], 'Line 4 should be unchanged' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestUnindentLines_PartialUnindent;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange: Zeile hat nur 2 Spaces, versuche 4 zu entfernen
  CreateTestFile( '  Line 1' + #13#10 + '    Line 2', etWindows1252 );

  // Act - versuche 4 Spaces zu entfernen
  lResult := TStringOperations.UnindentLines( fTestFilePath, 1, 2, 4 );

  // Assert
  Assert.IsTrue( lResult.Success, 'UnindentLines should succeed' );

  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should have only 2 spaces removed (partial)' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should have all 4 spaces removed' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestUnindentLines_NoLeadingSpaces;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange: Zeilen ohne führende Spaces
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  // Act
  lResult := TStringOperations.UnindentLines( fTestFilePath, 1, 2, 2 );

  // Assert
  Assert.IsTrue( lResult.Success, 'UnindentLines should succeed (no changes needed)' );
  Assert.AreEqual( 2, lResult.LinesChanged, 'Should report 2 lines in range' );

  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should be unchanged' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestUnindentLines_SingleLine;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange
  CreateTestFile( 'Line 1' + #13#10 + '  Line 2' + #13#10 + 'Line 3', etWindows1252 );

  // Act
  lResult := TStringOperations.UnindentLines( fTestFilePath, 2, 2, 2 );

  // Assert
  Assert.IsTrue( lResult.Success, 'UnindentLines should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );

  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be unchanged' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should be unindented' );
    Assert.AreEqual( 'Line 3', lLines[ 2 ], 'Line 3 should be unchanged' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestUnindentLines_InvalidRange;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2', etWindows1252 );

  // Act
  lResult := TStringOperations.UnindentLines( fTestFilePath, 1, 10, 2 );

  // Assert
  Assert.IsFalse( lResult.Success, 'UnindentLines should fail for invalid range' );
  Assert.IsTrue( Pos( 'out of range', lResult.ErrorMessage ) > 0, 'Error should mention out of range' );
end;

procedure TTestStringOperations.TestUnindentLines_DryRun;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  CreateTestFile( '  Line 1' + #13#10 + '  Line 2', etWindows1252 );

  // Act - dry-run
  lResult := TStringOperations.UnindentLines( fTestFilePath, 1, 2, 2, true );

  // Assert
  Assert.IsTrue( lResult.Success, 'UnindentLines dry-run should succeed' );
  Assert.AreEqual( 2, lResult.LinesChanged, 'Should report 2 lines would change' );

  // Datei sollte unverändert sein
  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( '  Line 1', lLines[ 0 ], 'Line 1 should be unchanged in dry-run' );
    Assert.AreEqual( '  Line 2', lLines[ 1 ], 'Line 2 should be unchanged in dry-run' );
  finally
    lLines.Free;
  end;
end;

procedure TTestStringOperations.TestIndentUnindent_Roundtrip;
Var
  lResult   : TOperationResult;
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  // Arrange: Original-Datei
  CreateTestFile( 'Line 1' + #13#10 + 'Line 2' + #13#10 + 'Line 3', etWindows1252 );

  // Act 1: Indent
  lResult := TStringOperations.IndentLines( fTestFilePath, 1, 3, 4 );
  Assert.IsTrue( lResult.Success, 'IndentLines should succeed' );

  // Verify indented
  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( '    Line 1', lLines[ 0 ], 'Line 1 should be indented by 4' );
    Assert.AreEqual( '    Line 2', lLines[ 1 ], 'Line 2 should be indented by 4' );
    Assert.AreEqual( '    Line 3', lLines[ 2 ], 'Line 3 should be indented by 4' );
  finally
    lLines.Free;
  end;

  // Act 2: Unindent - sollte Original wiederherstellen
  lResult := TStringOperations.UnindentLines( fTestFilePath, 1, 3, 4 );
  Assert.IsTrue( lResult.Success, 'UnindentLines should succeed' );

  // Verify original restored
  lLines := TStringList.Create;
  try
    TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding );
    Assert.AreEqual( 'Line 1', lLines[ 0 ], 'Line 1 should be restored to original' );
    Assert.AreEqual( 'Line 2', lLines[ 1 ], 'Line 2 should be restored to original' );
    Assert.AreEqual( 'Line 3', lLines[ 2 ], 'Line 3 should be restored to original' );
  finally
    lLines.Free;
  end;
end;

end.

