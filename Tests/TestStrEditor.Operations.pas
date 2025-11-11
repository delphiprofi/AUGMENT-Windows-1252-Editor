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

end.

