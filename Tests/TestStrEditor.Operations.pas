Unit TestStrEditor.Operations;

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, StrEditor.Encoding
, StrEditor.Operations
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

end.

