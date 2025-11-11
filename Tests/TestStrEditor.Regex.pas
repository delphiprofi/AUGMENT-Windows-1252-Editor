Unit TestStrEditor.Regex;

interface

Uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  StrEditor.Regex,
  StrEditor.Encoding;

Type
  [TestFixture]
  TTestRegexOperations = class
    private
      fTestFilePath : string;

      procedure CreateTestFile( const aContent : string );
      function ReadTestFile : string;

    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestRegexReplace_Simple;

      [Test]
      procedure TestRegexReplace_CaptureGroups;

      [Test]
      procedure TestRegexReplace_CaseInsensitive;

      [Test]
      procedure TestRegexReplace_MultiLine;

      [Test]
      procedure TestRegexReplace_WithLineRange;

      [Test]
      procedure TestRegexTest_PatternFound;

      [Test]
      procedure TestRegexTest_PatternNotFound;

      [Test]
      procedure TestRegexReplace_PatternNotFound;

      [Test]
      procedure TestRegexReplace_FileNotFound;

      [Test]
      procedure TestRegexReplace_MultipleMatches;
  end;

implementation

{ TTestRegexOperations }

procedure TTestRegexOperations.Setup;
begin
  fTestFilePath := 'test_regex_' + IntToStr( Random( 10000 ) ) + '.pas';
end;

procedure TTestRegexOperations.TearDown;
begin
  if FileExists( fTestFilePath ) then
    DeleteFile( fTestFilePath );
end;

procedure TTestRegexOperations.CreateTestFile( const aContent : string );
Var
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  lLines := TStringList.Create;
  try
    lLines.Text := aContent;
    lEncoding   := etWindows1252;

    TEncodingHelper.WriteFile( fTestFilePath, lLines, lEncoding );
  finally
    lLines.Free;
  end;
end;

function TTestRegexOperations.ReadTestFile : string;
Var
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  Result := '';

  if TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ) then
    try
      Result := lLines.Text;
    finally
      lLines.Free;
    end;
end;

procedure TTestRegexOperations.TestRegexReplace_Simple;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'Line1'#13#10'Line2'#13#10'Line3'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'Line\d', 'NewLine', 0, 0, false, false );

  Assert.IsTrue( lResult.Success, 'RegexReplace should succeed' );
  Assert.AreEqual( 3, lResult.LinesChanged, 'Should change 3 lines' );
  Assert.Contains( ReadTestFile, 'NewLine' );
end;

procedure TTestRegexOperations.TestRegexReplace_CaptureGroups;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'fValue : Integer;'#13#10'fName : string;'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'f(\w+)', 'l$1', 0, 0, false, false );

  Assert.IsTrue( lResult.Success, 'RegexReplace should succeed' );
  Assert.Contains( ReadTestFile, 'lValue' );
  Assert.Contains( ReadTestFile, 'lName' );
end;

procedure TTestRegexOperations.TestRegexReplace_CaseInsensitive;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'VALUE'#13#10'value'#13#10'Value'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'value', 'REPLACED', 0, 0, true, false );

  Assert.IsTrue( lResult.Success, 'RegexReplace should succeed' );
  Assert.AreEqual( 3, lResult.LinesChanged, 'Should change 3 lines' );
end;

procedure TTestRegexOperations.TestRegexReplace_MultiLine;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'Line1'#13#10'Line2'#13#10'Line3'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'Line', 'Row', 0, 0, false, true );

  Assert.IsTrue( lResult.Success, 'RegexReplace should succeed' );
  Assert.Contains( ReadTestFile, 'Row' );
end;

procedure TTestRegexOperations.TestRegexReplace_WithLineRange;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'Line1'#13#10'Line2'#13#10'Line3'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'Line', 'Row', 2, 2, false, false );

  Assert.IsTrue( lResult.Success, 'RegexReplace should succeed' );
  Assert.AreEqual( 1, lResult.LinesChanged, 'Should change 1 line' );
  Assert.Contains( ReadTestFile, 'Line1' );
  Assert.Contains( ReadTestFile, 'Row2' );
  Assert.Contains( ReadTestFile, 'Line3' );
end;

procedure TTestRegexOperations.TestRegexTest_PatternFound;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'procedure Foo;'#13#10'function Bar : Integer;'#13#10 );

  lResult := TRegexOperations.RegexTest( fTestFilePath, 'procedure\s+\w+', 0, 0, false, false );

  Assert.IsTrue( lResult.Success, 'RegexTest should succeed' );
  Assert.IsTrue( lResult.MatchCount > 0, 'Should find matches' );
end;

procedure TTestRegexOperations.TestRegexTest_PatternNotFound;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'Line1'#13#10'Line2'#13#10 );

  lResult := TRegexOperations.RegexTest( fTestFilePath, 'NotFound', 0, 0, false, false );

  Assert.IsFalse( lResult.Success, 'RegexTest should fail' );
  Assert.AreEqual( 0, lResult.MatchCount, 'Should find no matches' );
end;

procedure TTestRegexOperations.TestRegexReplace_PatternNotFound;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'Line1'#13#10'Line2'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'NotFound', 'Replacement', 0, 0, false, false );

  Assert.IsFalse( lResult.Success, 'RegexReplace should fail' );
  Assert.Contains( lResult.ErrorMessage, 'not found' );
end;

procedure TTestRegexOperations.TestRegexReplace_FileNotFound;
Var
  lResult : TRegexOperationResult;
begin
  lResult := TRegexOperations.RegexReplace( 'nonexistent.pas', 'Pattern', 'Replacement', 0, 0, false, false );

  Assert.IsFalse( lResult.Success, 'RegexReplace should fail' );
  Assert.Contains( lResult.ErrorMessage, 'not found' );
end;

procedure TTestRegexOperations.TestRegexReplace_MultipleMatches;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'fValue fName fIndex'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'f(\w+)', 'l$1', 0, 0, false, false );

  Assert.IsTrue( lResult.Success, 'RegexReplace should succeed' );
  Assert.Contains( ReadTestFile, 'lValue' );
  Assert.Contains( ReadTestFile, 'lName' );
  Assert.Contains( ReadTestFile, 'lIndex' );
end;

end.

