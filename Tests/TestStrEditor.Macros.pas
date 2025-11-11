Unit TestStrEditor.Macros;

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, StrEditor.Macros
, StrEditor.Operations
, StrEditor.Regex
;

Type
  [TestFixture]
  TTestMacroExpander = class
    strict private
      fTestFilePath : string;
      procedure CreateTestFile( const aContent : string );
      function ReadTestFile : string;
    public
      [Setup]
      procedure Setup;
      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestExpandMacros_LineNumber;
      [Test]
      procedure TestExpandMacros_FileName;
      [Test]
      procedure TestExpandMacros_Date;
      [Test]
      procedure TestExpandMacros_Time;
      [Test]
      procedure TestExpandMacros_AllMacros;
      [Test]
      procedure TestStrReplace_WithMacros;
      [Test]
      procedure TestRegexReplace_WithMacros;
      [Test]
      procedure TestRegexReplace_WithMacrosAndCaptureGroups;
  end;

implementation

procedure TTestMacroExpander.Setup;
begin
  fTestFilePath := 'test_macros_' + IntToStr( Random( 10000 ) ) + '.pas';
end;

procedure TTestMacroExpander.TearDown;
begin
  if FileExists( fTestFilePath ) then
    DeleteFile( fTestFilePath );
end;

procedure TTestMacroExpander.CreateTestFile( const aContent : string );
Var
  lFileStream : TFileStream;
  lBytes      : TBytes;
begin
  lFileStream := TFileStream.Create( fTestFilePath, fmCreate );
  try
    lBytes := TEncoding.ANSI.GetBytes( aContent );
    lFileStream.WriteBuffer( lBytes[ 0 ], Length( lBytes ) );
  finally
    lFileStream.Free;
  end;
end;

function TTestMacroExpander.ReadTestFile : string;
Var
  lFileStream : TFileStream;
  lBytes      : TBytes;
begin
  lFileStream := TFileStream.Create( fTestFilePath, fmOpenRead );
  try
    SetLength( lBytes, lFileStream.Size );
    lFileStream.ReadBuffer( lBytes[ 0 ], lFileStream.Size );
    Result := TEncoding.ANSI.GetString( lBytes );
  finally
    lFileStream.Free;
  end;
end;

procedure TTestMacroExpander.TestExpandMacros_LineNumber;
Var
  lResult : string;
begin
  lResult := TMacroExpander.ExpandMacros( 'Line {{LINE_NUMBER}}', 'test.pas', 42 );

  Assert.AreEqual( 'Line 42', lResult );
end;

procedure TTestMacroExpander.TestExpandMacros_FileName;
Var
  lResult : string;
begin
  lResult := TMacroExpander.ExpandMacros( 'File {{FILE_NAME}}', 'C:\Temp\test.pas', 1 );

  Assert.AreEqual( 'File test.pas', lResult );
end;

procedure TTestMacroExpander.TestExpandMacros_Date;
Var
  lResult : string;
begin
  lResult := TMacroExpander.ExpandMacros( 'Date {{DATE}}', 'test.pas', 1 );

  Assert.Contains( lResult, 'Date 2025-' );
end;

procedure TTestMacroExpander.TestExpandMacros_Time;
Var
  lResult : string;
begin
  lResult := TMacroExpander.ExpandMacros( 'Time {{TIME}}', 'test.pas', 1 );

  Assert.Contains( lResult, 'Time ' );
  Assert.Contains( lResult, ':' );
end;

procedure TTestMacroExpander.TestExpandMacros_AllMacros;
Var
  lResult : string;
begin
  lResult := TMacroExpander.ExpandMacros( 'Line {{LINE_NUMBER}} in {{FILE_NAME}} - {{DATE}} {{TIME}}', 'test.pas', 10 );

  Assert.Contains( lResult, 'Line 10' );
  Assert.Contains( lResult, 'test.pas' );
  Assert.Contains( lResult, '2025-' );
  Assert.Contains( lResult, ':' );
end;

procedure TTestMacroExpander.TestStrReplace_WithMacros;
Var
  lResult : TOperationResult;
begin
  CreateTestFile( 'interface'#13#10'implementation'#13#10 );

  lResult := TStringOperations.StrReplace( fTestFilePath, 'interface', 'interface // Line {{LINE_NUMBER}}', 0, 0 );

  Assert.IsTrue( lResult.Success, 'StrReplace should succeed' );
  Assert.Contains( ReadTestFile, 'interface // Line 1' );
end;

procedure TTestMacroExpander.TestRegexReplace_WithMacros;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'fValue : Integer;'#13#10'fName : string;'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'f(\w+)', 'l$1 // Line {{LINE_NUMBER}}', 0, 0, false, false );

  Assert.IsTrue( lResult.Success, 'RegexReplace should succeed' );
  Assert.Contains( ReadTestFile, 'lValue // Line 1' );
  Assert.Contains( ReadTestFile, 'lName // Line 2' );
end;

procedure TTestMacroExpander.TestRegexReplace_WithMacrosAndCaptureGroups;
Var
  lResult : TRegexOperationResult;
begin
  CreateTestFile( 'fValue : Integer;'#13#10'fName : string;'#13#10 );

  lResult := TRegexOperations.RegexReplace( fTestFilePath, 'f(\w+)', 'l$1 // {{FILE_NAME}} Line {{LINE_NUMBER}}', 0, 0, false, false );

  Assert.IsTrue( lResult.Success, 'RegexReplace should succeed' );
  Assert.Contains( ReadTestFile, 'lValue' );
  Assert.Contains( ReadTestFile, 'lName' );
  Assert.Contains( ReadTestFile, fTestFilePath );
  Assert.Contains( ReadTestFile, 'Line 1' );
  Assert.Contains( ReadTestFile, 'Line 2' );
end;

end.

