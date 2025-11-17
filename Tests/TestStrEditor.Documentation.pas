Unit TestStrEditor.Documentation;

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, System.IOUtils
, StrEditor.Documentation
;

Type
  [TestFixture]
  TTestDocumentation = class
    strict private
      fTestDocPath : string;

      procedure CreateTestDocFile( const aContent : string );
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestShowDocs_FileExists;

      [Test]
      procedure TestShowDocs_FileNotFound;

      [Test]
      procedure TestShowDocs_WithHead;

      [Test]
      procedure TestShowDocs_WithTail;

      [Test]
      procedure TestShowDocs_WithLineNumbers;

      [Test]
      procedure TestGetDocumentationPath_ExeDirectory;

      [Test]
      procedure TestGetDocumentationPath_CustomFile;

      [Test]
      procedure TestOpenInBrowser_FileExists;

      [Test]
      procedure TestOpenInBrowser_FileNotFound;
  end;

implementation

{ TTestDocumentation }

procedure TTestDocumentation.Setup;
begin
  fTestDocPath := TPath.Combine( TPath.GetTempPath, 'README.md' );
end;

procedure TTestDocumentation.TearDown;
begin
  if TFile.Exists( fTestDocPath ) then
    TFile.Delete( fTestDocPath );
end;

procedure TTestDocumentation.CreateTestDocFile( const aContent : string );
begin
  TFile.WriteAllText( fTestDocPath, aContent, TEncoding.UTF8 );
end;

procedure TTestDocumentation.TestShowDocs_FileExists;
Var
  lResult : TDocumentationResult;
begin
  CreateTestDocFile( '# Test Documentation' + sLineBreak + 'Line 1' + sLineBreak + 'Line 2' );

  lResult := TDocumentation.ShowDocs( fTestDocPath, 0, 0, false );

  Assert.IsTrue( lResult.Success, 'ShowDocs should succeed' );
  Assert.Contains( lResult.Content, '# Test Documentation' );
  Assert.Contains( lResult.Content, 'Line 1' );
  Assert.Contains( lResult.Content, 'Line 2' );
end;

procedure TTestDocumentation.TestShowDocs_FileNotFound;
Var
  lResult : TDocumentationResult;
begin
  lResult := TDocumentation.ShowDocs( 'NonExistent.md', 0, 0, false );

  Assert.IsFalse( lResult.Success, 'ShowDocs should fail for non-existent file' );
  Assert.Contains( lResult.ErrorMessage, 'not found' );
end;

procedure TTestDocumentation.TestShowDocs_WithHead;
Var
  lResult : TDocumentationResult;
begin
  CreateTestDocFile( 'Line 1' + sLineBreak + 'Line 2' + sLineBreak + 'Line 3' + sLineBreak + 'Line 4' );

  lResult := TDocumentation.ShowDocs( fTestDocPath, 2, 0, false );

  Assert.IsTrue( lResult.Success );
  Assert.Contains( lResult.Content, 'Line 1' );
  Assert.Contains( lResult.Content, 'Line 2' );
  Assert.DoesNotContain( lResult.Content, 'Line 3' );
  Assert.DoesNotContain( lResult.Content, 'Line 4' );
end;

procedure TTestDocumentation.TestShowDocs_WithTail;
Var
  lResult : TDocumentationResult;
begin
  CreateTestDocFile( 'Line 1' + sLineBreak + 'Line 2' + sLineBreak + 'Line 3' + sLineBreak + 'Line 4' );

  lResult := TDocumentation.ShowDocs( fTestDocPath, 0, 2, false );

  Assert.IsTrue( lResult.Success );
  Assert.DoesNotContain( lResult.Content, 'Line 1' );
  Assert.DoesNotContain( lResult.Content, 'Line 2' );
  Assert.Contains( lResult.Content, 'Line 3' );
  Assert.Contains( lResult.Content, 'Line 4' );
end;

procedure TTestDocumentation.TestShowDocs_WithLineNumbers;
Var
  lResult : TDocumentationResult;
begin
  CreateTestDocFile( 'Line 1' + sLineBreak + 'Line 2' );

  lResult := TDocumentation.ShowDocs( fTestDocPath, 0, 0, true );

  Assert.IsTrue( lResult.Success );
  Assert.Contains( lResult.Content, '1:' );
  Assert.Contains( lResult.Content, '2:' );
end;

procedure TTestDocumentation.TestGetDocumentationPath_ExeDirectory;
Var
  lPath : string;
begin
  lPath := TDocumentation.GetDocumentationPath( '' );

  Assert.IsNotEmpty( lPath );
  Assert.Contains( lPath, 'README.md' );
end;

procedure TTestDocumentation.TestGetDocumentationPath_CustomFile;
Var
  lPath : string;
begin
  lPath := TDocumentation.GetDocumentationPath( 'CHANGELOG.md' );

  Assert.IsNotEmpty( lPath );
  Assert.Contains( lPath, 'CHANGELOG.md' );
end;

procedure TTestDocumentation.TestOpenInBrowser_FileExists;
Var
  lResult : TDocumentationResult;
begin
  CreateTestDocFile( '# Test Documentation' + sLineBreak + 'This is a test.' );

  lResult := TDocumentation.OpenInBrowser( fTestDocPath );

  Assert.IsTrue( lResult.Success, 'OpenInBrowser should succeed for existing file' );
  Assert.IsNotEmpty( lResult.Content );
  Assert.Contains( lResult.Content, 'Opened in browser' );
end;

procedure TTestDocumentation.TestOpenInBrowser_FileNotFound;
Var
  lResult : TDocumentationResult;
begin
  lResult := TDocumentation.OpenInBrowser( 'C:\NonExistent\File.md' );

  Assert.IsFalse( lResult.Success, 'OpenInBrowser should fail for non-existent file' );
  Assert.IsNotEmpty( lResult.ErrorMessage );
  Assert.Contains( lResult.ErrorMessage, 'not found' );
end;

end.

