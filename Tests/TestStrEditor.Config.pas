Unit TestStrEditor.Config;

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, System.IOUtils
, System.JSON
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
    DeleteFile( fTestFilePath );

  if FileExists( fTestConfigPath ) then
    DeleteFile( fTestConfigPath );

  if FileExists( fTestFilePath + '.bak' ) then
    DeleteFile( fTestFilePath + '.bak' );
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

end.

