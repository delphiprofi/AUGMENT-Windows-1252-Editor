Unit TestStrEditor.ChangeReport;

{***************************************************************************
 * Tests für StrEditor ChangeReport
 *
 * Testet die automatische Ausgabe von Änderungen nach Operationen
 ***************************************************************************}

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, System.IOUtils
, StrEditor.CommandLine
, StrEditor.Operations
, StrEditor.ChangeReport
, StrEditor.Encoding
, StrEditor.BatchProcessor
;

Type
  [TestFixture]
  TTestChangeReport = class
    strict private
      fTestFilePath : string;
      fOutput       : TStringList;

      procedure CreateTestFile( const aContent : string );
      function  ReadTestFile : string;
      procedure CaptureOutput( const aLine : string );
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestSingleInsert_ShowsInsertedLines;

      [Test]
      procedure TestSingleDelete_ShowsDeletedLine;

      [Test]
      procedure TestSingleReplace_ShowsOldAndNew;

      [Test]
      procedure TestStrReplace_ShowsChangedLine;

      [Test]
      procedure TestBatchOperations_ShowsAllChanges;

      [Test]
      procedure TestBatchOperations_ShowsLineMappings;

      [Test]
      procedure TestChangeRecord_StoresCorrectData;

      [Test]
      procedure TestOutputFormat_ContainsMarkers;
  end;

implementation

{ TTestChangeReport }

procedure TTestChangeReport.Setup;
begin
  fTestFilePath := TPath.Combine( TPath.GetTempPath, 'TestChangeReport.pas' );
  fOutput       := TStringList.Create;
end;

procedure TTestChangeReport.TearDown;
begin
  if TFile.Exists( fTestFilePath ) then
    TFile.Delete( fTestFilePath );

  fOutput.Free;
end;

procedure TTestChangeReport.CreateTestFile( const aContent : string );
Var
  lBytes : TBytes;
begin
  lBytes := TEncodingHelper.StringToWindows1252( aContent );
  TFile.WriteAllBytes( fTestFilePath, lBytes );
end;

function TTestChangeReport.ReadTestFile : string;
Var
  lBytes : TBytes;
begin
  lBytes := TFile.ReadAllBytes( fTestFilePath );
  Result := TEncodingHelper.Windows1252ToString( lBytes );
end;

procedure TTestChangeReport.CaptureOutput( const aLine : string );
begin
  fOutput.Add( aLine );
end;

procedure TTestChangeReport.TestSingleInsert_ShowsInsertedLines;
Var
  lReport  : TChangeReport;
  lOutput  : string;
begin
  // Arrange: Datei mit 5 Zeilen
  CreateTestFile( 'Line 1' + #13#10 +
                  'Line 2' + #13#10 +
                  'Line 3' + #13#10 +
                  'Line 4' + #13#10 +
                  'Line 5' );

  lReport := TChangeReport.Create( fTestFilePath );
  try
    // Act: Füge nach Zeile 2 zwei neue Zeilen ein
    lReport.BeginOperation( ctInsert, 2 );
    // Simuliere die Operation
    lReport.SetInsertedText( 'New Line A' + #13#10 + 'New Line B' );
    lReport.EndOperation( true, '' );

    lOutput := lReport.GenerateReport;

    // Debug-Ausgabe des Reports
    WriteLn( '--- ACTUAL REPORT OUTPUT ---' );
    WriteLn( lOutput );
    WriteLn( '--- END OF REPORT ---' );

    // Assert: Report enthält die eingefügten Zeilen
    Assert.Contains( lOutput, 'INSERT', 'Should contain INSERT marker' );
    Assert.Contains( lOutput, 'New Line A', 'Should show first inserted line' );
    Assert.Contains( lOutput, 'New Line B', 'Should show second inserted line' );
    Assert.Contains( lOutput, 'after line 2', 'Should reference original line number' );
  finally
    lReport.Free;
  end;
end;

procedure TTestChangeReport.TestSingleDelete_ShowsDeletedLine;
Var
  lReport : TChangeReport;
  lOutput : string;
begin
  // Arrange: Datei mit 5 Zeilen
  CreateTestFile( 'Line 1' + #13#10 +
                  'Line 2' + #13#10 +
                  'Line 3' + #13#10 +
                  'Line 4' + #13#10 +
                  'Line 5' );

  lReport := TChangeReport.Create( fTestFilePath );
  try
    // Act: Lösche Zeile 3
    lReport.BeginOperation( ctDeleteLine, 3 );
    lReport.SetOldContent( 'Line 3' );
    lReport.EndOperation( true, '' );

    lOutput := lReport.GenerateReport;

    // Debug-Ausgabe
    WriteLn( '--- DELETE REPORT ---' );
    WriteLn( lOutput );

    // Assert
    Assert.Contains( lOutput, 'DELETE', 'Should contain DELETE marker' );
    Assert.Contains( lOutput, 'Line 3', 'Should show deleted line content' );
    Assert.Contains( lOutput, 'line 3', 'Should reference original line number' );
  finally
    lReport.Free;
  end;
end;

procedure TTestChangeReport.TestSingleReplace_ShowsOldAndNew;
Var
  lReport : TChangeReport;
  lOutput : string;
begin
  // Arrange
  CreateTestFile( 'Line 1' + #13#10 +
                  'Line 2' + #13#10 +
                  'Line 3' + #13#10 +
                  'Line 4' + #13#10 +
                  'Line 5' );

  lReport := TChangeReport.Create( fTestFilePath );
  try
    // Act: Ersetze Zeile 2
    lReport.BeginOperation( ctReplaceLine, 2 );
    lReport.SetOldContent( 'Line 2' );
    lReport.SetInsertedText( 'New Line 2' );
    lReport.EndOperation( true, '' );

    lOutput := lReport.GenerateReport;

    // Debug-Ausgabe
    WriteLn( '--- REPLACE LINE REPORT ---' );
    WriteLn( lOutput );

    // Assert - Format hat sich geändert: Jetzt mit Zeilennummern und -/+ Markern
    Assert.Contains( lOutput, 'REPLACE', 'Should contain REPLACE marker' );
    Assert.Contains( lOutput, '- ', 'Should show old content marker' );
    Assert.Contains( lOutput, 'Line 2', 'Should show old content' );
    Assert.Contains( lOutput, '+ ', 'Should show new content marker' );
    Assert.Contains( lOutput, 'New Line 2', 'Should show new content' );
  finally
    lReport.Free;
  end;
end;

procedure TTestChangeReport.TestStrReplace_ShowsChangedLine;
Var
  lReport : TChangeReport;
  lOutput : string;
begin
  // Arrange
  CreateTestFile( 'foo bar foo' + #13#10 +
                  'baz qux' + #13#10 +
                  'foo again' );

  lReport := TChangeReport.Create( fTestFilePath );
  try
    // Act: String-Ersetzung in Zeile 1
    lReport.BeginOperation( ctStrReplace, 1 );
    lReport.SetOldContent( 'foo bar foo' );
    lReport.SetInsertedText( 'FOO bar FOO' );
    lReport.EndOperation( true, '' );

    lOutput := lReport.GenerateReport;

    // Debug-Ausgabe
    WriteLn( '--- STR-REPLACE REPORT ---' );
    WriteLn( lOutput );

    // Assert - Format hat sich geändert: Jetzt mit Zeilennummern und -/+ Markern
    Assert.Contains( lOutput, 'STR-REPLACE', 'Should contain STR-REPLACE marker' );
    Assert.Contains( lOutput, '- ', 'Should show old content marker' );
    Assert.Contains( lOutput, 'foo bar foo', 'Should show original line' );
    Assert.Contains( lOutput, '+ ', 'Should show new content marker' );
    Assert.Contains( lOutput, 'FOO bar FOO', 'Should show modified line' );
  finally
    lReport.Free;
  end;
end;

procedure TTestChangeReport.TestBatchOperations_ShowsAllChanges;
Var
  lReport : TChangeReport;
  lOutput : string;
begin
  // Arrange
  CreateTestFile( 'Line 1' + #13#10 +
                  'Line 2' + #13#10 +
                  'Line 3' + #13#10 +
                  'Line 4' + #13#10 +
                  'Line 5' );

  lReport := TChangeReport.Create( fTestFilePath );
  try
    // Act: Mehrere Operationen
    // 1. Lösche Zeile 2
    lReport.BeginOperation( ctDeleteLine, 2 );
    lReport.SetOldContent( 'Line 2' );
    lReport.EndOperation( true, '' );

    // 2. Füge nach Zeile 4 (orig) ein
    lReport.BeginOperation( ctInsert, 4 );
    lReport.SetInsertedText( 'New After 4' );
    lReport.EndOperation( true, '' );

    // 3. Ersetze Zeile 5 (orig)
    lReport.BeginOperation( ctReplaceLine, 5 );
    lReport.SetOldContent( 'Line 5' );
    lReport.SetInsertedText( 'Modified Line 5' );
    lReport.EndOperation( true, '' );

    lOutput := lReport.GenerateReport;

    // Debug-Ausgabe
    WriteLn( '--- BATCH REPORT ---' );
    WriteLn( lOutput );

    // Assert
    Assert.Contains( lOutput, 'Operations: 3', 'Should show 3 operations' );
    Assert.Contains( lOutput, 'DELETE', 'Should contain DELETE' );
    Assert.Contains( lOutput, 'INSERT', 'Should contain INSERT' );
    Assert.Contains( lOutput, 'REPLACE', 'Should contain REPLACE' );
  finally
    lReport.Free;
  end;
end;

procedure TTestChangeReport.TestBatchOperations_ShowsLineMappings;
Var
  lReport     : TChangeReport;
  lOutput     : string;
  lOperations : TArray<TCommandLineParams>;
  lOp         : TCommandLineParams;
begin
  // Arrange: Datei mit 10 Zeilen
  CreateTestFile( 'Line 1' + #13#10 +
                  'Line 2' + #13#10 +
                  'Line 3' + #13#10 +
                  'Line 4' + #13#10 +
                  'Line 5' + #13#10 +
                  'Line 6' + #13#10 +
                  'Line 7' + #13#10 +
                  'Line 8' + #13#10 +
                  'Line 9' + #13#10 +
                  'Line 10' );

  lReport := TChangeReport.Create( fTestFilePath );

  try
    // Erstelle Batch-Operationen (alle beziehen sich auf ORIGINAL-Zeilennummern)
    SetLength( lOperations, 3 );

    // 1. Lösche Zeile 3 (orig)
    lOp := Default( TCommandLineParams );
    lOp.Command    := ctDeleteLine;
    lOp.LineNumber := 3;
    lOperations[ 0 ] := lOp;

    // 2. Füge nach Zeile 5 (orig) ein
    lOp := Default( TCommandLineParams );
    lOp.Command         := ctInsert;
    lOp.InsertAfterLine := 5;
    lOp.Text            := 'New Line After 5';
    lOperations[ 1 ] := lOp;

    // 3. Ersetze Zeile 8 (orig)
    lOp := Default( TCommandLineParams );
    lOp.Command    := ctReplaceLine;
    lOp.LineNumber := 8;
    lOp.Text       := 'Replaced Line 8';
    lOperations[ 2 ] := lOp;

    // Act: Führe Batch-Operationen mit ChangeReport aus
    TBatchProcessor.ProcessLineOperations( fTestFilePath, lOperations, lReport );

    lOutput := lReport.GenerateReport;

    // Debug-Ausgabe
    WriteLn( '--- BATCH WITH LINE MAPPINGS ---' );
    WriteLn( lOutput );

    // Assert: Report enthält alle Operationen
    Assert.AreEqual( 3, lReport.GetChangeCount, 'Should have 3 changes' );
    Assert.Contains( lOutput, 'DELETE', 'Should contain DELETE' );
    Assert.Contains( lOutput, 'INSERT', 'Should contain INSERT' );
    Assert.Contains( lOutput, 'REPLACE', 'Should contain REPLACE' );

    // Assert: Original-Zeilennummern werden angezeigt
    Assert.Contains( lOutput, 'line 3', 'Should reference original line 3' );
    Assert.Contains( lOutput, 'line 5', 'Should reference original line 5' );
    Assert.Contains( lOutput, 'line 8', 'Should reference original line 8' );

    // Assert: Gelöschter Inhalt wird angezeigt
    Assert.Contains( lOutput, 'Line 3', 'Should show deleted content' );

    // Assert: Eingefügter Inhalt wird angezeigt
    Assert.Contains( lOutput, 'New Line After 5', 'Should show inserted content' );

    // Assert: Ersetzter Inhalt wird angezeigt
    Assert.Contains( lOutput, 'Replaced Line 8', 'Should show replacement content' );
  finally
    lReport.Free;
  end;
end;

procedure TTestChangeReport.TestChangeRecord_StoresCorrectData;
Var
  lReport : TChangeReport;
begin
  // Arrange
  CreateTestFile( 'Test Line' );

  lReport := TChangeReport.Create( fTestFilePath );
  try
    // Act
    lReport.BeginOperation( ctInsert, 5 );
    lReport.SetOldContent( 'Old Content' );
    lReport.SetInsertedText( 'New Content' );
    lReport.EndOperation( true, '' );

    // Assert über GetChanges
    Assert.AreEqual( 1, lReport.GetChangeCount, 'Should have 1 change' );
  finally
    lReport.Free;
  end;
end;

procedure TTestChangeReport.TestOutputFormat_ContainsMarkers;
Var
  lReport : TChangeReport;
  lOutput : string;
begin
  // Arrange
  CreateTestFile( 'Test Line' );

  lReport := TChangeReport.Create( fTestFilePath );
  try
    lReport.BeginOperation( ctInsert, 1 );
    lReport.SetInsertedText( 'New Line' );
    lReport.EndOperation( true, '' );

    lOutput := lReport.GenerateReport;

    // Assert: Format-Marker
    Assert.Contains( lOutput, '=== CHANGE REPORT ===', 'Should have header marker' );
    Assert.Contains( lOutput, '=====================', 'Should have footer marker' );
    Assert.Contains( lOutput, 'File:', 'Should show file path' );
    Assert.Contains( lOutput, 'Operations:', 'Should show operation count' );
  finally
    lReport.Free;
  end;
end;

end.

