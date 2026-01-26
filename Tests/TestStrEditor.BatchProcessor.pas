Unit TestStrEditor.BatchProcessor;

{***************************************************************************
 * Tests für StrEditor BatchProcessor
 *
 * Testet die korrekte Sortierung und Verarbeitung von gemischten
 * Line-Operationen (delete, insert, replace)
 ***************************************************************************}

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, System.IOUtils
, StrEditor.CommandLine
, StrEditor.Operations
, StrEditor.BatchProcessor
, StrEditor.Encoding
;

Type
  [TestFixture]
  TTestBatchProcessor = class
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
      procedure TestHasLineOperations_WithInsert;

      [Test]
      procedure TestHasLineOperations_WithInsertBefore;

      [Test]
      procedure TestMixedOperations_DeleteAndInsertBefore;

      [Test]
      procedure TestMixedOperations_InsertBeforeAndAfter;

      [Test]
      procedure TestSorting_HighestLineFirst;

      [Test]
      procedure TestLineNumberAdjustment_MultipleInserts;

      [Test]
      procedure TestOriginalLineNumbers_BasicExample;

      [Test]
      procedure TestOriginalLineNumbers_RemoveAndAddLogBlocks;
  end;

implementation

{ TTestBatchProcessor }

procedure TTestBatchProcessor.Setup;
begin
  fTestFilePath := TPath.Combine( TPath.GetTempPath, 'TestBatchProcessor.pas' );
end;

procedure TTestBatchProcessor.TearDown;
begin
  if TFile.Exists( fTestFilePath ) then
    TFile.Delete( fTestFilePath );
end;

procedure TTestBatchProcessor.CreateTestFile( const aContent : string );
Var
  lBytes : TBytes;
begin
  lBytes := TEncodingHelper.StringToWindows1252( aContent );
  TFile.WriteAllBytes( fTestFilePath, lBytes );
end;

function TTestBatchProcessor.ReadTestFile : string;
Var
  lBytes : TBytes;
begin
  lBytes := TFile.ReadAllBytes( fTestFilePath );
  Result := TEncodingHelper.Windows1252ToString( lBytes );
end;

procedure TTestBatchProcessor.TestHasLineOperations_WithInsert;
Var
  lOps : TArray<TCommandLineParams>;
  lOp  : TCommandLineParams;
begin
  // Arrange: Eine insert-after Operation
  SetLength( lOps, 1 );
  lOp.Command         := ctInsert;
  lOp.InsertAfterLine := 5;
  lOp.Text            := '// Comment';
  lOps[ 0 ]           := lOp;

  // Act & Assert
  Assert.IsTrue( TBatchProcessor.HasLineOperations( lOps ),
    'HasLineOperations should return true for ctInsert' );
end;

procedure TTestBatchProcessor.TestHasLineOperations_WithInsertBefore;
Var
  lOps : TArray<TCommandLineParams>;
  lOp  : TCommandLineParams;
begin
  // Arrange: Eine insert-before Operation
  SetLength( lOps, 1 );
  lOp.Command          := ctInsertBefore;
  lOp.InsertBeforeLine := 5;
  lOp.Text             := '// Comment';
  lOps[ 0 ]            := lOp;

  // Act & Assert
  Assert.IsTrue( TBatchProcessor.HasLineOperations( lOps ),
    'HasLineOperations should return true for ctInsertBefore' );
end;

procedure TTestBatchProcessor.TestMixedOperations_DeleteAndInsertBefore;
Var
  lOps    : TArray<TCommandLineParams>;
  lOp     : TCommandLineParams;
  lResult : Boolean;
  lContent: string;
begin
  // Arrange: Datei mit 5 Zeilen
  CreateTestFile( 'Line 1' + #13#10 +
                  'Line 2' + #13#10 +
                  'Line 3' + #13#10 +
                  'Line 4' + #13#10 +
                  'Line 5' );

  SetLength( lOps, 2 );

  // Operation 1: Lösche Zeile 3
  lOp := Default( TCommandLineParams );  // Record initialisieren!
  lOp.Command    := ctDeleteLine;
  lOp.LineNumber := 3;
  lOp.Diff       := true;  // Für Debug-Ausgabe
  lOps[ 0 ]      := lOp;

  // Operation 2: Füge vor Zeile 2 "New Line" ein
  lOp := Default( TCommandLineParams );  // Record initialisieren!
  lOp.Command          := ctInsertBefore;
  lOp.InsertBeforeLine := 2;
  lOp.Text             := 'New Line';
  lOp.Diff             := true;  // Für Debug-Ausgabe
  lOps[ 1 ]            := lOp;

  // Act
  lResult := TBatchProcessor.ProcessLineOperations( fTestFilePath, lOps );

  // Assert
  Assert.IsTrue( lResult, 'ProcessLineOperations should succeed' );

  lContent := ReadTestFile;
  // Erwartetes Ergebnis (wenn von hinten nach vorne sortiert):
  // 1. Lösche Zeile 3 -> "Line 1", "Line 2", "Line 4", "Line 5"
  // 2. Füge vor Zeile 2 ein -> "Line 1", "New Line", "Line 2", "Line 4", "Line 5"
  Assert.Contains( lContent, 'New Line', 'Content should contain inserted line' );
  Assert.IsFalse( lContent.Contains( 'Line 3' ), 'Line 3 should be deleted' );
end;

procedure TTestBatchProcessor.TestMixedOperations_InsertBeforeAndAfter;
Var
  lOps    : TArray<TCommandLineParams>;
  lOp     : TCommandLineParams;
  lResult : Boolean;
  lContent: string;
begin
  // Arrange: Datei mit 3 Zeilen
  CreateTestFile( 'Line 1' + #13#10 +
                  'Line 2' + #13#10 +
                  'Line 3' );

  SetLength( lOps, 2 );

  // Operation 1: Füge nach Zeile 3 "After Line 3" ein
  lOp := Default( TCommandLineParams );  // Record initialisieren!
  lOp.Command         := ctInsert;
  lOp.InsertAfterLine := 3;
  lOp.Text            := 'After Line 3';
  lOp.Diff            := true;  // Für Debug-Ausgabe
  lOps[ 0 ]           := lOp;

  // Operation 2: Füge vor Zeile 1 "Before Line 1" ein
  lOp := Default( TCommandLineParams );  // Record initialisieren!
  lOp.Command          := ctInsertBefore;
  lOp.InsertBeforeLine := 1;
  lOp.Text             := 'Before Line 1';
  lOp.Diff             := true;  // Für Debug-Ausgabe
  lOps[ 1 ]            := lOp;

  // Act
  lResult := TBatchProcessor.ProcessLineOperations( fTestFilePath, lOps );

  // Assert
  Assert.IsTrue( lResult, 'ProcessLineOperations should succeed' );

  lContent := ReadTestFile;
  // Erwartetes Ergebnis:
  // "Before Line 1", "Line 1", "Line 2", "Line 3", "After Line 3"
  Assert.Contains( lContent, 'Before Line 1', 'Should contain line inserted before' );
  Assert.Contains( lContent, 'After Line 3', 'Should contain line inserted after' );
end;

procedure TTestBatchProcessor.TestSorting_HighestLineFirst;
Var
  lOps    : TArray<TCommandLineParams>;
  lOp     : TCommandLineParams;
  lResult : Boolean;
  lContent: string;
begin
  // Arrange: Datei mit 10 Zeilen
  CreateTestFile( 'Line 01' + #13#10 +
                  'Line 02' + #13#10 +
                  'Line 03' + #13#10 +
                  'Line 04' + #13#10 +
                  'Line 05' + #13#10 +
                  'Line 06' + #13#10 +
                  'Line 07' + #13#10 +
                  'Line 08' + #13#10 +
                  'Line 09' + #13#10 +
                  'Line 10' );

  SetLength( lOps, 3 );

  // Operationen in "falscher" Reihenfolge (niedrige Zeile zuerst)
  // Der BatchProcessor sollte diese nach höchster Zeilennummer sortieren

  // Operation 1: Füge vor Zeile 2 ein (wird als letztes ausgeführt)
  lOp := Default( TCommandLineParams );  // Record initialisieren!
  lOp.Command          := ctInsertBefore;
  lOp.InsertBeforeLine := 2;
  lOp.Text             := 'Inserted at 2';
  lOp.Diff             := true;  // Für Debug-Ausgabe
  lOps[ 0 ]            := lOp;

  // Operation 2: Lösche Zeile 8 (wird als erstes ausgeführt)
  lOp := Default( TCommandLineParams );  // Record initialisieren!
  lOp.Command    := ctDeleteLine;
  lOp.LineNumber := 8;
  lOp.Diff       := true;  // Für Debug-Ausgabe
  lOps[ 1 ]      := lOp;

  // Operation 3: Ersetze Zeile 5 (wird als zweites ausgeführt)
  lOp := Default( TCommandLineParams );  // Record initialisieren!
  lOp.Command    := ctReplaceLine;
  lOp.LineNumber := 5;
  lOp.Text       := 'Replaced Line 5';
  lOp.Diff       := true;  // Für Debug-Ausgabe
  lOps[ 2 ]      := lOp;

  // Act
  lResult := TBatchProcessor.ProcessLineOperations( fTestFilePath, lOps );

  // Assert
  Assert.IsTrue( lResult, 'ProcessLineOperations should succeed' );

  lContent := ReadTestFile;
  // Prüfe dass alle Operationen durchgeführt wurden
  Assert.Contains( lContent, 'Inserted at 2', 'Should contain inserted line' );
  Assert.Contains( lContent, 'Replaced Line 5', 'Should contain replaced line' );
  Assert.IsFalse( lContent.Contains( 'Line 08' ), 'Line 08 should be deleted' );
end;

procedure TTestBatchProcessor.TestLineNumberAdjustment_MultipleInserts;
Var
  lOps     : TArray<TCommandLineParams>;
  lOp      : TCommandLineParams;
  lResult  : Boolean;
  lContent : string;
  lLines   : TArray<string>;
begin
  // Arrange: Datei mit 5 Zeilen
  // Dieser Test prüft, ob die Zeilennummern korrekt bleiben wenn mehrere Inserts passieren
  CreateTestFile( 'Line 1' + #13#10 +
                  'Line 2' + #13#10 +
                  'Line 3' + #13#10 +
                  'Line 4' + #13#10 +
                  'Line 5' );

  SetLength( lOps, 3 );

  // Wir geben die Operationen in "falscher" Reihenfolge an (niedrige Zeile zuerst)
  // Der BatchProcessor sollte sie sortieren und von hinten nach vorne ausführen

  // Operation 1: Füge nach Zeile 2 "Insert A" ein
  lOp := Default( TCommandLineParams );
  lOp.Command         := ctInsert;
  lOp.InsertAfterLine := 2;
  lOp.Text            := 'Insert A';
  lOps[ 0 ]           := lOp;

  // Operation 2: Füge nach Zeile 4 "Insert B" ein
  lOp := Default( TCommandLineParams );
  lOp.Command         := ctInsert;
  lOp.InsertAfterLine := 4;
  lOp.Text            := 'Insert B';
  lOps[ 1 ]           := lOp;

  // Operation 3: Füge nach Zeile 3 "Insert C" ein
  lOp := Default( TCommandLineParams );
  lOp.Command         := ctInsert;
  lOp.InsertAfterLine := 3;
  lOp.Text            := 'Insert C';
  lOps[ 2 ]           := lOp;

  // Erwartete Ausführungsreihenfolge (höchste Zeile zuerst):
  // 1. Insert nach Zeile 4 -> "Insert B" (Zeilen: 1,2,3,4,InsertB,5)
  // 2. Insert nach Zeile 3 -> "Insert C" (Zeilen: 1,2,3,InsertC,4,InsertB,5)
  // 3. Insert nach Zeile 2 -> "Insert A" (Zeilen: 1,2,InsertA,3,InsertC,4,InsertB,5)

  // Act
  lResult := TBatchProcessor.ProcessLineOperations( fTestFilePath, lOps );

  // Assert
  Assert.IsTrue( lResult, 'ProcessLineOperations should succeed' );

  lContent := ReadTestFile;
  lLines   := lContent.Split( [ #13#10 ], TStringSplitOptions.None );

  // Erwartete Reihenfolge: Line 1, Line 2, Insert A, Line 3, Insert C, Line 4, Insert B, Line 5
  // Hinweis: Split kann eine leere Zeile am Ende erzeugen wenn die Datei mit CRLF endet
  Assert.IsTrue( Length( lLines ) >= 8, 'Should have at least 8 lines (5 original + 3 inserts)' );
  Assert.AreEqual( 'Line 1',   lLines[ 0 ], 'Line 1 should be at position 0' );
  Assert.AreEqual( 'Line 2',   lLines[ 1 ], 'Line 2 should be at position 1' );
  Assert.AreEqual( 'Insert A', lLines[ 2 ], 'Insert A should be at position 2 (after Line 2)' );
  Assert.AreEqual( 'Line 3',   lLines[ 3 ], 'Line 3 should be at position 3' );
  Assert.AreEqual( 'Insert C', lLines[ 4 ], 'Insert C should be at position 4 (after Line 3)' );
  Assert.AreEqual( 'Line 4',   lLines[ 5 ], 'Line 4 should be at position 5' );
  Assert.AreEqual( 'Insert B', lLines[ 6 ], 'Insert B should be at position 6 (after Line 4)' );
  Assert.AreEqual( 'Line 5',   lLines[ 7 ], 'Line 5 should be at position 7' );
end;

procedure TTestBatchProcessor.TestOriginalLineNumbers_BasicExample;
// Dieses Beispiel aus der Dokumentation:
// Original:
//   1 #ZEILE1
//   2 #ZEILE2
//   3 #ZEILE3
//   4 #ZEILE4
//   5 #ZEILE5
//   6 #ZEILE6
//
// Operationen (alle Zeilennummern beziehen sich auf ORIGINAL):
//   - Loesche Zeile 2
//   - Loesche Zeile 5
//   - Aendere Zeile 3 von "#ZEILE3" nach ["#ZEILE3a", "#ZEILE3b"]
//   - Fuege vor Zeile 2 den Text "#ZEILE1a" ein
//   - Fuege nach Zeile 4 den Text "#ZEILE4a" ein
//
// Erwartetes Ergebnis:
//   1 #ZEILE1
//   2 #ZEILE1a    (eingefuegt vor Original-Zeile 2)
//   3 #ZEILE3a    (Original-Zeile 3 ersetzt)
//   4 #ZEILE3b
//   5 #ZEILE4
//   6 #ZEILE4a    (eingefuegt nach Original-Zeile 4)
//   7 #ZEILE6
Var
  lOps     : TArray<TCommandLineParams>;
  lOp      : TCommandLineParams;
  lResult  : Boolean;
  lContent : string;
  lLines   : TArray<string>;
begin
  // Arrange: Original-Datei mit 6 Zeilen
  CreateTestFile( '#ZEILE1' + #13#10 +
                  '#ZEILE2' + #13#10 +
                  '#ZEILE3' + #13#10 +
                  '#ZEILE4' + #13#10 +
                  '#ZEILE5' + #13#10 +
                  '#ZEILE6' );

  SetLength( lOps, 5 );

  // Operation 1: Loesche Zeile 2 (Original)
  lOp := Default( TCommandLineParams );
  lOp.Command    := ctDeleteLine;
  lOp.LineNumber := 2;  // Original-Zeile 2 = #ZEILE2
  lOps[ 0 ]      := lOp;

  // Operation 2: Loesche Zeile 5 (Original)
  lOp := Default( TCommandLineParams );
  lOp.Command    := ctDeleteLine;
  lOp.LineNumber := 5;  // Original-Zeile 5 = #ZEILE5
  lOps[ 1 ]      := lOp;

  // Operation 3: Aendere Zeile 3 (Original) - wird zu 2 Zeilen
  lOp := Default( TCommandLineParams );
  lOp.Command    := ctReplaceLine;
  lOp.LineNumber := 3;  // Original-Zeile 3 = #ZEILE3
  lOp.Text       := '#ZEILE3a' + #13#10 + '#ZEILE3b';
  lOps[ 2 ]      := lOp;

  // Operation 4: Fuege vor Zeile 2 (Original) ein
  lOp := Default( TCommandLineParams );
  lOp.Command          := ctInsertBefore;
  lOp.InsertBeforeLine := 2;  // Vor Original-Zeile 2 = vor #ZEILE2
  lOp.Text             := '#ZEILE1a';
  lOps[ 3 ]            := lOp;

  // Operation 5: Fuege nach Zeile 4 (Original) ein
  lOp := Default( TCommandLineParams );
  lOp.Command         := ctInsert;
  lOp.InsertAfterLine := 4;  // Nach Original-Zeile 4 = nach #ZEILE4
  lOp.Text            := '#ZEILE4a';
  lOps[ 4 ]           := lOp;

  // Act
  lResult := TBatchProcessor.ProcessLineOperations( fTestFilePath, lOps );

  // Assert
  Assert.IsTrue( lResult, 'ProcessLineOperations should succeed' );

  lContent := ReadTestFile;
  lLines   := lContent.Split( [ #13#10 ], TStringSplitOptions.None );

  // Erwartetes Ergebnis: 7 Zeilen (6 - 2 geloescht + 1 ersetzt durch 2 + 2 eingefuegt = 7)
  Assert.IsTrue( Length( lLines ) >= 7, 'Should have at least 7 lines' );
  Assert.AreEqual( '#ZEILE1',  lLines[ 0 ], 'Line 1: Original #ZEILE1' );
  Assert.AreEqual( '#ZEILE1a', lLines[ 1 ], 'Line 2: Inserted before original line 2' );
  Assert.AreEqual( '#ZEILE3a', lLines[ 2 ], 'Line 3: Replaced from original #ZEILE3' );
  Assert.AreEqual( '#ZEILE3b', lLines[ 3 ], 'Line 4: Second line of replacement' );
  Assert.AreEqual( '#ZEILE4',  lLines[ 4 ], 'Line 5: Original #ZEILE4' );
  Assert.AreEqual( '#ZEILE4a', lLines[ 5 ], 'Line 6: Inserted after original line 4' );
  Assert.AreEqual( '#ZEILE6',  lLines[ 6 ], 'Line 7: Original #ZEILE6' );

  // #ZEILE2 und #ZEILE5 sollten NICHT mehr vorhanden sein
  Assert.IsFalse( lContent.Contains( '#ZEILE2' ), '#ZEILE2 should be deleted' );
  Assert.IsFalse( lContent.Contains( '#ZEILE5' ), '#ZEILE5 should be deleted' );
end;

procedure TTestBatchProcessor.TestOriginalLineNumbers_RemoveAndAddLogBlocks;
// Roundtrip-Test: Log-Bloecke entfernen und wieder einfuegen
// Alle Zeilennummern beziehen sich auf den ORIGINAL-Zustand!
const
  cMethodWithLogs =
    'procedure TeAkteTreeFrame.AkteTreeChange( Sender : TObject; Node : TTreeNode );' + #13#10 +  // 1
    'begin' + #13#10 +                                                                            // 2
    '  {$IFDEF FRANK_LOG}' + #13#10 +                                                             // 3
    '  TLog.Debug( ''Start'' );' + #13#10 +                                                       // 4
    '  {$ENDIF}' + #13#10 +                                                                       // 5
    '  if Node = NIL then' + #13#10 +                                                             // 6
    '    begin' + #13#10 +                                                                        // 7
    '      {$IFDEF FRANK_LOG}' + #13#10 +                                                         // 8
    '      TLog.Debug( ''NIL'' );' + #13#10 +                                                     // 9
    '      {$ENDIF}' + #13#10 +                                                                   // 10
    '      Exit;' + #13#10 +                                                                      // 11
    '    end;' + #13#10 +                                                                         // 12
    '  DoSomething;' + #13#10 +                                                                   // 13
    '  {$IFDEF FRANK_LOG}' + #13#10 +                                                             // 14
    '  TLog.Debug( ''End'' );' + #13#10 +                                                         // 15
    '  {$ENDIF}' + #13#10 +                                                                       // 16
    'end;';                                                                                       // 17

  cMethodWithoutLogs =
    'procedure TeAkteTreeFrame.AkteTreeChange( Sender : TObject; Node : TTreeNode );' + #13#10 +
    'begin' + #13#10 +
    '  if Node = NIL then' + #13#10 +
    '    begin' + #13#10 +
    '      Exit;' + #13#10 +
    '    end;' + #13#10 +
    '  DoSomething;' + #13#10 +
    'end;';
Var
  lOps     : TArray<TCommandLineParams>;
  lOp      : TCommandLineParams;
  lResult  : Boolean;
  lContent : string;
begin
  // ========================================
  // TEIL 1: Log-Bloecke ENTFERNEN
  // ========================================
  CreateTestFile( cMethodWithLogs );

  // Log-Bloecke sind auf Original-Zeilen: 3-5, 8-10, 14-16
  // Alle Zeilennummern beziehen sich auf ORIGINAL!
  SetLength( lOps, 3 );

  // Loesche Block 1 (Zeilen 3-5)
  lOp := Default( TCommandLineParams );
  lOp.Command   := ctDeleteLines;
  lOp.StartLine := 3;
  lOp.EndLine   := 5;
  lOps[ 0 ]     := lOp;

  // Loesche Block 2 (Zeilen 8-10)
  lOp := Default( TCommandLineParams );
  lOp.Command   := ctDeleteLines;
  lOp.StartLine := 8;
  lOp.EndLine   := 10;
  lOps[ 1 ]     := lOp;

  // Loesche Block 3 (Zeilen 14-16)
  lOp := Default( TCommandLineParams );
  lOp.Command   := ctDeleteLines;
  lOp.StartLine := 14;
  lOp.EndLine   := 16;
  lOps[ 2 ]     := lOp;

  lResult := TBatchProcessor.ProcessLineOperations( fTestFilePath, lOps );
  Assert.IsTrue( lResult, 'Remove logs should succeed' );

  lContent := ReadTestFile;
  Assert.IsFalse( lContent.Contains( 'FRANK_LOG' ), 'All log blocks should be removed' );
  Assert.IsFalse( lContent.Contains( 'TLog.Debug' ), 'All TLog calls should be removed' );
  Assert.IsTrue( lContent.Contains( 'DoSomething' ), 'DoSomething should remain' );
  Assert.IsTrue( lContent.Contains( 'Exit' ), 'Exit should remain' );

  // ========================================
  // TEIL 2: Log-Bloecke wieder EINFUEGEN
  // ========================================
  // Jetzt haben wir die Version OHNE Logs - fuegen wir sie wieder ein
  // Die Zeilennummern beziehen sich auf den aktuellen Zustand (ohne Logs)

  CreateTestFile( cMethodWithoutLogs );

  // Neue Zeilennummern in der Version ohne Logs:
  // 1: procedure...
  // 2: begin
  // 3: if Node = NIL then
  // 4:   begin
  // 5:     Exit;
  // 6:   end;
  // 7: DoSomething;
  // 8: end;

  SetLength( lOps, 3 );

  // Fuege Log-Block 1 nach "begin" (Zeile 2) ein
  lOp := Default( TCommandLineParams );
  lOp.Command         := ctInsert;
  lOp.InsertAfterLine := 2;
  lOp.Text            := '  {$IFDEF FRANK_LOG}' + #13#10 +
                         '  TLog.Debug( ''Start'' );' + #13#10 +
                         '  {$ENDIF}';
  lOps[ 0 ]           := lOp;

  // Fuege Log-Block 2 vor "Exit" (Zeile 5) ein
  lOp := Default( TCommandLineParams );
  lOp.Command          := ctInsertBefore;
  lOp.InsertBeforeLine := 5;
  lOp.Text             := '      {$IFDEF FRANK_LOG}' + #13#10 +
                          '      TLog.Debug( ''NIL'' );' + #13#10 +
                          '      {$ENDIF}';
  lOps[ 1 ]            := lOp;

  // Fuege Log-Block 3 vor "end;" (Zeile 8) ein
  lOp := Default( TCommandLineParams );
  lOp.Command          := ctInsertBefore;
  lOp.InsertBeforeLine := 8;
  lOp.Text             := '  {$IFDEF FRANK_LOG}' + #13#10 +
                          '  TLog.Debug( ''End'' );' + #13#10 +
                          '  {$ENDIF}';
  lOps[ 2 ]            := lOp;

  lResult := TBatchProcessor.ProcessLineOperations( fTestFilePath, lOps );
  Assert.IsTrue( lResult, 'Add logs should succeed' );

  lContent := ReadTestFile;
  Assert.IsTrue( lContent.Contains( 'FRANK_LOG' ), 'Log blocks should be present' );
  Assert.IsTrue( lContent.Contains( 'TLog.Debug( ''Start'' )' ), 'Start log should be present' );
  Assert.IsTrue( lContent.Contains( 'TLog.Debug( ''NIL'' )' ), 'NIL log should be present' );
  Assert.IsTrue( lContent.Contains( 'TLog.Debug( ''End'' )' ), 'End log should be present' );
end;

end.

