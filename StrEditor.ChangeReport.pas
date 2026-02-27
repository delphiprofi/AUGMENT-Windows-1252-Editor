Unit StrEditor.ChangeReport;

{***************************************************************************
 * StrEditor Change Report
 *
 * Automatische Ausgabe von Änderungen nach Operationen.
 * Zeigt was geändert wurde mit Original-Zeilennummern.
 ***************************************************************************}

interface

Uses
  System.SysUtils
, System.Classes
, System.Generics.Collections
, StrEditor.CommandLine
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Einzelne Änderung in einer Datei
  /// </summary>
  {$ENDREGION}
  TChangeRecord = record
    Operation       : TCommandType;   // Was wurde gemacht
    OriginalLine    : Integer;        // Original-Zeilennummer (aus JSON/Kommando)
    AdjustedLine    : Integer;        // Angepasste Zeilennummer (nach Offsets)
    OldContent      : string;         // Alter Inhalt (bei delete/replace)
    NewContent      : string;         // Neuer Inhalt (bei insert/replace)
    LinesAffected   : Integer;        // Anzahl betroffener Zeilen
    Success         : Boolean;        // Operation erfolgreich?
    ErrorMessage    : string;         // Fehlermeldung falls nicht erfolgreich
  end;

  {$REGION 'Documentation'}
  /// <summary>
  ///   Sammelt alle Änderungen und generiert einen Report
  /// </summary>
  {$ENDREGION}
  TChangeReport = class
    private
      fFilePath          : string;
      fChanges           : TList<TChangeRecord>;
      fOriginalLineCount : Integer;
      fFinalLineCount    : Integer;
      fCurrentChange     : TChangeRecord;
      fOriginalContent   : TStringList;
      fContextLinesBefore: Integer;
      fContextLinesAfter : Integer;

      function GetOperationName( aOp : TCommandType ) : string;
      function FormatLineContent( aLineNum : Integer; const aContent : string; const aMarker : string = '' ) : string;
      procedure AppendContextBefore( aSB : TStringBuilder; aLine : Integer );
      procedure AppendContextAfter( aSB : TStringBuilder; aLine : Integer );
    public
      constructor Create( const aFilePath : string );
      destructor  Destroy; override;

      // Tracking-Methoden
      procedure BeginOperation( aOperation : TCommandType; aOriginalLine : Integer );
      procedure SetOldContent( const aContent : string );
      procedure SetInsertedText( const aText : string );
      procedure SetNewContent( const aContent : string );
      procedure SetAdjustedLine( aLine : Integer );
      procedure EndOperation( aSuccess : Boolean; const aErrorMessage : string = '' );

      // Report-Generierung
      function  GenerateReport : string;
      procedure PrintReport;

      // Abfragen
      function  GetChangeCount : Integer;

      // Properties
      property FilePath          : string  read fFilePath;
      property OriginalLineCount : Integer read fOriginalLineCount write fOriginalLineCount;
      property FinalLineCount    : Integer read fFinalLineCount    write fFinalLineCount;
      property ContextLinesBefore: Integer read fContextLinesBefore write fContextLinesBefore;
      property ContextLinesAfter : Integer read fContextLinesAfter  write fContextLinesAfter;
  end;

implementation

Uses
  System.IOUtils
, StrEditor.Encoding
, StrEditor.Settings
;

{ TChangeReport }

constructor TChangeReport.Create( const aFilePath : string );
begin
  inherited Create;

  fFilePath           := aFilePath;
  fChanges            := TList<TChangeRecord>.Create;
  fOriginalContent    := TStringList.Create;
  fOriginalLineCount  := 0;
  fFinalLineCount     := 0;

  // Lade Context-Settings aus INI
  fContextLinesBefore := TStrEditorSettings.Instance.ContextLinesBefore;
  fContextLinesAfter  := TStrEditorSettings.Instance.ContextLinesAfter;

  // Lade Original-Inhalt wenn Datei existiert (für Kontext-Anzeige)
  if ( fContextLinesBefore > 0 ) or ( fContextLinesAfter > 0 ) then
    if TFile.Exists( aFilePath ) then
      begin
        Var lLines    : TStringList;
        Var lEncoding : TEncodingType;

        if TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
          begin
            fOriginalContent.Assign( lLines );
            fOriginalLineCount := fOriginalContent.Count;
            lLines.Free;
          end;
      end;
end;

destructor TChangeReport.Destroy;
begin
  fChanges.Free;
  fOriginalContent.Free;

  inherited Destroy;
end;

function TChangeReport.GetOperationName( aOp : TCommandType ) : string;
begin
  case aOp of
    ctInsert       : Result := 'INSERT';
    ctInsertBefore : Result := 'INSERT-BEFORE';
    ctDeleteLine   : Result := 'DELETE';
    ctDeleteLines  : Result := 'DELETE-LINES';
    ctReplaceLine  : Result := 'REPLACE';
    ctStrReplace   : Result := 'STR-REPLACE';
    ctRegexReplace : Result := 'REGEX-REPLACE';
    ctIndent       : Result := 'INDENT';
    ctUnindent     : Result := 'UNINDENT';
    ctMoveLines    : Result := 'MOVE-LINES';
    else             Result := 'OPERATION';
  end;
end;

function TChangeReport.FormatLineContent( aLineNum : Integer; const aContent : string; const aMarker : string ) : string;
begin
  if aMarker <> ''
    then Result := Format( '  %s %4d: %s', [ aMarker, aLineNum, aContent ] )
    else Result := Format( '     %4d: %s', [ aLineNum, aContent ] );
end;

procedure TChangeReport.AppendContextBefore( aSB : TStringBuilder; aLine : Integer );
Var
  lStartLine : Integer;
  i          : Integer;
begin
  if ( fContextLinesBefore <= 0 ) or ( fOriginalContent.Count = 0 ) then
    Exit;

  lStartLine := aLine - fContextLinesBefore;

  if lStartLine < 1 then
    lStartLine := 1;

  for i := lStartLine to aLine - 1 do
    if ( i >= 1 ) and ( i <= fOriginalContent.Count ) then
      aSB.AppendLine( FormatLineContent( i, fOriginalContent[ i - 1 ], ' ' ) );
end;

procedure TChangeReport.AppendContextAfter( aSB : TStringBuilder; aLine : Integer );
Var
  lEndLine : Integer;
  i        : Integer;
begin
  if ( fContextLinesAfter <= 0 ) or ( fOriginalContent.Count = 0 ) then
    Exit;

  lEndLine := aLine + fContextLinesAfter;

  if lEndLine > fOriginalContent.Count then
    lEndLine := fOriginalContent.Count;

  for i := aLine + 1 to lEndLine do
    if ( i >= 1 ) and ( i <= fOriginalContent.Count ) then
      aSB.AppendLine( FormatLineContent( i, fOriginalContent[ i - 1 ], ' ' ) );
end;

procedure TChangeReport.BeginOperation( aOperation : TCommandType; aOriginalLine : Integer );
begin
  fCurrentChange := Default( TChangeRecord );
  fCurrentChange.Operation    := aOperation;
  fCurrentChange.OriginalLine := aOriginalLine;
  fCurrentChange.AdjustedLine := aOriginalLine;  // Default, kann später angepasst werden
end;

procedure TChangeReport.SetOldContent( const aContent : string );
begin
  fCurrentChange.OldContent := aContent;
end;

procedure TChangeReport.SetInsertedText( const aText : string );
begin
  fCurrentChange.NewContent := aText;
end;

procedure TChangeReport.SetNewContent( const aContent : string );
begin
  fCurrentChange.NewContent := aContent;
end;

procedure TChangeReport.SetAdjustedLine( aLine : Integer );
begin
  fCurrentChange.AdjustedLine := aLine;
end;

procedure TChangeReport.EndOperation( aSuccess : Boolean; const aErrorMessage : string );
begin
  fCurrentChange.Success      := aSuccess;
  fCurrentChange.ErrorMessage := aErrorMessage;
  fChanges.Add( fCurrentChange );
end;

function TChangeReport.GenerateReport : string;
Var
  lSB           : TStringBuilder;
  lChange       : TChangeRecord;
  lLines        : TArray<string>;
  i             : Integer;
  lCurrentLines : TStringList;
  lEncoding     : TEncodingType;
begin
  // Finale Zeilenzahl aus der Datei lesen falls noch nicht gesetzt
  if ( fFinalLineCount = 0 ) and ( fOriginalLineCount > 0 ) then
    begin
      if TEncodingHelper.ReadFileWithRetry( fFilePath, lCurrentLines, lEncoding ) then
        begin
          fFinalLineCount := lCurrentLines.Count;
          lCurrentLines.Free;
        end;
    end;

  lSB := TStringBuilder.Create;
  try
    // Header
    lSB.AppendLine( '=== CHANGE REPORT ===' );
    lSB.AppendLine( 'File: ' + fFilePath );
    lSB.AppendLine( 'Operations: ' + IntToStr( fChanges.Count ) );

    if ( fOriginalLineCount > 0 ) and ( fFinalLineCount > 0 ) then
      begin
        lSB.Append( 'Lines: ' + IntToStr( fOriginalLineCount ) + ' -> ' + IntToStr( fFinalLineCount ) );

        if fFinalLineCount > fOriginalLineCount
          then lSB.AppendLine( ' (+' + IntToStr( fFinalLineCount - fOriginalLineCount ) + ')' )
          else
        if fFinalLineCount < fOriginalLineCount
          then lSB.AppendLine( ' (' + IntToStr( fFinalLineCount - fOriginalLineCount ) + ')' )
          else lSB.AppendLine( ' (no change)' );
      end;

    lSB.AppendLine( '' );

    // Änderungen
    for lChange in fChanges do
      begin
        if lChange.Success then
          begin
            case lChange.Operation of
              ctInsert, ctInsertBefore:
                begin
                  if lChange.Operation = ctInsert
                    then lSB.AppendLine( GetOperationName( lChange.Operation ) + ' after line ' + IntToStr( lChange.OriginalLine ) + ':' )
                    else lSB.AppendLine( GetOperationName( lChange.Operation ) + ' before line ' + IntToStr( lChange.OriginalLine ) + ':' );

                  // Kontext vorher
                  AppendContextBefore( lSB, lChange.OriginalLine );

                  // Eingefügter Inhalt
                  lLines := lChange.NewContent.Split( [ #13#10 ], TStringSplitOptions.None );

                  for i := 0 to High( lLines ) do
                    lSB.AppendLine( '  +      : ' + lLines[ i ] );

                  // Kontext nachher
                  AppendContextAfter( lSB, lChange.OriginalLine );
                end;

              ctDeleteLine:
                begin
                  lSB.AppendLine( GetOperationName( lChange.Operation ) + ' line ' + IntToStr( lChange.OriginalLine ) + ':' );

                  // Kontext vorher
                  AppendContextBefore( lSB, lChange.OriginalLine );

                  // Gelöschte Zeile
                  lSB.AppendLine( FormatLineContent( lChange.OriginalLine, lChange.OldContent, '-' ) );

                  // Kontext nachher
                  AppendContextAfter( lSB, lChange.OriginalLine );
                end;

              ctDeleteLines:
                begin
                  lSB.AppendLine( GetOperationName( lChange.Operation ) + ' from line ' + IntToStr( lChange.OriginalLine ) + ':' );

                  // Kontext vorher
                  AppendContextBefore( lSB, lChange.OriginalLine );

                  // Gelöschte Zeilen
                  if lChange.OldContent <> '' then
                    begin
                      Var lCleanContent : string := lChange.OldContent.Replace( #13, '' );
                      lLines := lCleanContent.Split( [ #10 ], TStringSplitOptions.None );

                      for i := 0 to High( lLines ) do
                        if lLines[ i ] <> '' then
                          lSB.AppendLine( FormatLineContent( lChange.OriginalLine + i, lLines[ i ], '-' ) );
                    end;

                  // Kontext nachher (nach der letzten gelöschten Zeile)
                  // TODO: EndLine speichern für korrekten Kontext
                end;

              ctReplaceLine:
                begin
                  lSB.AppendLine( GetOperationName( lChange.Operation ) + ' line ' + IntToStr( lChange.OriginalLine ) + ':' );

                  // Kontext vorher
                  AppendContextBefore( lSB, lChange.OriginalLine );

                  // Alte Zeile
                  lSB.AppendLine( FormatLineContent( lChange.OriginalLine, lChange.OldContent, '-' ) );

                  // Neue Zeile(n)
                  lLines := lChange.NewContent.Split( [ #13#10 ], TStringSplitOptions.None );

                  for i := 0 to High( lLines ) do
                    lSB.AppendLine( '  +      : ' + lLines[ i ] );

                  // Kontext nachher
                  AppendContextAfter( lSB, lChange.OriginalLine );
                end;

              ctStrReplace, ctRegexReplace:
                begin
                  lSB.AppendLine( GetOperationName( lChange.Operation ) + ' in line ' + IntToStr( lChange.OriginalLine ) + ':' );

                  // Kontext vorher
                  AppendContextBefore( lSB, lChange.OriginalLine );

                  // Änderung
                  lSB.AppendLine( FormatLineContent( lChange.OriginalLine, lChange.OldContent, '-' ) );
                  lSB.AppendLine( FormatLineContent( lChange.OriginalLine, lChange.NewContent, '+' ) );

                  // Kontext nachher
                  AppendContextAfter( lSB, lChange.OriginalLine );
                end;

              ctIndent, ctUnindent:
                begin
                  lSB.AppendLine( GetOperationName( lChange.Operation ) + ' lines from ' + IntToStr( lChange.OriginalLine ) + ':' );

                  // Kontext vorher
                  AppendContextBefore( lSB, lChange.OriginalLine );

                  // Betroffene Zeilen (vor der Änderung)
                  if lChange.OldContent <> '' then
                    begin
                      Var lCleanContent : string := lChange.OldContent.Replace( #13, '' );
                      lLines := lCleanContent.Split( [ #10 ], TStringSplitOptions.None );

                      for i := 0 to High( lLines ) do
                        if lLines[ i ] <> '' then
                          lSB.AppendLine( FormatLineContent( lChange.OriginalLine + i, lLines[ i ], '|' ) );
                    end;

                  // Kontext nachher - TODO: EndLine speichern
                end;

              ctMoveLines:
                begin
                  lSB.AppendLine( GetOperationName( lChange.Operation ) + ' from line ' + IntToStr( lChange.OriginalLine ) + ':' );

                  // Kontext vorher
                  AppendContextBefore( lSB, lChange.OriginalLine );

                  // Verschobene Zeilen
                  if lChange.OldContent <> '' then
                    begin
                      Var lCleanContent : string := lChange.OldContent.Replace( #13, '' );
                      lLines := lCleanContent.Split( [ #10 ], TStringSplitOptions.None );

                      for i := 0 to High( lLines ) do
                        if lLines[ i ] <> '' then
                          lSB.AppendLine( FormatLineContent( lChange.OriginalLine + i, lLines[ i ], '>' ) );
                    end;

                  // Kontext nachher - TODO: EndLine speichern
                end;
            end;
          end
        else begin
               lSB.AppendLine( 'ERROR in ' + GetOperationName( lChange.Operation ) + ' at line ' + IntToStr( lChange.OriginalLine ) + ':' );
               lSB.AppendLine( '  ' + lChange.ErrorMessage );
             end;

        lSB.AppendLine( '' );
      end;

    lSB.AppendLine( '=====================' );
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

procedure TChangeReport.PrintReport;
begin
  Write( GenerateReport );
end;

function TChangeReport.GetChangeCount : Integer;
begin
  Result := fChanges.Count;
end;

end.

