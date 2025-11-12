Unit StrEditor.BatchProcessor;

interface

Uses
  System.SysUtils
, System.Classes
, System.Generics.Collections
, System.Generics.Defaults
, StrEditor.CommandLine
, StrEditor.Operations
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Batch Processor für Line-Operationen
  ///   Sortiert Operationen nach Zeilennummer (höchste zuerst) und führt sie aus
  /// </summary>
  {$ENDREGION}
  TBatchProcessor = class
    private
      Type
        TLineOperation = record
          Command     : TCommandType;
          LineNumber  : Integer;
          LineNumbers : string;
          StartLine   : Integer;
          EndLine     : Integer;
          Text        : string;
          TextIsBase64: Boolean;
          Backup      : Boolean;
          DryRun      : Boolean;
          Diff        : Boolean;
          Verbose     : Boolean;
        end;

    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Prüft, ob die Operationen Line-Operationen enthalten
      /// </summary>
      {$ENDREGION}
      class function HasLineOperations( const aOperations : TArray<TCommandLineParams> ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Verarbeitet Line-Operationen in der richtigen Reihenfolge (höchste Zeile zuerst)
      /// </summary>
      {$ENDREGION}
      class function ProcessLineOperations( const aFilePath : string; const aOperations : TArray<TCommandLineParams> ) : Boolean;
  end;

implementation

class function TBatchProcessor.HasLineOperations( const aOperations : TArray<TCommandLineParams> ) : Boolean;
Var
  lOp : TCommandLineParams;
begin
  Result := false;

  for lOp in aOperations do
    begin
      if ( lOp.Command = ctDeleteLine ) or ( lOp.Command = ctDeleteLines ) or ( lOp.Command = ctReplaceLine ) then
        begin
          Result := true;
          Exit;
        end;
    end;
end;

class function TBatchProcessor.ProcessLineOperations( const aFilePath : string; const aOperations : TArray<TCommandLineParams> ) : Boolean;
Var
  lLineOps : TList<TLineOperation>;
  lOp      : TCommandLineParams;
  lLineOp  : TLineOperation;
  lResult  : TOperationResult;
  i        : Integer;
begin
  Result   := true;
  lLineOps := TList<TLineOperation>.Create;

  try
    for lOp in aOperations do
      begin
        if ( lOp.Command = ctDeleteLine ) or ( lOp.Command = ctDeleteLines ) or ( lOp.Command = ctReplaceLine ) then
          begin
            lLineOp.Command      := lOp.Command;
            lLineOp.LineNumber   := lOp.LineNumber;
            lLineOp.LineNumbers  := lOp.LineNumbers;
            lLineOp.StartLine    := lOp.StartLine;
            lLineOp.EndLine      := lOp.EndLine;
            lLineOp.Text         := lOp.Text;
            lLineOp.TextIsBase64 := lOp.TextIsBase64;
            lLineOp.Backup       := lOp.Backup;
            lLineOp.DryRun       := lOp.DryRun;
            lLineOp.Diff         := lOp.Diff;
            lLineOp.Verbose      := lOp.Verbose;

            lLineOps.Add( lLineOp );
          end;
      end;

    lLineOps.Sort( TComparer<TLineOperation>.Construct(
      function( const Left, Right : TLineOperation ) : Integer
      Var
        lLeftMax  : Integer;
        lRightMax : Integer;
      begin
        if Left.Command = ctDeleteLine
          then lLeftMax := Left.LineNumber
          else
        if Left.Command = ctReplaceLine
          then lLeftMax := Left.LineNumber
          else
        if ( Left.StartLine > 0 ) and ( Left.EndLine > 0 )
          then lLeftMax := Left.EndLine
          else lLeftMax := 0;

        if Right.Command = ctDeleteLine
          then lRightMax := Right.LineNumber
          else
        if Right.Command = ctReplaceLine
          then lRightMax := Right.LineNumber
          else
        if ( Right.StartLine > 0 ) and ( Right.EndLine > 0 )
          then lRightMax := Right.EndLine
          else lRightMax := 0;

        Result := lRightMax - lLeftMax;
      end ) );

    for i := 0 to lLineOps.Count - 1 do
      begin
        lLineOp := lLineOps[ i ];

        case lLineOp.Command of
          ctDeleteLine:
            begin
              lResult := TStringOperations.DeleteLine( aFilePath, lLineOp.LineNumber, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;
            end;

          ctDeleteLines:
            begin
              if lLineOp.LineNumbers <> ''
                then lResult := TStringOperations.DeleteLines( aFilePath, lLineOp.LineNumbers, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose )
                else lResult := TStringOperations.DeleteLines( aFilePath, lLineOp.StartLine, lLineOp.EndLine, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;
            end;

          ctReplaceLine:
            begin
              lResult := TStringOperations.ReplaceLine( aFilePath, lLineOp.LineNumber, lLineOp.Text, lLineOp.DryRun, lLineOp.Backup, lLineOp.Diff, lLineOp.Verbose, lLineOp.TextIsBase64 );

              if not lResult.Success then
                begin
                  WriteLn( 'ERROR: ' + lResult.ErrorMessage );
                  Result := false;
                  Exit;
                end;
            end;
        end; // of case
      end;
  finally
    lLineOps.Free;
  end;
end;

end.

