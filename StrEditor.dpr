program StrEditor;

{$APPTYPE CONSOLE}

{$R *.res}

Uses
  System.SysUtils
, System.Classes
, StrEditor.Encoding
, StrEditor.Operations
, StrEditor.CommandLine
, StrEditor.Regex
, StrEditor.Batch
, StrEditor.Undo
, StrEditor.Config
, StrEditor.BatchProcessor
;

procedure ProcessSingleFile( const aParams : TCommandLineParams );
Var
  lResult      : TOperationResult;
  lRegexResult : TRegexOperationResult;
begin
  case aParams.Command of
    ctHelp:
      begin
        TCommandLineParser.ShowHelp;
        ExitCode := Ord( ecSuccess );
      end;

    ctVersion:
      begin
        TCommandLineParser.ShowVersion;
        ExitCode := Ord( ecSuccess );
      end;

    ctDetectEncoding:
      begin
        if aParams.FilePath = '' then
          begin
            WriteLn( 'ERROR: --file parameter required for --detect-encoding' );
            ExitCode := Ord( ecParameterError );
            Exit;
          end;

        if not FileExists( aParams.FilePath ) then
          begin
            WriteLn( 'ERROR: File not found: ' + aParams.FilePath );
            ExitCode := Ord( ecFileNotFound );
            Exit;
          end;

        Var lEncoding     := TEncodingHelper.DetectEncoding( aParams.FilePath );
        Var lEncodingName : string;

        case lEncoding of
          etUTF8        : lEncodingName := 'UTF-8 with BOM';
          etWindows1252 : lEncodingName := 'Windows-1252 (no BOM)';
          else            lEncodingName := 'Unknown';
        end;

        WriteLn( 'File: ' + aParams.FilePath );
        WriteLn( 'Encoding: ' + lEncodingName );

        if aParams.Verbose then
          begin
            WriteLn;
            WriteLn( '--- Details ---' );

            Var lFileStream := TFileStream.Create( aParams.FilePath, fmOpenRead or fmShareDenyWrite );
            try
              Var lBytes : TBytes;
              SetLength( lBytes, 3 );

              if lFileStream.Size >= 3 then
                lFileStream.Read( lBytes[ 0 ], 3 );

              WriteLn( 'File size: ' + IntToStr( lFileStream.Size ) + ' bytes' );
              WriteLn( 'First 3 bytes (hex): ' + IntToHex( lBytes[ 0 ], 2 ) + ' ' + IntToHex( lBytes[ 1 ], 2 ) + ' ' + IntToHex( lBytes[ 2 ], 2 ) );

              if lEncoding = etUTF8
                then WriteLn( 'BOM detected: EF BB BF (UTF-8)' )
                else WriteLn( 'No BOM detected (Windows-1252)' );
            finally
              lFileStream.Free;
            end;
          end;

        ExitCode := Ord( ecSuccess );
      end;

    ctConvertEncoding:
      begin
        if aParams.FilePath = '' then
          begin
            WriteLn( 'ERROR: --file parameter required for --convert-encoding' );
            ExitCode := Ord( ecParameterError );
            Exit;
          end;

        if not FileExists( aParams.FilePath ) then
          begin
            WriteLn( 'ERROR: File not found: ' + aParams.FilePath );
            ExitCode := Ord( ecFileNotFound );
            Exit;
          end;

        lResult := TStringOperations.ConvertEncoding( aParams.FilePath, aParams.TargetEncoding, aParams.Backup, aParams.DryRun, aParams.Verbose );

        if not lResult.Success then
          begin
            WriteLn( 'ERROR: ' + lResult.ErrorMessage );
            ExitCode := Ord( ecEncodingError );
            Exit;
          end;

        ExitCode := Ord( ecSuccess );
      end;

    ctReinterpretEncoding:
      begin
        if aParams.FilePath = '' then
          begin
            WriteLn( 'ERROR: --file parameter required for --reinterpret-as' );
            ExitCode := Ord( ecParameterError );
            Exit;
          end;

        if not FileExists( aParams.FilePath ) then
          begin
            WriteLn( 'ERROR: File not found: ' + aParams.FilePath );
            ExitCode := Ord( ecFileNotFound );
            Exit;
          end;

        lResult := TStringOperations.ReinterpretEncoding( aParams.FilePath, aParams.SourceEncoding, aParams.Backup, aParams.DryRun, aParams.Verbose );

        if not lResult.Success then
          begin
            WriteLn( 'ERROR: ' + lResult.ErrorMessage );
            ExitCode := Ord( ecEncodingError );
            Exit;
          end;

        ExitCode := Ord( ecSuccess );
      end;

    ctShow:
      begin
        if aParams.FilePath = '' then
          begin
            WriteLn( 'ERROR: --file parameter required for --show' );
            ExitCode := Ord( ecParameterError );
            Exit;
          end;

        if not FileExists( aParams.FilePath ) then
          begin
            WriteLn( 'ERROR: File not found: ' + aParams.FilePath );
            ExitCode := Ord( ecFileNotFound );
            Exit;
          end;

        lResult := TStringOperations.Show( aParams.FilePath, aParams.StartLine, aParams.EndLine, aParams.ShowHead, aParams.ShowTail, aParams.ShowLineNumbers, aParams.ShowRaw, aParams.Verbose );

        if not lResult.Success then
          begin
            WriteLn( 'ERROR: ' + lResult.ErrorMessage );
            ExitCode := Ord( ecEncodingError );
            Exit;
          end;

        ExitCode := Ord( ecSuccess );
      end;

    ctStrReplace:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Replacing "' + aParams.OldStr + '" with "' + aParams.NewStr + '" in ' + aParams.FilePath )
              else WriteLn( 'Replacing "' + aParams.OldStr + '" with "' + aParams.NewStr + '" in ' + aParams.FilePath );
          end;

        lResult := TStringOperations.StrReplace( aParams.FilePath, aParams.OldStr, aParams.NewStr, aParams.StartLine, aParams.EndLine, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.CaseConversion, aParams.IndentLevel, aParams.ConditionPattern, aParams.Verbose, aParams.OldStrIsBase64, aParams.NewStrIsBase64, aParams.MultiLine, aParams.ReplaceAll );

        if lResult.Success then
          begin
            if aParams.Verbose then
              begin
                if aParams.DryRun
                  then WriteLn( '[DRY-RUN] Would change ' + IntToStr( lResult.LinesChanged ) + ' line(s)' )
                  else WriteLn( 'Success: ' + IntToStr( lResult.LinesChanged ) + ' line(s) changed' );
              end;

            if aParams.Stats then
              begin
                WriteLn;
                WriteLn( '--- Statistics ---' );
                WriteLn( 'Lines changed: ' + IntToStr( lResult.LinesChanged ) );
                WriteLn( 'Operation: String Replace' );
              end;

            ExitCode := Ord( ecSuccess );
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );

               if Pos( 'not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else
               if Pos( 'String not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecStringNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    ctInsert:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Inserting text after line ' + IntToStr( aParams.InsertAfterLine ) + ' in ' + aParams.FilePath )
              else WriteLn( 'Inserting text after line ' + IntToStr( aParams.InsertAfterLine ) + ' in ' + aParams.FilePath );
          end;

        lResult := TStringOperations.Insert( aParams.FilePath, aParams.Text, aParams.InsertAfterLine, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.TextIsBase64 );

        if lResult.Success then
          begin
            if aParams.Verbose then
              begin
                if aParams.DryRun
                  then WriteLn( '[DRY-RUN] Would insert text' )
                  else WriteLn( 'Success: Text inserted' );
              end;

            if aParams.Stats then
              begin
                WriteLn;
                WriteLn( '--- Statistics ---' );
                WriteLn( 'Lines inserted: ' + IntToStr( lResult.LinesChanged ) );
                WriteLn( 'Operation: Insert' );
              end;

            ExitCode := Ord( ecSuccess );
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );

               if Pos( 'not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    ctInsertBefore:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Inserting text before line ' + IntToStr( aParams.InsertBeforeLine ) + ' in ' + aParams.FilePath )
              else WriteLn( 'Inserting text before line ' + IntToStr( aParams.InsertBeforeLine ) + ' in ' + aParams.FilePath );
          end;

        lResult := TStringOperations.InsertBefore( aParams.FilePath, aParams.Text, aParams.InsertBeforeLine, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.TextIsBase64 );

        if lResult.Success then
          begin
            if aParams.Verbose then
              begin
                if aParams.DryRun
                  then WriteLn( '[DRY-RUN] Would insert text' )
                  else WriteLn( 'Success: Text inserted' );
              end;

            if aParams.Stats then
              begin
                WriteLn;
                WriteLn( '--- Statistics ---' );
                WriteLn( 'Lines inserted: ' + IntToStr( lResult.LinesChanged ) );
                WriteLn( 'Operation: InsertBefore' );
              end;

            ExitCode := Ord( ecSuccess );
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );

               if Pos( 'not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    ctRegexReplace:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Regex replacing pattern "' + aParams.RegexPattern + '" with "' + aParams.RegexReplace + '" in ' + aParams.FilePath )
              else WriteLn( 'Regex replacing pattern "' + aParams.RegexPattern + '" with "' + aParams.RegexReplace + '" in ' + aParams.FilePath );
          end;

        lRegexResult := TRegexOperations.RegexReplace( aParams.FilePath, aParams.RegexPattern, aParams.RegexReplace, aParams.StartLine, aParams.EndLine, aParams.CaseInsensitive, aParams.MultiLine, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.CaseConversion, aParams.IndentLevel );

        if lRegexResult.Success then
          begin
            if aParams.Verbose then
              begin
                if aParams.DryRun
                  then WriteLn( '[DRY-RUN] Would change ' + IntToStr( lRegexResult.LinesChanged ) + ' line(s), ' + IntToStr( lRegexResult.MatchCount ) + ' match(es) found' )
                  else WriteLn( 'Success: ' + IntToStr( lRegexResult.LinesChanged ) + ' line(s) changed, ' + IntToStr( lRegexResult.MatchCount ) + ' match(es) found' );
              end;

            if aParams.Stats then
              begin
                WriteLn;
                WriteLn( '--- Statistics ---' );
                WriteLn( 'Lines changed: ' + IntToStr( lRegexResult.LinesChanged ) );
                WriteLn( 'Matches found: ' + IntToStr( lRegexResult.MatchCount ) );
                WriteLn( 'Operation: Regex Replace' );
              end;

            ExitCode := Ord( ecSuccess );
        end
        else begin
               WriteLn( 'ERROR: ' + lRegexResult.ErrorMessage );

               if Pos( 'not found', lRegexResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecStringNotFound )
                 else
               if Pos( 'File not found', lRegexResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    ctRegexTest:
      begin
        if aParams.Verbose then
          WriteLn( 'Testing regex pattern "' + aParams.RegexPattern + '" in ' + aParams.FilePath );

        lRegexResult := TRegexOperations.RegexTest( aParams.FilePath, aParams.RegexPattern, aParams.StartLine, aParams.EndLine, aParams.CaseInsensitive, aParams.MultiLine );

        if lRegexResult.Success then
          begin
            WriteLn( 'Pattern found: ' + IntToStr( lRegexResult.MatchCount ) + ' match(es)' );

            if aParams.Stats then
              begin
                WriteLn;
                WriteLn( '--- Statistics ---' );
                WriteLn( 'Matches found: ' + IntToStr( lRegexResult.MatchCount ) );
                WriteLn( 'Operation: Regex Test' );
              end;

            ExitCode := Ord( ecSuccess );
          end
        else begin
               WriteLn( 'ERROR: ' + lRegexResult.ErrorMessage );

               if Pos( 'not found', lRegexResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecStringNotFound )
                 else
               if Pos( 'File not found', lRegexResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    ctDeleteLine:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Deleting line ' + IntToStr( aParams.LineNumber ) + ' in ' + aParams.FilePath )
              else WriteLn( 'Deleting line ' + IntToStr( aParams.LineNumber ) + ' in ' + aParams.FilePath );
          end;

        lResult := TStringOperations.DeleteLine( aParams.FilePath, aParams.LineNumber, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Deleted line ' + IntToStr( aParams.LineNumber ) );

            ExitCode := Ord( ecSuccess );
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );

               if Pos( 'File not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    ctDeleteLines:
      begin
        if aParams.Verbose then
          begin
            if aParams.LineNumbers <> ''
              then begin
                     if aParams.DryRun
                       then WriteLn( '[DRY-RUN] Deleting lines ' + aParams.LineNumbers + ' in ' + aParams.FilePath )
                       else WriteLn( 'Deleting lines ' + aParams.LineNumbers + ' in ' + aParams.FilePath );
                   end
              else begin
                     if aParams.DryRun
                       then WriteLn( '[DRY-RUN] Deleting lines ' + IntToStr( aParams.StartLine ) + '-' + IntToStr( aParams.EndLine ) + ' in ' + aParams.FilePath )
                       else WriteLn( 'Deleting lines ' + IntToStr( aParams.StartLine ) + '-' + IntToStr( aParams.EndLine ) + ' in ' + aParams.FilePath );
                   end;
          end;

        if aParams.LineNumbers <> ''
          then lResult := TStringOperations.DeleteLines( aParams.FilePath, aParams.LineNumbers, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose )
          else lResult := TStringOperations.DeleteLines( aParams.FilePath, aParams.StartLine, aParams.EndLine, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Deleted ' + IntToStr( lResult.LinesChanged ) + ' line(s)' );

            ExitCode := Ord( ecSuccess );
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );

               if Pos( 'File not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    ctReplaceLine:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Replacing line ' + IntToStr( aParams.LineNumber ) + ' in ' + aParams.FilePath )
              else WriteLn( 'Replacing line ' + IntToStr( aParams.LineNumber ) + ' in ' + aParams.FilePath );
          end;

        lResult := TStringOperations.ReplaceLine( aParams.FilePath, aParams.LineNumber, aParams.Text, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose, aParams.TextIsBase64 );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Replaced line ' + IntToStr( aParams.LineNumber ) );

            ExitCode := Ord( ecSuccess );
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );

               if Pos( 'File not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    else begin
           TCommandLineParser.ShowError( 'Unknown command' );
           ExitCode := Ord( ecParameterError );
         end;
  end; // of case
end;

Var
  lParams : TCommandLineParams;

begin
  try
    if not TCommandLineParser.Parse( lParams ) then
      begin
        ExitCode := Ord( ecParameterError );
        Exit;
      end;

    if lParams.ConfigFile <> '' then
      begin
        WriteLn( 'Loading config from: ' + lParams.ConfigFile );

        if TConfigHelper.IsMultipleOperationsConfig( lParams.ConfigFile ) then
          begin
            Var lOperations : TArray<TCommandLineParams>;

            if not TConfigHelper.LoadMultipleOperations( lParams.ConfigFile, lOperations ) then
              begin
                ExitCode := Ord( ecJSONParseError );
                Exit;
              end;

            WriteLn( 'Config loaded successfully - ' + IntToStr( Length( lOperations ) ) + ' operation(s)' );

            if TBatchProcessor.HasLineOperations( lOperations ) then
              begin
                Var lFilePath := '';

                for Var lOp in lOperations do
                  begin
                    if lOp.FilePath <> '' then
                      begin
                        lFilePath := lOp.FilePath;
                        Break;
                      end;
                  end;

                if lFilePath = '' then
                  begin
                    WriteLn( 'ERROR: No file path specified in operations' );
                    ExitCode := Ord( ecParameterError );
                    Exit;
                  end;

                if lParams.Verbose then
                  WriteLn( 'Processing line operations in batch mode (sorted by line number)' );

                if not TBatchProcessor.ProcessLineOperations( lFilePath, lOperations ) then
                  begin
                    ExitCode := Ord( ecOperationFailed );
                    Exit;
                  end;
              end
            else begin
                   for Var lOp in lOperations do
                     begin
                       if lOp.Verbose then
                         WriteLn( 'Executing operation: ' + lOp.FilePath );

                       ProcessSingleFile( lOp );
                     end;
                 end;

            Exit;
          end
        else begin
               if not TConfigHelper.LoadFromJSON( lParams.ConfigFile, lParams ) then
                 begin
                   ExitCode := Ord( ecJSONParseError );
                   Exit;
                 end;

               WriteLn( 'Config loaded successfully' );
             end;
      end;

    if lParams.Command = ctUndo then
      begin
        if lParams.Verbose then
          WriteLn( 'Undoing changes for: ' + lParams.FilePath );

        if TUndoHelper.UndoChanges( lParams.FilePath, lParams.Verbose )
          then ExitCode := Ord( ecSuccess )
          else ExitCode := Ord( ecFileNotFound );

        Exit;
      end;

    ProcessSingleFile( lParams );
  except
    on E : Exception do
      begin
        WriteLn( 'EXCEPTION: ' + E.ClassName + ': ' + E.Message );
        ExitCode := Ord( ecEncodingError );
      end;
  end;
end.
