program StrEditor;

{$APPTYPE CONSOLE}

{$R *.res}

Uses
  System.SysUtils
, StrEditor.Encoding
, StrEditor.Operations
, StrEditor.CommandLine
, StrEditor.Regex
;

Var
  lParams      : TCommandLineParams;
  lResult      : TOperationResult;
  lRegexResult : TRegexOperationResult;

begin
  try
    if not TCommandLineParser.Parse( lParams ) then
      begin
        ExitCode := Ord( ecParameterError );
        Exit;
      end;

    case lParams.Command of
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

      ctStrReplace:
        begin
          if lParams.Verbose then
            begin
              if lParams.DryRun
                then WriteLn( '[DRY-RUN] Replacing "' + lParams.OldStr + '" with "' + lParams.NewStr + '" in ' + lParams.FilePath )
                else WriteLn( 'Replacing "' + lParams.OldStr + '" with "' + lParams.NewStr + '" in ' + lParams.FilePath );
            end;

          lResult := TStringOperations.StrReplace( lParams.FilePath, lParams.OldStr, lParams.NewStr, lParams.StartLine, lParams.EndLine, lParams.DryRun, lParams.Backup );

          if lResult.Success then
            begin
              if lParams.Verbose then
                begin
                  if lParams.DryRun
                    then WriteLn( '[DRY-RUN] Would change ' + IntToStr( lResult.LinesChanged ) + ' line(s)' )
                    else WriteLn( 'Success: ' + IntToStr( lResult.LinesChanged ) + ' line(s) changed' );
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
          if lParams.Verbose then
            begin
              if lParams.DryRun
                then WriteLn( '[DRY-RUN] Inserting text after line ' + IntToStr( lParams.InsertAfterLine ) + ' in ' + lParams.FilePath )
                else WriteLn( 'Inserting text after line ' + IntToStr( lParams.InsertAfterLine ) + ' in ' + lParams.FilePath );
            end;

          lResult := TStringOperations.Insert( lParams.FilePath, lParams.Text, lParams.InsertAfterLine, lParams.DryRun, lParams.Backup );

          if lResult.Success then
            begin
              if lParams.Verbose then
                begin
                  if lParams.DryRun
                    then WriteLn( '[DRY-RUN] Would insert text' )
                    else WriteLn( 'Success: Text inserted' );
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
          if lParams.Verbose then
            begin
              if lParams.DryRun
                then WriteLn( '[DRY-RUN] Regex replacing pattern "' + lParams.RegexPattern + '" with "' + lParams.RegexReplace + '" in ' + lParams.FilePath )
                else WriteLn( 'Regex replacing pattern "' + lParams.RegexPattern + '" with "' + lParams.RegexReplace + '" in ' + lParams.FilePath );
            end;

          lRegexResult := TRegexOperations.RegexReplace( lParams.FilePath, lParams.RegexPattern, lParams.RegexReplace, lParams.StartLine, lParams.EndLine, lParams.CaseInsensitive, lParams.MultiLine, lParams.DryRun, lParams.Backup );

          if lRegexResult.Success then
            begin
              if lParams.Verbose then
                begin
                  if lParams.DryRun
                    then WriteLn( '[DRY-RUN] Would change ' + IntToStr( lRegexResult.LinesChanged ) + ' line(s), ' + IntToStr( lRegexResult.MatchCount ) + ' match(es) found' )
                    else WriteLn( 'Success: ' + IntToStr( lRegexResult.LinesChanged ) + ' line(s) changed, ' + IntToStr( lRegexResult.MatchCount ) + ' match(es) found' );
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
          if lParams.Verbose then
            WriteLn( 'Testing regex pattern "' + lParams.RegexPattern + '" in ' + lParams.FilePath );

          lRegexResult := TRegexOperations.RegexTest( lParams.FilePath, lParams.RegexPattern, lParams.StartLine, lParams.EndLine, lParams.CaseInsensitive, lParams.MultiLine );

          if lRegexResult.Success then
            begin
              WriteLn( 'Pattern found: ' + IntToStr( lRegexResult.MatchCount ) + ' match(es)' );
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

      else begin
             TCommandLineParser.ShowError( 'Unknown command' );
             ExitCode := Ord( ecParameterError );
           end;
    end;
  except
    on E : Exception do
      begin
        WriteLn( 'EXCEPTION: ' + E.ClassName + ': ' + E.Message );
        ExitCode := Ord( ecEncodingError );
      end;
  end;
end.
