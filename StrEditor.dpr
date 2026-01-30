program StrEditor;

{$APPTYPE CONSOLE}

{$R *.res}

Uses
  System.SysUtils
, System.Classes
, System.IOUtils
, StrEditor.Encoding
, StrEditor.Operations
, StrEditor.CommandLine
, StrEditor.Regex
, StrEditor.Batch
, StrEditor.Undo
, StrEditor.Config
, StrEditor.BatchProcessor
, StrEditor.Documentation
, StrEditor.Repair
, StrEditor.Settings
, StrEditor.ChangeReport
, StrEditor.SessionLog
;

procedure ProcessSingleFile( const aParams : TCommandLineParams );
Var
  lResult       : TOperationResult;
  lRegexResult  : TRegexOperationResult;
  lChangeReport : TChangeReport;
  lOldContent   : string;

  procedure OutputChangeReport;
  begin
    if ( lChangeReport <> NIL ) and TStrEditorSettings.Instance.ChangeReportEnabled then
      begin
        WriteLn( lChangeReport.GenerateReport );
        FreeAndNIL( lChangeReport );
      end;
  end;

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
            TSessionLog.Instance.LogError( '--show: --file parameter required' );
            ExitCode := Ord( ecParameterError );
            Exit;
          end;

        if not FileExists( aParams.FilePath ) then
          begin
            WriteLn( 'ERROR: File not found: ' + aParams.FilePath );
            TSessionLog.Instance.LogError( '--show: File not found: ' + aParams.FilePath );
            ExitCode := Ord( ecFileNotFound );
            Exit;
          end;

        // Log VIEW request
        TSessionLog.Instance.LogView( aParams.FilePath, aParams.StartLine, aParams.EndLine );

        lResult := TStringOperations.Show( aParams.FilePath, aParams.StartLine, aParams.EndLine, aParams.ShowHead, aParams.ShowTail, aParams.ShowLineNumbers, aParams.ShowRaw, aParams.Verbose, aParams.ShowHex, aParams.ShowBase64 );

        if not lResult.Success then
          begin
            WriteLn( 'ERROR: ' + lResult.ErrorMessage );
            TSessionLog.Instance.LogError( '--show: ' + lResult.ErrorMessage );
            ExitCode := Ord( ecEncodingError );
            Exit;
          end;

        TSessionLog.Instance.LogSuccess( '--show: ' + aParams.FilePath );
        ExitCode := Ord( ecSuccess );
      end;

    ctDocs:
      begin
        if aParams.DocsListFiles then
          begin
            Var lFiles := TDocumentation.ListDocumentationFiles;

            if Length( lFiles ) = 0 then
              begin
                WriteLn( 'No documentation files found in exe directory' );
                ExitCode := Ord( ecFileNotFound );
                Exit;
              end;

            WriteLn( 'Available documentation files:' );
            WriteLn;

            for Var lFile in lFiles do
              WriteLn( '  ' + lFile );

            ExitCode := Ord( ecSuccess );
            Exit;
          end;

        Var lDocPath := TDocumentation.GetDocumentationPath( aParams.DocsFile );

        if aParams.DocsOpenBrowser then
          begin
            Var lBrowserResult := TDocumentation.OpenInBrowser( lDocPath );

            if lBrowserResult.Success then
              begin
                WriteLn( lBrowserResult.Content );
                ExitCode := Ord( ecSuccess );
              end
            else begin
                   WriteLn( 'ERROR: ' + lBrowserResult.ErrorMessage );
                   ExitCode := Ord( ecFileNotFound );
                 end;
          end
        else begin
               Var lDocResult := TDocumentation.ShowDocs( lDocPath, aParams.ShowHead, aParams.ShowTail, aParams.ShowLineNumbers );

               if lDocResult.Success then
                 begin
                   WriteLn( lDocResult.Content );
                   ExitCode := Ord( ecSuccess );
                 end
               else begin
                      WriteLn( 'ERROR: ' + lDocResult.ErrorMessage );
                      ExitCode := Ord( ecFileNotFound );
                    end;
             end;
      end;

    ctStrReplace:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Replacing "' + aParams.OldStr + '" with "' + aParams.NewStr + '" in ' + aParams.FilePath )
              else WriteLn( 'Replacing "' + aParams.OldStr + '" with "' + aParams.NewStr + '" in ' + aParams.FilePath );
          end;

        // ChangeReport: Für str-replace speichern wir OldStr und NewStr
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctStrReplace, aParams.StartLine );
            lChangeReport.SetOldContent( aParams.OldStr );
            lChangeReport.SetNewContent( aParams.NewStr );
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

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

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

        // ChangeReport: Neuen Inhalt für Report vorbereiten
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctInsert, aParams.InsertAfterLine );
            lChangeReport.SetNewContent( aParams.Text );
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

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

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

        // ChangeReport: Neuen Inhalt für Report vorbereiten
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctInsertBefore, aParams.InsertBeforeLine );
            lChangeReport.SetNewContent( aParams.Text );
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

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

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

        // ChangeReport: Für regex-replace speichern wir Pattern und Replacement
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctRegexReplace, aParams.StartLine );
            lChangeReport.SetOldContent( aParams.RegexPattern );
            lChangeReport.SetNewContent( aParams.RegexReplace );
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

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lRegexResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

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

        // ChangeReport: Alten Inhalt vor der Operation speichern
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lOldContent   := TBatchProcessor.GetLineContent( aParams.FilePath, aParams.LineNumber );
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctDeleteLine, aParams.LineNumber );
            lChangeReport.SetOldContent( lOldContent );
          end;

        lResult := TStringOperations.DeleteLine( aParams.FilePath, aParams.LineNumber, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Deleted line ' + IntToStr( aParams.LineNumber ) );

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

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

        // ChangeReport: Alten Inhalt vor der Operation speichern
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lOldContent := TBatchProcessor.GetLinesContent( aParams.FilePath, aParams.StartLine, aParams.EndLine );
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctDeleteLines, aParams.StartLine );
            lChangeReport.SetOldContent( lOldContent );
          end;

        if aParams.LineNumbers <> ''
          then lResult := TStringOperations.DeleteLines( aParams.FilePath, aParams.LineNumbers, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose )
          else lResult := TStringOperations.DeleteLines( aParams.FilePath, aParams.StartLine, aParams.EndLine, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Deleted ' + IntToStr( lResult.LinesChanged ) + ' line(s)' );

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

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

        // ChangeReport: Alten und neuen Inhalt für Report vorbereiten
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lOldContent   := TBatchProcessor.GetLineContent( aParams.FilePath, aParams.LineNumber );
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctReplaceLine, aParams.LineNumber );
            lChangeReport.SetOldContent( lOldContent );
            lChangeReport.SetNewContent( aParams.Text );
          end;

        lResult := TStringOperations.ReplaceLine( aParams.FilePath, aParams.LineNumber, aParams.Text, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose, aParams.TextIsBase64 );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Replaced line ' + IntToStr( aParams.LineNumber ) );

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

               if Pos( 'File not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecEncodingError );
             end;
      end;

    ctRepairUmlauts:
      begin
        if aParams.FilePath = '' then
          begin
            WriteLn( 'ERROR: --file parameter required for --repair-umlauts' );
            ExitCode := Ord( ecParameterError );
            Exit;
          end;

        if not FileExists( aParams.FilePath ) then
          begin
            WriteLn( 'ERROR: File not found: ' + aParams.FilePath );
            ExitCode := Ord( ecFileNotFound );
            Exit;
          end;

        Var lRepairResult : TRepairResult;

        if aParams.ReferencePath <> '' then
          begin
            // Mit Referenz-Datei reparieren
            lRepairResult := TUmlautRepair.RepairFromReference(
              aParams.FilePath,
              aParams.ReferencePath,
              aParams.DryRun,
              aParams.Verbose,
              aParams.Backup
            );
          end
        else begin
               // Mit VCS reparieren
               Var lVCS := TUmlautRepair.StringToVCS( aParams.VCS );

               lRepairResult := TUmlautRepair.RepairFromVCS(
                 aParams.FilePath,
                 lVCS,
                 '',  // Repo-Root auto-detect
                 aParams.Revision,
                 aParams.DryRun,
                 aParams.Verbose,
                 aParams.Backup
               );
             end;

        if lRepairResult.Success then
          begin
            if lRepairResult.BytesRepaired > 0 then
              begin
                WriteLn( 'SUCCESS: Repaired ' + IntToStr( lRepairResult.BytesRepaired ) +
                         ' umlaut(s) in ' + aParams.FilePath );

                if lRepairResult.VCSUsed <> vcsAuto then
                  WriteLn( 'VCS used: ' + TUmlautRepair.VCSToString( lRepairResult.VCSUsed ) );
              end
            else WriteLn( 'INFO: ' + lRepairResult.ErrorMessage );

            ExitCode := Ord( ecSuccess );
          end
        else begin
               WriteLn( 'ERROR: ' + lRepairResult.ErrorMessage );
               ExitCode := Ord( ecOperationFailed );
             end;
      end;

    ctMoveLines:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Moving lines ' + IntToStr( aParams.StartLine ) + '-' + IntToStr( aParams.EndLine ) + ' from ' + aParams.FromFile + ' to ' + aParams.ToFile )
              else WriteLn( 'Moving lines ' + IntToStr( aParams.StartLine ) + '-' + IntToStr( aParams.EndLine ) + ' from ' + aParams.FromFile + ' to ' + aParams.ToFile );
          end;

        // ChangeReport: Inhalt der zu verschiebenden Zeilen speichern
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lOldContent := TBatchProcessor.GetLinesContent( aParams.FromFile, aParams.StartLine, aParams.EndLine );
            lChangeReport := TChangeReport.Create( aParams.FromFile );
            lChangeReport.BeginOperation( ctMoveLines, aParams.StartLine );
            lChangeReport.SetOldContent( lOldContent );
          end;

        lResult := TStringOperations.MoveLines( aParams.FromFile, aParams.ToFile, aParams.StartLine, aParams.EndLine, aParams.InsertAfterLine, aParams.InsertBeforeLine, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Moved ' + IntToStr( lResult.LinesChanged ) + ' line(s) from ' + aParams.FromFile + ' to ' + aParams.ToFile );

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

               if Pos( 'File not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecOperationFailed );
             end;
      end;

    ctIndent:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Indenting lines ' + IntToStr( aParams.StartLine ) + '-' + IntToStr( aParams.EndLine ) + ' by ' + IntToStr( aParams.IndentSpaces ) + ' spaces in ' + aParams.FilePath )
              else WriteLn( 'Indenting lines ' + IntToStr( aParams.StartLine ) + '-' + IntToStr( aParams.EndLine ) + ' by ' + IntToStr( aParams.IndentSpaces ) + ' spaces in ' + aParams.FilePath );
          end;

        // ChangeReport: Alten Inhalt vor der Operation speichern
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lOldContent := TBatchProcessor.GetLinesContent( aParams.FilePath, aParams.StartLine, aParams.EndLine );
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctIndent, aParams.StartLine );
            lChangeReport.SetOldContent( lOldContent );
          end;

        lResult := TStringOperations.IndentLines( aParams.FilePath, aParams.StartLine, aParams.EndLine, aParams.IndentSpaces, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Indented ' + IntToStr( lResult.LinesChanged ) + ' line(s) in ' + aParams.FilePath );

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

               if Pos( 'File not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecOperationFailed );
             end;
      end;

    ctUnindent:
      begin
        if aParams.Verbose then
          begin
            if aParams.DryRun
              then WriteLn( '[DRY-RUN] Unindenting lines ' + IntToStr( aParams.StartLine ) + '-' + IntToStr( aParams.EndLine ) + ' by ' + IntToStr( aParams.IndentSpaces ) + ' spaces in ' + aParams.FilePath )
              else WriteLn( 'Unindenting lines ' + IntToStr( aParams.StartLine ) + '-' + IntToStr( aParams.EndLine ) + ' by ' + IntToStr( aParams.IndentSpaces ) + ' spaces in ' + aParams.FilePath );
          end;

        // ChangeReport: Alten Inhalt vor der Operation speichern
        lChangeReport := NIL;

        if TStrEditorSettings.Instance.ChangeReportEnabled and ( not aParams.DryRun ) then
          begin
            lOldContent := TBatchProcessor.GetLinesContent( aParams.FilePath, aParams.StartLine, aParams.EndLine );
            lChangeReport := TChangeReport.Create( aParams.FilePath );
            lChangeReport.BeginOperation( ctUnindent, aParams.StartLine );
            lChangeReport.SetOldContent( lOldContent );
          end;

        lResult := TStringOperations.UnindentLines( aParams.FilePath, aParams.StartLine, aParams.EndLine, aParams.IndentSpaces, aParams.DryRun, aParams.Backup, aParams.Diff, aParams.Verbose );

        if lResult.Success then
          begin
            if not aParams.DryRun then
              WriteLn( 'SUCCESS: Unindented ' + IntToStr( lResult.LinesChanged ) + ' line(s) in ' + aParams.FilePath );

            if lChangeReport <> NIL then
              lChangeReport.EndOperation( true );

            ExitCode := Ord( ecSuccess );
            OutputChangeReport;
          end
        else begin
               WriteLn( 'ERROR: ' + lResult.ErrorMessage );
               FreeAndNIL( lChangeReport );

               if Pos( 'File not found', lResult.ErrorMessage ) > 0
                 then ExitCode := Ord( ecFileNotFound )
                 else ExitCode := Ord( ecOperationFailed );
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

        // Log JSON-Config
        if FileExists( lParams.ConfigFile ) then
          TSessionLog.Instance.LogConfig( TFile.ReadAllText( lParams.ConfigFile ) );

        if TConfigHelper.IsMultipleOperationsConfig( lParams.ConfigFile ) then
          begin
            Var lOperations : TArray<TCommandLineParams>;

            if not TConfigHelper.LoadMultipleOperations( lParams.ConfigFile, lOperations ) then
              begin
                TSessionLog.Instance.LogError( 'JSON parse error: ' + lParams.ConfigFile );
                ExitCode := Ord( ecJSONParseError );
                Exit;
              end;

            WriteLn( 'Config loaded successfully - ' + IntToStr( Length( lOperations ) ) + ' operation(s)' );

            if TBatchProcessor.HasLineOperations( lOperations ) then
              begin
                Var lFilePath := '';
                Var lChangeReport : TChangeReport := NIL;

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

                // ChangeReport erstellen wenn aktiviert
                if TStrEditorSettings.Instance.ChangeReportEnabled then
                  lChangeReport := TChangeReport.Create( lFilePath );

                try
                  if not TBatchProcessor.ProcessLineOperations( lFilePath, lOperations, lChangeReport ) then
                    begin
                      TSessionLog.Instance.LogError( 'ProcessLineOperations failed: ' + lFilePath );
                      ExitCode := Ord( ecOperationFailed );
                      Exit;
                    end;

                  TSessionLog.Instance.LogSuccess( 'ProcessLineOperations: ' + IntToStr( Length( lOperations ) ) + ' operations on ' + lFilePath );

                  // ChangeReport ausgeben wenn vorhanden
                  if lChangeReport <> NIL then
                    WriteLn( lChangeReport.GenerateReport );
                finally
                  lChangeReport.Free;
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

            // Auto-delete config file on success (unless --keep-config is specified)
            if ( not lParams.KeepConfig ) and ( ExitCode = Ord( ecSuccess ) ) and ( not lParams.DryRun ) then
              begin
                if lParams.Verbose then
                  WriteLn( 'Auto-deleting config file: ' + lParams.ConfigFile );

                if DeleteFile( lParams.ConfigFile )
                  then WriteLn( 'Config file deleted successfully' )
                  else WriteLn( 'WARNING: Could not delete config file: ' + lParams.ConfigFile );
              end
            else
            if ( not lParams.KeepConfig ) and lParams.DryRun then
              WriteLn( 'Dry-run mode: Config file NOT deleted' )
            else
            if lParams.KeepConfig and ( ExitCode = Ord( ecSuccess ) ) then
              WriteLn( 'Config file preserved (--keep-config)' );

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

    // Auto-delete config file on success (for single operation config, unless --keep-config)
    if ( lParams.ConfigFile <> '' ) and ( not lParams.KeepConfig ) and ( ExitCode = Ord( ecSuccess ) ) and ( not lParams.DryRun ) then
      begin
        if lParams.Verbose then
          WriteLn( 'Auto-deleting config file: ' + lParams.ConfigFile );

        if DeleteFile( lParams.ConfigFile )
          then WriteLn( 'Config file deleted successfully' )
          else WriteLn( 'WARNING: Could not delete config file: ' + lParams.ConfigFile );
      end
    else
    if ( lParams.ConfigFile <> '' ) and ( not lParams.KeepConfig ) and lParams.DryRun then
      WriteLn( 'Dry-run mode: Config file NOT deleted' )
    else
    if ( lParams.ConfigFile <> '' ) and lParams.KeepConfig and ( ExitCode = Ord( ecSuccess ) ) then
      WriteLn( 'Config file preserved (--keep-config)' );
  except
    on E : Exception do
      begin
        WriteLn( 'EXCEPTION: ' + E.ClassName + ': ' + E.Message );
        TSessionLog.Instance.LogError( 'EXCEPTION: ' + E.ClassName + ': ' + E.Message );
        ExitCode := Ord( ecEncodingError );
      end;
  end;
end.
