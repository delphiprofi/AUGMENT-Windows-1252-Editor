Unit StrEditor.Operations;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

Uses
  System.SysUtils
, System.Classes
, System.Math
, System.IOUtils
, System.NetEncoding
, System.StrUtils
, System.Generics.Collections
, Winapi.Windows
, StrEditor.Encoding
, StrEditor.Macros
, StrEditor.Diff
, StrEditor.CommandLine
, StrEditor.CaseConversion
, StrEditor.Indent
, StrEditor.Conditional
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Ergebnis einer String-Operation
  /// </summary>
  {$ENDREGION}
  TOperationResult = record
    Success      : Boolean;
    ErrorMessage : string;
    LinesChanged : Integer;
    OutputText   : string;  // For Show command output
  end;

  {$REGION 'Documentation'}
  /// <summary>
  ///   String-Operationen: Replace und Insert
  /// </summary>
  {$ENDREGION}
  TStringOperations = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Ersetzt einen String in einer Datei. Unterstützt Makros: {{LINE_NUMBER}}, {{FILE_NAME}}, {{DATE}}, {{TIME}}.
      /// </summary>
      {$ENDREGION}
      class function StrReplace( const aFilePath : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aCaseConversion : TCaseConversion = ccNone; const aIndentLevel : Integer = 0; const aConditionPattern : string = ''; const aVerbose : Boolean = false; const aOldStrIsBase64 : Boolean = false; const aNewStrIsBase64 : Boolean = false; const aMultiLine : Boolean = false; const aReplaceAll : Boolean = false ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Fügt Text nach einer bestimmten Zeile ein
      /// </summary>
      {$ENDREGION}
      class function Insert( const aFilePath : string; const aText : string; const aInsertAfterLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aTextIsBase64 : Boolean = false ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Fügt Text vor einer bestimmten Zeile ein
      /// </summary>
      {$ENDREGION}
      class function InsertBefore( const aFilePath : string; const aText : string; const aInsertBeforeLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aTextIsBase64 : Boolean = false ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Zeigt Datei-Inhalt an (encoding-aware, wie Get-Content)
      /// </summary>
      {$ENDREGION}
      class function Show( const aFilePath : string; const aStartLine : Integer; const aEndLine : Integer; const aHead : Integer; const aTail : Integer; const aLineNumbers : Boolean; const aRaw : Boolean; const aVerbose : Boolean ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Konvertiert Datei-Encoding (UTF-8 <-> Windows-1252)
      /// </summary>
      {$ENDREGION}
      class function ConvertEncoding( const aFilePath : string; const aTargetEncoding : string; const aBackup : Boolean; const aDryRun : Boolean; const aVerbose : Boolean ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Re-interpretiert Datei-Encoding (repariert kaputte Encodings)
      ///   Liest Datei als aSourceEncoding und speichert als Ziel-Encoding
      /// </summary>
      {$ENDREGION}
      class function ReinterpretEncoding( const aFilePath : string; const aSourceEncoding : string; const aBackup : Boolean; const aDryRun : Boolean; const aVerbose : Boolean ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Löscht eine einzelne Zeile aus einer Datei
      /// </summary>
      {$ENDREGION}
      class function DeleteLine( const aFilePath : string; const aLineNumber : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Löscht einen Zeilenbereich oder mehrere einzelne Zeilen aus einer Datei
      ///   aStartLine/aEndLine: Zeilenbereich (z.B. 10-20)
      ///   aLineNumbers: Komma-separierte Liste (z.B. "1,3,5")
      /// </summary>
      {$ENDREGION}
      class function DeleteLines( const aFilePath : string; const aStartLine : Integer; const aEndLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false ) : TOperationResult; overload;
      class function DeleteLines( const aFilePath : string; const aLineNumbers : string; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false ) : TOperationResult; overload;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Ersetzt eine komplette Zeile durch neuen Text
      /// </summary>
      {$ENDREGION}
      class function ReplaceLine( const aFilePath : string; const aLineNumber : Integer; const aNewText : string; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false; const aTextIsBase64 : Boolean = false ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Verschiebt Zeilen von einer Datei in eine andere
      ///   aFromFile: Quelldatei
      ///   aToFile: Zieldatei (muss existieren)
      ///   aStartLine/aEndLine: Zeilenbereich in der Quelldatei
      ///   aInsertAfterLine: Zeile in Zieldatei nach der eingefügt wird (0 = am Anfang)
      ///   aInsertBeforeLine: Zeile in Zieldatei vor der eingefügt wird
      /// </summary>
      {$ENDREGION}
      class function MoveLines( const aFromFile : string; const aToFile : string; const aStartLine : Integer; const aEndLine : Integer; const aInsertAfterLine : Integer; const aInsertBeforeLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false ) : TOperationResult;

    strict private
      class function FindAndReplace( const aLines : TStringList; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aFilePath : string; out aLinesChanged : Integer; const aVerbose : Boolean = false ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Multi-Line String Replace - Sucht und ersetzt Strings über mehrere Zeilen hinweg
      /// </summary>
      {$ENDREGION}
      class function FindAndReplaceMultiLine( const aFileContent : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aFilePath : string; out aLinesChanged : Integer; out aNewContent : string; const aReplaceAll : Boolean; const aVerbose : Boolean = false ) : Boolean;

      class function InsertText( const aLines : TStringList; const aText : string; const aInsertAfterLine : Integer; const aFilePath : string ) : Boolean;
      class function InsertTextBefore( const aLines : TStringList; const aText : string; const aInsertBeforeLine : Integer; const aFilePath : string ) : Boolean;
      class function CreateBackup( const aFilePath : string ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Dekodiert Base64-String zu normalem String
      /// </summary>
      {$ENDREGION}
      class function DecodeBase64( const aBase64 : string; out aDecoded : string ) : Boolean;
  end;

implementation

{ TStringOperations }

class function TStringOperations.StrReplace( const aFilePath : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aCaseConversion : TCaseConversion = ccNone; const aIndentLevel : Integer = 0; const aConditionPattern : string = ''; const aVerbose : Boolean = false; const aOldStrIsBase64 : Boolean = false; const aNewStrIsBase64 : Boolean = false; const aMultiLine : Boolean = false; const aReplaceAll : Boolean = false ) : TOperationResult;
Var
  lLines        : TStringList;
  lOriginal     : TStringList;
  lEncoding     : TEncodingType;
  lLinesChanged : Integer;
  lOldStr       : string;
  lNewStr       : string;
  lFileContent  : string;
  lNewContent   : string;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;

  if aMultiLine and ( aConditionPattern <> '' ) then
    begin
      Result.ErrorMessage := 'ERROR: --multi-line and --condition-pattern cannot be used together';
      Exit;
    end;

  if aOldStrIsBase64 then
    begin
      if not DecodeBase64( aOldStr, lOldStr ) then
        begin
          Result.ErrorMessage := 'Invalid Base64 string for old-str';
          Exit;
        end;
    end
  else
    lOldStr := aOldStr;

  if aNewStrIsBase64 then
    begin
      if not DecodeBase64( aNewStr, lNewStr ) then
        begin
          Result.ErrorMessage := 'Invalid Base64 string for new-str';
          Exit;
        end;
    end
  else
    lNewStr := aNewStr;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  if not TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
      Exit;
    end;

  lOriginal := NIL;

  try
    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end;

    if aMultiLine then
      begin
        lFileContent := lLines.Text;

        if not FindAndReplaceMultiLine( lFileContent, lOldStr, lNewStr, aStartLine, aEndLine, aFilePath, lLinesChanged, lNewContent, aReplaceAll, aVerbose ) then
          begin
            Result.ErrorMessage := 'String not found: ' + lOldStr;
            Exit;
          end;

        lLines.Text := lNewContent;
      end
    else
    if aConditionPattern <> '' then
      begin
        lLinesChanged := TConditionalHelper.ConditionalReplace( lLines, aConditionPattern, lOldStr, lNewStr, aStartLine, aEndLine );

        if lLinesChanged = 0 then
          begin
            Result.ErrorMessage := 'No lines matched condition: ' + aConditionPattern;
            Exit;
          end;
      end
    else begin
           if not FindAndReplace( lLines, lOldStr, lNewStr, aStartLine, aEndLine, aFilePath, lLinesChanged, aVerbose ) then
             begin
               Result.ErrorMessage := 'String not found: ' + lOldStr;
               Exit;
             end;
         end;

    if aCaseConversion <> ccNone then
      begin
        for Var i := 0 to lLines.Count - 1 do
          lLines[ i ] := TCaseConversionHelper.ConvertCase( lLines[ i ], aCaseConversion );
      end;

    if aIndentLevel <> 0 then
      TIndentHelper.ApplyIndent( lLines, aIndentLevel, aStartLine, aEndLine );

    if aDiff then
      TDiffHelper.ShowDiff( lOriginal, lLines, aFilePath );

    if not aDryRun then
      begin
        if aBackup then
          begin
            if not CreateBackup( aFilePath ) then
              begin
                Result.ErrorMessage := 'Failed to create backup: ' + aFilePath + '.bak';
                Exit;
              end;
          end;

        if not TEncodingHelper.WriteFile( aFilePath, lLines, lEncoding ) then
          begin
            Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
            Exit;
          end;
      end;

    Result.Success      := true;
    Result.LinesChanged := lLinesChanged;
  finally
    lLines.Free;

    if lOriginal <> NIL then
      lOriginal.Free;
  end;
end;

class function TStringOperations.Insert( const aFilePath : string; const aText : string; const aInsertAfterLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aTextIsBase64 : Boolean = false ) : TOperationResult;
Var
  lLines    : TStringList;
  lOriginal : TStringList;
  lEncoding : TEncodingType;
  lText     : string;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;

  if aTextIsBase64 then
    begin
      if not DecodeBase64( aText, lText ) then
        begin
          Result.ErrorMessage := 'Invalid Base64 string for text';
          Exit;
        end;
    end
  else
    lText := aText;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  if not TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
      Exit;
    end;

  lOriginal := NIL;

  try
    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end;

    if not InsertText( lLines, lText, aInsertAfterLine, aFilePath ) then
      begin
        Result.ErrorMessage := 'Invalid line number: ' + IntToStr( aInsertAfterLine );
        Exit;
      end;

    if aDiff then
      TDiffHelper.ShowDiff( lOriginal, lLines, aFilePath );

    if not aDryRun then
      begin
        if aBackup then
          begin
            if not CreateBackup( aFilePath ) then
              begin
                Result.ErrorMessage := 'Failed to create backup: ' + aFilePath + '.bak';
                Exit;
              end;
          end;

        if not TEncodingHelper.WriteFile( aFilePath, lLines, lEncoding ) then
          begin
            Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
            Exit;
          end;
      end;

    Result.Success      := true;
    Result.LinesChanged := 1;
  finally
    lLines.Free;

    if lOriginal <> NIL then
      lOriginal.Free;
  end;
end;

class function TStringOperations.InsertBefore( const aFilePath : string; const aText : string; const aInsertBeforeLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aTextIsBase64 : Boolean = false ) : TOperationResult;
Var
  lLines    : TStringList;
  lOriginal : TStringList;
  lEncoding : TEncodingType;
  lText     : string;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;

  if aTextIsBase64 then
    begin
      if not DecodeBase64( aText, lText ) then
        begin
          Result.ErrorMessage := 'Invalid Base64 string for text';
          Exit;
        end;
    end
  else
    lText := aText;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  if not TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
      Exit;
    end;

  lOriginal := NIL;

  try
    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end;

    if not InsertTextBefore( lLines, lText, aInsertBeforeLine, aFilePath ) then
      begin
        Result.ErrorMessage := 'Invalid line number: ' + IntToStr( aInsertBeforeLine );
        Exit;
      end;

    if aDiff then
      TDiffHelper.ShowDiff( lOriginal, lLines, aFilePath );

    if not aDryRun then
      begin
        if aBackup then
          begin
            if not CreateBackup( aFilePath ) then
              begin
                Result.ErrorMessage := 'Failed to create backup: ' + aFilePath + '.bak';
                Exit;
              end;
          end;

        if not TEncodingHelper.WriteFile( aFilePath, lLines, lEncoding ) then
          begin
            Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
            Exit;
          end;
      end;

    Result.Success      := true;
    Result.LinesChanged := 1;
  finally
    lLines.Free;

    if lOriginal <> NIL then
      lOriginal.Free;
  end;
end;

class function TStringOperations.FindAndReplace( const aLines : TStringList; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aFilePath : string; out aLinesChanged : Integer; const aVerbose : Boolean = false ) : Boolean;
Var
  i               : Integer;
  lStartLine      : Integer;
  lEndLine        : Integer;
  lExpandedNewStr : string;
begin
  Result        := false;
  aLinesChanged := 0;

  lStartLine := aStartLine;
  lEndLine   := aEndLine;

  if lStartLine < 1 then
    lStartLine := 1;

  if ( lEndLine < 1 ) or ( lEndLine > aLines.Count ) then
    lEndLine := aLines.Count;

  if aVerbose then
    begin
      WriteLn( 'Searching for: "', aOldStr, '"' );
      WriteLn( 'In lines ', lStartLine, ' to ', lEndLine, ' (', lEndLine - lStartLine + 1, ' lines)' );
      WriteLn;
    end;

  for i := lStartLine - 1 to lEndLine - 1 do
    begin
      if aVerbose then
        WriteLn( 'Line ', i + 1, ': "', aLines[ i ], '"' );

      if Pos( aOldStr, aLines[ i ] ) > 0 then
        begin
          if aVerbose then
            WriteLn( '  -> MATCH found at position ', Pos( aOldStr, aLines[ i ] ) );

          lExpandedNewStr := TMacroExpander.ExpandMacros( aNewStr, aFilePath, i + 1 );
          aLines[ i ]     := StringReplace( aLines[ i ], aOldStr, lExpandedNewStr, [ rfReplaceAll ] );
          Inc( aLinesChanged );
          Result := true;
        end
      else
        begin
          if aVerbose then
            WriteLn( '  -> No match' );
        end;
    end;
end;

class function TStringOperations.FindAndReplaceMultiLine( const aFileContent : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aFilePath : string; out aLinesChanged : Integer; out aNewContent : string; const aReplaceAll : Boolean; const aVerbose : Boolean = false ) : Boolean;
Var
  lIndex       : Integer;
  lStartLine   : Integer;
  lEndLine     : Integer;
  lLineCount   : Integer;
  lSearchText  : string;
  lOccurrences : Integer;
  i            : Integer;
begin
  Result        := false;
  aLinesChanged := 0;
  aNewContent   := aFileContent;

  lLineCount := 1;

  for i := 1 to Length( aFileContent ) do
    if aFileContent[ i ] = #10 then
      Inc( lLineCount );

  lStartLine := aStartLine;
  lEndLine   := aEndLine;

  if lStartLine < 1 then
    lStartLine := 1;

  if ( lEndLine < 1 ) or ( lEndLine > lLineCount ) then
    lEndLine := lLineCount;

  if aVerbose then
    begin
      WriteLn( 'Multi-Line Mode: ENABLED' );
      WriteLn( 'Replace All: ', aReplaceAll );
      WriteLn( 'Line Range: ', lStartLine, '-', lEndLine, ' (', lEndLine - lStartLine + 1, ' lines)' );
      WriteLn;
      WriteLn( 'Searching for multi-line string:' );
      WriteLn( '"', aOldStr, '"' );
      WriteLn;
    end;

  lSearchText := aFileContent;

  if aReplaceAll then
    begin
      lOccurrences := 0;
      lIndex := Pos( aOldStr, lSearchText );

      while lIndex > 0 do
        begin
          Inc( lOccurrences );
          lIndex := PosEx( aOldStr, lSearchText, lIndex + Length( aOldStr ) );
        end;

      lSearchText := StringReplace( lSearchText, aOldStr, aNewStr, [ rfReplaceAll ] );

      if lSearchText <> aFileContent then
        begin
          aNewContent := lSearchText;
          aLinesChanged := lOccurrences;
          Result := true;

          if aVerbose then
            WriteLn( 'SUCCESS: ', lOccurrences, ' replacements made' );
        end;
    end
  else begin
         lSearchText := StringReplace( lSearchText, aOldStr, aNewStr, [] );

         if lSearchText <> aFileContent then
           begin
             aNewContent := lSearchText;
             aLinesChanged := 1;
             Result := true;

             if aVerbose then
               WriteLn( 'SUCCESS: 1 replacement made' );
           end;
       end;
end;

class function TStringOperations.InsertText( const aLines : TStringList; const aText : string; const aInsertAfterLine : Integer; const aFilePath : string ) : Boolean;
Var
  lInsertPos     : Integer;
  lExpandedText  : string;
begin
  Result := false;

  lInsertPos := aInsertAfterLine;

  if ( lInsertPos < 0 ) or ( lInsertPos > aLines.Count ) then
    Exit;

  lExpandedText := TMacroExpander.ExpandMacros( aText, aFilePath, lInsertPos + 1 );
  aLines.Insert( lInsertPos, lExpandedText );
  Result := true;
end;

class function TStringOperations.InsertTextBefore( const aLines : TStringList; const aText : string; const aInsertBeforeLine : Integer; const aFilePath : string ) : Boolean;
Var
  lInsertPos     : Integer;
  lExpandedText  : string;
begin
  Result := false;

  if ( aInsertBeforeLine < 1 ) or ( aInsertBeforeLine > aLines.Count ) then
    Exit;

  lInsertPos := aInsertBeforeLine - 1;

  lExpandedText := TMacroExpander.ExpandMacros( aText, aFilePath, lInsertPos + 1 );
  aLines.Insert( lInsertPos, lExpandedText );
  Result := true;
end;

class function TStringOperations.CreateBackup( const aFilePath : string ) : Boolean;
Var
  lBackupPath : string;
begin
  Result := false;

  lBackupPath := aFilePath + '.bak';

  try
    if FileExists( lBackupPath ) then
      System.SysUtils.DeleteFile( lBackupPath );

    if not CopyFile( PChar( aFilePath ), PChar( lBackupPath ), false ) then
      Exit;

    Result := true;
  except
    Result := false;
  end;
end;

class function TStringOperations.Show( const aFilePath : string; const aStartLine : Integer; const aEndLine : Integer; const aHead : Integer; const aTail : Integer; const aLineNumbers : Boolean; const aRaw : Boolean; const aVerbose : Boolean ) : TOperationResult;
Var
  lLines     : TStringList;
  lEncoding  : TEncodingType;
  lStart     : Integer;
  lEnd       : Integer;
  i          : Integer;
  lLineNum   : string;
  lMaxDigits : Integer;
  lOutput    : TStringList;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;
  Result.OutputText   := '';

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  lLines  := TStringList.Create;
  lOutput := TStringList.Create;
  try
    if not TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
        Exit;
      end;

    if aVerbose then
      begin
        WriteLn( 'File: ' + aFilePath );
        if lEncoding = etUTF8
          then WriteLn( 'Encoding: UTF-8 with BOM' )
          else WriteLn( 'Encoding: Windows-1252 (no BOM)' );
        WriteLn( 'Total lines: ' + IntToStr( lLines.Count ) );
        WriteLn;
      end;

    if lLines.Count = 0 then
      begin
        Result.Success    := true;
        Result.OutputText := '';
        Exit;
      end;

    lStart := 1;
    lEnd   := lLines.Count;

    if aHead > 0 then
      lEnd := Min( aHead, lLines.Count );

    if aTail > 0 then
      begin
        lStart := Max( 1, lLines.Count - aTail + 1 );
        lEnd   := lLines.Count;
      end;

    if aStartLine > 0 then
      lStart := Max( 1, aStartLine );

    if aEndLine > 0 then
      lEnd := Min( aEndLine, lLines.Count );

    if lStart > lEnd then
      begin
        Result.ErrorMessage := 'Invalid line range: start > end';
        Exit;
      end;

    if aRaw then
      begin
        for i := lStart - 1 to lEnd - 1 do
          begin
            if i > lStart - 1 then
              Result.OutputText := Result.OutputText + ' ';

            Result.OutputText := Result.OutputText + lLines[ i ];
          end;

        WriteLn( Result.OutputText );
      end
    else begin
           if aLineNumbers then
             begin
               lMaxDigits := Length( IntToStr( lEnd ) );

               for i := lStart - 1 to lEnd - 1 do
                 begin
                   lLineNum := IntToStr( i + 1 );
                   while Length( lLineNum ) < lMaxDigits do
                     lLineNum := ' ' + lLineNum;

                   lOutput.Add( lLineNum + ': ' + lLines[ i ] );
                 end;
             end
           else begin
                  for i := lStart - 1 to lEnd - 1 do
                    lOutput.Add( lLines[ i ] );
                end;

           Result.OutputText := lOutput.Text;
           if Result.OutputText.EndsWith( #13#10 ) then
             Result.OutputText := Copy( Result.OutputText, 1, Length( Result.OutputText ) - 2 );

           Write( Result.OutputText );
           if not aVerbose then
             WriteLn;
         end;

    Result.Success := true;
  finally
    lLines.Free;
    lOutput.Free;
  end;
end;

class function TStringOperations.ConvertEncoding( const aFilePath : string; const aTargetEncoding : string; const aBackup : Boolean; const aDryRun : Boolean; const aVerbose : Boolean ) : TOperationResult;
Var
  lLines          : TStringList;
  lSourceEncoding : TEncodingType;
  lTargetEncoding : TEncodingType;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;
  Result.OutputText   := '';

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  if LowerCase( aTargetEncoding ) = 'utf8'
    then lTargetEncoding := etUTF8
    else
  if LowerCase( aTargetEncoding ) = 'windows1252'
    then lTargetEncoding := etWindows1252
    else begin
           Result.ErrorMessage := 'Invalid target encoding: ' + aTargetEncoding + ' (use utf8 or windows1252)';
           Exit;
         end;

  if not TEncodingHelper.ReadFile( aFilePath, lLines, lSourceEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
      Exit;
    end;

  try
    if aVerbose then
      begin
        WriteLn( 'File: ', aFilePath );
        Write( 'Source Encoding: ' );

        if lSourceEncoding = etUTF8
          then WriteLn( 'UTF-8 with BOM' )
          else WriteLn( 'Windows-1252 (no BOM)' );

        Write( 'Target Encoding: ' );

        if lTargetEncoding = etUTF8
          then WriteLn( 'UTF-8 with BOM' )
          else WriteLn( 'Windows-1252 (no BOM)' );

        WriteLn;
      end;

    if lSourceEncoding = lTargetEncoding then
      begin
        Result.ErrorMessage := 'File is already in target encoding';
        Exit;
      end;

    if aDryRun then
      begin
        WriteLn( 'DRY RUN: Would convert from ', if lSourceEncoding = etUTF8 then 'UTF-8' else 'Windows-1252', ' to ', if lTargetEncoding = etUTF8 then 'UTF-8' else 'Windows-1252' );
        Result.Success := true;
        Exit;
      end;

    if aBackup then
      begin
        if not CreateBackup( aFilePath ) then
          begin
            Result.ErrorMessage := 'Failed to create backup: ' + aFilePath + '.bak';
            Exit;
          end;
      end;

    if not TEncodingHelper.WriteFile( aFilePath, lLines, lTargetEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
        Exit;
      end;

    if aVerbose then
      WriteLn( 'Encoding converted successfully!' );

    Result.Success := true;
  finally
    lLines.Free;
  end;
end;

class function TStringOperations.ReinterpretEncoding( const aFilePath : string; const aSourceEncoding : string; const aBackup : Boolean; const aDryRun : Boolean; const aVerbose : Boolean ) : TOperationResult;
Var
  lBytes          : TBytes;
  lSourceEnc      : TEncoding;
  lTargetEncoding : TEncodingType;
  lContent        : string;
  lLines          : TStringList;
begin
  Result.Success := False;

  if aVerbose then
    begin
      WriteLn( 'File: ' + aFilePath );
      WriteLn( 'Source Encoding (to interpret as): ' + aSourceEncoding );
    end;

  if LowerCase( aSourceEncoding ) = 'utf8' then
    begin
      lSourceEnc      := TEncoding.UTF8;
      lTargetEncoding := etWindows1252;

      if aVerbose then
        WriteLn( 'Target Encoding: Windows-1252 (no BOM)' );
    end
  else
  if LowerCase( aSourceEncoding ) = 'windows1252' then
    begin
      lSourceEnc      := TEncoding.GetEncoding( 1252 );
      lTargetEncoding := etUTF8;

      if aVerbose then
        WriteLn( 'Target Encoding: UTF-8 with BOM' );
    end
  else
    begin
      Result.ErrorMessage := 'Invalid source encoding: ' + aSourceEncoding + ' (use utf8 or windows1252)';
      Exit;
    end;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  lBytes := TFile.ReadAllBytes( aFilePath );

  lContent := lSourceEnc.GetString( lBytes );

  if aDryRun then
    begin
      WriteLn( 'DRY RUN: Would reinterpret as ', aSourceEncoding, ' and save as ', if lTargetEncoding = etUTF8 then 'UTF-8' else 'Windows-1252' );
      Result.Success := true;
      Exit;
    end;

  if aBackup then
    if not CreateBackup( aFilePath ) then
      begin
        Result.ErrorMessage := 'Failed to create backup: ' + aFilePath + '.bak';
        Exit;
      end;

  lLines := TStringList.Create;

  try
    lLines.Text := lContent;

    if not TEncodingHelper.WriteFile( aFilePath, lLines, lTargetEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
        Exit;
      end;

    if aVerbose then
      WriteLn( 'Encoding reinterpreted successfully!' );

    Result.Success := true;
  finally
    lLines.Free;
  end;
end;

class function TStringOperations.DecodeBase64( const aBase64 : string; out aDecoded : string ) : Boolean;
Var
  lBytes : TBytes;
begin
  if aBase64 = '' then
    begin
      aDecoded := '';
      Result   := true;
      Exit;
    end;

  Result := false;

  try
    lBytes   := TNetEncoding.Base64.DecodeStringToBytes( aBase64 );
    aDecoded := TEncoding.UTF8.GetString( lBytes );
    Result   := true;
  except
    on E : Exception do
      aDecoded := '';
  end;
end;

class function TStringOperations.DeleteLine( const aFilePath : string; const aLineNumber : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false ) : TOperationResult;
Var
  lLines    : TStringList;
  lOriginal : TStringList;
  lEncoding : TEncodingType;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  if not TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
      Exit;
    end;

  try
    if ( aLineNumber < 1 ) or ( aLineNumber > lLines.Count ) then
      begin
        Result.ErrorMessage := 'Invalid line number: ' + IntToStr( aLineNumber ) + ' (file has ' + IntToStr( lLines.Count ) + ' lines)';
        Exit;
      end;

    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end
      else lOriginal := NIL;

    lLines.Delete( aLineNumber - 1 );
    Result.LinesChanged := 1;

    if aDiff then
      begin
        TDiffHelper.ShowDiff( lOriginal, lLines, aFilePath );
        lOriginal.Free;
      end;

    if aDryRun then
      begin
        if aVerbose then
          WriteLn( 'DRY-RUN: Would delete line ', aLineNumber );

        Result.Success := true;
        Exit;
      end;

    if aBackup then
      if not CreateBackup( aFilePath ) then
        begin
          Result.ErrorMessage := 'Failed to create backup';
          Exit;
        end;

    if not TEncodingHelper.WriteFile( aFilePath, lLines, lEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
        Exit;
      end;

    if aVerbose then
      WriteLn( 'SUCCESS: Deleted line ', aLineNumber );

    Result.Success := true;
  finally
    lLines.Free;
  end;
end;

class function TStringOperations.DeleteLines( const aFilePath : string; const aStartLine : Integer; const aEndLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false ) : TOperationResult;
Var
  lLines    : TStringList;
  lOriginal : TStringList;
  lEncoding : TEncodingType;
  i         : Integer;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  if not TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
      Exit;
    end;

  try
    if ( aStartLine < 1 ) or ( aStartLine > lLines.Count ) then
      begin
        Result.ErrorMessage := 'Invalid line number: ' + IntToStr( aStartLine ) + ' (file has ' + IntToStr( lLines.Count ) + ' lines)';
        Exit;
      end;

    if ( aEndLine < aStartLine ) or ( aEndLine > lLines.Count ) then
      begin
        Result.ErrorMessage := 'Invalid line number: ' + IntToStr( aEndLine ) + ' (file has ' + IntToStr( lLines.Count ) + ' lines)';
        Exit;
      end;

    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end
      else lOriginal := NIL;

    for i := aEndLine - 1 downto aStartLine - 1 do
      lLines.Delete( i );

    Result.LinesChanged := aEndLine - aStartLine + 1;

    if aDiff then
      begin
        TDiffHelper.ShowDiff( lOriginal, lLines, aFilePath );
        lOriginal.Free;
      end;

    if aDryRun then
      begin
        if aVerbose then
          WriteLn( 'DRY-RUN: Would delete lines ', aStartLine, '-', aEndLine );

        Result.Success := true;
        Exit;
      end;

    if aBackup then
      if not CreateBackup( aFilePath ) then
        begin
          Result.ErrorMessage := 'Failed to create backup';
          Exit;
        end;

    if not TEncodingHelper.WriteFile( aFilePath, lLines, lEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
        Exit;
      end;

    if aVerbose then
      WriteLn( 'SUCCESS: Deleted lines ', aStartLine, '-', aEndLine );

    Result.Success := true;
  finally
    lLines.Free;
  end;
end;

class function TStringOperations.DeleteLines( const aFilePath : string; const aLineNumbers : string; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false ) : TOperationResult;
Var
  lLines       : TStringList;
  lOriginal    : TStringList;
  lEncoding    : TEncodingType;
  lLineNumbers : TArray<Integer>;
  lParts       : TArray<string>;
  i            : Integer;
  lLineNum     : Integer;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  if not TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
      Exit;
    end;

  try
    lParts := aLineNumbers.Split( [ ',' ] );
    SetLength( lLineNumbers, Length( lParts ) );

    for i := 0 to High( lParts ) do
      begin
        if not TryStrToInt( Trim( lParts[ i ] ), lLineNum ) then
          begin
            Result.ErrorMessage := 'Invalid line number: ' + lParts[ i ];
            Exit;
          end;

        if ( lLineNum < 1 ) or ( lLineNum > lLines.Count ) then
          begin
            Result.ErrorMessage := 'Invalid line number: ' + IntToStr( lLineNum ) + ' (file has ' + IntToStr( lLines.Count ) + ' lines)';
            Exit;
          end;

        lLineNumbers[ i ] := lLineNum;
      end;

    TArray.Sort<Integer>( lLineNumbers );

    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end
      else lOriginal := NIL;

    for i := High( lLineNumbers ) downto 0 do
      lLines.Delete( lLineNumbers[ i ] - 1 );

    Result.LinesChanged := Length( lLineNumbers );

    if aDiff then
      begin
        TDiffHelper.ShowDiff( lOriginal, lLines, aFilePath );
        lOriginal.Free;
      end;

    if aDryRun then
      begin
        if aVerbose then
          WriteLn( 'DRY-RUN: Would delete lines ', aLineNumbers );

        Result.Success := true;
        Exit;
      end;

    if aBackup then
      if not CreateBackup( aFilePath ) then
        begin
          Result.ErrorMessage := 'Failed to create backup';
          Exit;
        end;

    if not TEncodingHelper.WriteFile( aFilePath, lLines, lEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
        Exit;
      end;

    if aVerbose then
      WriteLn( 'SUCCESS: Deleted ', Length( lLineNumbers ), ' lines' );

    Result.Success := true;
  finally
    lLines.Free;
  end;
end;

class function TStringOperations.ReplaceLine( const aFilePath : string; const aLineNumber : Integer; const aNewText : string; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false; const aTextIsBase64 : Boolean = false ) : TOperationResult;
Var
  lLines         : TStringList;
  lOriginal      : TStringList;
  lEncoding      : TEncodingType;
  lText          : string;
  lExpandedText  : string;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;

  if not FileExists( aFilePath ) then
    begin
      Result.ErrorMessage := 'File not found: ' + aFilePath;
      Exit;
    end;

  if aTextIsBase64 then
    begin
      if not DecodeBase64( aNewText, lText ) then
        begin
          Result.ErrorMessage := 'Invalid Base64 string';
          Exit;
        end;
    end
    else lText := aNewText;

  if not TEncodingHelper.ReadFile( aFilePath, lLines, lEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read file: ' + aFilePath;
      Exit;
    end;

  try
    if ( aLineNumber < 1 ) or ( aLineNumber > lLines.Count ) then
      begin
        Result.ErrorMessage := 'Invalid line number: ' + IntToStr( aLineNumber ) + ' (file has ' + IntToStr( lLines.Count ) + ' lines)';
        Exit;
      end;

    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end
      else lOriginal := NIL;

    lExpandedText := TMacroExpander.ExpandMacros( lText, aFilePath, aLineNumber );
    lLines[ aLineNumber - 1 ] := lExpandedText;
    Result.LinesChanged := 1;

    if aDiff then
      begin
        TDiffHelper.ShowDiff( lOriginal, lLines, aFilePath );
        lOriginal.Free;
      end;

    if aDryRun then
      begin
        if aVerbose then
          WriteLn( 'DRY-RUN: Would replace line ', aLineNumber, ' with: ', lExpandedText );

        Result.Success := true;
        Exit;
      end;

    if aBackup then
      if not CreateBackup( aFilePath ) then
        begin
          Result.ErrorMessage := 'Failed to create backup';
          Exit;
        end;

    if not TEncodingHelper.WriteFile( aFilePath, lLines, lEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to write file: ' + aFilePath;
        Exit;
      end;

    if aVerbose then
      WriteLn( 'SUCCESS: Replaced line ', aLineNumber );

    Result.Success := true;
  finally
    lLines.Free;
  end;
end;

class function TStringOperations.MoveLines( const aFromFile : string; const aToFile : string; const aStartLine : Integer; const aEndLine : Integer; const aInsertAfterLine : Integer; const aInsertBeforeLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aVerbose : Boolean = false ) : TOperationResult;
Var
  lFromLines       : TStringList;
  lToLines         : TStringList;
  lFromOriginal    : TStringList;
  lToOriginal      : TStringList;
  lFromEncoding    : TEncodingType;
  lToEncoding      : TEncodingType;
  lMovedLines      : TStringList;
  lInsertPos       : Integer;
  i                : Integer;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;

  // Validate source file
  if not FileExists( aFromFile ) then
    begin
      Result.ErrorMessage := 'Source file not found: ' + aFromFile;
      Exit;
    end;

  // Validate target file (must exist)
  if not FileExists( aToFile ) then
    begin
      Result.ErrorMessage := 'Target file not found: ' + aToFile + ' (target file must exist)';
      Exit;
    end;

  // Read source file
  if not TEncodingHelper.ReadFile( aFromFile, lFromLines, lFromEncoding ) then
    begin
      Result.ErrorMessage := 'Failed to read source file: ' + aFromFile;
      Exit;
    end;

  // Read target file
  if not TEncodingHelper.ReadFile( aToFile, lToLines, lToEncoding ) then
    begin
      lFromLines.Free;
      Result.ErrorMessage := 'Failed to read target file: ' + aToFile;
      Exit;
    end;

  lFromOriginal := NIL;
  lToOriginal   := NIL;
  lMovedLines   := TStringList.Create;

  try
    // Validate line range in source file
    if ( aStartLine < 1 ) or ( aStartLine > lFromLines.Count ) then
      begin
        Result.ErrorMessage := 'Invalid start line: ' + IntToStr( aStartLine ) + ' (source file has ' + IntToStr( lFromLines.Count ) + ' lines)';
        Exit;
      end;

    if ( aEndLine < aStartLine ) or ( aEndLine > lFromLines.Count ) then
      begin
        Result.ErrorMessage := 'Invalid end line: ' + IntToStr( aEndLine ) + ' (source file has ' + IntToStr( lFromLines.Count ) + ' lines)';
        Exit;
      end;

    // Determine insert position in target file
    if aInsertAfterLine > 0 then
      begin
        if aInsertAfterLine > lToLines.Count then
          begin
            Result.ErrorMessage := 'Invalid insert-after-line: ' + IntToStr( aInsertAfterLine ) + ' (target file has ' + IntToStr( lToLines.Count ) + ' lines)';
            Exit;
          end;

        lInsertPos := aInsertAfterLine;
      end
    else
    if aInsertBeforeLine > 0 then
      begin
        if ( aInsertBeforeLine < 1 ) or ( aInsertBeforeLine > lToLines.Count ) then
          begin
            Result.ErrorMessage := 'Invalid insert-before-line: ' + IntToStr( aInsertBeforeLine ) + ' (target file has ' + IntToStr( lToLines.Count ) + ' lines)';
            Exit;
          end;

        lInsertPos := aInsertBeforeLine - 1;
      end
    else begin
           Result.ErrorMessage := 'Either insert-after-line or insert-before-line must be specified';
           Exit;
         end;

    if aVerbose then
      begin
        WriteLn( 'Source file: ', aFromFile );
        WriteLn( 'Source encoding: ', if lFromEncoding = etUTF8 then 'UTF-8 with BOM' else 'Windows-1252' );
        WriteLn( 'Source lines: ', lFromLines.Count );
        WriteLn( 'Moving lines: ', aStartLine, '-', aEndLine, ' (', aEndLine - aStartLine + 1, ' lines)' );
        WriteLn;
        WriteLn( 'Target file: ', aToFile );
        WriteLn( 'Target encoding: ', if lToEncoding = etUTF8 then 'UTF-8 with BOM' else 'Windows-1252' );
        WriteLn( 'Target lines: ', lToLines.Count );
        WriteLn( 'Insert position: after line ', lInsertPos );
        WriteLn;
      end;

    // Store originals for diff
    if aDiff then
      begin
        lFromOriginal := TStringList.Create;
        lFromOriginal.Text := lFromLines.Text;
        lToOriginal := TStringList.Create;
        lToOriginal.Text := lToLines.Text;
      end;

    // Extract lines to move
    for i := aStartLine - 1 to aEndLine - 1 do
      lMovedLines.Add( lFromLines[ i ] );

    // Insert into target file
    for i := lMovedLines.Count - 1 downto 0 do
      lToLines.Insert( lInsertPos, lMovedLines[ i ] );

    // Delete from source file (from high to low to preserve indices)
    for i := aEndLine - 1 downto aStartLine - 1 do
      lFromLines.Delete( i );

    Result.LinesChanged := lMovedLines.Count;

    // Show diff
    if aDiff then
      begin
        WriteLn( '=== Source file changes ===' );
        TDiffHelper.ShowDiff( lFromOriginal, lFromLines, aFromFile );
        WriteLn;
        WriteLn( '=== Target file changes ===' );
        TDiffHelper.ShowDiff( lToOriginal, lToLines, aToFile );
      end;

    if aDryRun then
      begin
        if aVerbose then
          WriteLn( 'DRY-RUN: Would move ', lMovedLines.Count, ' lines from ', aFromFile, ' to ', aToFile );

        Result.Success := true;
        Exit;
      end;

    // Create backups
    if aBackup then
      begin
        if not CreateBackup( aFromFile ) then
          begin
            Result.ErrorMessage := 'Failed to create backup for source file: ' + aFromFile + '.bak';
            Exit;
          end;

        if not CreateBackup( aToFile ) then
          begin
            Result.ErrorMessage := 'Failed to create backup for target file: ' + aToFile + '.bak';
            Exit;
          end;
      end;

    // Write source file
    if not TEncodingHelper.WriteFile( aFromFile, lFromLines, lFromEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to write source file: ' + aFromFile;
        Exit;
      end;

    // Write target file
    if not TEncodingHelper.WriteFile( aToFile, lToLines, lToEncoding ) then
      begin
        Result.ErrorMessage := 'Failed to write target file: ' + aToFile;
        Exit;
      end;

    if aVerbose then
      WriteLn( 'SUCCESS: Moved ', lMovedLines.Count, ' lines from ', aFromFile, ' to ', aToFile );

    Result.Success := true;
  finally
    lFromLines.Free;
    lToLines.Free;
    lMovedLines.Free;

    if lFromOriginal <> NIL then
      lFromOriginal.Free;

    if lToOriginal <> NIL then
      lToOriginal.Free;
  end;
end;

end.

