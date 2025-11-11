Unit StrEditor.Operations;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

Uses
  System.SysUtils
, System.Classes
, System.Math
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
      class function StrReplace( const aFilePath : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aCaseConversion : TCaseConversion = ccNone; const aIndentLevel : Integer = 0; const aConditionPattern : string = ''; const aVerbose : Boolean = false ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Fügt Text nach einer bestimmten Zeile ein
      /// </summary>
      {$ENDREGION}
      class function Insert( const aFilePath : string; const aText : string; const aInsertAfterLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false ) : TOperationResult;

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

    strict private
      class function FindAndReplace( const aLines : TStringList; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aFilePath : string; out aLinesChanged : Integer; const aVerbose : Boolean = false ) : Boolean;
      class function InsertText( const aLines : TStringList; const aText : string; const aInsertAfterLine : Integer; const aFilePath : string ) : Boolean;
      class function CreateBackup( const aFilePath : string ) : Boolean;
  end;

implementation

{ TStringOperations }

class function TStringOperations.StrReplace( const aFilePath : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false; const aCaseConversion : TCaseConversion = ccNone; const aIndentLevel : Integer = 0; const aConditionPattern : string = ''; const aVerbose : Boolean = false ) : TOperationResult;
Var
  lLines        : TStringList;
  lOriginal     : TStringList;
  lEncoding     : TEncodingType;
  lLinesChanged : Integer;
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

  lOriginal := NIL;

  try
    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end;

    if aConditionPattern <> '' then
      begin
        lLinesChanged := TConditionalHelper.ConditionalReplace( lLines, aConditionPattern, aOldStr, aNewStr, aStartLine, aEndLine );

        if lLinesChanged = 0 then
          begin
            Result.ErrorMessage := 'No lines matched condition: ' + aConditionPattern;
            Exit;
          end;
      end
    else begin
           if not FindAndReplace( lLines, aOldStr, aNewStr, aStartLine, aEndLine, aFilePath, lLinesChanged, aVerbose ) then
             begin
               Result.ErrorMessage := 'String not found: ' + aOldStr;
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

class function TStringOperations.Insert( const aFilePath : string; const aText : string; const aInsertAfterLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false; const aDiff : Boolean = false ) : TOperationResult;
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

  lOriginal := NIL;

  try
    if aDiff then
      begin
        lOriginal := TStringList.Create;
        lOriginal.Text := lLines.Text;
      end;

    if not InsertText( lLines, aText, aInsertAfterLine, aFilePath ) then
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

end.

