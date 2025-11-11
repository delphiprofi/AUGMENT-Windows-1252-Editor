Unit StrEditor.Operations;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

Uses
  System.SysUtils
, System.Classes
, Winapi.Windows
, StrEditor.Encoding
, StrEditor.Macros
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
      class function StrReplace( const aFilePath : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false ) : TOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Fügt Text nach einer bestimmten Zeile ein
      /// </summary>
      {$ENDREGION}
      class function Insert( const aFilePath : string; const aText : string; const aInsertAfterLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false ) : TOperationResult;

    strict private
      class function FindAndReplace( const aLines : TStringList; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aFilePath : string; out aLinesChanged : Integer ) : Boolean;
      class function InsertText( const aLines : TStringList; const aText : string; const aInsertAfterLine : Integer; const aFilePath : string ) : Boolean;
      class function CreateBackup( const aFilePath : string ) : Boolean;
  end;

implementation

{ TStringOperations }

class function TStringOperations.StrReplace( const aFilePath : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false ) : TOperationResult;
Var
  lLines        : TStringList;
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

  try
    if not FindAndReplace( lLines, aOldStr, aNewStr, aStartLine, aEndLine, aFilePath, lLinesChanged ) then
      begin
        Result.ErrorMessage := 'String not found: ' + aOldStr;
        Exit;
      end;

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
  end;
end;

class function TStringOperations.Insert( const aFilePath : string; const aText : string; const aInsertAfterLine : Integer; const aDryRun : Boolean = false; const aBackup : Boolean = false ) : TOperationResult;
Var
  lLines    : TStringList;
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
    if not InsertText( lLines, aText, aInsertAfterLine, aFilePath ) then
      begin
        Result.ErrorMessage := 'Invalid line number: ' + IntToStr( aInsertAfterLine );
        Exit;
      end;

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
  end;
end;

class function TStringOperations.FindAndReplace( const aLines : TStringList; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer; const aFilePath : string; out aLinesChanged : Integer ) : Boolean;
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

  for i := lStartLine - 1 to lEndLine - 1 do
    begin
      if Pos( aOldStr, aLines[ i ] ) > 0 then
        begin
          lExpandedNewStr := TMacroExpander.ExpandMacros( aNewStr, aFilePath, i + 1 );
          aLines[ i ]     := StringReplace( aLines[ i ], aOldStr, lExpandedNewStr, [ rfReplaceAll ] );
          Inc( aLinesChanged );
          Result := true;
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

end.

