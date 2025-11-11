Unit StrEditor.Regex;

interface

Uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  Winapi.Windows,
  StrEditor.Encoding,
  StrEditor.Macros;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Ergebnis einer Regex-Operation
  /// </summary>
  {$ENDREGION}
  TRegexOperationResult = record
    Success      : Boolean;
    ErrorMessage : string;
    LinesChanged : Integer;
    MatchCount   : Integer;
  end;

  {$REGION 'Documentation'}
  /// <summary>
  ///   Regex-Operationen: Pattern-basierte Ersetzung mit Capture Groups
  /// </summary>
  {$ENDREGION}
  TRegexOperations = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Ersetzt Text basierend auf einem Regular Expression Pattern. Unterstützt Makros: {{LINE_NUMBER}}, {{FILE_NAME}}, {{DATE}}, {{TIME}}.
      /// </summary>
      {$ENDREGION}
      class function RegexReplace( const aFilePath : string; const aPattern : string; const aReplacement : string; const aStartLine : Integer; const aEndLine : Integer; const aCaseInsensitive : Boolean; const aMultiLine : Boolean; const aDryRun : Boolean = false; const aBackup : Boolean = false ) : TRegexOperationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Testet ein Regex-Pattern und zeigt Matches ohne Änderungen
      /// </summary>
      {$ENDREGION}
      class function RegexTest( const aFilePath : string; const aPattern : string; const aStartLine : Integer; const aEndLine : Integer; const aCaseInsensitive : Boolean; const aMultiLine : Boolean ) : TRegexOperationResult;

    strict private
      class function FindAndReplaceRegex( const aLines : TStringList; const aPattern : string; const aReplacement : string; const aStartLine : Integer; const aEndLine : Integer; const aCaseInsensitive : Boolean; const aMultiLine : Boolean; const aFilePath : string; out aLinesChanged : Integer; out aMatchCount : Integer ) : Boolean;
      class function TestRegex( const aLines : TStringList; const aPattern : string; const aStartLine : Integer; const aEndLine : Integer; const aCaseInsensitive : Boolean; const aMultiLine : Boolean; out aMatchCount : Integer ) : Boolean;
      class function CreateBackup( const aFilePath : string ) : Boolean;
  end;

implementation

{ TRegexOperations }

class function TRegexOperations.RegexReplace( const aFilePath : string; const aPattern : string; const aReplacement : string; const aStartLine : Integer; const aEndLine : Integer; const aCaseInsensitive : Boolean; const aMultiLine : Boolean; const aDryRun : Boolean = false; const aBackup : Boolean = false ) : TRegexOperationResult;
Var
  lLines        : TStringList;
  lEncoding     : TEncodingType;
  lLinesChanged : Integer;
  lMatchCount   : Integer;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;
  Result.MatchCount   := 0;

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
    if not FindAndReplaceRegex( lLines, aPattern, aReplacement, aStartLine, aEndLine, aCaseInsensitive, aMultiLine, aFilePath, lLinesChanged, lMatchCount ) then
      begin
        Result.ErrorMessage := 'Pattern not found: ' + aPattern;
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
    Result.MatchCount   := lMatchCount;
  finally
    lLines.Free;
  end;
end;

class function TRegexOperations.RegexTest( const aFilePath : string; const aPattern : string; const aStartLine : Integer; const aEndLine : Integer; const aCaseInsensitive : Boolean; const aMultiLine : Boolean ) : TRegexOperationResult;
Var
  lLines      : TStringList;
  lEncoding   : TEncodingType;
  lMatchCount : Integer;
begin
  Result.Success      := false;
  Result.ErrorMessage := '';
  Result.LinesChanged := 0;
  Result.MatchCount   := 0;

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
    if not TestRegex( lLines, aPattern, aStartLine, aEndLine, aCaseInsensitive, aMultiLine, lMatchCount ) then
      begin
        Result.ErrorMessage := 'Pattern not found: ' + aPattern;
        Exit;
      end;

    Result.Success    := true;
    Result.MatchCount := lMatchCount;
  finally
    lLines.Free;
  end;
end;

class function TRegexOperations.FindAndReplaceRegex( const aLines : TStringList; const aPattern : string; const aReplacement : string; const aStartLine : Integer; const aEndLine : Integer; const aCaseInsensitive : Boolean; const aMultiLine : Boolean; const aFilePath : string; out aLinesChanged : Integer; out aMatchCount : Integer ) : Boolean;
Var
  i                   : Integer;
  lStartLine          : Integer;
  lEndLine            : Integer;
  lOptions            : TRegExOptions;
  lRegex              : TRegEx;
  lNewLine            : string;
  lExpandedReplacement : string;
begin
  Result        := false;
  aLinesChanged := 0;
  aMatchCount   := 0;

  lStartLine := aStartLine;
  lEndLine   := aEndLine;

  if lStartLine < 1 then
    lStartLine := 1;

  if ( lEndLine < 1 ) or ( lEndLine > aLines.Count ) then
    lEndLine := aLines.Count;

  lOptions := [ roNotEmpty ];

  if aCaseInsensitive then
    lOptions := lOptions + [ roIgnoreCase ];

  if aMultiLine then
    lOptions := lOptions + [ roMultiLine ];

  lRegex := TRegEx.Create( aPattern, lOptions );

  for i := lStartLine - 1 to lEndLine - 1 do
    begin
      if lRegex.IsMatch( aLines[ i ] ) then
        begin
          aMatchCount          := aMatchCount + lRegex.Matches( aLines[ i ] ).Count;
          lExpandedReplacement := TMacroExpander.ExpandMacros( aReplacement, aFilePath, i + 1 );
          lNewLine             := lRegex.Replace( aLines[ i ], lExpandedReplacement );

          if lNewLine <> aLines[ i ] then
            begin
              aLines[ i ] := lNewLine;
              Inc( aLinesChanged );
            end;

          Result := true;
        end;
    end;
end;

class function TRegexOperations.TestRegex( const aLines : TStringList; const aPattern : string; const aStartLine : Integer; const aEndLine : Integer; const aCaseInsensitive : Boolean; const aMultiLine : Boolean; out aMatchCount : Integer ) : Boolean;
Var
  i          : Integer;
  lStartLine : Integer;
  lEndLine   : Integer;
  lOptions   : TRegExOptions;
  lRegex     : TRegEx;
begin
  Result      := false;
  aMatchCount := 0;

  lStartLine := aStartLine;
  lEndLine   := aEndLine;

  if lStartLine < 1 then
    lStartLine := 1;

  if ( lEndLine < 1 ) or ( lEndLine > aLines.Count ) then
    lEndLine := aLines.Count;

  lOptions := [ roNotEmpty ];

  if aCaseInsensitive then
    lOptions := lOptions + [ roIgnoreCase ];

  if aMultiLine then
    lOptions := lOptions + [ roMultiLine ];

  lRegex := TRegEx.Create( aPattern, lOptions );

  for i := lStartLine - 1 to lEndLine - 1 do
    begin
      if lRegex.IsMatch( aLines[ i ] ) then
        begin
          aMatchCount := aMatchCount + lRegex.Matches( aLines[ i ] ).Count;
          Result      := true;
        end;
    end;
end;

class function TRegexOperations.CreateBackup( const aFilePath : string ) : Boolean;
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

