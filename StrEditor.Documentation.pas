Unit StrEditor.Documentation;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

Uses
  System.SysUtils
, System.Classes
, System.IOUtils
, System.Math
, System.Generics.Collections
, WinApi.ShellAPI
, WinApi.Windows
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Result of documentation operation
  /// </summary>
  {$ENDREGION}
  TDocumentationResult = record
    Success      : Boolean;
    Content      : string;
    ErrorMessage : string;
  end;

  {$REGION 'Documentation'}
  /// <summary>
  ///   Documentation display and management
  /// </summary>
  {$ENDREGION}
  TDocumentation = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Shows documentation file content
      /// </summary>
      /// <param name="aFilePath">Path to documentation file</param>
      /// <param name="aHead">Show first N lines (0 = all)</param>
      /// <param name="aTail">Show last N lines (0 = all)</param>
      /// <param name="aLineNumbers">Show line numbers</param>
      {$ENDREGION}
      class function ShowDocs( const aFilePath : string; const aHead : Integer = 0; const aTail : Integer = 0; const aLineNumbers : Boolean = false ) : TDocumentationResult;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Gets the path to documentation file in exe directory
      /// </summary>
      /// <param name="aFileName">Documentation file name (default: README.md)</param>
      {$ENDREGION}
      class function GetDocumentationPath( const aFileName : string = '' ) : string;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Lists available documentation files in exe directory
      /// </summary>
      {$ENDREGION}
      class function ListDocumentationFiles : TArray<string>;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Opens documentation file in default browser
      /// </summary>
      /// <param name="aFilePath">Path to documentation file</param>
      {$ENDREGION}
      class function OpenInBrowser( const aFilePath : string ) : TDocumentationResult;
  end;

implementation

{ TDocumentation }

class function TDocumentation.ShowDocs( const aFilePath : string; const aHead : Integer; const aTail : Integer; const aLineNumbers : Boolean ) : TDocumentationResult;
Var
  lLines : TStringList;
  i      : Integer;
  lStart : Integer;
  lEnd   : Integer;
begin
  Result.Success      := false;
  Result.Content      := '';
  Result.ErrorMessage := '';

  if not TFile.Exists( aFilePath ) then
    begin
      Result.ErrorMessage := 'Documentation file not found: ' + aFilePath;
      Exit;
    end;

  try
    lLines := TStringList.Create;
    try
      lLines.LoadFromFile( aFilePath, TEncoding.UTF8 );

      if lLines.Count = 0 then
        begin
          Result.ErrorMessage := 'Documentation file is empty';
          Exit;
        end;

      lStart := 0;
      lEnd   := lLines.Count - 1;

      if aHead > 0 then
        lEnd := Min( aHead - 1, lEnd );

      if aTail > 0 then
        lStart := Max( 0, lLines.Count - aTail );

      for i := lStart to lEnd do
        begin
          if aLineNumbers
            then Result.Content := Result.Content + IntToStr( i + 1 ) + ': ' + lLines[ i ] + sLineBreak
            else Result.Content := Result.Content + lLines[ i ] + sLineBreak;
        end;

      Result.Success := true;
    finally
      lLines.Free;
    end;
  except
    on E : Exception do
      begin
        Result.ErrorMessage := 'Error reading documentation: ' + E.Message;
      end;
  end;
end;

class function TDocumentation.GetDocumentationPath( const aFileName : string ) : string;
Var
  lExePath : string;
  lDocFile : string;
begin
  lExePath := ExtractFilePath( ParamStr( 0 ) );

  if aFileName <> ''
    then lDocFile := aFileName
    else lDocFile := 'README.md';

  Result := TPath.Combine( lExePath, lDocFile );
end;

class function TDocumentation.ListDocumentationFiles : TArray<string>;
Var
  lExePath : string;
  lFiles   : TArray<string>;
  lResult  : TList<string>;
  lFile    : string;
begin
  lExePath := ExtractFilePath( ParamStr( 0 ) );
  lResult  := TList<string>.Create;
  try
    lFiles := TDirectory.GetFiles( lExePath, '*.md' );

    for lFile in lFiles do
      lResult.Add( ExtractFileName( lFile ) );

    Result := lResult.ToArray;
  finally
    lResult.Free;
  end;
end;

class function TDocumentation.OpenInBrowser( const aFilePath : string ) : TDocumentationResult;
begin
  Result.Success      := false;
  Result.Content      := '';
  Result.ErrorMessage := '';

  if not TFile.Exists( aFilePath ) then
    begin
      Result.ErrorMessage := 'Documentation file not found: ' + aFilePath;
      Exit;
    end;

  try
    ShellExecute( 0, 'open', PChar( aFilePath ), NIL, NIL, SW_SHOWNORMAL );
    Result.Success := true;
    Result.Content := 'Opened in browser: ' + aFilePath;
  except
    on E : Exception do
      begin
        Result.ErrorMessage := 'Error opening browser: ' + E.Message;
      end;
  end;
end;

end.

