Unit StrEditor.Macros;

interface

Uses
  System.SysUtils
, System.Classes
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Macro expansion for replacement strings. Supports variables like {{LINE_NUMBER}}, {{FILE_NAME}}, {{DATE}}, {{TIME}}.
  /// </summary>
  {$ENDREGION}
  TMacroExpander = class
    strict private
      class function GetCurrentDate : string;
      class function GetCurrentTime : string;
      class function ExtractFileName( const aFilePath : string ) : string;
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Expands macros in the replacement string. Supported macros: {{LINE_NUMBER}}, {{FILE_NAME}}, {{DATE}}, {{TIME}}.
      /// </summary>
      /// <param name="aReplacement">Replacement string with macros</param>
      /// <param name="aFilePath">File path for {{FILE_NAME}}</param>
      /// <param name="aLineNumber">Line number for {{LINE_NUMBER}}</param>
      /// <returns>Expanded replacement string</returns>
      {$ENDREGION}
      class function ExpandMacros( const aReplacement : string; const aFilePath : string; aLineNumber : Integer ) : string;
  end;

implementation

class function TMacroExpander.GetCurrentDate : string;
begin
  Result := FormatDateTime( 'yyyy-mm-dd', Now );
end;

class function TMacroExpander.GetCurrentTime : string;
begin
  Result := FormatDateTime( 'hh:nn:ss', Now );
end;

class function TMacroExpander.ExtractFileName( const aFilePath : string ) : string;
begin
  Result := System.SysUtils.ExtractFileName( aFilePath );
end;

class function TMacroExpander.ExpandMacros( const aReplacement : string; const aFilePath : string; aLineNumber : Integer ) : string;
begin
  Result := aReplacement;

  Result := StringReplace( Result, '{{LINE_NUMBER}}', IntToStr( aLineNumber ), [ rfReplaceAll, rfIgnoreCase ] );
  Result := StringReplace( Result, '{{FILE_NAME}}', ExtractFileName( aFilePath ), [ rfReplaceAll, rfIgnoreCase ] );
  Result := StringReplace( Result, '{{DATE}}', GetCurrentDate, [ rfReplaceAll, rfIgnoreCase ] );
  Result := StringReplace( Result, '{{TIME}}', GetCurrentTime, [ rfReplaceAll, rfIgnoreCase ] );
end;

end.

