Unit StrEditor.CaseConversion;

interface

Uses
  System.SysUtils
, System.Classes
, StrEditor.CommandLine
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Case Conversion Helper
  /// </summary>
  {$ENDREGION}
  TCaseConversionHelper = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Konvertiert Text basierend auf dem Case Conversion Type
      /// </summary>
      {$ENDREGION}
      class function ConvertCase( const aText : string; const aCaseConversion : TCaseConversion ) : string;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Konvertiert Text zu Title Case (erster Buchstabe jedes Wortes groß)
      /// </summary>
      {$ENDREGION}
      class function ToTitleCase( const aText : string ) : string;
  end;

implementation

class function TCaseConversionHelper.ConvertCase( const aText : string; const aCaseConversion : TCaseConversion ) : string;
begin
  case aCaseConversion of
    ccNone  : Result := aText;
    ccUpper : Result := UpperCase( aText );
    ccLower : Result := LowerCase( aText );
    ccTitle : Result := ToTitleCase( aText );
    else Result := aText;
  end; // of case
end;

class function TCaseConversionHelper.ToTitleCase( const aText : string ) : string;
Var
  i            : Integer;
  lCapitalize  : Boolean;
  lChar        : Char;
begin
  Result      := LowerCase( aText );
  lCapitalize := true;

  for i := 1 to Length( Result ) do
    begin
      lChar := Result[ i ];

      if lCapitalize and ( ( lChar >= 'a' ) and ( lChar <= 'z' ) ) then
        begin
          Result[ i ]  := UpCase( lChar );
          lCapitalize := false;
        end
      else
      if ( lChar = ' ' ) or ( lChar = #9 ) or ( lChar = '.' ) or ( lChar = ',' ) or ( lChar = ';' ) or ( lChar = ':' ) or ( lChar = '!' ) or ( lChar = '?' ) then
        lCapitalize := true;
    end;
end;

end.

