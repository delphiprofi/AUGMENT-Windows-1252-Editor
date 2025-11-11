Unit StrEditor.Indent;

interface

Uses
  System.SysUtils
, System.Classes
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Indent/Outdent Helper
  /// </summary>
  {$ENDREGION}
  TIndentHelper = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Fügt Indentation hinzu oder entfernt sie
      /// </summary>
      {$ENDREGION}
      class function ApplyIndent( const aLines : TStringList; const aIndentLevel : Integer; const aStartLine : Integer; const aEndLine : Integer ) : Integer;
  end;

implementation

class function TIndentHelper.ApplyIndent( const aLines : TStringList; const aIndentLevel : Integer; const aStartLine : Integer; const aEndLine : Integer ) : Integer;
Var
  i          : Integer;
  lStart     : Integer;
  lEnd       : Integer;
  lIndent    : string;
  lLine      : string;
  lRemove    : Integer;
begin
  Result := 0;

  lStart := aStartLine;
  lEnd   := aEndLine;

  if lStart < 0 then
    lStart := 0;

  if ( lEnd < 0 ) or ( lEnd >= aLines.Count ) then
    lEnd := aLines.Count - 1;

  if aIndentLevel > 0 then
    begin
      lIndent := StringOfChar( ' ', aIndentLevel );

      for i := lStart to lEnd do
        begin
          aLines[ i ] := lIndent + aLines[ i ];
          Inc( Result );
        end;
    end
  else
  if aIndentLevel < 0 then
    begin
      lRemove := Abs( aIndentLevel );

      for i := lStart to lEnd do
        begin
          lLine := aLines[ i ];

          if Length( lLine ) > 0 then
            begin
              Var lSpaces := 0;

              for Var k := 1 to Length( lLine ) do
                begin
                  if lLine[ k ] = ' '
                    then Inc( lSpaces )
                    else break;
                end;

              if lSpaces > 0 then
                begin
                  if lSpaces >= lRemove
                    then aLines[ i ] := Copy( lLine, lRemove + 1, Length( lLine ) )
                    else aLines[ i ] := Copy( lLine, lSpaces + 1, Length( lLine ) );

                  Inc( Result );
                end;
            end;
        end;
    end;
end;

end.

