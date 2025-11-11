Unit StrEditor.Conditional;

interface

Uses
  System.SysUtils
, System.Classes
, System.RegularExpressions
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Conditional Replacement Helper
  /// </summary>
  {$ENDREGION}
  TConditionalHelper = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Führt eine bedingte Ersetzung durch
      /// </summary>
      {$ENDREGION}
      class function ConditionalReplace( const aLines : TStringList; const aConditionPattern : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer ) : Integer;
  end;

implementation

class function TConditionalHelper.ConditionalReplace( const aLines : TStringList; const aConditionPattern : string; const aOldStr : string; const aNewStr : string; const aStartLine : Integer; const aEndLine : Integer ) : Integer;
Var
  i          : Integer;
  lStart     : Integer;
  lEnd       : Integer;
  lRegex     : TRegEx;
  lLine      : string;
begin
  Result := 0;

  lStart := aStartLine;
  lEnd   := aEndLine;

  if lStart <= 0 then
    lStart := 0;

  if ( lEnd <= 0 ) or ( lEnd >= aLines.Count ) then
    lEnd := aLines.Count - 1;

  lRegex := TRegEx.Create( aConditionPattern, [ roIgnoreCase ] );

  for i := lStart to lEnd do
    begin
      lLine := aLines[ i ];

      if lRegex.IsMatch( lLine ) then
        begin
          if Pos( aOldStr, lLine ) > 0 then
            begin
              aLines[ i ] := StringReplace( lLine, aOldStr, aNewStr, [ rfReplaceAll ] );
              Inc( Result );
            end;
        end;
    end;
end;

end.

