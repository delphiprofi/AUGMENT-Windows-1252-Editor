Unit StrEditor.Diff;

interface

Uses
  System.SysUtils
, System.Classes
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Diff Helper - zeigt Unterschiede zwischen Original und modifiziertem Text
  /// </summary>
  {$ENDREGION}
  TDiffHelper = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Zeigt Unterschiede zwischen zwei TStringList-Objekten
      /// </summary>
      {$ENDREGION}
      class procedure ShowDiff( const aOriginal : TStringList; const aModified : TStringList; const aFilePath : string );
  end;

implementation

{ TDiffHelper }

class procedure TDiffHelper.ShowDiff( const aOriginal : TStringList; const aModified : TStringList; const aFilePath : string );
Var
  i            : Integer;
  lChangeCount : Integer;
begin
  lChangeCount := 0;

  WriteLn( '--- Diff for: ' + aFilePath );
  WriteLn;

  for i := 0 to aOriginal.Count - 1 do
    begin
      if i >= aModified.Count then
        begin
          Inc( lChangeCount );
          WriteLn( '- Line ' + IntToStr( i + 1 ) + ': ' + aOriginal[ i ] );
        end
      else
      if aOriginal[ i ] <> aModified[ i ] then
        begin
          Inc( lChangeCount );
          WriteLn( '! Line ' + IntToStr( i + 1 ) + ':' );
          WriteLn( '  - ' + aOriginal[ i ] );
          WriteLn( '  + ' + aModified[ i ] );
        end;
    end;

  for i := aOriginal.Count to aModified.Count - 1 do
    begin
      Inc( lChangeCount );
      WriteLn( '+ Line ' + IntToStr( i + 1 ) + ': ' + aModified[ i ] );
    end;

  WriteLn;
  WriteLn( '--- Total changes: ' + IntToStr( lChangeCount ) );
end;

end.

