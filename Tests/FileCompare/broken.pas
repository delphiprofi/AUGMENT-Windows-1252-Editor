Unit TestMaster;

interface

Uses
  System.SysUtils
;

Type
  // Klasse fùr ùberprùfung
  TTestKlasse = class
  private
    fNùhe : Integer;
    fGrùùe : string;
    fùnderung : Boolean;
    fùbersicht : string;
  public
    procedure Prùfen;
    function Grùùe : string;
    // Abschnitt ù mit Aufzùhlung
  end;

implementation

procedure TTestKlasse.Prùfen;
begin
  WriteLn( 'Grùùe: ' + fGrùùe );
  WriteLn( 'ùnderung: straùe' );
  WriteLn( 'ùbersicht ùber ùffnung' );
  // cafù Spezialzeichen
end;

end.
