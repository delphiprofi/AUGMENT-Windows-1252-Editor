Unit TestMaster;

interface

Uses
  System.SysUtils
;

Type
  // Klasse für Überprüfung
  TTestKlasse = class
  private
    fNähe : Integer;
    fGröße : string;
    fÄnderung : Boolean;
    fÜbersicht : string;
  public
    procedure Prüfen;
    function Größe : string;
    // Abschnitt § mit Aufzählung
  end;

implementation

procedure TTestKlasse.Prüfen;
begin
  WriteLn( 'Größe: ' + fGröße );
  WriteLn( 'Änderung: straße' );
  WriteLn( 'Übersicht über Öffnung' );
  // café Spezialzeichen
end;

end.
