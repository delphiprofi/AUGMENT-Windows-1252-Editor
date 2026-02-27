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
    fÜbersicht : string;
  public
    procedure Prüfen;
    // Abschnitt § mit Aufzählung
  end;

implementation

procedure TTestKlasse.Prüfen;
begin
  WriteLn( 'Größe: ' + fGröße );
  WriteLn( 'Übersicht über Öffnung' );
  // café Spezialzeichen
end;

end.
