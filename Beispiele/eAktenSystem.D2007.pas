unit eAktenSystem.D2007;
{*************************************************************************
 * D2007-only Klassen fuer eAkte-System
 *
 * Diese Unit enthaelt Code der NUR fuer D2007 (nicht fuer DLL07) ist:
 * - TPDF_To_eAkte
 * - IeAktenSystem, TeAktenSystem
 * - InitSpoolHeader(), AlloweAkte(), eAktePossible()
 *
 * Types (tSpoolHeader, TQESType, etc.) bleiben in eAktenSystem.Intf.pas
 * weil TNrTypEx sie verwendet.
 *************************************************************************}

interface

Uses
  SysUtils
, Classes
, RefStream.Intf
, MemHandle.Intf
, ResString
, Dek
, MultiCoreBackup.Intf
, PDF.QuickPDF.Intf
, eAktenSystem.Intf       // TAktenKind, TNrTypEx, TQESType, tSpoolHeader
;

Type
  PPDF_To_eAkte = ^TPDF_To_eAkte;
  TPDF_To_eAkte = Record
    NrTypEx  : TNrTypEx;
    Filename : String;
    QES      : TQESType;

    Constructor Create( Const aNrTypEx : TNrTypEx; Const aFilename : WideString;
                        Const aKategorie : WideString = ''; aQES : TQESType = qesNichts );
  end;

  IeAktenSystem = Interface
    ['{0AE4CC16-A305-4CB3-887A-D49D5F6C5044}']
    procedure New( const aDaten : IMemHandle; const aDateiname : String; aPageCount : Word );
    procedure Delete;
    procedure NoSave;
    procedure IsNeu( aValue : Boolean );
  end;

  TeAktenSystemFactory = function( Const aNrTypEx : TNrTypEx ) : IeAktenSystem;

  TeAktenSystem = class abstract
  private
    class var
      fIsInit : String;
      factory : TeAktenSystemFactory;
  public
    class function  Create( aNrTypEx : TNrTypEx ) : IeAktenSystem;
    class function  Construct : IeAktenSystem;
    class procedure SetFactory( aFactory : TeAktenSystemFactory );
    class procedure Speichern( aWohin : TPDF_To_eAkte ); overload;
    class procedure Speichern( aWohin : PPDF_To_eAkte ); overload;
  end;

procedure InitSpoolHeader( var Header : tSpoolHeader;
                           Kind : TAktenKind = eIgnore;
                           Bezeichnung : ShortString = '';
                           Nr : Integer = 0; Jahr : Byte = 0 );

function AlloweAkte : Boolean;
function eAktePossible : Boolean;

implementation

Uses
  basis
, wbasis
, wdek
, RefStream
, MemHandle
, MulticoreBackup
, eAktenHandler.Intf      // IeAktenHandler
;

function GetVersion : Integer;
var
  e : Integer;
begin
  Val( DEK.Internal, Result, e );
end;

function AlloweAkte : Boolean;
begin
  Result := ( cAlloweAkte and NurBestimmteKunden( [ NurAktiveGV, AuchVollziehungsbeamte ] ) );
end;

function eAktePossible : Boolean;
begin
  Result := NurBestimmteKunden( [ NurAktiveGV, AuchVollziehungsbeamte ] );
end;

{ TeAktenSystem }

class function TeAktenSystem.Construct : IeAktenSystem;
var
  lNrTypEx : TNrTypEx;
begin
  if fIsInit = '' then
    DoException( cFactoryException, [ Self.ClassName ] );

  lNrTypEx.Clear;

  Result := factory( lNrTypEx );
end;

class function TeAktenSystem.Create( aNrTypEx : TNrTypEx ) : IeAktenSystem;
begin
  if fIsInit = '' then
    DoException( cFactoryException, [ Self.ClassName ] );

  Result := factory( aNrTypEx );
end;

class procedure TeAktenSystem.SetFactory( aFactory : TeAktenSystemFactory );
begin
  factory := aFactory;
  fIsInit := '*';
end;

class procedure TeAktenSystem.Speichern( aWohin : PPDF_To_eAkte );
var
  leAktenSystem : IeAktenSystem;
  lPDF          : IQuickPDF;
  lPageCount    : Word;
begin
  // PageCount VOR der Kompression ermitteln
  lPageCount := 0;
  try
    lPDF := TQuickPDF.Create( false );
    if lPDF.LoadFromFile( aWohin^.Filename, '' ) = 1 then
      lPageCount := lPDF.PageCount;
  except
    lPageCount := 0;
  end;

  leAktenSystem := TeAktenSystem.Create( aWohin^.NrTypEx );
  leAktenSystem.New( TMemHandle.FromFile( aWohin^.Filename, btPDF, TLZ4Compressor.Construct ), aWohin^.Filename, lPageCount );
end;

class procedure TeAktenSystem.Speichern( aWohin : TPDF_To_eAkte );
begin
  Speichern( @aWohin );
end;

{ TPDF_To_eAkte }

constructor TPDF_To_eAkte.Create( Const aNrTypEx : TNrTypEx; Const aFilename : WideString;
                                  Const aKategorie : WideString = ''; aQES : TQESType = qesNichts );
begin
  try
    if AlloweAkte then
      begin
        NrTypEx  := aNrTypEx;
        Filename := aFilename;
        QES      := aQES;
        // PDF to eAkte speichern
      end;
  except
  end;
end;

{ InitSpoolHeader }

procedure InitSpoolHeader( var Header : tSpoolHeader;
                           Kind : TAktenKind = eIgnore;
                           Bezeichnung : ShortString = '';
                           Nr : Integer = 0; Jahr : Byte = 0 );
begin
  FillChar( Header, SizeOf( Header ), 0 );
  Header.SpoolHeaderVersion := SpoolHeaderVersion;
  Header.Datenbestand       := Datenbestand;
  Header.AktDRIINr          := AktDRIINr;
  Header.BenutzerName       := GetBenutzerName;
  Header.Kind               := Kind;
  Header.Nr                 := Nr;
  Header.Jahr               := Jahr;
  Header.Bezeichnung        := Bezeichnung;
end;

end.

