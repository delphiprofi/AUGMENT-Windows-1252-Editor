unit eAktenSystem.Intf;

interface

Uses
  SysUtils
, Classes
, FileMetadata.Intf
  {$IF Not(Defined(UNICODE)) AND NOT Defined(CONSOLE_TESTRUNNER)}
, RefStream.Intf
, MemHandle.Intf
, ResString
, Dek
, MultiCoreBackup.Intf
, PDF.QuickPDF.Intf
  {$ELSE}
, MemHandle.Intf
, MultiCoreBackup.Intf
  {$IFEND}
;

Const
  cAlloweAkte : Boolean = false;
  SpoolHeaderVersion = 1;//max 3stellig

Type
  TAktenKind      = (eDRI, eDRII, eKBI, eKBII, eSammel, eGeneral, eSonstige, eIgnore); // eAkte.Intf

  {$IF Not(Defined(UNICODE)) AND NOT Defined(CONSOLE_TESTRUNNER)}
  TQESType        = (qesNichts,qesSignieren,qesFragen);
  TDocType        = (DruckDoc{Standard}, PDFDoc, FaxDoc, EMailDoc, EGVPDoc);

  //wird einzeln und nicht als Record in OeffneSpooler gespeichert. Neue Variablen sind entsprechend dort einzuf?gen!!
  tSpoolHeader=record //07.08.25
                SpoolHeaderVersion:Integer;
                Kind:tAktenKind;
                Nr:word;                   //Kein Integer wegen Kompatiilit?t
                Jahr:Byte;
                Unterkategorie:ShortString;//zB fr Sammelakte
                Datenbestand:integer;      //fr verschiedene Datenbest?nde
                AktDRIINr:Integer;         //fr die 10 Register
                DocType:TDocType;          //Ausgabeformat, damit man wei?, wie versendet wurde
                QES:TQESType;
                BenutzerName:ShortString;
                Bezeichnung:Shortstring;   //frher Initial
                Kontaktdaten:ShortString;  //frher Inital und GloabalOeffneSpoolerInitial - enthlt eMail, safe-ID, Fax
               end;
  {$IFEND}

  PNrTypEx = ^TNrTypEx;
  TNrTypEx = packed Record
    Nr           : Integer;
    Jahr         : Integer;
    Datenbestand : Integer;      //fr verschiedene Datenbest?nde
    AktDRIINr    : Integer;      //fr die 10 Register
    Kind         : TAktenKind;

    function  ToString   : String;
    function  ToFilename(Out Dir : WideString) : WideString;
    procedure Clear;
    Constructor Create(aKind : TAktenKind;aNr, aJahr, aDatenbestand, aAktDRIINr : Integer);overload;

    Class operator Equal( a, b: TNrTypEx ): Boolean;

    {$IF Not(Defined(UNICODE)) AND NOT Defined(CONSOLE_TESTRUNNER)}
    function  AsMultiNr : MultiNr_Typ;

    constructor Create(aHeader : tSpoolHeader);overload;//tSpoolHeader enth?t alle n?tigen Infos
    Constructor Create(aIsDRI : boolean;aNrTyp : Nr_Typ);overload;
    Constructor Create(aMultiNr : MultiNr_Typ);overload;
    {$IFEND}
  end;

  // Status-Flags - koennen kombiniert werden
  // Entsprechen den Flags in eAktenHandler.Intf.pas:
  //   cStatusSigniert  = $00000010
  //   cStatusIrrig     = $00000020
  //   cStatusGeloescht = $00000040
  TeAkteStatusFlag  = ( asfSigniert, asfIrrig, asfGeloescht );
  TeAkteStatusFlags = set of TeAkteStatusFlag;

  // Filter fuer LoadData
  TAktenKindSet = set of TAktenKind;

  PTeAkteFilter = ^TeAkteFilter;
  TeAkteFilter = Record
    Jahr      : Integer;           // Pflicht - Jahrgang
    DRNr      : Integer;           // 0 = alle, >0 = nur diese DR-Nummer
    Kinds     : TAktenKindSet;     // Akten-Arten (leer = alle)
    StatusIn  : TeAkteStatusFlags; // Mindestens einer dieser Status (leer = alle)
    StatusOut : TeAkteStatusFlags; // Keiner dieser Status (leer = keine Ausschluesse)

    procedure Clear;
    class function All( aJahr : Integer ) : TeAkteFilter; static;
    class function ByDRNr( aJahr, aDRNr : Integer ) : TeAkteFilter; static;
  end;

  // Forward
  IeAkteEntry = Interface;

  IeAkteEntry = Interface
    ['{A1B2C3D4-E5F6-4789-ABCD-EF0123456789}']
    function GetID         : Integer;
    function GetDRNr       : Integer;
    function GetDRJahr     : Integer;
    function GetNrTypEx    : TNrTypEx;
    function GetAktenKind  : TAktenKind;
    function GetDate       : TDateTime;
    function GetTitle      : WideString;
    function GetFilename   : WideString;
    function GetStatus     : TeAkteStatusFlags;
    function GetChildCount : Integer;
    function GetChild( aIndex : Integer ) : IeAkteEntry;
    function GetPageNumber : WideString;  // Paginierung z.B. "6", "9.1", "9.5.2.1"
    function GetPageCount  : Word;        // Anzahl Seiten im PDF
    function GetParentGUID : TGUID;       // Parent-GUID (NIL = Hauptdokument)
    function GetRevision   : Word;
    function GetGUID       : TGUID;
    function MetaDaten     : IMetaDaten;

    procedure SetStatus( aValue : TeAkteStatusFlags );
    procedure SetPageNumber( const aValue : WideString );
    procedure AddChild( aEntry : IeAkteEntry );

    property ID         : Integer           read GetID;
    property DRNr       : Integer           read GetDRNr;
    property DRJahr     : Integer           read GetDRJahr;
    property NrTypEx    : TNrTypEx          read GetNrTypEx;
    property AktenKind  : TAktenKind        read GetAktenKind;
    property Date       : TDateTime         read GetDate;
    property Title      : WideString        read GetTitle;
    property Filename   : WideString        read GetFilename;
    property Status     : TeAkteStatusFlags read GetStatus     write SetStatus;
    property ChildCount : Integer           read GetChildCount;
    property Child[ aIndex : Integer ] : IeAkteEntry read GetChild;
    property PageNumber : WideString        read GetPageNumber write SetPageNumber;
    property PageCount  : Word              read GetPageCount;
    property ParentGUID : TGUID             read GetParentGUID;
    property Revision   : Word              read GetRevision;
    property GUID       : TGUID             read GetGUID;
  end;

  // Eine Akte (z.B. 0002/24) mit ihren Eintraegen
  IeAkte = Interface
    ['{B2C3D4E5-F6A7-4890-BCDE-F01234567890}']
    function GetAktenID    : String;
    function GetEntryCount : Integer;
    function GetEntry( aIndex : Integer ) : IeAkteEntry;

    procedure AddEntry( aEntry : IeAkteEntry );

    property AktenID    : String  read GetAktenID;
    property EntryCount : Integer read GetEntryCount;
    property Entry[ aIndex : Integer ] : IeAkteEntry read GetEntry;
  end;


  // Ergebnis von ListEAkte - Liste der Eintraege einer Akte mit Datenzugriff
  IeAkteDataList = Interface
    ['{D4E5F6A7-B8C9-4012-DEF0-123456789ABC}']
    function GetCount : Integer;
    function GetEntry( aIndex : Integer ) : IeAkteEntry;
    function GetData( aIndex : Integer; out aData : IMemHandle; const aLZ4 : ILZ4 = NIL ) : Boolean; overload;
    function GetData( aIndex : Integer; const aFilename : WideString; const aLZ4 : ILZ4 ) : Boolean; overload;

    property Count : Integer read GetCount;
    property Entry[ aIndex : Integer ] : IeAkteEntry read GetEntry;
  end;

  // Verfuegbare Jahre fuer eAkte-View
  IeAkteJahrListe = Interface
    ['{9F4A5C21-7B9C-4B32-9D8A-6A4F1E2D3C58}']
    function GetCount : Integer;
    function GetJahr( aIndex : Integer ) : Integer;

    property Count : Integer read GetCount;
    property Jahr[ aIndex : Integer ] : Integer read GetJahr;
  end;

  // Liste der DR-Nummern fuer Lazy Loading (nur Nummern, keine Dokumente)
  IeAkteDRNrListe = Interface
    ['{7E8F9A0B-1C2D-4E3F-5A6B-7C8D9E0F1A2B}']
    function GetCount : Integer;
    function GetDRNr( aIndex : Integer ) : Integer;
    function GetDocCount( aIndex : Integer ) : Integer;  // Anzahl Dokumente pro DR

    property Count : Integer read GetCount;
    property DRNr[ aIndex : Integer ] : Integer read GetDRNr;
    property DocCount[ aIndex : Integer ] : Integer read GetDocCount;
  end;

  // Helper-Klasse fuer AktenKind und Status (D2007 hat keine Record Helper)
  TeAkteHelper = Class
    public
      // AktenKind Funktionen
      Class function AktenKindToIcon( aKind : TAktenKind ) : String;
      Class function AktenKindToName( aKind : TAktenKind ) : String;
      Class function AktenKindToHTML( aKind : TAktenKind ) : String;
      // Status Funktionen
      Class function StatusToHTML( aStatus : TeAkteStatusFlags ) : String;
  end;

implementation

{$IF Not(Defined(UNICODE)) AND NOT Defined(CONSOLE_TESTRUNNER)}
{ TNrTypEx - D2007-only Konstruktor }

constructor TNrTypEx.Create( aHeader : tSpoolHeader );
begin
  Kind         := aHeader.Kind;
  Nr           := aHeader.Nr;
  Jahr         := aHeader.Jahr;
  Datenbestand := aHeader.Datenbestand;
  AktDRIINr    := aHeader.AktDRIINr;
end;

{$IFEND}

constructor TNrTypEx.Create(aKind: TAktenKind; aNr, aJahr, aDatenbestand, aAktDRIINr : Integer);
begin
  Kind         := aKind;
  Nr           := aNr;
  Jahr         := aJahr;
  Datenbestand := aDatenbestand;
  AktDRIINr    := aKtDRIINr;
end;

class operator TNrTypEx.Equal(a, b: TNrTypEx): Boolean;
begin
  Result := CompareMem(@A,@B,Sizeof(TNrTypEx));
end;

procedure TNrTypEx.Clear;
begin
  Kind         := eIgnore;
  Nr           := -1;
  Jahr         := -1;
  Datenbestand := -1;
  AktDRIINr    := -1;
end;

{$IF Not(Defined(UNICODE)) AND NOT Defined(CONSOLE_TESTRUNNER)}

function TNrTypEx.AsMultiNr: MultiNr_Typ;
begin
  Result.DRI     := (Kind = eDRI);
  Result.Nr.DR   := Nr;

  if Jahr > 255
    then Result.Nr.Jahr := Jahr - 1900
    else Result.Nr.Jahr := Jahr;
end;

constructor TNrTypEx.Create(aIsDRI: boolean; aNrTyp: Nr_Typ);
begin
  if aIsDRI
    then Kind := eDRI
    else Kind := eDRII;

  Nr   := aNrTyp.DR;
  Jahr := Integer(aNrTyp.Jahr) + 1900;
end;

constructor TNrTypEx.Create(aMultiNr: MultiNr_Typ);
begin
  Create(aMultiNr.DRI,aMultiNr.Nr);
end;

{$IFEND}

function TNrTypEx.ToString: String;
begin
  case Kind of
    eDRI      : Result := 'DR I ';
    eDRII     : Result := 'DR II ';
    eKBI      : Result := 'KB I ';
    eKBII     : Result := 'KB II ';
    eSammel   : Result := 'Sammelakte ';
    eGeneral  : Result := 'Generalakte ';
    eSonstige : Result := 'Sonstige '
    else Result := '????';
  end; // of case

  if (Nr > 0)
    then Result := Result + Inttostr(Nr)+'/'+Inttostr(Jahr)
    else Result := trim(Result);
end;

function TNrTypEx.ToFilename(Out Dir : WideString): WideString;

  function PadInt(aValue, aWidth: Integer): String;
  begin
    Result := IntToStr(aValue);

    case aWidth - Length(Result) of
      1 : Result := '0'+Result;
      2 : Result := '00'+Result;
      3 : Result := '000'+Result;
      4 : Result := '0000'+Result;
      5 : Result := '00000'+Result;
    end; // of case
  end;

var
  lKind,
  lNr,
  lDaten,
  lDRNr : String;

begin
  if Jahr > 0
    then begin
           if Jahr < 1900 then
             Jahr := Jahr + 1900;

           Dir := Widestring(PadInt(Jahr,4))
         end
    else Dir := '';

  case Kind of
    eDRI      : lKind := 'DR1';
    eDRII     : lKind := 'DR2';
    eKBI      : lKind := 'KB1';
    eKBII     : lKind := 'KB2';
    eSammel   : lKind := 'SAM';
    eGeneral  : lKind := 'GEN';
    eSonstige : lKind := 'SON';
    else lKind := 'IGN';
  end; // of case

  if (Kind in [eDRI, eDRII, eKBI, eKBII]) and (Datenbestand >= 0)
    then lDaten := PadInt(Datenbestand, 2)
    else lDaten := '00';

  if (Kind = eDRII) and (AktDRIINr >= 1)
    then lDRNr := PadInt(AktDRIINr,2)
    else lDRNr := '00';

  if Kind in [eSammel, eGeneral, eSonstige]
    then lNr := '00000'
    else lNr := PadInt(Nr, 5);

  if Dir = ''
    then Result := WideString(Format('%s-%s-%s-%s.eAkte',[lNr,lDRNr,lDaten,lKind]))
    else Result := WideString(Format('%s\%s-%s-%s-%s.eAkte',[Dir,lNr,lDRNr,lDaten,lKind]));
end;

{ TeAkteFilter }

procedure TeAkteFilter.Clear;
begin
  Jahr      := 0;
  DRNr      := 0;
  Kinds     := [];
  StatusIn  := [];
  StatusOut := [];
end;

class function TeAkteFilter.All( aJahr : Integer ) : TeAkteFilter;
begin
  Result.Clear;
  Result.Jahr := aJahr;
end;

class function TeAkteFilter.ByDRNr( aJahr, aDRNr : Integer ) : TeAkteFilter;
begin
  Result.Clear;
  Result.Jahr := aJahr;
  Result.DRNr := aDRNr;
end;

{ TeAkteHelper }

class function TeAkteHelper.AktenKindToIcon( aKind : TAktenKind ) : String;
begin
  // Ressourcen-Name fuer PNG-Icon (img src="res://NAME")
  case aKind of
    eDRI      : Result := 'icon_dri';
    eDRII     : Result := 'icon_drii';
    eKBI      : Result := 'icon_kbi';
    eKBII     : Result := 'icon_kbii';
    eSammel   : Result := 'icon_sammel';
    eGeneral  : Result := 'icon_general';
    eSonstige : Result := 'icon_sonstige';
    eIgnore   : Result := 'icon_ignore';
  else
    Result := 'icon_folder';
  end; // of case
end;

class function TeAkteHelper.AktenKindToName( aKind : TAktenKind ) : String;
begin
  case aKind of
    eDRI      : Result := 'DR I';
    eDRII     : Result := 'DR II';
    eKBI      : Result := 'KB I';
    eKBII     : Result := 'KB II';
    eSammel   : Result := 'Sammelakte';
    eGeneral  : Result := 'General';
    eSonstige : Result := 'Sonstige';
    eIgnore   : Result := 'Ignoriert';
  else
    Result := 'Unbekannt';
  end; // of case
end;

class function TeAkteHelper.AktenKindToHTML( aKind : TAktenKind ) : String;
begin
  // PNG-Icon aus Ressource laden: <img src="res://NAME">
  Result := Format( '<img src="res://%s">', [ AktenKindToIcon( aKind ) ] );
end;

class function TeAkteHelper.StatusToHTML( aStatus : TeAkteStatusFlags ) : String;

  function StatusImg( aActive : Boolean; const aActiveName, aInactiveName : String ) : String;
  begin
    if aActive
      then Result := Format( '<img src="res://%s">', [ aActiveName ] )
      else Result := Format( '<img src="res://%s">', [ aInactiveName ] );
  end;

begin
  // PNG-Icons aus Ressourcen fuer Status (aktiv/inaktiv)
  Result := StatusImg( asfSigniert  in aStatus, 'status_signiert',  'status_signiert_off' ) +
            StatusImg( asfIrrig     in aStatus, 'status_irrig',     'status_irrig_off' ) +
            StatusImg( asfGeloescht in aStatus, 'status_geloescht', 'status_geloescht_off' );
end;

end.
