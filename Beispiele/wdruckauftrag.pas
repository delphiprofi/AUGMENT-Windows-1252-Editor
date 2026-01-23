//{$H+}//25.07.13
unit wdruckauftrag;
{$REALCOMPATIBILITY ON} // FL 03.01.14 nicht mehr Global
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, WEingabe, Buttons, dek, ComCtrls,XEComp,
  wownlabel, WRadioButtonN, myform, WCheckBox, Grids, SortGrid, AdvOfficePager,
  Mask, WMask, AdvGlowButton, MyStdButton;

CONST Auftragbreaked:boolean=FALSE;
             prttest:boolean=FALSE;

(*geht, aber andere Elemente sind dann mit weiùem Background!
type
  TTabSheet = class(ComCtrls.TTabSheet)
  private
    FColor: TColor;
    procedure SetColor(Value: TColor);
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd);
      message WM_ERASEBKGND;
  public
    constructor Create(aOwner: TComponent); override;
    property Color: TColor read FColor write SetColor;
  end;*)

type
  TFDruckabfrage = class(TmyForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label11: townlabel;
    Label10: townlabel;
    el: TEingabe;
    BitBtn4: TMyStdButton;
    eo: TEingabe;
    Label13: townlabel;
    Label12: townlabel;
    Label4: townlabel;
    art: TEingabe;
    discr: TEingabe;
    BitBtn1: TMyStdButton;
    BitBtn2: TMyStdButton;
    druckername: TEingabe;
    papierweg: TEingabe;
    BitBtn5: TMyStdButton;
    BitBtn6: TMyStdButton;
    BitBtn7: TMyStdButton;
    BitBtn8: TMyStdButton;
    Panel1: TPanel;
    ZWHint: townlabel;
    aus: tmyradiobuttonN;
    einh: tmyradiobuttonN;
    einv: tmyradiobuttonN;
    dup: townlabel;
    PD_: TComboBox;
    einb: TmyRadioButtonN;
    BitBtn9: TMyStdButton;
    BitBtn10: TMyStdButton;
    ImagePanel: TPanel;
    Keins: TOwnLabel;
    keins2: TOwnLabel;
    keins3: TOwnLabel;
    Image1: TImage;
    Panel3: TPanel;
    Zoom: TMyStdButton;
    last: TMyStdButton;
    next: TMyStdButton;
    Bevel2: TBevel;
    OwnLabel1: TOwnLabel;
    SubPreView: TPanel;
    OwnLabel3: TOwnLabel;
    PreView: TMyStdButton;
    Preview2: TMyStdButton;
    BitBtn11: TMyStdButton;
    BitBtn12: TMyStdButton;
    Panel4: TPanel;
    Label2: TOwnLabel;
    Panel5: TPanel;
    Panel2: TPanel;
    UpDown: TUpDown;
    Posi: TOwnLabel;
    SaveDialog1: TSaveDialog;
    Collect: TMyStdButton;
    CollectFax: TMyStdButton;
    CollectMail: TMyStdButton;
    qual: TComboBox;
    QualT: TOwnLabel;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    Panel6: TPanel;
    BitBtn13: TMyStdButton;
    BitBtn14: TMyStdButton;
    BitBtn15: TMyStdButton;
    AddToHist: TmyCheckBox;
    TabSheet4: TTabSheet;
    Liste: TListBox;
    Preview3: TMyStdButton;
    Stati: TOwnLabel;
    Timer1: TTimer;
    BitBtn16: TMyStdButton;
    CollectPDF: TMyStdButton;
    Parteien: TSortGrid;
    Panel7: TPanel;
    CollectShow: TMyStdButton;
    CollectEGVP: TMyStdButton;
    BitBtn18: TMyStdButton;
    Panel10: TPanel;
    Panel11: TPanel;
    OwnLabel9: TOwnLabel;
    TEMP: TEingabe;
    SigL: TOwnLabel;
    Sig: TComboBox;
    EGVPStart: TmyCheckBox;
    CheckBox1: TCheckBox;
    Panel8: TPanel;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    PrinterImg: TImage;
    PrinterLabel: TLabel;
    SigPDF: TmyCheckBox;
    SigH: TMyStdButton;
    Sammelakte: TMyStdButton;
    SammelakteDruck: TMyStdButton;
    Sammelausgabe: TMyStdButton;
    Label3: TLabel;
    BitBtn3: TMyStdButton;
    SchachtWahl: TComboBox;
    OwnLabel2: TOwnLabel;
    ZuAkte: TMyStdButton;
    ZuAkteDruck: TMyStdButton;
    IneAkte: TmyCheckBox;
    NureAkte: TMyStdButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure PDClick(Sender: TObject);
    procedure L6Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure nextClick(Sender: TObject);
    procedure lastClick(Sender: TObject);
    procedure ZoomClick(Sender: TObject);
    procedure PreViewClick(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure CollectClick(Sender: TObject);
    procedure CollectDruckClick(Sender: TObject);//16.02.21
    procedure CollectFaxClick(Sender: TObject);
    procedure CollectMailClick(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PageControl1DrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure BitBtn16Click(Sender: TObject);
    procedure CollectPDFClick(Sender: TObject);
    procedure ParteienClick(Sender: TObject);
    procedure ParteienGetCellFormat(Sender: TObject; Col, Row: Integer;
      State: TGridDrawState; var FormatOptions: TFormatOptions);
    procedure CollectShowClick(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure CollectEGVPClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox1Click(Sender: TObject);//13.08.13
    procedure AltOnClick(Sender: TObject);
    procedure AChange(Sender: TObject);
    procedure SigPDFClick(Sender: TObject);
    procedure SigHClick(Sender: TObject);
    procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SammelakteClick(Sender: TObject);
    procedure SammelakteDruckClick(Sender: TObject);
    procedure SammelausgabeClick(Sender: TObject);
    procedure NureAkteClick(Sender: TObject);
    procedure IneAkteClick(Sender: TObject);//13.08.13
  private
    { Private-Deklarationen }
   Schaechte:Array of Integer;
   DPI,
   AktPage,
   Pages:Integer;
   BMP:Boolean;
//07.08.13    BildList:tstringlist;

    datei : Shortstring;  // FDXE_S
    WINLPTNr,
    lptnr,
    SchachtWahlMerk,
    Schacht : Integer;
       anz,
     direct : byte;
     id : word;
     buff : tmean2;
     wkey : VCLChar;  // FDXE_&C
     CantEnd : Boolean;
    MultiQueued:integer;
    _DRI:Boolean;
    _DR:Nr_Typ;
//29.08.13   Function  AskPDFFilename(var filename, defext : Ansistring;Var OK:Boolean):Boolean;
   Function  GetSPLName(Nr:Integer):ShortString;//16.07.09
   procedure showit;
   function  GetEinzugX : real;
   function  GetEinzugY : real;
   procedure ShowPage;
   procedure SearchImages;
   Function  GetFileName(Page:Integer):ShortString;  // FDXE_S
   procedure previewhint;
   procedure viewasbmp(s:Shortstring);  // FDXE_S
   procedure ENAB;
   procedure DISAB;
   procedure ShowDR(DAT:ShortString);//24.03.05
   procedure Analyse(Fax:Boolean;Nr,PNr:Integer);//15.01.07
   Procedure ChkHist;//15.07.09
   Function  GetQual:Integer;//23.08.12
  public
   procedure chk;
    { Public-Deklarationen }
  end;

procedure DruckAuftragParteienGrid2ClipBoard(Parteien:tSortGrid);

function  LSCheck( ID : Word ) : boolean;
procedure SAVPRTIDX;
function  GETWSC(LPTNR : word) : byte;
procedure MakeDruck( VAR CODE : VCLCHAR; // FDXE_C
                     DIRECT : Byte; {LPTNr, Schach t : Byte;} ID, Serie : Word; Anz : Byte;
                     MultiQueued : Integer{24.03.05} );
procedure NoMorePrint(ID:Word);
procedure CHKPRTIDX;
procedure CHKMargins(LPTNR,NR:Integer);
Function  GetSchrift(NO501:Boolean) : Byte;

//Erzeugt Liste aller Drucker!
Procedure FillPrintScrollBox(S:tScrollBox;
                             IMGSrc:tImage;
                             LabelSrc:tLabel;
                             OnClick:tNotifyEvent
                            );//13.08.13

implementation

{$R *.DFM}

uses wspooler, wdek, wbasis, wEinstellungenDrucker, spooler, printers, basis, winspool, wRegCode,
     DRII_IO,
     druckmen, spez2, wcopyfile, loadstd, wleiste, wAbfrageAbschaltbar, registry, scaleform,
     wemail, wvorschau, richedit, wdruckmenu, wgetstrmodal, weditor, wkopf, utilitie, wtools,
     winsys, speziell, wHilfeDruckerprobleme2, emu, VISTA, eAktenSystem.Intf
     , eAktenSystem.D2007
     , Dialogs.Intf
     , Hinweise,
     wgeneralakte, baselib, menuesys, wpopupmodal
, eAktenSystem.Helper
;

CONST LASTDRUCK : Array[0..MAXD] of Word = (0,0,0,0,0,0,0,0,0,0);
         LASTPD : Integer = 0;
     LastDuplex : Array[0..MAXD] of Word = (0,0,0,0,0,0,0,0,0,0);
       PerEMail : Boolean = FALSE;//19.01.08
   LocalAddHist : Boolean=False;//15.07.09

(*type
  TPrinterInfo2Easy = record
    ServerName: strin g;
    PrinterName: s tring;
    ShareName: stri ng;
    PortName: stri  ng;
    DriverName: str ing;
    Comment: str ing;
    Location: str ing;
    SepFile: strin g;
    PrintProcessor: s tring;
    Datatype: st ring;
    Parameters: s tring;
    Attributes: DWORD;
    Priority: DWORD;
    DefaultPriority: DWORD;
    StartTime: DWORD;
    UntilTime: DWORD;
    Status: DWORD;
    cJobs: DWORD;
    AveragePPM: DWORD;

    DevMode : TDeviceModeA;
// SecurityDescriptor : TSecurityDescriptor;
  end;

procedure GetPrinterInfo2(hPrinter: HDC; var Info: TPrinterInfo2Easy); overload;//20.06.11
var
  hGlobal : THandle;
  PPI2 : ^Printer_Info_2;
  dwNeeded : DWORD;
begin
  GetPrinter(hPrinter, 2, Nil, 0, @dwNeeded);

  hGlobal := GlobalAlloc(GHND, dwNeeded);
  try
    PPI2 := PPrinterInfo2(GlobalLock(hGlobal));

{    PrinterErrCheck( GetPrinter(hPrinter, 2, PPI2, dwNeeded, @dwNeeded));

    Info.ServerName := PPI2^.pServerName;
    Info.PrinterName := PPI2^.pPrinterName;
    Info.ShareName := PPI2^.pShareName;
    Info.PortName := PPI2^.pPortName;
    Info.DriverName := PPI2^.pDriverName;
    Info.Comment := PPI2^.pComment;
    Info.Location := PPI2^.pLocation;
    Info.SepFile := PPI2^.pSepFile;
    Info.PrintProcessor := PPI2^.pPrintProcessor;
    Info.Datatype := PPI2^.pDatatype;
    Info.Parameters := PPI2^.pParameters;
    Info.Attributes := PPI2^.Attributes;
    Info.Priority := PPI2^.Priority;
    Info.DefaultPriority := PPI2^.DefaultPriority;
    Info.StartTime := PPI2^.StartTime;
    Info.UntilTime := PPI2^.UntilTime;
    Info.Status := PPI2^.Status;
    Info.cJobs := PPI2^.cJobs;
    Info.AveragePPM := PPI2^.AveragePPM;

      // folgender Move fùhrt auf manchen Rechnern zu einer Zugriffverletzung
// Move(PPI2^.pDevMode^, Info.DevMode, sizeof(Info.DevMode));
// Info.SecurityDescriptor
}
  finally
    GlobalUnlock(hGlobal);
    GlobalFree(hGlobal);
  end;
end;

procedure GetPrinterInfo2(const pname: strin g; var Info: TPrinterInfo2Easy); overload;//20.06.11
var
  hPrinter : THandle;
  PD : TPrinterDefaults;
begin
 try
  FillChar(PD, SizeOf(PD), 0);
  PD.DesiredAccess := PRINTER_READ; //
  PrinterErrCheck( OpenPrinter(PChar(pname), hPrinter, @PD));
  try
    GetPrinterInfo2(hPrinter, Info);
  finally
    ClosePrinter(hPrinter);
  end;
 except
   on EXC_:Exception do
    ExceptionHintUser0('02127',EXC_.Message,EXC_.ClassName);
 end;
end;*)

//06.09.22:
function AskZurAkte( VorgabeDR_:Nr_Typ;VorgabeDRI:boolean;VorgabeBez:String{13.09.22} ):boolean;
var s, ss:shortstring;
    code:char; // FL 23.12.22
    DR:Nr_Typ;
    DRI:boolean;

 Procedure GetFileName;
 begin
  S := CleanFileName(VorgabeBez); // FL 16.09.22 ist eine Funktion kein Var parameter  //15.09.22   { TODO 2 -oXEFrank -cS_2_Short : XE }
  // FL 16.09.22   s:=VorgabeBez;
  repeat
   repeat
    WinGetStrMODAL(Code,S,40,
                   'Bezeichnung',
                   'Verwenden Sie nicht die Zeichen +:\"/*? fùr die Bezeichnung des Dokuments. '+
                   'Max. 40 Zeichen kùnnen zur Bezeichnung verwendet werden. '+
                   '".pdf" wird automatisch der Bezeichnung angefùgt.',
                   'Bezeichnung',0,'',FALSE);
    s:=trim(s);                                                                                     { TODO 2 -oXEFrank -cS_2_Short : XE }
    if (code<>t_esc) then begin
     if not IsValidFilename(s,false) or (s='') then
      wfehler('Unzulùssiger Dateiname',2);
    end;
   until (Code=t_esc) or IsValidFilename(s,false);
   if (code<>t_esc) and (s>'') then begin
    //Wzu eine neue GetValidFilename? Und vor allem nicht mit Uhrzeit. Und wahrscheinlich anders als alle anderen in der Ablage
    if DRI then
     WSpooler.PDFName:=FotoDir+'I'+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+' '+s+'.pdf'
    else
     WSpooler.PDFName:=FotoDir+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+' '+s+'.pdf';
    //13.05.24 WSpooler.PDFFileName:=WSpooler.PDFName;

    // FL 16.09.22 Eigentlich ganzer block nicht mehr nùtig... Wofùr ist das OK? //Eigenartige Frage, wenn die Datei halt vorhanden ist.
    if fileexists(WSpooler.PDFName) then begin
     if TDialogs.MyMessagedlg('Datei existiert bereits. ùberschreiben?',mtconfirmation,[mbyes,mbno],0)=mryes then begin
      result:=true;
     end
     else begin
      //dann wird noch einmal nach dem Dateinamen gefragt!
     end;
    end
    else
     result:=true;

   end;
  until result or (code=t_esc);//20.09.22 <t_esc
end;

begin //AskZurAkte
 result:=false;
 try
  SS := ''; // FL 16.09.22 war nur belegt wenn keine DR Nummer vorgegeben wurde
  //15.09.22:
  if VorgabeDR_.DR>0 then begin
   if VorgabeDRI then
    s:='I'
   else
    s:='';
   s:=s+DRToString(VorgabeDR_); 
  end
  else begin

   if (Land in DRILaender) and not(PEVMode) then
    SS:='Bei DR I Nummern bitte ein "I" voranstellen. ';
//   else
//    SS:=''; // FL s.o.
   s:='';
  end;

  WinGetStrMODAL(Code,S,8,
                 'Registernummer','Dokument speichern zu '+cdr+' Akte.'#13+
                 ss,'Registernummer',0,
                 '???????zz',FALSE);//27.07.12
  if upstr(Copy(s,1,1)) = 'I' then begin
   DRI := TRUE;
   delete(s,1,1);
  end
  else
   dri := FALSE;
  if code<>T_ESC then begin
   ExtractNrJahr(S,DR.DR,DR.Jahr);
   ValidateDRNr(DR,DRI);
   if DRI then begin
    if DRIExist(DR.DR,DR.Jahr,DRIJahre,True) then begin
     GetFilename;
    end
    else
     wfehler('Akte '+DRToString(dr)+' existiert nicht!',2);
   end
   else
   if DRIIExist(DR.DR,DR.Jahr,DRIIJahre,TRUE) then begin
    GetFilename;
   end
   else
    wfehler('Akte '+DRToString(dr)+' existiert nicht!',2);
  end
  else begin
   TDialogs.MyMessagedlg('Diese DR-Nummer existiert nicht und das Dokument wurde deshalb entfernt.',mtwarning,[mbok],0);
   exit;
  end;

 except
 end;
end;

function GetStatus(PNr:Integer):Shortstring;//20.06.11  // FDXE_S
Var
  PrinterName,
  SDevice,Port : VCLCharBuffer;
  PDMode       : THandle;
  PHandle      : THandle;
  i,
  Wert         : integer;
  pi2          : ^Printer_info_2;
  Const
    Defaults: TPrinterDefaults = (
      pDatatype : nil;
      pDevMode  : nil;
      DesiredAccess : PRINTER_ALL_ACCESS;//PRINTER_ACCESS_USE or PRINTER_ACCESS_ADMINISTER
);

 function PrinterStatusText(Status: Integer): ShortString;  // FDXE_S
 begin
 case Status of
 0: Result := '';//'Waiting';
 JOB_STATUS_PAUSED: Result := 'Angehalten';//'Paused';
 JOB_STATUS_ERROR: Result := 'Fehler';//Error';
 JOB_STATUS_DELETING: Result := '';//'Deleting';
 JOB_STATUS_SPOOLING: Result := '';//'Empfùngt Daten';//'Spooling';
 JOB_STATUS_PRINTING: Result := 'Druckt';//'Printing';
 JOB_STATUS_OFFLINE: Result := 'Offline';
 JOB_STATUS_PAPEROUT: Result := 'Kein Papier';//'Paper Out';
 JOB_STATUS_PRINTED: Result := '';//'Printed';
 JOB_STATUS_DELETED: Result := '';//'Deleted';
 JOB_STATUS_BLOCKED_DEVQ: Result := '';//'Blocked';
 JOB_STATUS_USER_INTERVENTION: Result := 'Benutzereingriff';//'User Intervention';
 JOB_STATUS_RESTART: Result := 'Neustart';//'Restart';
 else
 Result:= 'Status '+IntToStr(Status);                                                               { TODO 2 -oXEFrank -cS_2_Short : XE }
 end;
 end;

begin
 Result:='';
 try
  i:=Printer.Printerindex;

  if Printer.Printerindex <> PNr then //27.07.12
   try
    Printer.Printerindex := PNr;
   except //27.07.12
   end;
  Fillchar(Printername,Sizeof(Printername),0);
  try
   Printer.GetPrinter(PrinterName,SDevice,Port,PDMode);
   try
    Printer.SetPrinter(PrinterName,SDevice,Port,0);
   except
{05.08.20
    on EXC_:Exception do
     ExceptionHintUser0('02128',EXC_.Message,EXC_.ClassName);}
   end;
   if PDMode = 0 then begin
    Printer.Printerindex := {Local}Printer.Printerindex;
    Printer.GetPrinter(PrinterName,SDevice,Port,PDMode);
   end;
   try
    if OpenPrinter(PrinterName, PHandle, @DEFAULTS) then begin
     GetPrinter(Phandle,2,NIL,0,@wert);
     PDmode := GlobalAlloc(GPTR,Wert);
     if PDMode <> 0 then begin
      Pi2 := GlobalLock(PDmode);
      Pi2^.pSecurityDescriptor := NIL;
      GetPrinter(Phandle,2,pi2,wert,@wert);
//      WFehler('Druckerstatus '+inttostr(pi2^.Status),2);
      result:=Printerstatustext(pi2^.status);
      GlobalUnlock(PDmode);
      GlobalFree(PDmode);
      PDmode := 0;
     end;
    end;
    try
     ClosePrinter(PHandle);
    except
     on EXC_:Exception do
      ExceptionHintUser0('02129',EXC_.Message,EXC_.ClassName);
    end;
   except
    ON E:EPrinter do
   end;
  except
     on EXC_:Exception do
      ExceptionHintUser0('02130',EXC_.Message,EXC_.ClassName);
  end;
  Printer.Printerindex:=i; 
 except
     on EXC_:Exception do
      ExceptionHintUser0('02131',EXC_.Message,EXC_.ClassName);
 end;
end;


Function GetSchrift(NO501:Boolean) : Byte;
var i : integer;
begin
 Result:=FF_SWISS;
 try
  if no501 then
   i:=_ERWEITERT.Schrift
  else
   i:=_ERWEITERT.Schrift501;
  Case i of
   0 : Result := FF_DECORATIVE;
   1 : Result := FF_DONTCARE;
   2 : Result := FF_MODERN;
   3 : Result := FF_ROMAN;
   4 : Result := FF_SCRIPT;
   5 : Result := FF_SWISS
  else
   Result := FF_Swiss;
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('02132',EXC_.Message,EXC_.ClassName);
 end;
end;

function LSCheck( ID : Word ) : boolean;
begin
 Result := (ID in GrafPrintIDs); //30.10.03 (ID in [140,142,144,149{,145},126,145]);
//      GrafPrintIDs = [128,131,140,142,144,126,145];
end;

procedure tfdruckabfrage.showit;
var m : tmean2;

 Function Prep(s:Shortstring):Shortstring;//08.05.09  // FDXE_S
 begin
  if (pos('<',s)>0) and (pos('>',s)>0) then
   delete(s,pos('<',s),200);
  Result:=s;
 end;

 procedure SchachtBelege;//27.11.17;

  procedure GetBins;
  (*
  var
   a,
    i : Integer;
    pc : PChar;
    pci : PChar;
    ADeviceMode:tHandle;
    ADevice,ADriver,APort:VCLCharBuffer512;
    pp : Pointer;
    ww : inte ger abs olute pp;   geht nur bis 2 GB
    W : Word;
  begin
   try
    Fillchar(ADevice,Sizeof(ADevice),0);
    Printer.GetPrinter(ADevice,ADriver,APort,ADeviceMode);
    GetMem(pc,24*DeviceCapabilities(ADevice,APort,DC_BINNAMES,nil,nil));
    GetMem(pci,2*DeviceCapabilities(ADevice,APort,DC_BINS,nil,nil));
    A:=DeviceCapabilities(ADevice,APort,DC_BINNAMES,pc,nil);
    DeviceCapabilities(ADevice,APort,DC_BINS,pci,nil);
    Pp:=PCI;
    for i:=1 to A do begin
     SchachtWahl.Items.Add((pc+24*(i-1)));
     Move(pp^,W,2);
     Inc(ww,2);
     SetLength(Schaechte,SchachtWahl.Items.Count);
     Schaechte[high(Schaechte)]:=W;
     if W=Schacht then
      SchachtWahl.ItemIndex:=SchachtWahl.Items.Count-1;
    end;
    FreeMem(pc);
    FreeMem(pci);

   except
    on EXC_:Exception do
     ExceptionHintUser0('02133',EXC_.Message,EXC_.ClassName);
   end;
  end;
  *)
  Const
    CBinNames   = 256; {Maximum number of bin sources}
    CPaperNames = 256; {Maximum number of paper sizes}
  type
    TcchPaperName = VCLCharBuffer63;
    TPaperArray   = array[1..cPaperNames] of TcchPaperName;
    PPaperArray   = ^TPaperArray;
    TcchBinName   = VCLCharBuffer23;
    TBinArray     = array[1..cBinNames] of TcchBinName;
    PBinArray     = ^TBinArray;
    TBins         = array[1..cBinNames] of word;
    PBins         = ^TBins;
  var
    BinArray      : PBinArray;
    Bins          : PBins;
    i             : integer;
    PrinterName,
    SDevice,Port : VCLCharBuffer;
    PDMode       : THandle;
    Wert,Wert2:LongInt;
   begin //GetBins
    try
      Fillchar(Printername,Sizeof(Printername),0);
      Printer.GetPrinter(PrinterName,SDevice,Port,PDMode);
      Printer.SetPrinter(PrinterName,SDevice,Port,0);

      Wert  := DeviceCapabilities(PrinterName,Port,DC_BinNames,NIL ,NIL);
      Wert2 := DeviceCapabilities(PrinterName,Port,DC_Bins    ,NIL ,NIL);
      if Wert > 0 then begin //19.12.05 <> -1 then begin
       GetMem (BinArray, Wert * SizeOf (TcchBinName));
       Fillchar(BinArray^,Wert * SizeOf (TcchBinName),0);
       DeviceCapabilities(PrinterName,Port,DC_BinNames,PChar(BinArray),NIL);
       if Wert2 > 0 then begin //19.12.05 <> -1 then begin
        GetMem (Bins, Wert2 * 2);
        Fillchar(Bins^,Wert2 * 2,0);
        DeviceCapabilities(PrinterName,Port,DC_Bins,PChar(Bins),NIL);
       end
       else
        BINS:=NIL;
       if (Wert<=0) or (BINS=NIL) then begin
       end
       else begin
        SetLength(Schaechte,Wert);
        for i := 1 to Wert do begin
         SchachtWahl.Items.Add(sysutils.StrPAS(BinArray^[i]));
         Schaechte[i-1]:=Bins^[i];
         if Bins^[i]=Schacht then
          SchachtWahl.ItemIndex:=SchachtWahl.Items.Count-1;
        end;
       end;
      end;
    except
     on EXC_:Exception do
      ExceptionHintUser0('02134',EXC_.Message,EXC_.ClassName);
    end;
    if SchachtWahl.Items.Count<=0 then begin
     //SchachtWahl.Items.Add('Keine Papierwegeinfos');
     //AUF KEINEN FALL SchachtWahl.ItemIndex:=0;
     SchachtWahl.Enabled  :=false;
    end;
   end;

 begin //SchachtBelege
  SetLength(Schaechte,0);
  SchachtWahl.Items.Clear;
  SchachtWahl.ItemIndex:=-1;
  try
   GetBins;
  except
   on EXC_:Exception do
    ExceptionHintUser0('02135',EXC_.Message,EXC_.ClassName);
  end;
  SchachtWahlMerk:=SchachtWahl.ItemIndex;
 end;

 begin //showit
  try

   GetMeaning(ID,m);
   loadprinternummer;
   art.text   := (Prep(m.art));
   discr.text := (m.discr);
   LPTNR      := M.Drucker;
   WINLPTnr   := PRINTERNUMMER[m.drucker].nr;
   Schacht    := m.PapierWeg;

   SchachtBelege;//27.11.17

//   PF.Caption:='EGV P Postfach '+EGVPMailBo xID;//27.09.12

   //28.06.12:
   try

    {06.12.13 wird in MakeDurkc gesetzt!

    SigPDF.Checked:=EGVPPDF Override;//03.12.13

    //27.09.12:
    if DirektEGVP or DirektEGVPDruck or DirektSammelnEGVP then
     Sig.ItemIndex:=EGVPOverride
    else

    //06.12.13: passiert nie, weil nur in der Discr des Spool-Docs dieser Hinweis steht:
    if ((Pos('EV-Abnahme',m.Discr)>0) or
        (Pos('EV-/VAK-Abnahme',m.Discr)>0) or//06.12.13
        (Pos('VAK-Abnahme',m.Discr)>0))//29.10.12
        and (ID in FormID) then begin
     Sig.ItemIndex :=GlS ig-1;//06.12.13 Ze nSig-1;
     SigPDF.Enabled:=true;//06.12.13
     SigH.Enabled:=true;//06.12.13
     if (DIP[8 8] and 128=128) then//06.12.13
      SigPDF.Checked:=true;//03.12.13
    end
    else begin
     Sig.ItemIndex:=GlS ig-1;
    end;  }
   except
    Sig.ItemIndex:=0;
   end;

   EGVPStart.Checked:=EGVPDirectStart;//16.07.12

   Case m.Duplex of
    DMdup_horizontal:EINH.Checked:=TRUE;
      DMdup_vertical:EINV.Checked:=TRUE
    else
     AUS.Checked:=TRUE;
   end;

   if dek.DoDuplex then
     EINV.Checked := true;    

   //12.12.06:
   Case QUALITY[m.drucker] of
      Integer(DMRES_High):Qual.ItemIndex:=1;
    Integer(DMRES_Medium):Qual.ItemIndex:=2;
       Integer(DMRES_Low):Qual.ItemIndex:=3;
     Integer(DMRES_DRAFT):Qual.ItemIndex:=4;
    else
     Qual.ItemIndex:=0;
   end;

   if WINLPTNR > Printer.Printers.Count then begin
    WINLPTnr         := 0;
    druckername.text := '';
   end
   else begin
    druckername.text := Printer.Printers[WINLPTNR];                                                 { TODO 2 -oXEFrank -cS_2_Ansi : XE }

    //20.06.11:
    {
    GetPrinterInfo2(Printer.Printers[WINLPTNR], Info);
    Info.Status and JOB_STATUS_OFFLINE}
    Timer1Timer(Self);
    //    Timer1.Enabled:=TRUE;

   end;
   try
    //27.11.17 nunmehr Papierwegewahl    papierweg.text := GETPAPIERWEGFromBinListe(M.Drucker,m.papier weg); //wEinstellungenDrucker
   except
    on EXC_:Exception do
     ExceptionHintUser0('02136',EXC_.Message,EXC_.ClassName);
   end;
   //06.05.03   papi erweg.text := GETPAPIER WEG(M.Drucker,m.papier weg);
   //22.03.17   Caption := 'Ausgabe - Druckauftrag [ID '+IntToStr(ID)+', ';       // fùr Tests 24.3.98

   //17.05.13:
   if ID in PDFID then begin
    eo.Visible       :=FALSE;
    el.visible       :=FALSE;
    label10.visible  :=FALSE;
    label11.visible  :=FALSE;
    label12.visible  :=FALSE;
    label13.visible  :=FALSE;
    ownlabel1.visible:=FALSE;
    PD_.Visible      :=FALSE;
    ownlabel1.visible:=PD_.Visible;
   end;

   if ID in FORMID then begin
    eo.Visible       :=FALSE;
    el.visible       :=FALSE;
    label10.visible  :=FALSE;
    label11.visible  :=FALSE;
    label12.visible  :=FALSE;
    label13.visible  :=FALSE;
    ownlabel1.visible:=FALSE;//03.07.03
    PD_.Visible      :=FALSE;
    ownlabel1.visible:=PD_.Visible;
//    L6.Visible     :=FALSE;
   end;
   if (ID in RichID) or (ID in DirectID) or (ID in GrafPrintIDs) then begin
    eo.Visible     :=FALSE;
    el.visible     :=FALSE;
    label10.visible:=FALSE;
    label11.visible:=FALSE;
    label12.visible:=FALSE;
    label13.visible:=FALSE;
   end
   else begin

    //02.06.06:
    if ID in CanvasPrintID then begin
     PD_.Visible:=FALSE;
     ownlabel1.visible:=PD_.Visible;
    end;

    eo.text := StrEinzug(m.yoffs);
    el.text := StrEinzug(m.xoffs);
   end;
   {06.10.22
    if (PD_.Visible) and (PD_.Enabled) and (PD_.ItemIndex in [1..3]) and not(AllisCan vasTemp) then begin
    //22.03.17    caption      :=caption+'Direktdruck im RAW-Modus]';
    aus.enabled  :=FALSE;
    einh.enabled :=FALSE;
    einv.enabled :=FALSE;
    Qual.enabled :=FALSE;
    QualT.enabled:=FALSE;
   end
   else} begin
    if {(PD_.Visible) and (PD_.Enabled) and}
       //06.10.22 (not(All isCanvasTemp) and (PD_.ItemIndex in [4])) or
       ((PD_.ItemIndex in [1]))
        then
    else
     ;
   end;

   if (ID in RichID) and (DIP[31] and 1=1) then begin
    Druckername.Text:='Druck ùber gewùhlten Standarddrucker!';
    Papierweg.Visible:=False;
    SchachtWahl.Enabled:=FALSE;//27.11.17
   end;

  except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 1');//22.08.12
  end;
 end;

function tfdruckabfrage.GetEinzugX : real;
 var s : String; // FL 23.12.22
     R : Real;
  begin
   S := el.text;
   if pos(',',s) > 0 then
    s[pos(',',s)] := '.';
   Val(S,R,Error);
   getEinzugX := R;
  end;

function tfdruckabfrage.GetEinzugY : real;
 var s : Shortstring;  // FDXE_S
     R : Real;
  begin
   S := eo.text;
   if pos(',',s) > 0 then
    s[pos(',',s)] := '.';
   Val(S,R,Error);
   getEinzugY := R;
  end;

procedure tfdruckabfrage.chk;
begin
 try
  //17.05.13:
  if ID in PDFID then begin
   PD_.itemIndex    :=0;
   ownlabel1.visible:=FALSE;//03.07.03
   PD_.Enabled      :=FALSE;
 //  L6.Visible:=FALSE;
  end
  else

  if ID in FormID then begin
   PD_.itemIndex    :=0;
   ownlabel1.visible:=FALSE;//03.07.03
   PD_.Enabled      :=FALSE;
 //  L6.Visible:=FALSE;
  end
  else
 
  //02.06.06:
  if ID in CanvasPrintID then begin
   PD_.Visible:=FALSE;
   ownlabel1.visible:=PD_.Visible;
  end
  else

  if Direct in [1..2,9] then begin // richprint etc.
   if idisprintdirect[lptnr]=4 then
    //29.09.06:
    PD_.itemIndex:=1
   else
    PD_.itemIndex:=0;
   ownlabel1.visible:=FALSE;//03.07.03
   PD_.Enabled  :=FALSE;
  end
  else
  {06.10.22
  if (LPTNR in [1..MAXD]) and (idIsprintDirect[lptnr] in [1..3]) and
     not(AllIsCanvasTemp) and
     not(ID in NoDirectIds) then begin

   //29.09.06:
   if AllisCanvasTemp then
    if idIsprintDirect[lptnr]=4 then
     PD_.itemIndex   :=1
    else
     PD_.itemIndex   :=0
   else

    PD_.itemIndex   :=idIsprintDirect[lptnr];
   PD_.Enabled     :=TRUE;
  end
  else}
  if  (LPTNR in [1..MAXD]) and not(ID in NoDirectIds) then begin

   //29.09.06:
   if idIsprintDirect[lptnr]=4 then
    PD_.itemIndex:=1
   else
    PD_.itemIndex:=0;
   PD_.Enabled  :=TRUE;
 //19.11.99  L6.Visible:=TRUE;
  end
  else begin
   PD_.itemIndex    :=0;
   ownlabel1.visible:=FALSE;//03.07.03
   PD_.Enabled      :=FALSE;
 //  L6.Visible:=FALSE;
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 2');//22.08.12
 end;
end;

procedure MakeDruck;
var           F : TFDruckAbfrage;
          datei : Shortstring;  // FDXE_S
 LPTNr, Schacht : Integer;
           buff : tmean2;
            DRI : Boolean;
             DR : Nr_Typ;

             //11.08.13:
     _F,
     SAFE,
     DeMail,
     _ESubj,
     eMailPw,
      _E : ansistring;//ShortString;

 Procedure DOHistorie;//15.07.09
 VAR DRI : Boolean;
      DR : Nr_Typ;
     _F,
     SAFE,
     DeMail,
     _ESubj,
     eMailPw,
      _E : Ansistring;//ShortString;
 begin
  if LocalAddHist then begin
   //18.07.13:
   Get_Fax_eMail(SPLd+Datei,_F,_E,SAFE,_ESubj,DeMail,eMailPw,DRI,DR);
   Get_SPL_DiscrOnly(SPLd+Datei,_E);

   AddSpoolToHist(DR,DRI,_E,SPLd+Datei);//15.07.09
  end;
 end;

 procedure ShowDocs;//16.07.09
 var     ff:textFile;
        //_F,
        //_E,
    //_ESubj,
   //eMailPw,
      //Safe,
    //DeMail,
         ss:ansistring;//shortstring;
        //DRI:Boolean;
       //DRNr:Nr_Typ;
     Header:tSpoolHeader;
 begin
  With f do begin
   Liste.items.beginupdate;//07.09.12
   Liste.Items.Clear;
   try
    ss:='';
    ioresult;
    assignfile(ff,temppath+'QUEUE'+strword(GetWorkStationNr,1)+'.TMP');
    Reset(Ff);
    if IOResult=0 then begin
     While not(eof(fF)) do begin
      system.readln(fF,ss);
      if ioresult>0 then
       SS:='';

      //08.08.25:
      //Man KùNNTE auch die Queue umbauen, um mehr Infos zu zeigen, ist aber nicht essentiell!
      GetSpoolInitStr(SPLd+ss,Header);
      ss:=GetNrStrfromSpoolHeader(Header);
      if ss>'' then
       ss:=ss+': ';
      ss:=ss+Header.Bezeichnung;
      {Kill_Fax_eMail(Header.Kontaktdaten,_F,_E,SAFE,_ESubj,DeMail,eMailPw);
      _E:=Header.Bezeichnung;}

      (*11.08.25
      Get_Fax_eMail(SPLd+ss,_F,_E,SAFE,_ESubj,DeMail,eMailPw,DRI,DRNr);
      Get_SPL_Discr(SPLd+ss,_E);//_E ist jetzt die Beschreibung

      //22.07.09:
      if DRNr.DR=0 then
       ss:=''
      else begin

       if DRI then
        ss:='I '+DRToString(DRNr)
       else begin
        ss:=DRToString(DRNr);
        While length(ss)<8 do
         ss:='0'+ss;
       end;
      end;
      if Pos('[',_E)>0 then
       _E:=Copy(_E,1,Pos('[',_E)-1);

      //20.06.11:
      if trim(ss)='' then
       ss:=_E
      else

       ss:=ss+': '+_E;*)

      //26.10.11:
      if Liste.Items.Count<=0 then
       Liste.Items.Add('Liste der Druckdokumente:');

      Liste.Items.Add((ss){22.07.09});
     end;
     CloseFile(Ff);
     ioresult;
    end;
   except
          on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 3');//22.08.12
   end;
   Liste.items.endupdate;//07.09.12
  end;
 end;

var discr_:ansistring;//shortstring;
begin //MakeDruck;
 PerEMail:=FALSE;
 try
  DRI:=FALSE;
  DR.DR:=0;
  Auftragbreaked:=FALSE;
  try
   CHKPRTIDX;
   GetMeaning(id,buff);
  except
   on EXC_:Exception do
    ExceptionHintUser0('02137',EXC_.Message,EXC_.ClassName);
  end;
 // lptnr := PRINTERNUMMER[buff.drucker].nr;
  schacht := buff.papierweg;
  LPTNr   := buff.Drucker;

  //17.05.13:
  if direct = PDFSplNr then
   datei := PDFSplChar
  else

  if direct = 33 then
   datei := 'F'
  else
  if direct = 9 then
   datei := 'G'
  else
  if direct = 2 then
   datei := 'R'
  else
  if direct = 1 then
   datei := 'P'
  else
   datei := 'S'{chr(LPTNr+48)+Chr(Schach t+48)};
  datei := datei+GetSPLAnzCoded(Anz)+StrWordNull(Serie,4)+'.'+StrWordNull(ID,3);
  if (Buff.Drucker = 0) or (LASTDRUCK[BUFF.DRUCKER] <> ID) then begin
   LocalAddHist:=FALSE;
   //MerkLetzteAktion('wDrAu');//24.04.14
   f := tfdruckabfrage.create(application);
   try
    //f.left := getleft(f.width);
    //f.top := gettop(f.height);
    try
     if f.TabSheet1<>nil then //15.06.21 da unerklùrliche Exception, kein Onter vorhanden
      f.PageControl1.ActivePage:=f.TabSheet1;//28.12.11
     if f.BitBtn1.canfocus and (F.BitBTn1<>nil) then//15.06.21
      f.ActiveControl:=F.BitBtn1;//15.12.11
    except
     on EXC_:Exception do
       ExceptionHintUser0('02138',EXC_.Message,EXC_.ClassName);
     //06.07.21 15:48:03, 30.0 i3310, #2961, Nr. 1999: Zugriffsverletzung bei Adresse 00405A4E in Modul 'GVSERVICE.EXE'. Lesen von Adresse 00000000 - EAccessViolation - 02138, Laufzeit 4:53
     //15.06.21 15:22:28, 30.0 i3307, #3672, Nr. 1999: Zugriffsverletzung bei Adresse 004B2FB4 in Modul 'GVSERVICE.EXE'. Schreiben von Adresse 000002C0 - EAccessViolation - 02138, Laufzeit 2:04
    end;

    f.IneAkte.Visible:=eAktePossible;//08.08.25
    f.IneAkte.Enabled:=AlloweAkte;//08.08.25
    f.IneAkte.Checked := TeAktenHelper.IsSaveToEAkte;
    f.NureAkte.Enabled:=AlloweAkte;//18.08.25

    f.datei  :=DATEI;
    F.CANTEND:=FALSE;
    F._DRI   :=FALSE;
    F._DR.DR :=0;

    //06.09.22:
    if not(P367Mode) and not(PEVMode) then begin
     f.ZuAkte.visible     :=AllowDruckZuAkte;
     f.ZuAkteDruck.visible:=AllowDruckZuAkte;
    end;

    //05.06.12:
    f.Art.Font.Style  :=[fsbold];
    f.Discr.Font.Style:=[fsbold];

    if AllowBroschure then
     F.einb.visible:=TRUE;

    //29.09.06:
    F.PD_.Items.Clear;
    F.PD_.Items.Add('ùber Treiber (Druckbild je nach Druckertreiber)');
    F.PD_.Items.Add('ùber Treiber (zuverlùssig, dafùr langsamer - Bitmap)');

    //13.08.13:
    FillPrintScrollBox(F.ScrollBox1,F.PrinterIMG,F.PrinterLabel,F.AltOnClick);

    //28.06.12:
    f.Sig.Visible        :=not(P367Mode) and not(PEVMode);
    f.SigL.Visible       :=f.Sig.Visible;

    F.BitBtn6.Visible    :=not(P367Mode) and not(PEVMode);//02.06.21

    //18.10.06:
    F.Collect.Visible    :=not(P367Mode) and not(PEVMode);// and (CheckVersion('W18.0',true) or AllowCollect);
    //22.03.17:
    F.Sammelausgabe.Visible:=not(P367Mode) and not(PEVMode);
    F.Sammelausgabe.Enabled:=CollectExist(GetWorkStationNr);

    if P367Mode then begin
     F.BitBtn16.Visible:=FALSE;//04.04.12
     F.BitBtn18.Visible:=FALSE;//07.06.12

     f.SammelAkte.visible:=FALSE;
     f.SammelAkteDruck.visible:=FALSE;

     //08.08.08:
     F.Panel4.Visible:=FALSE;
     F.BitBtn2.Visible:=FALSE;

     F.Bitbtn7.visible :=FALSE;
     F.Bitbtn8.visible :=FALSE;
     F.Bitbtn6.visible :=FALSE;
     F.Bitbtn9.visible :=FALSE;
     F.Bitbtn10.visible:=FALSE;

     F.ZuAkte.visible:=false;
     F.ZuAkteDruck.visible:=false;
    end;
    if GlobalZwangsDruck then begin
     F.BitBtn16.Enabled :=FALSE;//04.04.12
     F.BitBtn18.Enabled :=FALSE;//07.06.12
     F.BitBtn2.Enabled :=FALSE;
     F.BitBtn5.Enabled :=FALSE;
     F.BitBtn6.Enabled :=FALSE;
     F.BitBtn7.Enabled :=FALSE;
     F.BitBtn8.Enabled :=FALSE;
     {01.02.05 PDF und alternative Drucker zulassen
     F.BitB tn9.Enabled :=FALSE;
     F.BitBt n10.Enabled:=FALSE;
     F.BitBtn 11.Enabled:=FALSE;//01.02.05}
     F.ZWHint.Visible  :=TRUE;

     //14.11.06:
     F.Collect.Enabled     :=FALSE;
     F.CollectFax.Enabled  :=FALSE;
     F.CollectMail.Enabled :=FALSE;
     F.CollectPDF.Enabled  :=FALSE;//20.06.12
     F.CollectEGVP.Enabled :=FALSE;//20.06.12
     F.CollectShow.Enabled :=FALSE;//06.06.12
    end;
    {
    if AllowSMS then begin
     F.BitB tn9.Visible :=TRUE;
     F.BitBt n10.Visible:=TRUE;
     F.BitBtn 9.Enabled :=TRUE;
     F.BitBtn 10.Enabled:=TRUE;
    end;}

    //25.01.05:
    F.BitBtn9.Visible :=true;
    F.BitBtn10.Visible:=f.BitBtn9.Visible;
    F.BitBtn9.Enabled :=true;
    F.BitBtn10.Enabled:=f.BitBtn10.Enabled;

    //06.09.22:
    if AllowDruckZuAkte then begin
     F.ZuAkte.visible     :=f.BitBtn9.Visible;
     F.ZuAkteDruck.visible:=f.BitBtn10.Visible;
     F.ZuAkte.Enabled     :=f.BitBtn9.Enabled;
     F.ZuAkteDruck.Enabled:=f.BitBtn10.Enabled;
    end;

    try
     F.MultiQueued    :=MultiQueued;
     F.UpDown.Max     :=MultiQueued;
     F.UpDown.Visible :=(MultiQueued>0);
     F.Posi.Visible   :=(MultiQueued>0);
     F.UpDown.Position:=1;

     F.Panel7.Visible:=(MultiQueued>1);//28.06.12

     //12.08.09:
     LastPD     := 0;
     f.id       := id;
     f.lptnr    := buff.drucker;
     f.anz      := ANZ;
     F.WinLPTNR := PRINTERNUMMER[buff.drucker].nr;
     f.buff     := buff;
     f.direct   := direct;

     F.ShowDR(F.Datei);//24.03.05

     LastPD     := 0;
     f.id       := id;
     f.lptnr    := buff.drucker;
     f.anz      := ANZ;
     F.WinLPTNR := PRINTERNUMMER[buff.drucker].nr;
     f.buff     := buff;
     f.direct   := direct;
     F.PageControl1.ActivePage:=F.TabSheet1;
     if direct = 1 then
      f.bitbtn3.Enabled := FALSE;
    except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 4');//22.08.12
    end;
    try
     F.CHK;
     F.SearchImages;
    except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 5');//22.08.12
    end;

    //06.12.13:
    try
     Get_SPL_Discr(SPLd+Datei,DISCR_);
     if ((Pos('EV-Abnahme',Discr_)>0) or
         (Pos('EV-/VAK-Abnahme',Discr_)>0) or//06.12.13
         (Pos('VAK-Abnahme',Discr_)>0))//29.10.12
         and (ID in FormID) then begin
      f.Sig.ItemIndex :=GlSig-1;
      {16.02.23
      f.SigPDF.visible:=true;
      f.SigH.Enabled:=true;}
{ // BW 27.02.23 Ticket #1495
      if (DIP[88] and 128=128) then//06.12.13
       f.SigPDF.Checked:=true;//03.12.13
}
     end
     //war zuvor in Showit:
     else begin
 // BW 27.02.23 Ticket #1495      f.SigPDF.Checked:=EGVPPDFOverride;//03.12.13
      // FL 03.03.23
      if DISCR_ <> '' then
       F.Caption := F.Caption + ' ('+trim(DISCR_)+')';

      //27.09.12:
      if DirektEGVP or DirektEGVPDruck or DirektSammelnEGVP then
       f.Sig.ItemIndex:=EGVPOverride
     end;
    except
     on EXC_:Exception do
      ExceptionHintUser0('02139',EXC_.Message,EXC_.ClassName);
    end;

    try
     ShowDocs;//16.07.09
    except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag ShowDocs');//08.05.13
    end;

    f.wkey := #0;
    try
     f.SHOWit;
    except
     on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag ShowIt');//08.05.13
    end;

    if FALSE then begin //26.05.03 Direct=33 then begin//23.05.03
     F.BitBtn7.Enabled :=FALSE;
     F.BitBtn8.Enabled :=FALSE;
     F.BitBtn9.Enabled :=FALSE;
     F.BitBtn10.Enabled:=FALSE;
     F.ZuAkte.Enabled :=FALSE;
     F.ZuAkteDruck.Enabled:=FALSE;
    end;
    try
    if (f.pd_.itemindex<=0) and not(ID in RichID) and not(ID in DirectID) and
       not(ID in GrafPrintIDs) then
     ChkMargins(F.LPTNR,F.WinLPTNR);
    except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 6');//22.08.12
    end;

    {Problem, fùr mehrere Druckwerke und mehrere markierte Druckwerke mùsste die Druckanzahl ùberschrieben werden.
     Druckwerk A = 1x, B = 2x, dann erscheint A.Text=2
     Tja. Mùsste also Schaltflùche "fùr alle Dok Druckanzahl ùndern" sein.!

    //16.08.13:
    if Anz>99 then begin
     F.A.EditMask :='999;1;_';
     F.A.MaxLength:=3;
     F.Label2.Caption:='Max. 999 Druckexemplare sind mùglich.';
    end;
    F.A.Text:=inttostr(Anz);}
    //29.07.09:
    if F.AddtoHist.Visible and
       F.AddtoHist.Enabled and
       (DIP[72] and 32=32) then
     F.AddtoHist.Checked:=TRUE;

    {unnùtig! //26.12.21:
    if eZUWanted then begin
     DirektePDF:=FALSE;
     f.BitBtn1Click(Application);
     //NEIN, es muss ùber den Spooler laufen!! f.BitBtn9Click(Application);
    end
    else}

    //24.02.21:
    if DirekteSammelnUndDrucken then begin
     DirekteSammelnUndDrucken:=false;
     F.BitBtn15Click(Application);//Sammelt + Druckt den akt. Druckauftrag
     //25.02.21 F.CollectDruckClick(Application);//16.02.21
    end
    else

    //23.04.08:
    if DirekteSammelnD then begin

     //25.02.21:
     DirekteSammelnD:=FALSE;
     //F.BitBtn15Click(Application);//Sammelt + Druckt den akt. Druckauftrag
     F.CollectDruckClick(Application);//16.02.21, Legt in Sammelablage und druckt diese

     {25.02.21
     DirekteSammelnD:=FALSE;
     //16.02.21 F.BitBtn15Click(Application);
     F.CollectDruckClick(Application);//16.02.21
     }
    end
    else

    //18.10.06:
    if DirekteSammeln then begin
     DirekteSammeln:=FALSE;
     F.CollectClick(Application);
    end
    else
    if DirekteSammelnF then begin
     DirekteSammelnF:=FALSE;
     F.CollectFaxClick(Application);
    end
    else
    if DirekteSammelnE then begin
     DirekteSammelnE:=FALSE;
     F.CollectMailClick(Application);
    end
    else
    //05.06.12:
    if DirekteSammelnP then begin
     DirekteSammelnP:=FALSE;
     F.CollectPDFClick(Application);
    end
    else

    //13.07.09:
    if DirektDruckWahl then begin //vor DirektDruck abfragen!!!!
     DirektDruckWahl:=FALSE;
     F.BitBtn11Click(Application);
    end
    else

    //21.03.17:
    if DirektSammelAkte then begin
     DirektSammelAkte:=false;
     F.SammelAkteClick(Application);
    end
    else
    if DirektSammelAkteDruck then begin
     DirektSammelAkteDruck:=false;
     F.SammelAkteDruckClick(Application);
    end
    else

    //06.09.22:
    if DirektDruckZurAkte then begin
     DirektDruckZurAkte:=false;
     F.BitBtn9Click(F.ZuAkte);
    end
    else
    if DirektDruckZurAkteDruck then begin
     DirektDruckZurAkteDruck:=false;
     F.BitBtn10Click(F.ZuAkteDruck);
    end
    else

    //18.08.25:
    if DirekteAkte then begin
     DirekteAkte:=FALSE;
     F.NureAkteClick(Application);
    end
    else

    //28.06.12:
    if DirektEGVP then begin
     DirektEGVP:=FALSE;
     F.BitBtn16Click(Application);
    end
    else
    if DirektEGVPDruck then begin
     DirektEGVPDruck:=FALSE;
     F.BitBtn18Click(Application);
    end
    else
    if DirektSammelnEGVP then begin
     DirektSammelnEGVP:=FALSE;
     F.CollectEGVPClick(Application);
    end
    else

    if DirektFax then begin
     DirektFax:=FALSE;
     F.BitBtn5Click(Application);
    end
    else
    if DirektFaxDruck then begin
     DirektFaxDruck:=FALSE;
     F.BitBtn6Click(Application)
    end
    else
    if DirekteMail then begin
     DirekteMail:=FALSE;
     F.BitBtn7Click(Application)
    end
    else
    if DirekteMailDruck then begin
     DirekteMailDruck:=FALSE;
     F.BitBtn8Click(Application)
    end
    else
    if DirektePDF then begin
 //25.01.05   if DirekteSMS then begin
     DirektePDF:=FALSE;
     F.BitBtn9Click(Application)
    end
    else
    if DirektePDFDruck then begin
     DirektePDFDruck:=FALSE;
     F.BitBtn10Click(Application)
    end
    else

{08.01.13    //26.07.12:
    if DirektDruck then begin
     DirektDruck:=FALSE;
     F.BitBtn1Click(Application);
    end
    else }

    //04.09.22: wieso war das rausgenommen?!
    if DirektDruck
       and (DIP[108] and 2=0) //05.09.22 weil sonst keine Abfrage nach Papierweg, Duplexer kommt
       then begin
     DirektDruck:=FALSE;
     F.BitBtn1Click(Application);
    end
    else

    if DIP[43] and 2=2 then
     F.BitBtn1Click(Application)
    else begin

     MyShowModal(f);//14.08.14 F.s howmodal;
    end;

    {11.08.13 wird nicht genutzt!
    DRI:=F._DRI;
    DR :=F._DR;}    

    CODE := F.WKEY;
  //  F.CLOSE;
   finally
    F.Free;
   end;
   application.processmessages;

   //19.01.08: Pseudo-Modal, um Eingaben abzuwarten!
   if PerEMail and BriefWaitActive then begin
    try
     While lastemailp<>NIL do begin
      Application.Processmessages;
      _Sleep(10);
     end;
    except
     on EXC_:Exception do
      ExceptionHintUser0('02140',EXC_.Message,EXC_.ClassName);
    end;
   end;

   // MODAL !!!!
   {  lptnr := buff.drucker;
   schach t := buff.papier weg;
   f.lptnr := LPTNr;
   f.Sch acht := schac ht;
   f.serie := serie;
   f.anz := anz;}
  end
  else begin

   Get_Fax_eMail(SPLd+Datei,_F,_E,SAFE,_ESubj,DeMail,eMailPw,DRI,DR);//11.08.13

   try
    DoHistorie;//15.07.09
   except
    on EXC_:Exception do
     ExceptionHintUser0('02141',EXC_.Message,EXC_.ClassName);
   end; 

   Application.ProcessMessages;//30.6.99 wegen Ressourcenproblem
   try
(*    if LASTPD in [1..3] then
     printDirectProc(SPLd+Datei,TRUE,ID,buff.Drucker,LASTPD,FALSE,ANZ,SCHACH T,PRINTERNUMMER[buff.drucker].nr{WINLPTNR})
    else*) begin
     WinSpoolCreate(ANZ,LPTNR{+1},SPLd+Datei,PRINTERNUMMER[buff.drucker].nr,
                    schacht,direct,LSCheck(ID),GetSchrift(TRUE),Buff.Xoffs,Buff.YOffs,FALSE,
                    buff.oncevorschub,buff.laenge,buff.zoll,ID,LastDuplex[LPTNr],
                    {06.10.22 (not(AllisCanvasTemp) and (LastPD=4)) or} ((LastPD=1)),


                    False,buff.drucker,FALSE,FALSE,FALSE,DR,DRI,QUALITY[buff.drucker],FALSE);
    end;
   except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 7');//22.08.12
   end;
   Application.ProcessMessages;//30.6.99 wegen Ressourcenproblem
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 8');//22.08.12
 end;
end;

procedure TFDruckabfrage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
//action:=cafree;
end;

procedure TFDruckabfrage.BitBtn2Click(Sender: TObject);
begin
 //*** VERBLEIB IN DRUCKABLAGE
 if GlobalZwangsDruck then
  TDialogs.MyMessageDlgOK('~Zwangsdruck!'#13'Abbruch nicht erlaubt!',mterror,[mbok],0)
 else begin
  ChkHist;//15.07.09
  Auftragbreaked:=TRUE;
  wkey:=t_esc;
  CLOSE;//15.3.99
 end;
 //!!16.7.98 reconstructSpool(spld,true);
 //CLOSE;
end;

Function TFDruckabfrage.GetQual:Integer;//23.08.12
var Q:integer;
begin
   Case QUAL.ItemIndex of
    1:Q:=Integer(DMRES_HIGH);
    2:Q:=Integer(DMRES_MEDIUM);
    3:Q:=Integer(DMRES_LOW);
    4:Q:=Integer(DMRES_DRAFT)
    else
     Q:=0;
   end;
   Result:=Q;
end;

procedure TFDruckabfrage.BitBtn1Click(Sender: TObject);
var      //07.08.13 i : byte;
        Q,
    Duplex : Integer;
begin
 try
  DISAB;
  ChkHist;//15.07.09
  CHK; // falls buttonclick vor show!!!
  WFehler('Der Druckauftrag wird ùbergeben...',3);//24.06.03
  LastDruck[LPTNr] := ID;
  if (buff.Drucker <> LPTNr) or (buff.PapierWeg <> Schacht) then begin
   Buff.PapierWeg := Schacht;
   Buff.Drucker   := LPTNr;
   SetMeaning(id,buff);
  end;
  begin
   Panel1.Align  :=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;

   //29.09.06:
   if PD_.ItemIndex=1 then
    LASTPD:=4
   else
    LASTPD:=0;

   if EINH.Checked then
    Duplex:=DMdup_horizontal
   else
   if EINV.Checked then
    Duplex:=DMdup_vertical
   else
    Duplex:=DMdup_simplex;
   LastDuplex[LPTNr] := Duplex;

   Q:=GetQual;//23.08.12

   //27.11.17:
   if SchachtWahl.ItemIndex<>SchachtWahlMerk then begin
    try
     Schacht:=Schaechte[SchachtWahl.ItemIndex];
    except
     on EXC_:Exception do
      ExceptionHintUser0('02142',EXC_.Message,EXC_.ClassName);
    end;
   end;                            

   WinSpoolCreate(ANZ,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,direct,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,Duplex,
                  {(not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or} ((PD_.ItemIndex=1)),
                  False,buff.drucker,FALSE,FALSE,false,_DR,_DRI,Q,FALSE);
 end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 9');//22.08.12
 end;
 if not(CANTEND) then begin
  wkey:=t_end;
  CLOSE;//15.3.99
 end
 else
  ENAB;
end;

procedure TFDruckabfrage.BitBtn3Click(Sender: TObject);
//07.08.13 var hh : integer;
begin
 WinDruckZiele(ID);//27.11.17 wEinstellungenDrucker ist jetzt auch modal!
 TDialogs.MyMessagedlg('Jede ùnderung in den Einstellungen hat erst beim nùchsten Druckvorgang eine Wirkung.',mtinformation,[mbok],0);
{nù, wegen MODAL!!!
 Enabled:=FALSE;
// WindowState := wsminimized;
 hh:=height;
 Height:=0;
 WinDruckZiele(ID);
 While wEinstellungenDrucker.F <> NIL do
  application.processmessages;
 Enabled:=TRUE;
// WindowState := wsnormal;
 height:=hh;
 SHOW;}
end;

procedure TFDruckabfrage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var pp:integer;  
begin
 try
  if ChkENDKeys(Key,Shift) then
   bitbtn1click(sender)
  else
  if key=vk_next then
   PageControl1.SelectNextPage(TRUE)
  else
  if key=vk_prior then begin
   try
    PP:=PageControl1.ActivePage.PageIndex;
    repeat
     if PP>0 then
      PP:=PP-1
     else
      PP:=PageControl1.PageCount-1;
    until PageControl1.Pages[PP].TabVisible;
    PageControl1.ActivePage:=PageControl1.Pages[PP];
   except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 10');//22.08.12
   end;
  end; 
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 11');//22.08.12
 end;
end;

procedure TFDruckabfrage.FormKeyPress(Sender: TObject; var Key: Char);//30.06.12
begin
 KeyToAccel(key,Self);
end;

procedure TFDruckabfrage.BitBtn4Click(Sender: TObject);
begin
 try
  if Printernummer[LPTNR].nr<=Printer.Printers.Count-1 then
   Printer.PrinterIndex := Printernummer[LPTNR].nr;
  if printer.printerindex<>-1 then begin
   TDialogs.MyMessageDlgOK('Der folgende Dialog ist Bestandteil des Druckertreibers bzw. Windows und nicht des Programms '+produktname+'!'#13+
                'Inhalt, Form und Funktionalitùt liegen vùllig bei Windows bzw. dem Druckertreiber.',
                mtinformation,[mbok],0); //02.03.05
   if not(ShowPrinterOwnDLGLocal(Application.Handle)) then
    TDialogs.MyMessageDlgOK('Aufruf bei diesem Gerùt nicht mùglich!',mtinformation,[mbabort],0);
   SHOW;
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 12');//22.08.12
 end;
end;

function GETWSC(LPTNR : word) : byte;
begin
 getwsc:=1;
end;

procedure TFDruckabfrage.FormCloseQuery(Sender: TObject;
                                        var CanClose: Boolean);
begin
 if wkey = #0 then
  if GlobalZwangsDruck then begin
   TDialogs.MyMessageDlgOK('~Zwangsdruck!'#13'Abbruch nicht erlaubt!',mterror,[mbok],0);
   Canclose:=FALSE;
  end
  else begin
   Auftragbreaked:=TRUE;
   wkey:=t_esc;
//!!16.7.98   reconstructSpool(spld,true);
//   canclose := FALSE; //15.3.99
  end;
end;

procedure TFDruckabfrage.BitBtn5Click(Sender: TObject);
var //07.08.13 i:byte;
 r1,r2,r3:integer;
begin
 //*** FAX 
 if PrinterNummer[0].Name > '' then begin
  try
   DISAB;
   ChkHist;//15.07.09
   R1:= LPTNr;
   R2:= Schacht;
   R3:= WINLPTNr;
   LPTNr := 0;
   Schacht := 0;
   WINLPTNr := PrinterNummer[LPTNr].Nr;
   Panel1.Align:=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;

    //  SetFritz Fax('02 25595 0303');

   WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,direct,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,

                  {(not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or} ((PD_.ItemIndex=1)),

                  False,buff.drucker,FALSE,FALSE,false,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},FALSE);

   LPTNr:=R1;
   Schacht:=R2;
   WINLPTNr:=R3;
  except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 13');//22.08.12
  end;
  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;//15.3.99
  end
  else
   ENAB;
 end
 else
  TDialogs.MyMessageDlgOK('Sie haben kein Faxgerùt in den Einstellungen angegeben!'#13+
             'Bitte holen Sie das in den Einstellungen nach!',
             mtwarning,[mbok],0);
end;

procedure NoMorePrint;
var LPTNr: WORD;
    buff : tmean2;
begin
 GetMeaning(id,buff);
 LPTNr   := buff.Drucker;
 LASTDRUCK[LPTNR] := 0;
 LastDuplex[LPTNr] := 0;
end;

const max = 32;
type   tn = record
             name : shortstring;
             free : array[1..128] of byte;
            end;

procedure SAVPRTIDX;
var IDX, i, j : integer;
        names : array[0..max] of tn;
            f : file;
begin
 try
  IDX := Printer.Printers.Count;
  Fillchar(names,sizeof(names),0);
  //29.06.04:
  if printer.Printers.Count-1>MAX then
   j:=MAX
  else
   j:=printer.Printers.Count-1;
  for i := 0 to j do
   names[i].name := printer.Printers[i];                                                            { TODO 2 -oXEFrank -cS_2_Short : XE }
  io:=ioresult;
  if NetServer or (NetClientNr=1){02.05.11} then
   assignfile(f,pgm{dts}+'PRTIDX1_'+wtools.GetComputername+'.DAT')
  else
  if NetClient or (NetClientNr=2){02.05.11} then
   assignfile(f,pgm{dts}+'PRTIDX2_'+wtools.GetComputername+'.DAT')
  else
  if NetClient2 or (NetClientNr=3){02.05.11} then
   assignfile(f,pgm{dts}+'PRTIDX_'+wtools.GetComputername+'3.DAT')
  else
  if NetClient3 or (NetClientNr=4){02.05.11} then
   assignfile(f,pgm{dts}+'PRTIDX_'+wtools.GetComputername+'4.DAT')
  else
  if NetClient4 or (NetClientNr=5){02.05.11} then
   assignfile(f,pgm{dts}+'PRTIDX_'+wtools.GetComputername+'5.DAT')
  else
  if NetClientNr>0 then begin
   assignfile(f,pgm{dts}+'PRTIDX'+inttostr(NetClientNr)+'_'+wtools.GetComputername+'.DAT');
  end
  else
   assignfile(f,pgm{dts}+'PRTIDX_'+wtools.GetComputername+'.DAT');
  rewrite(F,1);
  BlockWrite(F,IDX,Sizeof(IDX));
  BlockWrite(F,Names,Sizeof(Names));
  CloseFile(F);
  io:=ioresult;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 14');//22.08.12
 end;
end;

procedure CHKPRTIDX;
var       f : file;
        idx : integer;
   _j, j, i : integer;
      names : array[0..max] of tn;
        nam : Shortstring;  // FDXE_S
        st,
       ss2,
         ss : Ansistring;  // FDXE_S

 procedure ResetPapierschaechte;
 var f : file of tmean2;
     m : tmean2;
     i : integer;
 begin
  io := ioresult;
  assignfile(f,_GetMeaning);//10.01.13 pgm{04.04.03 DTS}+Meaning);
  RESET(F);
  if ioresult=0 then begin
   For i:=0 to MaxIDs do begin
    Seek(F,i);
    Read(F,M);
    if ioresult=0 then begin
     m.Papierweg:=-1;
     Seek(F,i);
     system.write(F,M);
    end;
   end;
   CloseFile(F);
   io := ioresult;
//unnùtig!   DoToAllMeaning;//10.01.13
  end;
 end;

 Procedure _PrintStr(S:AnsiString);  // FDXE_S
 var s1,s2:Ansistring;  // FDXE_S
 begin
  While S>'' do begin
   ZeilenUmbruchNotDruckLong(S,s1,s2,80,500);
   PrintStr(s1+#13#10);
   s:=s2;
  end;
 end;

 //23.07.17, versucht auch bei geùnderter Konfiguration die Drucker wieder zuzuordnen
 Function Remapping:Boolean;
 var UsedPrinter:array[0..5] of boolean;
     ok,ok2:boolean;
     buff:tmean2;
     i,j:integer;
     EndlosOld:Array[0..MaxD] of boolean;
     OldPrinterNummer:tPrinterNummer;
 begin
  Result:=false;
  try
{   if Printer.Printers.Count=0 then begin
    TDialogs.MyMessagedlg
    MyHalt;
   end;}

   fillchar(UsedPrinter,sizeof(UsedPrinter),false);
   for i:=2 to 161 do begin
    if i in [2,
             11,12,13,15,18,19,
             21,24,25,30,32,34,40,68,88,89,92,
             105,106,107,114,115,117..125,128,148,150,151,154,155,157,158,161] then begin
     GetMeaning(i,buff);
     UsedPrinter[Buff.Drucker]:=true;
    end;
   end;

   LoadPrinterNummer;

   OK:=true;
   OldPrinterNummer:=PrinterNummer;
   InitPrinterNummer;
   for i:=0 to 5 do
    EndLosOld[i]:=_Endlos[i];
   for i:=0 to 5 do begin //eigene Druckerkonfig
    if UsedPrinter[i] then begin
     ok2:=false;
     for j:=0 to Printer.Printers.Count-1 do begin
      if  Printer.Printers[j] = OldPrinterNummer[i].name then begin// Names[i].Name) then begin
       PrinterNummer[i].nr  :=j;
       PrinterNummer[i].name:=Printer.Printers[j];                                                  { TODO 2 -oXEFrank -cS_2_Short : XE }
//ToDo       _Endlos[i]:=EndlosOld[ tja, wùre zu ermitteln
//s.u. ****
//   IdIsPrintDirect : Array[0..MaxD] of Integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
//  _ENDLOS : Array[0..MAXD] of Boolean = (FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);

       ok2:=true;
       break;
      end;
     end;
     if not(OK2) then begin
      ok:=false;
      break;
     end;
    end;
   end;

   if OK then begin
    Result:=true;
    SavePrinternummer;
    SAVPRTIDX;
    WriteStammdaten//Endlos und IdIsPrintDirect
    //****
    //**** ENDLOS und IdIsPrintDirect dùrften nicht in loadstd in der OPTIONEN Datei sein, sondern woe Printernummern etc. separat, da Einstellungen je Netzwerk
    //**** So gilt das fùr alle Rechner!
    //**** Wùrde auch problemlos in PRTIDXx.DAT passn
    //****
   end
   else
    //geùndert!
  except
   InitPrinterNummer;
  end;
 end;

 var wpr, pp, snew1,snew2,snew3:Shortstring;  // FDXE_S
begin
 if PRTTEST then
  exit;
 try
  PRtTEST :=TRUE;

  io:=ioresult;

  //17.05.11:
  ss:='';
  For i:=0 to printer.printers.count-1 do begin
   if SS>'' then
    ss:=ss+', '
   else
    ss:='Drucker: '; 
   SS:=SS+inttostr(i+1)+':';                                                                        { TODO 2 -oXEFrank -cS_2_Ansi : XE }
   if length(SS)+length(Printer.Printers[i])<160 then
    ss:=ss+Printer.Printers[i]
   else                                                                                             { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    Break; 
  end;
  AddUltimateLog(14,SS);//17.05.11

  if NetServer or (NetClientNr=1){03.05.11} then
   nam:='PRTIDX1'
  else
  if NetClient or (NetClientNr=2){03.05.11} then
   nam:='PRTIDX2'
  else
  if NetClient2 or (NetClientNr=3){03.05.11} then
   nam:='PRTIDX3'
  else
  if NetClient3 or (NetClientNr=4){03.05.11} then
   nam:='PRTIDX4'
  else
  if NetClient4 or (NetClientNr=5){03.05.11} then
   nam:='PRTIDX5'
  else
  if NetClientNr>0 then
   nam:='PRTIDX'+inttostr(NetClientNr)+''
  else                                                                                              { TODO 2 -oXEFrank -cS_2_Short : XE }
   nam:='PRTIDX';

  //31.07.17 jetzt mit Computernamen verknùpft:
  assignfile(f,pgm+NAM+'_'+wtools.GetComputername+'.DAT');
  reset(F,1);
  io:=ioresult;
  if (IO>0) and (FileExists(pgm+NAM+'.DAT')) then begin
   ioresult;
   RenameFile(pgm+NAM+'.DAT',pgm+NAM+'_'+wtools.GetComputername+'.DAT');
   assignfile(f,pgm+NAM+'_'+wtools.GetComputername+'.DAT');
   reset(F,1);
   io:=ioresult;
  end;
  
  if io = 0 then begin
   BlockRead(F,IDX,Sizeof(IDX));
   BlockRead(F,Names,Sizeof(Names));
   CloseFile(F);
   io:=ioresult;

   //29.06.04:
   if IDX>MAX then
    IDX:=MAX;
   if Printer.Printers.Count-1>MAX then begin
    TDialogs.MyMessageDlgOK('Sie haben ùber '+inttostr(max)+' Druckertreiber installiert.'#13#13+
                 'Bitte reduzieren Sie die Anzahl der Treiber auf unter '+inttostr(max)+'!!',
                 mterror,[mbok],0);
    AddLogInfo(63,inttostr(Printer.Printers.Count-1));                                              { TODO 2 -oXEFrank -cS_2_Short : XE }
    AddUltimateLog(14,inttostr(Printer.Printers.Count-1)+' Druckertreiber installiert');//17.05.11  { TODO 2 -oXEFrank -cS_2_Short : XE }
   end;

   if (IDX <= Printer.Printers.Count) then begin
    i := 0;
    While (i <= IDX-1) and
          (i <= Printer.Printers.Count-1) and //13.05.03
          (Printer.Printers[i] = Names[i].Name) do
     inc(i);
   end
   else
    i := IDX+1;

   if ((IDX > Printer.Printers.Count) or (i <> IDX)) and
      Remapping then begin
    PRTTest:=false;
    exit;
   end;
    
   if (IDX > Printer.Printers.Count) or (i <> IDX) then begin
    SS:='Momentane Konfigurationsliste '+inttostr(Printer.Printers.Count)+' Treiber (vorher '+inttostr(IDX)+' Treiber):'#13;//29.11.10 ùnderungen  { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    if i>IDX then
     j:=i
    else
     j:=idx;
    _j:=j;
    if j>8 then
     j:=8;
    SNEW1:='';
    SNEW2:='';
    SNEW3:='';
    try
     For i:=0 to j-1 do begin
      SS:=SS+inttostr(i+1)+':';                                                                     { TODO 2 -oXEFrank -cS_2_Ansi : XE }
      if i<=printer.printers.count-1 then
       SS:=SS+' "'+printer.printers[i]+'"';                                                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }

      //18.08.11:
      if i<=printer.printers.count-1 then
       PP:=printer.printers[i]
      else                                                                                          { TODO 2 -oXEFrank -cS_2_Short : XE }
       PP:='-';
        
      if i<=3 then
       snew1:=snew1+inttostr(i+1)+':'+pp+' '+Names[i].Name
      else                                                                                          { TODO 2 -oXEFrank -cS_2_Short : XE }
      if i<=6 then
       snew2:=snew2+inttostr(i+1)+':'+pp+' '+Names[i].Name
      else                                                                                          { TODO 2 -oXEFrank -cS_2_Short : XE }
      if i<=9 then
       snew3:=snew3+inttostr(i+1)+':'+pp+' '+Names[i].Name;                                         { TODO 2 -oXEFrank -cS_2_Short : XE }

      //29.11.10:
      st:=Names[i].Name;
      SS:=SS+'  *  vorher: "'+st+'"';

      SS:=SS+#13;
     end;
    except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 15');//22.08.12
    end;

    if _J>8 then
     SS:=SS+'u.s.w.';
    AddLogInfo(12,'');//07.06.04
    AddUltimateLog(14,'Geùnderte Druckerkonfig: '+inttostr(Printer.Printers.Count)+' Treiber (vorher '+inttostr(IDX)+' Treiber)');//17.05.11  { TODO 2 -oXEFrank -cS_2_Short : XE }
    AddUltimateLog(14,'Geùnderte Druckerkonfig '+snew1);//17.05.11
    if snew2>'' then
     AddUltimateLog(14,'Geùnderte Druckerkonfig '+snew2);//17.05.11
    if snew3>'' then
     AddUltimateLog(14,'Geùnderte Druckerkonfig '+snew3);//17.05.11

    //06.01.10:
    if GetArbeitsPlatz=0 then
     ss2:='** Sie haben fùr dieses Programmstart KEINE Angaben ùber einen Netzwerkzugriff angegeben! **'
    else begin
     ss2:='** Sie haben dieses Programm als Arbeitsplatz '+inttostr(GetArbeitsPlatz)+' gestartet! **';{ TODO 2 -oXEFrank -cS_2_Ansi : XE }
    end;
    ss2:=ss2+#13+'Fùr jeden Arbeitsplatz wird eine eigene Druckerkonfiguration gespeichert.';//01.12.11

    ss:='Die Druckerkonfiguration im Windows hat sich geùndert (kein Programmfehler - bitte Info und Tipp lesen!). '+//01.12 11, was nicht das GV-Programm verursacht hat. '+
        'Solche ùnderungen erfolgen durch Verwendung mehrerer Rechner ohne Netzwerkparameter, eine Treiberinstallation, Windows-Updates oder Updates der Treiber - NIE durch das GV-Programm.'#13+//01.12.11
        'Oder Sie haben das Programm mit einer anderen Arbeitsplatznummer als zuvor (von einem oder verschiedenen Rechnern aus) gestartet. Siehe Tipp.'#13+//01.12.11
               'Wùhlen Sie im folgenden Dialog die Drucker neu aus.'#13#13+
               'TIPP fùr 99% aller Fùlle: Ist nichts an den Druckern geùndert worden und Sie arbeiten im Netzwerk, mit mehreren Rechnern oder vom USB-Stick ('+produktname+' am Schlùsselbund) '+
               'dann ist vielleicht keine Nummer fùr den Arbeitsplatz beim Start des Programms angegeben worden oder eine Nummer doppelt vergeben!! '+
               'Lesen Sie bitte unter Hilfe nach, wie Sie dieses korrekt einstellen oder kontaktieren Sie den Service!'+#13#13+
               'SEHR WICHTIG: Wenn Sie sich diesbezùglich an den Service wenden, mùssen Sie die gerade in die Druckablage gelegten Infos ausdrucken und mitteilen.'#13+
               #13+SS2+//06.01.10
               #13#13+SS;                                                                           { TODO 2 -oXEFrank -cS_2_Ansi : XE }

    //29.11.10:
    try
     OeffneSPOOLER(80,1,GetLPT(80),'Geùnderte Druckerkonfiguration',DirektDruck,true,false,leerDR);
     PrintStr(#7#6+InitDruckAND15CPI+PRN219+'122'+PRN219+'111');
     PrintStr(GetPageHeader('Geùnderte Druckerkonfiguration '+GetMakeStr));
     PrintStr(GetPageFooter);
     SS2:=SS;
     While (SS2>'') do begin
      if Pos(#13,SS2)>0 then begin
       _PrintStr(Copy(SS2,1,Pos(#13,SS2)-1));
       Delete(SS2,1,Pos(#13,SS2));
      end
      else begin
       _PrintStr(SS2);
       SS2:='';
      end;
      PrintStr(#13#10);
     end;
     printstr(PRN219+'341'#100#0);
     SchliesseSpoolerDat;
    except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 16');//22.08.12
    end;

    TDialogs.MyMessageDlgOK('~ACHTUNG!!!!!'#13+ss,mtwarning,[mbok],0);//29.11.10 ùnderungen...

    PutWarte('Prùfe Druckerkonfiguration (einen Moment bitte...)');//13.05.03
    try
     SAVPRTIDX;

     if NetServer or (NetClientNr=1){03.05.11} then
      WPR:='WPRWahl1'
     else
     if NetClient or (NetClientNr=2){03.05.11} then
      WPR:='WPRWahl2'
     else
     if NetClient2 or (NetClientNr=3){03.05.11} then
      WPR:='WPRWahl3'
     else
     if NetClient3 or (NetClientNr=4){03.05.11} then
      WPR:='WPRWahl4'
     else
     if NetClient4 or (NetClientNr=5){03.05.11} then
      WPR:='WPRWahl5'
     else
     if NetClientNr>0 then
      WPR:='WPRWahl'+inttostr(NetClientnr)
     else                                                                                           { TODO 2 -oXEFrank -cS_2_Short : XE }
      WPR:='WPRWahl';

     DelFile(PGM,WPR+'.DAT');
     DelFile(PGM,WPR+'_'+wtools.GetComputername+'.DAT');

     DelFile(DTS,'WPRWAHL*.DAT');
     InitPrinterNummer;//23.08.00!!!
     LoadPrinternummer;
     FILLCHAR(idisPrintDirect,Sizeof(idisPrintDirect),0);

     ResetPapierSchaechte;
    except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 17');//22.08.12
    end;
    DelWarte;

    try
     WinDruckZiele(0);
    except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 18');//22.08.12
    end;
   end;
  end
  else
   SAVPRTIDX;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 19');//22.08.12
 end;
 PRtTEST :=FALSE;
end;

procedure TFDruckabfrage.FormShow(Sender: TObject);
begin
 GetMaxWinSize(Self,_1280,true,false,true);//20.09.17

 ScaleFormMy(Self);//27.06.10

//lieber nicht height := GetClientheightModal;//05.06.12

 //13.12.11: gab es bisher ùberhaupt nicht!!!
 left:=getleft_modal(width);
 top :=gettop_modal(height);

 if {10.06.03(ID in RichID) or} (DIP[52] and 64=64) then begin
  ImagePanel.Visible:=FALSE;
 end;
end;

procedure TFDruckabfrage.BitBtn7Click(Sender: TObject);
begin
 begin
  //*** EMAIL
  try
   DISAB;
   ChkHist;//15.07.09
   ChkLasteMail:=TRUE;
   LASTPD:=0; //24.3.98
   Panel1.Align:=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;
   PerEMail:=TRUE;//19.01.08
   if (ID in RICHID) then
    WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}55,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,

                  {(not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or} ((PD_.ItemIndex=1)),

                   False,buff.drucker,FALSE,FALSE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},true)//13.12.12 herje FALSE)
   else begin

    //17.05.13:
    if (ID in PDFID) then
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}59,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},true)//13.12.12 herje FALSE)
    else

//07.11.06 kann auch PDF sein    WFehler('Gespeichert wird als BMP und nicht als JPG, da in diesem Fall kleinere Datei',3);
    if (ID in FormID) then
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}57,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},true)//13.12.12 herje FALSE)
    else
    //23.05.03
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}56,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},true);//13.12.12 herje FALSE)
   end;
  except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 20');//22.08.12
  end;
  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;//15.3.99
  end
  else
   ENAB
 end;
end;

procedure TFDruckabfrage.BitBtn8Click(Sender: TObject);
var r : Shortstring;  // FDXE_S
begin
 //*** Mail + Druck
 try
{ if Check Versi on('W04.5',FALSE) then 12.10.01} 
  CANTEND:=TRUE;
  R := Datei;
  DelFile(TempPath,R);
  MyCopyFile('E-Mail',SPLD,R,TempPath,true,'',FALSE,false,false,IgnoreArchivBit);
  BitBTN1Click(Sender);//06.02.06 zuerst Druck, da sonst Anlage.pdf von wspooler gelùscht wird!
//08.02.06  BitBTN7Click(Sender);//eMail
  MyCopyFile('Druck',TempPath,R,SPLD,true,'',FALSE,false,false,IgnoreArchivBit);
  Datei := R;
  AddToHist.Checked:=FALSE;//15.07.09
  BitBTN7Click(Sender);
  PerEMail:=TRUE;//19.01.08
//08.02.06  BitBTN1Click(Sender);//Druck
  CANTEND:=FALSE;
  WKEY:=T_END;
  CLOSE;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 21');//22.08.12
 end;
end;

procedure TFDruckabfrage.BitBtn6Click(Sender: TObject);
var r : Shortstring;  // FDXE_S
begin
 //*** FAX + DRUCK
 try
  CANTEND:=TRUE;
  R := Datei;
 // Datei := 'TEMP.DRK';
  DelFile(TempPath,R);
  MyCopyFile('Vorbereitung lùuft',SPLD,R,TempPath,true,'',FALSE,false,false,IgnoreArchivBit);
  BitBTN5Click(Sender);
  MyCopyFile('Vorbereitung abgeschlossen',TempPath,R,SPLD,true,'',FALSE,false,false,IgnoreArchivBit);
  Datei := R;
  AddToHist.Checked:=FALSE;//15.07.09
  BitBTN1Click(Sender);
  CANTEND:=FALSE;
  WKEY:=T_END;
  CLOSE;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 22');//22.08.12
 end;
end;

procedure CHKMargins;
//29.07.17 VAR x, y, ii : integer;
begin
 EXIT;
 (* //29.07.17
 try
  if (NR < Printer.Printers.Count) and
     (LPTNR in [1..MaxD]) and
     (idisPrintDirect[LPTNR]>0) then begin
   ii:=Printer.PrinterIndex;
   Printer.PrinterIndex := NR;
   X := GetDeviceCaps(printer.Handle,PhysicalOffsetX);
   y := GetDeviceCaps(printer.Handle,PhysicalOffsetY);
   if (X>0) or (Y>0) then begin
    {$H+}
    modal_WINASK(10,'ACHTUNG / WARNUNG!'#13#13+
               'Bei diesem Drucker ist ein linker oder oberer Rand eingestellt! '+
               'Das bewirkt ein falsches bzw. verschobenes Druckergebnis.'#13#13+
               'Gehen Sie beim Drucker '+printer.Printers[NR]+' '+
               'ùber Kontextmenù auf "EIGENSCHAFTEN / Papier / '+
               'NICHTBEDRUCKBARER Bereich" und stellen Sie ALLE Werte auf Null.',
               [mbok]);
    //25.07.13 { $ H -}
   end;
   Printer.PrinterIndex:=ii;
  end;
 except
 end;         *)
end;

procedure TFDruckabfrage.PageControl1DrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);//"6.10.11
{var
  AText: str ing;
  APoint: TPoint;}
begin
 Inherited;
 (*
  with (Control as TPageControl).Canvas do
  begin
    Brush.Color := clBtnFace;//ClGreen;
    FillRect(Rect);
    AText := TPageControl(Control).Pages[TabIndex].Caption;
    with Control.Canvas do begin
      APoint.x := (Rect.Right - Rect.Left) div 2 - TextWidth(AText) div 2;
      APoint.y := (Rect.Bottom - Rect.Top) div 2 - TextHeight(AText) div 2;
      TextRect(Rect, Rect.Left + APoint.x, Rect.Top + APoint.y, AText);
    end;
  end;*)
end;

procedure DruckAuftragParteienGrid2ClipBoard(Parteien:tSortGrid);
var s:VCLstring;
    c:vclchar;
    y:integer;
    aa:ta;

 procedure DoSo;
 begin
  Leiste.ForClipBrd2.Text:=Parteien.Cells[Parteien.Col,Parteien.Row];                               { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  Leiste.ForClipBrd2.SelStart:=0;
  Leiste.ForClipBrd2.SelLength:=Length(Leiste.ForClipBrd2.Text);
  Leiste.ForClipBrd2.CopyToClipboard;
  WFehler(Leiste.ForClipBrd2.Text+' liegt in der Zwischenablage!',2);
 end;

begin
 try
  if (Parteien.Row>0) and (Parteien.Col in [2,3,4,5]) then begin
   if Parteien.Col=5 then
    s:='De-Mail Adresse '+Parteien.Cells[Parteien.Col,Parteien.Row]
   else
   if Parteien.Col=4 then
    s:='safe-id '+Parteien.Cells[Parteien.Col,Parteien.Row]
   else
   if Parteien.Col=2 then
    s:='Faxnummer '+Parteien.Cells[Parteien.Col,Parteien.Row]
   else
    s:='E-Mail Adresse '+Parteien.Cells[Parteien.Col,Parteien.Row];
   if (trim(Parteien.Cells[Parteien.Col,Parteien.Row])>'') and
      (trim(Parteien.Cells[Parteien.Col,Parteien.Row])<>'-') then begin

    if Pos(#13,s)>0 then begin
     fillchar(AA,sizeof(AA),0);
     preinitpopupModal(AA);
     y:=0;
     while s>'' do begin
      inc(y);
      aa[y]:=GetNextItem(s,#13);                                                                    { TODO 2 -oXEFrank -cS_2_Short : XE }
     end;
     y:=WinPopUpModal( c, AA, 0, 'Auswahl',1,true,false,0);
     if c<>t_esc then begin
      s:=AA[y];
      DoSo;
     end;
    end
    else
    if (TDialogs.MyMessageDlgOK('Wollen Sie d. '+s+' ins Clipboard / die Zwischenablage legen?',
                   mtconfirmation,[mbyes,mbno],0)=mryes) then begin
     DoSo;
    end;
   end;
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 23');//22.08.12
 end;
end;

procedure TFDruckabfrage.ParteienClick(Sender: TObject);
begin
 DruckAuftragParteienGrid2ClipBoard(Parteien);
end;

procedure TFDruckabfrage.ParteienGetCellFormat(Sender: TObject; Col,
  Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
begin
 //06.06.12:
//20.06.12 formatoptions.brush.color := clwindow;
 formatoptions.brush.color := Parteien.Color;//22.06.12
 formatoptions.font.color  := Parteien.Font.Color;//clblack;//clwindowtext;

 formatoptions.alignmentvert := taMultiLine;

 //03.06.14:
 try
  if (Col>1) and (Row>0) and
     (Parteien.Cells[Col,Row]<>'-') and
     (Parteien.Cells[Col,Row]>'')
     then
   formatoptions.brush.color := Parteien.Color//$005353FF// $0099FDAD
  else
   formatoptions.brush.color := Parteien.Color;
 except
  on EXC_:Exception do
   ExceptionHintUser0('02143',EXC_.Message,EXC_.ClassName);
 end;
 
end;

procedure TFDruckabfrage.PDClick(Sender: TObject);
begin
 //29.09.06:
 if PD_.ItemIndex=1 then
  idisprintdirect[LPTNR]:=4
 else
  idisprintdirect[LPTNR]:=0;
 CHK;
 WriteStammdaten;//19.08.14  SAVEDP;
 CHK;
end;

procedure TFDruckabfrage.L6Click(Sender: TObject);
begin
(*26.05.03 {$H+}
 TDialogs.MyMessageDlgOK('Sie haben "Druckertreiber umgehen" (RAW-Modus) NICHT eingeschaltet.'#13+
              'Wenn das mùglich ist, sollten Sie das aber unbedingt einstellen. '+
              'Dadurch haben fehlerhafte Druckertreiber nùmlich keine Auswirkungen '+
              'mehr und der Druck ist wesentlich schneller!',
              mtinformation,[mbok],0);
 //25.07.13 { $ H -}*)
end;


(*29.08.13 jetzt in spooler
Function TFDruckabfrage.AskPDFFil ename(var filename, defext : Ansistring;Var OK:Boolean):Boolean;
var b:boolean;
begin
//07.08.13 s.u. b:=false;
 try
  //Dateinamen erfragen
  SaveDialog1.Filter    :='PDF|*.PDF';
//  SaveDialog1.DefaultExt:='.PDF';//03.10.12

   //16.08.06:
  if PDFPath>'' then
   SaveDialog1.InitialDir:=PDFPath
  else
   SaveDialog1.InitialDir:=GetMyFil esPath;

  //18.06.12: Fùr Bereitstellen...
  if BereitD ok>'' then begin
   SaveDialog1.FileName:=BereitDo k;
   B:=TRUE;
  end
  else

  if IsVista then begin
   Filename:='';
//   DefExt:='.PDF';//03.10.12 ''
 {$H+}
   B:=OpenSaveFileDialogVista(Self,DefExt,
                              SaveDialog1.Filter,
                              SaveDialog1.InitialDir,
                              'PD F spei chern un ter',FileName,
                              FALSE,False,False,FALSE);
 //25.07.13 { $ H -}
   SaveDialog1.FileName:=FileName;
  end
  else
   B:=SaveDialog1.Execute;                                     


  if B then begin //09.11.06 if SaveDialog1.Execute then begin

   //16.08.06:
   PDFPath:=ExtractFilePath(SaveDialog1.FileName);//31.08.06 SaveDialog1.InitialDir;
   WriteStammdaten;

   WSpooler.PDFNam e:=SaveDialog1.FileName;
   if Uppercase(ExtractFileExt(WSpooler.PDFNam e))<>'.PDF' then
    WSpooler.PDFNam e:=WSpooler.PDFNam e+'.pdf';

   WSpooler.PDFF ileNam e:=WSpooler.PD FNam e;//20.06.12, da PDFNam e schon gebracht wird. Was ein Durcheinander...

   //06.12.05:
   ok:=true;
   if FileExists(WSpooler.PDFNam e) then
    if TDialogs.MyMessageDlgOK('Die Datei '+WSpooler.PDFNam e+' existiert bereits!'#13#13+
                    'Soll diese ùberschrieben werden?',
                    mtconfirmation,[mbyes,mbno],0)=mryes then
     DeleteFile sWithUndo(WSpooler.PDFNam e)
    else
     ok:=false;
  end
  
  //20.06.12:
  else
   OK:=FALSE;

 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 24');//22.08.12
 end;
 Result:=OK;
end;
*)
procedure TFDruckabfrage.BitBtn9Click(Sender: TObject); //PDF
var  r1,r2,r3:integer;
     ZurAkte:Boolean;//06.09.22

 procedure Ausgabe(PDFMode:Boolean);
 var plus:byte;
 begin

  //06.09.22:
  if ZurAkte then
   plus:=10
  else
   plus:=0;

  if (ID in RICHID) then
   WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,77+plus,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,

                  {(not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or} ((PD_.ItemIndex=1)),

                  False,buff.drucker,FALSE,FALSE,TRUE,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},PDFMode)//03.07.21 TRUE)//20.06.12 FALSE)
  else

  //17.05.13:
  if (ID in PDFID) then
   WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,80+plus,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                  TRUE,TRUE,buff.drucker,FALSE,TRUE,TRUE,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},PDFMode)//03.07.21 TRUE)//20.06.12 FALSE)
  else

  if (ID in FormID) then
   WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,78+plus,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                  TRUE,TRUE,buff.drucker,FALSE,TRUE,TRUE,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},PDFMode)//03.07.21 TRUE)//20.06.12 FALSE)
  else
  //23.05.03
   WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,79+plus,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                  TRUE,TRUE,buff.drucker,FALSE,TRUE,TRUE,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},PDFMode);//03.07.21 TRUE);//20.06.12 FALSE)
 end;

var _E:ansistring;
begin //BitBtn9Click
 //*** PDF speichern

 ZurAkte:=AllowDruckZuAkte and ((Sender=ZuAkte) or (Sender=ZuAkteDruck));//06.09.22
 
 //15.07.09:
 if PrinterNummer[9].Name='' then
  DIP[60]:=DIP[60] or 4;

 if (PrinterNummer[9].Name > '') or
    (DIP[60] and 4=4) //05.12.05
    then begin
  try
   DISAB;
   ChkHist;//15.07.09
   R1:= LPTNr;
   R2:= Schacht;
   R3:= WINLPTNr;
   LPTNr := 9;
   Schacht := 0;
   WINLPTNr := PrinterNummer[LPTNr].Nr;
   Panel1.Align:=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;

   //06.09.22:
   if ZurAkte then begin
    Get_SPL_DiscrOnly(SPLd+Datei,_E);
    if AskZurAkte( _DR,_DRI,_E{13.09.22} ) then //KillDRAusDateiname kann ich nicht ùbernehmen. Berùcksichtigt kein DR I
     Ausgabe(true);
   end
   else

   //01.03.05:
   if (DIP[60] and 4=4) then begin  //Eigene PDF Routinen

    //03.09.13:
    if AskPDFFileName then begin //Ok) then begin
     Ausgabe(true);
    end
    else begin
     WFehler('Abbruch der PDF-Erstellung, da kein Dateiname eingegeben wurde!',2);
    end;
   end

   else begin

    //03.07.21: Ist wie ein normaler Druck, nur eben fauf den fùr PDF gewùhltem Gerùt
    AltPrinter:=WINLPTNr;
    BitBtn11Click(Sender);
    AltPrinter:=-1;

    //03.07.21 NEIN,
    (* //03.07.21 UNFASSBAR, das scheint seit 2012 oder sogar 2005 nie einer genutzt zu haben, denn es funktionierte so nicht
    WinSpoolCreate(A NZ,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,direct,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                   (not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or (AllisCanvasTemp and (PD_.ItemIndex=1)),
                   False,buff.drucker,FALSE,FALSE,FALSE,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},TRUE);//20.06.12 FALSE);*)
    (*Das klappt zB bei Kassensturz nicht
    //03.06.21 muss wie ein Druck und nicht PDF Print behandelt werden: Eigenartigerweise wurde genau das 2012 geùndert
    WinSpoolCreate(A NZ,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,direct,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                   (not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or (AllisCanvasTemp and (PD_.ItemIndex=1)),
                   False,buff.drucker,FALSE,FALSE,FALSE,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},FALSE);*)

   end;                

   LPTNr:=R1;
   Schacht:=R2;
   WINLPTNr:=R3;
  except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 25');//22.08.12
  end;
  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;
  end
  else
   ENAB;
 end
 else
  TDialogs.MyMessageDlgOK('Sie haben keinen PDF-Erzeuger in den Einstellungen angegeben!'#13+
               'Bitte holen Sie das in den Einstellungen nach!',
               mtwarning,[mbok],0);
(* 25.01.05
begin
{
You have only to connect those GSM Modules via a 1:1 serial cabel to one
serial port of your pc. In Textmode you can easiely send SMs via a
terminalprogram like Hyperterminal.

Example:
PC:AT+CMGS="phonenr"<CR>Message<Ctrl-Z>
Modul:OK

Sometimes you have to wait for an '>' cha r after the first
<CR> (AT+CMGS="phonenr"<CR><Wait for '>'>Message<Ctrl-Z>

oder

SMSSEN32 [zieltelefonnummer] textdatei
55 ù Shareware mit Delphi geschrieben
}

 begin
  try
   ChkLasteMail:=TRUE;
   LAST PD:=0;
   Panel1.Align:=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;
   WinSpool Create(A NZ,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}66,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                  P D_.ItemIndex=4,False,buff.drucker,FALSE,FALSE);
  except
   on EXC_:Exception do
    ExceptionHintUser0('02144',EXC_.Message,EXC_.ClassName);
  end;
  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;
  end;
 end;
*)
end;

procedure TFDruckabfrage.CheckBox1Click(Sender: TObject);
begin
 //08.10.12:
 if checkbox1.checked then
  DIP[52]:=DIP[52] or 64
 else
  DIP[52]:=DIP[52] and (255-64);
 WriteOneDip(52);
end;

procedure TFDruckabfrage.SigHClick(Sender: TObject);
begin
 HintSigPDF;//06.12.13
end;

procedure TFDruckabfrage.BitBtn10Click(Sender: TObject);//*** PDF + DRUCK
var r : Shortstring;  // FDXE_S
begin
 //*** PDF + DRUCK
 try
  CANTEND:=TRUE;
  R := Datei;
  DelFile(TempPath,R);
  MyCopyFile('Vorbereitung lùuft',SPLD,R,TempPath,true,'',FALSE,false,false,IgnoreArchivBit);
  BitBTN9Click(Sender);//speichern
  MyCopyFile('Vorbereitung abgeschlossen',TempPath,R,SPLD,true,'',FALSE,false,false,IgnoreArchivBit);
  Datei := R;
  AddToHist.Checked:=FALSE;//15.07.09
  BitBTN1Click(Sender);//Drucken
  CANTEND:=FALSE;
  WKEY:=T_END;
  CLOSE;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 26');//22.08.12
 end;
end;

procedure TFDruckabfrage.nextClick(Sender: TObject);
begin
 try
  if AktPage<Pages then begin
   AktPage:=AktPage+1;
   ShowPage;
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 27');//22.08.12
 end;
end;

procedure TFDruckabfrage.lastClick(Sender: TObject);
begin
 try
  if AktPage>1 then begin
   AktPage:=AktPage-1;
   ShowPage;
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 28');//22.08.12
 end;
end;

procedure TFDruckabfrage.ZoomClick(Sender: TObject);
var i:integer;
    s:shortstring;
begin
 try
  PreWShowBMP;
  for i:=1 to Pages do begin
   S:=GetFileName(i);
   if pos('_39',S)>0 then begin
    S[pos('_39',S)+1]:='9';
    S[pos('_99',S)+2]:='6';
   end;
   if BMP then
    CopyRenameSilent(FormPICPath+S,temppath+'BMP'+inttostr(i)+ExtractFileExt(S))
   else
    CopyRenameSilent(FormPICPath+S,temppath+'Anlage'+inttostr(i)+ExtractFileExt(S));
  end;
  WShowBMP('');
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 29');//22.08.12
 end;
end;

procedure TFDruckabfrage.ShowPage;
begin
 try
  PreView2.Enabled:=FALSE;
  PreView2.Visible:=FALSE;
  Preview3.Enabled:=FALSE;//16.07.09

  if (ID in RichID) or (ID in FormID)
     or (ID in PDFID)//17.05.13
     then
   PreViewHint
  else
  if Pages<=0 then begin

//   PreViewHint;
   PreView2.Enabled:=TRUE; //20.06.03
   PreView2.Visible:=TRUE; //20.06.03

   PreView3.Enabled:=TRUE; //16.07.09

   Keins.Visible:=TRUE;
   Keins2.Visible:=TRUE;
   Keins3.Visible:=TRUE;
   if (ID in RichID) then
    Image1.Visible:=TRUE
   else
    Image1.Visible:=FALSE;
   next.enabled:=FALSE;
   last.enabled:=FALSE;
   zoom.enabled:=FALSE;
  end
  else begin
   Keins.Visible :=FALSE;
   Keins2.Visible :=FALSE;
   Keins3.Visible :=FALSE;
   next.enabled:=TRUE;
   last.enabled:=TRUE;
   zoom.enabled:=TRUE;
   Image1.Visible:=TRUE;

   if DPI=39 then begin
    Image1.AutoSize:=TRUE;
    Image1.Proportional:=FALSE;
   end
   else begin
    Image1.AutoSize:=FALSE;
    Image1.Proportional:=TRUE;
   end;

   try
 //   StatusBar1.Panels[0].Text:='Seite '+Inttostr(AktPage);
    Image1.Picture.LoadFromFile(FormPICPath+GetFileName(AktPage));
   except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 30');//22.08.12
   end;
   Next.Enabled:=(AktPage<Pages);
   Last.Enabled:=(AktPage>1);
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 31');//22.08.12
 end;
end;

procedure TFDruckabfrage.SigPDFClick(Sender: TObject);
begin
 {16.02.23
 //06.12.13:
 if SigPDF.checked then
  DIP[88]:=DIP[88] or 128
 else
  DIP[88]:=DIP[88] and (255-128);
 WriteOneDip(88);}
end;

procedure TFDruckabfrage.SammelakteClick(Sender: TObject);
var //dok:shortstring;
    LD,
    ID:Word;
    Serie:Word;
    c:vclchar;
    ddd:byte;
    Sammel:integer;
    Sammelakte:Boolean;
    Bezeichnung:ansistring;
begin
 //*** GENERALAKTE
 Disab;
 try

  Sammel:=AskGeneralakte(Sammelakte);
  if Sammel=-1 then begin
   ENAB;
   exit;
  end;

  Get_SPL_DiscrOnly(SPLd+Datei,Bezeichnung);
  Bezeichnung:=CrunchToLen(CleanFileName(Bezeichnung),40);//Korrekt CrunchToLen, da nur ASCII       { TODO 2 -oXEFrank -cW_2_Ansi : XE }

  if Sammelakte then
   WSpooler.PDFName:=FotoDIR_K+'SA'
  else
   WSpooler.PDFName:=FotoDIR_K+'GA';
  WSpooler.PDFName:=WSpooler.PDFName+inttostr(Sammel)+' '+Bezeichnung+' '+
                    GetMakeStrFileSortFriendy+' '+stringreplace(GetMakeStrZeit,':','-',[rfreplaceall])+
                    '.PDF';                                                                         { TODO 2 -oXEFrank -cS_2_Short : XE }
  //13.05.24 WSpooler.PDFFileName:=WSpooler.PDFName;
  BereitDok:=WSpooler.PDFName;//Trick, damit nicht nach dem PDF Namen gefragt wird!

  DirektePDF:=TRUE;
//  dok:=SPLd+Datei;

  Val(Copy(datei,5-1,4),Serie,dek.error);
  VAL(Copy(datei,Pos('.',datei)+1,3),ID,dek.error);

  if UpperCase(Copy(DATEI,2-1,1)) = PDFSplChar then
   DDD := PDFSplNr
  else
  if UpperCase(Copy(DATEI,2-1,1)) = 'F' then
   DDD := 33
  else
  if UpperCase(Copy(DATEI,2-1,1)) = 'G' then
   DDD := 9
  else
  if UpperCase(Copy(DATEI,2-1,1)) = 'R' then
   DDD := 2
  else
  if UpperCase(Copy(DATEI,2-1,1)) = 'P' then
   DDD := 1
  else
   DDD := 0;

  LD:=ID;
  C:=T_END;
  InSchlange(C,
             ddd,
             ID,
             Serie,
             GetSPLAnzDeCoded(datei),
             1);
  if not(DirektDruckInSerie) then //08.06.12
   DirektDruck := FALSE; {22.7.98}
  NoMorePrint(LD);

  BereitDok:='';
  DirektePDF:=FALSE;
  WFehler('Dokument abgelegt.',2);

  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;//15.3.99
  end;

 except
  on EXC_:Exception do
   ExceptionHintUser0('02145',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TFDruckabfrage.SammelakteDruckClick(Sender: TObject);
var rett:shortstring;
    Serie:Word;
    name:shortstring;
begin
 //*** GENERALAKTE + DRUCK
 try

  CantEnd:=TRUE;

  rett:=datei;

  //Kopie anlegen:
  Val(Copy(rett,5-1,4),Serie,dek.error);
  VAL(Copy(rett,Pos('.',rett)+1,3),ID,dek.error);

  //21.11.11:
  SearchNextSerie(Name,Serie,copy(rett,1,3),0,ExtractFileExt(rett));                                { TODO 2 -oXEFrank -cS_2_Short : XE }
  CopyRenameSilent(SPLd+rett,spld+name);

  datei:=name;
  SammelAkteClick(Sender);

  datei:=rett;
  BitBtn1Click(Sender);

  CANTEND:=FALSE;
  WKEY:=T_END;
  CLOSE;
  
 except
  on EXC_:Exception do
   ExceptionHintUser0('02146',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TFDruckabfrage.SammelausgabeClick(Sender: TObject);
var c:VCLChar;
    y:longint;
   aa:spez2.ta;
begin
 preinitpopupModal(AA);
 AA[1]:='Gesammelte Dokumente zeigen';
 AA[2]:='Alles drucken (aktueller Druckauftrag + gesammelte Dokumente)';
 AA[3]:='Alles faxen (aktueller Druckauftrag + gesammelte Dokumente)';
 AA[4]:='Alles per E-Mail (aktueller Druckauftrag + gesammelte Dokumente)';
 AA[5]:='Alles in eine PDF (aktueller Druckauftrag + gesammelte Dokumente)';
 AA[6]:='Alles per EGVP / eBO (aktueller Druckauftrag + gesammelte Dokumente)';
 y:=WinPopUpModal(c,aa,0,'Ausgabe der gesammelten Dokumente',1,true,false,0);
 if c<>t_esc then begin
  case y of
   1:CollectShowClick(Sender);
   2:CollectDruckClick(Sender);//16.02.21 BitBtn15Click(Sender);
   3:CollectFaxClick(Sender);
   4:CollectMailClick(Sender);
   5:CollectPDFClick(Sender);
   6:CollectEGVPClick(Sender);
  end;
 end;
end;

procedure TFDruckabfrage.ScrollBox1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
 //06.12.16:
 inherited;
 if PtInRect(ScrollBox1.ClientRect, ScrollBox1.ScreenToClient(MousePos)) then begin

  try
   //28.07.20:
   if InComboBox((Self as tForm),handled) then
    exit;

   ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta;
  except
   on EXC_:Exception do
    ExceptionHintUser0('02147',EXC_.Message,EXC_.ClassName);
  end;
  Handled := true;
 end;

end;

procedure TFDruckabfrage.SearchImages;
begin
 try
  Pages:=0;
  AktPage:=1;
  DPI:=39;
  BMP:=FALSE;

  if (ID in PDFID) then begin
   next.Enabled:=FALSE;
   last.enabled:=FALSE;
   zoom.enabled:=FALSE;
   PreViewHint;
   EXIT;
  end
  else

  if (ID in RichID) or (ID in FormID) then begin
   next.Enabled:=FALSE;
   last.enabled:=FALSE;
   zoom.enabled:=FALSE;
   PreViewHint;
   EXIT;
  end
  else
  if (ID in RichID) or (DIP[52] and 64=64) then begin
   next.Enabled:=FALSE;
   last.enabled:=FALSE;
   zoom.enabled:=FALSE;
   exit;
  end;

{  if ID in RichID then begin
   GetTextMetrics(Image1.Canvas.Handle, fTextMetrics);
   with Range do begin

    LogX := 312;
    LogY := 430;

    rc.left  :=0;//Trunc(StartingLeft*1440/LogX);
    rc.top   :=0;//Trunc(StartingTop*1440/LogY);

    MeasureUnitsToPixelsH(AvailablePageWidth)
    rc.right :=LogX;//Trunc((StartingLeft+PixelPrintWidth)* 1440/LogX);
    rc.Bottom:=LogY;//Trunc((StartingTop+PixelPrintHeight)* 1440/LogY);
    rcPage := rc;

    hdc := Image1.Canvas.Handle;
    hdcTarget := hdc;

    chrg.cpMin := 0;
//    OldMap := SetMapMode(hdc, MM_TEXT);
    SendMessage(Image1.Canvas.Handle, EM_FORMATRANGE, 0, 0);    // flush buffer
    SendMessage(Handle, EM_FORMATRANGE, 1, Longint(@Range));
   end;
  end;}


{  if not(FileExists(FormPICPath+GetFileName(1))) then //39er JPG
   DPI:=96;                 //96er JPG
  if not(FileExists(FormPICPath+GetFileName(1))) then
   BMP:=TRUE;               //96er BMP
  if not(FileExists(FormPICPath+GetFileName(1))) then
   DPI:=39;                 //39er BMP}

  if (FileExists(FormPICPath+GetFileName(1))) then begin

   if POS('_39',GetFileName(1))>0 then
    DPI:=39
   else
    DPI:=96;
   if Pos('.JPG',Uppercase(GetFileName(1)))>0 then
    BMP:=FALSE
   else
    BMP:=TRUE;

   Pages:=1;
   While FileExists(FormPICPath+GetFileName(pages)) do
    inc(Pages);
   Dec(Pages);
  end
  else
   Pages:=0;
  ShowPage;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 32');//22.08.12
 end;
end;

Function TFDruckabfrage.GetFileName(Page:Integer):ShortString;  // FDXE_S
var Picname:shortstring;
begin
 Result:='';
 try
  ReadMeaningPic(ID,PICName);
  if Picname<>'' then
   if Page=1 then
    Result:=PICNAME {+'_'+inttostr(DPI)+'.'+ext}
   else begin
    if POS('_',Picname)>0 then
     Insert('_'+inttostr(Page),PicName,POS('_',Picname));                                           { TODO 2 -oXEFrank -cS_2_Short : XE }
    Result:=PICNAME{+'_'+inttostr(page)+'_'+inttostr(DPI)+'.'+ext};
   end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 33');//22.08.12
 end;
end;

procedure tfdruckabfrage.previewhint;
begin
 SubPreView.Align:=alClient;
 SubPreview.Visible:=TRUE;
 PreView.Enabled:=TRUE;
 PreView3.Enabled:=TRUE;//16.07.09
 Keins.Visible:=FALSE;
 Keins2.Visible:=FALSE;
 Keins3.Visible:=FALSE;
end;

procedure tfdruckabfrage.viewasbmp;
var    LPTNr,
      Schacht : Integer;
         buff : tmean2;
          DDD : Byte;
begin
 try
  GetMeaning(id,buff);
  schacht := buff.papierweg;
  LPTNr   := buff.Drucker;

  //28.08.25:
  if UpperCase(Copy(DATEI,2-1,1)) = PDFSplChar then begin
   wdruckmenu.MakeRealFile(s,temppath+'VOR.PDF');
   ShowPDFFile(temppath+'VOR.PDF');
   {28.08.25
   DDD := PDFSplNr //es wùrde Sinn machen, einfach das PDF abzulegen und den Win Viewer aufzurufen, wie schon andernorts im Prog!
   }
  end

  //17.05.13:
  else begin
   if UpperCase(Copy(DATEI,2-1,1)) = 'F' then
    DDD := 33
   else
   if UpperCase(Copy(DATEI,2-1,1)) = 'G' then
    DDD := 9
   else
   if UpperCase(Copy(DATEI,2-1,1)) = 'R' then
    DDD := 2
   else
   if UpperCase(Copy(DATEI,2-1,1)) = 'P' then
    DDD := 1
   else
    DDD := 0;
   PreWShowBMP;
   InVorschau:=TRUE;//28.12.11
   try
    WinSpoolCreate(1{ANZ},LPTNR{+1},S,PRINTERNUMMER[buff.drucker].nr,
                   schacht,DDD,LSCheck(ID),GetSchrift(TRUE),Buff.Xoffs,Buff.YOffs,FALSE,
                   buff.oncevorschub,buff.laenge,buff.zoll,ID,0{LastDuplex[LPTNr]},
                   TRUE{Las tPD=4},TRUE,buff.drucker,FALSE,FALSE,FALSE,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},FALSE);
    WShowBMP('');
   except
            on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 34');//22.08.12
   end;
   InVorschau:=FALSE;//28.12.11
  end;
 except
  on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 35');//22.08.12
 end;
end;

procedure TFDruckabfrage.PreViewClick(Sender: TObject);
var s:shortstring;
begin
 //*** VORSCHAU
 try

  //16.07.09:
  if Liste.ItemIndex>0 then
   s:=SPLD+GetSPLName(Liste.ItemIndex+1)
  else
   s:=SPLD+DATEI;

  if ID in RICHID then begin
   wdruckmenu.MakeRealFile(s,temppath+'VOR.RTF');
   PreviewRTF(temppath+'VOR.RTF',_DR,_DRI);
{   !!MODAL! RichVorschau!!!
   WinE ditor( 55, temppath+'VOR.RTF', 0,0,0,0,CODE,FALSE,FALSE,FALSE,FALSE,FALSE,0,FALSE,true);}
  end
  else
   ViewAsBMP(s);
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 36');//22.08.12
 end;
end;

procedure TFDruckabfrage.DISAB; //10.12.03
begin

 Timer1.Enabled:=false;//16.01.13

 BitBtn1.Enabled :=FALSE;
 BitBtn2.Enabled :=FALSE;
 BitBtn5.Enabled :=FALSE;
 BitBtn6.Enabled :=FALSE;
 BitBtn7.Enabled :=FALSE;
 BitBtn8.Enabled :=FALSE;
 BitBtn9.Enabled :=FALSE;//31.01.05
 BitBtn10.Enabled:=FALSE;//31.01.05
 ZuAkte.Enabled :=FALSE;
 ZuAkteDruck.Enabled :=FALSE;
//13.08.13 BitBtn11.Enabled:=FALSE;//01.02.05
 ScrollBox1.Enabled:=FALSE;//13.08.13
 BitBtn12.Enabled:=FALSE;//02.03.05
 Preview.Enabled :=FALSE;//02.03.05
 Preview2.Enabled:=FALSE;//02.03.05
 Preview3.Enabled:=FALSE;//16.07.09
 BitBtn15.Enabled:=FALSE;//28.08.08
 BitBtn16.Enabled:=FALSE;//04.04.12
// BitBtn17.Enabled:=FALSE;//07.06.12
 BitBtn18.Enabled:=FALSE;//07.06.12

 //18.10.06:
 Collect.Enabled    :=FALSE;
 CollectFax.Enabled :=FALSE;
 CollectMail.Enabled:=FALSE;
 CollectPDF.Enabled :=FALSE;//20.06.12
 CollectEGVP.Enabled:=FALSE;//20.06.12
 CollectShow.Enabled:=FALSE;//06.06.12
 
 SammelAusgabe.Enabled  :=FALSE;
 SammelAkte.Enabled     :=FALSE;
 SammelAkteDruck.Enabled:=FALSE;

 //12.12.06:
 Qual.Enabled:=FALSE;
 aus.Enabled :=FALSE;
 einv.Enabled:=FALSE;
 einh.Enabled:=FALSE;

end;

procedure TFDruckabfrage.ENAB;  //10.12.03
begin
 BitBtn1.Enabled :=true;
 BitBtn2.Enabled :=true;
 BitBtn5.Enabled :=true;
 BitBtn6.Enabled :=true;
 BitBtn7.Enabled :=true;
 BitBtn8.Enabled :=true;
 BitBtn9.Enabled :=true;//31.01.05
 BitBtn10.Enabled:=true;//31.01.05
 ZuAkte.Enabled :=true;
 ZuAkteDruck.Enabled :=true;
 ScrollBox1.Enabled:=TRUE;//13.08.13
//13.08.13 BitBtn11.Enabled:=TRUE;//01.02.05
 BitBtn12.Enabled:=TRUE;//02.03.05
 Preview.Enabled :=TRUE;//02.03.05
 Preview2.Enabled:=TRUE;//02.03.05
 Preview3.Enabled:=TRUE;//16.07.09
 BitBtn15.Enabled:=TRUE;//28.08.08
 BitBtn16.Enabled:=TRUE;//04.04.12
// BitBtn17.Enabled:=TRUE;//07.06.12
 BitBtn18.Enabled:=TRUE;//07.06.12

 //18.10.06:
 Collect.Enabled    :=TRUE;
 CollectFax.Enabled :=CollectExist(GetWorkStationNr);
 CollectMail.Enabled:=CollectFax.Enabled;
 CollectPDF.Enabled :=CollectFax.Enabled;//20.06.12
 CollectEGVP.Enabled:=CollectFax.Enabled;//20.06.12
 CollectShow.Enabled:=CollectFax.Enabled;

 SammelAusgabe.Enabled  :=true;
 SammelAkte.Enabled     :=true;
 SammelAkteDruck.Enabled:=true;

 //12.12.06:
 Qual.Enabled:=TRUE;
 aus.Enabled :=TRUE;
 einv.Enabled:=TRUE;
 einh.Enabled:=TRUE;

end;

procedure TFDruckabfrage.AChange(Sender: TObject);
//var i:integer;
begin
{ //16.08.13:
 Val(A.Text,i,dek.error);
 if i=0 then begin
  a.text:='1';
  an z:=1;
 end
 else
  An z:=i;}
end;

procedure TFDruckabfrage.AltOnClick(Sender: TObject);//13.08.13
begin
 try
  AltPrinter:=TControl(Sender).Tag;
  BitBtn11Click(Sender);
  AltPrinter:=-1;
 except
  on EXC_:Exception do
   ExceptionHintUser0('02148',EXC_.Message,EXC_.ClassName);
 end; 
end;

procedure TFDruckabfrage.BitBtn11Click(Sender: TObject); //17.03.04
var //07.08.13 i:byte;
 r1,r2,r3:integer;
begin
 try
  DISAB;
  ChkHist;//15.07.09
  R1:= LPTNr;
  R2:= Schacht;
  R3:= WINLPTNr;
  LPTNr := 255;
  Schacht := 0;
  WINLPTNr := -1;
  Panel1.Align:=alclient;
  Panel1.Visible:=TRUE;
  Application.ProcessMessages;

  WinSpoolCreate(ANZ,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,direct,
                 LSCheck(ID),GetSchrift(TRUE),
                 0{GetEinzugX},0{GetEinzugY},FALSE,
                 0{buff.oncevorschub},
                 11{buff.laenge},
                 true{buff.zoll},ID,dmdup_simplex,
                 FALSE{als BMP},False,-1{buff.drucker},FALSE,FALSE,FALSE,_DR,_DRI,0,FALSE);

  LPTNr:=R1;
  Schacht:=R2;
  WINLPTNr:=R3;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 37');//22.08.12
 end;
 if not(CANTEND) then begin
  wkey:=t_end;
  CLOSE;//15.3.99
 end;
end;

procedure TFDruckabfrage.BitBtn12Click(Sender: TObject);
begin
 if TDialogs.MyMessagedlg('Handbuch? Bei Nein wird eine Sammlung von Tipps und Kurzhilfen bei Problemen gezeigt.',mtconfirmation,[mbyes,mbno],0)=mryes then
  showhilfeidx(helpcontext)
 else
  DruckHelp;
end;

 procedure tfdruckabfrage.ShowDR;//24.03.05
 Var  DRNr:Nr_Typ;
       DRI:Boolean;
       _F,
       Safe,
       DeMail,
       _ESubj,
       eMailPw,
        _E:ansistring;//ShortString;

  procedure _Show(G,gf,ge:tOwnLabel;ADR:AdressenTyp);
  Const P='........................................................................................';
  begin
   if trim(ADR.Fax[0].Tel)>'' then
    GF.Caption:=ADR.Fax[0].Tel+P
   else
    GF.Caption:='Nein'+P;
   if trim(ADR.eMail[0].Adr)>'' then
    GE.Caption:=ADR.eMail[0].Adr
   else
    GE.Caption:='Nein';
   G.Caption :=(EMU.GetAdrLine(ADR))+P;
  end;

 begin

  if not(P367Mode) and not(PEVMode) then begin
   //Anzeige Parteien und Verfùgbarkeit von Fax/eMail
   //Bei Fax und eMail dann Auswahlbox!!!

   try
    DRNr.DR:=0;
    Posi.Caption:=inttostr(Updown.Position)+'/'+inttostr(updown.max);

    Get_Fax_eMail(SPLd+DAT,_F,_E,SAFE,_ESubj,DeMail,eMailPw,DRI,DRNr);

{    //16.07.09:
    Ge t_SPL_Discr(SPLd+DAT,_ E);
    Discr.Text:=_ E;}

    if (DRNr.DR>0) and (DRNr.Jahr>0) then begin

     //15.07.09:
     if AutoAddHist or (ID in RichID) then begin
      AddToHist.Caption  :='Automatisch in Historie zur Sache abgelegt!';//11.02.10 Text war zu lang
      AddToHist.Font.Size:=QualT.Font.Size;//12.08.09
      AddToHist.Visible  :=TRUE;
      AddToHist.Enabled  :=FALSE;
      AddToHist.Checked  :=TRUE;
     end
     else
      AddToHist.Visible:=CheckVersion('W20.0',true);//15.07.09

     AddToHist.Font.Size :=11;
     AddToHist.Font.Style:=[fsbold];

     //05.06.12:
     ParteienInGrid(DRI,DRNr,Parteien);
     _DRI:=DRI;
     _DR :=DRNR;
     if DRI then begin
      Panel4.Visible:=TRUE;
{      if CheckVersion('W18.0',true) then
       DR.Caption:=GetDRStr(DRNr,TRUE,DR_In^.Seite[Nr].Bezirksnummer)
      else
       DR.Caption:='DR I '+DRToString(DRNr);}
     end
     else begin
      Panel4.Visible:=TRUE;
//      DR.Caption:=GetDRSTR(DRNr,FALSE,DR_II.BezirksNummer); // cdr2+' '+MakeS tr1(DRNr);
     end;

(*05.06.12
{     //16.07.09:
     if (Pos('E V-Abnahme',Discr.Text)>0) and (ID in Form ID) and (Land in PruefGrupp e08) then begin
      AddToHist.Caption:='Ablage in Historie in Ihrem Land leider untersagt!';
      AddToHist.Enabled:=FALSE;
      AddToHist.Checked:=FALSE;
     end;}

     A  :=Adresse;
     AZ :=AdressenZeiger;
     SUP:=AdresseSuper;
     if DRI then begin
      _DRIR:=DR_In^;
      if DRIExist(DRNr.DR,DRNr.Jahr,DRIJahre,TRUE) then begin
       _DRI:=TRUE;
       _DR :=DRNR;
       Panel4.Visible:=TRUE;

       Seite := (DRNr.DR+7) div 8;
       Nr    := Succ(Pred(DRNr.DR) mod 8);

       //12.01.07:
       if CheckVersion('W18.0',true) then
        DR.Caption:=GetDRStr(DRNr,TRUE,DR_In^.Seite[Nr].Bezirksnummer)
       else

        DR.Caption:='DR I '+DRToString(DRNr);

       LiesD RI(DRNr.Jahr,Seite);
       ReLoadParteienDRI(DR_In^.Seite[Nr]);//08.10.07
       _Show(G,gf,ge,DR_In^.Seite[Nr].Glaubiger);
       _Show(V,Vf,Ve,DR_In^.Seite[Nr].Glaubiger_V);
       _Show(S,Sf,Se,DR_In^.Seite[Nr].Schuldner);
       SV.Visible:=FALSE;
       SVf.Visible:=FALSE;
       SVe.Visible:=FALSE;
       _Show(D,Df,De,DR_In^.Seite[Nr].Schuldner_3);
       K.Visible:=FALSE;
       Kf.Visible:=FALSE;
       Ke.Visible:=FALSE;
      end;
      DR_In^:=_DRIR;
     end
     else begin
      _DRR:=dr_II;
      if DRIIExist(DRNr.DR,DRNr.Jahr,DRIIJahre,TRUE) then begin
       _DRI:=FALSE;
       _DR :=DRNR;
       Panel4.Visible:=TRUE;
       DR.Caption:=GetDRSTR(DRNr,FALSE,DR_II.BezirksNummer); // cdr2+' '+MakeS tr1(DRNr);
       LiesDRII(DRNr.Jahr,DRNr.DR);
       _Show(G,gf,ge,DR_II.Glaubiger);
       _Show(V,Vf,Ve,DR_II.Glaubiger_V);
       _Show(S,Sf,Se,DR_II.Schuldner);
       GetAdresse(DR_ii.Schuldner_V);
       _Show(SV,SVf,SVe,Adresse);
       GetAdresse(DR_ii.Schuldner_3);
       _Show(D,Df,De,Adresse);
       GetAdresse(DR_ii.KostSchuld);
       _Show(K,Kf,Ke,Adresse);
      end;
      DR_II:=_DRR;
     end;
     Adresse       :=A;
     AdressenZeiger:=AZ;
     AdresseSuper  :=Sup;*)
    end

    //16.12.13:
    else
    if not(DruckInfosInGrid(_F,_E,Safe,DeMail,Parteien)) then
     Parteien.Visible:=FALSE;
     
   except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 38');//22.08.12
   end;
  end

  else//16.121.3:

  //28.06.12:
  if DRNr.DR=0 then
   Parteien.Visible:=FALSE;

  //29.07.12:
  if not(Parteien.Visible) then
   Panel7.Visible:=FALSE;
 end;

procedure TFDruckabfrage.UpDownClick(Sender: TObject; Button: TUDBtnType);//24.03.05
begin
 try
  ShowDR(GetSPLName(Updown.Position));
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 39');//22.08.12
 end;
end;

Function tfdruckabfrage.GetSPLName(Nr:Integer):ShortString;//16.07.09
var f:textFile;
    i:integer;
    s:shortstring;
begin
 s:='';
 try
  ioresult;
  assignfile(f,temppath+'QUEUE'+strword(GetWorkStationNr,1)+'.TMP');
  Reset(F);
  if IOResult=0 then begin
   For i:=1 to Nr do
    system.readln(F,s);
   if ioresult>0 then
    S:='';
   CloseFile(F);
   ioresult;
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 40');//22.08.12
 end;
 Result:=s;
end;

procedure TFDruckabfrage.IneAkteClick(Sender: TObject);
begin
  TeAktenHelper.ToggleSaveToEAkte(IneAkte.Checked);
end;

procedure TFDruckabfrage.CollectClick(Sender: TObject);//18.10.06
begin
 //*** SAMMELN
 
//Als PDF/BMP ablegen und dort belassen
//Dann ùber Druck, Sammelablage faxen / per E-Mail
  try
   WSpooler.PDFName:='';//20.06.12
   DISAB;
   ChkHist;//15.07.09
   ChkLasteMail:=TRUE;
   LASTPD:=0; //24.3.98
   Panel1.Align:=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;
   if (ID in RICHID) then
    WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}155,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,

                  {(not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or} ((PD_.ItemIndex=1)),

                   False,buff.drucker,FALSE,FALSE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},TRUE)//20.06.12 FALSE)
   else begin

    //17.05.13:
    if (ID in PDFID) then
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}158,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},TRUE)//20.06.12 FALSE)
    else

    if (ID in FormID) then
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}157,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},TRUE)//20.06.12 FALSE)
    else
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}156,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},TRUE)//20.06.12 FALSE)
   end;
  except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 41');//22.08.12
  end;
  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;//15.3.99
  end
  else
   ENAB
end;

procedure TFDruckabfrage.CollectEGVPClick(Sender: TObject);//28.06.12
begin
(*08.12.12 Unsinn, dennoch zulassen!
   //27.09.12:
   if GetImportF='' then begin
    TDialogs.MyMessageDlgOK('EGV P ist nicht korrekt konfiguriert!',mterror,[mbabort],0);
    Exit;
   end;*)

 CantEnd:=TRUE;
 CollectClick(Sender);
 CantEnd:=FALSE;
 Disab;
 AddToHist.Checked:=FALSE;
 EGVPPDFOverride:= true; // BW 27.02.23 Ticket #1495SigPDF.Checked;//03.12.13
 EGVPOverride:=Sig.ItemIndex;
 DoCollectEGVP(GetWorkStationNr);
 EGVPOverride:=-1;
 wkey:=t_end;
 CLOSE;
end;

procedure TFDruckabfrage.CollectDruckClick(Sender: TObject);//16.02.21
begin
 CantEnd:=TRUE;
 CollectClick(Sender);
 CantEnd:=FALSE;
 Disab;
 AddToHist.Checked:=FALSE;//15.07.09
 DoCollectFax(GetWorkStationNr,true);
 wkey:=t_end;
 CLOSE;
end;

procedure TFDruckabfrage.CollectFaxClick(Sender: TObject);//18.10.06
//07.08.13 i:byte;
//07.08.13  r1,r2,r3:integer;
begin
 CantEnd:=TRUE;
 CollectClick(Sender);
 CantEnd:=FALSE;
 Disab;
 AddToHist.Checked:=FALSE;//15.07.09
 DoCollectFax(GetWorkStationNr);
 wkey:=t_end;
 CLOSE;

 (*
 if PrinterNummer[0].Name > '' then begin
  try
   DISAB;
   R1:= LPTNr;
   R2:= Schacht;
   R3:= WINLPTNr;
   LPTNr := 0;
   Schacht := 0;
   WINLPTNr := PrinterNummer[LPTNr].Nr;
   Panel1.Align:=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;

{  noch zu programmieren!!!!
   WinSpoolCreate(AN Z,LPTNR,SPLd+Datei,WINlptnr,schacht,direct,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,

                  (not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or (AllisCanvasTemp and (PD_.ItemIndex=1)),

                  False,buff.drucker,FALSE,FALSE,false,_DR,_DRI,FALSE);}

   LPTNr:=R1;
   Schacht:=R2;
   WINLPTNr:=R3;
  except
   on EXC_:Exception do
    ExceptionHintUser0('02149',EXC_.Message,EXC_.ClassName);
  end;
  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;
  end
  else
   ENAB;
 end
 else
  TDialogs.MyMessageDlgOK('Sie haben kein Faxgerùt in den Einstellungen angegeben!'#13+
             'Bitte holen Sie das in den Einstellungen nach!',
             mtwarning,[mbok],0);

// DoCollectFax;*)
end;

procedure TFDruckabfrage.CollectMailClick(Sender: TObject);//18.10.06
begin
 CantEnd:=TRUE;
 CollectClick(Sender);
 CantEnd:=FALSE;
 Disab;
 AddToHist.Checked:=FALSE;//15.07.09
 DoCollectMail(GetWorkStationNr);
 wkey:=t_end;
 CLOSE;
(*
  try
   DISAB;
   ChkLasteMail:=TRUE;
   LASTPD:=0; //24.3.98
   Panel1.Align:=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;
   if (ID in RICHID) then
    WinSpoolCreate(AN Z,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}16 5,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,

                  (not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or (AllisCanvasTemp and (PD_.ItemIndex=1)),

                   False,buff.drucker,FALSE,FALSE,DIP[6 0] and 2=2,_DR,_DRI)
   else begin
//07.11.06    WFehler('Gespeichert wird als BMP und nicht als JPG, da in diesem Fall kleinere Datei',3);
    if (ID in Form ID) then
     WinSpoolCreate(AN Z,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}16 7,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[6 0] and 2=2,_DR,_DRI)
    else
    //23.05.03
     WinSpoolCreate(AN Z,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}16 6,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[6 0] and 2=2,_DR,_DRI)
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('02150',EXC_.Message,EXC_.ClassName);
  end;
  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;//15.3.99
  end
  else
   ENAB

// DoCollectMail;*)
end;

procedure TFDruckabfrage.Analyse(Fax:Boolean;Nr,PNr:Integer);//15.01.07
var     r:integer;
        Zoll12,
     Zoll:Boolean;
    _LEN,
  Duplex,
   Laenge:Integer;
       _R:Real;
      Rec:TPrinterRec;
begin
 PutWarte('Analyse...');
 Memo1.Lines.Clear;
 try
  r:=printer.printerindex;
  try
   printer.printerindex:=PNr;
   Fillchar(SaveprinterRecs,Sizeof(SaveprinterRecs),0);//damit neu geladen!
   try
    GetPrinterStatus2(Rec,TRUE,true,false,false);
   except
    on EXC_:Exception do
     ExceptionHintUser0('02151',EXC_.Message,EXC_.ClassName);
   end;

   if not(Fax) then begin
    Memo1.Lines.Add(DruckerName.Text);
    //27.11.17
    Memo1.Lines.Add(SchachtWahl.Text);
    {27.11.17
    if Papierweg.Text>'' then
     Memo1.Lines.Add(Papierweg.Text);}

    Memo1.Lines.Add('');
   end;

   Memo1.Lines.Add('Momentane Einstellungen im Treiber:');
   Memo1.Lines.Add('Druckerauflùsung: '+Crunch(StrLongint(Rec.HorzDPI,10))+'x'+Crunch(StrLongInt(Rec.VertDPI,10)));
   Memo1.Lines.Add('Papiergrùùe in Punkten: '+Crunch(StrLongInt(Rec.SizeX,10))+'x'+Crunch(StrLongint(Rec.SizeY,10)));
   Memo1.Lines.Add('Bedruckbarer Bereich in Punkten: '+Crunch(StrLongint(Rec.HorzRes,10))+'x'+Crunch(StrLongInt(Rec.VertRes,10)));

   if EINH.Checked then
    Duplex:=DMdup_horizontal
   else
   if EINV.Checked then
    Duplex:=DMdup_vertical
   else
    Duplex:=DMdup_simplex;

   //aus WSpooler:
   Zoll12:=FALSE;
   if ID in A4Formate then begin
    if (Nr<>255) and _ENDLOS[Nr] then begin //12"
     Zoll12:=TRUE;
     LAENGE := 12;
     ZOLL   := TRUE;
    end
    else begin
     LAENGE := 11;
     ZOLL   := TRUE;
    end;
   end
   else begin
    Zoll  :=FALSE;
    Laenge:=buff.laenge;//!!!
   end;

   if Zoll then begin
    _LEN := Round(Laenge*EinInchCM*10);
    if Laenge=11 then
     SetPaperlen_BIN_Duplex_TTFOpt_FAST(TRUE,0,Schacht,Duplex,GetQual{23.08.12 QUALITY[Nr]},ID,Zoll12)
    else
     SetPaperlen_BIN_Duplex_TTFOpt_FAST(FALSE,_LEN,Schacht,Duplex,GetQual{23.08.12 QUALITY[Nr]},ID,Zoll12);
   end
   else begin
    _R   := Laenge;
    _Len := Round((_R*EinInchCM*10)/6);
    SetPaperlen_BIN_Duplex_TTFOpt_FAST(FALSE,_LEN,Schacht,Duplex,GetQual{23.08.12 QUALITY[Nr]},ID,false);
   end;

   Memo1.Lines.Add('');
   Memo1.Lines.Add('Nach Einstellung der Druckparameter (Papierlùnge, Papierweg, Duplex-Modus, Druckqualitùt etc.) liefert der Treiber:');
   Memo1.Lines.Add('Druckerauflùsung: '+Crunch(StrIntDez(Rec.HorzDPI,10))+' x '+Crunch(StrIntDez(Rec.VertDPI,10))+' DPI');
   Memo1.Lines.Add('Papiergrùùe in Punkten: '+Crunch(StrIntDez(Rec.SizeX,10))+' x '+Crunch(StrIntDez(Rec.SizeY,10)));
   Memo1.Lines.Add('Bedruckbarer Bereich in Punkten: '+Crunch(StrIntDez(Rec.HorzRes,10))+' x '+Crunch(StrIntDez(Rec.VertRes,10)));

   if not(Fax) and (Rec.SizeX=Rec.HorzRes) and (Rec.SizeY=Rec.VertRes) then begin //05.06.12 VertSize) then begin
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Der bedruckbare Bereich ist identisch mit der Papiergrùùe.');
    Memo1.Lines.Add('Dadurch wird eventuell randlos gedruckt und Randeinstellungen sind ohne Wirkung.');
    Memo1.Lines.Add('Dies ist z.B. bei folgenden EPSON-Druckern der Fall: Stylus Color Drucker, LQ-860/1060.');
    Memo1.Lines.Add('Falls Sie einen Farbtintenstrahl oder Farbnadeldrucker verwenden, installieren Sie bitte zusùtzlich einen Treiber fùr komp. Gerùte.');
    Memo1.Lines.Add('Bei EPSON Druckern empfehlen wir einen Treiber fùr EPSON LQ-870 zu installieren.');
    Memo1.Lines.Add('');
   end;

  except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 42');//22.08.12
  end;
  printer.printerindex:=r;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 43');//22.08.12
 end;
 DelWarte;
end;

procedure TFDruckabfrage.BitBtn13Click(Sender: TObject);//15.01.07
begin
 Analyse(False,LPTNR,WINLPTnr);
end;

procedure TFDruckabfrage.BitBtn14Click(Sender: TObject);
begin
 Analyse(TRUE,0,PrinterNummer[0].Nr);
end;

//16.02.21: Routine ist wohl grober Unfug:
procedure TFDruckabfrage.BitBtn15Click(Sender: TObject);//23.04.08
var r : Shortstring;  // FDXE_S
begin
 //*** SAMMELN + DRUCK
 try
  ChkHist;//15.07.09
  CANTEND:=TRUE;                                                  
  R := Datei;
  DelFile(TempPath,R);
  MyCopyFile('Vorbereitung lùuft',SPLD,R,TempPath,true,'',FALSE,false,false,IgnoreArchivBit);
  CollectClick(Sender);
  MyCopyFile('Vorbereitung abgeschlossen',TempPath,R,SPLD,true,'',FALSE,false,false,IgnoreArchivBit);
  Datei := R;
  BitBTN1Click(Sender);
  CANTEND:=FALSE;
  WKEY:=T_END;
  CLOSE;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 44');//22.08.12
 end;
end;

procedure TFDruckabfrage.BitBtn16Click(Sender: TObject);
begin
 //*** E G V P

// PerEGVP;//15.12.11
//   Procedure EGVP_Mak eMail(Empfaenger,Betreff,_Text:AnsiString;Signatur:TSignatur;Anlagen:AnsiString;Test:Boolean);//28.06.12
  //28.06.12:
  try

   EGVPPDFOverride:= true; // BW 27.02.23 Ticket #1495SigPDF.Checked;//03.12.13
   EGVPOverride:=Sig.ItemIndex;
   DISAB;
   ChkHist;//15.07.09
//   ChkLasteMail:=TRUE;
   LASTPD:=0; //24.3.98
   Panel1.Align:=alclient;
   Panel1.Visible:=TRUE;
   Application.ProcessMessages;
//   PerEMail:=TRUE;//19.01.08
   if (ID in RICHID) then
    WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}190,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,

                  {(not(AllisCanvasTemp) and (PD_.ItemIndex=4)) or} ((PD_.ItemIndex=1)),

                   False,buff.drucker,FALSE,FALSE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},FALSE)
   else begin

    //17.05.13:
    if (ID in PDFID) then
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}193,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},FALSE)
    else

    if (ID in FormID) then
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}191,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},FALSE)
    else
    //23.05.03
     WinSpoolCreate(1{26.09.22 ANZ},LPTNR{+1},SPLd+Datei,WINlptnr,schacht,{direct}192,LSCheck(ID),GetSchrift(TRUE),
                    GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                    FALSE,FALSE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual{23.08.12 QUALITY[buff.drucker]},TRUE)//21.05.13 kein BMP, aber PDF!
   end;

   //16.07.12:
   if EGVPStart.Checked then
    SubAufruf(2698);

  except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 45');//22.08.12
  end;
  EGVPOverride:=-1;
  if not(CANTEND) then begin
   wkey:=t_end;
   CLOSE;//15.3.99
  end
  else
   ENAB
end;

procedure TFDruckabfrage.BitBtn18Click(Sender: TObject);//28.06.12
var r : Shortstring;  // FDXE_S
begin
 //E G V P + DRUCK
 try
  CANTEND:=TRUE;
  R := Datei;
  DelFile(TempPath,R);
  MyCopyFile('Vorbereitung lùuft',SPLD,R,TempPath,true,'',FALSE,false,false,IgnoreArchivBit);
  BitBTN16Click(Sender);
  MyCopyFile('Vorbereitung abgeschlossen',TempPath,R,SPLD,true,'',FALSE,false,false,IgnoreArchivBit);
  Datei := R;
  AddToHist.Checked:=FALSE;//15.07.09
  BitBTN1Click(Sender);
  CANTEND:=FALSE;
  WKEY:=T_END;
  CLOSE;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 46');//22.08.12
 end;
end;

procedure TFDruckabfrage.CollectShowClick(Sender: TObject);
begin
 //06.06.12
 ShowCollect(GetWorkStationNr);
end;

procedure TFDruckabfrage.CollectPDFClick(Sender: TObject);
begin
 //05.06.12, 20.06.12:
 //03.09.13:

 if AskPDFFileName then begin
  CantEnd:=TRUE;
  CollectClick(Sender);
  CantEnd:=FALSE;
  Disab;
  AddToHist.Checked:=FALSE;//15.07.09
  DoCollectPDF(GetWorkStationNr);
  wkey:=t_end;
  CLOSE;
 end;
end;

Procedure tfDruckabfrage.ChkHist;//15.07.09
Var _E:ansistring;//ShortString;
begin
 try
  if AddToHist.Checked and AddToHist.Enabled and AddToHist.Visible then begin
   LocalAddHist:=TRUE;
   if (_DR.DR>0) and (_DR.Jahr>0) then begin

    //11.02.10: eben nicht GlobalSplName!
    Get_SPL_DiscrOnly(SPLd+Datei,_E);
    AddSpoolToHist(_DR,_DRI,_E,SPLd+Datei);//15.07.09
    //Wichtig: Weitere Druckauftrùge werden ebenfalls in der Historie abgelegt, was LocalAddHist steuert!
   end;
  end;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 47');//22.08.12
 end;
end;

procedure TFDruckabfrage.Timer1Timer(Sender: TObject);//20.06.11
var s:shortstring;
begin
 try
  s:=GetStatus(WinLPTNr);
  Stati.Visible:=S>'';
  Stati.Caption:='Druckerstatus: '{21.11.11}+S;
  Timer1.Enabled:=TRUE;
 except
           on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag 48');//22.08.12
 end;
end;

Procedure FillPrintScrollBox;//13.08.13
var //19.08.13 w,
    i:integer;
    IM:tImage;
    L:tLabel;

 Function GetAccel:Shortstring;
 begin
{klappt nicht
  if i<8 then
   Result:='&'+inttostr(i+1)+' '
  else}
   Result:='';
 end;

begin
 if Printer.Printers.Count>0 then
  try
    for i := 0 to Printer.Printers.Count-1 do begin
     if i=0 then begin
      IMGSrc.Tag:=i;
      IMGSrc.OnClick:=OnClick;
      LabelSrc.Caption:=GetAccel+Printer.Printers[i];
      LabelSrc.Tag:=i;
      LabelSrc.OnClick:=OnClick;
     end
     else begin
      IM:=tImage.Create(S);
      IM.Parent:=s;
      IM.Top:=IMGSrc.Top;
      IM.Left:=1+i*(IMGSrc.Width)+15;
      IM.Width:=IMGSrc.Width;
      IM.Height:=IMGSrc.Height;
      IM.AutoSize:=false;
      IM.Center:=true;
      IM.Transparent:=true;
      IM.Picture.Assign(ImgSrc.Picture);
      IM.Tag:=i;
      IM.OnClick:=OnClick;

      L:=tLabel.Create(S);
      L.Parent:=s;
      L.Top:=LabelSrc.Top;
      L.Left:=5+i*(IMGSrc.Width)+15; //Label ist kùrzer wegen Abstand der Texte!
      L.Width:=LabelSrc.Width;
      L.Height:=LabelSrc.Height;
      L.AutoSize:=false;
      L.AlignMent:=taCenter;
      L.Transparent:=true;
      L.WordWrap:=true;
      L.ShowAccelChar:=true;
      L.Font.Assign(LabelSrc.Font);
      L.Caption:=GetAccel+Printer.Printers[i];
      L.Tag:=i;
      L.OnClick:=OnClick;
//      L.FocusControl:=IM;
     end;
    end;
    S.HorzScrollBar.Range:=Printer.Printers.Count*IMGSrc.Width+2;
  except
   on EXC_:Exception do
    ExceptionHintUser0('02152',EXC_.Message,EXC_.ClassName);
  end;
end;

procedure TFDruckabfrage.NureAkteClick(Sender: TObject);//18.08.25
begin
 //*** eAkte
 try
  DISAB;
  ChkHist;
  LASTPD:=0;
  Panel1.Align:=alclient;
  Panel1.Visible:=TRUE;
  Application.ProcessMessages;
  if (ID in RICHID) then
   WinSpoolCreate(1,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,170,LSCheck(ID),GetSchrift(TRUE),
                  GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                  ((PD_.ItemIndex=1)),
                  False,buff.drucker,FALSE,FALSE,DIP[60] and 2=2,_DR,_DRI,GetQual,FALSE)
  else begin
   if (ID in PDFID) then
    WinSpoolCreate(1,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,173,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                   TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual,FALSE)
   else
   if (ID in FormID) then
    WinSpoolCreate(1,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,171,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                   TRUE,TRUE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual,FALSE)
   else
    WinSpoolCreate(1,LPTNR{+1},SPLd+Datei,WINlptnr,schacht,172,LSCheck(ID),GetSchrift(TRUE),
                   GetEinzugX,GetEinzugY,FALSE,buff.oncevorschub,buff.laenge,buff.zoll,ID,dmdup_simplex,
                   FALSE,FALSE,buff.drucker,FALSE,TRUE,DIP[60] and 2=2,_DR,_DRI,GetQual,TRUE)
  end;
 except
  on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'wdruckauftrag e');
 end;
 if not(CANTEND) then begin
  wkey:=t_end;
  CLOSE;
 end
 else
  ENAB
end;

end.