{$H+}
unit wVVEinlieferung;//19.09.12

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, wXJustizKundeneingabe, ExtCtrls, Buttons, MyColorBitBtn, wownlabel,
  dek, CheckLst, MaskOwn, WDREingabe, WEingabe, Wmemo, AdvOfficePager, Mask,
  WMask, CSEZform, WDatum, wFrameOffeneAkten, WCheckBoxN, UniStdCtrls,
  WUniMEMO, AdvGlowButton, MyStdButton, AdvOfficePagerStylers, WideStrUtils, WCheckBox
, Classes.Helper
, PDF.Intf
, PDF.QuickPDF.Intf             // FL 22.01.25
;

//Const OneVVPDFName='Komplettes Vermùgensverzeichnis';//04.11.12
{
Merke:
Das EINE PDF ist:
DR Jahr GVS!GUID.pdf
}
var FormVVEinlieferaktiv:boolean=false;//21.04.14

type
  TfVVEinlieferung = class(TCSEZForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ZenVGTransfer: TComboBox;
    OwnLabel3: TOwnLabel;
    KopieGl: TCheckBox;
    KopieS: TCheckBox;
    KopieGlArt: TComboBox;
    KopieSArt: TComboBox;
    DruckNachweis: TCheckBox;
    Panel3: TPanel;
    Label1: TLabel;
    Liste: TCheckListBox;
    VerfNrL: TOwnLabel;
    VerfNr: TEingabe;
    Neu: TRadioButton;
    Nachbesserung: TRadioButton;
    Erneut: TRadioButton;
    Panel4: TPanel;
    Header: TPanel;
    FuerT: TOwnLabel;
    OwnLabel2: TOwnLabel;
    DRNrL: TOwnLabel;
    Auftrag: TOwnLabel;
    RubrumS: TComboBox;
    RubrumG: tOwnUniMemo;
    DRNr: TDREingabe;
    AdvOfficePager1: TAdvOfficePager;
    Grundlagen: TAdvOfficePage;
    Angaben: TAdvOfficePage;
    Einstellungen: TAdvOfficePage;
    Anleit: TAdvOfficePage;
    Label2: TLabel;
    Label3: TLabel;
    OK1: TMyStdButton;
    BitBtn4: TMyStdButton;
    OK2: TMyStdButton;
    BitBtn5: TMyStdButton;
    Panel6: TPanel;
    AddDocument: TMyStdButton;
    OwnLabel1: TOwnLabel;
    PAnz2: TMask;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Label5: TLabel;
    BearbeitHint: TLabel;
    VerfOK: TShape;
    Loeschung: TRadioButton;
    Label7: TLabel;
    Irrig: TCheckBox;
    bestaetigt: TAdvOfficePage;
    ColorBitButton1: TMyStdButton;
    OwnLabel4: TOwnLabel;
    PAnz: TMask;
    OwnLabel5: TOwnLabel;
    PrintDialog1: TPrintDialog;
    Label9: TLabel;
    Label10: TLabel;
    Schuldner: TLabel;
    Frist1: TCheckBox;
    Label13: TLabel;
    EintragSVPost: TCheckBox;
    OwnLabel7: TOwnLabel;
    VVListe: TListBox;
    VVListeDatNamen: TListBox;
    ShowPDF: TMyStdButton;
    Label11: TLabel;
    EintragSVpers: TCheckBox;
    ImTermin: TCheckBox;
    Wiedervorlage: TCheckBox;
    WiedervorlageD: TDatum;
    DrittGl: TCheckBox;
    KeineFrist: TCheckBox;
    Label14: TLabel;
    ZAPost: TCheckBox;
    ZApers: TCheckBox;
    BitBtn17: TMyStdButton;
    BitBtn9: TMyStdButton;
    BitBtn1: TMyStdButton;
    helpA: TMyStdButton;
    BitBtn3: TMyStdButton;
    BitBtn7: TMyStdButton;
    BitBtn8: TMyStdButton;
    helpb: TMyStdButton;
    OwnLabel8: TOwnLabel;
    OwnLabel9: TOwnLabel;
    KopieSchuldnerDirekt: TCheckBox;
    Proto: TCheckBox;
    WeitereGl: TCheckBox;
    ProtoVAKVfg: TCheckBox;
    RubrumSNeu: tOwnUniMemo;
    Label15: TLabel;
    HintCall: TOwnLabel;
    CheckBox1: TCheckBox;
    VAK3Art: TComboBox;
    CheckBox2: TCheckBox;
    safeid3L: TOwnLabel;
    safeid3: TEingabe;
    Panel7: TPanel;
    Panel12: TPanel;
    Label16: TLabel;
    Label17: TLabel;
    VerfNrDatL: TOwnLabel;
    VerfNrDat: TDatum;
    Sengenberger: TMyStdButton;
    Richter: TMyStdButton;
    Dwl: TMyStdButton;
    Label18: TLabel;
    Zu882: TMyStdButton;
    OwnLabel11: TOwnLabel;
    OwnLabel12: TOwnLabel;
    Label19: TLabel;
    DR_: TEingabe;
    Label20: TLabel;
    Weitere: TCheckBox;
    Proto2: TCheckBox;
    ProtoVAKVfg2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    KopieAkte: TCheckBox;
    KopieAkte2: TCheckBox;
    OwnLabel6: TOwnLabel;
    Korrektur: TRadioButton;
    Warn: TLabel;
    OwnLabel15: TOwnLabel;
    JetztSV: TCheckBox;
    OwnLabel17: TOwnLabel;
    JetztSVDat: TDatum;
    CheckBox6: TCheckBox;
    Panel13: TPanel;
    Vorbuch: TCheckBox;
    VorbuchMerk: TCheckBox;
    NoSa: TmyCheckBoxN;
    NachbesserHint: TOwnLabel;
    MyStdButton1: TMyStdButton;
    MyStdButton2: TMyStdButton;
    MyStdButton3: TMyStdButton;
    AdvOfficePagerOfficeStyler2: TAdvOfficePagerOfficeStyler;
    Aktenzeichen_EmpfaengerL: TOwnLabel;
    Aktenzeichen_Empfaenger: TEingabe;
    NotSave: TmyCheckBox;
    OwnLabel10: TOwnLabel;
    procedure BitBtn2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RubrumSDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure VerfNrExit(Sender: TObject);
    procedure ChangeEintragGrund(Sender: TObject);
    procedure ZenVGTransferChange(Sender: TObject);
    procedure f755802l1Button1Click(Sender: TObject);
    procedure OK1Click(Sender: TObject);
    procedure OK2Click(Sender: TObject);
    procedure AddDocumentClick(Sender: TObject);
    procedure OnePDFBtnClick(Sender: TObject);
    procedure VerfNrChange(Sender: TObject);
    procedure AdvOfficePager1Changing(Sender: TObject; FromPage,
      ToPage: Integer; var AllowChange: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ListeClick(Sender: TObject);
    procedure ColorBitButton1Click(Sender: TObject);
    procedure RubrumSChange(Sender: TObject);
    procedure KopieSClick(Sender: TObject);
    procedure KopieGlClick(Sender: TObject);
    procedure Frist1Click(Sender: TObject);
    procedure EintragSVPostClick(Sender: TObject);
    procedure ShowPDFClick(Sender: TObject);
    procedure ImTerminClick(Sender: TObject);
    procedure EintragSVpersClick(Sender: TObject);
    procedure WiedervorlageClick(Sender: TObject);
    procedure WiedervorlageDExit(Sender: TObject);
    procedure KeineFristClick(Sender: TObject);
    procedure ZAPostClick(Sender: TObject);
    procedure ZApersClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure VAK3ArtChange(Sender: TObject);
    procedure LeerClick(Sender: TObject);
    procedure RichterClick(Sender: TObject);
    procedure SengenbergerClick(Sender: TObject);
    procedure DwlClick(Sender: TObject);
    procedure Zu882Click(Sender: TObject);
    procedure DrittGlClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DruckNachweisClick(Sender: TObject);
    procedure KopieSchuldnerDirektClick(Sender: TObject);
    procedure KopieAkteClick(Sender: TObject);
    procedure KopieAkte2Click(Sender: TObject);
    procedure VerfNrDatExit(Sender: TObject);
    procedure JetztSVClick(Sender: TObject);
    procedure JetztSVDatExit(Sender: TObject);
    procedure NoSaClick(Sender: TObject);
    procedure MyStdButton1Click(Sender: TObject);
    procedure MyStdButton3Click(Sender: TObject);
  private
    { Private-Deklarationen }
   f755802l1:Tf755802l;
   INOK2,
   CloseOK,
   DoWait,
   OnePDFDone:Boolean;
   DR:Nr_Typ;
   S_Adresse:AdressenTyp;
   InInit,
   BelegeEingaben :Boolean;
   Sache:AnsiString;
   Az,
   FertigesPDF:Shortstring;//10.10.12
   RettUse_Name,//29.05.15
   VorgabeID,
   MyGUID:Shortstring;//04.11.12
   fFrameOffeneAkten1:tfFrameOffeneAkten;//20.04.16 dynamisch erzeugt
   DateiNamen:Array of ShortString;// FL 13.10.12 nicht mehr//super mit FileNamen doppelt vorhanden, grmpf
   OptionalesDatumVVQuittung : Datumstyp;// BW 17.07.24
   procedure SearchVV;
   procedure Init;
   procedure DoOnePDF;
//07.12.12   procedure WMSize(var M : TWMSIZE) ; message WM_Size;
   Procedure PrintPDFFile(name:Shortstring;FirstFile,HintGl:Boolean;Var abort:boolean);//30.10.12
   Function  GetTheLastVV:AnsiString;
   Procedure ChkWarn;//25.03.14
   Procedure SetSexAnAnrede(aFemal,aMale,aUnknown,aDivers : boolean);// FL 02.01.23
  public
    { Public-Deklarationen }
   ChkExit,//18.04.13
   Abgearbeitet,//18.04.13
   _ANotFound:Boolean;//21.03.13
   Obj:tObj;
   procedure CheckVV_and_LoadinForm;
  end;

  Procedure MakePDFDummy(Dateiname:Shortstring);   // FL 17.10.12

Procedure PrintPDFGlHint(Vorgabe:Nr_Typ);//27.02.13
Procedure DoSavePDFGlHint(name:Shortstring);//27.02.13
Procedure DoPrintPDFFile(PrintDialog1:tPrintDialog;
                         name:Shortstring;FirstFile,HintGl:Boolean;Var abort:boolean);//30.10.12;

//Druckt gesetzlichen Hinweis in VV fùr Glùubiger ein:
Procedure AddVVGlHint(PDFLibrary:IQuickPDF; Fremd:Boolean{28.11.13} );//03.11.12
//wie zuvor und speichert unter selben Dateinamen!
Procedure AddVVGlHintAndSave(FileName:Shortstring; Fremd:Boolean{28.11.13} );//03.11.12
//Sendet ein VV an Glùubiger per EGVP / eBO, sonst E-Mail:
Procedure VVanGl(EGVP:Boolean{sonst email};EMail{14.05.13}:Shortstring;Sache,Az:AnsiString;DR:Nr_Typ;FertigesPDF:Shortstring);

Procedure VVEinlieferung(DR__:Nr_Typ;
                         Bezirksnummer:Shortstring;//19.03.13
                         A:AdressenTyp;
                         ANotFound:Boolean;//21.03.13
                         ZPO802D:Boolean;
                         XJustizDatei,{fùr ggf. spùtere Nutzung}
                         VorgabeMsgID:Shortstring;//GUID
                         Page882c:boolean;//15.12.12
                         VorgabeDatum:Datumstyp;//13.04.14
                         Wait:Boolean;//31.07.14
                         aOptionalesDatum : PDatumstyp = NIL // BW 17.07.24
                         );overload;
                         //DR_II muss belegt sein!!
Procedure VVEinlieferung(DR__:Nr_Typ;
                         Bezirksnummer:Shortstring;//19.03.13
                         A:AdressenTyp;
                         ANotFound:Boolean;//21.03.13
                         ZPO802D:Boolean;
                         XJustizDatei,{fùr ggf. spùtere Nutzung}
                         VorgabeMsgID:Shortstring;//GUID
                         Page882c:boolean;//15.12.12
                         VorgabeDatum:Datumstyp//13.04.14
                         );overload;
                         //DR_II muss belegt sein!!
Procedure VVEinlieferung(DR__:Nr_Typ;
                         Bezirksnummer:Shortstring;//19.03.13
                         A:AdressenTyp;
                         ANotFound:Boolean;//21.03.13
                         XJustizDatei,{fùr ggf. spùtere Nutzung}
                         VorgabeMsgID:Shortstring;//GUID
                         Page882c:boolean//15.12.12
                         );overload;//01.11.12
                         //DR_II muss belegt sein!!
{;
                         //Overwrite aus VV!
                         Vorname,Rufname,Name,Alias,
                         Geb Name,GebDatum,GebOrt,GebLand,
                         Str,HsNr,PLZ,Ort,
                         HR,HRAG,Sitz:Shortstring;
                         Privat:Boolean}

Function GetVVGlHint3(elektron,Fremd:boolean):AnsiString;//20.11.13

var
  Dokumentenname : String; // BW 24.11.22

implementation

{$R *.dfm}

uses
  wdek
, basis
, wbasis
, ev
, emu
, speziell
, loadstd
, eAktenSystem.Intf
, eAktenSystem.D2007
, wBLCom
, DRII_IO
, IniFiles
, PDFUtil.Helper
, PDFUtil.View
, wfile
, wspooler
, winsys
, spez2
, wleiste
, synpdf
, Printers
, w882c
, driiutil
, wemail
, wcopyfile
, form
, wpopupmodal
, kunden
, spooler
, wvorschau3
, wtermineallg
, protoein
, XEComp
, DRII
, menuesys
, wRegCode
, driizust
, wdrii2
, utilitie
, sicher
, xmlZeug
, wSupportstellen // FL 04.03.23
, Dialogs.Intf
, wDokumentenAblage
, wDRIIKostenrechnung
, wverfahren
, uxJustizVersion
;

Function GetVVGlHint3;//20.11.13
begin
 try
  Result:=GetMakeStr+', '+(Crunch(BezRec[AktBezRec].UserText[2])+' '+Crunch(BezRec[AktBezRec].UserText[1]));

  if elektron then
   Result:=Result+' gez. Unterschr., da elektron. ùbermittelt'
  else

   Result:=Result+' _____________________ (Unterschr. entfùllt bei elektron. ùbermittlung)';
 except
  Result:='';
 end;
end;

Procedure AddVVGlHint;
begin
 try
  //tja, wenn das PDF von Dritten kommt, wùre eine Seite vorweg besser!
  PDFLibrary.NormalizePage(0); // BW 09.05.23 
  PDFLibrary.SelectPage(1);
  PDFLibrary.SetMeasurementUnits(0);
  PDFLibrary.AddStandardFont(0);
  PDFLibrary.SelectFont(0);//Courier, 8=TimesRoman
  PDFLibrary.SetTextSize(8);

  //17.04.13:
  PDFLibrary.SetLineColor(10,10,10);
  PDFLibrary.SetFillColor(10,10,10);

//klappt nicht wie gewollt:  PDFLibrary.DrawRotatedBox(35,60,20,700,90,1);   //function TDebenuPDFLibrary0911.DrawRotatedBox(Left, Bottom, Width, Height, Angle: Double; DrawOptions: Integer): Integer;
{20.11.13
  PDFLibrary.DrawBox(5,60+730,21,730,2);
  PDFLibrary.DrawRotatedText(15,60,90,  //X, Y,  Angle
                             VVGlHint1);//'Vermerk fùr den Glùubiger: Dieser Abdruck des Vermùgensverzeichnis stimmt mit dem Inhalt des ausgefùllten Vermùgensverzeichnisses ùberein.');
  PDFLibrary.DrawRotatedText(25,60,90,  //X, Y,  Angle
                             VVGlHint2);//'ù 802d Abs.1 S. 3 ZPO: Der Glùubiger darf die erlangten Daten nur zu Vollstreckungszwecken nutzen und hat die Daten nach Zweckerreichung zu lùschen.');
}

{hmmm
  //28.11.13:
  if Fremd then begin
   PDFLibrary.SetTextSize(7);
   PDFLibrary.DrawBox(1,10+800,21,800,2);
   PDFLibrary.DrawRotatedText(15,10,90,  //X, Y,  Angle
                              VVGlHint1Fremd);
   PDFLibrary.DrawRotatedText(25,10,90,  //X, Y,  Angle
                              VVGlHint2Fremd+' '+GetVVGlHint3(false,fremd));
  end
  else} begin

   //20.11.13:

   PDFLibrary.DrawBox(1,60+730,44,730,2);

   PDFLibrary.DrawRotatedText(15,60,90,  //X, Y,  Angle
                              VVGlHint1);//'Vermerk fùr den Glùubiger: Dieser Abdruck des Vermùgensverzeichnis stimmt mit dem Inhalt des ausgefùllten Vermùgensverzeichnisses ùberein.');
   PDFLibrary.DrawRotatedText(25,60,90,  //X, Y,  Angle
                              VVGlHint2);//'ù 802d Abs.1 S. 3 ZPO: Der Glùubiger darf die erlangten Daten nur zu Vollstreckungszwecken nutzen und hat die Daten nach Zweckerreichung zu lùschen.');
   PDFLibrary.DrawRotatedText(44,60,90,  //X, Y,  Angle
                              GetVVGlHint3(false,fremd));//'ù 802d Abs.1 S. 3 ZPO: Der Glùubiger darf die erlangten Daten nur zu Vollstreckungszwecken nutzen und hat die Daten nach Zweckerreichung zu lùschen.');

  end;

{17.04.13
  PDFLibrary.DrawRotatedText(35,60,90,  //X, Y,  Angle
                             'Vermerk fùr den Glùubiger: Dieser Abdruck des Vermùgensverzeichnis stimmt mit dem Inhalt des ausgefùllten Vermùgensverzeichnisses ùberein.');
  PDFLibrary.DrawRotatedText(45,60,90,  //X, Y,  Angle
                             'ù 802d Abs.1 S. 3 ZPO: Der Glùubiger darf die erlangten Daten nur zu Vollstreckungszwecken nutzen und hat die Daten nach Zweckerreichung zu lùschen.');
}
 except
  on EXC_:Exception do
   ExceptionHintUser0('05311',EXC_.Message,EXC_.ClassName);
 end;
end;

Procedure AddVVGlHintAndSave;
Var PDFLibrary:IQuickPDF;
begin
 try
   PDFLibrary := TQuickPDF.Create(false);

   PDFLibrary.LoadFromFile(filename,'');
   AddVVGlHint(PDFLibrary,Fremd);
   try
    PDFLibrary.SaveToFile(filename);
   except
    TDialogs.MyMessagedlgOK('Die Datei '+filename+' konnte nicht gespeichert werden. Vielleicht haben Sie diese noch geùffnet?',mtwarning,[mbabort],0);//20.11.13
   end;
 except
    on EXC_:Exception do
     ExceptionHintUser0('05312',EXC_.Message,EXC_.ClassName);
 end;
end;

Function GetBodyVVanGl(DR:Nr_Typ;Az,Sache:Shortstring):AnsiString;//25.06.13
begin
 try
  Result:=(Crunch(BezRec[AktBezRec].UserText[1])+' '+Crunch(BezRec[AktBezRec].UserText[2]));
  Result:='Sache: '+Sache+#13#10+
        Az+
        'Mein Zeichen: '+DRToString(DR)+#13#10#13#10+
        'Sehr geehrte Damen und Herren,'#13#10#13#10+
        'anbei erhalten Sie das ausgefùllte Vermùgensverzeichnis ggf. mit Anlagen wie dieses beim zentralen Vollstreckungsgericht hinterlegt wurde.'#13#10#13#10+
        'Mit freundlichen Grùùen'+#13#10+
        Result;
 except
  Result:='';
 end;
end;

Procedure VVanGl(EGVP:Boolean{sonst email};EMail{14.05.13}:Shortstring;Sache,Az:AnsiString;DR:Nr_Typ;FertigesPDF:Shortstring);
var 
  body      : Ansistring;
  tempfile  : Shortstring;
  lAnlagen,
  lVV       : String; // BW 27.02.23
  i         : integer;
  SpoolHeader : tSpoolHeader;
begin
 try
  if Az>'' then
   Az:='Ihr Zeichen: '+Az+#13#10;

  tempfile:=TempPath+'VVGl'+inttostr(GetWorkstationNr)+'.PDF';                                      { TODO 2 -oXEFrank -cS_2_Short : XE }
  Body:=GetBodyVVanGl(DR,Az,Sache);

  if EGVP then begin

  // BW 27.02.23
  for i := High(FertigesPDF) downto 0 do
    begin
      if FertigesPDF[i] = ';' then
        break;
    end;

  lAnlagen := Copy(FertigesPDF,0,i);   // BW 27.02.23
  lVV      := Copy(FertigesPDF,i+1);   // BW 27.02.23

   CopyRenameSilent(lVV,tempfile);
   AddVVGlHintAndSave(tempfile,false);//Hinweis fùr Gl. einfùgen

   tempfile := Trim(lAnlagen + tempfile);   // BW 27.02.23                                          { TODO 2 -oXEFrank -cS_2_Short : XE }

   InitSpoolHeader(SpoolHeader, eDRII, 'Abgenommene Vermùgensauskunft', DR.DR, DR.Jahr );//kein DR etc.
   EGVP_MakeMail(true,'',//'',//Empf
                 'Abgenommene Vermùgensauskunft',//Betreff
                 Body,//Msg
                 DRToString(DR),
                 Az,
                 tSignatur(GlSig-1),//02.12.14 tSignatur(GlSig),
                 tempfile,//25.06.13 FertigesPDF,
                 EGVPMsgTESTONLY,
{// BW 27.02.23 Ticket #1495                 (DIP[88] and 128=128),//11.12.13} TRUE,
                 DR,false,
                 false,DLeer, NIL, SpoolHeader,
                  true // BW 27.02.23 #XGVServ-1356
                 );
//11.12.13   DeleteFile(tempfile);
   ioresult;
  end
  else begin
   //Hinweis fùr Gl. wird autom. eingefùgt!
   InitSpoolHeader(SpoolHeader, eDRII, 'Abgenommene Vermùgensauskunft', DR.DR, DR.Jahr );//kein DR etc.
   Wemaile(FertigesPDF,
           DR,False,
           EMail, //14.05.13 '',//VorgabeeMail,
           'Abgenommene Vermùgensauskunft '+DRToString(DR),//VorgabeSubj:Shortstring;
           '',//VorgabeAnlage:AnsiString;//16.08.12 Shortstring
           Body,
           False,False,False,//AutoStart,EditMode,_DelCollect
           true,true, SpoolHeader);//sign+crypt
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05313',EXC_.Message,EXC_.ClassName);
 end;
end;

Procedure VVanDritt(EGVP:Boolean{sonst email};SAFEID:Shortstring;Sache,Az:AnsiString;DR:Nr_Typ;FertigesPDF:Shortstring);
var body:ansistring;
    tempfile:shortstring;
    SpoolHeader : tSpoolHeader;

 function Txt2HTML(s:ansistring):ansistring;//04.08.14
 begin
  result:=stringreplace(s,#13#10,'<br>',[rfreplaceall]);                                            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
 end;

begin
 try
  if Az>'' then
   Az:='Ihr Zeichen: '+Az+#13#10;

  tempfile:=TempPath+'VVGl'+inttostr(GetWorkstationNr)+'.PDF';                                      { TODO 2 -oXEFrank -cS_2_Short : XE }
  Body:='Sache: '+Sache+#13#10+
        Az+
        'Mein Zeichen: '+DRToString(DR)+#13#10#13#10+
        'Sehr geehrte Damen und Herren,'#13#10#13#10+
        'anbei erhalten Sie das ausgefùllte Vermùgensverzeichnis ggf. mit Anlagen wie dieses beim zentralen Vollstreckungsgericht hinterlegt wurde.'#13#10#13#10+
        'Mit freundlichen Grùùen'+#13#10+
        (Crunch(BezRec[AktBezRec].UserText[1])+' '+Crunch(BezRec[AktBezRec].UserText[2]));

  if EGVP then begin
   CopyRenameSilent(FertigesPDF,tempfile);
   AddVVGlHintAndSave(tempfile,false);//Hinweis fùr Gl. einfùgen
   InitSpoolHeader(SpoolHeader, eDRII, 'Abgenommene Vermùgensauskunft', DR.DR, DR.Jahr );
   EGVP_MakeMail(true,SAFEID,//'',//Empf
                 'Abgenommene Vermùgensauskunft',//Betreff
                 Txt2HTML(Body),//Msg
                 DRToString(DR),
                 Az,
                 tSignatur(GlSig-1), //02.12.14 tSignatur(GlSig),
                 tempFile,//25.06.13 FertigesPDF,
                 EGVPMsgTESTONLY,
{// BW 27.02.23 Ticket #1495                 (DIP[88] and 128=128),//11.12.13} TRUE,
                 DR,false,
                 false,DLeer, NIL,
                 SpoolHeader,
                 TRUE // BW 28.02.23 Damit das PDF Util-Fenster aufgeht. SOnst fùhrt es zu Verwirrung
                 );
//11.12.13   DeleteFile(tempfile);
   ioresult;
  end
  else begin
  //Hinweis fùr Gl. wird autom. eingefùgt!
   InitSpoolHeader(SpoolHeader, eDRII, 'Abgenommene Vermùgensauskunft', DR.DR, DR.Jahr );//kein DR etc.
   Wemaile(FertigesPDF,
           DR,False,
           '',//VorgabeeMail,
           'Abgenommene Vermùgensauskunft '+DRToString(DR),//VorgabeSubj:Shortstring;
           '',//VorgabeAnlage:AnsiString;//16.08.12 Shortstring
           Body,
           False,False,False,//AutoStart,EditMode,_DelCollect
           true,true, SpoolHeader);//sign+crypt
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05314',EXC_.Message,EXC_.ClassName);
 end;
end;

Procedure DoPrintPDFFile;//30.10.12;
var c, Scale, Rotate : Integer;
   PDFLibrary:IQuickPDF;
   newname:shortstring;
begin
 //11.10.13:
 if not(FileExists(name)) then begin
  TDialogs.MyMessagedlgOK('~Druck nicht mùglich!'#13+
               'Die Druckdatei '+name+' konnte nicht gedruckt werden, da nicht existent!'#13#13+
               'Sollte es sich z.B. um ein VermVerz handeln, wurde vielleicht auf einem anderen PC das VermVerz erstellt, aber nicht die Daten zwischen den PCs synchronisiert.'#13+
               'Zur Not kann auch stets das VermVerz vom Vollstreckungsportal neu geholt werden.',
               mtwarning,[mbabort],0);
  exit;
 end;

 pdflibrary:=nil;//06.08.13
 abort:=FALSE;
 try
   PDFLibrary := TQuickPDF.Create(false);
   if PDFLibrary<>NIL then begin
    PDFLibrary.LoadFromFile(name,'');
    PDFLibrary.SelectPage(1);
    Scale  := 1; // 0 = None; 1 = Fit to paper,2 = Shrink large pages.
    Rotate := 1; // 0 = Do not rotate pages automatically ; 1 = Rotate pages to fit on the output medium, and center on the page

    if HintGl then begin
     AddVVGlHint(PDFLIBRARY,false);
    end;

    //21.05.13:
    if PDFSplPrint then begin
     newname:=temppath+'PDFDruck'+inttostr(GetWorkstationNr)+'.pdf';                                { TODO 2 -oXEFrank -cS_2_Short : XE }
     PDFLibrary.SaveToFile(newname);
     PDF2Spool(newname,false,LeerDR,1,'Dokument '+ExtractFileName(Name))                            { TODO 2 -oXEFrank -cS_2_Short : XE }
    end
    else begin

      try
       PrintDialog1.MaxPage  := PDFLibrary.PageCount;
       PrintDialog1.MinPage  := 1;
       PrintDialog1.FromPage := 1;
       PrintDialog1.ToPage   := PrintDialog1.MaxPage;
       if not(FirstFile) or PrintDialog1.Execute() then begin
        for c:=1 to PrintDialog1.Copies do//10.10.12
         if PrintDialog1.PrintRange=prPageNums then
          PDFLibrary.PrintDocument(Printer.Printers[Printer.PrinterIndex],PrintDialog1.fromPage,PrintDialog1.toPage,
                                   PDFLibrary.PrintOptions(Scale,Rotate,''))//22.08.12
         else
          PDFLibrary.PrintDocument(Printer.Printers[Printer.PrinterIndex],PrintDialog1.MinPage,PrintDialog1.MaxPage,
                                   PDFLibrary.PrintOptions(Scale,Rotate,''))
       end
       else
        abort:=TRUE;
      finally
      end;
    end;
   end;
 except
   on EXC_:Exception do
     ExceptionHintUser0('05315',EXC_.Message,EXC_.ClassName);
 end;
end;

Procedure TfVVEinlieferung.PrintPDFFile(name:Shortstring;FirstFile,HintGl:Boolean;Var abort:boolean);//27.02.13
begin
 DoPrintPDFFile(PrintDialog1,name,FirstFile,HintGl,abort);
end;

Procedure MakePDFDummy(Dateiname:Shortstring);
var
 lPdf   : TPdfDocumentGDI;// : TPdfDocument;
//11.08.13  lPage  : TPdfPage;
begin
  lPdf := TPdfDocumentGDI.Create; //TPdfDocument.Create;
  try
    lPdf.PDFA1                := false; // Ist kùrzer

    lPdf.Info.Author          := (Crunch(Crunch(BezRec[AktBezRec].UserText[2]))+' '+Crunch(BezRec[AktBezRec].UserText[1]) +
                                          ' aus ' + Crunch(GetGVOrt) );

    lPdf.Info.CreationDate    := Now;
    lPdf.Info.Creator         := 'GVService - Software fùr Gerichtsvollzieher';
    lPdf.Info.Title           := 'Gelùschtes Vermùgensverzeichnis';
    lPDF.EmbeddedTTF          :=TRUE;
    lPDF.StandardFontsReplace :=FALSE;
    lPDF.EmbeddedWholeTTF     :=FALSE;
    lPdf.Info.Subject         := 'Gelùscht';;
    lPdf.DefaultPaperSize     := psA4;
    lpdf.ScreenLogPixels      :=300;

    //11.08.13 lPage :=
    lPDF.AddPage;

    lPDF.Canvas.SetFont('Verdana',12.0,[]);
    lPDF.Canvas.SetLeading(lPDF.Canvas.Page.FontSize);
    lPDF.Canvas.SetLineWidth(0.1);

    // PDF Zugriff...
    lPdf.Canvas.BeginText;
    lPdf.Canvas.TextOut( 50, 800,  'GVService');
    lPdf.Canvas.TextOut( 50, 700,  'Dieses Vermùgensverzeichnis wurde gelùscht am: '+FormatDateTime('dd.mm.yyyy (hh:nn:ss ',Now)+'Uhr)');  { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    lPdf.Canvas.EndText;

    lPdf.SaveToFile(Dateiname);
  finally
    lPdf.Free;
  end;
end;

Procedure VVEinlieferung(DR__:Nr_Typ;
                         Bezirksnummer:Shortstring;//19.03.13
                         A:AdressenTyp;
                         ANotFound:Boolean;//21.03.13
                         XJustizDatei,VorgabeMsgID:Shortstring{fùr ggf. spùtere Nutzung};Page882c:boolean
                         );overload;//01.11.12
var LeerDatum:Datumstyp;
begin
 fillchar(LeerDatum,sizeof(LeerDatum),0);
 VVEinlieferung(DR__,
                Bezirksnummer,//19.03.13
                A,
                ANotFound,//21.03.13

                FALSE,

                XJustizDatei,VorgabeMsgID,Page882c,LeerDatum,false);
end;

Procedure VVEinlieferung(DR__:Nr_Typ;
                         Bezirksnummer:Shortstring;//19.03.13
                         A:AdressenTyp;
                         ANotFound:Boolean;//21.03.13
                         ZPO802D:Boolean;
                         XJustizDatei,VorgabeMsgID:Shortstring{fùr ggf. spùtere Nutzung};Page882c:boolean;
                         VorgabeDatum:Datumstyp//13.04.14
                        );overload;
begin
 VVEinlieferung(DR__,
                Bezirksnummer,//19.03.13
                A,
                ANotFound,//21.03.13

                FALSE,

                XJustizDatei,VorgabeMsgID,Page882c,VorgabeDatum,false);
end;

Procedure VVEinlieferung(DR__:Nr_Typ;
                         Bezirksnummer:Shortstring;//19.03.13
                         A:AdressenTyp;
                         ANotFound:Boolean;//21.03.13
                         ZPO802D:Boolean;
                         XJustizDatei,{fùr ggf. spùtere Nutzung}
                         VorgabeMsgID:Shortstring;//GUID
                         Page882c:boolean;//15.12.12
                         VorgabeDatum:Datumstyp;//13.04.14
                         Wait:Boolean;//31.07.14
                         aOptionalesDatum : PDatumstyp = NIL // BW 17.07.24
                         );overload;
                         //DR_II muss belegt sein!!
var //06.08.13 WS : TWindowState;
     E : tVVEinliefer;
    sr : tsearchrec;
    ss : shortstring;
     h : integer;
    SAV : dr_II_typ;
begin
 try

  //25.06.13:
  if (DR__.DR<>0) and not(DRIIExist(DR__.DR,DR__.Jahr,DRIIJahre,true)) then begin
   WFehler('DR Nr. existiert nicht!',2);
   Exit;
  end;
  
  SetXJustizVVSV; // FL 25.10.22

//  if not(SetXJu stizVVSV) and (User_Status=0) then // FL 02.05.16
//    TDialogs.MyMessagedlgOK('Fehlerhafte XJustiz Version! Dringend Programmierer melden!',mterror,[mbabort],0);

try
  with TfVVEinlieferung.create(application) do begin
   AdvOfficePager1.ActivePage := Grundlagen; // FL 14.08.17

   if Assigned(aOptionalesDatum) and (aOptionalesDatum.Tag > 0) then
     OptionalesDatumVVQuittung.Fill(aOptionalesDatum);

   CloseOK:=FALSE;//31.07.14
   FormVVEinlieferaktiv:=true;//21.04.14
   INOK2:=false;//07.04.14

   f755802l1:=Tf755802l.create(Angaben);
   f755802l1.parent:=Angaben;

   f755802l1.kbapanel.visible:=false;//08.04.14
   f755802l1.mrapanel.visible:=false;//28.02.16
   f755802l1.mrapanel2.visible:=false;//28.02.16
   RettUse_Name:='';//29.05.15

   NoSa.checked:=DIP[94] and 1=1;//24.08.15
   DruckNachweis.Checked:=DIP[111] and 32=0;//05.11.24

   {if Land=_NR then
    ZenVGTransfer.enabled:=true;}

   //20.04.16:
   fFrameOffeneAkten1:=tfFrameOffeneAkten.Create(Panel13);
   fFrameOffeneAkten1.Parent:=Panel13;

   SetAdvOfficePagerSelectedTab(AdvOfficePager1);//11.08.13

   Proto.Checked      :=DIP[89] and 1=1;//18.03.14, war doppelt belegt!!! 86] and 2=2;//02.07.13
   ProtoVAKVfg.Checked:=DIP[89] and 2=2;//18.03.14 86] and 4=4;//02.07.13

   KopieGl.Checked    :=DIP[92] and 1=0;//30.12.14

   //18.08.13:
   KopieSchuldnerDirekt.Checked:=(DIP[87] and 2=2);
   KopieAkte.Checked           :=(DIP[87] and 4=4);
   KopieS.Checked              :=(DIP[87] and 8=8);
   KopieAkte2.Checked          :=(DIP[87] and 16=16);

//13.08.15 ?? Was auch immer   OwnLabel13.Visible:=Land<>_BA;//14.10.13

   //10.07.13:
   ImTermin.Checked  := DIP[86] and 2=0;
   KeineFrist.Checked:= DIP[86] and 4=4;

   Vorbuch.Checked   := DIP[91] and 2=2;//15.08.14

   //19.04.14:
   JetztSV.Checked   := DIP[89] and 64=64;
   if JetztSV.Checked then
    JetztSVClick(JetztSV);

{NEIN, nur nùtig, wenn VAK gedownloaded!
   Proto2.Checked:=DIP[86] and 8=8;//02.07.13
   ProtoVAKVfg2.Checked:=DIP[86] and 8=8;//02.07.13}

   caption:=DRToString(DR__)+' '+caption;//19.04.13

   caption:=caption+' ('+xJustizDaten.XJustizVersion+')'; // FL 23.11.22

   {07.12.12
   tùtlich, wenn user selbst minimiert, fùhrt zudem zu erheblichen problemen im ablauf
   WS := WindowState;
   WindowState := wsMinimized; // FL 17.10.12 Keine halben Fenster in der Anzeige!
   }
   ChkExit:=FALSE;//18.04.13
   ENABLED:=FALSE;
   h:=Height;
   Height:=0;
   _ANotFound:=ANotFound;//21.03.13

   Dwl.Visible:=false;//21.02.13 (User_Status=0) or AllowTester;//06.02.13

   try

    //13.04.14 fùr Nachebsserung
    if VorgabeDatum.Tag>0 then
     VerfNrDat.Text:=makestr(VorgabeDatum)
    else

     VerfNrDat.Text:=GetMakeStr;//15.01.13

    //13.11.12:
    HintCall.Font.Style:=[fsbold];
    HintCall.Visible:=VorgabeMsgID='';
    //ToDo Sonst VermVerz komplett suchen und fragen, um welches es geht!!! Ne, geht auch nicht, weil keine Zuordnung...

    PutWarte('Lese die gespeicherten Vermùgensauskùnfte bitte warten...');      // FL 17.10.12
    Application.ProcessMessages;

    try
     VorgabeID:=VorgabeMsgID;//04.11.12
     OnePDFDone:=FALSE;
     Sache:='';
     Az:='';
     DR:=DR__;
     S_Adresse :=A;
     Caption:=Caption+' - '+DRToString(DR);

     //20.04.14:
     try
      Obj:=tObj.Create;
      Obj.A:=S_Adresse;
      fFrameOffeneAkten1.AusschlussDRI:=false;
      fFrameOffeneAkten1.AusschlussDR:=DR;

      fFrameOffeneAkten1.Init(Obj.A,Obj.GetDRNrn,Obj.SetDRNrn);
      fFrameOffeneAkten1.ShowAkten;
     except
      on EXC_:Exception do
       ExceptionHintUser0('05316',EXC_.Message,EXC_.ClassName);
     end;

     //24.08.14:
     VVListeDatNamen.Items.BeginUpdate;
     VVListe.Items.BeginUpdate;

     //04.11.12:
     VVListeDatNamen.Items.Clear;
     VVListe.Items.Clear;
     if VorgabeMsgID>'' then begin
      ChkExit:=TRUE;//18.04.13
      AdvOfficePager1.ActivePage:=bestaetigt;
      Grundlagen.TabVisible:=FALSE;
      Angaben.TabVisible:=FALSE;
      VVListeDatNamen.Items.Add(EVPath+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+'_'+VorgabeMsgID+'.PDF'); // FL 13.12.19
      FindVV(E,DR,VorgabeMsgID);//01.11.12
      if E.Datum.Tag>0 then
       VVListe.Items.Add('VermVerz vom '+MakeStr(E.Datum)+' '+(GetAdresseFullName(E.S)))
      else
       VVListe.Items.Add('VermVerz '+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+'_'+VorgabeMsgID+'.PDF'); // FL 13.12.19
      VVListe.Enabled:=FALSE;
      try
       VVListe.ItemIndex:=0;
      except
       on EXC_:Exception do
        ExceptionHintUser0('05317',EXC_.Message,EXC_.ClassName);
      end;
     end

     //15.12.12
     else
     if Page882c then begin
      AdvOfficePager1.ActivePage:=bestaetigt;
      Grundlagen.TabVisible:=FALSE;
      Angaben.TabVisible:=FALSE;
{      VVListeDatNamen.Items.Add(EVPath+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+' '+VorgabeMsgID+'.PDF');
      FindVV(E,DR,VorgabeMsgID);//01.11.12
      if E.Datum.Tag>0 then
       VVListe.Items.Add('VermVerz vom '+MakeStr(E.Datum)+' '+(GetAdresseFullName(E.S)))
      else
       VVListe.Items.Add('VermVerz '+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+' '+VorgabeMsgID+'.PDF');}
      VVListe.Enabled:=FALSE;
      if VVListe.Items.Count>0 then//04.02.13
       try
        VVListe.ItemIndex:=0;
       except
        on EXC_:Exception do
         ExceptionHintUser0('05318',EXC_.Message,EXC_.ClassName);
       end;
     end

     else begin

      AdvOfficePager1.ActivePage:=Grundlagen;

      //04.11.12:
      if findfirst(EVPath+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+'_GVS*.pdf',faanyfile,sr)=0 then begin // FL 13.12.19
       repeat
        ss:=copy(sr.Name,pos('GVS',sr.name),200);                                                   { TODO 2 -oXEFrank -c'TFileName' zu 'ShortString' : XE }
        if pos('.',ss)>0 then
         delete(ss,pos('.',ss),200);
        //ss jetzt GUID!
        VVListeDatNamen.Items.Add(EVPath+sr.name);
        FindVV(E,DR,ss);//01.11.12
        if E.Datum.Tag>0 then
         VVListe.Items.Add('VermVerz vom '+MakeStr(E.Datum)+' '+(GetAdresseFullName(E.S)))
        else
         VVListe.Items.Add('VermVerz '+sr.name);
       until findnext(sr)<>0;
       findclose(sr);
       try
        VVListe.ItemIndex:=0;
       except
        on EXC_:Exception do
         ExceptionHintUser0('05319',EXC_.Message,EXC_.ClassName);
       end;
      end
      else begin
       ShowPDF.Enabled:=FALSE;
       ColorBitButton1.Enabled:=FALSE;
       Label14.Visible:=TRUE;
      end;

      //06.02.17:
//      try  // FL 14.08.17 Weg Hier
//       VerfNrDat.SetFocus;//Exception, klappt aber
//      except
//      end;

     end;

     //24.08.14:
     VVListeDatNamen.Items.EndUpdate;
     VVListe.Items.EndUpdate;

     if ZPO802D then
      Erneut.Checked:=true;
     with f755802l1 do begin
      ScrollBox1.VertScrollBar.Position:=0;//10.10.12

      DRNummer:='DR II '+DRToString(DR); // FL 30.11.22 wieder mit DR II  //22.05.13
      
      //06.05.13:
      DR_:=DR;
      Bezirk_:=Bezirksnummer;

      SetXJustizVVSV; // 04.05.15

      ForVV := true;
      AuslandWohnsitz.Visible := false;
      ForEAO:=false;//17.07.15
      FrameInit([StoerungVV,StoerungEGVP,StoerungSafe]);
      InView(false); // 17.10.12 Damit der Timer nicht lùuft
      Init;
  // FL 14.10.12 NIE Lese aus VV    Adr2Anfrage(A);
      BelegeEingaben := true;
      LocalAdr:=A;
     end;

     //04.11.12:
     try
      if ZVAntragElementChecked(DR_II,DR,'P4') then // DR_II.Be antragt2 and 1=1 then
       if (DR_II.Glaubiger.SafeID>'') or (DR_II.Glaubiger_V.SafeID>'') then
        KopieGlArt.ItemIndex:=0//EGVP
       else
        KopieGlArt.ItemIndex:=1//EMAIL
      else
      //11.12.12:
      if MerkKopieVVWeg<>-1 then
       KopieGlArt.ItemIndex:=MerkKopieVVWeg
      else

       KopieGlArt.ItemIndex:=2;//Post

      //11.12.12:
      if MerkVAK3Weg<>-1 then
       VAK3Art.ItemIndex:=MerkVAK3Weg
      else

       VAK3Art.ItemIndex:=2;//Post

     except
      on EXC_:Exception do
       ExceptionHintUser0('05320',EXC_.Message,EXC_.ClassName);
     end;

     // BW 24.11.22
     if Dokumentenname <> ''
       then FertigesPDF := EVPath+Dokumentenname
(* BW Kommt mit einem spùteren Ticket erst. SInd vorbereitungen
       else begin
              if VorgabeDatum.Tag > 0
                then FertigesPDF := EVPath+StrWordNull(KorrektJahr(DR.Jahr),2) + '-' + StrWord(DR.DR,1)+'_'+ 'VAK ' + VorgabeDatum.ToStringMonatAlsText
                else FertigesPDF := EVPath+StrWordNull(KorrektJahr(DR.Jahr),2) + '-' + StrWord(DR.DR,1)+'_'+ 'VAK ' + FormatDateTime('DD.MMM.YYYY', now);

              lBuff := FertigesPDF + '.pdf';
              i := 0;

              While FileExists(lBuff) do
                begin
                  Inc(i);
                  lBuff := FertigesPDF + ' (' + IntToStr(i) + ')'  + '.pdf';
                end;

              FertigesPDF := lBuff;
            end;    *)
//     FertigesPDF:= EVPath+'EV_A_'+StrWord(DR.DR,1)+'_'+StrWordNull(korrektJahr(DR.Jahr),2)
{      VV an ZenVG '+
                   StringReplace(StringReplace(trim(DRNr.Text),'/','_',[]),
                                 ' ','',[rfreplaceall])+'.pdf';}
    else FertigesPDF:=EVPath+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+                               { TODO 2 -oXEFrank -cS_2_Short : XE }
                                            StrWord(DR.DR,1)+'_VAK_'+MyGUID // FL 04.12.19 kein Space erlaubt
{                  Trim(MakeStr(Datum)+' '+
                  StringReplce(Ma keStr0(Zeit),':','-')+
                  ' '+OneVVPDFName0}+'.pdf';
  // FL 17.10.12
     SearchVV;

     //18.04.13:
     KopieGlClick(Application);
     DrittGlClick(Application);

     EintragSVpersClick(Application);//08.05.13

    finally
      DelWarte;
    end;
   except
    on EXC_:Exception do Catchme(EXC_.Message,EXC_.ClassName,'VVE 3');
   end;

   try
//07.12.12    WindowState := WS;
    ENABLED:=TRUE;

    try
     if VerfNrDat.CanFocus then//03.07.20
      VerfNrDat.SetFocus;  // FL 14.08.17 nach dem Enabled...
    except
     on EXC_:Exception do
      ExceptionHintUser0('05321',EXC_.Message,EXC_.ClassName);
    end;

    Height :=h;
    BringTofront;//19.12.12

    //31.07.14:
    DoWait:=Wait;
    if Wait then begin
     SAV:=DR_II;//26.10.17
     repeat
      application.processmessages;
      Sleep(50);
     until CloseOK;
     DR_II:=SAV;//26.10.17
     Close;
    end;

   except
    //06.07.21 15:27:57, 30.0 i3310, #3635, CatchMe VVE 2, Laufzeit 4:12
    CatchMe('VVE 2');
   end;

  end; // With
except
  On e : Exception do WFehler(E.Message,2);
end;
  except
   CatchMe('VVE 1');
  end;
end;

procedure TfVVEinlieferung.CheckBox1Click(Sender: TObject);
begin
 //11.12.12:
 MerkKopieVVWeg:=KopieGlArt.ItemIndex;
 WriteStammdaten;
end;

procedure TfVVEinlieferung.CheckBox2Click(Sender: TObject);
begin
 //11.12.12:
 MerkVAK3Weg:=VAK3Art.ItemIndex;
 WriteStammdaten;
end;

  Function TfVVEinlieferung.GetTheLastVV:AnsiString;
  var
    i         : Integer;
    S         : AnsiString;
    Age,
    MaxAge    : Integer;
  begin
    MaxAge := -1;
    Result := '';
    try
      for i:=0 to Liste.Count - 1 do
        begin
          if Liste.Checked[i] then
            begin
              S :=  Uppercase(DateiNamen[i]);                                                       { TODO 2 -oXEFrank -cS_2_Ansi : XE }
              if (ExtractFileExt(S) = '.G-V') and ((Pos('EV_P',S)>0) or (Pos('EV_G',S)>0))then
                begin
       {$WARNINGS OFF}
                  Age := FileAge(DateiNamen[i]);
                         {$WARNINGS ON}

                  if Age > MaxAge  then
                    begin
                      Result   := Uppercase(DateiNamen[i]);                                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                      MaxAge   := Age;
                    end;
                end;
            end;
        end;
    except
     on EXC_:Exception do
      ExceptionHintUser0('05322',EXC_.Message,EXC_.ClassName);
    end;
  end;


procedure TfVVEinlieferung.CheckVV_and_LoadinForm;

  Function DelUnsinn(s:WIDEstring):WIDEstring;
  begin
   s:=WideStringReplace(s,'#U','',[rfreplaceall]);
   s:=WideStringReplace(s,'~U','',[rfreplaceall]);
   s:=WideStringReplace(s,'#G','',[rfreplaceall]);
   s:=WideStringReplace(s,'~G','',[rfreplaceall]);
   s:=WideStringReplace(s,'#F','',[rfreplaceall]);
   s:=WideStringReplace(s,'~F','',[rfreplaceall]);
   Result:=s;
  end;

  Function SearchLand(Land:String; out aStaat : TStaaten ):Integer;
  var
    lHelp : String;
  begin
    if xJustizDaten.StaatenHandler.FindCodeFromVVStaatText(Land,lHelp)
      then begin
             Result   := xJustizDaten.StaatenHandler.ItemindexFromCode(lHelp);
             aStaat.ItemIndex := Result; // FL 04.11.21
           end
      else begin
             Result   := -1;
             aStaat := TStaaten.Create('');
           end;
  end;

Var
  Use_Name,
  StrNr,
  PLZOrt,
  OT_,
//  AGS_,
  FullAdr,
  G,
  Help        : AnsiString;
  templ       : TStaaten; // FL 02.11.21 Str2;
  Load        : TIniFile;
  n           : Integer;
  dl:datumstyplong;
  W:WideString;
begin//CheckVV_and_LoadinForm;

  //29.05.15:
  if RettUse_Name>'' then
   Use_Name:=RettUse_Name
  else

   Use_Name := GetTheLastVV; // ich mach das jetzt mal so...;-)

  if trim(Use_Name) <> '' then
    begin

{      //15.01.13:
      valdate(d,GetEVDate(Use_Name));
      if d.tag<>0 then
       VerfNrDat.Text:=MakeStr(d);
       }

      Load := TInifile.Create(Use_Name);
      try
        with f755802l1 do
          begin
            if Pos('EV_P',Use_Name) > 0
              then begin // Privat
                     Priv.Checked := true;
                     f755802l1.Name.Text    := AnsiOrUTF8(Load.readString('Daten', 'Name',''));     { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     W:=AnsiOrUTF8(Load.readString('Daten', 'Vorname',''));                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     f755802l1.Vorname.Text := W;
                     WeitereNamen.Text := AnsiOrUTF8(Load.readString('Daten', 'WeitereNamen',''));//19.03.13{ TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     try
//19.03.13                       GebAm.Text := Load.readString('Daten', 'Geburtsdatum','');
                       G := Load.readString('Daten', 'Geburtsdatum','');                            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
{                       if Pos('.',G)>0 then begin
                        Val(Copy(G,1,Pos('.',G)-1,dl.tag,dek.error);}
                       //19.03.13:
                       ValDateLong(DL,G);
                       GebAm.Text := DatumToStringLong(DL);

                     except
                      on EXC_:Exception do
                       ExceptionHintUser0('05323',EXC_.Message,EXC_.ClassName);
                     end;
                     GebNam.Text  := AnsiOrUTF8(Load.readString('Daten', 'GebName',''));            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     GebIn.Text   := AnsiOrUTF8(Load.readString('Daten', 'gebOrt',''));             { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     StrNr        := Load.readString('Daten', 'Anschrift','');                      { TODO 2 -oXEFrank -cS_2_Ansi : XE }

                     Alias.text   := AnsiOrUTF8(Load.readString('Daten', 'Alias',''));              { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     WeitereNamen.Text:=AnsiOrUTF8(Load.readString('Daten', 'WeitereNamen',''));    { TODO 2 -oXEFrank -cS_2_Ansi : XE }

                     Rufname.text := AnsiOrUTF8(Load.readString('Daten', 'Eingabe12',''));//26.08.13{ TODO 2 -oXEFrank -cS_2_Ansi : XE }

                     Titel.Text   :=AnsiOrUTF8(Load.readString('Daten', 'Titel',''));               { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     Vorsatz.Text :=AnsiOrUTF8(Load.readString('Daten', 'Vorsatz',''));             { TODO 2 -oXEFrank -cS_2_Ansi : XE }

                     //25.08.13:
                     {
                     if Load.readString('Daten', 'CheckBox70','') = 'X' then begin
                      Sex.Itemindex := 2;
                      Anrede.ItemIndex := 0;
                     end
                     else}
                     // FL 02.01.23
                     SetSexAnAnrede(Load.readString('Daten', 'CheckBox48','') = 'X',
                                    Load.readString('Daten', 'CheckBox49','') = 'X',
                                    Load.readString('Daten', 'CheckBox70','') = 'X',
                                    Load.readString('Daten', 'CheckBox75','') = 'X');

                     { FL 02.01.23
                     if Load.readString('Daten', 'CheckBox48','') = 'X'
                       then begin
                              if Load.readString('Daten', 'CheckBox49','') = 'X'
                                then begin
                                       Sex.Itemindex := 2;
                                       Anrede.ItemIndex := 0;

                                     end
                                else begin
                                       Sex.Itemindex := 1;
                                       Anrede.Itemindex := 2;
                                     end;
                            end
                       else begin
                              if Load.readString('Daten', 'CheckBox49','') = 'X'
                                then begin
                                       Sex.Itemindex := 0;
                                       Anrede.Itemindex := 1;
                                     end
                                else begin
                                       Sex.Itemindex := 2;
                                       Anrede.ItemIndex := 0;
                                     end;
                            end;}

                     PLZOrt     := Load.readString('Daten', 'AnschriftOrt','');                     { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     OT_        := Load.readString('Daten', 'AnschriftOT','');//30.04.13            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     Str.Text   := Load.readString('Daten', 'Anschrift','');                        { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     HausNr.Text:= Load.readString('Daten', 'HausNr','');                           { TODO 2 -oXEFrank -cS_2_Ansi : XE }
//                     AGS_:= Load.readString('Daten', 'AnschriftAGS','');//26.02.16

                     //23.04.21: Auskunftssperre 
                     if (Str.Text='') and (PLZOrt='') and IstAuskunftssperre(S_Adresse) then begin
                      Str.Text    := S_Adresse.Anschrift.Strasse;
                      HausNr.Text := S_Adresse.Anschrift.HausNr; // FL 14.08.17
                      HausNr.Text := PrepHausNrXJustiz(HausNr.Text);//22.07.15

                      PLZ.Text    := crunch(S_Adresse.Anschrift.PLZ);
                      Ort.Text    := crunch((S_Adresse.Anschrift.Ort)); // 03.10.12
                      OT.Text     := crunch((S_Adresse.Anschrift.OT));  // 03.10.12
                      if trim(S_Adresse.Anschrift.AGS) <> '' then  // FL 27.20.16
                       AGSEdit.Text:=PrepAGS(S_Adresse.Anschrift.AGS);//26.02.16
                      Staat.ItemIndex:= xJustizDaten.StaatenHandler.ItemindexFromCode(S_Adresse.Anschrift.Staat); // FL 27.10.21 GetStaatindex(S_Adresse.Anschrift.Staat,Staat);
                     end
                     else begin
                      try
                        help := '';
                        n := 1;
                        while (length(PLZOrt) >= N) and (PLZOrt[n] in ['0'..'9']) do
                          Begin
                            help := Help + PLZOrt[n];
                            inc(n);
                          End;

                        Delete(PLZOrt,1,length(Help));
                        PLZOrt := trim(PLZOrt);                                                     { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                        PLZ.Text := Help;
                        Ort.Text := PLZOrt;
                        OT.Text  := OT_;//30.04.13
   //                     AGS.Text := AGS_;//26.02.16
                      except
                        PLZ.Text := '';
                        Ort.Text := '';
                        OT.Text  := '';//30.04.13
    //                    AGS.Text :='';//26.02.16
                      end;
                     end;

                     //24.08.13:
                     if Load.readString('Daten', 'Auskunftssperre','') = 'X'   //22.04.21 ist nicht besetzt
                       then f755802l1.CBAuskunftssperre.Checked := true     // FL 26.08.21
                       else f755802l1.CBAuskunftssperre.Checked := false;
// FL 28.06.21                      f755802l1.AuskunftssperreBtnClick(f755802l1);


                     //Doppelt? s.o.!
                     Rufname.Text := AnsiOrUTF8(Load.readString('Daten', 'Eingabe12',''));          { TODO 2 -oXEFrank -cS_2_Ansi : XE }

                     if Pos(trim(Rufname.Text),Vorname.Text) = 0 then
                      Vorname.Text := trim(trim(Rufname.Text) + ' '+Vorname.Text);

                     if Load.readString('Daten', 'DE','') = 'X' then
                      GebLand.Itemindex := 0
                     else
                      GebLand.Itemindex := SearchLand(Load.readString('Daten', 'GebLand',''),tempL);

                     //24.06.15:
                     if Load.readString('Daten', 'StaatsangehD','') = 'X' then
                      F755802l1.MerkStaatsangeh.ISO:='DE'
                     else
                      SearchLand(Load.readString('Daten', 'Staatsangeh',''),F755802l1.MerkStaatsangeh);

                     //16.04.13:
                     Vorname.Text:=DelUnsinn(Vorname.Text);
                     Rufname.Text:=DelUnsinn(Rufname.Text);
                     name.Text   :=DelUnsinn(name.Text);

                   end
              else // Gesellschft
              if Pos('EV_G',Use_Name) > 0 then begin//14.12.12
                     Jur.Checked := true;

                     F755802l1.Adr2Anfrage(S_Adresse);//14.12.12 Wegen Anschrift etc.!

                     Firmenname.Clear;
                     N := 0;
                     repeat
                       Help := inttostr(N);                                                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                       inc(N);
                       Help := AnsiOrUTF8(Load.readString('Daten', 'MP95'+'|'+Help,'~'));           { TODO 2 -oXEFrank -cS_2_Ansi : XE }  { TODO 2 -oXEFrank -cW_2_Ansi : XE }
                       if Help <> '~' then
                         Firmenname.Lines.Add(Help);
                     until Help='~';

                     HRNr.Text := Load.readString('Daten', 'Edit5','');                             { TODO 2 -oXEFrank -cS_2_Ansi : XE }

                     //14.12.12:
                     try
                       if RechtsformNeu.ItemIndex = -1 then // FL 11.01.22
                         RechtsformNeu.ItemIndex :=FindXJustizRechtsForm(Firmenname.text,RechtsformNeu.Items);{ TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }

                       help := Load.readString('Daten', 'Edit4','');                                { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                       if help>'' then
                        Gericht.ItemIndex:=FindMatchStr(Gericht.Items,{// BW 23.08.23 da kein Amtsgericht damit anfùngt';Amtsgericht '+}help);//; weil sonst "aufgelùst Amtsgericht" gefunden werden kùnnte!

                       if RegisterArtNeu.ItemIndex = -1 then // FL 11.01.22
                         RegisterArtNeu.ItemIndex:=FindMatchStr(RegisterArtNeu.Items,copy(HRNr.Text,1,3));

                       if (RegisterArtNeu.ItemIndex<>-1) and (SameText(trim(RegisterArtNeu.Items[RegisterArtNeu.ItemIndex]),trim(Copy(HRNr.Text,1,4)))) then // FL 11.01.22 SameText
                        HRNr.Text:=crunch(copy(HRNr.Text,4,200));

                        Sex.ItemIndex := -1; // BW 23.08.23
                     except
                      on EXC_:Exception do
                       ExceptionHintUser0('05324',EXC_.Message,EXC_.ClassName);
                     end;

                     //14.12.12 s.o., schon erledigt!
                     PLZOrt := Load.readString('Daten', 'Edit6','');                                { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                     Fulladr := '';

                     N := 0;
                     repeat
                       Help := inttostr(N);                                                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                       inc(n);
                       Help := Load.readString('Daten', 'MP1'+'|'+Help,'~');                        { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                       if Help <> '~' then
                         Fulladr := FullAdr + Help +';';
                     until Help='~';

                     // ggf. Split Fulladr

{ TODO -oFrank : DoGerichteSearch }//            Gericht.ItemIndex := DoGerichtSearch(Gericht.Items,Load.readString('Daten', 'Edit4',''));
                   end;
              end;
      finally
        Load.Free;
      end;
    end;
end;

procedure TfVVEinlieferung.ColorBitButton1Click(Sender: TObject);
var abort:boolean;
    code:char;
    ss:shortstring;
    E : tVVEinliefer;
    termin:ttermin;
    buff:longint;

 procedure Brief;
  var code:char;
  begin
   try

    if Frist1.Checked then begin
     fillchar(termin,sizeof(termin),0);
     termin.S:='Zahlungsfrist vor Eintragungsanordnung abgelaufen';
     termin.Magic:='882c';
     datumlesen(termin.datum);
     CheckFristen_UrlaubFeierTag(termin.datum,30,false,true);
     termin.REG  :=DRIIR;
     termin.DR   :=DR;
     termin.TerminWo:='GV-Bùro';
     termin.KundeNeu:=GetKey(S_Adresse,1);
     termin.AktionsNr:=102;
     Savetermin(termin,false,Buff);
    end;

    if DrittGl.Checked then
     ss:='882C3.TXT'
    else
     ss:='882C.TXT';

    FormbriefDATAExist    := True;
    FormbriefDATA^.AnzP[1] := 1;
    if Frist1.Checked then
     FormbriefDATA^.P_[1,1]:= 2
    else
     FormbriefDATA^.P_[1,1]:= 1;
    //FormBrief(10,ss,0,0,DR.DR,DR.Jahr,Code,True,False,true,true,false,_A,false);
//Wegen Pop up debuggen...
    LiesDRII(DR.jahr,DR.dr);//22.01.13
    FormBrief(10,ss,0,0,DR.DR,DR.Jahr,Code,
                false,//11.07.13 true, //usevorgabe
                False, //usef4
                false, //abfrageb
                true,//11.07.13 false, //dochabfragen
                false, //serienbrief
                0,     //autoanzahl
                true); //kein stampit
    modal_FormBriefWait;
    FormbriefDATAExist := False;
    Fillchar(formbriefdata^,sizeof(formbriefdata^),0);
   except
    on EXC_:Exception do
     ExceptionHintUser0('05325',EXC_.Message,EXC_.ClassName);
   end;
 end;

(*14.05.13
 procedure Weitere;//22.11.12
 var c:ansichar;
//14.05.13     DR:Nr_Typ;
     AA:tpa;
     y:integer;
 begin
  c:=#27;
  repeat

{*****
 Problem: Brief + PDF in einem Druckauftrag wegen EGVP/EMAIL/FAX Versand!
 *****}
//DR-Nummer
//Versandart
//bei EGVP Nutzer-ID
//dann Brief,VV
   initpopup(aa);
   for y:=0 to KopieGlArt.Items.Count-1 do
    aa^[y+1]:= KopieGlArt.Items[y];
   y := Pop Up(C,AA^,0,'Versandart '+DRToString(dr),KopieGlArt.ItemIndex+1,TRUE,FALSE,0);
   deinitpopup(aa);
   if c<>t_esc then begin
    inc(WeitereKopie);

   LiesDRII(DR.jahr,DR.dr);//22.01.13
   FormB rief(10,'VVGL13.TXT',0,0,DR.DR,DR.Jahr,Code,
              False, //usevorgabe
              False, //usef4
              false, //abfrageb
              false, //dochabfragen
              false, //serienbrief
              0,     //autoanzahl
              true); //kein stampit
   modal_Form BriefWait;

   end;
  until c=t_esc;
 end;
 *)

 procedure DoDritt;//14.05.13
 var AA:ta;
     y:integer;
     c:char;
     nr:nr_Typ;
     ok:boolean;
     EM:Shortstring;
     buff:shortstring;
     rFIRSTDRIIListe,//30.11.17
     rLastDRIIListe:nr_typ;//30.11.17
 begin

  //30.11.17:
  BUFF              := DRIIListenStr;
  rFIRSTDRIIListe   := FIRSTDRIIListe;
  rLastDRIIListe    := LastDRIIListe;

  if (FertigesPDF>'') then
   try

   //30.11.17:
   FIRSTDRIIListe.DR := 0;
   LastDRIIListe.DR  := 0;

    DRIIListenStr:=DR_.Text;
    ok    :=TRUE;

    repeat
     GetNextDrAusListe(Nr.DR,Nr.Jahr,FALSE);
     if Nr.DR > 0 then begin
      ValidateDRNr(Nr,FALSE);
      OK := DRIIExist(Nr.DR,Nr.Jahr,DRIIJahre,true);
      if ok then begin

       LiesDRII(NR.jahr,NR.dr);
       preinitpopupmodal(aa);
       for y:=0 to KopieGlArt.Items.Count-1 do
        aa[y+1]:= KopieGlArt.Items[y];                                                              { TODO 2 -oXEFrank -cS_2_Short : XE }
       y := winPopUpmodal(C,AA,0,'Versandart '+DRToString(nr)+' '+(DR_II.Glaubiger.Name),KopieGlArt.ItemIndex+1,TRUE,FALSE,0);
//02.07.14       y := Pop Up(C,AA^,0,'Versandart '+DRToString(nr)+' '+(DR_II.Glaubiger.Name),KopieGlArt.ItemIndex+1,TRUE,FALSE,0);

       dec(y);//18.09.13

//02.07.14       deinitpopup(aa);
       if c<>t_esc then begin
        LiesDRII(NR.jahr,NR.dr);

        //14.10.13:
        Inc(DR_II.VAK3);
        DatumLesen(DR_II.VAK3Datum);
        SchreibeDRII(NR.jahr,NR.dr);
        if Land<>_BA then
         WFehler('Statistik ergùnzt!',2);

{
0 EGVP qualifizierte Signatur
1 E-Mail verschlùsselt+qualifizierte Signatur
2 Post/Fax mit Anschreiben
3 Aushùndigung am Ort der Abnahme
}
        if y in [2,3] then //per Post oder Aushùndigung!
         PrintPDFFile(FertigesPDF,
                      true,
                      true,abort);
        if y=2 then begin//per Post
         //Frage nach Brief!
         FormBrief(10,'VVGL13.TXT',0,0,NR.DR,NR.Jahr,Code,
              False, //usevorgabe
              False, //usef4
              false, //abfrageb
              false, //dochabfragen
              false, //serienbrief
              0,     //autoanzahl
              true); //kein stampit
         modal_FormBriefWait;
        end
        else
        if y=0 then begin//EGVP
         EM:=AskDRIIEMailEmpf(Code,'Empfùnger',DR_II,[Glaeubiger,GVertreter]);
         VVanGl(true,EM,Sache,Az,NR,FertigesPDF);
        end
        else
        if y=1 then begin//EMAIL
         VVanGl(false,'',Sache,Az,NR,FertigesPDF);
        end;
       end;

      end;
     end;
    until (Nr.DR = 0) or not(OK);
   except
    on EXC_:Exception do
     ExceptionHintUser0('05326',EXC_.Message,EXC_.ClassName);
   end;

   //30.11.17:
   DRIIListenStr :=BUFF;
   FIRSTDRIIListe:= rFIRSTDRIIListe;
   LastDRIIListe := rLastDRIIListe;

 end;


VAR R:DR_II_Typ;
    dd:datumstyp;
  var // FL 06.12.22 geb : tgeb;
      einstell : teinstell;
      Kosteneinzug:boolean;
      Vorlage:string;
      lBriefname : String; // BW 27.02.23
begin//ColorBitButton1Click
 Abgearbeitet:=TRUE;//18.04.13

 AddMenueAufrufe(9007);//26.09.13

  //10.07.13:
  if CheckBox3.Checked then begin
   if ImTermin.Checked and (DIP[86] and 2=2) then begin
    DIP[86]:=DIP[86] and (255-2);
    WriteOneDip(86);
   end
   else
   if not(ImTermin.Checked) and (DIP[86] and 2=0) then begin
    DIP[86]:=DIP[86] or 2;
    WriteOneDip(86);
   end;
  end;
  if CheckBox4.Checked then begin
   if KeineFrist.Checked and (DIP[86] and 4=0) then begin
    DIP[86]:=DIP[86] or 4;
    WriteOneDip(86);
   end
   else
   if not(KeineFrist.Checked) and (DIP[86] and 4=4) then begin
    DIP[86]:=DIP[86] and (255-4);
    WriteOneDip(86);
   end;
  end;

  //19.04.14:
  if CheckBox6.Checked and CheckBox6.Visible then begin
   if JetztSV.Checked and (DIP[89] and 64=0) then begin
    DIP[89]:=DIP[89] or 64;
    WriteOneDip(89);
   end
   else
   if not(JetztSV.Checked) and (DIP[89] and 64=64) then begin//15.08.14 hui! 4) then begin
    DIP[89]:=DIP[89] and (255-64);
    WriteOneDip(89);
   end;
  end;

  //15.08.14:
  if VorbuchMerk.Checked and VorbuchMerk.Visible then begin
   if Vorbuch.Checked and (DIP[91] and 2=0) then begin
    DIP[91]:=DIP[91] or 2;
    WriteOneDip(91);
   end
   else
   if not(Vorbuch.Checked) and (DIP[91] and 2=2) then begin
    DIP[91]:=DIP[91] and (255-2);
    WriteOneDip(91);
   end;
  end;

  //30.12.14:
  if CheckBox1.Checked and CheckBox1.Visible then begin
   if NOT({05.01.18} KopieGl.Checked) and (DIP[92] and 1=0) then begin
    DIP[92]:=DIP[92] or 1;
    WriteOneDip(92);
   end
   else
   if {05.01.18 not}(KopieGl.Checked) and (DIP[92] and 1=1) then begin
    DIP[92]:=DIP[92] and (255-1);
    WriteOneDip(92);
   end;
  end;

  //15.08.14:
  if Vorbuch.Checked then begin
// FL 06.12.22   fillchar(geb,sizeof(geb),0);
   datumleseN(dd);
   einstell:=keinst;//10.10.17

   //16.06.18:
   R:=DR_II;//das puffern ist woh leher nicht nùtig
   LiesDRII(DR.jahr,DR.dr);

   GetDRIIKostenRGEasy(GetForderung367(DR,DD),{ FL 06.12.22Geb,}DR,FALSE,Einstell
                       ,FALSE,FALSE,false ,FALSE,Kosteneinzug,Vorlage);

   DR_II:=R;//16.06.18

  end;
  
  //04.11.12:
  try
   //13.12.12:
   if VVListe.ItemIndex=-1 then
    FertigesPDF:=''
   else // BW 24.11.22
      if Dokumentenname <> ''
      then FertigesPDF := EVPath + Dokumentenname
      else FertigesPDF:=VVListeDatNamen.Items[VVListe.ItemIndex];                                   { TODO 2 -oXEFrank -cS_2_Short : XE }
  except
   WFehler('oops',2);
  end;

 fillchar(E,sizeof(E),0);//06.02.13
 if VVListe.ItemIndex>=0 then//06.02.13
  //04.11.12:
  try
   ss:=ExtractFileName(VVListeDatNamen.Items[VVListe.ItemIndex]);                                   { TODO 2 -oXEFrank -cS_2_Short : XE }
// FL 23.11.22 Gibt es nicht mehr   ss:=copy(ss,pos('GVS',ss),200);
   ss:=copy(ss,Succ(pos('_',ss)),200); // FL 23.11.22
   if pos('.',ss)>0 then
    delete(ss,pos('.',ss),200);
   //ss jetzt GUID!
   FindVV(E,DR,ss);
  except
   on EXC_:Exception do
    ExceptionHintUser0('05327',EXC_.Message,EXC_.ClassName);
  end;

  //21.03.13:
 if _ANotFound then begin
  R:=DR_II;
  LiesDRII(DR.jahr,DR.dr);
//  if (GetNextAdr(DRI,DR,AA,Partei,true) then

  //03.07.13
  if (Pos((AnsiUpperCase(DR_II.Schuldner.NameUTF)),(AnsiUpperCase(S_Adresse.NameUTF)))>0) and
     ((Pos((AnsiUpperCase(DR_II.Schuldner.VornameUTF)),(AnsiUpperCase(S_Adresse.VornameUTF)))>0) or
      (Pos((AnsiUpperCase(S_Adresse.VornameUTF)),(AnsiUpperCase(DR_II.Schuldner.VornameUTF)))>0))
     then

   Adresse:=S_Adresse;//16.01.13
  DR_II:=R;
 end
 else

  Adresse:=S_Adresse;//16.01.13

  //26.03.13:
  if Adresse.Name='' then begin//26.03.13 (E.S.Name='') and (S.Name='') then begin
   Adresse:=AskMehrereAdressen(DR_II.Schuldner,DR,False,DEK.Schuldner);
  end;

         if Adresse.Name>'' then //09.07.13
          if E.VerfahrensNr<>'' then begin//24.01.13
           Adresse.VerfNr:=E.VerfahrensNr;
           Adresse.EinlieferDatum:=E.Datum;
           //! Ggf. nicht der Schuldner der Sache, falls nùmlich bei der Verarbeitung der EGVP Quittung keine gesp. Daten zu finden waren!
           AdressSave(Adresse,TRUE);//Open und Close der Kunden gekapselt
           WFehler('Verfahrensnr. beim Schuldner gespeichert!',2);
   end;

 //27.03.13:
 if ImTermin.Checked or
    EintragSVPost.checked or
    EintragSVpers.checked then
  if Adresse.Name>'' then begin
   DatumLesen(Adresse.EAO_);
   Adresse.EAODR     :=DR;//11.03.14
   Adresse.ZUEAO_.Tag:=0;//11.03.14, zurùcksetzen!
   AdressSave(Adresse,true);//Open und Close der Kunden gekapselt
  end;

 S_Adresse:=Adresse;//16.07.15 VerfNr belegt etc. 

 if //25.11.24 immer in Historie, dafùr ist der Druck nùtig, der dann gelùscht wird.   DruckNachweis.Checked and
    (FertigesPDF>'')//13.12.12
    then begin
  //***************************************************************************
  if E.Datum.Tag>0 then begin

   OeffneSPOOLER(80,1,GetLPT(80),'Nachweis Einlieferung VermVerz '+GetDRSTR(DR,FALSE,''),
                 DirektDruck,true,false,dr);
   //es geht auch pdflibrary.DrawHTMLText / DrawHTMLTextBox
   printstr(#5#7+InitDruckAnd12CPI+PRN219+'111');
   printstr(getpageheader('Nachweis Einlieferung VermVerz - '+GetDRSTR(DR,FALSE,''))+getpagefooter);
   printstr(#13#10#10);
   PrintStr('Schuldner:'#13#10);

   //14.02.13:
   if E.S.Name='' then
    PrintStr(UTF8Decode(GetAdresseFullName(S_Adresse,true,(not(S_Adresse.Bits3 AND 128 = 128))))+#13#10#10)     // FL 19.08.17 UTF8Decode ist WIDESTRING + true
   else
    PrintStr(UTF8Decode(GetAdresseFullName(E.S,true,(not(S_Adresse.Bits3 AND 128 = 128))))+#13#10#10);  // FL 19.08.17 UTF8Decode ist WIDESTRING + true

   //02.09.13   PrintStr('Das Vermùgensverzeichnis wurde am '+makestr(E.datum)+' beim ZenVG eingeliefert.'#13#10);
   if OptionalesDatumVVQuittung.Tag > 0
     then PrintStr('Das Vermùgensverzeichnis vom '+makestr(OptionalesDatumVVQuittung)+' wurde beim ZenVG eingeliefert.'#13#10)
     else PrintStr('Das Vermùgensverzeichnis vom '+makestr(E.datum)+' wurde beim ZenVG eingeliefert.'#13#10);//02.09.13
   case E.Art of
    0:PrintStr('Neue Abgabe.'#13#10);
    1:PrintStr('Nachbesserung.'#13#10);
    2:PrintStr('Erneute Abgabe.'#13#10);
   end;
   //Vorgang:Byte;//0=Eintragung;1=Korrektur,2=Korrektur irriger Eintrag,3=Lùschung,4=Lùschung irriger Eintrag
   PrintStr('Die Antwort des ZenVG erfolgte am '+makestr(E.Antw)+'.'#13#10);
   PrintStr('Gemeldetes Ergebnis: '+E.ErgebnisTxt+#13#10);
   PrintStr('EGVP / eBO Verfahrensnummer: '+E.VerfahrensNr+#13#10);
   PrintStr('EGVP / eBO Nachrichten-ID: '+E.NachrichtenID+#13#10);

   //25.11.24: Zwangsweise Ablage in Historie
   if not(DruckNachweis.Checked) then
    WINDirektDruck:=false;
   
   //23.08.13:
   if WINDirektDruck then
    AutoAddHistGetAnz:=TRUE;

   SchliesseSpoolerDat;
   if not(WINDirektDruck) then//23.08.13
    AddSpoolToHist(DR,FALSE,'Nachweis Einlieferung VermVerz',SPLr+GlobalSplName);//25.03.13

   //25.11.24: 
   if not(DruckNachweis.Checked) then
    DelFile(SPLr+GlobalSplName);

   WFehler('Einlieferungsnachweis gedruckt!',2);
  end
  else
   WFehler('Keine Information ùber Einlieferung des VermVerz per EGVP / eBO gefunden. Nachweis bitte selbst ausdrucken!',2);
 end;
                                     
 if KopieS.checked
    and (FertigesPDF>'') then//13.12.12
  if KopieSArt.itemindex =0 then begin//per Post
   LiesDRII(DR.jahr,DR.dr);//22.01.13
   FormBrief(10,'VVS13.TXT',0,0,DR.DR,DR.Jahr,Code,
              False, //usevorgabe
              False, //usef4
              false, //abfrageb
              false, //dochabfragen
              false, //serienbrief
              0,     //autoanzahl
              true); //kein stampit
   modal_FormBriefWait;
  end;

 if KopieGl.checked
    and (FertigesPDF>'') then//13.12.12
// BW 27.02.23  if KopieGlArt.itemindex =2 then begin//per Post
  if KopieGlArt.itemindex in [0,2] then begin //per EGVP oder Post // BW 27.02.23
   if KopieGlArt.ItemIndex = 2
     then begin
            LiesDRII(DR.jahr,DR.dr);//22.01.13
            FormBrief(10,'VVGL13.TXT',0,0,DR.DR,DR.Jahr,Code,
                      False, //usevorgabe
                      False, //usef4
                      false, //abfrageb
                      false, //dochabfragen
                      false, //serienbrief
                      0,     //autoanzahl
                      true); //kein stampit
            modal_FormBriefWait;
          end
     else begin
            lBriefName := temppath+'VVSchreibenGlaeubiger'+FormatDateTime('YY-MM-DD_HH-NN-SS-ZZZZ',NOW)+'.pdf';
            ColorBitButton1.Enabled := false; // BW 27.04.23
            if TPrintToPDFHandler.FormbriefToPDF(false,DR.DR,DR.Jahr,171,10{10 // BW 27.04.23},'VVGL13.TXT',lBriefName,true{// BW 27.04.23}) then
              FertigesPDF := lBriefname + ';' + FertigesPDF;                                        { TODO 2 -oXEFrank -cS_2_Short : XE }
            ColorBitButton1.Enabled := true; // BW 27.04.23
          end;
  end;

  {04.11.12 und direkt wieder weg damit...
 //PDF aussuchen!!
 //04.11.12:
 if (VorgabeID>'') and
    FileExists(EVPath+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+' '+VorgabeID+'.PDF')
    then begin
  FertigesPDF:=EVPath+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+' '+VorgabeID+'.PDF';
 end
 else begin

  PreInitPopupModal(AA);
  i:=0;
  if findfirst(EVPath+StrWordNull(KorrektJahr(DR.Jahr),2)+'-'+StrWord(DR.DR,1)+
               '*.pdf',faanyfile,sr)=0 then begin
   repeat
    inc(i);
    aa[i]:=sr.name;
    aa2[i]:=sr.name;
   until (findnext(sr)<>0) or (i>=Spez2.max);
   findclose(sr);
  end;
  if i=0 then begin
   TDialogs.MyMessagedlgOK('Es wurde kein ùbertragenes PDF zu dieser Akte gefunden!',
                mtwarning,[mbabort],0);
   exit;
  end;
  if i>1 then begin
   i:=WinPopUpModal( Code, AA, 0, 'Gespeicherte komplette VV', 1, true, false, 0);
   if code=t_esc then
    exit
   else
    FertigesPDF:=EVPath+aa2[i];
  end
  else
   FertigesPDF:=EVPath+aa2[1];
 end;}

 //14.05.13:
 if (KopieGl.checked or KopieS.Checked or Weitere.Checked) and (FertigesPDF='') then
  WFehler('Es ist kein VermVerz ausgewùhlt! Dieses Fenster ist zur Abarbeitung nach Quittungseingang!',2);

 //14.05.13:
 if Weitere.checked and (FertigesPDF>'') then
  PrintPDFFile(FertigesPDF,
               true,
               true,abort);

 //14.05.13:
 LiesDRII(DR.Jahr,DR.DR);
 if KopieGl.checked
    and (FertigesPDF>'') then begin //13.12.12
  if KopieGlArt.itemindex in [2,3] then //per Post oder Aushùndigung!
   PrintPDFFile(FertigesPDF,
               true,
               true,abort);
  if KopieGlArt.itemindex =2 then begin//per Post
   //brief wird oben schon gedruckt!
  end
  else
  if KopieGlArt.itemindex=0 then begin//EGVP mit QES
   VVanGl(true,DR_II.Glaubiger.EMail[0].Adr,Sache,Az,DR,FertigesPDF);
  end
  else
  if KopieGlArt.itemindex=1 then begin//EMAIL
   VVanGl(false,DR_II.Glaubiger.EMail[0].Adr,Sache,Az,DR,FertigesPDF);
  end;
 end;

 //14.05.13:
 DoDritt;

(*14.05.13
 //22.11.12:
 WeitereKopie:=0;
 if WeitereGl.Checked
    and WeitereGl.Enabled //27.02.13
    and (FertigesPDF>'') then begin//13.12.12
  Weitere; //erhùht ggf. auch WeitereKopien
 end;

 //30.10.12: neu 03.11.12:
 Val(Panz2.Text,a,dek.error);
 if KopieS.checked then
  inc(a);
 inc(a,WeitereKopie);
 if a>0 then
  for i:=1 to a do begin
   PrintPDFFile(FertigesPDF,(i=1),false,abort);
   if abort then
    break;
  end;

 if KopieGl.checked
    and (FertigesPDF>'') then //13.12.12
  if KopieGlArt.itemindex in [2,3] then //per Post oder Aushùndigung!
   PrintPDFFile(FertigesPDF,
               a=0,//das ist der 1. Ausdruck!
               true,abort);

 if KopieGl.checked
    and (FertigesPDF>'') then //13.12.12
  if KopieGlArt.itemindex =2 then begin//per Post
   //brief wird oben schon gedruckt!
  end
  else
  if KopieGlArt.itemindex=0 then begin//EGVP
   VVa nGl(true,SAF EID.Text,Sache,Az,DR,FertigesPDF);
  end
  else
  if KopieGlArt.itemindex=1 then begin//EMAIL
   VVa nGl(false,S AFEID.Text,Sache,Az,DR,FertigesPDF);
  end;

 //11.12.12:
 if DrittGl.checked
    and (FertigesPDF>'') then //13.12.12
  if VAK3Art.itemindex in [2,3] then //per Post oder Aushùndigung!
   Pri ntPDFFile(FertigesPDF,
               a=0,//das ist der 1. Ausdruck!
               true,abort);
 if DrittGl.checked then
  if VAK3Art.itemindex =2 then begin//per Post
   //brief wird oben schon gedruckt!
  end
  else
  if VAK3Art.itemindex=0 then begin//EGVP
   VVa nGl(true,SA FEID.Text,Sache,Az,DR,FertigesPDF);
  end
  else
  if VAK3Art.itemindex=1 then begin//EMAIL
   VVa nGl(false,S AFEID.Text,Sache,Az,DR,FertigesPDF);
  end;
*)

 //02.07.13:
 if ProtoVAKVfg2.Checked or Proto2.Checked then begin
  LiesDRII(DR.jahr,DR.dr);
  if Proto2.Checked then
   Protokoll13VAK(DR,1);
  if ProtoVAKVfg2.Checked then
   Protokoll13VAKVfg(DR,1);
 end;
 
 LiesDRII(DR.jahr,DR.dr);//19.03.13

{19.03.13
 //05.11.12:
 if KeineFrist.Checked then
  Eintrag882 c(DR,DR_II.Bezirksnummer,S,FALSE);
  }

 //27.03.13:

 if EintragSVPost.checked or
    (ZAPost.checked and ZAPost.Visible)//08.11.12
    then begin
{  R:=DR_II;
  LiesDRII(DR.jahr,DR.dr);}
  //nù:  AddKostenPostZU(DR_II);//09.12.05
  {08.05.13
  //ZU von Amst wegen, also nur Gebùhren!
  dr_ii.DRIIKos tenRG[10] := dr_ii.DRIIKost enRG[10]+
                            ZustellP ostAuslagen;
                            }
  WFehler('Keine Ergùnzung der Kostenrg. um Auslagen der PostZU erfolgt.',2);//08.05.13                          

  //27.02.13:
  DruckeDRIIZustellPost('Eintragungsanordnung',DR_II.Titel[1],DR,TRUE,S_Adresse);
(*27.02.13
  DRIIZustellun gen(DR,DR_II.Titel[1].D6.Titel,,DR_II.Titel[1],
                                    DR_II.Titel[1].ZustellUrkunde,'',S
                                    {DR_II.Schuldne r_3},3,TRUE,'');*)
//14.02.14 NEIN! Sonst falsche Kostenrg!  INC(DR_II.anzz);{POST}
  SecureWriteDRIIOne(dr.Jahr,dr.DR);
//  DR_II:=R;
 end
 else
 if EintragSVPers.checked or
    (ZApers.checked and ZApers.Visible)//08.11.12
    then begin
{  R:=DR_II;
  LiesDRII(DR.jahr,DR.dr);}
  //ToDo: Kostenrg. mùsste mit Wegegeld ergùnzt werden!
  //nù:  AddKostenPersZU(DR_II);

  //11.07.13:
  DRIIZustellungen(DR,DR_II.Titel[1].D6.Titel,
                      DR_II.Titel[1],
                                    DR_II.Titel[1].ZustellUrkunde,'',S_Adresse
                                    {DR_II.Schuldne r_3},2,TRUE,
                                    'Eintragungsanordnung'
                                    ,false
                                    );

  (*11.07.13
  DRIIZustellun gen(DR,DR_II.Titel[1].D6.Titel,DR_II.Titel[1],
                                    DR_II.Titel[1].ZustellUrkunde,'',S
                                    {DR_II.Schuldne r_3},2,TRUE,'');*)
//14.02.14 NEIN! Sonst falsche Kostenrg!  INC(DR_II.anzpz);{Pers}
  SecureWriteDRIIOne(dr.Jahr,dr.DR);
//  DR_II:=R;
 end;
 if Frist1.checked or EintragSVPost.checked or EintragSVpers.checked then begin
  Brief;
 end;
 if Wiedervorlage.checked and Wiedervorlage.Visible then begin
  fillchar(termin,sizeof(termin),0);
  termin.S:='Erinnerung: Ggf. Eintragung SV vornehmen';
  termin.Magic:='Eintr. SV';
  ValDate(termin.Datum,WiederVorlageD.Text);                                                        { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
  if termin.Datum.Tag>0 then begin
   termin.REG  :=DRIIR;
   termin.DR   :=DR;
   termin.TerminWo:='GV-Bùro';
{   R:=DR_II;
   LiesDRII(DR.jahr,DR.dr);}
   termin.KundeNeu:=Getkey(S_Adresse,1);//16.07.15 DR_II.Schuldner,1);
//   DR_II:=R;
   Savetermin(termin,false,Buff);
  end;
 end;

 //19.04.14:
 if JetztSV.Checked then begin
  LiesDRII(DR.jahr,DR.dr);
  valdate(dd,jetztsvdat.text);                                                                      { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
  Eintrag882c(DR,DR_II.Bezirksnummer,S_Adresse,FALSE,dd,
              ''//NEIN, geht nicht, da hier f755802l ùberhaupt nicht ausgefùllt wurde - Form2Data(T Form(f755802l1),'')//19.04.14
              ,TRUE//13.12.17 WAIT!
              );
 end
 else

 //19.03.13: von oben
 if KeineFrist.Checked then begin
  LiesDRII(DR.jahr,DR.dr);//19.03.13
  Eintrag882c(DR,DR_II.Bezirksnummer,S_Adresse,FALSE,
              ''//NEIN, geht nicht, da hier f755802l ùberhaupt nicht ausgefùllt wurde - Form2Data(T Form(f755802l1),''));//19.04.14
              ,TRUE//13.12.17 WAIT!
             );
 end;

 Close;//06.11.12

end;

procedure TfVVEinlieferung.ImTerminClick(Sender: TObject);
begin
 if imtermin.checked then begin
  EintragSVPost.checked:=false;
  EintragSVpers.checked:=false;
//19.08.13  KeineFrist.Checked:=FALSE;
  frist1.checked:=false;
 end;
 EintragSVpersClick(sender);
end;

procedure TfVVEinlieferung.Init;
begin
 InInit:=TRUE;
 //20.09.12:
 try
  MyGUID:=MyMessageID;//04.11.12
  DRNr.Text:=DRToString(DR);
//22.01.13   R:=DR_II;
  LiesDRII(DR.jahr,DR.dr);

//26.07.13  Sache      :='Sie ./. '+GetAdresseFull(S);//16.01.13 DR_II.Schuldne r);
  Sache      :=GetAdresseFull(DR_II.Glaubiger)+' ./. '+GetAdresseFull(S_Adresse);//16.01.13 DR_II.Schuldne r);
  Az         :=DR_II.GAktz;

  //25.06.13:
  if Az='' then
   Az:=DR_II.GVAktz;

  //01.11.12:
  Schuldner.Font.Style:=[fsbold];
  Schuldner.Caption:=//DRToString(DR)+#13+
                     GetAdresseFull(DR_II.Schuldner);
  if ZVAntragElementChecked(DR_II,DR,'P4') then // DR_II.Bea ntragt2 and 1=1 then
   Schuldner.Caption:=Schuldner.Caption+#13'Glùubig. hat Auftrag zur elektronischen ùbermittlung der VAK gestellt!'
  else
   Schuldner.Caption:=Schuldner.Caption+#13'Glùubig. hat KEINEN Auftrag zur elektronischen ùbermittlung der VAK gestellt!';

  FillAdrLongForMemo(DR_II.Glaubiger,RubrumG,DR,False,Glaeubiger,TRUE);
  FillAdrLongForCombo(DR_II.Schuldner,RubrumS,DR,False,dek.Schuldner);
  FindSchuldnerInCombo(RubrumS,S_Adresse);//wird nicht mehr genutzt, da RubrumSNeu drùberliegt

  FillAdrLongForMemo(S_Adresse,RubrumSNeu,DR,False,DEK.Schuldner,TRUE,TRUE);//07.12.12

  Auftrag.Caption  :=GetDRIIVerfahren(DR_II);
//22.01.13   DR_II:=R;

  //18.11.19:
{21.11.19  if Land=_NR then
   ZenVGTransfer.ItemIndex:=ZenSig-1
  else} begin
   ZenVGTransfer.ItemIndex:=0;
   ZenVGTransfer.Enabled:=False;
  end;
{18.11.19
   ZenVGTransfer.ItemIndex:=ZenSig-1;//28.09.12}

 except
  on EXC_:Exception do
   ExceptionHintUser0('05328',EXC_.Message,EXC_.ClassName);
 end;
 InInit:=FALSE;
end;

procedure TfVVEinlieferung.JetztSVClick(Sender: TObject);
var d:datumstyp;
begin
 //18.04.14:
 JetztSVDat.Enabled:=JetztSV.checked;
 if JetztSV.checked then begin
  KeineFrist.Checked:=False;//19.04.14
  Wiedervorlage.Checked:=False;//19.04.14
  datumlesen(d);
  CheckFristen_UrlaubFeierTag(D,15,false,true);//30.05.14 ,true);
  JetztSVDat.Text:=MakeStr(D);
 end;
end;

procedure TfVVEinlieferung.JetztSVDatExit(Sender: TObject);
var t,d:datumstyp;
begin
 //18.04.14:
 datumlesen(t);
 valdate(d,JetztSVDat.text);                                                                        { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
 if absolut_31(d)<=absolut_31(t) then begin
  CheckFristen_UrlaubFeierTag(t,15,false,true);
  WFehler('Nur Angaben in der Zukunft werden zugelassen. Ansonsten ist sofort einzuliefern!',2);
  JetztSVDat.Text:=makestr(t);
  try
   if AdvOfficePager1.ActivePage=bestaetigt then//22.04.14
    if JetztSVDat.CanFocus then//03.07.20
     JetztSVDat.setfocus;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05329',EXC_.Message,EXC_.ClassName);
  end;
 end
 else begin
  incdatum(t,30);
  if absolut_31(d)>absolut_31(t) then begin
   datumlesen(t);
   CheckFristen_UrlaubFeierTag(t,15,false,true);
   WFehler('Nur Angaben bis max. in 30 Tagen werden zugelassen!',2);
   JetztSVDat.Text:=makestr(t);
   try
    if AdvOfficePager1.ActivePage=bestaetigt then//22.04.14
     if JetztSVDat.CanFocus then//03.07.20
      JetztSVDat.setfocus;
   except
    on EXC_:Exception do
     ExceptionHintUser0('05330',EXC_.Message,EXC_.ClassName);
   end;
  end;
 end;
end;

procedure TfVVEinlieferung.KeineFristClick(Sender: TObject);
begin
 if KeineFrist.checked then begin
  JetztSV.Checked:=FALSE;//19.04.14
  EintragSVpers.checked:=false;
//19.08.13  imtermin.checked:=false;
  EintragSVPost.Checked:=FALSE;
  frist1.checked:=false;
 end;
 EintragSVpersClick(sender);
end;

procedure TfVVEinlieferung.KopieAkte2Click(Sender: TObject);
begin
 //18.08.13:
 if KopieAkte2.Checked then
  DIP[87] := DIP[87] or 16
 else
  DIP[87] := DIP[87] and (255-16);
 WriteOneDip(87);
end;

procedure TfVVEinlieferung.KopieAkteClick(Sender: TObject);
begin
 //18.08.13:
 if KopieAkte.Checked then
  DIP[87] := DIP[87] or 4
 else
  DIP[87] := DIP[87] and (255-4);
 WriteOneDip(87);
end;

procedure TfVVEinlieferung.KopieGlClick(Sender: TObject);
begin
 //04.11.12:
 KopieGlArt.Enabled:=KopieGl.Checked;
 CheckBox1.Enabled :=KopieGlArt.Enabled;//21.03.13
end;

procedure TfVVEinlieferung.KopieSchuldnerDirektClick(Sender: TObject);
begin
 //18.08.13:
 if KopieSchuldnerDirekt.Checked then
  DIP[87] := DIP[87] or 2
 else
  DIP[87] := DIP[87] and (255-2);
 WriteOneDip(87);
end;

procedure TfVVEinlieferung.KopieSClick(Sender: TObject);
begin
 KopieSArt.Visible:=KopieS.Checked;

 //18.08.13:
 if KopieS.Checked then
  DIP[87] := DIP[87] or 8
 else
  DIP[87] := DIP[87] and (255-8);
 WriteOneDip(87);

end;

procedure TfVVEinlieferung.LeerClick(Sender: TObject);
begin
//
end;

procedure TfVVEinlieferung.ListeClick(Sender: TObject);
begin
  BelegeEingaben := true; // Neu belegen da ggf. anderes VV 14.10.12
end;

procedure TfVVEinlieferung.MyStdButton1Click(Sender: TObject);
begin
Unterstuetzung;
end;

procedure TfVVEinlieferung.MyStdButton3Click(Sender: TObject);
begin
 showhilfeidx(helpcontext);
end;

procedure TfVVEinlieferung.NoSaClick(Sender: TObject);
begin
 if NoSa.checked and (DIP[94] and 1=0) then begin
  DIP[94]:=DIP[94] or 1;
  WriteOneDip(94);
 end
 else
 if not(NoSa.checked) and (DIP[94] and 1=1) then begin
  DIP[94]:=DIP[94] and (255-1);
  WriteOneDip(94);
 end;

end;

procedure TfVVEinlieferung.OK1Click(Sender: TObject);
begin
 AdvOfficePager1.ActivePage:=Angaben;
end;

 procedure TfVVEinlieferung.DoOnePDF;
 var s,s2:IStringlist;
     i:integer;
      h:integer;
      lPDFResult : IPDFResults;
 begin
  if OnePDFDone then
   Exit;

  //29.05.15: da sonst nicht mehr die zugehùrige Datendatei mit den Schuldnerdaten geladen wird!!
  RettUse_Name := GetTheLastVV;

  s:=nil;//06.08.13
  s2:=nil;//06.08.13
  //10.10.12:

  h:=height;
  height:=0;
  enabled:=false;

  try
   PutWarte('Umwandlung von Vermùgensverzeichnissen und Bildern in PDF...');

   try
    s := TIStringlist.create;
    s2:= TIStringList.Create;
    s.clear;
    S2.Clear;
    try
     for i := 0 to Liste.Count - 1 do
      if Liste.Checked[i] then
        begin
          s.add(Dateinamen[i]);//EVPath+Liste.items[i]);
          // BW 08.05.24 s2.add(Liste.Items[i]);
          s2.Add(ExtractFileName(Dateinamen[i]));
        end;
     CreatePDFfromDiffSrc(s,TRUE,DR);
    except
     on EXC_:Exception do
      ExceptionHintUser0('05331',EXC_.Message,EXC_.ClassName);
    end;
   except
     on EXC_:Exception do
      ExceptionHintUser0('05332',EXC_.Message,EXC_.ClassName);
   end;

   DelWarte;

   enabled:=true;
   height:=h;
//07.12.12   WindowState := WS;  // 14.10.12

   WFehler('Das zusammengesetzte PDF wird jetzt angezeigt...',2);
   lPDFResult := (TPDFUtil.MergeSplit(s,s2,FertigesPDF,true, false, false, DR{15.07.22}));
   OnePDFDone:=lPDFResult.OK; // FL 04.03.23 PDF Hinzufùgen.

   if OnePDFDone then begin
    // FL 14.10.12 Sonst funktioniert der Rest nicht...
    for i:=0 to Liste.Items.Count-1 do
      Liste.Checked[i] := false;

    for i := 0 to lPDFResult.ItemCount -1 do
      begin
        Liste.Items.Add(ExtractFilename(lPDFResult.Item(i).FileName));
        Liste.Checked[Liste.Items.Count-1] := true;

        SetLength(DateiNamen,length(DateiNamen)+1);
        DateiNamen[High(DateiNamen)]:=lPDFResult.Item(i).FileName;                                  { TODO 2 -oXEFrank -c'WideString' zu 'ShortString' : XE }

        CopyFile(ExtractFilePath(lPDFResult.Item(i).FileName),ExtractFilePath(FertigesPDF),ExtractFileName(lPDFResult.Item(i).Filename),true);  { TODO 2 -oXEFrank -cS_2_Short : XE }
        RenameFile(ExtractFilePath(FertigesPDF) + ExtractFileName(lPDFResult.Item(i).Filename),FertigesPDF);
      end;
    // FL s.o.

    Liste.Enabled       := FALSE;
    AddDocument.Enabled := FALSE; // FL 09.10.23

    BearbeitHint.Caption:='Das zu ùbermittelnde PDF wurde bereits angezeigt/bearbeitet.';
   end;
  except
    on EXC_:Exception do
      ExceptionHintUser0('05333',EXC_.Message,EXC_.ClassName);
  end;
 end;

procedure TfVVEinlieferung.DrittGlClick(Sender: TObject);
begin
 //21.03.13:
 VAK3Art.Enabled   :=DrittGl.Checked; //14.05.13 egal
 CheckBox2.Enabled :=VAK3Art.Enabled; //14.05.13 egal
end;

procedure TfVVEinlieferung.DruckNachweisClick(Sender: TObject);
begin
 {06.11.24 https://xgvservice.atlassian.net/browse/XGVSERV-2123 soll nicht mehr Pflicht sein
 //18.04.13:
 if not(DruckNachweis.Checked) then begin
  DruckNachweis.Checked:=true;
  WFehler('Verpflichtend!',2);
 end;}

 if DruckNachweis.Checked and (DIP[111] and 32=32) then begin
  DIP[111]:=DIP[111] and (255-32);
  WriteOneDip(111);
 end
 else
 if not(DruckNachweis.Checked) and (DIP[111] and 32=0) then begin
  DIP[111]:=DIP[111] or 32;
  WriteOneDip(111);
 end;

end;

procedure TfVVEinlieferung.DwlClick(Sender: TObject);
begin
 CallVollPortalRef(DR,S_Adresse.VerfNr,false,false);//06.02.13
end;

procedure TfVVEinlieferung.EintragSVpersClick(Sender: TObject);
begin
 if EintragSVpers.checked then begin
  EintragSVPost.checked:=false;
  imtermin.checked:=false;
  KeineFrist.Checked:=FALSE;
  frist1.checked:=false;
 end;
 Wiedervorlage.Visible :=EintragSVPost.Checked or EintragSVpers.Checked or ImTermin.Checked{21.02.13};
 WiedervorlageD.Visible:=Wiedervorlage.Visible;
end;

procedure TfVVEinlieferung.EintragSVPostClick(Sender: TObject);
begin
 if EintragSVPost.checked then begin
  EintragSVpers.checked:=false;
  imtermin.checked:=false;
  KeineFrist.Checked:=FALSE;
  frist1.checked:=false;
 end;
 EintragSVpersClick(sender);
end;

procedure TfVVEinlieferung.OK2Click(Sender: TObject);
var //SAFE,
    a,i:Integer;
    abort,
    LuhnFehler : boolean;
    d:datumstyp;
begin

 (*08.12.12 blùdsinn, auf rechnern ohne egvp soll es ohne nerviges fenster weitergehen!
 if trim(GetIm portF) = '' then // FL 10.11.2012
   begin
     TDialogs.MyMessageDlgOK('Sie haben E GVP n icht installiert oder wir konnten die Installation nicht finden!',mtwarning,[mbabort],0);
     exit;
   end;
*)

  //02.07.13:
  if Proto.Checked and (DIP[89] and 1=0) then begin
   DIP[89]:=DIP[89] or 1;
   WriteOneDip(89);
  end
  else
  if not(Proto.Checked) and (DIP[89] and 1=1) then begin
   DIP[89]:=DIP[89] and (255-1);
   WriteOneDip(89);
  end;
  if ProtoVAKVfg.Checked and (DIP[89] and 2=0) then begin
   DIP[89]:=DIP[89] or 2;
   WriteOneDip(89);
  end
  else
  if not(ProtoVAKVfg.Checked) and (DIP[89] and 2=2) then begin
   DIP[89]:=DIP[89] and (255-2);
   WriteOneDip(89);
  end;

 if not(loeschung.Checked) then
   begin
     a:=0;
     for i := 0 to Liste.Count - 1 do
     if Liste.Checked[i] then
      inc(a);
     if a=0 then begin
      TDialogs.MyMessageDlgOK('Sie haben kein VermVerz zur ùbertragung gewùhlt!',mtwarning,[mbabort],0);
      Exit;
     end;
   end;

 // FL 17.10.12
 if Neu.Checked
    and (S_Adresse.Name>'')//09.07.13
    then begin
  VerfNr.Text := '';

  //16.01.13:
{  LiesDRII(DR.jahr,DR.dr);
  LoadAdresse(DR_II.Schuldn er);}
  Adresse:=S_Adresse;
//  Adresse.Verf Nr:=E.VerfahrensNr;
  valdate(D,VerfNrDat.Text);                                                                        { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
  Adresse.EinlieferDatum:=D;//hmmm, in Meta rausgenommen?!!?
  AdressSave(Adresse,TRUE);//Open und Close der Kunden gekapselt

  f755802l1.LocalAdr.EinlieferDatum:=D;//17.06.15
    
 end

 else begin

  //07.07.21:
  if companyrights(true) then
   LuhnFehler:=false
  else begin

   //14.01.13:
   valdate(D,VerfNrDat.Text);                                                                       { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
   if D.Tag=0 then
    LuhnFehler := true
   else

   if trim(VerfNr.Text) = ''
     then LuhnFehler := true
     else LuhnFehler := not(checkLuhn(verfnr.text));
  end;

  if LuhnFehler { FL 17.10.12 VerfNr.Enabled and (trim(VerfNr.Text)='')}  then
    begin
      AdvOfficePager1.ActivePage:=Grundlagen;

      //14.01.13:
      if D.Tag=0 then
       TDialogs.MyMessageDlgOK('Sie mùssen neben der Verfahrensnummer auch das Datum der zu korrigierenden VAK eingeben!',
                     mtwarning,[mbok],0)
      else

      if trim(VerfNr.Text) = ''
        then TDialogs.MyMessageDlgOK('Sie mùssen die Verfahrensnummer ausfùllen, die als Referenz gilt.'#13#13+
                          'Diese ist entweder in Ihrem GV-Programm gespeichert oder wurde bei Abholung vom Vollstreckungsportal angegeben.',mtwarning,[mbok],0)
        else TDialogs.MyMessageDlgOK('Bitte ùberprùfen Sie die Verfahrensnummer. Die eingegebene Nummer ist nicht korrekt!'#13#13+
                          'Bitte vergleichen Sie Ihre Eingabe mit den Daten aus dem Vollstreckungsportal.',mtwarning,[mbok],0);
      try
       if VerfNr.CanFocus then//03.07.20
        VerfNr.SetFocus;
      except
 on EXC_:Exception do
 ExceptionHintUser0('05334',EXC_.Message,EXC_.ClassName);
      end;
      Exit;
  end;
 end;

 //12.10.12:

{04.11.12
 if (KopieGlArt.itemindex=0) and (trim(safei d.Text)='') then begin
  TDialogs.MyMessageDlgOK('Sie haben die ùbermittlung an den Glùubiger per EGVP gewùhlt, aber das Nutzer-ID (safe-id/govello-id) nicht eingegeben!',
               mtwarning,[mbok],0);
  Exit;
 end;}

 //Validierung!!!
 if not(f755802l1.CheckGebAm) then begin
  WFehler('Geburtsdtaum korrigieren!',2);
  AdvOfficePager1.ActivePage:=Angaben;
  Exit;
 end
 else
 if f755802l1.CheckListBox1.Items.Count>0
   then begin
          TDialogs.MyMessageDlgOK('~Daten unvollstùndig!'#13+
                       'Es sind nicht alle erforderlichen Daten im 2. Schritt erfasst worden!',
                       mtwarning,[mbabort],0);
          AdvOfficePager1.ActivePage:=Angaben;
          Exit;
        end
   else begin // FL 10.11.17
          if f755802l1.Jur.Checked and (f755802l1.RechtsformNeu.Itemindex < 0)
            then begin
                    TDialogs.MyMessageDlgOK('~Daten unvollstùndig!'#13+
                                 'Bei juristischen Personen ist die Rechtsform ein Pflichtfeld, bitte auswùhlen!',
                                 mtwarning,[mbabort],0);
                    AdvOfficePager1.ActivePage:=Angaben;
                    Exit;
                 end
            else begin // Next Pflaster
                     if (f755802l1.HinweisNotOK) then
                       begin
                          TDialogs.MyMessageDlgOK('~Daten unvollstùndig!'#13+
                                         'Bitte zunùchst alle Hinweise entfernen.',
                                         mtwarning,[mbabort],0);
                          Exit;
                       end;
                 end;
        end;

 if not(loeschung.Checked) then
   begin
     { $ IFDEF PDFExport}
     if AllowPDFExport then//11.06.14
      TFFS2XML.PDFSignatur := TPDFSignatur.Create; // FL 21.05.14 kein Free ist ein Interface!
     { $ ENDIF}
     try
      //Erstellt EIN PDF
      DoOnePDF;//wird nur 1x aufgerufen, Dank OnePDFDone boolean...
     except
      on EXC_:Exception do
       ExceptionHintUser0('05335',EXC_.Message,EXC_.ClassName);
     end;

     if NOT(OnePDFDone) or not(FileExists(FertigesPDF)) then begin
      { $ IFDEF PDFExport}
      if AllowPDFExport then//11.06.14
       TFFS2XML.PDFSignatur := NIL; // FL 21.05.14
     { $ ENDIF}

      TDialogs.MyMessageDlgOK('Mangels PDF wird die ùbergabe an EGVP / eBO NICHT eingeleitet!'#13#13+
                   FertigesPDF,
                   mtwarning,[mbok],0);
      Exit;
     end;
     { $ IFDEF PDFExport}
     try
      if AllowPDFExport then begin//11.06.14
       PDF_PrepOCR(FertigesPDF,TFFS2XML.PDFSignatur); // FL 21.05.14
       TFFS2XML.PDFSignatur := NIL;
      end;
     except
      on EXC_:Exception do
       ExceptionHintUser0('05336',EXC_.Message,EXC_.ClassName);
     end;
     { $EN DIF}
   end;

{ schon in proc unten...
 SAFE:=Zen Voll[Land];
 i:=length(SAFE);
 while (i>0) and (SAFE[i]<>';') do
  dec(i);
 SAFE:=Copy(SAFE,i+1,80);}

 //07.04.14:
 if INOK2 then
  exit;
 INOK2:=TRUE;

 Enabled:=False;//20.06.18 Warum nicht ganz stilllegen?

 try

  f755802l1.SaveAdresse( not(NotSave.Checked){24.03.20 true});//24.08.13, 17.06.15 aktualisiert auch DR II!

  S_Adresse:=f755802l1.LocalAdr;//17.06.15

  if not(NotSave.Checked) then//24.03.20
   AdressSave(S_Adresse,TRUE);//17.06.15

  f755802l1.VV_PDF_Name:=FertigesPDF;
  f755802l1.Verfahrensnummer:=VerfNr.Text;

  //14.01.13:
  valdate(d,VerfNrDat.text);                                                                        { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
  if D.Tag=0 then
   Datumlesen(D);
  f755802l1.VerfDatum:=D;//15.01.13 VerfahrensnummerDatum:=MakeStr_to_Long(d);

  //26.09.13:
  if loeschung.checked then
   AddMenueAufrufe(9003)
  else
  if nachbesserung.checked then
   AddMenueAufrufe(9004)
  else
  if erneut.checked then
   AddMenueAufrufe(9005)
  else
  if Korrektur.Checked then
   AddMenueAufrufe(9012) // FL 17.12.13
  else
   AddMenueAufrufe(9006);

 // FL 17.10.12 Wird nicht belegt if OK=mrok then
   f755802l1.Eintragung_VV(tSignatur(ZenVGTransfer.itemindex),TempPath+'xjustiz_nachricht.xml',DR,S_Adresse,MyGUID,FALSE,Aktenzeichen_Empfaenger.Text);
 { else
   WFehler('Abbruch durch den Anwender!',2); }

  //VVs drucken und zwar alle angekreuzten!
  Val(Panz.Text,a,dek.error);
 {30.10.12 erst nach Quittung!!
  if KopieGl.checked then
   if KopieGlArt.itemindex in [2,3] then//per Post oder Aushùndigung!
    inc(a);
  if KopieS.checked then
   inc(a);}

   //19.11.12:
  if KopieSchuldnerDirekt.Checked then
   inc(a);

  //18.08.13:
  if KopieAkte.Checked then
   inc(a);

  if a>0 then
   for i:=1 to a do begin
    PrintPDFFile(FertigesPDF,(i=a),false,abort);//12.10.12 Druckt direkt ein VV aus.
    if abort then
     break;
   end;

  //21.11.12:
  if Proto.Checked then begin
   LiesDRII(DR.jahr,DR.dr);//23.05.13
   Protokoll13VAK(DR,1);
  end;
  //23.11.12:
  if ProtoVAKVfg.Checked then begin
   LiesDRII(DR.jahr,DR.dr);//23.05.13
   Protokoll13VAKVfg(DR,1);
  end; 


 {  for i := 0 to Liste.Count - 1 do
   if Liste.Checked[i] then
    PrintVVDirect(FileNamen.Items[i],DR);//12.10.12 Druckt direkt ein VV aus.}

 (*03.11.12
  if KopieGlArt.itemindex=0 then begin
   VVa nGl(true,SAFE ID.Text,Sache,Az,DR,FertigesPDF);
 {03.11.12
   if Az>'' then
    Az:='Ihr Zeichen: '+Az+#13#10;
   EGVP_Mak eMail(SA FEID.Text,//'',//Empf
                 'Abgenommene Vermùgensauskunft',//Betreff
                 'Sache: '+Sache+#13#10+
                 Az+
                 'Mein Zeichen: '+DRToString(DR)+#13#10#13#10+
                 'Sehr geehrte Damen und Herren,'#13#10#13#10+
                 'anbei erhalten Sie das ausgefùllte Vermùgensverzeichnis ggf. mit Anlagen wie dieses beim Zentralen Vollstreckungsgericht hinterlegt wurde.'#13#10#13#10+
                 'Mit freundlichen Grùùen'+#13#10+
                 (Crunch(BezRec[AktBezRec].UserText[1])+' '+Crunch(BezRec[AktBezRec].UserText[2])),//Msg
                 DRToString(DR),
                 Az,
                 tSignatur(GlSig),
                 FertigesPDF,
                 EGVPMsgTESTONLY)//TEST}
  end
  else
  if KopieGlArt.itemindex=1 then begin//EMAIL
   //***************************************************************************
   VVa nGl(false,SAF EID.Text,Sache,Az,DR,FertigesPDF);
  end;*)

  //***************************************************************************

  //04.11.12 !NEIN!! DelFile(FertigesPDF);

 except
  on EXC_:Exception do
   ExceptionHintUser0('05337',EXC_.Message,EXC_.ClassName);
 end;

 Close;
{
 OK, macht schon die Proc da drùber...
 n := n+';'+temppath+'xjustiz_nachric ht.xml';
 EGVP_Mak eMail(SAFE,//'',//Empf
               'VV-Einlieferung',//Betreff
               '',//Msg
               tSignatur(ZenVGTransfer),
               n,
               EGVPMsgTESTONLY);//TEST
}
end;

procedure TfVVEinlieferung.RichterClick(Sender: TObject);
begin
 SubAufruf(2984);//30.01.13
end;

procedure TfVVEinlieferung.RubrumSChange(Sender: TObject);
begin
 //
end;

procedure TfVVEinlieferung.RubrumSDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
 //20.09.12:
 try
  begin//if odSelected IN State then begin
   RubrumS.Canvas.Brush.Color := S_color;
   RubrumS.Canvas.Font.Color  := ParteiFontColor;
  end;
  RubrumS.Canvas.FillRect(Rect);
  RubrumS.Canvas.TextOut(Rect.Left,Rect.Top,RubrumS.Items[Index]);
 except
  on EXC_:Exception do
   ExceptionHintUser0('05338',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TfVVEinlieferung.SearchVV;

(*10.10.12
 procedure Such(Art:byte;Discr,Name:ShortString);
 var  sr:tsearchrec;
     ext:shortstring;
 begin
  Ext :=ExtractFileExt(Name);
  Name:=Copy(Name,1,length(Name)-(length(Ext)))+'*'+Ext;
  if sysutils.FindFirst(EVPath+Name,faanyfile,sr)=0 then begin
   repeat
    SetLength(DateiNamen,length(DateiNamen)+1);
    DateiNamen[High(DateiNamen)]:=EVPath+sr.name;
    Liste.Items.Add(Discr+' vom '+GetEVDate(EVPath+SR.Name));
    Liste.Checked[Liste.Count-1]:=TRUE;
   until (sysutils.findnext(sr)<>0);
   sysutils.findclose(SR);
  end;
 end;*)

  procedure Such(Art:byte;Discr,Name:ShortString);//10.10.12
  var  sr:tsearchrec;
       ext:shortstring;
       Load : TInifile;
       Feld1,Feld2,NN : AnsiString;
  begin
   try
    Ext :=ExtractFileExt(Name);                                                                     { TODO 2 -oXEFrank -cS_2_Short : XE }
    Name:=Copy(Name,1,length(Name)-(length(Ext)))+'*'+Ext;
    if sysutils.FindFirst(EVPath+Name,faanyfile,sr)=0 then begin
     repeat
      SetLength(DateiNamen,length(DateiNamen)+1);
      DateiNamen[High(DateiNamen)]:=EVPath+sr.name;                                                 { TODO 2 -oXEFrank -cS_2_Short : XE }

      Load := TInifile.Create(EVPath+sr.Name);
      try
        try
          case Art of
            1 : begin
                  Feld1 := UTF8AnsiNormalized(Load.readString('Daten', 'Name',''));                 { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                  Feld2 := UTF8AnsiNormalized(Load.readString('Daten', 'Vorname',''));              { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                  NN := Discr+' vom '+GetEVDate(EVPath+SR.Name) + ' ('+Feld1+', '+Feld2+')';        { TODO 2 -oXEFrank -cS_2_Short : XE }
                end;
            2 : begin
                  Feld1 := UTF8AnsiNormalized(Load.readString('Daten', 'MP95|0',''));               { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                  Feld2 := UTF8AnsiNormalized(Load.readString('Daten', 'MP95|1',''));               { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                  NN := Discr+' vom '+GetEVDate(EVPath+SR.Name) + ' ('+Feld1+' '+Feld2+')';         { TODO 2 -oXEFrank -cS_2_Short : XE }
                end;
            3 : begin
                  Feld1 := UTF8AnsiNormalized(Load.readString('Daten', 'EDIT38',''));               { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                  NN := Discr+' vom '+GetEVDate(EVPath+SR.Name) + ' ('+Feld1+')';                   { TODO 2 -oXEFrank -cS_2_Short : XE }
                end;
            4 : begin
                  Feld1 := UTF8AnsiNormalized(Load.readString('Daten', 'EDIT29',''));               { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                  NN := Discr+' vom '+GetEVDate(EVPath+SR.Name) + ' ('+Feld1+')';                   { TODO 2 -oXEFrank -cS_2_Short : XE }
                end;
            5 : begin
                  Feld1 := UTF8AnsiNormalized(Load.readString('Daten', 'NAME_EIII',''));            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                  NN := Discr+' vom '+GetEVDate(EVPath+SR.Name) + ' ('+Feld1+')';                   { TODO 2 -oXEFrank -cS_2_Short : XE }
                end;
                //12.04.14:
            6 : begin
                  Feld1 := UTF8AnsiNormalized(Load.readString('Daten', 'NAME_Nach',''));            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                  NN := Discr+' vom '+GetEVDate(EVPath+SR.Name) + ' ('+Feld1+')';                   { TODO 2 -oXEFrank -cS_2_Short : XE }
                end;

          end; // of case
        except
          NN := Discr+' vom '+GetEVDate(EVPath+SR.Name);                                            { TODO 2 -oXEFrank -cS_2_Short : XE }
        end;
      finally
        Load.Free;
      end;

      Liste.Items.Add(NN); // Discr+' vom '+GetEVDate(EVPath+SR.Name));
      Liste.Checked[Liste.Count-1]:=TRUE;
      // FL 13.10.12FileNamen.Items.Add(EVPath+SR.Name);//10.10.12
     until (sysutils.findnext(sr)<>0);
     sysutils.findclose(SR);
    end;
   except
         on EXC_:Exception do
          ExceptionHintUser0('05339',EXC_.Message,EXC_.ClassName);
   end;
  end;


 procedure Anlagen;
 var s2:Shortstring;

  procedure add(s:shortstring);
  var sr:tsearchrec;

   function Gesamt:boolean;//15.12.12
   begin
    if (Pos('-',sr.name)>0) and
       (Pos(' GVS',sr.name)>0) then 
     Result:=true
    else
     Result:=false;
   end;

  begin //add
   if sysutils.FindFirst(EVPath+s,faanyfile,SR)=0 then begin
    Repeat
     if (AnsiUppercase(ExtractFileExt(sr.name))<>'.JPG') and
        (AnsiUppercase(ExtractFileExt(sr.name))<>'.BMP') and
        (AnsiUppercase(ExtractFileExt(sr.name))<>'.PDF') then begin
     end
     else begin

      SetLength(DateiNamen,length(DateiNamen)+1);
      DateiNamen[High(DateiNamen)]:=EVPath+sr.name;                                                 { TODO 2 -oXEFrank -cS_2_Short : XE }
      Liste.Items.Add('Anlage '+sr.name);
      // FL 13.10.12FileNamen.Items.Add(EVPath+sr.name);//10.10.12

      //15.12.12:
      if (AnsiUppercase(ExtractFileExt(sr.name))<>'.PDF') or
         not(Gesamt) then

       Liste.Checked[Liste.Count-1]:=TRUE;
     end;
    Until sysutils.findnext(sr)>0;
    sysutils.FindClose(sr);
   end;
  end;

  procedure SE(J:Integer;Addt,Addt2:ShortString);
  var i:integer;
      s:Shortstring;
  begin
   For i:=Length(S2) to 5 do begin
    s:=s2;
    While length(s)<i do
     s:='0'+s;
    s:=StrWordNull(KorrektJahr(J),2)+Addt+S+Addt2;
    Add(s);
   end;
   For i:=length(s2) to 5 do begin
    s:=s2;
    While length(s)<i do
     s:='0'+s;
    s:=StrWordNull(1900+J,4)+Addt;
    Add(s);
   end;
  end;

 begin//Anlagen
  str(DR.DR,S2);
  SE(DR.Jahr,'-',' *.*');
  SE(DR.Jahr,'-','-*.*');
  SE(DR.Jahr,'_',' *.*');
  SE(DR.Jahr,'_','_*.*');

  //30.12.19:
  SE(DR.Jahr,'-','_*.*');
 end;

var d:datumstyp;
begin
 Liste.Items.Clear;
// FL 13.10.12 Filenamen.Item s.Clear;
 SetLength(DateiNamen,0);
// FL 13.10.12 Bezeichnungen geùndert
 Such(1,'VermVerz Privat'       ,GETEVDATNAME(DR,'EVP'));
 Such(2,'VermVerz Gesellschaft' ,GETEVDATNAME(DR,'EVG'));
 Such(3,'VermVerz Ergùnzungsblatt I'  ,GETEVDATNAME(DR,'EV1'));
 Such(4,'VermVerz Ergùnzungsblatt II' ,GETEVDATNAME(DR,'EV2'));
 Such(5,'VermVerz Ergùnzungsblatt III',GETEVDATNAME(DR,'EV3'));
 Such(6,'VermVerz Nachbesserung',GETEVDATNAME(DR,'EVN'));
 Anlagen;

      //16.01.13:
      valdate(d,GetEVDate(GetTheLastVV));
      if d.tag<>0 then
       VerfNrDat.Text:=MakeStr(d);


end;

procedure TfVVEinlieferung.SengenbergerClick(Sender: TObject);
begin
 SubAufruf(2985);//30.01.13
end;

procedure TfVVEinlieferung.SetSexAnAnrede(aFemal, aMale, aUnknown, aDivers: boolean);
begin
  if aFemal
    then begin
           f755802l1.Sex.Itemindex    := 1;       // 2
// BW 06.02.23           f755802l1.Anrede.ItemIndex := 0;
           f755802l1.Anrede.Itemindex := 2; // BW 06.02.23
         end
    else
  if aMale
    then begin
           f755802l1.Sex.Itemindex    := 0;       // 1
// BW 06.02.23           f755802l1.Anrede.Itemindex := 2;
           f755802l1.Anrede.Itemindex := 1; // BW 06.02.23
         end
    else
  if aUnknown
    then f755802l1.Sex.Itemindex := 2
    else
  if aDivers
    then f755802l1.Sex.Itemindex := 3;
end;

procedure TfVVEinlieferung.ShowPDFClick(Sender: TObject);
begin
 try
  if VVListe.ItemIndex<>-1 then
  if Dokumentenname <> '' // BW 24.11.22
    then New_WShowBMP3('Vermùgensverzeichnis',
                EVPath+Dokumentenname,False)                                                        { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    else New_WShowBMP3('Vermùgensverzeichnis',
                VVListeDatNamen.items[VVListe.ItemIndex],False);                                    { TODO 2 -oXEFrank -cS_2_Ansi : XE }
 except
  on EXC_:Exception do
   ExceptionHintUser0('05340',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TfVVEinlieferung.VAK3ArtChange(Sender: TObject);
begin
 //11.12.12:
 safeid3.enabled :=VAK3Art.itemindex=0;
 safeid3L.enabled:=safeid3.enabled;
end;

procedure TfVVEinlieferung.VerfNrChange(Sender: TObject);
begin
 if trim(verfnr.text)='' then
  verfok.brush.color:=clwhite
 else
 if checkLuhn(verfnr.text) then
  verfok.brush.color:=clgreen
 else
  verfok.brush.color:=clred;
end;

procedure TfVVEinlieferung.VerfNrDatExit(Sender: TObject);
begin
 ChkWarn;//25.03.14
end;

Procedure tfVVEinlieferung.ChkWarn;//25.03.14
var T,D:Datumstyp;
begin
 DatumLesen(T);
 if T.Monat=1 then begin
  T.Monat:=12;
  Dec(T.Jahr);
 end
 else
  Dec(T.Monat);
 //Tag ggf. auùerhalb Range, aber das ist egal 
 ValDate(D,VerfNrDat.Text);                                                                         { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
 Warn.Visible:=Absolut_31(D)<Absolut_31(T);//ùlter als 1 Monat

 //06.02.17:
 DatumLesen(T);
 if nachbesserung.checked and (absolut_31(d)=absolut_31(t)) then begin
  TDialogs.MyMessagedlg('~VermVerz Datum'#13+
                'Bei einer Korrektur ist das Datum der ursprùnglichen Abnahme und nicht das Datum der Nachbesserung anzugeben!',mtwarning,[mbabort],0);
  try
   if VerfNrDat.CanFocus then//03.07.20
    VerfNrDat.SetFocus;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05341',EXC_.Message,EXC_.ClassName);
  end;
  exit;
 end;
 
 //27.01.17:
 DatumLesen(T);
 if absolut_31(d)>absolut_31(t) then begin
  TDialogs.MyMessagedlgOK('~VermVerz Datum'#13+
                'Das VermVerz Datum in der Zukunft ist unzulùssig.',mtwarning,[mbabort],0);
  VerfNrDat.Text:=GetMakeStr;
 end;
 if Neu.Checked then begin
  DatumLesen(T);
  Dec(T.Jahr,2);
  if absolut_31(d)<absolut_31(t) then begin
   TDialogs.MyMessagedlgOK('~VermVerz Datum'#13+
                  'Das VermVerz Datum ùlter als 2 Jahre ist unzulùssig.',mtwarning,[mbabort],0);
   VerfNrDat.Text:=GetMakeStr;
   try
    if VerfNrDat.CanFocus then//03.07.20
     VerfNrDat.SetFocus;
   except
    on EXC_:Exception do
     ExceptionHintUser0('05342',EXC_.Message,EXC_.ClassName);
   end;
  end;
 end;

end;

procedure TfVVEinlieferung.VerfNrExit(Sender: TObject);
var d:datumstyp;
begin
 //02.10.12:
 f755802l1.Verfahrensnummer:=Verfnr.Text;

 //14.01.13:
 valdate(d,VerfNrDat.text);                                                                         { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
 f755802l1.VerfDatum:=D;//15.01.13 VerfahrensnummerDatum:=MakeStr_to_Long(d);

end;

procedure TfVVEinlieferung.ZApersClick(Sender: TObject);
begin
 if zapers.checked then
  zapost.checked:=false;
end;

procedure TfVVEinlieferung.ZAPostClick(Sender: TObject);
begin
 if ZAPost.checked then
  ZApers.checked:=False;
end;

procedure TfVVEinlieferung.ZenVGTransferChange(Sender: TObject);
begin
 if not(InInit) then begin
  ZenSig:= ZenVGTransfer.ItemIndex+1;
  WriteStammdaten;
 end;
end;

procedure TfVVEinlieferung.Zu882Click(Sender: TObject);
begin
 LiesDRII(DR.jahr,DR.dr);//19.03.13
 Eintrag882c(DR,DR_II.Bezirksnummer,S_Adresse,TRUE,''

               ,TRUE//13.12.17 WAIT!

 );//27.02.13
end;

procedure TfVVEinlieferung.AdvOfficePager1Changing(Sender: TObject; FromPage,
  ToPage: Integer; var AllowChange: Boolean);
begin
   if Topage = 1 then
    begin
      if false // FL 19.02.13 Loeschung.Checked
        then begin
               // FL #todo KILL FORMFelder
               f755802l1.Scrollbox1.Visible := False;
               f755802l1.Panel2.Color := Integer(RGB($DB,$DB,$DB));
               Panel1.Color := Integer(RGB($DB,$DB,$DB));
             end
        else begin
               f755802l1.Scrollbox1.Visible := true;
               f755802l1.Panel2.Color := clBtnFace;
               Panel1.Color := clBtnFace;

               if BelegeEingaben then
                 begin

                   f755802l1.Adr2Anfrage(S_Adresse);

                   CheckVV_and_LoadinForm;

                   BelegeEingaben := false;
                 end;

               //03.11.12:
               if (F755802l1.name.text='') and
                  (F755802l1.firmenname.text='') then
                F755802l1.Adr2Anfrage(S_Adresse);

             end;

       OK2.Enabled        := true; // FL 14.10.12
{?? was soll denn das?
       OK2.Color          := Integer(RGB($BF,$FF,$BF));
       OK2.ShadowColor    := Integer(RGB($9B,$FF,$9B));
       OK2.Font.Color     := clBlac k;
       OK2.OverTextColor  := clBtnText; // FL eigentlich clWindowText aber das ist richtiger...
       OK2.PressTextColor := clBtnText; // FL eigentlich clWindowText aber das ist richtiger...}

       if Neu.Checked or Erneut.Checked
         then begin
                OK2.Caption := '&OK, VermVerz an ZenVG per EGVP / eBO';

  //26.08.15:
  if Neu.checked and f755802l1.Dublette then
   CLOSE;

                if Erneut.Checked
                  then begin
                         f755802l1.SetVermoegensverzeichnis_Art := 2; // Erneute Vermùgensauskunft, ù 802d ZPO
                         f755802l1.SetKorrektur                 := 5; // Vorzeitige Ersetzung einer Vermùgensauskunft
                       end
                  else begin
                         f755802l1.SetVermoegensverzeichnis_Art := 0; // Neue Vermùgensauskunft
                         f755802l1.SetKorrektur                 := 0; // GVS-INTERN NIX
                       end;
              end
//         else if Nachbesserung.Checked  // FL 17.12.13
           else if Nachbesserung.Checked or Korrektur.Checked
                then begin
                       OK2.Caption := '&OK, korrigiertes VermVerz an ZenVG';

// FL 17.12.13         f755802l1.SetVermoegensverzeichnis_Art   := 1; // Nachbesserung der Vermùgensauskunft;
                       if Korrektur.Checked  // FL 17.12.13 NEU
                         then f755802l1.SetVermoegensverzeichnis_Art   := 0  // Neu
                         else f755802l1.SetVermoegensverzeichnis_Art   := 1; // Nachbesserung der Vermùgensauskunft;

                       if Irrig.Checked
                         then f755802l1.SetKorrektur            := 2  // Korrektur irriger Eintrag
                         else f755802l1.SetKorrektur            := 1  // Korrektur
                     end
                else if Loeschung.Checked
                       then begin
                              OK2.Caption := '&OK, VermVerz an ZenVG zur Lùschung';//20.10.12 '&OK, Vermùgensverzeichnis im ZenVG per EGVP = LùSCHEN =';
{                              OK2.Color := Integer(RGB($FF,$80,$80));
                              OK2.ShadowColor := clRed;
                              OK2.Font.Color := clWhite;
                              // FL 17.10.12 Das ist echt die schow... Geil....
                              OK2.PressTextColor  := clBtnText;
                              OK2.OverTextColor   := clRED;}

                              MakePDFDummy(FertigesPDF);

                              f755802l1.SetVermoegensverzeichnis_Art   := 1; // 0 geht auch

                              if Irrig.Checked
                                then f755802l1.SetKorrektur            := 4  // Lùschung irriger Eintrag
                                else f755802l1.SetKorrektur            := 3  // Lùschung
                            end
                       else begin
                              OK2.Caption := 'Fehler, ùertragungsart wùhlen'; // Kann nicht passieren
                              OK2.Enabled := false;
                            end;
       f755802l1.InView(true);
       f755802l1.Timer1Timer(NIL); // BW 23.10.23 - AutoRepeat Return bug
    end
    else if Assigned(f755802l1) then  // FL 19.08.17
           f755802l1.InView(false);
end;

procedure TfVVEinlieferung.BitBtn2Click(Sender: TObject);
begin
 if TDialogs.MyMessagedlgOK('Wirklich abbrechen?',mtconfirmation,[mbyes,mbno],0)=mryes then
  Close;
end;

procedure TfVVEinlieferung.OnePDFBtnClick(Sender: TObject);
var 
    a,i:Integer;
    //i:integer;
begin
 a:=0;
 for i := 0 to Liste.Count - 1 do
 if Liste.Checked[i] then
  inc(a);
 if a=0 then begin
  TDialogs.MyMessageDlgOK('Sie haben kein einziges Dokument zur Erstellung des zu ùbertragenden PDF ausgewùhlt. '+
               'Daher gibt es auch nichts anzuzeigen oder zu bearbeiten.',mtwarning,[mbabort],0);
  Exit;
 end;
 DoOnePDF;
end;

procedure TfVVEinlieferung.AddDocumentClick(Sender: TObject);
var e,n:ansistring;
begin
 //Add
 N:=WinGetFile('Hinzuzufùgendes Dokument','PDF, JPG oder BMP Dokument','*.PDF;*.BMP;*.JPG');
 if N>'' then begin
  E:=AnsiUppercase(ExtractFileExt(N));                                                              { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  if (E='.PDF') or (E='.BMP') or (E='.JPG') then begin
   Liste.Items.Add(ExtractFileName(N));
// FL 13.10.12   FileNamen.Items.Add(N);

   SetLength(DateiNamen,length(DateiNamen)+1);
   DateiNamen[High(DateiNamen)]:=N;

   Liste.Checked[Liste.Items.Count-1]:=TRUE;
  end
  else
   WFehler('Nur PDF, JPG oder BMP!',2);
 end;
end;

procedure TfVVEinlieferung.Button1Click(Sender: TObject);
begin
// WinStamm1
end;

procedure TfVVEinlieferung.ChangeEintragGrund(Sender: TObject);
var d:datumstyp;
begin

 //20.10.12:
 irrig.enabled:=not(neu.checked) and not(erneut.checked);
 VerfNr.Enabled:=not(Neu.Checked);
 VerfNrL.Enabled:=VerfNr.Enabled;
 //07.07.21 VerfNrL2.Enabled:=VerfNr.Enabled;
 VerfOK.Enabled:=VerfNr.Enabled;
 if not(VerfNr.Enabled) then
  VerfNr.Text:='';
 //14.01.13:
 VerfNrDat.Enabled :=VerfNr.Enabled;
 VerfNrDatL.Enabled:=VerfNr.Enabled;

 //14.01.13:
 if VerfNr.Enabled then begin
  VerfNr.Text   :=S_Adresse.VerfNr;
  if not(erneut.checked) then//13.08.15
   VerfNrDat.Text:=MakeStr(S_Adresse.EinlieferDatum);//15.01.13
 end;

 NachbesserHint.Visible:=Nachbesserung.Checked;

 //13.07.19:
 Aktenzeichen_Empfaenger.Enabled :=Nachbesserung.Checked;
 Aktenzeichen_EmpfaengerL.Enabled:=Nachbesserung.Checked;

 //02.10.12:
 { FL 17.10.12 1. war falsch gild nur fùr SV 2. passier an anderer Stelle
 if Neu.checked then
  f755802l1.SetEintragungsGrund:=0
 else
 if Nachbesserung.checked then
  f755802l1.SetEintragungsGrund:=1
 else
  f755802l1.SetEintragungsGrund:=2;
  }

(*20.10.12 quatsch, natùrlich muss der gv die parteien informieren kùnnen
  if loeschung.Checked then // FL 17.10.12
    begin
      KopieGl.Checked := false;
      KopieS.Checked  := false;
    end;*)

  if not(neu.checked) then begin//20.10.12 loeschung.Checked or Nachbesserung.Checked or Erneut.Checked then
   ValDate(D,VerfnrDat.Text);//14.01.13                                                             { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
   if VerfNr.Enabled and
      ((trim(VerfNr.Text)='')
       or (D.Tag=0))//14.01.13
      then begin
      TDialogs.MyMessageDlgOK('Sie mùssen die Verfahrensnummer nebst Datum ausfùllen, die als Referenzen gelten.'#13#13+
                   'Da diese nicht gespeichert waren, mùssen Sie diese ùber das Vollstreckungsportal ermitteln.',
                   mtwarning,[mbok],0);
      try
       if verfnr.enabled then
        if VerfNr.CanFocus and (AdvOfficePager1.ActivePage=Grundlagen) then//03.07.20, 17.10.20
         VerfNr.SetFocus;
      except
       on EXC_:Exception do
        ExceptionHintUser0('05343',EXC_.Message,EXC_.ClassName);
      end;
   end;
  end;
end;

procedure TfVVEinlieferung.f755802l1Button1Click(Sender: TObject);
begin
  f755802l1.Button1Click(Sender);
end;

procedure TfVVEinlieferung.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 FormVVEinlieferaktiv:=false;//21.04.14
 action:=cafree;
end;

procedure TfVVEinlieferung.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 //31.07.14:
 if DoWait and CloseOK then
  canclose:=true
 else 

 //18.04.13:
 if not(Abgearbeitet) and CHkExit then begin
  if TDialogs.MyMessagedlgOK('~S T O P !!! B I T T E   L E S E N - HùUFIGER FEHLER!'#13+
                  'Bitte klicken Sie auf die groùe Schaltflùche OK!'#13#13+
                  'Wenn Sie dieses Fenster einfach beenden, bedeutet das:'#13+
                  ' - K E I N E  Quittung ùber die Einlieferung'#13+
                  ' - K E I N E  Speicherung der Verfahrensnr. beim Schuldner etc.'#13+
                  'Wenn Sie keine Kopie fùr den Gl. wùnschen o.ù., entfernen Sie einfach diese Hùkchen.'#13#13+
                  'Im Verarbeitungsfenster bleiben [EMPFOHLEN]?',
                  mtconfirmation,[mbyes,mbno],0)=mryes then

   //31.07.14:
   if DoWait then begin
    closeok:=true;
    canclose:=false;
   end
   else

    canclose:=FALSE;

 end
 else
 //31.07.14:
 if DoWait then begin
  closeok:=true;
  canclose:=false;
 end;

 f755802l1.FrameDeinit(canclose);//26.02.16

end;

procedure TfVVEinlieferung.FormCreate(Sender: TObject);
begin
  BelegeEingaben := false;
  Abgearbeitet   := false;//18.04.13
  FillChar(OptionalesDatumVVQuittung,SizeOf(OptionalesDatumVVQuittung),0);
end;

procedure TfVVEinlieferung.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if chkendkeys(key,shift) then begin
  if AdvOfficePager1.ActivePage=Grundlagen then
   ok1Click(sender)
  else
  if AdvOfficePager1.ActivePage=Angaben then
   ok2Click(sender)
   
  //20.03.14:
  else
  if AdvOfficePager1.ActivePage=Bestaetigt then
   ColorBitButton1Click(sender)

 end
 else
 if key = vk_escape then
  BitBtn2Click(Sender);
end;

procedure TfVVEinlieferung.FormShow(Sender: TObject);
begin
 if GetClientHeight>900 then
  Height:=900
 else
  Height:=GetClientHeight-20;

 left:=getleft2(width);
 top :=gettop2(height);
end;

procedure TfVVEinlieferung.Frist1Click(Sender: TObject);
begin
 if frist1.checked then begin
  EintragSVPost.checked:=false;
  EintragSVpers.checked:=false;
  KeineFrist.Checked:=FALSE;
  imtermin.checked:=false;
 end;

 //08.11.12:
 ZAPost.Visible:=frist1.checked and (DR_.Text>''); //14.05.13 and DrittGl.Checked;
 ZApers.Visible:=frist1.checked and (DR_.Text>''); //14.05.13 and DrittGl.Checked;
 OwnLabel8.Visible:=ZApers.Visible;
 OwnLabel9.Visible:=ZApers.Visible;

 EintragSVpersClick(sender);
end;

procedure TfVVEinlieferung.WiedervorlageClick(Sender: TObject);
var d:datumstyp;
begin
 //05.11.12:
 if Wiedervorlage.checked then begin
  valdate(d,WiedervorlageD.Text);                                                                   { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
  if d.tag=0 then begin
   Datumlesen(D);
   CheckFristen_UrlaubFeierTag(D,15,false,true);
   WiedervorlageD.Text:=makestr(d);
  end;
 end;
end;

procedure TfVVEinlieferung.WiedervorlageDExit(Sender: TObject);
var t,d:datumstyp;
begin
 try
  ValDate(D,WiederVorlageD.Text);                                                                   { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
  datumlesen(t);
  incdatum(t,15);
  if Absolut_31(D)<Absolut_31(t) then begin
   WFehler('Minimum 14 Tage Frist, also frùhestens am 15. Tag!',2);
   wiedervorlaged.Text:=makestr(t);
   try
    if wiedervorlaged.CanFocus then//03.07.20
     wiedervorlaged.SetFocus;
   except
    on EXC_:Exception do
     ExceptionHintUser0('05344',EXC_.Message,EXC_.ClassName);
   end;
  end;
 except
    on EXC_:Exception do
     ExceptionHintUser0('05345',EXC_.Message,EXC_.ClassName);
 end;
end;

{07.12.12
procedure TfVVEinlieferung.WMSize(var M : TWMSIZE) ;
begin
  if M.SizeType=Size_Minimized
    then begin
           ShowWindow(Handle,Sw_Hide) ;
           M.Result := 0;
         end
    else inherited;
end;
}

Procedure DoSavePDFGlHint;//27.02.13
var
   PDFLibrary : IQuickPDF;
begin
 try
    PDFLibrary := TQuickPDF.Create(false);
    PDFLibrary.LoadFromFile(name,'');
    PDFLibrary.SelectPage(1);
    AddVVGlHint(PDFLIBRARY,false);
    PDFLibrary.SaveToFile(name);
 except
   on EXC_:Exception do
     ExceptionHintUser0('05346',EXC_.Message,EXC_.ClassName);
 end;
end;

Procedure PrintPDFGlHint;//27.02.13
VAR PrintDialog1:TPrintDialog;
    abort:boolean;
    y:byte;
    aa:ta;
    dr:nr_typ;
    c:VCLChar;
    Az,
    Sache,
    Body:AnsiString;
    Safe:Shortstring;
    S:Adressentyp;
    SpoolHeader : tSpoolHeader;
begin
 fillchar(dr,sizeof(dr),0);
 try
  Leiste.PDFOpen.FileName:='';

  //Reihenfolge geùndert!

  safe:='';
  //25.06.13:
  Az:='';
  if Vorgabe.DR>0 then begin
   c:=t_end;
   DR:=Vorgabe;
  end
  else
   GETDRIINr( c, DR.DR, DR.Jahr, 'Registerbezug - ESC = kein Bezug gewùnscht!');
  if c<>t_esc then begin
   LiesDRII(DR.Jahr,DR.DR);
   S:=AskMehrereAdressen(DR_II.Schuldner,DR,False,DEK.Schuldner);//MODAL!
//26.07.13   Sache:='Sie ./. '+GetAdresseFull(S);
   Sache:=GetAdresseFull(DR_II.Glaubiger)+' ./. '+GetAdresseFull(S);
   Az   :=DR_II.GAktz;
   if Az='' then
    Az:=DR_II.GVAktz;
   if Az>'' then
    Az:='Ihr Zeichen: '+Az+#13#10;
   Body:=GetBodyVVanGl(DR,Az,Sache);

   //20.11.13:
   if DR_II.Glaubiger_V.SafeID>'' then
    safe:=DR_II.Glaubiger_V.SafeID
   else
    safe:=DR_II.Glaubiger.SafeID;

  end
  else
   Body:='';

  repeat
//02.12.13   initpopup(aa);
   preinitpopupmodal(aa);
   aa[1]:='Mit Belehrung fùr Glùubiger ausdrucken (Druck/Fax)';
   aa[2]:='Mit Belehrung fùr Glùubiger speichern';
   aa[3]:='Mit Belehrung fùr Glùubiger per E-Mail senden';
   aa[4]:='Mit Belehrung fùr Glùubiger per EGVP / eBO senden';

   aa[5]:='Standardordner ùndern (momentan '+PDFDirGlHint+')';
{22.05.13
   if DIP[85] and 128=0 then
    aa^[5]:='Standardordner ùndern auf '+EVPath+' (bisher '+GetDwlsPath+')'
   else
    aa^[5]:='Standardordner ùndern auf '+GetDwlsPath+' (bisher '+EVPath+')';}

//02.12.13    y := Pop Up(C,AA^,0,'PDF Vermùgensverzeichnis',1,TRUE,FALSE,0);
   y := WinPopUpModal( C, AA, 0, 'PDF Vermùgensverzeichnis',1,TRUE,FALSE,0);

   //20.04.13:
   if y=5 then begin

    SelectDir(PDFDirGlHint);
    WriteStammdaten;
{22.05.13
    if dip[85] and 128=0 then
     DIP[85]:=DIP[85] or 128
    else
     DIP[85]:=DIP[85] and (255-128);
    WriteOneDip(85);}
   end;

//02.12.13   deinitpopup(aa);
  until (c=t_esc) or (y<>5);//20.04.13

{22.05.13
  //20.04.13:
  if dip[85] and 128=128 then
   Leiste.PDFOpen.InitialDir:=EVPath
  else

   Leiste.PDFOpen.InitialDir:=GetDwlsPath;}
  Leiste.PDFOpen.InitialDir:=PDFDirGlHint;//22.05.13

  if (c<>t_esc) and Leiste.PDFOpen.Execute then begin

{ne, s.u.   //20.11.13:
   if safe>'' then
    GlobalOeffneSPOOLERInital:='[SAFE:'+Safe+']';}

   if c<>t_esc then begin

    //22.05.13:
    if AnsiUpperCase(ExtractFilePath(Leiste.PDFOpen.FileName))<>AnsiUpperCase(TrailingBs(PDFDirGlHint)) then begin
     PDFDirGlHint:=ExtractFilePath(Leiste.PDFOpen.FileName);                                        { TODO 2 -oXEFrank -cS_2_Short : XE }
     WriteStammdaten;
    end;

    Case y of
     1:begin
        PrintDialog1:=tPrintDialog.Create(Application);
        DoPrintPDFFile(PrintDialog1,
                       Leiste.PDFOpen.FileName,true,true,abort);                                    { TODO 2 -oXEFrank -c'TFileName' zu 'ShortString' : XE }
        PrintDialog1.Free;
       end;
     2:begin
        AddVVGlHintAndSave(Leiste.PDFOpen.FileName,true);                                           { TODO 2 -oXEFrank -c'TFileName' zu 'ShortString' : XE }
        WFehler(Leiste.PDFOpen.FileName+' gespeichert mit Vermerk',2);
       end;
       //DoSavePDFGlHint(Leiste.PDFOpen.FileName);
     3:begin
        AddVVGlHintAndSave(Leiste.PDFOpen.FileName,true);                                           { TODO 2 -oXEFrank -c'TFileName' zu 'ShortString' : XE }
        InitSpoolHeader(SpoolHeader, eDRII, '', DR.DR, DR.Jahr );//kein DR etc.
        Wemaile(Leiste.PDFOpen.FileName,
               DR,False,
               '',//VorgabeeMail,
               'Abgenommene Vermùgensauskunft '+DRToString(DR),//VorgabeSubj:Shortstring;
               '',//VorgabeAnlage:AnsiString;//16.08.12 Shortstring
               Body,//25.06.13 '',
               False,False,False,//AutoStart,EditMode,_DelCollect
               true,true, SpoolHeader);//sign+crypt                                                              { TODO 2 -oXEFrank -c'TFileName' zu 'AnsiString' : XE }

       end;
     4:begin
        AddVVGlHintAndSave(Leiste.PDFOpen.FileName,true);//25.06.13                                 { TODO 2 -oXEFrank -c'TFileName' zu 'ShortString' : XE }
        InitSpoolHeader(SpoolHeader, eDRII, 'Abgenommene Vermùgensauskunft', DR.DR, DR.Jahr );//kein DR etc.
        EGVP_MakeMail(true,safe,//20.11.13 '',// SAFEID,
                     'Abgenommene Vermùgensauskunft',//Betreff
                     Body,//25.06.13 '',// Body,//Msg
                     DRToString(DR),//25.06.13  '', // DRToString(DR),
                     Az,//25.06.13 '', // Az,
                     tSignatur(GlSig-1),//02.12.14 tSignatur(GlSig),
                     Leiste.PDFOpen.FileName,                                                       { TODO 2 -oXEFrank -c'TFileName' zu 'AnsiString' : XE }
                     EGVPMsgTESTONLY,
{// BW 27.02.23 Ticket #1495                     (DIP[88] and 128=128),//11.12.13} TRUE,
                     DR,false,
                     false,DLeer, NIL, SpoolHeader,
                     TRUE // BW 28.02.23 Damit das PDF Util-Fenster aufgeht. SOnst fùhrt es zu Verwirrung
                     );

                     //27.08.13:
        TDialogs.MyMessagedlgOK('ùbergeben an EGVP / eBO! Wie immer wird EGVP / eBO die Daten im eingestellten Intervall ùbernehmen. '+
                     'Wenn Sie SOFORT den Versand wùnschen, klicken Sie im EGVP / eBO auf "Fachdaten ùbernehmen".'#13#13+
                     'EGVP / eBO kann NUR im eingestellten Intervall holen und versenden. Das kùnnte auch nur der Hersteller von EGVP / eBO ùndern.',
                     mtinformation,[mbok],0);

       end;
    end;
   end;
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05347',EXC_.Message,EXC_.ClassName);
 end;
end;

end.