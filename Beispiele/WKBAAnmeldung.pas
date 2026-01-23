unit WKBAAnmeldung;//04.04.14

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WEingabe, Buttons, MyColorBitBtn, csEZform, ExtCtrls,
  AdvGlowButton, MyStdButton, AdvOfficeImage;

type
  TfKBAAnmeldung = class(TcsEZForm)
    Label1: TLabel;
    Eingabe1: TEingabe;
    Label2: TLabel;
    Eingabe2: TEingabe;
    Label3: TLabel;
    Eingabe3: TEingabe;
    BitBtn1: TMyStdButton;
    BitBtn2: TMyStdButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    MyStdButton2: TMyStdButton;
    MyStdButton1: TMyStdButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MyStdButton2Click(Sender: TObject);
    procedure MyStdButton1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

Function AskKBAAnmeldung(var knr, benutzer, pw, XMLFile:shortstring):boolean;

implementation

{$R *.dfm}

uses dek
, basis, wbasis, Drv, winsys, loadstd, wSupportstellen, eAktenSystem.Intf, eAktenSystem.D2007
, xj_SchriftgutObjectHandler   // FL 04.09.22
, wXJustizKundeneingabe        // FL 04.09.22
, Dialogs.Intf
, XML_Defs                     // FL 04.10.22
, xJustiz_GVS_EnumHandler.Intf // FL 28.10.22
, xJustiz_XML_Handler          // FL 02.11.22
;

Function AskKBAAnmeldung(var knr, benutzer, pw, XMLFile:shortstring):boolean;
var _dr:nr_typ;
    lError: String;
    SpoolHeader : tSpoolHeader;

  function SgO4KBA(Const aFilename,aDocname : String;out Error : String) : boolean;
  var
    lSgOWriter      : ISchriftgutObjectWriter;
    lMySafeID       : String;
  begin
    Result := true;
    try
      lSgOWriter := TSchriftgutObjectWriter.Create(aFilename,NIL);

      if lSgOWriter.GetSafeID(lMySafeID)
        then begin
               lSgOWriter.NachrichtenKopf(TGVSHandler.DoCreateGUID32,'','',lMySafeID,cKBAPostfachSafeID,'') // FL 11.11.22
                         .Sachgebiet('KBA Registrierung','NEU') // FL 06.09.22
                         .AddDokument(dkSonstige,aDocName)
             end
        else begin
               Result := false;
               Error  := 'Eigenes SafeID nicht gefunden! Bitte überprüfen Sie Ihre EGVP/eBO Einstellungen!';
             end;
    except
      On E : Exception do
       begin
         Error := E.Message; // FL 19.09.22
         Result := false;
       end;
    end;
  end;

begin
 Result :=False;
 xmlfile:=temppath+cKBARegXMLName; // FL 07.09.22 'Registrierung.xml';
 try
  if SgO4KBA(temppath+'xjustiz_nachricht.xml',cKBARegXMLName,lError) // FL 07.09.22
    then begin
            with TfKBAAnmeldung.create(application) do begin
             Eingabe1.Text:=KBAKnr;
             Eingabe2.Text:=KBABenutzer;
             ShowModal;
             if modalresult=mrok then begin
              Result:=true;
              knr:=Eingabe1.Text;
              benutzer:=Eingabe2.Text;
              pw:=Eingabe3.Text;
              Einmal_RegistrierungKBANeu(xmlFile,knr,benutzer,pw);

              //06.04.14:
              kbaknr:=Eingabe1.Text;
              kbabenutzer:=Eingabe2.Text;
              kbapw:=Eingabe3.Text;
              WriteStammdaten;

              XMLFile := XMLFile + ';' + temppath+'xjustiz_nachricht.xml';

              fillchar(_DR,sizeof(_DR),0);
              InitSpoolHeader(SpoolHeader, eIgnore, 'Registrierung Kopfstelle', 0, 0 );//kein DR etc.
              if EGVP_MakeMail(true,cKBAPostfachSafeID,                                             { TODO 2 -oXEFrank -cS_2_Short : XE }
                                  'Registrierung Kopfstelle',//Betreff
                                  '', //Leer!
                                  '','',//Az...
                                  tSignatur(NoSignatur),//19.05.14 Zen Sig),
                                  XMLFile,
                                  EGVPMsgTESTONLY,false,_DR,false,
                                  false, DLeer,NIL, SpoolHeader) then
               TDialogs.mymessagedlg('Die EGVP / eBO Nachricht ist erzeugt!',mtinformation,[mbok],0);
             end;
             Free;
            end;
         end
    else TDialogs.MyMessageDLG('SchriftgutObject konnte nicht erzeugt werden!'+#1313+lError,mtinformation,[mbok],0);
 except
  on EXC_:Exception do
   ExceptionHintUser0('03479',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TfKBAAnmeldung.BitBtn1Click(Sender: TObject);
begin
 if (trim(eingabe1.text)>'') and
    (trim(eingabe2.text)>'') and
    (trim(eingabe3.text)>'') then
  modalresult:=mrok
 else
  wfehler('Eingaben unvollständig',2);   

end;

procedure TfKBAAnmeldung.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if chkendkeys(key,shift) then
  modalresult:=mrok;
end;

procedure TfKBAAnmeldung.FormShow(Sender: TObject);
begin
 left:=getleft_modal(width);
 top :=gettop_modal(height);
end;

procedure TfKBAAnmeldung.MyStdButton1Click(Sender: TObject);
begin
 Unterstuetzung;
end;

procedure TfKBAAnmeldung.MyStdButton2Click(Sender: TObject);
begin
 showhilfeidx(helpcontext);
end;

end.
