unit wKontakt; //05.10.22

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, MLButton, ExtCtrls, ComCtrls, wownlabel, WEingabe, myform,
  AdvOfficePager, AdvOfficePagerStylers, AdvGlowButton, MyStdButton, Wmemo, WRadioButton, WCheckBox, AdvCombo;

type
  TFKontakt = class(TmyForm)
    Panel2: TPanel;
    Panel1: TPanel;
    OK: TMyStdButton;
    BitBtn20: TMyStdButton;
    MyStdButton2: TMyStdButton;
    Panel3: TPanel;
    Label2: TLabel;
    Grund: TOwnMemo;
    Label3: TLabel;
    EMail: TEdit;
    Tel: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Erreichbarkeit: TOwnMemo;
    gering: TmyRadioButton;
    Label6: TLabel;
    mittel: TmyRadioButton;
    hoch: TmyRadioButton;
    arbeitsunfaehig: TmyRadioButton;
    Betreff: TAdvComboBox;
    AnzZeichen: TLabel;
    Fehler: TLabel;
    Label9: TLabel;
    Gelesen: TmyCheckBox;
    Label10: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    AdvOfficePagerOfficeStyler1: TAdvOfficePagerOfficeStyler;
    PG: TAdvOfficePager;
    Anfrage: TAdvOfficePage;
    Neuerungswunsch: TAdvOfficePage;
    Panel6: TPanel;
    Panel5: TPanel;
    Label13: TLabel;
    MyStdButton1: TMyStdButton;
    MyStdButton3: TMyStdButton;
    MyStdButton4: TMyStdButton;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    OwnMemo1: TOwnMemo;
    BetreffNews: TAdvComboBox;
    Panel7: TPanel;
    Label18: TLabel;
    Fehlermeldung: TAdvOfficePage;
    Label16: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    OwnMemo2: TOwnMemo;
    myCheckBox1: TmyCheckBox;
    Label1: TLabel;
    Label21: TLabel;
    Panel4: TPanel;
    Label26: TLabel;
    MyStdButton5: TMyStdButton;
    MyStdButton6: TMyStdButton;
    MyStdButton7: TMyStdButton;
    Panel8: TPanel;
    Panel9: TPanel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    OwnMemo3: TOwnMemo;
    Edit3: TEdit;
    Edit4: TEdit;
    OwnMemo4: TOwnMemo;
    myRadioButton1: TmyRadioButton;
    myRadioButton2: TmyRadioButton;
    myRadioButton3: TmyRadioButton;
    myRadioButton4: TmyRadioButton;
    myCheckBox2: TmyCheckBox;
    BetreffFehler: TAdvComboBox;
    Label33: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Haeufigkeit: TAdvComboBox;
    AdvOfficePage1: TAdvOfficePage;
    Btn1: TMyStdButton;
    AdvGlowButton3: TMyStdButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MyStdButton2Click(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure GrundChange(Sender: TObject);
    procedure ChkFehler(Sender: TObject);
    procedure MyStdButton3Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure MyStdButton1Click(Sender: TObject);
    procedure OwnMemo1Change(Sender: TObject);
    procedure MyStdButton5Click(Sender: TObject);
    procedure Btn1Click(Sender: TObject);
    procedure AdvGlowButton3Click(Sender: TObject);
    procedure OwnMemo3Change(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
   okClicked:boolean;
   procedure ChkFehlerFehler(Sender: TObject);
   procedure ChkFehlerNews(Sender: TObject);
  end;

procedure WinKontakt(What:byte=0);

implementation

{$IFNDEF P367}
{$R *.DFM}
{$ENDIF}

uses wdek, dek, basis, wbasis, scaleform, loadstd, wemail, wMenuGVS, menuesys, eAktenSystem.Intf, eAktenSystem.D2007;

procedure WinKontakt(What:byte=0);
var f:tfKontakt;
    i:integer;
begin
 f:=tfKontakt.create(application);
 f.okClicked          :=false;
 f.Label7.caption     :=f.Label7.caption+#13+LizenzDZeiten;
 f.Label25.caption    :=f.Label25.caption+#13+LizenzDZeiten;
 f.Label37.caption    :=f.Label37.caption+#13+LizenzDZeiten;
 f.caption            :=f.caption+' '+LIZENZ1D;
 f.Erreichbarkeit.Text:=Kontaktaufnahme.Erreichbarkeit;
 f.Tel.Text           :=Kontaktaufnahme.Tel;
 f.EMail.Text         :=Kontaktaufnahme.EMail;
 f.Grund.OnChange     :=f.GrundChange;
 f.Tel.OnChange       :=f.GrundChange;
 f.EMail.OnChange     :=f.GrundChange;
 f.Erreichbarkeit.OnChange:=f.GrundChange;

 f.OwnMemo1.Onchange  :=f.OwnMemo1Change; 
 f.OwnMemo3.Onchange  :=f.OwnMemo3Change; 

 f.OwnMemo2.Text:=Kontaktaufnahme.Erreichbarkeit;
 f.Edit2.Text   :=Kontaktaufnahme.Tel;
 f.Edit1.Text   :=Kontaktaufnahme.EMail;
 f.OwnMemo4.Text:=Kontaktaufnahme.Erreichbarkeit;
 f.Edit4.Text   :=Kontaktaufnahme.Tel;
 f.Edit3.Text   :=Kontaktaufnahme.EMail;

 if f.Tel.Text='' then
  f.Tel.Text:=BezRec[AktBezRec].UserText[ 6];
 if f.Edit2.Text='' then
  f.Edit2.Text:=BezRec[AktBezRec].UserText[ 6];
 if f.Edit4.Text='' then
  f.Edit4.Text:=BezRec[AktBezRec].UserText[ 6];

 for i := 0 to f.Betreff.items.Count - 1 do
  //hmm f.Betreff.items[i]:=stringreplace(f.Betreff.items[i],'[PROG]',Progname,[rfignorecase]);
  f.Betreff.items[i]:=stringreplace(f.Betreff.items[i],'[PROG] ','',[rfignorecase]);

 if What=1 then
  f.PG.ActivePage:=f.Neuerungswunsch
 else
 if What=2 then
  f.PG.ActivePage:=f.Fehlermeldung
 else
  f.PG.ActivePage:=f.Anfrage;

end;

procedure TFKontakt.AdvGlowButton3Click(Sender: TObject);
begin
 SubAufruf(2584);//7,FBD+'ERZIEH.TXT'
end;

procedure TFKontakt.BitBtn20Click(Sender: TObject);
begin
 Close;
end;

procedure TFKontakt.Btn1Click(Sender: TObject);
begin
 AenderungUser;
end;

procedure TFKontakt.ChkFehler(Sender: TObject);
var s:string;
begin
 s:='';
 if okclicked then begin
  if length(Betreff.Text)<3 then
   s:=AddKomma(s,'Angabe des Betreffs ist zu kurz');
  if length(Grund.Text)<20 then
   s:=AddKomma(s,'Angabe des Grundes ist zu kurz');
  if length(Erreichbarkeit.Text)<5 then
   s:=AddKomma(s,'Erreichbarkeit Text ist zu kurz');
  if length(Tel.Text)<6 then
   s:=AddKomma(s,'Keine Telefonnummer');
  if (length(EMail.Text)<8) or (pos('@',EMail.Text)=0) then
   s:=AddKomma(s,'keine gltige E-Mail Adresse');
  if not(arbeitsunfaehig.checked) and not(hoch.checked) and not(mittel.checked) and not(gering.checked) then
   s:=AddKomma(s,'keine Dringlichkeit angegeben');
  if not(Gelesen.checked) then
   s:=AddKomma(s,'"Hinweis gelesen" nicht ausgewhlt');
 end;
 if s<>Fehler.caption then
  Fehler.caption:=s;
 Fehler.visible:=Fehler.caption>'';
end;

procedure TFKontakt.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if PG.ActivePage=Neuerungswunsch then begin
  Kontaktaufnahme.Erreichbarkeit:=OwnMemo2.Text;                                                    { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
  Kontaktaufnahme.Tel           :=Edit2.Text;                                                       { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
  Kontaktaufnahme.EMail         :=Edit1.Text;                                                       { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
 end
 else
 if PG.ActivePage=Fehlermeldung then begin
  Kontaktaufnahme.Erreichbarkeit:=OwnMemo4.Text;                                                    { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
  Kontaktaufnahme.Tel           :=Edit4.Text;                                                       { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
  Kontaktaufnahme.EMail         :=Edit3.Text;                                                       { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
 end
 else begin
  Kontaktaufnahme.Erreichbarkeit:=Erreichbarkeit.Text;                                              { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
  Kontaktaufnahme.Tel           :=Tel.Text;                                                         { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
  Kontaktaufnahme.EMail         :=EMail.Text;                                                       { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
 end;
 WriteStammdaten;
 action:=cafree;
end;

procedure TFKontakt.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var pp:integer;  
begin
 if ChkENDKeys(Key,Shift) then
  OKClick(Sender)
 else
 if Key=vk_escape then
  NOKClick(Sender)
 else
 if key=vk_next then begin
  PG.SelectNextPage(TRUE);
 end
 else
 if key=vk_prior then begin
  try
   PP:=PG.ActivePage.PageIndex;
   repeat
    if PP>0 then
     PP:=PP-1
    else
     PP:=PG.AdvPageCount-1;
   until PG.AdvPages[PP].TabVisible;
   PG.ActivePage:=PG.AdvPages[PP];
  except
   on EXC_:Exception do
    ExceptionHintUser0('03845xy',EXC_.Message,EXC_.ClassName);
  end;
 end;
end;

procedure TFKontakt.NOKClick(Sender: TObject);
begin
 CLOSE;
end;

procedure TFKontakt.OKClick(Sender: TObject);

 procedure Send;
 var s3,s2,s:string;
     SpoolHeader : tSpoolHeader;
 begin
  s:=crunch(BezRec[AktBezRec].UserText[2])+' '+crunch(BezRec[AktBezRec].UserText[1])+', Kundennr. '+KNR+', '+Laender[Land]+#13;
  s:=s+'Versand ber '+ProduktName+'-Kontaktformular, iNr '+Internal+#13;
  s3:='';
  if arbeitsunfaehig.checked then begin
   s2:=arbeitsunfaehig.caption;
   s3:='[!!] ';
  end
  else
  if hoch.checked then begin
   s2:=hoch.caption;
   s3:='[!] ';
  end
  else
  if mittel.checked then
   s2:=mittel.caption
  else
   s2:=gering.caption;
  s:=s+
     'Dringlichkeit '+s2+#13+
     'Grund: '+grund.text+#13#13+
     'E-Mail '+email.text+#13+
     'Tel. '+tel.text+#13+
     'Erreichbarkeit '+erreichbarkeit.text+#13#13#13; // FL 27.06.24 -e
  s3:=s3+'Anfrage wegen '+Betreff.text;
  InitSpoolHeader(SpoolHeader, eIgnore, Betreff.Text, 0, 0 );//kein DR etc.
  Wemaile('',LeerDR,False,LizenzeMailD2,
          s3,
          '',s,False,TRUE,false, SpoolHeader);                                                                   { TODO 2 -oXEFrank -cS_2_Short : XE }  { TODO 2 -oXEFrank -cS_2_Ansi : XE }
 end;

begin
 okClicked:=true;
 Betreff.Text:=trim(Betreff.Text);
 Grund.Text  :=trim(Grund.Text);
 Tel.Text    :=trim(Tel.Text);
 EMail.Text  :=trim(EMail.Text);
 Erreichbarkeit.Text:=trim(Erreichbarkeit.Text);
 ChkFehler(Sender);

 if Fehler.Caption='' then begin
  Send;
  Close;
 end
 else
  wFehler('Bitte Hinweise beachten und Angaben vervollstndigen!',2);
end;

procedure TFKontakt.OwnMemo1Change(Sender: TObject);
begin
 Label15.Caption:=inttostr(GetWindowTextLength(OwnMemo1.Handle))+' / '+inttostr(OwnMemo1.MaxLength);
end;

procedure TFKontakt.OwnMemo3Change(Sender: TObject);
begin
 Label32.Caption:=inttostr(GetWindowTextLength(OwnMemo1.Handle))+' / '+inttostr(OwnMemo1.MaxLength);
end;

procedure TFKontakt.FormShow(Sender: TObject);
begin
 ScaleFormMy(Self);
 top :=gettop2(height);
 left:=getleft2(width);
end;

procedure TFKontakt.GrundChange(Sender: TObject);
begin
 AnzZeichen.Caption:=inttostr(GetWindowTextLength(Grund.Handle))+' / '+inttostr(Grund.MaxLength);
 ChkFehler(Sender);
end;

procedure TFKontakt.MyStdButton2Click(Sender: TObject);
begin
 showhilfeidx(helpcontext);
end;

procedure TFKontakt.MyStdButton3Click(Sender: TObject);
begin
 Close;
end;

procedure TFKontakt.MyStdButton1Click(Sender: TObject);
//Neuerungswnsche

 procedure Send;
 var s:string;
     SpoolHeader : tSpoolHeader;
 begin
  s:=crunch(BezRec[AktBezRec].UserText[2])+' '+crunch(BezRec[AktBezRec].UserText[1])+', Kundennr. '+KNR+', '+Laender[Land]+#13;
  s:=s+'Versand ber '+ProduktName+'-Formular, iNr '+Internal+#13;
  s:=s+
     'Neuerungswunsch '+ownmemo1.text+#13#13+
     'E-Mail '+edit1.text+#13+
     'Tel. '+edit2.text+#13+
     'Erreichbarkeit '+ownmemo2.text+#13#13#13; // FL 27.06.24 -e
  InitSpoolHeader(SpoolHeader, eIgnore, 'Neuerungswunsch', 0, 0 );//kein DR etc.
  Wemaile('',LeerDR,False,LizenzeMailD2,
          '[Neuerungswunsch] '+BetreffNews.text,
          '',s,False,TRUE,false, SpoolHeader);                                                                   { TODO 2 -oXEFrank -cS_2_Short : XE }  { TODO 2 -oXEFrank -cS_2_Ansi : XE }
 end;

begin

 //10.10.22:
 ChkFehlerNews(Sender);
 if Label22.caption>'' then
 else

 if myCheckBox1.checked then begin
  Send;
  Close;
 end
 else
  wfehler('Bitte den "Hinweis gelesen" Haken setzen',2);
end;

procedure TFKontakt.MyStdButton5Click(Sender: TObject);
//Fehlermeldungen

 procedure Send;
 var s3,s2,s:string;
     SpoolHeader : tSpoolHeader;
 begin
  s:=crunch(BezRec[AktBezRec].UserText[2])+' '+crunch(BezRec[AktBezRec].UserText[1])+', Kundennr. '+KNR+', '+Laender[Land]+#13;
  s:=s+'Versand ber '+ProduktName+'-Formular, iNr '+Internal+#13;

  s3:='';
  if myRadioButton4.checked then begin
   s2:=myRadioButton4.caption;
   s3:='[!!] ';
  end
  else
  if myRadioButton3.checked then begin
   s2:=myRadioButton3.caption;
   s3:='[!] ';
  end
  else
  if myRadioButton2.checked then
   s2:=myRadioButton2.caption
  else
   s2:=myRadioButton1.caption;
  s:=s+
     'Fehlergrad '+s2+#13+
     'Fehlermeldung '+ownmemo3.text+#13+ // BW 04.01.23 Schreibfehler
     'Fehlerhufigkeit '+Haeufigkeit.text+#13#13+
     'E-Mail '+edit3.text+#13+
     'Tel. '+edit4.text+#13+
     'Erreichbarkeit '+ownmemo4.text+#13#13#13; // FL 27.06.24 -e
  InitSpoolHeader(SpoolHeader, eIgnore, 'Fehlermeldung', 0, 0 );//kein DR etc.
  Wemaile('',LeerDR,False,LizenzeMailD2,
          '[Fehlermeldung] '+BetreffFehler.text,
          '',s,False,TRUE,false, SpoolHeader);                                                                   { TODO 2 -oXEFrank -cS_2_Short : XE }  { TODO 2 -oXEFrank -cS_2_Ansi : XE }
 end;

begin

 if Label34.caption>'' then
 else

 if myCheckBox2.checked then begin
  Send;
  Close;
 end
 else
  wfehler('Bitte den "Hinweis gelesen" Haken setzen',2);
end;

procedure TFKontakt.ChkFehlerFehler(Sender: TObject);
var s:string;
begin
 s:='';
 if okclicked then begin
  if length(BetreffFehler.Text)<3 then
   s:=AddKomma(s,'Angabe des Betreffs ist zu kurz');
  if length(OwnMemo3.Text)<20 then
   s:=AddKomma(s,'Angabe des Grundes ist zu kurz');
  if length(OwnMemo4.Text)<5 then
   s:=AddKomma(s,'Erreichbarkeit Text ist zu kurz');
  if length(Edit4.Text)<6 then
   s:=AddKomma(s,'Keine Telefonnummer');
  if length(Haeufigkeit.Text)<3 then
   s:=AddKomma(s,'Keine Hufigkeit angegeben');
  if (length(Edit3.Text)<8) or (pos('@',Edit3.Text)=0) then
   s:=AddKomma(s,'keine gltige E-Mail Adresse');
  if not(myRadioButton1.checked) and not(myRadioButton2.checked) and not(myRadioButton3.checked) and not(myRadioButton4.checked) then
   s:=AddKomma(s,'keine Dringlichkeit angegeben');
  if not(mycheckbox2.checked) then
   s:=AddKomma(s,'"Hinweis gelesen" nicht ausgewhlt');
 end;
 if s<>Label34.caption then
  Label34.caption:=s;
 Label34.visible:=Label34.caption>'';
end;

procedure TFKontakt.ChkFehlerNews(Sender: TObject);
var s:string;
begin
 s:='';
 if okclicked then begin
  if length(BetreffNews.Text)<3 then
   s:=AddKomma(s,'Angabe des Betreffs ist zu kurz');
  if length(OwnMemo1.Text)<20 then
   s:=AddKomma(s,'Angabe des Grundes ist zu kurz');
  if length(OwnMemo2.Text)<5 then
   s:=AddKomma(s,'Erreichbarkeit Text ist zu kurz');
  if length(Edit2.Text)<6then
   s:=AddKomma(s,'Keine Telefonnummer');
  if (length(Edit1.Text)<8) or (pos('@',Edit1.Text)=0) then
   s:=AddKomma(s,'keine gltige E-Mail Adresse');
  if not(myCheckBox1.checked) then
   s:=AddKomma(s,'"Hinweis gelesen" nicht ausgewhlt');
 end;
 if s<>Label22.caption then
  Label22.caption:=s;
 Label22.visible:=Label22.caption>'';
end;

end.

