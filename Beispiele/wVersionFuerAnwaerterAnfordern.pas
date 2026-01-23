unit wVersionFuerAnwaerterAnfordern;//05.09.14

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, csezForm, StdCtrls, wcombobox, WEingabe, wownlabel, ExtCtrls,
  Buttons, MyColorBitBtn, Mask, WDatum, WDatumlong, AdvGlowButton, jpeg,
  HTMLabel, AdvOfficePager, Wmemo, MyStdButton, AdvOfficeImage;

type
  TFAnwaerterMenu = class(TcsezForm)
    Panel1: TPanel;
    OwnLabel3: TOwnLabel;
    OwnLabel14: TOwnLabel;
    Vorname: TEingabe;
    Nachname: TEingabe;
    Anrede: TCombo;
    JS: TCombo;
    OwnLabel1: TOwnLabel;
    OwnLabel2: TOwnLabel;
    OwnLabel4: TOwnLabel;
    OwnLabel5: TOwnLabel;
    OwnLabel6: TOwnLabel;
    Label3: TOwnLabel;
    Label4: TOwnLabel;
    Label5: TOwnLabel;
    Str: TEingabe;
    Ort: TEingabe;
    Mail: TEingabe;
    Tel: TOwnLabel;
    Telefon: TEingabe;
    OwnLabel7: TOwnLabel;
    OwnLabel8: TOwnLabel;
    OwnLabel9: TOwnLabel;
    Geb: TDatumlong;
    OwnLabel10: TOwnLabel;
    Ausbilder: TEingabe;
    OwnMemo1: TOwnMemo;
    Panel2: TPanel;
    OK: TMyStdButton;
    MyStdButton1: TMyStdButton;
    AdvOfficeImage1: TAdvOfficeImage;
    BLand: TCombo;
    Ersteinrichtung: TCheckBox;
    OwnLabel11: TOwnLabel;
    AB: TEingabe;
    BJ: TEingabe;
    OwnLabel12: TOwnLabel;
    AG: TEingabe;
    MyStdButton2: TMyStdButton;
    MyStdButton3: TMyStdButton;
    AdvOfficeImage2: TAdvOfficeImage;
    OwnLabel13: TOwnLabel;
    OwnLabel15: TOwnLabel;
    procedure JSEnter(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ChkChange(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure MyStdButton1Click(Sender: TObject);
    procedure MyStdButton2Click(Sender: TObject);
    procedure MyStdButton3Click(Sender: TObject);
  private
    { Private-Deklarationen }
   procedure ShowHeader;
   procedure Lade;
   procedure Save;
  public
    { Public-Deklarationen }
  end;

var FAnwaerterMenu: TFAnwaerterMenu=nil;

procedure AnwMenu;

implementation

{$R *.dfm}

uses dek
, Dialogs.Intf, eAktenSystem.Intf, eAktenSystem.D2007
, basis, wbasis, loadstd, wemail, weditor;

procedure AnwMenu;
var i:integer;
begin

 try
  if FAnwaerterMenu<>nil then begin
   FAnwaerterMenu.BringToFront;
  end
  else begin

   FAnwaerterMenu:=tFAnwaerterMenu.Create(Application);

   try
    FAnwaerterMenu.BLand.Items.Clear;
    for i := 1 to 16 do 
     FAnwaerterMenu.BLand.Items.Add(Laender^[i]);
    FAnwaerterMenu.Lade;
   except
    on EXC_:Exception do
     ExceptionHintUser0('05141',EXC_.Message,EXC_.ClassName);
   end;

   FAnwaerterMenu.Show;

  end;
 except
    on EXC_:Exception do
     ExceptionHintUser0('05142',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TFAnwaerterMenu.Lade;
begin
    FAnwaerterMenu.BLand.Text:=Laender^[Land];
    FAnwaerterMenu.Anrede.Text:=AnwaerterRec.Anrede;
    FAnwaerterMenu.Vorname.Text:=AnwaerterRec.Vorname;
    FAnwaerterMenu.Nachname.Text:=AnwaerterRec.Name;
    FAnwaerterMenu.Str.Text:=AnwaerterRec.Str;
    FAnwaerterMenu.Ort.Text:=AnwaerterRec.Ort;
    FAnwaerterMenu.Telefon.Text:=AnwaerterRec.Tel;
    FAnwaerterMenu.Mail.Text:=AnwaerterRec.EMail;
    FAnwaerterMenu.AG.Text:=AnwaerterRec.AG;
    if AnwaerterRec.Ausbilder='' then
     AnwaerterRec.Ausbilder:=trim(BezRec[AktbezRec].UserText[1])+' '+trim(BezRec[AktbezRec].UserText[2]);{ TODO 2 -oXEFrank -cS_2_Short : XE }
    FAnwaerterMenu.Ausbilder.Text:=AnwaerterRec.Ausbilder;
    FAnwaerterMenu.JS.Text:=AnwaerterRec.Schule;
    FAnwaerterMenu.AB.Text:=AnwaerterRec.Ausbildungsbeginn;
    FAnwaerterMenu.BJ.Text:=AnwaerterRec.BeginnJustizschule;
    FAnwaerterMenu.Geb.Text:=DatumToStringLong(AnwaerterRec.Geburtsdatum);
    FAnwaerterMenu.BLand.text:=AnwaerterRec.Land;
    FAnwaerterMenu.Ersteinrichtung.Checked:=AnwaerterRec.Ersteinrichtung;
    FAnwaerterMenu.ShowHeader;
end;

procedure TFAnwaerterMenu.ShowHeader;
{VAR d:datumstyp;
    s:ShortString;}
begin
{  if AnwaerterRec.Name>'' then begin
   datumlesen(D);

   s:='';
   if Absolut_31(AnwaerterRec.Ausbildungsbeginn)<Absolut_31(D) then
    s:=' in Ausbildung seit '+inttostr(GetDaysBetween(AnwaerterRec.Ausbildungsbeginn,D))+' Tage(n) und';

   if Absolut_31(AnwaerterRec.BeginnJustizschule)<Absolut_31(D) then
    s:=s+' in der Justizschule seit '+inttostr(GetDaysBetween(AnwaerterRec.Ausbildungsbeginn,D))+' Tage(n)'
   else
   if Absolut_31(AnwaerterRec.BeginnJustizschule)=Absolut_31(D) then
    s:=s+' in der Justizschule seit heute'
   else
    s:=s+' in der Justizschule seit '+inttostr(GetDaysBetween(AnwaerterRec.Ausbildungsbeginn,D))+' Tage(n)';

   Label2.Caption:='Willkommen '+AnwaerterRec.Vorname+' '+AnwaerterRec.name+s;
   Label2.Visible:=true;
  end;}
end;

procedure TFAnwaerterMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 action:=cafree;
 FAnwaerterMenu:=nil;
end;

procedure TFAnwaerterMenu.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if chkendkeys(key,shift) and (OK.Enabled) then
  OKClick(Sender);
end;

procedure TFAnwaerterMenu.FormShow(Sender: TObject);
begin
 left:=getleft2(width);
 Top :=gettop2(height);
end;

procedure TFAnwaerterMenu.JSEnter(Sender: TObject);
begin
 if JS.ItemIndex=-1 then
  case Land of
   2,6,7,8,9,10:JS.ItemIndex:=2;
   4:JS.ItemIndex:=0;
   3,5,12:JS.ItemIndex:=1;
   13:// Brandenburg??
   else
    JS.ItemIndex:=3;//Pegnitz
  end;
end;

procedure TFAnwaerterMenu.MyStdButton1Click(Sender: TObject);
begin
 SAVE;//Yep, auch bei Abbruch
 Close;
end;

procedure TFAnwaerterMenu.MyStdButton2Click(Sender: TObject);
begin
 fillchar(AnwaerterRec,sizeof(AnwaerterRec),0);
 Lade;
end;

procedure TFAnwaerterMenu.MyStdButton3Click(Sender: TObject);
var SpoolHeader : tSpoolHeader;
begin
 TDialogs.MyMessagedlg('Eine Testnachricht wird an die angegebene Adresse gesandt.'#13'Prfen Sie den Eingang der Mail (kann je nach Ihrem Provider einige Zeit dauern).',
               mtinformation,[mbok],0);
 InitSpoolHeader(SpoolHeader, eIgnore, 'Testnachricht', 0, 0 );//kein DR etc.
 Wemaile('',LeerDR,False,mail.text,'Testnachricht','','Dies ist eine Testnachricht.',False,TRUE,false, SpoolHeader);
end;

procedure TFAnwaerterMenu.Save;
begin
 fillchar(AnwaerterRec,sizeof(AnwaerterRec),0);
 AnwaerterRec.Anrede:=Anrede.Text;                                                                  { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
 AnwaerterRec.Vorname:=Vorname.Text;
 AnwaerterRec.Name:=Nachname.Text;
 AnwaerterRec.Str:=Str.Text;
 AnwaerterRec.Ort:=Ort.Text;
 AnwaerterRec.Tel:=Telefon.Text;
 AnwaerterRec.EMail:=Mail.Text;
 AnwaerterRec.AG:=AG.Text;
 AnwaerterRec.Ausbilder:=Ausbilder.Text;
 AnwaerterRec.Schule:=JS.text;                                                                      { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
 AnwaerterRec.Ausbildungsbeginn:=AB.Text;
 AnwaerterRec.BeginnJustizschule:=BJ.Text;
 ValDateLong(AnwaerterRec.Geburtsdatum,Geb.Text);                                                   { TODO 2 -oXEFrank -c'TMaskedText' zu 'ShortString' : XE }
 AnwaerterRec.Land:=BLand.text;                                                                     { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
 AnwaerterRec.ErstEinrichtung:=ErstEinrichtung.Checked;
 WriteStammdaten;
end;

procedure TFAnwaerterMenu.OKClick(Sender: TObject);
var s2,s:Ansistring;
    SpoolHeader : tSpoolHeader;

 procedure Add(txt1,txt2:ansistring);
 begin
  s:=s+txt1+': '+txt2+#13;
 end;

begin
 Save;
 ShowHeader;

 try
  s :='Sehr geehrte Damen und Herren,'#13#13 +
      'ich bitte um Zusendung von Informationsmaterial und den Zugangsdaten fr eine Demoversion.'#13#13+
      'Ich versichere, dass ich mich in Ausbildung zum Gerichtsvollzieher befinde.'#13#13;
  s2:='Quelle der Anfrage: '+trim(BezRec[AktbezRec].UserText[1])+' '+trim(BezRec[AktbezRec].UserText[2])+', Kundennr. '+KNR+#13#13;  { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  s :=s+s2;
  Add(Ownlabel1.Caption,Anrede.text);                                                               { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel3.Caption,Vorname.text);                                                              { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel14.Caption,NachName.text);                                                            { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Label3.Caption,Str.text);                                                                     { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Label4.Caption,Ort.text);                                                                     { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(label5.Caption,Mail.text);                                                                    { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(tel.Caption,Telefon.text);                                                                    { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel12.Caption,AG.text);                                                                  { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel7.Caption,BLand.text);                                                                { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel10.Caption,Ausbilder.text);                                                           { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel2.Caption,JS.text);                                                                   { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel5.Caption,AB.text);                                                                   { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel6.Caption,BJ.text);                                                                   { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  Add(Ownlabel9.Caption,GEB.text);                                                                  { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }  { TODO 2 -oXEFrank -c'TMaskedText' zu 'AnsiString' : XE }
  if Ersteinrichtung.checked then
   Add(Ersteinrichtung.Caption,'Ja')                                                                { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  else
   Add(Ersteinrichtung.Caption,'Nein');                                                             { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  s:=s+#13#13+'Mit freundlichen Gren';

  InitSpoolHeader(SpoolHeader, eIgnore, 'Demoversion', 0, 0 );//kein DR etc.
  Wemaile('',LeerDR,False,LizenzeMailD2,'Demoversion','',s,False,TRUE,false, SpoolHeader);                       { TODO 2 -oXEFrank -cS_2_Short : XE }

  s2:='WICHTIG!'#13+
               'Sollten Sie nicht binnen 2 Arbeitstagen vom Kundensupport eine Antwort erhalten, kontaktieren Sie uns bitte unbedingt telefonisch oder per Fax.'#13#13+
               'E-Mails knnen immer unterwegs verloren gehen oder als Spam markiert gelscht werden und das schon auf dem Weg zum Empfnger!'#13#13+
               'Achten Sie in den nchsten 2 Tagen auch auf Ihrem Spam-Ordner, denn allzu schnell landet unsere Antwort vielleicht dort. Die Antwort kommt von der Domain baqueundlauter.de!'#13#13+
               'Als Nachweis und zur Erinnerung wird die Anfrage zudem ausgedruckt / in die Druckablage gelegt!';

  if EditorMakroChar='@' then
   BriefVorlageGlobal:=s2+#13#13+S
  else
   BriefVorlageGlobal:=s2+#13#13+stringreplace(S,'@','@@',[rfreplaceall]);                          { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  FormBriefDATAExist := False;
  WinEditor(111,'',0,0,0,0,CODE,TRUE,FALSE,FALSE,FALSE,FALSE,1,FALSE,false);

  TDialogs.MyMessagedlg('~'+s2,
               mtwarning,[mbok],0,clyellow,clblack);

 except
  on EXC_:Exception do
   ExceptionHintUser0('05143',EXC_.Message,EXC_.ClassName);
 end;

end;

procedure TFAnwaerterMenu.ChkChange(Sender: TObject);
begin
 OK.Enabled:=(trim(Anrede.Text)<>'') and
             (trim(Vorname.Text)<>'') and
             (trim(NachName.Text)<>'') and
             (trim(Str.Text)<>'') and
             (trim(Ort.Text)<>'') and
             (trim(AG.Text)<>'') and
             (trim(Mail.Text)<>'') and
             (trim(JS.Text)<>'') and
             (trim(AB.Text)<>'') and
             (trim(BJ.Text)<>'');
end;

end.