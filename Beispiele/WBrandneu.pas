{$H+}//25.07.13
unit WBrandneu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, WCheckBoxN, MyRotate, ActiveX, XEComp,
  wownlabel, myform, MyColorBitBtn, AdvGlowButton, MyStdButton,
  OleCtrls, SHDocVw, urllabel, Wmemo, AdvOfficePager, AdvOfficePagerStylers,
  AdvOfficeImage, jpeg, HTMLabel;

type
  TFBrandneu = class(TmyForm)
    AdvGlowButton11: TMyStdButton;
    AdvGlowButton2: TMyStdButton;
    AdvGlowButton3: TMyStdButton;
    Alles: TMyStdButton;
    AdvGlowButton7: TMyStdButton;
    AdvGlowButton8: TMyStdButton;
    AdvGlowButton9: TMyStdButton;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    MyStdButton1: TMyStdButton;
    Label5: TLabel;
    MyStdButton2: TMyStdButton;
    Label6: TLabel;
    Label7: TLabel;
    MyStdButton3: TMyStdButton;
    MyStdButton4: TMyStdButton;
    MyStdButton5: TMyStdButton;
    Label10: TLabel;
    MyStdButton20: TMyStdButton;
    MyStdButton22: TMyStdButton;
    ScrollBox1: TScrollBox;
    Panel2: TPanel;
    URLLabel1: TURLLabel;
    Video1: TWebBrowser;
    Button1: TButton;
    Panel3: TPanel;
    Label13: TLabel;
    Label14: TLabel;
    MyStdButton10: TMyStdButton;
    Panel4: TPanel;
    Label8: TLabel;
    AdvGlowButton1: TMyStdButton;
    MyStdButton7: TMyStdButton;
    Panel5: TPanel;
    Label12: TLabel;
    MyStdButton6: TMyStdButton;
    MyStdButton9: TMyStdButton;
    MyStdButton12: TMyStdButton;
    MyStdButton18: TMyStdButton;
    MyStdButton19: TMyStdButton;
    Panel6: TPanel;
    Label18: TLabel;
    Label19: TLabel;
    MyStdButton16: TMyStdButton;
    MyStdButton17: TMyStdButton;
    Panel7: TPanel;
    Label16: TLabel;
    Label17: TLabel;
    MyStdButton13: TMyStdButton;
    MyStdButton14: TMyStdButton;
    MyStdButton15: TMyStdButton;
    Panel8: TPanel;
    Panel9: TPanel;
    Label21: TLabel;
    MyStdButton21: TMyStdButton;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel1: TPanel;
    MyStdButton23: TMyStdButton;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label26: TLabel;
    MyStdButton24: TMyStdButton;
    MyStdButton25: TMyStdButton;
    MyStdButton26: TMyStdButton;
    Label25: TLabel;
    Panel12: TPanel;
    Label27: TLabel;
    Label28: TLabel;
    MyStdButton28: TMyStdButton;
    Panel13: TPanel;
    MyStdButton37: TMyStdButton;
    Label34: TLabel;
    MyStdButton38: TMyStdButton;
    MyStdButton39: TMyStdButton;
    Label35: TLabel;
    Panel15: TPanel;
    Label38: TLabel;
    MyStdButton41: TMyStdButton;
    MyStdButton44: TMyStdButton;
    Label39: TLabel;
    MyStdButton45: TMyStdButton;
    Label40: TLabel;
    MyStdButton42: TMyStdButton;
    Label41: TLabel;
    MyStdButton46: TMyStdButton;
    MyStdButton47: TMyStdButton;
    MyStdButton48: TMyStdButton;
    Label43: TLabel;
    MyStdButton50: TMyStdButton;
    Label44: TLabel;
    MyStdButton51: TMyStdButton;
    Label42: TLabel;
    PG: TAdvOfficePager;
    Inf: TAdvOfficePage;
    Anw: TAdvOfficePage;
    AdvOfficePagerOfficeStyler1: TAdvOfficePagerOfficeStyler;
    PG1: TAdvOfficePage;
    PG2: TAdvOfficePage;
    AdvOfficePage1: TAdvOfficePage;
    Weitere: TAdvOfficePage;
    Label9: TLabel;
    Label15: TLabel;
    Label29: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    AdvOfficeImage1: TAdvOfficeImage;
    MyStdButton27: TMyStdButton;
    MyStdButton29: TMyStdButton;
    AdvOfficeImage2: TAdvOfficeImage;
    MyStdButton30: TMyStdButton;
    MyStdButton31: TMyStdButton;
    MyStdButton32: TMyStdButton;
    Label31: TLabel;
    Panel16: TPanel;
    Label48: TLabel;
    OwnMemo3: TOwnMemo;
    ScrollBox2: TScrollBox;
    MyStdButton40: TMyStdButton;
    OwnLabel1: TOwnLabel;
    Label30: TLabel;
    MyStdButton8: TMyStdButton;
    Label33: TLabel;
    Panel14: TPanel;
    Label51: TLabel;
    MyStdButton35: TMyStdButton;
    Profis: TMyStdButton;
    Panel18: TPanel;
    Label32: TLabel;
    Schulung: TMyStdButton;
    ScrollBox3: TScrollBox;
    Image3: TImage;
    Image1: TImage;
    Label53: TLabel;
    Image4: TImage;
    Label54: TLabel;
    AdvOfficeImage4: TAdvOfficeImage;
    Label36: TLabel;
    Label37: TLabel;
    AdvOfficeImage5: TAdvOfficeImage;
    firewall: TMyStdButton;
    Label52: TLabel;
    ZurAusbildung: TMyStdButton;
    OrderDemo: TMyStdButton;
    DatenSeparat: TMyStdButton;
    AdvOfficeImage6: TAdvOfficeImage;
    Panel20: TPanel;
    AdvOfficeImage7: TAdvOfficeImage;
    Label20: TLabel;
    KBAErst: TMyStdButton;
    Memo1: TOwnMemo;
    Timer1: TTimer;
    Handbuch: TMyStdButton;
    MyStdButton34: TMyStdButton;
    druck: TMyStdButton;
    Vorschlag: TMyStdButton;
    Label49: TLabel;
    eBOInstaller: TMyStdButton;
    Signatursoftware: TMyStdButton;
    Panel17: TPanel;
    Label11: TLabel;
    Scanner: TMyStdButton;
    Governikus: TMyStdButton;
    Label50: TLabel;
    Ust: TMyStdButton;
    Anmeldezertifikat: TMyStdButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BitBtn23Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure MyStdButton1Click(Sender: TObject);
    procedure MyStdButton14Click(Sender: TObject);
    procedure MyStdButton21Click(Sender: TObject);
    procedure AllesClick(Sender: TObject);
    procedure MyStdButton23Click(Sender: TObject);
    procedure MyStdButton26Click(Sender: TObject);
    procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MyStdButton37Click(Sender: TObject);
    procedure MyStdButton22Click(Sender: TObject);
    procedure MyStdButton11Click(Sender: TObject);
    procedure MyStdButton6Click(Sender: TObject);
    procedure MyStdButton9Click(Sender: TObject);
    procedure MyStdButton18Click(Sender: TObject);
    procedure MyStdButton19Click(Sender: TObject);
    procedure AdvGlowButton1Click(Sender: TObject);
    procedure MyStdButton7Click(Sender: TObject);
    procedure MyStdButton24Click(Sender: TObject);
    procedure MyStdButton12Click(Sender: TObject);
    procedure MyStdButton40Click(Sender: TObject);
    procedure MyStdButton10Click(Sender: TObject);
    procedure MyStdButton13Click(Sender: TObject);
    procedure MyStdButton15Click(Sender: TObject);
    procedure MyStdButton17Click(Sender: TObject);
    procedure MyStdButton16Click(Sender: TObject);
    procedure MyStdButton45Click(Sender: TObject);
    procedure MyStdButton44Click(Sender: TObject);
    procedure MyStdButton39Click(Sender: TObject);
    procedure MyStdButton28Click(Sender: TObject);
    procedure MyStdButton41Click(Sender: TObject);
    procedure MyStdButton42Click(Sender: TObject);
    procedure MyStdButton46Click(Sender: TObject);
    procedure MyStdButton47Click(Sender: TObject);
    procedure MyStdButton48Click(Sender: TObject);
    procedure MyStdButton4Click(Sender: TObject);
    procedure MyStdButton20Click(Sender: TObject);
    procedure MyStdButton5Click(Sender: TObject);
    procedure MyStdButton49Click(Sender: TObject);
    procedure MyStdButton50Click(Sender: TObject);
    procedure MyStdButton27Click(Sender: TObject);
    procedure MyStdButton29Click(Sender: TObject);
    procedure MyStdButton30Click(Sender: TObject);
    procedure MyStdButton25Click(Sender: TObject);
    procedure MyStdButton31Click(Sender: TObject);
    procedure MyStdButton32Click(Sender: TObject);
    procedure MyStdButton33Click(Sender: TObject);
    procedure ScrollBox2MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure druckClick(Sender: TObject);
    procedure MyStdButton8Click(Sender: TObject);
    procedure MyStdButton35Click(Sender: TObject);
    procedure ProfisClick(Sender: TObject);
    procedure SchulungClick(Sender: TObject);
    procedure ScrollBox3MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure firewallClick(Sender: TObject);
    procedure ZurAusbildungClick(Sender: TObject);
    procedure OrderDemoClick(Sender: TObject);
    procedure DatenSeparatClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure VorschlagClick(Sender: TObject);
    procedure KBAErstClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HandbuchClick(Sender: TObject);
    procedure eBOInstallerClick(Sender: TObject);
    procedure SignatursoftwareClick(Sender: TObject);
    procedure ScannerClick(Sender: TObject);
    procedure GovernikusClick(Sender: TObject);
    procedure UstClick(Sender: TObject);
    procedure AnmeldezertifikatClick(Sender: TObject);
  end;

procedure WinBrandNeu;
Var FBrandneu: TFBrandneu = NIL;

implementation

{$R *.DFM}

uses dek, basis, wbasis, WebAdressen, eAktenSystem.Intf, eAktenSystem.D2007
, loadstd, wEinstellungen, wleiste, wgetstrmodal, wgovernikus
, wEinstellungenStempel, Einstellungen, wEinstellungenFaksimile, weditor, wdek
, form, wTastaturschablone
, Dialogs.Intf
, wemail, menuesys, wBriefKopfVorschlaege
, scaleform, DemodatenUnit, wEinstellungenDrucker, protoemu
, winsys, wVersionFuerAnwaerterAnfordern, Hinweise, wvisitenkarte, wAnwaerterInfos, utilitie,
     wdemodaten, wEmpfehlungen, WKBAAnmeldung;

procedure TFBrandneu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
action:=CAFREE;
FBrandneu:=NIL;
end;

procedure WinBrandNeu;
begin

 //11.08.08:
 if P367Mode or PEVMode or PolizeiMode then
  Exit;

 if assigned(fbrandneu) then
  fbrandneu.bringtofront
 else begin
  fbrandneu := tfbrandneu.create(application);
  fbrandneu.PG.ActivePage:=fbrandneu.Inf;
  fbrandneu.ScrollBox1.VertScrollBar.Position:=0;
  fbrandneu.ScrollBox2.VertScrollBar.Position:=0;
  fbrandneu.ScrollBox3.VertScrollBar.Position:=0;
  fbrandneu.profis.font.style:=[fsbold];
  fbrandneu.mystdbutton4.font.style:=[fsbold];
  fbrandneu.bringtofront;
  fbrandneu.KBAErst.Enabled:= DEK.Land in KBALaender;

 end;
end;

procedure TFBrandneu.AllesClick(Sender: TObject);
begin
enabled := False;
Canmodal_winstamm1(FALSE,0);
enabled := true;
end;

procedure TFBrandneu.AnmeldezertifikatClick(Sender: TObject);
begin
 //31.08.22:
 eBOAnmeldezertifikat;
end;

procedure TFBrandneu.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if key=vk_escape then
  close
 else
 if key=vk_next then begin
  PG.SelectNextPage(TRUE);
 end
 else
 if key=vk_prior then begin
  PG.SelectNextPage(false);
 end
 else
 if ChkENDKeys(Key,Shift) then
  close;
end;

procedure TFBrandneu.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 if PG.ActivePage=Inf then begin
  if PtInRect(ScrollBox3.ClientRect, ScrollBox3.ScreenToClient(Mouse.CursorPos)) then begin

   try
    //28.07.20:
    if InComboBox((Self as tForm),handled) then
     exit;
   except
   end;

   ScrollBox3.VertScrollBar.Position := ScrollBox3.VertScrollBar.Position - WheelDelta;
   Handled := True;
  end;
 end
 else
 if PG.ActivePage=PG1 then begin
  if PtInRect(ScrollBox1.ClientRect, ScrollBox1.ScreenToClient(Mouse.CursorPos)) then begin

   try
    //28.07.20:
    if InComboBox((Self as tForm),handled) then
     exit;
   except
   end;

   ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta;
   Handled := True;
  end;
 end
 else
 if PG.ActivePage=Weitere then begin
  if PtInRect(ScrollBox2.ClientRect, ScrollBox2.ScreenToClient(Mouse.CursorPos)) then begin

   try
    //28.07.20:
    if InComboBox((Self as tForm),handled) then
     exit;
   except
   end;

   ScrollBox2.VertScrollBar.Position := ScrollBox2.VertScrollBar.Position - WheelDelta;
   Handled := True;
  end;
 end;
end;

procedure TFBrandneu.BitBtn23Click(Sender: TObject);
begin
enabled := false;
WinStamm6(0);
enabled := true;
end;

procedure TFBrandneu.BitBtn20Click(Sender: TObject);
begin
enabled := false;
winstempel(0,true);
enabled := true;
end;

procedure TFBrandneu.BitBtn22Click(Sender: TObject);
var code:vclchar;
begin
 WinEditor(98,'',0,0,0,0,CODE,false,false,false,false,false,0,false,true);
 modal_FormBriefWait;
end;

procedure TFBrandneu.BitBtn25Click(Sender: TObject);
begin
 WinDruckZiele(0);
end;

procedure TFBrandneu.BitBtn26Click(Sender: TObject);
begin
 protowahl;
end;

procedure TFBrandneu.FormShow(Sender: TObject);
begin
 ScaleFormMy(Self);//27.06.10

 //01.03.17 height:=GetClientHeightAbs-20;
 GetMaxWinSize(Self,_1600,FALSE,FALSE,TRUE);//01.03.17 NUR Hùhe bis 1200

 top   :=gettop2(height);
 Left  :=getleft2(width);
end;

procedure TFBrandneu.GovernikusClick(Sender: TObject);
begin
 CallGovernikusContract;//29.05.22
end;

procedure TFBrandneu.HandbuchClick(Sender: TObject);
begin
 ShowHilfeIdx(helpcontext);
end;

procedure TFBrandneu.KBAErstClick(Sender: TObject);
var knr, benutzer, pw, XMLFile:shortstring;
begin
 AddUltimateLog(34,'KBA Erstregistrierung');//16.09.14
 AskKBAAnmeldung(knr, benutzer, pw, XMLFile);
end;

procedure TFBrandneu.MyStdButton10Click(Sender: TObject);
begin
 TDialogs.MyMessagedlg('~Hinweis'#13+
  'Oft wird bei einem Download zusùtzliche Software angeboten! Z.B. Virenscanner u.ù.'#13+
  'Achten Sie bitte darauf, diese Zusùtze wegzuklicken! Sonst erhalten Sie unerwùnschte Beigaben oder gar mehrere Virenscanner auf Ihren Rechner.',
  mtinformation,[mbok],0);
 CallBrowser(DwlAdobeReader);
end;

procedure TFBrandneu.MyStdButton11Click(Sender: TObject);
begin
 InstCommunicator;//10.07.15
end;

procedure TFBrandneu.MyStdButton14Click(Sender: TObject);
begin
 TDialogs.MyMessagedlg('Beim '+Produktname+' haben Sie die freie Wahl der Banksoftware.'#13+
  'Das gilt insbesondere fùr die Mùglichkeit, Kontoauszùge zu verbuchen!'#13+
  'Selbst ohne Banksoftware kùnnen Sie mit dem '+Produktname+' arbeiten. ùberweisungen und Lastschriften lassen sich mit dem '+Produktname+' bequem und '+
  'einfach fùr jede Banksoftware bereitstellen sowie ùber Webseiten vieler Banken ganz ohne Bankprogramm hochladen. '#13+
  'Ebenso lassen sich Kontoauszùge verbuchen, die von einem Bankprogramm oder von einer Webseite einer Bank kommen.'#13#13+
  'Manchmal bieten Banken ein kostenloses Bankprogramm, fragen Sie also Ihre Bank. Aber Vorsicht: Nicht selten versuchen Banken Sie auf eine bestimmte Banksoftware "festzunageln". Sie haben i.d.R. aber die freie Wahl.',
  mtinformation,[mbok],0);
end;

procedure TFBrandneu.MyStdButton15Click(Sender: TObject);
begin
 CallBrowser(BuhlData);
end;

procedure TFBrandneu.MyStdButton16Click(Sender: TObject);
begin
 CallBrowser(OutlookKaufen);
end;

procedure TFBrandneu.MyStdButton17Click(Sender: TObject);
begin
 CallBrowser(DwlThunderbird);//11.03.21 'htt ps://ww w.mozilla.org/de/thunderbird/');
end;

procedure TFBrandneu.MyStdButton21Click(Sender: TObject);
begin
 TDialogs.MyMessagedlg(
'Falls Sie mitten im Jahr beginnen, sind ein paar Einstellungen notwendig. Gehen Sie in das groùe Einstellungsmenù und erfassen Sie den aktuellen Buchungsmonat und das Buchungsjahr.'#13#13+
'Sodann sind alle Register und Kassenbùcher auf die korrekte lfd. zu setzen.'#13#13+
'Sofern Sie bereits als GVZ tùtig waren und Umsteiger sind, mùssen die offenen KB I Eintragungen einmalig erfasst werden.'#13#13+
'Die bisherigen Abschlùsse sowie die Eintragungen in der Schlusszusammenstellung sind zu prùfen. Es muss fùr jeden Monat einen Eintrag geben. Hier hilft Ihnen gerne der freundliche Kundensupport, der die Einstellungen fùr Sie vornimmt.'#13#13+
'Umsteiger haben zudem ggf. die Mùglichkeit, Daten vom bisherigen GVZ-Programm zu ùbernehmen. Befragen Sie bitte die Experten des Kundensupport hierzu.',
mtinformation,[mbok],0);

end;

procedure TFBrandneu.MyStdButton22Click(Sender: TObject);
begin
 EditCodeWort(false);//SetLoginPw;
end;

procedure TFBrandneu.MyStdButton23Click(Sender: TObject);
begin
enabled := False;
Canmodal_winstamm1(FALSE,70);
enabled := true;
end;

procedure TFBrandneu.MyStdButton26Click(Sender: TObject);
begin
enabled := False;
Canmodal_winstamm1(FALSE,40);
enabled := true;
end;

procedure TFBrandneu.MyStdButton27Click(Sender: TObject);
begin
 ViewWillich;
end;

procedure TFBrandneu.MyStdButton30Click(Sender: TObject);
begin
 SubAufruf(2610);
end;

procedure TFBrandneu.MyStdButton31Click(Sender: TObject);
begin
 TDialogs.MyMessagedlg('~Tipp'#13+
              'Lassen Sie das durch die erfahrenen Mitarbeiter des Kundensupport erledigen. '#13+
              'Hat man noch nie den Registrierungsclienten genutzt und installiert, kann das durchaus eine Herausforderung sein.'#13+
              'Da alte Java-Versionen benùtigt werden, brauchen Anwender meist 15-60min.'#13#13+
              'Fùr die Mitarbeiter des Kundensupport ist das Alltagsarbeit und fùr Sie per Fernwartung in 2 Minuten erledigt.',
              mtinformation,[mbok],0);
end;

procedure TFBrandneu.MyStdButton32Click(Sender: TObject);
begin
 CallBrowser(ZugangAntragHandelsregister);
end;

procedure TFBrandneu.MyStdButton33Click(Sender: TObject);
var code:vclchar;
begin
 BriefVorlageGlobal:=EditorMakroChar+'XZBùrozeiten'#13#13+
                     'hiermit teile ich Ihnen meine Bùrozeiten mit:'#13#13+
                     EditorMakroChar+'H014'#13+
                     EditorMakroChar+'H015'#13+
                     '%%%'#13+
                     EditorMakroChar+'=01';
 WinEditor(111,'',0,0,0,0,CODE,FALSE,FALSE,FALSE,FALSE,FALSE,0,FALSE,false);
 modal_FormBriefWait;
end;

procedure TFBrandneu.MyStdButton35Click(Sender: TObject);
begin
 //21.01.20 NeueDemodatenAb2017(false,true);
 WAskDemo//21.01.20 
end;

procedure TFBrandneu.MyStdButton37Click(Sender: TObject);
var sa,ss:shortstring;
    s,s2:ansistring;
    c:vclchar;
    SpoolHeader : tSpoolHeader;
begin
 Case TDialogs.AddCustomButton('Telefonisch (Empfohlen)',mbyes)
              .AddCustomButton('E-Mail',mbno)
              .MyMessagedlg('~Ersteinrichtung'#13+
                   'Wie mùchten Sie in Kontakt treten?'#13#13+
                   'Bitte beachten Sie, dass Anfragen per E-Mail stets einen Vorlauf benùtigen!',
                   mtconfirmation,[mbyes,mbno,mbabort],0) of
  mryes : SubAufruf(58);
   mrno : begin
           sa:='Anwender(in) '+crunch(BezRec[AktBezRec].UserText[2])+' '+crunch(BezRec[AktBezRec].UserText[1]);
           repeat
            WinGetStrModal(C,sa,200,
                           'Ihr vollstùndiger Name',
                           '',
                           'Ihr vollstùndiger Name',0,'',False);
            sa:=trim(sa);                                                                           { TODO 2 -oXEFrank -cS_2_Short : XE }
            if (c<>t_esc) and (length(sa)<10) then
             wfehler('Zu wenig Angaben...',2)
           until (c=t_esc) or (length(sa)>10);
           if c=t_esc then
            Exit;

           ss:='';
           repeat
            WinGetStrModal(C,ss,200,
                           'Ihre Erreichbarkeit (Rufnummer, Zeiten und E-Mail)',
                           'Bitte unbedingt angeben: Rufnummer (vorzugsweise Festnetz), Wochentage, Uhrzeiten (Zeitfenster) sowie eine E-Mail Adresse.',
                           'Ihre Erreichbarkeit (mind. 20, max. 200 Zeichen)',0,'',False);
            ss:=trim(ss);                                                                           { TODO 2 -oXEFrank -cS_2_Short : XE }
            if (c<>t_esc) and (length(ss)<20) then
             wfehler('Zu wenig Angaben...',2)
            else
            if pos('UHR',uppercase(ss))=0 then
             wfehler('Das Wort "Uhr" wurde nicht gefunden. Haben Sie die Uhrzeitangaben vergessen? Angaben wie "nùchste 30min" sind unzureichend.',2);
           until (c=t_esc) or (length(ss)>20);
           if c=t_esc then
            Exit;

           s :='Sehr geehrte Damen und Herren,'#13#13 +
               'ich mùchte die Ersteinrichtung mit Ihrer Hilfe vornehmen.'#13#13;
           s2:=sa+', Kundennr. '+KNR+#13#13;
           s :=s+s2;
           s2:=ss;
           s :=s+s2;
           s:=s+#13#13+'Mit freundlichen Grùùen';
           InitSpoolHeader(SpoolHeader, eIgnore, 'Ersteinrichtung', 0, 0 );//kein DR etc.
           Wemaile('',LeerDR,False,LizenzeMailD2,'Ersteinrichtung','',s,False,TRUE,false, SpoolHeader);          { TODO 2 -oXEFrank -cS_2_Short : XE }
           TDialogs.MyMessagedlg('~WICHTIG!'#13+
                        'Sollten Sie nicht binnen 2 Arbeitstagen vom Kundensupport eine Antwort erhalten, kontaktieren Sie uns bitte unbedingt telefonisch oder per Fax.'#13#13+
                        'E-Mails kùnnen immer unterwegs verloren gehen oder als Spam markiert gelùscht werden und das schon auf dem Weg zum Empfùnger!',
                        mtwarning,[mbok],0);
          end;
 End;
end;

procedure TFBrandneu.MyStdButton40Click(Sender: TObject);
begin
 PrintTastaturSchablone(1);//09.07.20 ShowHilfeIdx(1528);
// SUbAufruf(2689);// 2057);
end;

procedure TFBrandneu.MyStdButton41Click(Sender: TObject);
begin
 if TDialogs.MyMessageDlg
    ('Die Informationen und Ranglisten von Virenscannern ùndern sich sehr hùufig. '#13#13+
     'Daher empfehlen wir, die Fachpresse zu studieren. Eine Suche nach "Virenscanner Test" fùhrt unweigerlich zu "Fùngerseiten", welche den Anschein erwecken, irgendwer hùtte etwas getestet. '+
     'Leider sind mittlerweile 99% der Webseiten bloùe Bots, die Infos von amazon und google Bewertungen wiedergeben und die eine Provision fùr jeden Klick auf einen Link erhalten.'#13#13+
     'Verlùssliche jùhrliche Tests von Virenscannern durch renommierte Unternehmen gibt es nur in den USA. Aber in Deutschland hat der Heise Verlag mit der Zeitung c'#39't in unseren Augen die renommierteste deutsche Computerzeitschrift.'#13#13+
    'Google zu "Virenscanner Test Heise" befragen?',mtinformation,[mbyes,mbno],0)=mryes then
  CallBrowser('www.google.de Virenscanner Test Heise');
 //11.03.21 CallBrowser('ww w.avast.de');
end;

procedure TFBrandneu.MyStdButton42Click(Sender: TObject);
var code:vclchar;
begin
 FormBrief(7,FBD+'BZSTANTRAG.TXT',0,0,0,0,CODE,False,False,FALSE,false,false,0,true);
 modal_FormBriefWait;
end;

procedure TFBrandneu.MyStdButton44Click(Sender: TObject);
begin
 CallBrowser(DwlMalwareBytes);
end;

procedure TFBrandneu.MyStdButton45Click(Sender: TObject);
begin
 if TDialogs.MyMessagedlg('~Verschlùsselung'#13#13+
  'Alle relevanten Daten des GV-Programms sind bereits verschlùsselt.'#13#13+
  'Wùnschen oder mùssen Sie darùber hinaus eine Verschlùsselung der Festplatte vornehmen, kùnnen Sie auf das im Windows enthaltene Bitlocker zurùckgreifen.'#13+
  'Weitere kostenlose Mùglichkeiten, die jedoch eine separate Software sowie Einarbeitungszeit benùtigen, sind TrueCrypt bzw. VeraCrypt.'#13#13+
  'Eine professionelle, meist jedoch teure Lùsuung, ist eine Hardwareverschlùsselung.'#13#13+
  'Weitere Infos zu Microsoft Bitlocker?',mtinformation,[mbyes,mbno],0)=mryes then
 CallBrowser(WikiBitLocker);
end;

procedure TFBrandneu.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 if PtInRect(ScrollBox1.ClientRect, ScrollBox1.ScreenToClient(Mouse.CursorPos)) then begin

  try
   //28.07.20:
   if InComboBox((Self as tForm),handled) then
    exit;
  except
  end;

  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta;
  Handled := True;
 end;
end;

procedure TFBrandneu.ScrollBox2MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 if PtInRect(ScrollBox2.ClientRect, ScrollBox2.ScreenToClient(Mouse.CursorPos)) then begin

  try
   //28.07.20:
   if InComboBox((Self as tForm),handled) then
    exit;
  except
  end;

  ScrollBox2.VertScrollBar.Position := ScrollBox2.VertScrollBar.Position - WheelDelta;
  Handled := True;
 end;
end;

procedure TFBrandneu.ScrollBox3MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 if PtInRect(ScrollBox3.ClientRect, ScrollBox3.ScreenToClient(Mouse.CursorPos)) then begin

  try
   //28.07.20:
   if InComboBox((Self as tForm),handled) then
    exit;
  except
  end;

  ScrollBox3.VertScrollBar.Position := ScrollBox3.VertScrollBar.Position - WheelDelta;
  Handled := True;
 end;
end;

procedure TFBrandneu.Timer1Timer(Sender: TObject);
begin
 //06.03.17, warum auch immer das Form sich in den Hintergrund legt!?
 try
  timer1.enabled:=false;
  bringtofront;
 except
  on EXC_:Exception do
   ExceptionHintUser0('01704',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TFBrandneu.UstClick(Sender: TObject);
begin
 //21.08.22:
 enabled := False;
 Canmodal_winstamm1(FALSE,62);
 enabled := true;
end;

procedure TFBrandneu.VorschlagClick(Sender: TObject);
begin
 WinEmpfehlungen;
end;

procedure TFBrandneu.ZurAusbildungClick(Sender: TObject);
begin
 TDialogs.MyMessagedlg(
'Sie sind zur Ausbildung bei einem Gerichtsvollzieher?'#13#13+
'Damit Sie sich das Programm genau ansehen kùnnen, OHNE Sorge zu haben, ggf. Echtdaten des GVZ zu gefùhrden, gibt es folgende sinnvolle und einfache Mùglichkeiten, die mit d. Ausbilder(in) besprochen werden sollten:'#13#13+
'1.  Installieren Sie auf Ihrem Computer das '+Produktname+'-Programm. Kostenlose Demoprogramme werden Anwùrter(innen) zur Verfùgung gestellt. Kontaktieren Sie einfach den Hersteller.'#13#13+

'2.  Sofern Ihr(e) Ausbilder(in) damit einverstanden ist, kùnnen Sie im GVZ-Programm einen weiteren Datenbestand anlegen. '+
'Diese Funktion ist im Programm vorhanden, falls z.B. der Anwender fùr mehrere Gerichte tùtig ist und separate Register/Kassenbùcher zu fùhren hat. '+
'Oder eben fùr Anwùrter. Fùr Rùckfragen kontaktieren Sie einfach den Kundensupport. Wir helfen Ihnen weiter, die bestmùgliche Lùsung zu finden.'#13#13+

'3.  Das Programm kann zudem mehrmals auf einem Computer installiert werden. Als Installationsziel ist hierbei einfach ein anderes Laufwerk auszuwùhlen. '+
'Ist nur eine Festplatte eingebaut, ist das auch kein Problem. Kontaktieren Sie kurz den Kundensupport. Wir richten das fùr Sie ein.'#13#13+

'4.  Eine interessante und vor allem ultra-mobile Lùsung ist die Installation des Programms auf einem USB-Stick. Das geht mit dem Produkt '+Produktname+', da sich das Programm ùsanftù einfùgt und '+
'nicht Systemdateien manipuliert oder Drittprogramme benùtigt. Den USB Stick kùnnen Sie dann an jedem Computer verwenden. '+
'ABER: Der USB Stick muss hierfùr schnell sein und zwar wirklich schnell. Ansonsten macht das Arbeiten mit dem Programm keinen Spaù.'#13#13+

'Eine weitere Lùsung ist ein weiterer Datenbestand fùr Anwùrter(innen), der getrennt von den Daten des GVZs ist. Mehr hierzu ùber seaparate Schaltflùche.',
 mtinformation,[mbok],0);
end;

procedure TFBrandneu.BitBtn27Click(Sender: TObject);
begin
enabled := False;
schaltervii2;
enabled := true;
end;

procedure TFBrandneu.BitBtn4Click(Sender: TObject);
begin
 modal_WinWaehleKopf;
end;

procedure TFBrandneu.DatenSeparatClick(Sender: TObject);
begin

 DemoDatenSeparat;

end;

(*
procedure TFBrandneu.DoVideo(WebBrowser:tWebBrowser;link:string);
var s:string;
begin
 try
   if pos('?',link)=0 then //steht vielleicht schon im youtube link drin??
    s:='?showinfo=0'//keine Titelleiste' +
   else
    s:='&showinfo=0';//keine Titelleiste' +

   link:='src="'+link+
         s+
         '&autoplay=1'+//sofort loslegen
         '&rel=0'+//keine Empfehlungen!
         '&iv_load_policy=3'+//keine Anmerkungen zulassen
         '&modestbranding=1'+//Youtube Logo entfernen
         '&fs=0'+//kein Fullscreen
         '"';

   WB_LoadHTML(WebBrowser,
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "h ttp://ww w.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+
'<html xmlns="htt p://ww w.w3.org/1999/xhtml">'+
  '<head>'+
    '<meta content="text/html; charset=utf-8" http-equiv="Content-Type" />'+
    '<title>Video zur Ersteinrichtung</title>'+        
  '</head>'+
  '<body style="background-color: #000000">'+
!    '<i f r a m e width="'+inttostr(WebBrowser.ClientWidth-GetScrollBarWidth)+
         '" height="'+inttostr(WebBrowser.ClientHeight-GetScrollBarWidth)+
         '" '+

         link+

         ' frameborder="0" allowfullscreen'+
!         '></i f r a m e>'+
  '</body>'+
'</html>');
 except
  on EXC_:Exception do
   ExceptionHintUser0('01705',EXC_.Message,EXC_.ClassName);
 end;
end;

//ungetestet:
procedure TFBrandneu.DoVideoVersion2(Webbrowser1:twebbrowser;HTMLStr: String);
var
  aStream     : TMemoryStream;
begin
!{ htmlstr:=  '<i f r a m e width="'+inttostr(WebBrowser1.ClientWidth-GetScrollBarWidth)+
         '" height="'+inttostr(WebBrowser1.ClientHeight-GetScrollBarWidth)+
         '" '+
         htmlstr+
         ' frameborder="0" allowfullscreen'+
!         '></i f r a m e>';}

  htmlstr:= '<html> '+
            '<head> '+
            '</style> '+
            '    <style type="text/css">.recentcomments a{display:inline !important;padding:0 !important;margin:0 !important;}</style>'+
            '</head> '+
            '<body>  '+
            //'  <object width="640" height="390"> '+
            '  <object width="'+inttostr(WebBrowser1.ClientWidth-GetScrollBarWidth)+
                    '" height="'+inttostr(WebBrowser1.ClientHeight-GetScrollBarWidth)+
                    '" >'+


            '  <param name="Ersteinrichtung" value="'+htmlstr+'"> '+
            '  </param><param name="allowFullScreen" value="true"> '+
            '  </param><param name="allowScriptAccess" value="always"> '+
            '  </param><embed src="'+htmlstr+'" type="application/x-shockwave-flash" allowfullscreen="true" allowScriptAccess="always" '+


{das klappt:
     '  <param name="movie" value="htt ps://ww w.yo utube.com/v/L7NWdxFAHdY&hl=en_US&feature=player_embedded&version=3"> '+
            '  </param><param name="allowFullScreen" value="true"> '+
            '  </param><param name="allowScriptAccess" value="always"> '+
            '  </param><embed src="htt ps://ww w.yo utube.com/v/L7NWdxFAHdY&hl=en_US&feature=player_embedded&version=3" type="application/x-shockwave-flash" allowfullscreen="true" allowScriptAccess="always"'+// width="640" height="390"> '+
}


//            'width="640" height="390"> '+
            'width="'+inttostr(WebBrowser1.ClientWidth-GetScrollBarWidth)+
                    '" height="'+inttostr(WebBrowser1.ClientHeight-GetScrollBarWidth)+
                    '" >'+

            '  </embed></object> '+
            '</body> '+
            '</html> ';
 try
   WebBrowser1.Navigate('about:blank');//reset the webbrowser
   while WebBrowser1.ReadyState < READYSTATE_INTERACTIVE do //wait to load the empty page
   Application.ProcessMessages;

    if Assigned(WebBrowser1.Document) then
    begin
      aStream := TMemory Stream.Create;
      try
         aStream.WriteBuffer(Pointer(HTMLStr)^, Length(HTMLStr));
         aStream.Seek(0, soFromBeginning);
         (WebBrowser1.Document as IPersistStreamInit).Load(TStreamAdapter.Create(aStream));
      finally
         aStream.Free;
      end;
    end;
 except
       on EXC_:Exception do
        ExceptionHintUser0('01706',EXC_.Message,EXC_.ClassName);
 end;
end;*)

procedure TFBrandneu.druckClick(Sender: TObject);
var code:vclchar;
begin
{ BriefVorlageGlobal:='Eine kleine Hilfe des G VService fùr den Einstieg in die Berufswelt des Gerichtsvollziehers:'#13#13+
                     'Neben dieser Liste sind im Ersteinrichtungsfenster zahlreiche Hinweise, Antragsformulare usw. hinterlegt.'#13#13+
                     'Die vorliegende Liste ist nur eine kurze Erinnerung, an was Berufseinsteiger sonst noch denken mùssen.'#13#13+
                     ownmemo5.text;
 WinEditor(111,'',0,0,0,0,CODE,FALSE,FALSE,FALSE,FALSE,FALSE,0,FALSE,false);
 modal_FormBriefWait;}
 FormBrief(7,FBD+'ANWAERTER.TXT',0,0,0,0,CODE,False,False,FALSE,false,false,1,true);
 modal_FormBriefWait;
end;

procedure TFBrandneu.eBOInstallerClick(Sender: TObject);
var
  lVersion  : TVersion;
  lFilename : String;
  lError    : Integer;
begin
  if CheckGovernikusDownload(false,lVersion,lFilename,lError) // FL 29.05.22 Force download
    then InstComVibilia(lError,lFilename)
    else InstComVibilia(lError);
end;

procedure TFBrandneu.firewallClick(Sender: TObject);
begin
 TDialogs.MyMessagedlg('~Firewall'#13#13+
  'Eine Firewall ist ein Schutz gegen Eindringlinge ùber Ihren Internetanschluss.'#13#13+
  'Im Windows ist bereits eine Firewall vorhanden, die gute Dienste leistet.'#13#13+
  'Sie sollten in jedem Fall eine Firewall auf Ihrem Rechner aktiviert haben!'
  ,mtinformation,[mbok],0);
end;

procedure TFBrandneu.MyStdButton9Click(Sender: TObject);
begin
 CallSafeReg(False);
end;

procedure TFBrandneu.OrderDemoClick(Sender: TObject);
begin
 AnwMenu;
end;

procedure TFBrandneu.ProfisClick(Sender: TObject);
begin
 TDialogs.MyMessagedlg(
'Die freundlichen und kompetenten Mitarbeiter vom Kundensupport helfen gerne '+
'bei der Ersteinrichtung! Das geht per Fernwartung und Sie kùnnen sich '+
'zurùcklehnen und die ùShowù genieùen.'#13#13+
'Dieser Service steht jeden Neukunden einmal kostenlos zur Verfùgung.'#13#13+

'Rufen Sie an oder schreiben Sie eine E-Mail.'#13#13+

'Zugangsdaten bei Drittstellen (z.B. das safe-id fùr  / eBO) etc. kùnnen Sie im Vorfeld '+
'oder nach der Ersteinrichtung beantragen. Dann ist ggf. ein zweiter Termin mit dem '+
'Kundensupport notwendig, der ebenfalls kostenfrei ist.',
  mtinformation,[mbok],0);
end;

procedure TFBrandneu.MyStdButton18Click(Sender: TObject);
var code:vclchar;
begin
 FormBrief(7,FBD+'PWAG.TXT',0,0,0,0,CODE,False,False,FALSE,false,false,0,true);
 modal_FormBriefWait;
end;

procedure TFBrandneu.MyStdButton19Click(Sender: TObject);
var code:vclchar;
begin
 FormBrief(7,FBD+'SELF704.TXT',0,0,0,0,CODE,False,False,false,true,false,0,true);
 modal_FormBriefWait;
end;

procedure TFBrandneu.MyStdButton7Click(Sender: TObject);
begin

 CallBundesnotarkammer;
{14.12.21
 TDialogs.MyMessagedlg('~Signaturkarte der Bundesdruckerei'#13+
  'WICHTIG: In NRW und Sachsen-Anhalt benùtigen Sie eine Signaturkarte, damit VermVerze und Eintragungen im SV vorgenommen werden kùnnen!'#13#13+
  'Lieferzeit ca. 10-14 Tage.'#13+
  'Der Programmhersteller hat auf eine sonst ùbliche Provision fùr die Vermittlung der Signaturkarten verzichtet.'#13+
  'Sie mùchten mehr wissen? Die Bundesdruckerei ist telefonisch unter 030/2598-0 und per E-Mail unter: info@bdr.de zu erreichen.'#13+
  'Besonderes Merkmal der Signaturkarten der Bundesdruckerei: '+
  'Sollten Sie die PIN 3x falsch eingeben, kùnnen Sie die PIN durch eine so genannte PUK erneut 3x eingeben (Verfahren bekannt von SIM-Karten fùr Handys). Viele andere Signaturkarten sind direkt gesperrt und schlagartig unbrauchbar, was teuer werden kann.',
  mtinformation,[mbok],0);
 CallBundesdruckerei;}
end;

procedure TFBrandneu.MyStdButton8Click(Sender: TObject);
begin
 GetSEPAID;
end;

procedure TFBrandneu.MyStdButton24Click(Sender: TObject);
begin
 //SigSoft:
 TDialogs.MyMessagedlg('Eine Signatursoftware ist dann notwendig, wenn Sie E-Mails oder PDFs digital mittels Signaturkarte signieren mùchten/mùssen.'#13#13+
              'Fùr EGVP / eBO benùtigen Sie ab 2022 je nach Bundesland wieder eine Signatursoftware!'#13#13+ //14.12.21
              //14.12.21 'Das war frùher wunderbar im GV-Programm integriert durch Nutzung des Programm SecSigner. Leider bietet die Justiz diese Software nicht mehr kostenfrei an.'#13+
              'Es muss zum Signieren eine Signatursoftware gekauft werden.'{14.12.21 #13#13+
              'Das ist wie gesagt nur nùtg, falls Sie Mails oder PDFs digital signieren mùchten.'},
              mtinformation,[mbok],0);
end;

procedure TFBrandneu.MyStdButton25Click(Sender: TObject);
begin
 close;
end;

procedure TFBrandneu.MyStdButton12Click(Sender: TObject);
begin
 CallBrowser(DwlJava);
end;

procedure TFBrandneu.MyStdButton13Click(Sender: TObject);
begin
 CallBrowser(StarMoney);
end;

procedure TFBrandneu.MyStdButton6Click(Sender: TObject);
var code:vclchar;
begin
 if TDialogs.AddCustomButton('Antragsformular',mbyes)
            .MyMessagedlg('Die Freischaltung der notwendigen EGVP / eBO Rollen (Rollen sind eine Art Zugriffsberechtigung) erfolgt leider in Deutschland uneinheitlich.'#13#13+
                 'Haben Sie Glùck, dann erledigt Ihr AG das alles im Vorfeld.'#13#13+
                 'Falls nicht, ist der Antrag meistens an das AG, manchmal an das OLG oder als letzte Option an das Zentrale Vollstreckungsgericht zu richten.'#13+
                 'Das klingt jetzt schlimmer als es ist. Fragen Sie einfach bei Ihrem AG nach, wohin Sie den Antrag auf Freischaltung der EGVP / eBO Rollen zu senden haben!',
                 mtconfirmation,[mbyes,mbabort],0)=mryes then begin
  FormBrief(7,FBD+'ROLLE.TXT',0,0,0,0,CODE,False,False,FALSE,false,false,0,true);
  modal_FormBriefWait;
 end;
end;

procedure TFBrandneu.MyStdButton39Click(Sender: TObject);
var code:vclchar;
begin
 if (emailadr>'') and (emailadr<>'Ihre E-Mail-Adresse') then begin
  BriefVorlageGlobal:=emailadr;
  if EditorMakroChar='@' then//10.08.17
   if pos('@',BriefVorlageGlobal)>0 then //10.12.08
    Insert('@',BriefVorlageGlobal,pos('@',BriefVorlageGlobal));
 end
 else
  BriefVorlageGlobal:=EditorMakroChar+'E100';
 BriefVorlageGlobal:=EditorMakroChar+'XYKostenlose E-Mail Adresse'#13#13+
                     'ich interessiere mich fùr eine kostenlose E-Mail Adresse beim Programmhersteller.'#13#13+
                     'Bitte senden Sie mir das Antragsformular per E-Mail zu.'#13+
                     'Meine E-Mail Adresse lautet: '+BriefVorlageGlobal+'.'#13+
                     '%%%'#13+
                     EditorMakroChar+'=01';
 WinEditor(111,'',0,0,0,0,CODE,FALSE,FALSE,FALSE,FALSE,FALSE,0,FALSE,false);
 modal_FormBriefWait;
end;

procedure TFBrandneu.MyStdButton1Click(Sender: TObject);
begin
 if TDialogs.MyMessagedlg('Fùr Anwùrter(innen) wird einmalig auf Wunsch die Unterschrift (und/oder Dienstsiegel) kostenlos eingescannt und professionell digital aufbereitet.'#13+
 'Bestandskunden kùnnen diesen Service gegen eine geringe Kostenpauschale nutzen.'#13#13+
 'Dazu mùssen Sie nur Ihre Unterschrift auf ein weiùes Blatt Papier in normaler Grùùe mit einem Kugelschreiber schreiben. Diese Probe senden Sie bitte per POST (aus qualitùtsgrùnden nicht per Fax) an den Hersteller.'#13+
 'Sie erhalten sodann eine E-Mail. Das '+Produktname+' Programm kann diese digitale Unterschrift auf Tastendruck dann selbst holen und installieren!'#13#13+
 'Schreiben an Hersteller aufsetzen?',
 mtconfirmation,[mbyes,mbno],0)=mryes then
  SubAufruf(2272);
end;

procedure TFBrandneu.MyStdButton28Click(Sender: TObject);
begin
 //VKarten
 WinVisitenkarte;
end;

procedure TFBrandneu.MyStdButton29Click(Sender: TObject);
var s:ansistring;
    SpoolHeader : tSpoolHeader;
begin
 if TDialogs.MyMessagedlg('Das ausgefùllte Bestellformular fùr die Post-ZUen speichern Sie auf '+
                 'Ihrem Computer und senden dies mit nachfolgender E-Mail ab.'#13#13'E-Mail aufrufen (Sie werden sodann nach dem als PDF gespeicherten Bestellschein gefragt)?',
                 mtconfirmation,[mbyes,mbno],0)=mryes then begin
  s :='Sehr geehrte Damen und Herren,'#13#13 +
      'in der Anlage ùbersende ich das ausgefùllte Bestellformular.'#13#13+
      'Mit freundlichen Grùùen';
  Leiste.PDFOpen.InitialDir:=GetDwlsPath;
  Leiste.PDFOpen.FileName:='*.pdf';
  if Leiste.PDFOpen.Execute then begin
   InitSpoolHeader(SpoolHeader, eIgnore, 'Bestellformular', 0, 0 );//kein DR etc.
   Wemaile(leiste.pdfopen.filename,LeerDR,False,'formulare@jva-willich1.nrw.de','Bestellformular','',s,False,TRUE,false, SpoolHeader);{ TODO 2 -oXEFrank -c'TFileName' zu 'AnsiString' : XE }
  end;
 end;
end;

procedure TFBrandneu.MyStdButton46Click(Sender: TObject);
begin
 //Elster
 if TDialogs.MyMessagedlg(
   'Den Elster-Stick mùssen Sie hier auf der Internetseite '+ElsterStick+' bestellen. Dann ist noch ein Formular auszufùllen und zum BZSt zu senden. '+
   'Das BZSt wird ùber die OLGe prùfen, ob der Antragsteller wirklich ein GVZ ist.'#13#13+
   'Webseite aufrufen?',mtconfirmation,[mbyes,mbno],0)=mryes then
  CallBrowser(ElsterStick);
end;

procedure TFBrandneu.MyStdButton47Click(Sender: TObject);
begin
 BestelleKartenlesegeraet;
end;

procedure TFBrandneu.MyStdButton48Click(Sender: TObject);
begin
 HintScanSnap;
end;

procedure TFBrandneu.MyStdButton49Click(Sender: TObject);
begin
 //WinSicherHints(FALSE);
end;

procedure TFBrandneu.MyStdButton4Click(Sender: TObject);
begin
 //Kostenlose Erstinst Anwùrter
 TDialogs.MyMessagedlg('~Ersteinrichtung fùr Anwùrter(innen)'#13+
'Die freundlichen und kompetenten Mitarbeiter vom Kundensupport helfen gerne '+
'bei der Ersteinrichtung! Das geht per Fernwartung und Sie kùnnen sich '+
'zurùcklehnen und die ùShowù genieùen.'#13#13+
'Dieser Service steht Anwùrter(innen) kostenlos zur Verfùgung.'#13#13+

'Besonders wichtig: Wir nehmen uns wirklich Zeit fùr Sie. Keine leeren Versprechungen oder Ersteinrichtungen, die zeitlich limitiert sind.'#13#13+

'Rufen Sie an oder schreiben Sie eine E-Mail.'#13#13+

'Zugangsdaten bei Drittstellen (z.B. das safe-id fùr EGVP / eBO) etc. kùnnen Sie im Vorfeld '+
'oder nach der Ersteinrichtung beantragen. Dann ist ggf. ein zweiter Termin mit dem '+
'Kundensupport notwendig, der ebenfalls kostenfrei ist.',
  mtinformation,[mbok],0);
end;

procedure TFBrandneu.MyStdButton50Click(Sender: TObject);
begin
 HintBarcodeZU;
end;

procedure TFBrandneu.MyStdButton5Click(Sender: TObject);
begin
 //Umsteiger
 TDialogs.MyMessagedlg(
'Falls Sie bislang mit einem anderen Programm gearbeitet haben, erklùren die Mitarbeiter des Kundensupport gerne und ausfùhrlich, wie dieser Schritt effektiv vorgenommen werden kann.'#13#13+
'Ggf. ist die ùbernahme von Daten mùglich, was aber im Dialog mit dem Kundensupport geklùrt werden muss.'#13#13+
'Die laufenden Nummern der Register und Kassenbùcher mùssen einmalig eingestellt werden. Sobald Sie Ihre Arbeit mit dem bisherigen Programm einstellen, wird die lfd. Nr. im '+Produktname+' auf die nùchste freie DR-/KB-Nummer eingestellt.'#13#13+
'Noch offene Positionen im DR I und KB I sind unter neuer fortlaufender Nummer zu ùbertragen.',
 mtinformation,[mbok],0);
end;

procedure TFBrandneu.MyStdButton20Click(Sender: TObject);
begin
 //Vorschùsse
 FoerderProg;
 ViewAnwaerter;
end;

procedure TFBrandneu.AdvGlowButton1Click(Sender: TObject);
begin
 //Signaturkarte erklùrt
 if TDialogs.MyMessagedlg(
    'Eine Signaturkarte kann man sich wie einen digitalen Identifikationsnachweis vorstellen. Diese Karten im Scheckkartenformat kùnnen Dokumente mittels eines Kartenlesegerùtes signieren.'#13#13+
    //14.12.21 'Anwender in NRW und Sachsen-Anhalt mùssen verpflichtend Einlieferungen von VermVerzen mit einer so genannten fortgeschrittenen Signatur vornehmen.'#13#13+
    'Es gibt mehrere Signaturarten. Eine kurze Erklùrung hierzu?',
    mtconfirmation,[mbyes,mbno],0)=mryes then

   TDialogs.MyMessagedlg(
  'Einfache digitale Signatur'#13+
  'Diese Signaturart ist wie eine einfache Unterschrift zu sehen. Sie unterliegt jedoch keinen speziellen '+
  'Sicherheitsmechanismen, weswegen sie in bindenden Vertrùgen oder formellen Dokumenten nicht '+
  'zugelassen ist.'#13#13+

  'Fortgeschrittene digitale Signatur'#13+
  'Wesentlich sicherer ist die fortgeschrittene digitale Signatur. Sie wird direkt dem Signaturinhaber '+
  'zugeordnet und bescheinigt die Unverùndertheit einer Nachricht oder eines Dokumentes. Sollte die '+
  'entsprechende Nachricht verùndert worden sein, ist dies direkt ersichtlich. Nur der Signaturinhaber '+
  'ist berechtigt, die entsprechende Nachricht abzuùndern.'#13#13+

  'Qualifiziert digitale Signatur'#13+
  'Diese Signaturart muss die hùchsten Sicherheitskriterien erfùllen. Sie funktioniert im Grunde wie die '+
  'forstgeschrittene digitale Signatur, muss aber durch eine sichere Signaturerstellungseinheit (SSEE) '+
  'erzeugt werden. Durch dieses Vorgehen sichert der Signaturhersteller die organisatorische und '+
  'technische Sicherheit der Signatur zu.',
  mtinformation,[mbok],0);
end;

procedure TFBrandneu.ScannerClick(Sender: TObject);
begin
 HintScanSnap;
end;

procedure TFBrandneu.SchulungClick(Sender: TObject);
begin
 HintSchulung;

end;

procedure TFBrandneu.SignatursoftwareClick(Sender: TObject);
begin
 BestellungSignatursoftware;
end;

end.