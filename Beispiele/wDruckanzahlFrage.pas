unit wDruckanzahlFrage;
{$I Globalwarn.inc }
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WEingabe, Buttons, Mask, WMask, wownlabel, ExtCtrls,XEComp,
  dek, myform, WCheckBox, Grids, SortGrid, MyColorBitBtn, AdvGlowButton,
  MyStdButton, WRadioButtonN;

VAR _INANZ:Boolean=FALSE;

type
  TFGetAnz = class(TmyForm)
    AusText: TOwnLabel;
    Label2: TOwnLabel;
    BitBtn1: TMyStdButton;
    BitBtn2: TMyStdButton;
    A: TMask;
    BitBtn3: TMyStdButton;
    BitBtn8: TMyStdButton;
    BitBtn9: TMyStdButton;
    BitBtn10: TMyStdButton;
    BitBtn11: TMyStdButton;
    BitBtn4: TMyStdButton;
    BitBtn5: TMyStdButton;
    Zusatz: TOwnLabel;
    Fax: TOwnLabel;
    eMail: TOwnLabel;
    Panel1: TPanel;
    Collect: TMyStdButton;
    CollectFax: TMyStdButton;
    CollectMail: TMyStdButton;
    BitBtn6: TMyStdButton;
    BitBtn7: TMyStdButton;
    BitBtn12: TMyStdButton;
    AddToHist: TmyCheckBox;
    BitBtn13: TMyStdButton;
    CollectPDF: TMyStdButton;
    Parteien: TSortGrid;
    CollectShow: TMyStdButton;
    CollectEGVP: TMyStdButton;
    BitBtn18: TMyStdButton;
    Panel9: TPanel;
    Panel2: TPanel;
    TEMP: TEingabe;
    SAFE: TOwnLabel;
    SigL: TOwnLabel;
    Sig: TComboBox;
    EGVPStart: TmyCheckBox;
    Panel8: TPanel;
    Label3: TLabel;
    ScrollBox1: TScrollBox;
    PrinterImg: TImage;
    PrinterLabel: TLabel;
    SigPDF: TmyCheckBox;
    SigH: TMyStdButton;
    SammelakteDruck: TMyStdButton;
    Sammelakte: TMyStdButton;
    Sammelausgabe: TMyStdButton;
    Label4: TLabel;
    QualT: TOwnLabel;
    dup: TOwnLabel;
    OwnLabel2: TOwnLabel;
    qual: TComboBox;
    einv: TmyRadioButtonN;
    einh: TmyRadioButtonN;
    einb: TmyRadioButtonN;
    aus: TmyRadioButtonN;
    papierweg: TEingabe;
    SchachtWahl: TComboBox;
    OwnLabel1: TOwnLabel;
    WieFrueher: TmyCheckBox;
    ZuAkteDruck: TMyStdButton;
    ZuAkte: TMyStdButton;
    IneAkte: TmyCheckBox;
    NureAkte: TMyStdButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure CollectClick(Sender: TObject);
    procedure CollectDruckClick(Sender: TObject);//24.02.21
    procedure CollectFaxClick(Sender: TObject);
    procedure CollectMailClick(Sender: TObject);
    procedure AChange(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    Procedure _chkhist;
    procedure AKeyPress(Sender: TObject; var Key: Char);
    procedure BitBtn13Click(Sender: TObject);
    procedure CollectPDFClick(Sender: TObject);
    procedure ParteienGetCellFormat(Sender: TObject; Col, Row: Integer;
      State: TGridDrawState; var FormatOptions: TFormatOptions);
    procedure CollectShowClick(Sender: TObject);
    procedure ParteienClick(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure CollectEGVPClick(Sender: TObject);//15.07.09
    procedure AltOnClick(Sender: TObject);
    procedure SigPDFClick(Sender: TObject);
    procedure SigHClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SammelakteClick(Sender: TObject);
    procedure SammelakteDruckClick(Sender: TObject);
    procedure SammelausgabeClick(Sender: TObject);
    procedure WieFrueherClick(Sender: TObject);
    procedure ZuAkteClick(Sender: TObject);
    procedure ZuAkteDruckClick(Sender: TObject);
    procedure NureAkteClick(Sender: TObject);
    procedure IneAkteClick(Sender: TObject);//13.08.13
  private
    { Private-Deklarationen }
  public
    wkey:VCLchar;
    _VORGABE:Byte;
    MAX:Integer;//05.12.06
    Geladen:Boolean;//07.12.06
    DRI_:Boolean;
    DR_:Nr_Typ;
    Discr_:ShortString;
  end;

//WICHTIG: Direkt aufgerufen, funktionieren alle Ausgaben wie Sammeln nicht, da diese ùber CODE zurùckgeleifert werden und nur in der GetAnz ausgewertet werden!!!!
//ERGO, NIE DIREKT AUFRUFEN! Nur ùber spooler.GetAnz!!
Function modal_WinGetAnz( Var Code : VCLChar;
                           Vorgabe : Byte;
                             Discr : ShortString;  // FDXE_S
                           Stellen : Integer;
                                ID : Byte;
                               DRI : Boolean; //23.03.05
                                DR : Nr_Typ;  //23.03.05
                           AutoFax : ShortString;  //23.03.05  // FDXE_S
                         AutoeMail : ShortString;  //23.03.05  // FDXE_S
                       AutoeMailPw : ShortString;  //28.08.21
                          AutoSAFE : ShortString;  //28.06.12
                        AutoDeMail : Shortstring//14.03.17
                                     ) : Byte;


implementation

{$R *.DFM}

uses basis,  wbasis, wdek, wRegCode, Winsys
, spooler, loadstd, Hinweise
, emu
, Dialogs.Intf
, eAktenSystem.Intf
, eAktenSystem.D2007
, scaleform, wspooler, wHilfeDruckerprobleme2, wdruckauftrag, baselib,
     spez2, wpopupmodal
, eAktenSystem.Helper
; // FL 17.07.20, wleiste;

Function modal_WinGetAnz;
var FEingabe : TFGetAnz;
S:ShortString;  // FDXE_S
i:integer;

begin //modal_WinGetAnz;
 Result:=0;
 _INANZ:=TRUE;

 try
  Feingabe := tfgetanz.create(application);
  try
   FEingabe.Label2.Caption:='';//28.06.12
   feingabe.geladen:=FALSE;//07.12.06

   feingabe.wiefrueher.checked:=DIP[108] and 2=2;//06.09.22

   feingabe.IneAkte.Visible:=eAktePossible;//08.08.25
   feingabe.IneAkte.Enabled:=AlloweAkte;//08.08.25
   feingabe.IneAkte.Checked := TeAktenHelper.IsSaveToEAkte;
   feingabe.NureAkte.Enabled:=AlloweAkte;//18.08.25

   //06.09.22:
   if not(P367Mode) and not(PEVMode) then begin
    feingabe.ZuAkte.visible     :=AllowDruckZuAkte;
    feingabe.ZuAkteDruck.visible:=AllowDruckZuAkte;
   end;

   //13.08.13:
   FillPrintScrollBox(feingabe.ScrollBox1,feingabe.PrinterIMG,feingabe.PrinterLabel,feingabe.AltOnClick);

   if P367Mode then begin
    feingabe.bitbtn13.visible:=FALSE;//04.04.12
    feingabe.bitbtn18.visible:=FALSE;
    feingabe.SammelAkte.visible:=FALSE;
    feingabe.SammelAkteDruck.visible:=FALSE;
    feingabe.bitbtn1.visible:=FALSE;
    feingabe.panel1.visible:=FALSE;
   end;

   begin//21.06.12 if (DIP[43] and 3=3) then begin//09.04.09
    //28.06.12:
    FEingabe.Sig.Visible:=not(P367Mode) and not(PEVMode);
    FEingabe.SigL.Visible:=FEingabe.Sig.Visible;

    FEingabe.BitBtn6.Visible    :=not(P367Mode) and not(PEVMode);//02.06.21
    
    //18.10.06:
    FEingabe.Collect.Visible    :=not(P367Mode) and not(PEVMode);// and (CheckVersion('W18.0',true) or AllowCollect);
{    FEingabe.CollectFax.Visible :=not(P367Mode) and not(PEVMode) and (CheckVersion('W18.0',true) or AllowCollect);
    FEingabe.CollectMail.Visible:=not(P367Mode) and not(PEVMode) and (CheckVersion('W18.0',true) or AllowCollect);
    FEingabe.CollectFax.Enabled :=CollectExist(GetWorkStationNr);
    FEingabe.CollectPDF.Enabled :=CollectExist(GetWorkStationNr);//20.06.12
    FEingabe.CollectEGVP.Enabled:=CollectExist(GetWorkStationNr);//20.06.12
    FEingabe.CollectMail.Enabled:=CollectExist(GetWorkStationNr);
    FEingabe.BitBtn6.Visible    :=not(P367Mode) and not(PEVMode) and CheckVersion('W19.0',true);//23.04.08
    }
    //22.03.17:
    FEingabe.Sammelausgabe.Visible:=not(P367Mode) and not(PEVMode);
    FEingabe.Sammelausgabe.Enabled:=CollectExist(GetWorkStationNr)
   end;

//13.08.13   feingabe.BitBtn7.Visible:=CheckVersion('W20.0',true);//13.07.09

   //15.07.09:
   feingabe.DR_   :=DR;
   feingabe.DRI_  :=DRI;
   feingabe.DISCR_:=DISCR;

   //07.09.22 nicht bei Direktdruck
   if (DIP[43] and 2=2) then
    feingabe.WieFrueher.visible:=false;
    
   if DIP[43] and 1=1 then begin
    feingabe.bitbtn1.default:=FALSE;
    feingabe.bitbtn1.enabled:=FALSE;
    feingabe.bitbtn3.default:=TRUE;
   end;
   if (DIP[43] and 3=3) then begin
    feingabe.BitBtn8. Visible :=true;
    feingabe.BitBtn9. Visible :=true;
    feingabe.BitBtn10.Visible :=true;
    feingabe.BitBtn11.Visible :=true;
   end
   else begin

    //13.07.09:
    feingabe.BitBtn8. Visible :=true;
    feingabe.BitBtn9. Visible :=true;
    feingabe.BitBtn10.Visible :=true;
    feingabe.BitBtn11.Visible :=true;

 {13.07.09
    //warum eigentlich??:
    feingabe.BitBtn8. Visible :=FALSE;
    feingabe.BitBtn9. Visible :=FALSE;
    feingabe.BitBtn10.Visible :=FALSE;
    feingabe.BitBtn11.Visible :=FALSE;}
   end;

    {13.07.09  //13.07.09 wieso???:
   if not(ID in RICHID) then begin
    feingabe.BitBtn10.enabled :=FALSE;
    feingabe.BitBtn11.enabled :=FALSE;
   end;}

   //23.03.05:
   begin
    if feingabe.BitBtn8.Visible then
     feingabe.Fax.Caption  :=AutoFax;
    if feingabe.BitBtn10.Visible then
     feingabe.eMail.Caption:=AutoeMail; //28.08.21 tja, das Passwort hilft hier wenig, da die Funktion nur fùr eMail ins Clipboard legen ist
    //28.06.12: 
    if feingabe.BitBtn13.Visible then
     feingabe.SAFE.Caption:=AutoSAFE;

{ToDo
    if feingabe.BitBtn??.Visible then
     feingabe.DeMail.Caption:=AutoDeMail;
     }

    //Anzeige Parteien und Verfùgbarkeit von Fax/eMail
    //Bei Fax und eMail dann Auswahlbox!!!

    try

//     FEingabe.PF.Caption:='EGVP Postfach '+EGVPMailBo xID;//27.09.12

     //28.06.12:
     try
      if ((Pos('EV-Abnahme',Discr)>0) or
          (Pos('EV-/VAK-Abnahme',Discr)>0) or//09.12.12
          (Pos('VAK-Abnahme',Discr)>0))//29.10.12
          and (ID in FormID) then begin
       feingabe.Sig.ItemIndex:=GlSig-1;//06.12.13 Zen Sig-1;

       {16.02.23
       feingabe.SigPDF.visible:=true;//06.12.13
       feingabe.SigH.Enabled:=true;//06.12.13}

{ // BW 27.02.23 Ticket #1495
       if (DIP[88] and 128=128) then//06.12.13
        feingabe.SigPDF.Checked:=true;//06.12.13
}

      end
      else
       feingabe.Sig.ItemIndex:=GlSig-1; //wieso GlSig!?
     except
      feingabe.Sig.ItemIndex:=0;
     end;

     if not(P367Mode) and not(PEVMode) and (DR.DR>0) and (DR.Jahr>0) then begin

      //15.07.09:
      if WINDirektDruck then begin
       Feingabe.AddToHist.Font.Size :=11;
       Feingabe.AddToHist.Font.Style:=[fsbold];
       if AutoAddHist or (ID in RichID) then begin
        Feingabe.AddToHist.Caption:='Wird automatisch in der Historie zum Register abgelegt!';
        Feingabe.AddToHist.Visible:=TRUE;
        Feingabe.AddToHist.Enabled:=FALSE;
        Feingabe.AddToHist.Checked:=TRUE;
       end
       else
        Feingabe.AddToHist.Visible:=CheckVersion('W20.0',true);//15.07.09

       //16.07.09:
       if ((Pos('EV-Abnahme',Discr)>0) or
           (Pos('EV-/VAK-Abnahme',Discr)>0) or//09.12.12
           (Pos('VAK-Abnahme',Discr)>0))//29.10.12
          and (ID in FormID) and (Land in PruefGruppe08+[_SAN]{29.12.10})
          and not(PEVMode)//14.07.13
          then begin
        Feingabe.AddToHist.Caption:='Ablage in Historie in Ihrem Land leider untersagt!';
        Feingabe.AddToHist.Enabled:=FALSE;
        Feingabe.AddToHist.Checked:=FALSE;
       end;
      end

      //12.08.09:
      else begin
       Feingabe.AddToHist.Visible:=FALSE;
       Feingabe.AddToHist.Enabled:=FALSE;
      end;

      //05.06.12:
      ParteienInGrid(DRI,DR,FEingabe.Parteien);

     end

     //16.12.13 ;
     else
     if not(DruckInfosInGrid(AutoFAX,AutoEMAIL,AutoSafe,AutoDeMail,FEingabe.Parteien)) then//16.12.13

     //26.06.12:
     if P367Mode or (DR.DR=0) then
      FEingabe.Parteien.Visible:=FALSE;

    except
      on EXC_:Exception do
       ExceptionHintUser0('02121',EXC_.Message,EXC_.ClassName);
    end;
   end;

   //25.01.05:
   Feingabe.BitBtn4.Visible:=Feingabe.BitBtn10.Visible AND CheckVersion('W16.0',true);//01.02.05
   Feingabe.BitBtn5.Visible:=feingabe.BitBtn4.Visible;

   FEingabe.Zusatz.Caption := (Discr);
   if Vorgabe=0 then
    Vorgabe:=1;
   feingabe._Vorgabe:=Vorgabe;
   FEingabe.A.Text:=InttoStr(Vorgabe);

   //05.12.06:
   FEingabe.MAX:=9;
   if CheckVersion('W18.0',true) and (Stellen=1) then begin
    FEingabe.A.MaxLength:=2;
    S:='99';
    FEingabe.MAX:=99;
   end
   else begin

    FEingabe.A.MaxLength:=Stellen;
    S:='';
    for i:=1 to Stellen do
     S:='9'+S;
    Val(S,FEingabe.Max,Dek.Error);//05.12.06
   end;

   FEingabe.A.EditMask:=S+';1;_';
   FEingabe.Label2.Caption:='Max. '+S+' Druckexemplare sind mùglich.';

   FEingabe.A.Text:=InttoStr(Vorgabe);//06.12.06, da zuvor durch property wieder entfernt!

   feingabe.wkey:=#0;
   FEingabe.Geladen:=TRUE;//07.12.06

   //29.07.09:
   if FEingabe.AddtoHist.Visible and
      FEingabe.AddtoHist.Enabled and
      (DIP[72] and 32=32) then
    FEingabe.AddtoHist.Checked:=TRUE;

   MyShowModal(FEingabe);//14.08.14 FEingabe.ShowMod al;

   //VAL(feingabe.A.text,Vorgabe,dek.Error);
   Result:=feingabe._Vorgabe;
   CODE  :=feingabe.WKEY;

   //18.04.05:
   if Code=T_ESC then
    Result:=0;

  finally
   FEingabe.FREE;
  end;

  {Keine Lùsung dafùr, dass nach Alt+D aus VV der Foksu weg ist!
  //26.11.18:
  try
   if buff<>nil then begin
    Application.bringtofront;
    if AC<>nil then begin
     buff.BringToFront;
     AC.SetFocus;
    end
    else
     buff.SetFocus;
   end;
  except
  end;}

 except
   on EXC_:Exception do
    ExceptionHintUser0('02122',EXC_.Message,EXC_.ClassName);
 end;
 _INANZ:=FALSE;
end;

procedure TFGetAnz.BitBtn1Click(Sender: TObject);
begin
 wkey       :=t_return;
 _chkhist;//15.07.09
 ModalResult:=mrOK;
end;

procedure TFGetAnz.BitBtn2Click(Sender: TObject);
begin
 wkey       :=t_esc;
 _chkhist;//15.07.09, auch bei ESC
 ModalResult:=mrabort;
end;

procedure TFGetAnz.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if key=vk_escape then
  bitbtn2click(sender)
 else
 if (key = vk_return) or
     ChkENDKeys(Key,Shift) then begin
  if not(bitbtn1.visible) or //08.08.08
     (DIP[43] and 1=1) then
   bitbtn3click(sender)
  else
   bitbtn1click(sender);
 end;
end;

procedure TFGetAnz.FormKeyPress(Sender: TObject; var Key: Char);
begin
 KeyToAccel(key,Self);//19.04.16
end;

procedure TFGetAnz.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var v:integer;
begin
 VAL(A.text,V,dek.Error);
 _Vorgabe:=Byte(V);
 if wkey=#0 then begin
  wkey:=t_esc;
 end;
end;

procedure TFGetAnz.FormShow(Sender: TObject);
begin
 GetMaxWinSize(Self,_1280,true,false,true);//20.09.17
 
 ScaleFormMy(Self);//27.06.10
 //13.12.11:
 left:=getleft_modal(width);
 top :=gettop_modal(height);
end;

procedure TFGetAnz.IneAkteClick(Sender: TObject);
begin
  TeAktenHelper.ToggleSaveToEAkte(IneAkte.Checked);
end;

procedure TFGetAnz.ParteienClick(Sender: TObject);
begin
 DruckAuftragParteienGrid2ClipBoard(Parteien);
end;

procedure TFGetAnz.ParteienGetCellFormat(Sender: TObject; Col, Row: Integer;
  State: TGridDrawState; var FormatOptions: TFormatOptions);
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
   formatoptions.brush.color := Parteien.Color//$0099FDAD
  else
   formatoptions.brush.color := Parteien.Color;
 except
  on EXC_:Exception do
   ExceptionHintUser0('02123',EXC_.Message,EXC_.ClassName);
 end;

end;

procedure TFGetAnz.SigPDFClick(Sender: TObject);
begin
 {16.02.23
 //06.12.13:
 if SigPDF.checked then
  DIP[88]:=DIP[88] or 128
 else
  DIP[88]:=DIP[88] and (255-128);
 WriteOneDip(88);}
end;

procedure TFGetAnz.WieFrueherClick(Sender: TObject);
begin
 //06.09.22:
 if WieFrueher.checked then begin
  if DIP[108] and 2=0 then begin
   DIP[108]:=DIP[108] or 2;
   WriteOneDip(108);
  end;
 end
 else begin
  if DIP[108] and 2=2 then begin
   DIP[108]:=DIP[108] and (255-2);
   WriteOneDip(108);
  end;
 end;
end;

procedure TFGetAnz.ZuAkteClick(Sender: TObject);
begin
 //06.09.22:
 DirektDruckZurAkte := true;
 wkey        := 'X';
 _chkhist;
 ModalResult := mrOK;
end;

procedure TFGetAnz.ZuAkteDruckClick(Sender: TObject);
begin
 //06.09.22:
 DirektDruckZurAkteDruck := true;
 wkey        := 'W';
 _chkhist;
 ModalResult := mrOK;
end;

procedure TFGetAnz.BitBtn3Click(Sender: TObject);
begin
 DirektDruck := true;
 wkey        := T_F1;
 _chkhist;//15.07.09
 ModalResult := mrOK;
end;

procedure TFGetAnz.BitBtn8Click(Sender: TObject);
begin
 DirektFax  :=true;
 wkey       :=t_f2;
 _chkhist;//15.07.09
 ModalResult:=mrOK;
end;

procedure TFGetAnz.AKeyPress(Sender: TObject; var Key: Char);
begin
 KeyToAccel(key,Self);
end;

procedure TFGetAnz.SammelakteClick(Sender: TObject);
begin
 DirektSammelAkte  :=true;
 wkey       :='(';
 _chkhist;
 ModalResult:=mrOK;
end;

procedure TFGetAnz.SammelakteDruckClick(Sender: TObject);
begin
 DirektSammelAkteDruck  :=true;
 wkey       :=')';
 _chkhist;
 ModalResult:=mrOK;
end;

procedure TFGetAnz.SammelausgabeClick(Sender: TObject);
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
   2:CollectDruckClick(Sender);//24.02.21 BitBtn6Click(Sender);
   3:CollectFaxClick(Sender);
   4:CollectMailClick(Sender);
   5:CollectPDFClick(Sender);
   6:CollectEGVPClick(Sender);
  end;
 end;
end;

procedure TFGetAnz.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
    ExceptionHintUser0('02124',EXC_.Message,EXC_.ClassName);
  end;
  Handled := true;
 end;
end;

procedure TFGetAnz.SigHClick(Sender: TObject);
begin
 HintSigPDF;//06.12.13
end;

procedure TFGetAnz.BitBtn10Click(Sender: TObject);
begin
 DirekteMail:=true;
 wkey       :=t_Sf2;
 _chkhist;//15.07.09
 ModalResult:=mrOK;
end;

procedure TFGetAnz.BitBtn9Click(Sender: TObject);
begin
 DirektFaxDruck:=true;
 wkey          :=t_f3;
 _chkhist;//15.07.09
 ModalResult   :=mrOK;
end;

procedure TFGetAnz.BitBtn11Click(Sender: TObject);
begin
 DirekteMailDruck:=true;
 wkey            :=t_Sf3;
 _chkhist;//15.07.09
 ModalResult     :=mrOK;
end;

procedure TFGetAnz.BitBtn4Click(Sender: TObject);
begin
 DirektePDF :=true;
 wkey       :='C';
 _chkhist;//15.07.09
 ModalResult:=mrOK;
end;

procedure TFGetAnz.BitBtn5Click(Sender: TObject);
begin
 DirektePDFDruck:=true;
 wkey           :='D';
 _chkhist;//15.07.09
 ModalResult    :=mrOK;
end;

procedure TFGetAnz.CollectClick(Sender: TObject);//18.10.06
var e,i:integer;
begin
 //17.02.21:
 val(a.text,i,e);
 if i>1 then begin
  WFehler('Beim Sammeln wird immer nur 1 Exemplar hinzugefùgt! Die Druckanzahl bleibt unberùcksichtigt.',2);
 end;

 DirekteSammeln :=true;
 wkey           :='S';
 _chkhist;//15.07.09
 ModalResult    :=mrOK;
end;

procedure TFGetAnz.CollectEGVPClick(Sender: TObject);
begin
 EGVPPDFOverride:= true; // BW 27.02.23 Ticket #1495 SigPDF.Checked;//03.12.13

 //28.06.12:
 EGVPOverride:=Sig.ItemIndex;
 EGVPDirectStart:=EGVPStart.Checked;//16.07.12
 DirektSammelnEGVP:=true;
 wkey           :='G';
 _chkhist;
 ModalResult    :=mrOK;
end;

procedure TFGetAnz.CollectDruckClick(Sender: TObject);//24.02.21
begin
 //11.07.13 DirekteSammeln :=true;

 //25.02.21:
 DirekteSammelnD :=true;//11.07.13
 wkey           :='V';

 _chkhist;//15.07.09
 ModalResult    :=mrOK;
end;

procedure TFGetAnz.CollectFaxClick(Sender: TObject);//18.10.06
begin
 DirekteSammelnF :=true;
 wkey            :='T';
 _chkhist;//15.07.09
 ModalResult     :=mrOK;
end;

procedure TFGetAnz.CollectMailClick(Sender: TObject);//18.10.06
begin
 DirekteSammelnE :=true;
 wkey            :='U';
 _chkhist;//15.07.09
 ModalResult     :=mrOK;
end;

procedure TFGetAnz.CollectPDFClick(Sender: TObject);
begin
 //05.06.12:
 DirekteSammelnP :=true;
 wkey            :='P';
 _chkhist;
 ModalResult     :=mrOK;
end;

procedure TFGetAnz.CollectShowClick(Sender: TObject);
begin
 //07.06.12
 ShowCollect(GetWorkStationNr);
end;

procedure TFGetAnz.AChange(Sender: TObject);//05.12.06
var v:integer;
begin
 if geladen then begin
  VAL(A.text,V,dek.Error);
  if V=0 then begin
//19.05.09   WFehler('Die Druckanzahl Null ist nicht mùglich.',2);
   A.Text:='1';//07.12.06
  end
  else
  if V>MAX then begin
   WFehler('Die Druckanzahl darf '+inttostr(MAX)+' nicht ùberschreiten.',2);
   A.text:=inttostr(MAX);
  end;
 end;
end;

procedure TFGetAnz.BitBtn6Click(Sender: TObject);//23.04.08
begin
//11.07.13 DirekteSammeln :=true;

 //25.02.21:
 DirekteSammelnUndDrucken:=true;//24.02.21 DirekteSammelnD :=true;//11.07.13
 wkey           :='Z';
 
 _chkhist;//15.07.09
 ModalResult    :=mrOK;
end;

procedure TFGetAnz.BitBtn7Click(Sender: TObject);//13.07.09
begin
 DirektDruckWahl :=true;
 wkey            :='Y';
 _chkhist;//15.07.09
 ModalResult     :=mrOK;
end;

procedure TFGetAnz.BitBtn12Click(Sender: TObject);
begin
 if TDialogs.MyMessagedlg('Handbuch? Bei Nein wird eine Sammlung von Tipps und Kurzhilfen bei Problemen gezeigt.',mtconfirmation,[mbyes,mbno],0)=mryes then
  showhilfeidx(helpcontext)
 else
  DruckHelp;
end;

procedure TFGetAnz.BitBtn13Click(Sender: TObject);
begin
 EGVPPDFOverride:= true; // BW 27.02.23 Ticket #1495SigPDF.Checked;//03.12.13
 // PerEGVP;//15.12.11
 //28.06.12:
 EGVPOverride:=Sig.ItemIndex;
 EGVPDirectStart:=EGVPStart.Checked;//16.07.12
 DirektEGVP :=true;
 wkey       :='E';
 _chkhist;
 ModalResult:=mrOK;
end;

procedure TFGetAnz.BitBtn18Click(Sender: TObject);
begin
 EGVPPDFOverride:= true ; // BW 27.02.23 Ticket #1495SigPDF.Checked;//03.12.13
 //28.06.12:
 EGVPOverride:=Sig.ItemIndex;
 EGVPDirectStart:=EGVPStart.Checked;//16.07.12
 DirektEGVPDruck:=true;
 wkey           :='F';
 _chkhist;
 ModalResult    :=mrOK;
end;

Procedure tfGetAnz._chkhist;//15.07.09
begin
 try
  if AddToHist.Checked and AddToHist.Enabled and AddToHist.Visible then begin
//   LocalAddHist:=TRUE;
   if (DR_.DR>0) and (DR_.Jahr>0) then begin
    AutoAddHistGetAnz:=TRUE;
    //GlobalSplname wird verwendet!!
//    Add Spool ToHist(DR_,DRI_,discr_);
   end;
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('02125',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TFGetAnz.AltOnClick(Sender: TObject);//13.08.13
begin
 try
  AltPrinter:=TControl(Sender).Tag;
  BitBtn7Click(Sender);
 except
  on EXC_:Exception do
   ExceptionHintUser0('02126',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TFGetAnz.NureAkteClick(Sender: TObject);
begin
 //18.08.25:
 DirekteAkte :=true;
 wkey        :='Q';
 _chkhist;
 ModalResult :=mrOK;
end;

end.
