{$H+}
//massiv geùndert bis neu 29.-31.03.12
unit wVisitenkarten;
{
Ab jetzt Bestellung ùber wir-machen-druck, weil gnadenlos billig!:
Qualitùt OK, Lieferzeit was lùnger, dafùr halber Preis. Was fùr ein weggeworfenes Geld bei flyeralarm

VK-Preis inkl. Versand zu uns oder Alternativadresse(!) auf deren Homepage:
 alles 4farbig:
 1.000x 300g = 18,09; Doppelseitig: 19,48
 VK: 52; 56
 2.500x 300g = 26,75; 26,75
 VK: 65; 69
 5.000x 300g = 32,46; 32,46
 VK: 85; 89

 Leinen:
 1.000 = 33,21; doppel 39,84
 2.000 = 46,10;  52,31
}

interface

Uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, ImgList, Buttons, wcombobox, WCheckBox, ComCtrls, WEingabe
, AdvGlowButton
, MyStdButton
{$if CompilerVersion >=27}
, system.UITypes
, System.ImageList
{$IFEND}
, BLCOMinterface
;

type
  TBLVKarten = class(TForm) //nicht myform etc.!!!
    ImageList1: TImageList;
    ColorDialog1: TColorDialog;
    ScrollBox1: TScrollBox;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    RootRoot: TPanel;
    Layout4: TPanel;
    Wappen4: TImage;
    Name4: TLabel;
    Ort4: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Email14: TLabel;
    Fax14: TLabel;
    Tel14: TLabel;
    Label51: TLabel;
    Sprech14: TLabel;
    Strasse4: TLabel;
    Sprech24: TLabel;
    Label63: TLabel;
    Sprech34: TLabel;
    Sprech44: TLabel;
    Panel10: TPanel;
    OGV4: TLabel;
    Panel13: TPanel;
    Panel11: TPanel;
    Label61: TLabel;
    Layout3: TPanel;
    Wappen3: TImage;
    OGV3: TLabel;
    Ort3: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label31: TLabel;
    Email13: TLabel;
    Fax13: TLabel;
    Tel13: TLabel;
    Label41: TLabel;
    Sprech13: TLabel;
    Strasse3: TLabel;
    Panel5: TPanel;
    Panel6: TPanel;
    Name3: TLabel;
    Panel7: TPanel;
    Layout2: TPanel;
    Wappen2: TImage;
    Panel3: TPanel;
    Name2: TLabel;
    Strasse2: TLabel;
    Ort2: TLabel;
    OGV2: TLabel;
    Label22: TLabel;
    Sprech12: TLabel;
    Sprech22: TLabel;
    Email12: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Handy12: TLabel;
    Tel12: TLabel;
    Label30: TLabel;
    Layout1: TPanel;
    Image2: TImage;
    Wappen1: TImage;
    OGV1: TLabel;
    Name1: TLabel;
    Strasse1: TLabel;
    Ort1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Fax11: TLabel;
    Handy11: TLabel;
    Tel11: TLabel;
    Buero11: TLabel;
    Menu: TPanel;
    SpeedButton3: TMyStdButton;
    SpeedButton4: TMyStdButton;
    SpeedButton5: TMyStdButton;
    SpeedButton7: TMyStdButton;
    SpeedButton8: TMyStdButton;
    SpeedButton9: TMyStdButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    Menge: TCombo;
    SpeedButton12: TMyStdButton;
    SpeedButton13: TMyStdButton;
    Layout0: TPanel;
    Wappen0: TImage;
    OGV0: TLabel;
    Label12: TLabel;
    Sprech10: TLabel;
    Panel15: TPanel;
    Name0: TLabel;
    AG0: TLabel;
    Strasse0: TLabel;
    Ort0: TLabel;
    Label11: TLabel;
    Label10: TLabel;
    Label9: TLabel;
    Tel10: TLabel;
    Fax10: TLabel;
    email10: TLabel;
    Sprech20: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Sprech23: TLabel;
    Label25: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Layout5: TPanel;
    Wappen5: TImage;
    OGV5: TLabel;
    Name5: TLabel;
    AG5: TLabel;
    Panel4: TPanel;
    Strasse5: TLabel;
    Ort5: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label52: TLabel;
    Tel15: TLabel;
    Fax15: TLabel;
    Email15: TLabel;
    Label35: TLabel;
    Sprech15: TLabel;
    Sprech25: TLabel;
    Label43: TLabel;
    Rueck: TPanel;
    Vor: TPanel;
    Label59: TLabel;
    Label60: TLabel;
    Label62: TLabel;
    Image8: TImage;
    Label64: TLabel;
    Image0: TImage;
    Label65: TLabel;
    Image9: TImage;
    Label66: TLabel;
    RueckSeite0: TPanel;
    Label69: TLabel;
    Label70: TLabel;
    RueckSeite1: TPanel;
    Panel2: TPanel;
    Label68: TLabel;
    Image10: TImage;
    Label71: TLabel;
    Label72: TLabel;
    Image11: TImage;
    Label73: TLabel;
    Image1: TImage;
    Label1: TLabel;
    Layout6: TPanel;
    Wappen6: TImage;
    OGV6: TLabel;
    Ort6: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label13: TLabel;
    email16: TLabel;
    Fax16: TLabel;
    Tel16: TLabel;
    Label28: TLabel;
    Sprech16: TLabel;
    Str6: TLabel;
    Sprech26: TLabel;
    Panel9: TPanel;
    Panel12: TPanel;
    Name6: TLabel;
    Panel14: TPanel;
    Panel16: TPanel;
    Label84: TLabel;
    Panel17: TPanel;
    Label91: TLabel;
    Rueckseite2: TPanel;
    Label3: TLabel;
    QRV2: TImage;
    Label2: TLabel;
    EditPanel: TPanel;
    txt: TEingabe;
    colorbutton: TSpeedButton;
    underlinebutton: TSpeedButton;
    italicbutton: TSpeedButton;
    boldbutton: TSpeedButton;
    fontsize: TEingabe;
    fontname: TComboBox;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    updown1: TUpDown;
    Neu: TSpeedButton;
    Wappen: TRadioButton;
    Waage: TRadioButton;
    nichts: TRadioButton;
    Label29: TLabel;
    Fontnamestemp: TComboBox;
    Root: TPanel;
    Label34: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    IBAN14: TLabel;
    BIC14: TLabel;
    Label40: TLabel;
    IBAN16: TLabel;
    BIC16: TLabel;
    Label48: TLabel;
    Panel1: TPanel;
    Label74: TLabel;
    Label75: TLabel;
    IBAN13: TLabel;
    BIC13: TLabel;
    Label81: TLabel;
    BitBtn1: TMyStdButton;
    BitBtn2: TMyStdButton;
    OpenD: TOpenDialog;
    ABody: TMemo;
    BitBtn3: TMyStdButton;
    BitBtn4: TMyStdButton;
    Redak: TCheckBox;
    BitBtn5: TMyStdButton;
    Redak2: TCheckBox;
    Redak3: TCheckBox;
    Label23: TLabel;
    BitBtn6: TMyStdButton;
    Layout99: TPanel;
    GVS: TImage;
    Label16: TLabel;
    Label37: TLabel;
    Label54: TLabel;
    Label67: TLabel;
    Panel18: TPanel;
    Label76: TLabel;
    Label77: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    Label24: TLabel;
    Label36: TLabel;
    Label85: TLabel;
    Label58: TLabel;
    Image99: TPanel;
    StartV: TTimer;
    Redak4: TCheckBox;
    Posi: TLabel;
    Layout7: TPanel;
    Label92: TLabel;
    Label93: TLabel;
    email7: TLabel;
    Fax7: TLabel;
    Tel17: TLabel;
    Label98: TLabel;
    Sprech17: TLabel;
    Str7: TLabel;
    Sprech27: TLabel;
    Panel19: TPanel;
    Panel22: TPanel;
    Label109: TLabel;
    Label110: TLabel;
    IBAN7: TLabel;
    BIC7: TLabel;
    Label113: TLabel;
    Wappen7: TImage;
    ogvname7: TLabel;
    Ort7: TLabel;
    AG7: TLabel;
    Image12: TImage;
    Label89: TLabel;
    Panel8: TPanel;
    MyStdButton16: TMyStdButton;
    procedure Image3Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure Image11Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Waage2Click(Sender: TObject);
    procedure txtKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure colorbuttonClick(Sender: TObject);
    procedure underlinebuttonClick(Sender: TObject);
    procedure txtchange(Sender: TObject);
    procedure NeuClick(Sender: TObject);
    procedure txtExit(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure StartVTimer(Sender: TObject);
    procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MyStdButton16Click(Sender: TObject);
  private
    { Private-Deklarationen }
//06.08.13     VorlageR,
//06.08.13     Vorlage:tbitmap;
    BMP : TBitmap;
    EditL:tLabel;
    FLand : Byte;
    Changed,
    OK,
    RueckGewaehlt : Boolean;
    FDownX,
    FDownY: Integer;
    FDragging: Boolean;
    _ID,
    OrigL,
    OrigT,
    AktLayoutR : Integer;
    AktLayout : Int32; // FL 31.08.21
    vC:Array[1..20] of Shortstring;
    _SRC,
    _D,_Z,_Anwender,
    _BL,
    _KNR,
    _OnlineID,
    GlobalEMail,
    FileNameV,
    FileNameR,
    AktLayoutName,
    AktLayoutNameR:Shortstring;
    AktPanel,
    AktPanelR : tPanel;
    Procedure ShowHide(ID:Integer);
    Procedure SetWappen;
    procedure EditP(Sender:tobject);
    procedure Edit(Sender:tobject);
    Procedure Clean(I:tImage);
    Procedure GetFontNames;
    procedure MyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MyMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MyMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); 
    procedure NeuLabel(P:tPanel;Var L:tLabel;
                       LName,LCaption:Shortstring;
                       Le,T,c,FSize:Integer;FName:Shortstring;
                       FStyle:tFontStyles);
   procedure CreateVorlage(EDIT:Boolean);
   //23.04.12:
   procedure DoLabel(L:tLabel;var id,inh:shortstring);
   Procedure BestaetigungAnKunden;
  end;

var
  BLVKarten: TBLVKarten;

Procedure GetVKarte(Land:byte;Src:Shortstring);

implementation

Uses
  Dek
, wDek
, wBasis
, wBLCOM
, Types
, wEinstellungenFaksimile
, basis
, WGetStrModal
, wleiste
, wemail
, wwappen
, eAktenSystem.Intf
, eAktenSystem.D2007
, winsys
, BLComIntf
, Synpdf
, email
, wtools
, wcopyfile
, BLCOMStreamHandler
, Dialogs.Intf
;

{$R *.dfm}

 Function _VAL(s:shortstring):Integer;
 begin
  Val(s,Result,Dek.Error);
 end;

Procedure tBLVKarten.BestaetigungAnKunden;
var s,command:ansistring;
     FileName:Ansistring;
     i:integer;
begin
 try
   ABody.Lines.Clear;
   if pos(',',_Anwender)>0 then
    s:=Copy(_Anwender,1,pos(',',_Anwender)-1)
   else
    s:=_Anwender;
   //28.04.12: 
   ABody.Lines.Add('Kundennr. '+_KNR);
   ABody.Lines.Add(Menge.Text);

   ABody.Lines.Add('');
   ABody.Lines.Add('Sehr geehrte(r) Frau/Herr '+s+',');
   ABody.Lines.Add('');
   ABody.Lines.Add('vielen Dank fùr Ihre Bestellung einer Visitenkarte. Ihre Bestellung wurde von uns verarbeitet und anbei erhalten Sie Ihre Visitenkarte zur Vorschau und Kontrolle. Klicken Sie hierzu bitte auf die Anlage. Dort finden Sie ein PDF-Dokument.');
   ABody.Lines.Add('Bestellmenge und Preis sind: '+Menge.Text+'.');
   ABody.Lines.Add('');
   if Redak.Checked or Redak2.Checked or Redak3.Checked or Redak4.Checked then begin
    ABody.Lines.Add('*** Redaktioneller Hinweis ***. Uns ist folgendes aufgefallen:');
    if Redak.Checked then
     ABody.Lines.Add('[X] Mindestens eine Textzeile reicht ùber den Rand hinaus oder ist zu nah am Rand. (Es gibt auch einen nicht bedruckbaren Bereich am Rand, dort hinein darf kein Text ragen, da dieser sonst ggf. abgeschnitten wùrde).');
    if Redak2.Checked then
    ABody.Lines.Add('[X] Mindestens eine Textzeile enthùlt fehlende Angaben');
    if Redak3.Checked then
     ABody.Lines.Add('[X] Mindestens eine Textzeile enthùlt noch Standardvorgaben');
    if Redak4.Checked then
     ABody.Lines.Add('[X] Fehlerhafter Text: ');
    ABody.Lines.Add('');
    ABody.Lines.Add('*** Bitte senden Sie uns erneut Ihren Entwurf, der automatisch bei Ihnen gespeichert wurde. Rufen Sie den Visitenkartendesigner auf und klicken Sie auf Design laden. ***');
    ABody.Lines.Add('Jedwede ùnderung an den Visitenkarten mùssen Sie ausschlieùlich mit dem '+produktname+' Visitenkartendesigner selbst vornehmen. Bitte senden Sie keine Mails oder Faxe, was geùndert werden soll. Nur der Anwender selbst kann die ùnderungen vornehmen.');
    ABody.Lines.Add('Die beigefùgten PDF sind nur eine Druckvorschau - nicht zum Editieren.');
   end;
   ABody.Lines.Add('Bitte kontrollieren Sie alle Texte und das gesamte Layout auf der Visitenkarte mit Argusaugen.');
   ABody.Lines.Add('Sollten Sie einen QR-Code oder QR-Code vCard mit auf der Visitenkarte haben, kontrollieren Sie auch diesen mit Ihrem Smartphone.');
   ABody.Lines.Add('');

   //29.05.12:
   ABody.Lines.Add('Wichtig: Alle ùnderungen mùssen durch den Anwender erfolgen. Wir nehmen selbst keine ùnderungen vor. Wenn Sie im ùbersandten Layout ùnderungen wùnschen, nehmen Sie diese bitte selbst vor und senden Sie dann erneut eine Bestellung.');
   ABody.Lines.Add('Ihre vorherige Bestellung wird dann ùberschrieben und Sie erhalten natùrlich nichts doppelt.');

   ABody.Lines.Add('');
   ABody.Lines.Add('Hinweise: Es ist richtig, dass die Visitenkarte in der Anlage einen grùùeren Rand hat. Das ist nùtig, da spùter ein Zuschnitt in der Druckerei geschieht! Dann werden 3-5mm auf jeder Seite abgeschnitten.');
   ABody.Lines.Add('Bzgl. der Farben kann es im Druck Abweichungen geben, da Druckereien mit einer anderen Farbpalette (Info fùr PC-Profis: CMYK statt RGB) arbeiten. ùblicherweise sind die Farben aber sehr ùhnlich.');
   ABody.Lines.Add('Auùerdem sind je nach Monitor und dessen Einstellung bzw. Treiberansteuerung ebenfalls Farbunterschiede vorhanden.');
   ABody.Lines.Add('');
   if Redak.Checked or Redak2.Checked or Redak3.Checked or Redak4.Checked then
    ABody.Lines.Add('Sobald wir eine neue Vorlage von Ihnen erhalten haben, bekommen Sie die Visitenkarte erneut zur Begutachtung.')
   else begin
    ABody.Lines.Add('Wir geben die Visitenkarte erst zum Druck, wenn Sie uns die Richtigkeit aller Angaben bestùtigt haben. Sobald wir Ihre Bestùtigung haben, ist die Bestellung verbindlich und unwiderruflich.');
    ABody.Lines.Add('Sie erhalten Ihre Visitenkarte dann binnen 1-2 Wochen zugesandt.');
    ABody.Lines.Add('');
    ABody.Lines.Add('Fùr die Bestùtigung, falls alles wie gewùnscht ist, antworten Sie einfach auf diese Nachricht mit dem Wort OK.');
   end;
   ABody.Lines.Add('');
   ABody.Lines.Add('');
   ABody.Lines.Add('Mit freundlichen Grùùen');
   ABody.Lines.Add('Ihr '+produktname+'-Team');

   s:=FileNameV;
   if FileNameR>'' then
    s:=s+';'+FileNameR;

   FileName:=tempPath+'eMailV''.EML';
   Outlook_Express('service@gvinfo.de',
                   GlobalEMail{28.04.12},//Empfùnger
                   'Bitte bestùtigen Sie Ihre Visitenkartenbestellung vom '+_D,
                   ABody.Lines,
                   s,//Anlage!
                   FileName,
                   FALSE,'','',TRUE,
                   LIZENZ1D);//10.09.22 'Ba quù & Lauter GmbH');                                    { TODO 2 -oXEFrank -cS_2_Ansi : XE }
   Command := FileName;
   ShellExecute64BitSafe(nil,pchar(command), nil, nil,sw_shownormal);                               { TODO 1 -oXEFrank -cAnsiString in PWideChar : XE }

   ioresult;
   begin// if Redak.Checked or Redak2.Checked or Redak3.Checked or Redak4.Checked then begin
    i:=1;
    while (i<10) and FileExists(ExtractFilePath(OpenD.Filename)+'MAIL'+inttostr(i)+' - '+ExtractFilename(OpenD.Filename)) do
     inc(i);
    RenameFile(OpenD.Filename,ExtractFilePath(OpenD.Filename)+'MAIL'+inttostr(i)+' - '+ExtractFilename(OpenD.Filename));
    ioresult;
   end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05145',EXC_.Message,EXC_.ClassName);
 end;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
   FontType: Integer; Data: Pointer): Integer; stdcall;
 begin
  Result:=1;
  try
   TStrings(Data).Add(LogFont.lfFaceName);
   Result := 1;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05146',EXC_.Message,EXC_.ClassName);
  end;
 end;

 procedure tBLVKarten.DoLabel(L:tLabel;var id,inh:shortstring);
 var _inh:shortstring;
 begin
  try
       if pos(#9,inh)>0 then begin
        L.Caption:=Copy(inh,1,pos(#9,inh)-1);
        delete(inh,1,pos(#9,inh));
        while (inh>'') and (pos(';',inh)>0) do begin
         if pos(':',inh)>0 then begin
          id:=Ansiuppercase(trim(copy(inh,1,pos(':',inh)-1)));                                      { TODO 2 -oXEFrank -cS_2_Short : XE }
          Delete(inh,1,pos(':',inh));
          if (pos(';',inh)>0) then begin
           _inh:=Copy(inh,1,pos(';',inh)-1);
           delete(inh,1,pos(';',inh));

           if id='STYLE' then begin
            L.Font.Style:=[];
            if pos('BOLD',_inh)>0 then
             L.Font.Style:=
              L.Font.Style+[fsbold];
            if pos('ITALIC',_inh)>0 then
             L.Font.Style:=
              L.Font.Style+[fsitalic];
            if pos('UNDERLINE',_inh)>0 then
             L.Font.Style:=
              L.Font.Style+[fsunderline];
           end
           else
           if id='SIZE' then begin
            L.Font.Size:=_Val(_inh);
           end
           else
           if id='FONT' then begin
            L.Font.Name:=_inh;
           end
           else
           if id='COLOR' then begin
            L.Font.Color:=_Val(_inh);
           end
           else
           if id='BACKCOLOR' then begin
            L.Color:=_Val(_inh);
           end
           else
           if id='TOP' then begin
            L.Top:=_Val(_inh);
           end
           else
           if id='LEFT' then begin
            L.Left:=_Val(_inh);
           end
           else
           if id='WIDTH' then begin
            L.Width:=_Val(_inh);
           end
           else
           if id='HEIGHT' then begin
            L.Height:=_Val(_inh);
           end
          end;
         end
         else
          delete(inh,1,pos(';',inh));
        end;

       end
       //25.04.12:
       else
        L.Caption:='';
  except
   on EXC_:Exception do
    ExceptionHintUser0('05147',EXC_.Message,EXC_.ClassName);
  end;
 end;

 procedure tBLVKarten.GetFontNames;
 var DC: HDC;

  procedure add(n:shortstring);
  begin
   if FontNamesTemp.Items.IndexOf(n)<>-1 then
    Fontname.items.add(n);
  end;

 begin
  try
   DC := GetDC(0);
   EnumFonts(DC, nil, @EnumFontsProc, Pointer(FontNamesTemp.Items));
   ReleaseDC(0, DC);

   FontName.Items.Clear;
   Add('Arial');
   Add('Tahoma');
   Add('Times New Roman');
   Add('Verdana');
   Add('Wingdings');

   FontName.Sorted := True;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05148',EXC_.Message,EXC_.ClassName);
  end;
 end;

procedure TBLVKarten.colorbuttonClick(Sender: TObject);
begin
 if EditL<>NIL then
  try
   ColorDialog1.Color := EditL.Font.Color;
   if ColorDialog1.Execute then begin
    EditL.Font.Color := ColorDialog1.Color;
    Changed:=TRUE;
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05149',EXC_.Message,EXC_.ClassName);
  end;
end;

procedure TBLVKarten.Edit(Sender:tobject);
begin
 try
{  VEdit(Sender as tLabel);
  Changed:=TRUE;}

  EditPanel.Enabled:=TRUE;
  EditL:=(Sender as tLabel);

  OK:=FALSE;
  updown1.position:=EditL.font.size;
  try
   fontname.itemindex:=fontname.items.indexof(EditL.font.name);
  except
   on EXC_:Exception do
    ExceptionHintUser0('05150',EXC_.Message,EXC_.ClassName);
  end;
  boldbutton.down     :=fsbold in EditL.font.style;
  italicbutton.down   :=fsitalic in EditL.font.style;
  underlinebutton.down:=fsunderline in EditL.font.style;
  txt.text:=EditL.Caption;                                                                          { TODO 2 -oXEFrank -c'TCaption' zu 'AnsiString' : XE }
  txt.SelStart:=length(txt.text);

  OK:=TRUE;

  txtchange(Sender);//26.06.12

  if BLVKarten.Visible then // FL02.04.2012
   txt.SetFocus;
 except
   on EXC_:Exception do
    ExceptionHintUser0('05151',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.EditP(Sender:tobject);
begin
 try
  ColorDialog1.Color := (Sender as tPanel).Color;
  if ColorDialog1.Execute then begin
   (Sender as tPanel).Color := ColorDialog1.Color;
   Changed:=TRUE;
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05152',EXC_.Message,EXC_.ClassName);
 end;
end;

Procedure TBLVKarten.ShowHide(ID:Integer);
Var P:tPanel;

 Procedure Belege(P:tPanel);
 var //07.08.13 j
  i:integer;
     s:shortstring;
 begin
  i:=0;
  try
   while (p<>nil) and (i < P.ControlCount) do begin
    s:=AnsiUppercase(P.Controls[i].Name);                                                           { TODO 2 -oXEFrank -cS_2_Short : XE }

    if P.Controls[i] is tPanel then begin //Recursiv

     With (P.Controls[i] as tPanel) do begin
      Cursor:=crHandPoint;
      (P.Controls[i] as tPanel).OnClick:=EditP;
     end;

     Belege(P.Controls[i] as tPanel);
    end
    else

    if P.Controls[i] is tImage then begin //Bilder
     try
      if Copy(S,1,6)='WAPPEN' then begin
       Clean(P.Controls[i] as tImage);
       (P.Controls[i] as tImage).Picture.Bitmap.Assign(Bmp);
       (P.Controls[i] as tImage).Transparent:=TRUE;

{SPùTER...       (P.Controls[i] as tImage).OnMouseDown:=MouseDown;
       (P.Controls[i] as tImage).OnMouseMove:=MouseMove;
       (P.Controls[i] as tImage).OnMouseUp  :=MouseUp;}
{        //Trick:
        j:=(P.Controls[i] as tImage).top;
        (P.Controls[i] as tImage).top:=1000;
        Application.ProcessMessages;
        (P.Controls[i] as tImage).top:=j;}

      end;
     except
      on EXC_:Exception do
       ExceptionHintUser0('05153',EXC_.Message,EXC_.ClassName);
     end;
    end
    else

    if P.Controls[i] is tLabel then begin //Texte
     With (P.Controls[i] as tLabel) do begin
      Cursor:=crHandPoint;
      OnClick:=Edit;

      OnMouseDown:=MyMouseDown;
      OnMouseMove:=MyMouseMove;
      OnMouseUp  :=MyMouseUp;

      //01.04.12:
      if Copy(AnsiUpperCase((P.Controls[i] as tLabel).Name),1,4)='NAME' then
       OnClick(P.Controls[i] as tLabel);

     end;
    end;
    inc(i);
   end;
  except
      on EXC_:Exception do
       ExceptionHintUser0('05154',EXC_.Message,EXC_.ClassName);
  end;
 end;

begin
 try
  Layout0.Visible := False;
  Layout1.Visible := False;
  Layout2.Visible := False;
  Layout3.Visible := False;
  Layout4.Visible := False;
  Layout5.Visible := False;
  Layout6.Visible := False;
  Layout7.Visible := False;
  RueckSeite0.Visible := False;
  RueckSeite1.Visible := False;
  RueckSeite2.Visible := False;
  Label14.Visible:=TRUE;
  P:=nil;//06.08.13 
  case ID of
     0 : begin
          P:=Layout0;
          AktPanel:=LayOut0;
         end;
     1 : begin
          P:=Layout1;
          AktPanel:=LayOut1;
         end;
     2 : begin
          P:=Layout2;
          AktPanel:=LayOut2;
         end;
     3 : begin
          P:=Layout3;
          AktPanel:=LayOut3;
         end;
     4 : begin
          P:=Layout4;
          AktPanel:=LayOut4;
          Label14.Visible:=FALSE;//da hochkant
         end;
     5 : begin
          P:=Layout5;
          AktPanel:=LayOut5;
         end;
     6 : begin
          P:=Layout6;
          AktPanel:=LayOut6;
         end;
     7 : begin
          P:=Layout7;
          AktPanel:=LayOut7;
         end;
     99 : begin//30.04.12
          P:=Layout99;
          AktPanel:=LayOut99;
         end;
   200 : begin
          P:=RueckSeite0;
          AktPanelR:=RueckSeite0;
         end;
   201 : begin
          P:=RueckSeite1;
          AktPanelR:=RueckSeite1;
         end;
   202 : begin
          P:=RueckSeite2;
          AktPanelR:=RueckSeite2;
         end;
  end;

  //06.08.13
  if P=nil then
   exit;

  P.Visible:=TRUE;

  _ID:=ID;
  if ID <200 then begin
   AktLayout := ID;
   AktLayoutName:=P.Hint;                                                                           { TODO 2 -oXEFrank -cS_2_Short : XE }
   AktPanel.Cursor:=crHandPoint;
   AktPanel.OnClick:=EditP;
   Belege(AktPanel)
  end
  else begin
   AktLayoutR := ID;
   AktLayoutNameR:=P.Hint;                                                                          { TODO 2 -oXEFrank -cS_2_Short : XE }
   AktPanelR.Cursor:=crHandPoint;
   AktPanelR.OnClick:=EditP;
   Belege(AktPanelR);
  end;

  //vCard:
  SpeedButton7.Enabled:=(ID=202);
  if ID=202 then begin
   //
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05155',EXC_.Message,EXC_.ClassName);
 end;
end;


procedure TBLVKarten.SpeedButton10Click(Sender: TObject); //Vorderseite
begin
// SpeedButton1.Enabled:=TRUE;
// SpeedButton2.Enabled:=TRUE;
 Waage.Enabled:=TRUE;
 SpeedButton5.Enabled:=TRUE;
 Vor.Visible  :=TRUE;
 Rueck.Visible:=FALSE;
 ShowHide(AktLayout);
end;

procedure TBLVKarten.SpeedButton11Click(Sender: TObject); //Rùckseite
begin
 try
//  SpeedButton1.Enabled:=FALSE;
//  SpeedButton2.Enabled:=FALSE;
  RueckGewaehlt:=TRUE;
  Waage.Enabled:=TRUE;
  SpeedButton5.Enabled:=FALSE;
  Vor.Visible  :=FALSE;
  Rueck.Visible:=TRUE;
  ShowHide(AktLayoutR);

  if Pos('einseitig',Menge.Text)>0 then begin
   Menge.ItemIndex:=Menge.ItemIndex+1;
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05156',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.SpeedButton12Click(Sender: TObject);
Var F2, F:TextFile;
    kette,
    VOR:shortstring;
    ID:Integer;

 Function _IntToStr(i:tColor):shortstring;
 begin
  Str(i,Result);
 end;

 Procedure Belege(P:tPanel);
 var i:integer;
     s:shortstring;
 begin
  i:=0;
  try
   while (p<>nil) and (i < P.ControlCount) do begin
//    s:=VOR+AnsiUppercase(P.Controls[i].Name);

    if P.Controls[i] is tPanel then begin //Recursiv
     try
      Writeln(F,inttostr(ID)+'_'+(P.Controls[i] as tPanel).name+'='+_InttoStr((P.Controls[i] as tPanel).Color)+';'+//02.04.12
                  ';Top:'+InttoStr((P.Controls[i] as tPanel).Top)+
                  ';Left:'+InttoStr((P.Controls[i] as tPanel).Left)+
                  ';Width:'+InttoStr((P.Controls[i] as tPanel).Width)+
                  ';Height:'+InttoStr((P.Controls[i] as tPanel).Height)+';');
{19.04.12                 ';Top:'+InttoStr((P.Controls[i] as tImage).Top)+
                 ';Left:'+InttoStr((P.Controls[i] as tImage).Left)+
                 ';Width:'+InttoStr((P.Controls[i] as tImage).Width)+
                 ';Height:'+InttoStr((P.Controls[i] as tImage).Height)+';');}
     except
      on EXC_:Exception do
       ExceptionHintUser0('05157',EXC_.Message,EXC_.ClassName);
     end;
     Belege(P.Controls[i] as tPanel);
    end
    else

    if P.Controls[i] is tImage then begin //Bilder
     try
      if Copy(S,1,6)='WAPPEN' then
       Writeln(F,inttostr(ID)+'_'+'WappenPos='+
                 ';Top:'+InttoStr((P.Controls[i] as tImage).Top)+
                 ';Left:'+InttoStr((P.Controls[i] as tImage).Left)+
                 ';Width:'+InttoStr((P.Controls[i] as tImage).Width)+
                 ';Height:'+InttoStr((P.Controls[i] as tImage).Height)+';')
      else
      if Copy(S,1,3)='QRV' then
       Writeln(F,inttostr(ID)+'_'+'QRVPos='+
                 ';Top:'+InttoStr((P.Controls[i] as tImage).Top)+
                 ';Left:'+InttoStr((P.Controls[i] as tImage).Left)+
                 ';Width:'+InttoStr((P.Controls[i] as tImage).Width)+
                 ';Height:'+InttoStr((P.Controls[i] as tImage).Height)+';')
      else
      if Copy(S,1,2)='QR' then
      else
       ;
     except
      on EXC_:Exception do
       ExceptionHintUser0('05158',EXC_.Message,EXC_.ClassName);
     end;
    end
    else

    if P.Controls[i] is tLabel then begin //Texte
     s:=';Style:';
     if fsbold in (P.Controls[i] as tLabel).Font.Style then
      s:=s+'BOLD';
     if fsitalic in (P.Controls[i] as tLabel).Font.Style then
      s:=s+'ITALIC';
     if fsunderline in (P.Controls[i] as tLabel).Font.Style then
      s:=s+'UNDERLINE';
     s:=#9'Font:'+(P.Controls[i] as tLabel).Font.Name+
        ';Size:'+Inttostr((P.Controls[i] as tLabel).Font.Size)+
        s+
        ';Color:'+_InttoStr((P.Controls[i] as tLabel).Font.Color)+
        ';BackColor:'+_InttoStr((P.Controls[i] as tLabel).Color)+
        ';Top:'+InttoStr((P.Controls[i] as tLabel).Top)+
        ';Left:'+InttoStr((P.Controls[i] as tLabel).Left)+
        ';Width:'+InttoStr((P.Controls[i] as tLabel).Width)+
        ';Height:'+InttoStr((P.Controls[i] as tLabel).Height)+';';                                  { TODO 2 -oXEFrank -cS_2_Short : XE }
     Writeln(F,inttostr(ID)+'_'+(P.Controls[i] as tLabel).Name+'='+(P.Controls[i] as tLabel).Caption+s);
    end;
    inc(i);
   end;
  except
      on EXC_:Exception do
       ExceptionHintUser0('05159',EXC_.Message,EXC_.ClassName);
  end;
 end;

begin
 try
  ioresult;
  //28.04.12
  if (Sender as tmystdbutton).tag=99 then begin
   ioresult;
   RenameFile(OpenD.Filename,ChangeFileExt(OpenD.Filename,'.ORIGINAL.TXT'));
   ioresult;
   AssignFile(F,OpenD.Filename);
  end
  else

   AssignFile(F,DTS+'VISITENKARTE.'+StdExt);
  Rewrite(F);
  if ioresult=0 then begin
   try
    //28.04.12
    if (Sender as tmystdbutton).tag=99 then begin
     Writeln(F,_Anwender+','+_KNR);
     Writeln(F,_BL);
     Writeln(F,'Datum='+_D);
     Writeln(F,'Uhrzeit='+_Z);
    end
    //25.06.12:
    else begin
     Writeln(F,_Anwender+','+_KNR);
     Writeln(F,'Datum='+_D);
     Writeln(F,'Uhrzeit='+_Z);
    end;

    Writeln(F,'Layout='+inttostr(AktLayout));
    Writeln(F,'LayoutR='+inttostr(AktLayoutR));
    Writeln(F,'Menge='+Menge.Text);
    Writeln(F,'Wappen='+Inttostr(FLand));//16=Waage; 17=nichts!
    Vor:='';
    ID:=AktLayout;
    Writeln(F,inttostr(ID)+'_'+AktPanel.name+'='+_InttoStr(AktPanel.Color));//02.04.12
    Belege(AktPanel);
 //  Vor:='R_';

    if ((Sender as tmystdbutton).tag<>99) or (Pos('doppel',Menge.Text)>0) then//28.04.12
     if AktPanelR<>NIL then begin
      ID:=AktLayoutR;
      Writeln(F,inttostr(ID)+'_'+AktPanelR.name+'='+_InttoStr(AktPanelR.Color));//02.04.12
      Belege(AktPanelR);
     end;

    //28.04.12
    if (AktLayoutR=202) and ((Sender as tmystdbutton).tag=99) then begin
     ioresult;
     AssignFile(F2,DTS+'VCARD VISITENKARTE.'+stdext);
     Reset(F2);
     if IOResult=0 then begin
      while not(eof(f2)) do begin
       readln(f2,kette);
       writeln(f,'vCard='+kette);
      end;
      CloseFile(F2);
      ioresult;
     end;
    end;

   except
    on EXC_:Exception do
     ExceptionHintUser0('05160',EXC_.Message,EXC_.ClassName);
   end;
   ioresult;
   CloseFile(F);
   ioresult;
   WFehler('Visitenkarte gespeichert (falls gewùhlt: auch Rùckseite)',2);
  end
  else
   WFehler('Visitenkarte kann NICHT gespeichert werden!!!',2);
  SpeedButton13.Enabled:=TRUE;
 except
    on EXC_:Exception do
     ExceptionHintUser0('05161',EXC_.Message,EXC_.ClassName);
 end;
 //21.04.16:
 SpeedButton13.Enabled:=FileExists(DTS+'VISITENKARTE.'+StdExt);//Laden Button
end;

procedure TBLVKarten.SpeedButton13Click(Sender: TObject);
Var F:TextFile;
    IDNr,
    i:integer;
    id,inh:shortstring;
    found:boolean;
//06.08.13     rueck:boolean;
    L:tLabel;

 Procedure Get(Var id,inh:shortstring);
 begin
  try
   inh:='';
   readln(f,id);
   idnr:=-1;
   if pos('=',id)>0 then begin
    inh:=trim(copy(id,pos('=',id)+1,255));                                                          { TODO 2 -oXEFrank -cS_2_Short : XE }
    id :=AnsiUpperCase(trim(copy(id,1,pos('=',id)-1)));                                             { TODO 2 -oXEFrank -cS_2_Short : XE }

    //03.04.12 ID:
    if pos('_',ID)>0 then begin
     Val(Copy(ID,1,pos('_',ID)-1),IDNr,Dek.Error);
     delete(ID,1,pos('_',ID));
    end;

   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05162',EXC_.Message,EXC_.ClassName);
  end;
 end;

 Procedure Belege(P:tPanel;id:shortstring;var found:boolean);
 var i:integer;
     s:shortstring;
 begin
  i:=0;
  try
   while (p<>nil) and (i < P.ControlCount) and not(found) do begin
    s:=AnsiUppercase(P.Controls[i].Name);                                                           { TODO 2 -oXEFrank -cS_2_Short : XE }

    if P.Controls[i] is tPanel then begin //Recursiv
     if s=ID then begin
      found:=true;
      (P.Controls[i] as tPanel).color:=_val(inh);
     end;
     Belege(P.Controls[i] as tPanel,id,found);
    end
    else begin
     if s=ID then begin
      found:=true;

      if (P.Controls[i] is tImage) and
         (Copy(S,1,9)='WAPPENPOS') then begin
{       Writeln(F,'WappenPos='+
                 ';Top:'+InttoStr((P.Controls[i] as tLabel).Top)+
                 ';Left:'+InttoStr((P.Controls[i] as tLabel).Left)+
                 ';Width:'+InttoStr((P.Controls[i] as tLabel).Width)+
                 ';Height:'+InttoStr((P.Controls[i] as tLabel).Height)+';')}
      end
      else

{      if Copy(S,1,3)='QRV' then begin
      end
      else}
      if P.Controls[i] is tLabel then begin //Texte
       DoLabel(P.Controls[i] as tLabel,id,inh);
      end;
      found:=TRUE;//02.04.12
     end;
    end;
    inc(i);
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05163',EXC_.Message,EXC_.ClassName);
  end;
 end;

 procedure DoNotFound;
 var fp,p:tpanel;
     f:boolean;
     n:shortstring;

  procedure Such(P:tPanel;var f:boolean;var foundp:tpanel);
  var i:integer;
  begin
   try
    i:=0;
    while (p<>nil) and (i < P.ControlCount) and not(f) do begin
     if P.Controls[i] is tPanel then //Recursiv
      if AnsiUpperCase((P.Controls[i] as tPanel).name)=n then begin
       f:=true;
       fp:=(P.Controls[i] as tPanel);
      end
      else
       Such(P.Controls[i] as tPanel,f,fp);
     inc(i);
    end;
   except
    on EXC_:Exception do
     ExceptionHintUser0('05164',EXC_.Message,EXC_.ClassName);
   end;
  end;

 begin//DoNotFound
  try
   f:=false;
   P:=Root;

   n:=ID;
   if pos('_',n)>0 then begin
    delete(n,1,pos('_',n));
    if pos('_',n)>0 then begin
     delete(n,pos('_',n),255);

     fp:=nil;
     Such(P,f,fp); //Panel n suchen
     if f then begin
      NeuLabel(FP,L,'NEU_'+chr(random(20)+65)+chr(random(20)+65)+
               chr(random(20)+65)+chr(random(20)+65)+
               chr(random(20)+65)+chr(random(20)+65)+
               chr(random(20)+65)+chr(random(20)+65),                                               { TODO 2 -oXEFrank -cS_2_Short : XE }
               'Neu...',50,100,clblack,10,'Tahoma',[]);
      DoLabel(L,id,inh);
     end;
    end;
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05165',EXC_.Message,EXC_.ClassName);
  end;
 end;

begin //SpeedButton13Click
 if FileExists(DTS+'VISITENKARTE.'+StdExt) then begin
  if not(Changed) or
     (TDialogs.MyMessageDlgOK('Gespeichertes Design laden? Aktuelle Anzeige wird ùberschrieben!',
                   mtconfirmation,[mbyes,mbno],0)=mryes) then begin
   try
    try
     DeleteFile(TempPath+'QRV.BMP');
     WinStamm6(101);//QRvCard-Datei erzeugen!!!
     if FileExists(TempPath+'QRV.BMP') then begin
      QRV2.Picture.Bitmap.LoadFromFile(TempPath+'QRV.BMP');
     end;
    except
     on EXC_:Exception do
      ExceptionHintUser0('05166',EXC_.Message,EXC_.ClassName);
    end;

    AktLayout:=0;
    AktLayoutR:=200;

//06.08.13    RUECK:=TRUE;
    begin//    for j:=1 to 2 do begin
     ioresult;
     AssignFile(F,DTS+'VISITENKARTE.'+StdExt);
     Reset(F);
     if IOResult=0 then begin
      try
       for i := 1 to 4 do begin
        Get(id,inh);
        if ID='LAYOUT' then
         AktLayout:=_Val(Inh)
        else
        if ID='LAYOUTR' then
         AktLayoutR:=_Val(Inh)
        else
        if ID='MENGE' then begin
         try
          if Menge.Items.IndexOf(Inh)<>-1 then
           Menge.ItemIndex:=Menge.Items.IndexOf(Inh);
         except
          on EXC_:Exception do
           ExceptionHintUser0('05167',EXC_.Message,EXC_.ClassName);
         end;
        end
        else
        if ID='WAPPEN' then
         FLand:=_Val(Inh);
       end;
{       if Rueck then
        ShowHide(AktLayoutR)
       else}
       ShowHide(AktLayoutR);//02.04.12
       ShowHide(AktLayout);
       SetWappen;
       while not(Eof(F)) do begin
        Get(id,inh);
{        if RUECK and (Copy(ID,1,2)='R_' then
         Belege(Root}
        found:=FALSE;
        Belege(Root,id,found);//damit Vor- und Rùckseite geladen werden! AktPanel);

        //03.04.12: Neue Labels!
        if not(found) and (Pos(#9,Inh)>0) then 
         DoNotFound;
       end;

       if Fland<16 then
        Waage.Checked:=FALSE//SpeedButton1.Down:=TRUE
       else
        Waage.Checked:=TRUE;//SpeedButton2.Down:=TRUE;

      except
          on EXC_:Exception do
           ExceptionHintUser0('05168',EXC_.Message,EXC_.ClassName);
      end;
      CloseFile(F);
      ioresult;
//      RUECK:=FALSE;
     end
     else
      WFehler('ùffnen nicht mùglich! Ggf. bitte Programm neu starten.',2);

    end;
    ioresult;
    EditPanel.Enabled:=FALSE;
   except
          on EXC_:Exception do
           ExceptionHintUser0('05169',EXC_.Message,EXC_.ClassName);
   end;
  end;
 end
 else
  WFehler('Keine Visitenkarte gespeichert!',2);
end;

procedure tblvkarten.CreateVorlage;
var r:trect;
    Vorlage,
    VorlageR:tBitmap;

 lPdf  : TPdfDocumentGDI;// : TPdfDocument;
 lPage  : TPdfPage;
//06.08.13  NativeData : TStringlist;
//06.08.13  i     : Integer;
//06.08.13  Image : TBitmap;
//06.08.13  pdfImage : TPdfImage;

 procedure C(var Vorlage:tbitmap);
 begin
  Vorlage:=tBitmap.Create;
  Vorlage.PixelFormat:=pf24Bit;
  with Vorlage,Vorlage.Canvas do begin
   //300DPI 8,9x5,9cm
   //9,1x6,1 = 1075x721
   Width :=1076;//08.05.12 1059;
   Height:= 721;//08.05.12 697;

   Pen.Color   := clwhite;
   Brush.Color := clwhite;
   Brush.style := bssolid;
   r.top:=0;
   r.Left:=0;
   r.right:=width-1;
   r.bottom:=height-1;
   FillRect(R);

  end;
 end;

 procedure Lies;
 Const    Lw=480;//x2 = 960 = 85mm
          lh=311;//x2 = 480 = 55mm
          //91mm = 960x91/85 = 1028
          //61mm = 480x61/55 =  532
        PDFw=1076;//08.05.12 1059;                                              **300DPI!** 91mm
        PDFh= 721;//08.05.12 710;//08.05.12 697;                                **300DPI!** 61mm
   PDFwInnen=1003;//85mm
   PDFhInnen= 650;//55mm
      Factor=1003-960;
      RANDx_=(PDFw-PDFwInnen) div 2;//08.05.12 (PDFw - (2*Lw)) div 2;//28.04.12 49;//1059 - 960 / 2 = 99
      RANDy_=(PDFh-PDFhInnen) div 2;//08.05.12 (PDFh - (2*Lh)) div 2;//28.04.12 37;//697-622 / 2
 var f:textfile;
//06.08.13      m,
//06.08.13      id,
//03.10.14     inh:shortstring;
//06.08.13      d,
//06.08.13      z,
//06.08.13      onlineid,
//06.08.13      s:shortstring;
     Randx,Randy,
//     Layout,
//06.08.13      LayoutR,
//06.08.13      i,
     idnr:integer;
//03.10.14     w:integer;
     hoch,
     rueck:boolean;

   Procedure Get(Var id,inh:shortstring);
   begin
    try
     id:='';
     inh:='';
     readln(f,id);
     idnr:=-1;
     if pos('=',id)>0 then begin
      inh:=trim(copy(id,pos('=',id)+1,255));                                                        { TODO 2 -oXEFrank -cS_2_Short : XE }
      id :=AnsiUpperCase(trim(copy(id,1,pos('=',id)-1)));                                           { TODO 2 -oXEFrank -cS_2_Short : XE }

      //03.04.12 ID:
      if pos('_',ID)>0 then begin
       Val(Copy(ID,1,pos('_',ID)-1),IDNr,Dek.Error);
       delete(ID,1,pos('_',ID));
      end;

     end;
    except
     on EXC_:Exception do
      ExceptionHintUser0('05170',EXC_.Message,EXC_.ClassName);
    end;
   end;

   Function Aspecti(i:integer):Integer;
   begin
//08.05.12    Result:=i*2;
    //08.05.12:
    Result:=ROUND(i*2.04479);
   end;

   Procedure Aspect(L:tControl);
   begin
     L.Left :=Aspecti(L.Left);
     L.Top  :=Aspecti(L.Top);
   end;

    Function G(such:shortstring;inh:shortstring):Integer;
    var s:shortstring;
    begin
     Result:=0;
     if Pos(such,ansiuppercase(inh))>0 then begin
      s:=Copy(inh,Pos(such,ansiuppercase(inh))+length(such),10);
      if pos(';',s)>0 then begin
       Delete(s,pos(';',s),255);
       Result:=_Val(s);
      end;
     end;
    end;

    Procedure GetRect(var r:trect;inh:shortstring);
    begin
     r.Top:=G('TOP',inh);
     r.Left:=G('LEFT',inh);
     r.Bottom:=G('HEIGHT',inh);
     r.Right:=G('WIDTH',inh);
     inc(r.Right,r.Left);
     inc(r.Bottom,r.Top);
    end;

(*08.05.12   Procedure Belege;
   var id:shortstring;

     procedure _DoLabel;
     var L:tLabel;
     begin
      try

       L:=tLabel.Create(NIL);
       L.WordWrap   :=FALSE;
       L.Transparent:=TRUE;
       L.AutoSize   :=TRUE;
       L.ParentColor:=TRUE;
       L.Name       :='NEU';
       L.ShowAccelChar:=FALSE;
       L.Align   :=alnone;
       L.Visible :=TRUE;
       L.Enabled :=TRUE;

       try
        DoLabel(L,ID,Inh);

        Aspect(L);
        L.Font.Size:=Aspecti(L.Font.Size);//Todo? hmm....
        lPDF.VCLCanvas.Font.Assign(L.Font);

{        //TODO!:
        lPDF.Canva s.SetFont(L.Name,L.Font.Size,L.Font.Style);
 //        lPDF.Canva s.SetLeading(lPDF.Canvas.Page.FontSize);}

        lPDF.VCLCanvas.Textout(L.Left+Randx,L.Top+Randy,L.Caption);

       except
        on EXC_:Exception do
         ExceptionHintUser0('05171',EXC_.Message,EXC_.ClassName);
       end;
       L.Free;

      except
        on EXC_:Exception do
         ExceptionHintUser0('05172',EXC_.Message,EXC_.ClassName);
      end;
     end;

     procedure _DoPanel;
     var r:trect;
         c:integer;
     begin
      if Pos('=',inh)>0 then begin
       Delete(inh,1,Pos('=',inh));
      end
      else
       Exit;

      fillchar(r,sizeof(r),0);
      if Pos(';',inh)>0 then begin
       //Unterpanel
       c:=_Val(copy(inh,1,Pos(';',inh)-1));//Color
       Delete(inh,1,Pos(';',inh)-1);

       GetRect(r,inh);

       //ùber Rand gehen:
       if r.Left<=1 then begin
        inc(r.right,Randx);
       end
       else
        inc(r.left,Randx);
       if r.Top<=1 then begin
        inc(r.bottom,Randy);
       end
       else
        inc(r.Top,Randy);
       if r.Right>=lPdf.DefaultPageWidth-2 then begin
        inc(r.Right,Randx*2);
       end
       else
        inc(r.Right,Randx);
       if r.Bottom>=lPdf.DefaultPageHeight-2 then begin
        inc(r.Bottom,Randy*2);
       end
       else
        inc(r.Bottom,Randy);
{
    Writeln(F,inttostr(ID)+'_'+AktPanel.name+'='+_InttoStr(AktPanel.Color));//02.04.12

      Writeln(F,inttostr(ID)+'_'+(P.Controls[i] as tPanel).name+'='+_InttoStr((P.Controls[i] as tPanel).Color)+';'+//02.04.12
                  ';Top:'+InttoStr((P.Controls[i] as tPanel).Top)+
                  ';Left:'+InttoStr((P.Controls[i] as tPanel).Left)+
                  ';Width:'+InttoStr((P.Controls[i] as tPanel).Width)+
                  ';Height:'+InttoStr((P.Controls[i] as tPanel).Height)+';');

}
      end
      else begin
       //Hauptpanel Farbe!
       r.top:=0;
       r.Left:=0;
       r.Right:=lPage.PageWidth;
       r.bottom:=lPage.PageHeight;
       c:=_Val(inh);
      end;
      lPDF.VCLCanvas.Brush.style:= bssolid;
      lPDF.VCLCanvas.Brush.Color:=c;
      lPDF.VCLCanvas.FillRect(r);
     end;

   begin //Get
    try
     if Pos('VCARD',ID)>0 then begin
      //ToDo!
     end
     else
     if Pos('QRV',ID)>0 then begin//QRV Grafik
      //ToDo!
     end
     else
     if Pos(#9,INH)>0 then begin//Label
      _DoLabel;
     end
     else begin//Panel
      _DoPanel;
     end;
    except
     on EXC_:Exception do
      ExceptionHintUser0('05173',EXC_.Message,EXC_.ClassName);
    end;
   end;*)

(*17.09.14
     procedure _DoWappen;
     Var B:tBitmap;
         r:trect;
         f:Boolean;
         p:tPanel;
         i:timage;

      procedure Such(P:tPanel;var f:boolean;var fp:tpanel);
      var i:integer;
      begin
       try
        i:=0;
        while (p<>nil) and (i < P.ControlCount) and not(f) do begin
         if P.Controls[i] is tPanel then //Recursiv
          if AnsiUpperCase((P.Controls[i] as tPanel).name)='LAYOUT'+inttostr(Layout) then begin
           f:=true;
           fp:=(P.Controls[i] as tPanel);
          end
          else
           Such(P.Controls[i] as tPanel,f,fp);
         inc(i);
        end;
       except
        on EXC_:Exception do
         ExceptionHintUser0('05174',EXC_.Message,EXC_.ClassName);
       end;
      end;

      procedure SuchW(P:tPanel;var im:timage);
      var i:integer;
      begin
       try
        i:=0;
        while (p<>nil) and (i < P.ControlCount) and (im=nil) do begin
         if P.Controls[i] is tPanel then //Recursiv
          SuchW(P.Controls[i] as tPanel,im)
         else
         if (P.Controls[i] is timage) and
            (Pos('WAPPEN',AnsiUpperCase((P.Controls[i] as timage).name))>0) then begin
          IM:=(P.Controls[i] as timage);
         end;
         inc(i);
        end;
       except
        on EXC_:Exception do
         ExceptionHintUser0('05175',EXC_.Message,EXC_.ClassName);
       end;
      end;

     Var fWappen:tfwappen; 
     begin//_DoWappen
      try
       B:=tBitmap.Create;
       B.PixelFormat:=pf24Bit;
       B.Width:=0;

       begin
        try
         fWappen := tfwappen.Create(application);
         fwappen.visible:=FALSE;
         fwappen.closeok:=TRUE;
         try

          if w>16 then //Waage
           GetWapp(99,temppath+'WAPPEN.BMP')
          else
          //14.03.14:
          if w=4 then //12.04.14 5 then
           GetWapp(16,temppath+'WAPPEN.BMP')//einfaches Nds-Wappen
          else

           GetWapp(w,temppath+'WAPPEN.BMP');

          if FileExists(temppath+'WAPPEN.BMP') and (MyGetFileSize(temppath+'WAPPEN.BMP')<>0) then
           B.LoadFromFile(temppath+'WAPPEN.BMP')
         except
          on EXC_:Exception do
           ExceptionHintUser0('05176',EXC_.Message,EXC_.ClassName);
         end;
         fwappen.close;//24.04.14 Schit freeAndNil macht keinen Close!!!!
         freeAndNil(fwappen);
        except
          on EXC_:Exception do
           ExceptionHintUser0('05177',EXC_.Message,EXC_.ClassName);
        end;
       end;
       if B.Width>0 then begin
        GetRect(r,inh);
        //wenn keine Infos ùber Pos:
        if r.Top=0 then begin
         //TODO! Layout suchen, dann Wappenpos holen!
         f:=FALSE;
         SUCH(RootRoot,f,P);
         if f then begin
          i:=nil;
          SuchW(P,i);
          if i<>NIL then begin
           r.top   :=Aspecti(r.top)+Randy;
           r.left  :=Aspecti(r.Left)+Randx;
           r.Right :=r.left+Aspecti(r.right)+Randx;
           r.bottom:=r.top+Aspecti(r.bottom)+Randy;
          end;
         end;
        end;

        if (r.bottom<>0) and (r.right<>0) then
         lPDF.VCLCanvas.StretchDraw(r,B);
       end;
      except
          on EXC_:Exception do
           ExceptionHintUser0('05178',EXC_.Message,EXC_.ClassName);
      end;
     end;*)
{
       Writeln(F,inttostr(ID)+'_'+'WappenPos='+
                 ';Top:'+InttoStr((P.Controls[i] as tImage).Top)+
                 ';Left:'+InttoStr((P.Controls[i] as tImage).Left)+
                 ';Width:'+InttoStr((P.Controls[i] as tImage).Width)+
                 ';Height:'+InttoStr((P.Controls[i] as tImage).Height)+';')
      else
      if Copy(S,1,3)='QRV' then
       Writeln(F,inttostr(ID)+'_'+'QRVPos='+
                 ';Top:'+InttoStr((P.Controls[i] as tImage).Top)+
                 ';Left:'+InttoStr((P.Controls[i] as tImage).Left)+
                 ';Width:'+InttoStr((P.Controls[i] as tImage).Width)+
                 ';Height:'+InttoStr((P.Controls[i] as tImage).Height)+';')

}

   procedure CreatePDF;
   var fac:single;
   begin
    lPdf := TPdfDocumentGDI.Create; //TPdfDocument.Create;
    try
      lPdf.PDFA1              := false; // Ist kùrzer
      lPdf.Info.Author        := LIZENZ1D;//10.09.22 'Baq uù & Lauter GmbH';
      lPdf.Info.CreationDate  := Now;
      lPdf.Info.Creator       := produktname;
      lPdf.Info.Title         := 'Visitenkarte';

      //08.05.12:
      lPDF.EmbeddedTTF            :=TRUE;
//Exception      lPDF.EmbeddedTTFIgnore.Text :='';
      lPDF.StandardFontsReplace   :=FALSE;
      lPDF.EmbeddedWholeTTF       :=FALSE;

//      lPDF.EmbeddedWholeTTF
//    property CodePage: cardinal read FCodePage;
//    property CharSet: integer read FCharSet;

      if rueck then
       lPdf.Info.Subject      := 'Rùckseite'
      else
       lPdf.Info.Subject      := 'Vorderseite';
      lPdf.DefaultPaperSize   := psUserDefined;//psA4;

      //08.05.12:
      lpdf.ScreenLogPixels:=300;

      Fac:=72/lPdf.ScreenLogPixels;
      if hoch then begin
       lPdf.DefaultPageWidth  := trunc(PDFh*Fac);
       lPdf.DefaultPageHeight := trunc(PDFw*Fac);
       Randx:=trunc(Randy_);//*FAC);
       Randy:=trunc(Randx_);//*FAC);
      end
      else begin
       lPdf.DefaultPageWidth  := trunc(PDFw*Fac);
       lPdf.DefaultPageHeight := trunc(PDFh*Fac);
       Randx:=trunc(Randx_);
       Randy:=trunc(Randy_);
      end;

      lPage := lPDF.AddPage;

      lPDF.Canvas.SetFont('Tahoma',12.0,[]);
      lPDF.Canvas.SetLeading(lPDF.Canvas.Page.FontSize);
      lPDF.Canvas.SetLineWidth(0.1);

    except
     on EXC_:Exception do
      ExceptionHintUser0('05179',EXC_.Message,EXC_.ClassName);
    end;
   end;

   procedure Neu;
   VAR BIT:tBitmap;

     procedure Save;//23.04.12
     {$IFDEF FRANK}
     const gk='g:\Kunden\Visitenkarten\'; // FL 10.09.21
     {$ELSE}
     const gk='\\Achim2\g\kunden\Visitenkarten\';//15.10.19 'g:\kunden\Visitenkarten\';
     {$ENDIF}

      procedure Save2K(ad:shortstring);//25.04.12
      var //06.08.13  Bitmap : tBitmap;
          JpegImage : tJpegImage;
      begin
       try
        try
         JpegImage := tJpegImage.Create;
         JpegImage.Assign(BIT);
         JpegImage.SaveToFile(gk+_KNR+'-1'+ad+'.jpg');
         JpegImage.Free;
        except
         on EXC_:Exception do
          ExceptionHintUser0('05180',EXC_.Message,EXC_.ClassName);
        end;

        if Ad='V' then begin
         CopyRenameSilent(OpenD.Filename,gk+_KNR+'-1.txt');
        end;

  (* KLAPPT NICHT!!
        Bitmap := tBitmap.Create;
        Bitmap.Pixelformat:=pf24Bit;
        Bitmap.Width := lPdf.DefaultPageWidt h;//ClientWidth;
        Bitmap.Height := lPdf.DefaultPageHeight;//ClientHeight;
        Bitmap.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height),
                               lPdf.VCLCanvas, Rect(0, 0, lPdf.DefaultPageWidt h, lPdf.DefaultPageHeight));
        Bitmap.SaveToFile('g:\kunden\Visitenkarten\'+_KNR+ad+'.bmp');
        try
         JpegImage := tJpegImage.Create;
         JpegImage.Assign(Bitmap);
         JpegImage.SaveToFile('g:\kunden\Visitenkarten\'+_KNR+ad+'.jpg');
         JpegImage.Free;
        except
         on EXC_:Exception do
          ExceptionHintUser0('05181',EXC_.Message,EXC_.ClassName);
        end;
        Bitmap.Free;*)
       except
         on EXC_:Exception do
          ExceptionHintUser0('05182',EXC_.Message,EXC_.ClassName);
       end;
      end;

     begin//Save
      try

       if pos(',',_Anwender)>0 then
        _KNR:=Copy(_Anwender,pos(',',_Anwender)+1,20);

       try
        if _Anwender>'' then begin
         if Rueck then begin
          FileNameR:=ExtractFilePath(OpenD.Filename)+'Visitenkarte '+_Anwender+' Rùckseite.pdf';    { TODO 2 -oXEFrank -cS_2_Short : XE }
          if AktLayout<>99 then //30.04.12
           Save2K('R');
          lPdf.SaveToFile(FileNameR);
          if AktLayout<>99 then //30.04.12
           CopyRenameSilent(FileNameR,gk+_KNR+'-1R.pdf');
         end
         else begin
          FileNameV:=ExtractFilePath(OpenD.Filename)+'Visitenkarte '+_Anwender+' Vorderseite.pdf';  { TODO 2 -oXEFrank -cS_2_Short : XE }
          if AktLayout<>99 then //30.04.12
           Save2K('V');
          lPdf.SaveToFile(FileNameV);
          if AktLayout<>99 then //30.04.12
           CopyRenameSilent(FileNameV,gk+_KNR+'-1V.pdf');
         end;
         wfehler('Abgelegt unter: '+ExtractFilePath(OpenD.Filename)+'Visitenkarte '+_Anwender+'*.pdf + beim Kunden gespeichert!',2);
         //
        end
        else begin
         if Rueck then begin
          FileNameR:=GetLocalRootFolder+'Visitenkarte Rùckseite.pdf';
          lPdf.SaveToFile(FileNameR)
         end
         else begin
          FileNameV:=GetLocalRootFolder+'Visitenkarte Vorderseite.pdf';
          lPdf.SaveToFile(FileNameV);
         end;
         wfehler('Abgelegt unter '+GetLocalRootFolder+'Visitenkarte*.pdf',2);
        end;
       finally
       end;
      except
        on EXC_:Exception do
         ExceptionHintUser0('05183',EXC_.Message,EXC_.ClassName);
      end;
     end;

    Procedure FreePDF;
    begin
     try
      if assigned(lPDF) then
       lPdf.Free;
      if assigned(lPage) then
       lPage.Free;  //FL 13.02.19 Exception nicht nùtig, aber ich lasse es mal drin da abgefangen
     except
      on EXC_:Exception do
       ExceptionHintUser0('05184',EXC_.Message,EXC_.ClassName);
     end;
    end;

    procedure DoPDF;

       procedure _DovCard(IM:tImage);//28.04.12
       VAR B:tBitmap;
//06.08.13            f:textfile;
//06.08.13            s:shortstring;
//06.08.13            i:integer;
       begin
        try
         B:=tBitmap.Create;
         B.Pixelformat:=pf24Bit;
         B.Width:=0;

{schon erledigt!         ioresult;
         AssignFile(F,DTS+'VCARD VISITENKARTE.'+stdext);
         Rewrite(F);
         ioresult;
         for i := 1 to 20 do
          writeln(F,vC[i]);
         CloseFile(F);
         ioresult;}

         DeleteFile(TempPath+'QRV.BMP');
         WinStamm6(101);//QRvCard-Datei erzeugen!!!
         if FileExists(TempPath+'QRV.BMP') then begin
          B.LoadFromFile(TempPath+'QRV.BMP');

          if (B.width>0) then begin
           r.top   :=Aspecti(IM.top)+Randy;
           r.left  :=Aspecti(IM.Left)+Randx;
           r.Right :=r.left+Aspecti(IM.Width);
           r.bottom:=r.top+Aspecti(IM.Height);
           lPDF.VCLCanvas.StretchDraw(r,B);

           BIT.Canvas.StretchDraw(r,B);
          end;
         end;
        except
         on EXC_:Exception do
          ExceptionHintUser0('05185',EXC_.Message,EXC_.ClassName);
        end;
       end;

       Procedure _NativIMG(IM:tImage); //FL 13.02.19 Neu fùr Strich
       var
         B:tBitmap;
         r:trect;
       begin // Kein Transparent also nur fùr weiùe V-Karten
         try
           B := IM.Picture.Bitmap;
           r.top   :=Aspecti(IM.top)+Randy;
           r.left  :=Aspecti(IM.Left)+Randx;
           r.Right :=r.left+Aspecti(IM.Width);
           r.bottom:=r.top+Aspecti(IM.Height);
           lPDF.VCLCanvas.StretchDraw(r,B);

           BIT.Canvas.StretchDraw(r,B);
         except
          on EXC_:Exception do
           ExceptionHintUser0('05186',EXC_.Message,EXC_.ClassName);
         end;
       end;

       procedure _DoWappen(IM:tImage);
       Var B:tBitmap;
           r:trect;
//06.08.13            f:Boolean;
//06.08.13            p:tPanel;
       fWappen:tfwappen;
//06.08.13       pdfImage:TPdfImage;
        begin//_DoWappen
        try
         B:=tBitmap.Create;
         B.Pixelformat:=pf24Bit;
         B.Width:=0;

         //30.04.12:
         if (FLand=99) AND (AktLayout=99){02.12.15} then begin//28.01.15 and (Layout=99){17.09.14} then begin
          B.Free;
          {Problem Hintergrund
          B.Transparent:=TRUE;
          B.Assign(GVS.Picture.Bitmap);
          B.Transparent:=TRUE;
          }
          try
           if FileExists('g:\kunden\gvs-big.bmp') then
            GVS.Picture.Bitmap.LoadFromFile('g:\kunden\gvs-big.bmp');
          except
           on EXC_:Exception do
            ExceptionHintUser0('05187',EXC_.Message,EXC_.ClassName);
          end;

          B:=GVS.Picture.Bitmap;
          lPDF.VCLCanvas.Brush.Color:=clWhite;
          lPDF.VCLCanvas.Brush.Style:=bssolid;
          lPDF.VCLCanvas.CopyMode := cmSrcAnd;
         end
         else
         
         if FLand=17 then begin//nix, nein Waage
         end
         else begin
          try
           fWappen := tfwappen.Create(application);
           fwappen.visible:=FALSE;
           fwappen.closeok:=TRUE;
           try
{29.11.13, ??? 1. 17=Waage, 2. 4=NDS und es gibt kein 2. Logo auf 16
            if FLand>=16 then //Waage
             FLand:=99//25.04.12 Get Wapp(99,temppath+'WAPPEN.BMP')
            else
            if FLand=4 then //Nds
             FLand:=16;}
            ioresult;
            DeleteFile(temppath+'WAPPEN.BMP');
            ioresult;
//14.03.14            GetWapp(FLand,temppath+'WAPPEN.BMP');

            if FLand>16 then //17.03.14  w>16 then //Waage
             GetWapp(99,temppath+'WAPPEN.BMP')
            else
            //14.03.14:
            if FLand=4 then//12.04.14  5 then //17.03.14 w=5 then
             GetWapp(16,temppath+'WAPPEN.BMP')//einfaches Nds-Wappen
            else

             GetWapp(FLand,temppath+'WAPPEN.BMP');


            if FileExists(temppath+'WAPPEN.BMP') and (MyGetFileSize(temppath+'WAPPEN.BMP')<>0) then
             B.LoadFromFile(temppath+'WAPPEN.BMP')
           except
            on EXC_:Exception do
             ExceptionHintUser0('05188',EXC_.Message,EXC_.ClassName);
           end;
           fwappen.close;//24.04.14 Shit freeAndNil macht keinen Close!!!!
           freeAndNil(fwappen);
          except
            on EXC_:Exception do
             ExceptionHintUser0('05189',EXC_.Message,EXC_.ClassName);
          end;
         end;
{         if B.Width>0 then begin
          r.top   :=Aspecti(IM.top)+Rand y;
          r.left  :=Aspecti(IM.Left)+Rand x;
          r.bottom:=r.top+Aspecti(IM.Height)+Rand y;
          r.Right :=r.left+Aspecti(IM.Width)+Rand x;
         end;}
//         if (r.bottom<>0) and (r.right<>0) then begin
         if (B.width>0) then begin

          r.top   :=Aspecti(IM.top)+Randy;
          r.left  :=Aspecti(IM.Left)+Randx;
          r.Right :=r.left+Aspecti(IM.Width);
          r.bottom:=r.top+Aspecti(IM.Height);
          lPDF.VCLCanvas.StretchDraw(r,B);

          BIT.Canvas.StretchDraw(r,B);

{          pdfImage := TPdfImage.Create(lpdf, B,true);
          lpdf.AddXObject('image1', pdfimage);
          lpdf.Canvas.DrawXObject(//R.Left,R.Top,R.Right,R.Bottom,
                                  Aspecti(IM.Left)+Rand x,
                                  Aspecti(IM.top)+Rand y,
                                  Aspecti(IM.Width)+Rand x,
                                  Aspecti(IM.Height)+Rand y,
                                  'image1');}
         end;
        except
            on EXC_:Exception do
             ExceptionHintUser0('05190',EXC_.Message,EXC_.ClassName);
        end;
       end;


       Var PP:tPanel;
       r:trect;
       
       Procedure Belege(P:tPanel);
       var Addx,
           Addy,
//06.08.13            j,
           i:integer;
           s:shortstring;
           r2,
           r:trect;
           pr:tpdfrect;
           L:tLabel;
       begin
        i:=0;
        try
         while (p<>nil) and (i < P.ControlCount) do begin
          s:=AnsiUppercase(P.Controls[i].Name);                                                     { TODO 2 -oXEFrank -cS_2_Short : XE }

          if P.Controls[i] is tPanel then begin //Recursiv
           try
            r.Top   :=((P.Controls[i] as tPanel).Top);
            r.Left  :=((P.Controls[i] as tPanel).Left);
            r.Bottom:=(r.Top+(P.Controls[i] as tPanel).Height);
            r.Right :=(r.Left+(P.Controls[i] as tPanel).Width);

            r.top   :=Aspecti(r.top)+randy;
            r.left  :=Aspecti(r.left)+randx;
            r.bottom:=Aspecti(r.bottom)+randy;
            r.right :=Aspecti(r.right)+randx;

            //ùber Rand gehen:
            if (P.Controls[i] as tPanel).Left<=1 then begin
             r.left:=0;
            end;
            if (P.Controls[i] as tPanel).Top<=1 then begin
             r.top:=0;
            end;
            if ((P.Controls[i] as tPanel).Left+(P.Controls[i] as tPanel).Width)
               >=PP.Width-2 then begin
//08.05.12             inc(r.Right,Randx);
             r.right:=BIT.Width; //11.06.12 PDFw;
            end;
            if ((P.Controls[i] as tPanel).Top+(P.Controls[i] as tPanel).Height)
               >=PP.Height-2 then begin
//08.05.12             inc(r.Bottom,Randy);//28.04.12 *2);
             r.bottom:=BIT.HEIGHT; //11.06.12 PDFh;
            end;

            lPDF.VCLCanvas.Brush.style:= bssolid;
            lPDF.VCLCanvas.Brush.Color:=(P.Controls[i] as tPanel).Color;
            lPDF.VCLCanvas.FillRect(r);

            BIT.Canvas.Brush.style:= bssolid;
            BIT.Canvas.Brush.Color:=(P.Controls[i] as tPanel).Color;
            BIT.Canvas.FillRect(r);

            Belege(P.Controls[i] as tPanel);
           except
            on EXC_:Exception do
             ExceptionHintUser0('05191',EXC_.Message,EXC_.ClassName);
           end;
          end
          else

          if P.Controls[i] is tImage then begin //Bilder
           try
            if Copy(S,1,3)='GVS' then begin
             _DoWappen(P.Controls[i] as tImage);
            end
            else
            if Copy(S,1,6)='WAPPEN' then begin
             _DoWappen(P.Controls[i] as tImage);
            end
            else
            if Copy(S,1,3)='QRV' then begin
             _DoVCard(P.Controls[i] as tImage);
            end
            else _NativIMG(P.Controls[i] as tImage); //FL 13.02.19 andere Grafiken wurden NIE unterstùtzt.
           except
            on EXC_:Exception do
             ExceptionHintUser0('05192',EXC_.Message,EXC_.ClassName);
           end;
          end
          else

          if P.Controls[i] is tLabel then begin //Texte
           L:=(P.Controls[i] as tLabel);
           Addx:=0;
           Addy:=0;

           //Nur eine Iterationstiefe tief Parentpanel suchen:
           if (Rueck and ( (L.Parent as tPanel).name <> AktPanelR.name) ) OR
              (not(Rueck) and ( (L.Parent as tPanel).name <> AktPanel.name) )
              then begin
            Addx:=(L.Parent as tPanel).Left;
            Addy:=(L.Parent as tPanel).Top;
           end;

//           lPDF.VCLCanvas.Font.Assign(L.Font);
//           lPDF.VCLCanvas.Font.Size:=Aspecti(L.Font.Size);//Todo? hmm....

           lPDF.VCLCanvas.Font.Name :=L.Font.Name;
           lPDF.VCLCanvas.Font.Size :=Aspecti(L.Font.Size);
           lPDF.VCLCanvas.Font.Style:=L.Font.Style;
           lPDF.VCLCanvas.Font.Color:=L.Font.Color;//28.04.12

           BIT.Canvas.Font.Name :=L.Font.Name;
           BIT.Canvas.Font.Size :=Aspecti(L.Font.Size);
           BIT.Canvas.Font.Style:=L.Font.Style;
           BIT.Canvas.Font.Color:=L.Font.Color;//28.04.12

           //28.04.12:
           if L.WordWrap then begin
            R.Left  :=Aspecti(Addx)+Aspecti(L.Left)+Randx;
            R.Top   :=Aspecti(Addy)+Aspecti(L.Top)+Randy;
            R.Right :=R.Left+Aspecti(L.Width) +10;
            R.Bottom:=R.Top+Aspecti(L.Height) +10;

            pr.left  :=r.left;
            pr.top   :=r.top;
            pr.right :=r.right;
            pr.bottom:=r.bottom;
            r2:=r;

            //08.05.12:
            if (Pos(#13,L.Caption)>0) or (Pos(#10,L.Caption)>0) or (Pos(#9,L.Caption)>0) then
             Dek.error:=1;

//            lPDF.VCLCanvas.MultilineTextRect(pr,L.Caption,true);
            DrawText(lPDF.VCLCanvas.Handle, PChar(L.Caption), Length(L.Caption)+1, r,
                     DT_WORDBREAK or DT_LEFT);//DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);}
            DrawText(BIT.Canvas.Handle, PChar(L.Caption), Length(L.Caption)+1, r2,
                     DT_WORDBREAK or DT_LEFT);//DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);

{//            lPDF.VCLCanvas.Textrect(R,R.Left,R.Top,L.Caption);
            BIT.Canvas.Textrect(R,R.Left,R.Top,L.Caption);}


           end
           else begin

            lPDF.VCLCanvas.Textout(Aspecti(Addx)+Aspecti(L.Left)+Randx,
                                   Aspecti(Addy)+Aspecti(L.Top)+Randy,
                                   L.Caption);

            BIT.Canvas.Textout(Aspecti(Addx)+Aspecti(L.Left)+Randx,
                                   Aspecti(Addy)+Aspecti(L.Top)+Randy,
                                   L.Caption);
           end;

          end;

          inc(i);
         end;
        except
            on EXC_:Exception do
             ExceptionHintUser0('05193',EXC_.Message,EXC_.ClassName);
        end;
       end;

    begin //DoPDF
     try

      if Rueck then
       PP:=AktPanelR
      else
       PP:=AktPanel;

      r.top   :=0;
      r.Left  :=0;
//      r.Right :=lPage.PageWidth;
//      r.bottom:=lPage.PageHeight;
      r.Right :=PDFw;//lPdf.DefaultPageWidth;
      r.Bottom:=PDFh;//lPdf.DefaultPageHeight;
      lPDF.VCLCanvas.Brush.style:= bssolid;
      lPDF.VCLCanvas.Brush.Color:=PP.Color;
      lPDF.VCLCanvas.FillRect(r);

      BIT.Canvas.Brush.style:= bssolid;
      BIT.Canvas.Brush.Color:=PP.Color;
      BIT.Canvas.FillRect(r);

      Belege(PP);
     except
      on EXC_:Exception do
       ExceptionHintUser0('05194',EXC_.Message,EXC_.ClassName);
     end;
    end;

   Var F:TextFile;
   kette:shortstring;
   LS,LS2:AnsiString;
   begin //Neu
    try
     rueck:=FALSE;
     if not(EDIT) and (AktLayout<>99){30.04.12} then//25.04.12
      SpeedButton13Click(Application);

     Menu.Visible      :=FALSE;
//     EditPanel.Visible :=FALSE;
     ScrollBox1.Visible:=FALSE;
     Menu.Visible      :=FALSE;

     if Pos('DOPPEL',AnsiUpperCase(Menge.Text))>0 then
      ClientWidth       :=Label2.Width+Layout3.Width*2+60 +panel8.width
     else
      ClientWidth       :=Label2.Width+Layout3.Width+20 +panel8.width;

    if Assigned(AktPanelR) then // FL 13.02.19
      begin
        AktPanelR.Left    :=AktPanel.Left+AktPanel.Width+50;
        AktPanelR.Visible :=true;
      end;

     LS:='';

     //30.04.12:
     if (AktLayout=99) then begin
      SpeedButton7Click(Application);
     end
     else begin

       ioresult;
       AssignFile(F,DTS+'VCARD VISITENKARTE.'+stdext);
       Reset(F);
       if IOResult=0 then begin
        while not(eof(f)) do begin
         readln(f,kette);
         LS2:=kette;
         LS :=LS+LS2+#13;
        end;
        CloseFile(F);
        ioresult;

        if AktLayoutR=202 then
         LS:=#13'vCard:'#13#13+LS
        else
         LS:=#13'Keine Visitenkarte mit vCard!';
       end
       else
       if AktLayoutR=202 then
        LS:=#13'Visitenkarte mit vCard gewùhlt, aber vCard fehlt!'
       else
        LS:=#13'Keine vCard!';
       if User_Status<>0 then//13.08.17
        Label2.Caption:=LS;
     end;
     
     hoch:=(AktPanel.Width<AktPanel.Height);
     CreatePDF;
     BIT:=tBitmap.Create;
     BIT.Pixelformat:=pf24Bit;
     if hoch then begin
      BIT.Width :=pdfh;
      BIT.Height:=pdfw;
     end
     else begin
      BIT.Width :=pdfw;//lPDF.DefaultPageWidth;//pdfw;//lPDF.DefaultPageWidth;
      BIT.Height:=pdfh;//lPDF.DefaultPageHeight;//pdfh;//lPDF.DefaultPageHeight;
     end; 
     DoPDF;
     Save;
     BIT.FREE;
     FreePDF;
     if Pos('DOPPEL',AnsiUpperCase(Menge.Text))>0 then begin
      Rueck:=true;
      hoch:=(AktPanelR.Width<AktPanelR.Height);
      CreatePDF;
      BIT:=tBitmap.Create;
      BIT.Pixelformat:=pf24Bit;
      if hoch then begin
       BIT.Width :=pdfh;
       BIT.Height:=pdfw;
      end
      else begin
       BIT.Width :=pdfw;//lPDF.DefaultPageWidth;;//pdfw;//lPDF.DefaultPageWidth;
       BIT.Height:=pdfh;//lPDF.DefaultPageHeight;//pdfh;//lPDF.DefaultPageHeight;
      end; 
      DoPDF;
      Save;
      BIT.FREE;
      FreePDF;
     end;
     BitBtn3.Enabled:=(_Anwender>'');
    except
     on EXC_:Exception do
      ExceptionHintUser0('05195',EXC_.Message,EXC_.ClassName);
    end;
   end;

 begin
  NEU;
  //ToDo noch unfertig! Nicht zu gebrauchen, weil verdammt noch einmal Label.Parent Panel nicht bekannt!
(*  rueck:=false;
  m:='';
  w:=-1;
  onlineID:='';
  d:='';
  z:='';
  Layout:=-1;
  LayoutR:=-1;
  hoch:=FALSE;
  try
   ioresult;
   AssignFile(F,DTS+'VISITENKARTE.'+StdExt);//FileDialog oder so...
   Reset(F);
   if IOResult=0 then begin
    try
     repeat
      Get(id,inh);
      if ID='MENGE' then begin
       m:=inh;
      end
      else
      if ID='DATUM' then
       D:=inh
      else
      if ID='UHRZEIT' then
       Z:=inh
      else
      if ID='WAPPEN' then begin
       w:=_Val(Inh);
//       _DoWappen;
      end
      else
      if ID='LAYOUTR' then
       LayoutR:=_Val(inh)
      else
      if ID='LAYOUT' then begin
       Layout:=_Val(inh);
       if Layout=4 then
        Hoch:=TRUE;
      end
      else begin
       if Pos('<BLCOM ',inh)>0 then begin
        OnlineID:=Copy(inh,Pos('<BLCOM ',inh)+7,255);
        if Pos('>',OnlineID)>0 then
         Delete(OnlineID,Pos('>',OnlineID),255);
       end;
      end;
     until IDNr<>-1;
     CreatePDF;
     _DoWappen;
     if IDNr<>-1 then
      Belege;

     while not(Eof(F)) do begin
      Get(id,inh);
      if idnr<>-1 then
       Belege;

      //jetzt Rùckseite:
      if not(IDNr=LayoutR) and not(Rueck) then begin
       Rueck:=TRUE;
       Save;
       CreatePDF;
      end;
     end;

    except
     on EXC_:Exception do
      ExceptionHintUser0('05196',EXC_.Message,EXC_.ClassName);
    end;
    CloseFile(F);
    ioresult;

    Save;

   end;
  except
  end;*)
 end;

begin //CreateVorlage
 try
  C(Vorlage);
  C(VorlageR);
  Lies;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05197',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.SpeedButton3Click(Sender: TObject);
begin
 if (_Anwender>'') or//24.04.12
    (TDialogs.MyMessageDlgOK('Wirklich beenden?',mtconfirmation,[mbyes,mbno],0)=mryes) then//27.03.12
  Close;
end;

procedure TBLVKarten.SpeedButton4Click(Sender: TObject);
var
  VKarte            : TStringList;
  InterfaceVariable : IGVBaseUpdateNew;
  Error             : AnsiString;
  //Daten             : TByteDynArray;
  lWriter           : IStreamWriter;
  i                 : integer;
  Eintraege         : Int32; // FL 31.08.21
  kette             : AnsiString;
  f                 : Textfile;
  SpoolHeader : tSpoolHeader;

 procedure Err(s:shortstring);
 var buff:nr_typ;

  procedure Do1F;
  Var F,FF:TextFile;
  begin
   ioresult;
   assignfile(ff,temppath+'VISITENKARTE.TXT');
   rewrite(ff);
   if ioresult=0 then begin

    AssignFile(F,DTS+'VISITENKARTE.'+StdExt);
    Reset(F);
    if IOResult=0 then begin
     while not(eof(f)) do begin
      readln(f,kette);
      writeln(ff,kette);
     end;
     ioresult;
     CloseFile(F);
     ioresult;
    end;
    AssignFile(F,DTS+'VCARD VISITENKARTE.'+stdext);
    Reset(F);
    if IOResult=0 then begin
     while not(eof(f)) do begin
      readln(f,kette);
      writeln(ff,'vCard='+kette);
     end;
     CloseFile(F);
     ioresult;
    end;
    ioresult;
    closefile(FF);
    ioresult;
   end;
  end;
  
 begin
  TDialogs.MyMessageDlgOK('['+s+']: Es ist ein Fehler bei der Bestellung aufgetreten.'#13+
               'Vielleicht haben Sie kein Internet oder Ihr Virenscanner oder Ihre Firewall blockieren den Transfer per Internet?'#13#13+
               'Das Design wurde bereits gespeichert - nichts ist verloren!!'#13#13+
               'Es wird nach Klick auf OK versucht, die Bestellung per E-Mail aufzugeben. Sollte dies funktionieren, ist alles in Ordnung und Sie sollten binnen 2 Arbeitstagen eine Bestùtigung erhalten.'+
               'Funktioniert die auch E-Mail nicht oder Sie erhalten keine Bestùtigung, kontaktieren Sie bitte den Service.'
               ,mtinformation,[mbok],0);
  Do1F;
  buff.dr :=0;
  buff.jahr:=0;
  InitSpoolHeader(SpoolHeader, eIgnore, 'Visitenkartenbestellung', 0, 0 );//kein DR etc.
  Wemaile(temppath+'VISITENKARTE.TXT',Buff,False,
          LizenzeMailD2,'Visitenkartenbestellung',
          temppath+'VISITENKARTE.TXT',
          GetStandardHeader,
          FALSE,TRUE,false,
          SpoolHeader);                                                                        { TODO 2 -oXEFrank -cS_2_Short : XE }
  Close;//jaja, da fehlen ein paar Free...
 end;

begin
 try
  if Pos('doppelseitig',Menge.Text)>0 then begin
   kette:='doppelseitig bedruckte Visitenkarten (Vorderseite '+AktLayoutName+', Rùckseite '+AktLayoutNameR+')';
   if not(RueckGewaehlt) then begin
    TDialogs.MyMessageDlgOK('Sie haben zur Bestellung eine doppelseitige Visitenkarte, aber noch kein Design der Rùckseite gewùhlt.',mtwarning,[mbok],0);
    Exit;
   end;
  end
  else
   kette:='einseitig bedruckte Visitenkarten ('+AktLayoutName+')';
  kette:=kette+'Sie bestellen hiermit '+kette+': "'+Menge.Text+'"';                                 { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  if TDialogs.MyMessageDlgOK('~Verbindliche Bestellung einer Visitenkarte!'#13#13+
                kette+
                '"! Bitte achten Sie darauf, ob Sie einseitig oder doppelseitig bedruckte Visitenkarten nutzen.'#13#13+
//               'Hiermit bestellen Sie die angezeigte Visitenkarten (Menge und Preis: '+Menge.Text+')'#13#13+
                'Bitte kontrollieren Sie genau die gemachten Angaben. Fùr Schreibfehler wird keine Haftung ùbernommen. Es wird auch keine weitere Kontrolle vorgenommen.'#13#13+
                'Die Visitenkarten werden nach Ihren Vorgaben erstellt.'#13#13+
                'Farbe, Schriftart und Schriftgrùùe weichen im Druck ggf. etwas ab. '+
                'Sollten Sie eine Visitenkarte mit vCard nehmen, gelten die im Programm bei der vCard angegebenen Bedingungen.'#13#13+
                'Individuelle oder durch den Hersteller aufwendig geùnderte Designs der Visitenkarten werden zu einem anderen Preis angeboten.'#13#13+
                'Die Bestellung ist verbindlich und da fùr Sie der Druck in Auftrag gegeben wird, ist dieser nicht stornierbar.'#13#13+
                'Die Lieferzeit betrùgt zwischen 1-2 Wochen. Bei einer Verzùgerung erhalten Sie von uns eine Nachricht.',
                mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin

      SpeedButton12Click(Sender);//Speichern!

      VKarte := TStringList.Create;

      VKarte.Add('Datum='+GetMakeStr);
      VKarte.Add('Uhrzeit='+GetMakeStrZeit);
      try
       ioresult;
       AssignFile(F,DTS+'VISITENKARTE.'+StdExt);
       Reset(F);
       if IOResult=0 then begin
        while not(eof(f)) do begin
         readln(f,kette);
         VKarte.Add(kette);
        end;
        ioresult;
        CloseFile(F);
        ioresult;
       end;
       AssignFile(F,DTS+'VCARD VISITENKARTE.'+stdext);
       Reset(F);
       if IOResult=0 then begin
        while not(eof(f)) do begin
         readln(f,kette);
         VKarte.Add('vCard='+kette);
        end;
        CloseFile(F);
        ioresult;
       end;

      except
       on EXC_:Exception do
        ExceptionHintUser0('05198',EXC_.Message,EXC_.ClassName);
      end;

      try
        lWriter := TStreamWriter.Create;

        try
          lWriter.AddHeader(cVKKartenStreamVersion);
          lWriter.Schreibe(AktLayout);
          Eintraege := VKarte.Count-1;     // FL02.04.2012
          lWriter.Schreibe(Eintraege);   // FL02.04.2012

          for i:=0 to VKarte.Count-1 do
            begin
              kette := VKarte.Strings[i];                                                           { TODO 2 -oXEFrank -cS_2_Ansi : XE }
              lWriter.Schreibe(kette);
            end;

          try
            InterfaceVariable := TGVBaseUpdate.Create(FLThread.Server);
            try
              try
                FallBack_GetKtoAcc;//30.06.17
                if FLThread.KtoAcc=''
                  then Error:='YO'
                  else Error := InterfaceVariable.OrderVKarte(FLThread.KtoAcc,TRIM((BezRec[AktBezRec].UserText[1]))+','+KNR,lWriter.AsPackedDataStream);  { TODO 2 -oXEFrank -cS_2_Ansi : XE }

                if Error <> cOKStr
                  then Err('1')
                  else begin
                         TDialogs.MyMessageDlgOK('~Vielen Dank fùr Ihre Bestellung!'#13#13+
                                        'Sie erhalten eine Bestùtigung Ihrer Bestellung binnen 2 Arbeitstagen.'#13#13+
                                        'Falls nicht, kontaktieren Sie bitte den Service!',mtInformation,[mbok],0);
                         Close;
                       end;
              except
                Err('2');
              end;
            finally
              InterfaceVariable := NIL;
            end;
          except
            Err('3');
          end;

        except
               on EXC_:Exception do
                ExceptionHintUser0('05199',EXC_.Message,EXC_.ClassName);
        end;
      finally
        VKarte.Free;
      end;
    end;
 except
               on EXC_:Exception do
                ExceptionHintUser0('05201',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.SpeedButton5Click(Sender: TObject);
var
  str, ort,
  S : AnsiString;
//06.08.13   gefunden : byte;

 Procedure Belege(P:tPanel);
 var i:integer;
     s:shortstring;
 begin
  i:=0;
  try
   while (p<>nil) and (i < P.ControlCount) do begin
    s:=AnsiUppercase(P.Controls[i].Name);                                                           { TODO 2 -oXEFrank -cS_2_Short : XE }

    if P.Controls[i] is tPanel then begin //Recursiv
     Belege(P.Controls[i] as tPanel);
    end
    else

    if P.Controls[i] is tImage then begin //Bilder
     try
      if Copy(S,1,6)='WAPPEN' then
       SetWappen
      else
      if Copy(S,1,3)='QRV' then
      else
      if Copy(S,1,2)='QR' then
      else
       ;
     except
      on EXC_:Exception do
       ExceptionHintUser0('05202',EXC_.Message,EXC_.ClassName);
     end;
    end
    else

    if P.Controls[i] is tLabel then begin //Texte
     //10.09.12:
     if Copy(S,1,4)='OGVNAME' then
      (P.Controls[i] as tLabel).Caption:=trim((BezRec[AktBezRec].UserText[2]))+' '+trim((BezRec[AktBezRec].UserText[1]))
     else

     if Copy(S,1,4)='NAME' then
      (P.Controls[i] as tLabel).Caption:=trim((BezRec[AktBezRec].UserText[1]))
     else
     if (Copy(S,1,3)='OGV') or (Copy(S,1,3)='HGV') then
      (P.Controls[i] as tLabel).Caption:=trim((BezRec[AktBezRec].UserText[2]))
     else
     if Copy(S,1,3)='STR' then
      (P.Controls[i] as tLabel).Caption:=str
     else
     if Copy(S,1,3)='ORT' then
      (P.Controls[i] as tLabel).Caption:=ort
     else
     if Copy(S,1,2)='AG' then
      (P.Controls[i] as tLabel).Caption:='beim Amtsgericht '+trim(BezRec[AktBezRec].Amtsgericht)
     else

     //04.04.12:
     if Copy(S,1,4)='IBAN' then
      (P.Controls[i] as tLabel).Caption:=trim(GVIBAN)
     else
     if Copy(S,1,3)='BIC' then
      (P.Controls[i] as tLabel).Caption:=trim(GVBIC)
     else

     if Copy(S,1,6)='BUERO1' then
      (P.Controls[i] as tLabel).Caption:='Bùrozeiten: '+trim((BezRec[AktBezRec].UserText[7]))
     else
     if Copy(S,1,7)='SPRECH1' then
      (P.Controls[i] as tLabel).Caption:=trim((BezRec[AktBezRec].UserText[7]))
     else
     if Copy(S,1,7)='SPRECH2' then
      (P.Controls[i] as tLabel).Caption:=trim((BezRec[AktBezRec].UserText[8]))
     else
     if Copy(S,1,7)='SPRECH3' then
      (P.Controls[i] as tLabel).Caption:='weitere...'
     else
     if Copy(S,1,7)='SPRECH4' then
      (P.Controls[i] as tLabel).Caption:='weitere...'
     else
     if Copy(S,1,4)='TEL1' then
      (P.Controls[i] as tLabel).Caption:=trim((BezRec[AktBezRec].UserText[6]))
     else
     if (Copy(S,1,4)='TEL2') or
        (Copy(S,1,4)='TEL3') or
        (Copy(S,1,4)='TEL4') then
      (P.Controls[i] as tLabel).Caption:='weitere...'
     else
     if Copy(S,1,3)='FAX' then
      (P.Controls[i] as tLabel).Caption:=trim(BezRec[AktBezRec].FaxNr)
     else
     if Copy(S,1,5)='EMAIL' then
      (P.Controls[i] as tLabel).Caption:=trim(eMailAdr)
     else
      ;
    end;
    inc(i);
   end;
  except
      on EXC_:Exception do
       ExceptionHintUser0('05203',EXC_.Message,EXC_.ClassName);
  end;
 end;

 var //06.08.13  i:integer;
     P:tPanel;
begin
  S   := TRIM((BezRec[AktBezRec].UserText[3]));                                                     { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  Str := trim(copy(S,Succ(pos('*',S)),100));                                                        { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  Ort := trim(copy(S,1,pred(pos('*',S))));                                                          { TODO 2 -oXEFrank -cS_2_Ansi : XE }

//06.08.13   i:=0;
  try
   if SpeedButton11.Down then
    P:=AktPanelR
   else
    P:=AktPanel;
   Belege(P);
   WFehler('Es werden nur grob Vorgaben erstellt und eine Nachbearbeitung ist nùtig! Texte sind ggf. zu lang etc.',2);
  except
   on EXC_:Exception do
    ExceptionHintUser0('05204',EXC_.Message,EXC_.ClassName);
  end;
end;

procedure TBLVKarten.SpeedButton7Click(Sender: TObject);
begin
 try
  DeleteFile(TempPath+'QRV.BMP');
  WinStamm6(100);
  if FileExists(TempPath+'QRV.BMP') then begin
   QRV2.Picture.Bitmap.LoadFromFile(TempPath+'QRV.BMP');
   WFehler('vCard gemùù Vorgaben geladen!',2);
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05205',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.SpeedButton8Click(Sender: TObject);
begin
  TDialogs.MyMessageDlgOK('~Visitenkarten fùr GV'#13#13+
               'Der Programmhersteller bietet gedruckte Visitenkarten an. Das hier vorgestellte Programm dient zur einfachen Auswahl und Erstellung von verschiedenen Designs.'#13#13+
               'In der vorliegenden ersten Version gibt es nur eine beschrùnkte Auswahl. Je nach Interesse der Anwender wird diese Auswahl erweitert.'#13#13+
               'Ansonsten erstellen wir Visitenkarten auch individuell und mit verschiedensten Optionen. Der Preis wird dann je nach Aufwand festgesetzt.'#13#13+

               'Eine grobe Vorgabe mit Ihren Daten erhalten Sie durch Klick auf "Vorgabe aus Stempel". '+
               'Danach kùnnen Sie die einzelnen Texte anklicken und abùndern. '+
               'Auch kùnnen Sie die Grundfarben der Visitenkarte und der Teilbereiche durch einen Klick abùndern. ',
               mtInformation,[mbok],0);
end;

procedure TBLVKarten.SpeedButton9Click(Sender: TObject);
begin
 //ToDo!
end;

procedure TBLVKarten.StartVTimer(Sender: TObject);
begin
 StartV.Enabled:=FALSE;
 BitBtn2Click(Sender);
end;

procedure TBLVKarten.txtchange(Sender: TObject);
Var s:tfontstyles;
    p:tcontrol;
begin
 if OK and (EditL<>NIL) then
  try
   Posi.Font.Color:=clwindowtext;
   Posi.Color     :=clbtnface;
   Posi.Caption   :='Position '+inttostr(EditL.Left)+', '+inttostr(EditL.Top);//08.05.12
//26.06.12   Posi.Caption   :='Position '+inttostr(txt.Left)+', '+inttostr(txt.Top);//08.05.12
   if txt.Text<>EditL.Caption then begin
    EditL.Caption:=Txt.Text;
    changed:=TRUE;
   end;

   if AnsiUppercase(Fontname.Text)<>AnsiUppercase(EditL.Font.Name) then begin
    EditL.Font.Name:=Fontname.Text;
    changed:=TRUE;
   end;

   if updown1.position<>EditL.Font.Size then begin
    EditL.Font.Size:=updown1.position;
    changed:=TRUE;
   end;

   s:=[];
   if boldbutton.down then
    s:=s+[fsbold];
   if italicbutton.down then
    s:=s+[fsitalic];
   if underlinebutton.down then
    s:=s+[fsunderline];
   if s<>EditL.Font.Style then begin
    EditL.Font.Style:=s;
    changed:=TRUE;
   end;

   //28.04.12 Clipping testen!:
   P:=EditL.Parent;
   if P is tPanel then
    if (EditL.Left+
        EditL.Canvas.TextWidth(EditL.Caption))>
       (P.Width) then begin
     WFehler('Text ragt ùber den Rand hinaus!',2);
     Posi.Font.Color:=clwhite;
     Posi.Color:=clred;
     Posi.Caption:=Posi.Caption+' - Text ùber Rand!';
    end
    else
    if (EditL.Left+
        EditL.Canvas.TextWidth(EditL.Caption))>
       (P.Width-15) then begin
     WFehler('Text ragt zu nah an den Rand!',2);
     Posi.Font.Color:=clwhite;
     Posi.Color:=clred;
     Posi.Caption:=Posi.Caption+' - Text nah am Rand!';
    end;
{kùnnte sub-panel sein!    else
    if (EditL.Left<=10) then
     WFehler('Text ist zu nah am linken Rand!',2);}

  except
   on EXC_:Exception do
    ExceptionHintUser0('05206',EXC_.Message,EXC_.ClassName);
  end;
end;

procedure TBLVKarten.txtExit(Sender: TObject);
begin
 //03.04.12: sonst kann man Text nicht mehr anklicken!
 if trim(txt.text)='' then
  txt.Text:='    ';
end;

procedure TBLVKarten.txtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 Changed:=TRUE;
end;

procedure TBLVKarten.underlinebuttonClick(Sender: TObject);
begin
 txtChange(sender);
end;

procedure TBLVKarten.Waage2Click(Sender: TObject);
begin
 FLand := 17;
 SetWappen;
 if nichts.Checked then
  FLand:=17
 else
 if Waage.Checked then
  FLand:=99//17.09.14 16
 else
  FLand:=Land-1;
 SetWappen;//29.03.12 FLand);
end;

Procedure GetVKarte;
begin
 try
  BLVKarten := TBLVKarten.Create(Application);
  try
   With BLVKarten Do begin
    BMP := TBitmap.Create;
    OK:=FALSE;
    FDragging:=FALSE;
    RueckGewaehlt:=FALSE;
    FLand:=Land-1;
    AktLayout:=0;
    SRC:=stringreplace(SRC,'_',' ',[rfreplaceall]);                                                 { TODO 2 -oXEFrank -cS_2_Short : XE }
    _SRC:=SRC;
    AktLayoutR:=200;
    SetWappen;
    Menge.ItemIndex:=0;//!!!
    Changed:=FALSE;
    GetFontNames;
    ScrollBox1.VertScrollBar.Position:=0;
    SpeedButton13.Enabled:=FileExists(DTS+'VISITENKARTE.'+StdExt);//Laden Button

    //23.04.12:
    if (User_Status=0) and CompanyRights(FALSE) then begin
     BitBtn6.Visible:=TRUE;
     BitBtn5.Visible:=TRUE;
     BitBtn2.Visible:=TRUE;
     BitBtn3.Visible:=TRUE;
     BitBtn4.Visible:=TRUE;
     Image99.Visible:=CompanyRights(TRUE);//08.05.12 TRUE;//30.04.12

     //02.06.12:
     Label23.Visible:=TRUE;
     Redak.Visible:=TRUE;
     Redak2.Visible:=TRUE;
     Redak3.Visible:=TRUE;
     Redak4.Visible:=TRUE;

    end;

    _D       :='';
    _Z       :='';
    _Anwender:='';
    _KNR     :='';
    _OnlineID:='';
    FileNameV:='';
    FileNameR:='';
    fillchar(vC,sizeof(vC),0);

//13.08.17{08.05.12    //01.05.12:
    if (User_Status=0) and CompanyRights(FALSE) then
     Label2.Caption:=''
    else//}

    Label2.Caption:=
'Hinweise:'#13#13+
'Zum ùndern der Texte darauf klicken.'#13#13+
'Verschieben von Texten durch Klick mit gedrùckter Hochstelltaste.'#13#13+
'Hintergrundfarbe von Teilbereichen durch Klick hierauf ùndern.'#13#13+
'Eine grobe Vorgabe der Texte gemùù den Angaben in Ihren Stempeln erhalten Sie duch Klick auf die entsprechende Schaltflùche.'#13#13+
#13+
'Alle Preise inkl. der gesetzlichen Mehrwertsteuer. Irrtùmer und ùnderungen vorbehalten. '+
'Sollten sich die Preise geùndert haben, erhalten Sie nach Bestellung die Mùglichkeit zu stornieren.';
{12.08.17
    //08.05.12:
    if (User_Status=0) and CompanyRights(FALSE) then
     Label2.Visible:=FALSE;//Layout:=tlBottom;
}
    //Platz machen:
    if GetClientHeightAbs>BLVKarten.Height+100 then
     Height := GetClientHeightAbs-100;

    ShowHide(BLVKarten.IMage0.Tag);

    //08.05.12:
    if _SRC>'' then
     StartV.Enabled:=TRUE;

    ShowModal;
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05207',EXC_.Message,EXC_.ClassName);
  end;
  BLVKarten.Free;
 except
   on EXC_:Exception do
    ExceptionHintUser0('05208',EXC_.Message,EXC_.ClassName);
 end;

{13.03.14 //08.05.12:
 if SRC>'' then
  HALT;}
end;

procedure TBLVKarten.BitBtn1Click(Sender: TObject);
begin
 CreateVorlage(FALSE);
end;

procedure TBLVKarten.BitBtn2Click(Sender: TObject);//24.04.12
var f,f2:textfile;
    s2,s:shortstring;
    i:integer;
begin
 try
  GlobalEMail:='';
  DeleteFile(DTS+'VISITENKARTE.'+StdExt);
  DeleteFile(DTS+'VCARD VISITENKARTE.'+stdext);
  if (_Src>'') or OpenD.Execute then begin

   //08.05.12:
   if _SRC>'' then
    OpenD.Filename:=_SRC;

   ioresult;
   AssignFile(F,OpenD.FileName);
   Reset(F);
   if ioresult=0 then begin
    BitBtn2.Enabled:=FALSE;//nùchte Visitenkarte nur durch Neuladen des Forms!

    //Header:
    _Anwender:='';
    Repeat
     readln(f,s);
     s2:=ansiuppercase(s);                                                                          { TODO 2 -oXEFrank -cS_2_Short : XE }

     if Pos('DATUM=',s2)>0 then
      _D:=Copy(s,Pos('=',s)+1,200)
     else
     if Pos('UHRZEIT=',s2)>0 then
      _Z:=Copy(s,Pos('=',s)+1,200)
     else
     if Pos('<BLCOM ',s2)>0 then begin
      _BL:=s2;
      _OnlineID:=Copy(s2,Pos('<BLCOM ',s2)+7,255);
      if Pos('>',_OnlineID)>0 then
       Delete(_OnlineID,Pos('>',_OnlineID),255);
     end
     else
     if Pos('LAYOUT=',s2)>0 then
     else
     if _Anwender='' then//14.01.13 irgendwoher kommt jetzt eine Zeile ","
      _Anwender:=s;
    Until Eof(F) or (Pos('LAYOUT=',AnsiUpperCase(s))>0);
    Caption:='Bestellung: '+_D+', '+_Z+' von '+_Anwender+' (OnlineID='+_OnlineID+')';
    //Visitenkarte:
    if not(eof(f)) then begin
     ioresult;
     AssignFile(F2,DTS+'VISITENKARTE.'+StdExt);
     Rewrite(F2);
     writeln(f2,s);
     repeat
      readln(f,s);

      //28.04.12:
      if (Pos('EMAIL',Ansiuppercase(s))>0) and (Pos('=',AnsiUpperCase(s))>0) then begin
       GlobalEMail:=Copy(s,pos('=',s)+1,200);
       if pos(#9,GlobalEMail)>0 then
        GlobalEMail:=copy(GlobalEMail,1,pos(#9,GlobalEMail)-1);
      end;

      if Pos('VCARD=',AnsiUpperCase(s))=0 then
       writeln(f2,s);
     Until Eof(F) or (Pos('VCARD=',AnsiUpperCase(s))>0);
     CloseFile(F2);

     if not(eof(f)) then begin
      ioresult;
      AssignFile(F2,DTS+'VCARD VISITENKARTE.'+stdext);
      Rewrite(F2);
      s:=Copy(s,Pos('=',s)+1,255);
      writeln(f2,s);
      vC[1]:=s;
      i:=2;
      while not(eof(f)) and (i<20) do begin
       readln(f,s);
       inc(i);
       s:=Copy(s,Pos('=',s)+1,255);
       vC[i]:=s;
       writeln(f2,s);
      end;
      CloseFile(F2);
     end;

    end;
    ioresult;
    CloseFile(F);
    ioresult;

    BitBtn1Click(Sender);
    BitBtn6.Enabled:=true;//28.04.12
    BitBtn5.Enabled:=true;//25.04.12
    Caption:=Caption+'; '+Menge.Text;
   end
   else
    TDialogs.MyMessageDlgOK('Datei '+OpenD.FileName+' nicht zu ùffnen.',mterror,[mbabort],0);
  end
  else
   WFehler('Abbruch',2);
 except

 end;
end;

procedure TBLVKarten.BitBtn3Click(Sender: TObject);
begin
 BestaetigungAnKunden;
end;

procedure TBLVKarten.BitBtn5Click(Sender: TObject);
begin
 //30.04.12:
 if AktLayout=99 then begin
  FLand:=99;
  _Anwender:=Label67.Caption;                                                                       { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
  OpenD.Filename:='G:\Bestellungen Visitenkarten\TEST.PDF';
 end;

 CreateVorlage(TRUE);
end;

Procedure TBLVKarten.Clean(I:tImage);
 var r:trect;
 begin
  try
EXIT;
   R.Top    := 0;
   R.Left   := 0;
   R.Bottom := I.Picture.Bitmap.Height;
   R.Right  := I.Picture.Bitmap.Width;
   With I.Picture.Bitmap.Canvas do begin
    Pen.Color   := clwhite;
    Brush.Color := clblack;
    Brush.style := bssolid;
    FillRect(R);
    Application.ProcessMessages;
    Brush.Color := clwhite;
    Brush.style := bssolid;
    FillRect(R);
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('05209',EXC_.Message,EXC_.ClassName);
  end;
 end;

procedure TBLVKarten.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
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
    ExceptionHintUser0('05210',EXC_.Message,EXC_.ClassName);
  end;
  Handled := true;
 end;

end;

Procedure TBLVKarten.SetWappen;

  Procedure MakeR(Var R : TRect;X,Y:Integer);
  begin
   R.Top := y;
   R.Left := X;
   R.bottom := y+32;
   R.right := X+25;
  end;

 Procedure Belege(P:tPanel);
 var //06.08.13 j,
     i:integer;
     s:shortstring;
 begin
  i:=0;
  try
   while (p<>nil) and (i < P.ControlCount) do begin
    s:=AnsiUppercase(P.Controls[i].Name);                                                           { TODO 2 -oXEFrank -cS_2_Short : XE }

    if P.Controls[i] is tPanel then begin //Recursiv
     Belege(P.Controls[i] as tPanel);
    end
    else

    if P.Controls[i] is tImage then begin //Bilder
     try
      if Copy(S,1,6)='WAPPEN' then begin
       Clean(P.Controls[i] as tImage);
       (P.Controls[i] as tImage).Picture.Bitmap.Assign(Bmp);
       (P.Controls[i] as tImage).Transparent:=TRUE;

{       //Trick:
       j:=(P.Controls[i] as tImage).top;
       (P.Controls[i] as tImage).top:=1000;
       Application.ProcessMessages;
       (P.Controls[i] as tImage).top:=j;}

      end;
     except
      on EXC_:Exception do
       ExceptionHintUser0('05211',EXC_.Message,EXC_.ClassName);
     end;
    end;
    inc(i);
   end;
  except
      on EXC_:Exception do
       ExceptionHintUser0('05212',EXC_.Message,EXC_.ClassName);
  end;
 end;

begin //SetWappen
 try

  //17.09.14:
  if FLand=99 then
   ImageList1.GetBitmap(16,BMP)
  else 

   ImageList1.GetBitmap(FLand,BMP);
  if FLand=17 then
    BMP.Transparent := false;
  Belege(AktPanel);
  if AktPanelR<>NIL then
   Belege(AktPanelR);
 except
  on EXC_:Exception do
   ExceptionHintUser0('05213',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 try
  Application.OnHint := Leiste.ShowHint;
  if bmp<>nil then
   BMP.free;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05214',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.FormCreate(Sender: TObject);
begin
  AktLayout := 1;
  {
  ObjectInspector := TObjectInspector.Create(Application);
  ObjectInspector.Left := Self.Left+Self.Width+5;}
end;

procedure TBLVKarten.FormDestroy(Sender: TObject);
begin
//  ObjectInSpector.free;
end;

procedure TBLVKarten.FormShow(Sender: TObject);
begin
 left:=getleft_modal(width);
 top :=gettop_modal(height);
end;

procedure TBLVKarten.Image11Click(Sender: TObject);
var i:integer;
begin
 i:=_ID;
 Image3Click(Sender);
 if (i<>_ID) and (_ID=202) then begin
  TDialogs.MyMessageDlgOK('Sie haben eine Rùckseite mit einer QR-Code vCard gewùhlt.'#13#13+
               'Im nùchsten Fenster kùnnen Sie die Daten fùr den QR-Code erfassen.'#13#13+
               'Wahrscheinlich wissen Sie, was ein QR-Code ist und wozu dieser dient.'#13+
               'Besitzer von Smartphones mit z.B. Android, Windows Phone oder Apple-Betriebssystem kennen QR-Codes normalerweise schon.'#13#13+
               'Falls nicht, erhalten Sie im nùchsten Fenster auch eine Erklùrung.',
               mtinformation,[mbok],0);
  SpeedButton7Click(Sender);
 end;
end;

procedure TBLVKarten.Image3Click(Sender: TObject);
begin
 //30.04.12:
 if Sender is tPanel then begin
  if TPanel(Sender).Tag = 99 then begin
//   Layout:=99;//28.01.15
   AktPanel:=Layout99;
   AktPanelR:=Rueckseite2;
   AktLayoutR:=202;//QR
   Menge.ItemIndex:=1;
   _Anwender:=Label67.Caption;                                                                      { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
  end;
  if TPanel(Sender).Tag = _ID then begin
   WFehler('Visitenkarte ist schon gewùhlt.',2);
   Exit;
  end;
 end
 else

 //schon gewùhlt:
 if TImage(Sender).Tag = _ID then begin
  WFehler('Visitenkarte ist schon gewùhlt.',2);
  Exit;
 end;

 Changed:=FALSE;
 EditPanel.Enabled:=FALSE;

 //30.04.12:
 if Sender is tPanel then
  ShowHide(TPanel(Sender).Tag)
 else

  ShowHide(TImage(Sender).Tag);
end;

procedure tblvkarten.NeuLabel;
begin
 try
  L:=tLabel.Create(P);
  L.Parent     :=P;
  L.Font.Name  :=FName;
  L.Font.Size  :=FSize;
  L.Font.Style :=FStyle;
  L.Font.Color :=c;
  L.Color      :=clwhite;
  L.Caption    :=LCaption;
  L.Left       :=Le;
  L.Top        :=T;
  L.WordWrap   :=FALSE;
  L.Transparent:=TRUE;
  L.AutoSize   :=TRUE;
  L.ParentColor:=TRUE;
  L.Name       :=LName;
  L.ShowAccelChar:=FALSE;
  L.OnClick      :=Edit;
  L.OnMouseDown  :=MyMouseDown;
  L.OnMouseMove  :=MyMouseMove;
  L.OnMouseUp    :=MyMouseUp;
  L.Cursor  :=crHandPoint;
  L.Align   :=alnone;
  L.ShowHint:=FALSE;
  L.Visible :=TRUE;
  L.Enabled :=TRUE;
  Changed   :=TRUE;
  WFehler('Neues Textelement hinzugefùgt. Dies ist nur auf der Hauptebene mùglich!',2);
 except
  on EXC_:Exception do
   ExceptionHintUser0('05215',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.NeuClick(Sender: TObject);
Var P:tPanel;
    L:tLabel;
begin
 //Problem: mùsste vorher panel gewùhlt werden! Panename mùsste dann in Labelname!
 try
  if SpeedButton11.Down then
   P:=AktPanelR
  else
   P:=AktPanel;
  NeuLabel(P,L,
           'NEU_'+P.Name+'_'+
           //Panelname+'_'
           chr(random(20)+65)+chr(random(20)+65)+
           chr(random(20)+65)+chr(random(20)+65)+
           chr(random(20)+65)+chr(random(20)+65),                                                   { TODO 2 -oXEFrank -cS_2_Short : XE }
           'Neu...',50,100,clblack,10,'Tahoma',[]);

  {
  Idee:
  Klick verlangen und alle Klicks von allen Panel umleiten!!!
  }
 except
  on EXC_:Exception do
   ExceptionHintUser0('05216',EXC_.Message,EXC_.ClassName);
 end;
end;

type TMoveCracker = class(TControl);

procedure TBLVKarten.MyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if (Button=mbLeft) and (ssShift in Shift) then
  if (Sender is tLabel) or (Sender is tImage) then begin

   FDownX := X;
   FDownY := Y;
   FDragging := True;
   TMoveCracker(Sender).MouseCapture := True;
   OrigL:=(Sender as tLabel).Left;
   OrigT:=(Sender as tLabel).Top;
   if Sender is tLabel then begin
    (Sender as tLabel).Hint:='Originalposition '+inttostr(OrigL)+':'+inttostr(OrigT);
    (Sender as tLabel).ShowHint:=TRUE;
   end;
{

   EditL:=(Sender as tLabel);
//Caption:='START '+inttostr(x)+':'+inttostr(y)+'; '+inttostr(editl.left)+':'+inttostr(editl.top);
   OrigL:=EditL.Left;
   OrigT:=EditL.Top;
   MoveStartL:=X;
   MoveStartT:=Y;}
  end;
end;

procedure TBLVKarten.MyMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const SC_DragMove = $F012; //magische Zahl !!!
begin
 try
  if FDragging then
   if Sender is tImage then begin
    with Sender as TImage do begin
     Left := X - FDownX + Left;
     Top  := Y - FDownY + Top;
     (Sender as tImage).Hint:='Position '+inttostr(left)+':'+inttostr(top)+
                              '; Originalposition '+inttostr(OrigL)+':'+inttostr(OrigT);
     (Sender as tImage).ShowHint:=TRUE;

     //26.06.12:
   Posi.Font.Color:=clwindowtext;
   Posi.Color     :=clbtnface;
   Posi.Caption   :='Position '+inttostr((Sender as TImage).Left)+', '+inttostr((Sender as TImage).Top);//08.05.12

    end;
   end
   else
    with Sender as TLabel do begin
     Left := X - FDownX + Left;
     Top  := Y - FDownY + Top;
     (Sender as tLabel).Hint:='Position '+inttostr(left)+':'+inttostr(top)+
                              '; Originalposition '+inttostr(OrigL)+':'+inttostr(OrigT);
     (Sender as tLabel).ShowHint:=TRUE;

     //26.06.12:
   Posi.Font.Color:=clwindowtext;
   Posi.Color     :=clbtnface;
   Posi.Caption   :='Position '+inttostr((Sender as TLabel).Left)+', '+inttostr((Sender as TLabel).Top);//08.05.12

    end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05217',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.MyMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 try
  if FDragging then begin
   if Sender is tImage then
    (Sender as tImage).Hint:=''
   else
    (Sender as tLabel).Hint:='';
   FDragging := False;
   TMoveCracker(Sender).MouseCapture := False;
   Application.OnHint := Leiste.ShowHint;
  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('05218',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TBLVKarten.MyStdButton16Click(Sender: TObject);
begin
 showhilfeidx(helpcontext);
end;

end.
