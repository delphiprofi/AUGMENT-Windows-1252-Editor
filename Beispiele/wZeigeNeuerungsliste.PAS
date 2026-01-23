unit WZeigeNeuerungsliste;//12.08.16

(*

Aufbau der news20xx.rtf:

{ixxxx}

{Neuerung|Wichtig|Hervorheben|Call:xxxx|Link:Bezeichnung;xxxx ODER URL:Bezeichnung;www.xxx|Bild:xxxx ODER Bilder:xxxxx|Video:xxxx oder VIDEOS:xxxx|[Thema]|EMAIL:}

Wichtig = Text Wichtig am Rand
Hervorheben = roter Rahmen um Text

Land: unbedingt zweistellig angeben, mehrere mit Komma trennen
01: Schleswig-Holstein
02: Hamburg
03: Bremen
04: Berlin
05: Niedersachsen
06: NRW
07: Hessen
08: Rheinlandpfalz
09: Saarland
10: Baden-Wùrttemberg
11: Bayern
12: Mecklenburg
13: Brandenburg
14: Sachsen-Anhalt
15: Thùringen
16: Sachsen

Thema (mehrere Themen durch Komma trennen):
Auùendienst, MW, Mobil, Apple, Android
HomeWork
DR I
DR II
KB I, KB II
Formulare oder Formular oder Brief oder Briefe
VAK
Drittstelle [ist auch immer gleichzeitig das Thema Schnittstelle]
Schnittstelle
EGV P
Der Filter sucht nach den diesen Texten 

KEINE E-Mail oder Weblinks in den Text einfùgen!

*)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, MLButton, ExtCtrls, ComCtrls, wownlabel, WEingabe, myform,
  MSHTML, RichEdit, XEComp, 
  RxRichEd, Mask, WMask, MyColorBitBtn, AdvGlowButton, Wmemo, OleCtrls, SHDocVw,
  MyStdButton;

type
  tfundstellen=Array of record
                  Beginn, Len:integer;
                  Color,BColor:integer;
                 end;

  TfZeigeNeuerungsliste = class(TmyForm)
    Container: TScrollBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label5: TLabel;
    StandardFont: TLabel;
    NOK: TMyStdButton;
    BETA: TCheckBox;
    Button1: TMyStdButton;
    Jahr: TComboBox;
    Druck: TMyStdButton;
    Button2: TMyStdButton;
    SuchEingabe: TEingabe;
    Label2: TLabel;
    SuchBtn: TMyStdButton;
    LastS: TMyStdButton;
    NextS: TMyStdButton;
    Suchergebnis: TLabel;
    Panel3: TPanel;
    Label3: TLabel;
    Filter: TComboBox;
    Filtern: TMyStdButton;
    Editor2: TRxRichEdit;
    SpeechBtn: TMyStdButton;
    StopSpeechBtn: TMyStdButton;
    StopSpeechL: TTimer;
    Button3: TMyStdButton;
    Disclaimer: TPanel;
    DisclaimerTxt: TOwnMemo;
    BitBtn1: TMyStdButton;
    StandardFontBtn: TLabel;
    ShowAll: TCheckBox;
    Panel6: TPanel;
    Panel7: TPanel;
    BitBtn2: TMyStdButton;
    BitBtn3: TMyStdButton;
    VideoPanel: TPanel;
    BitBtn4: TMyStdButton;
    WebBrowser1: TWebBrowser;
    BilderPanel: TPanel;
    BitBtn5: TMyStdButton;
    WebBrowser2: TWebBrowser;
    Neu: TPanel;
    NewsContainer: TPanel;
    Image1: TImage;
    Image2: TImage;
    RahmenTop: TPanel;
    RahmenBottom: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    OwnLabel1: TOwnLabel;
    MyStdButton16: TMyStdButton;
    Panel5: TPanel;
    MyStdButton1: TMyStdButton;
    OwnLabel2: TOwnLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BETAClick(Sender: TObject);
    procedure DruckClick(Sender: TObject);
    procedure MehrClick(Sender: TObject);
    procedure JahrChange(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SearchClick(Sender: TObject);
    procedure SearchagainClick(Sender: TObject);
    procedure SuchBtnClick(Sender: TObject);
    procedure LastSClick(Sender: TObject);
    procedure NextSClick(Sender: TObject);
    procedure SuchEingabeClick(Sender: TObject);
    procedure FilterClick(Sender: TObject);
    procedure FilternClick(Sender: TObject);
    procedure SuchEingabeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure StopSpeechBtnClick(Sender: TObject);
    procedure StopSpeechLTimer(Sender: TObject);
    procedure SpeechBtnClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ShowAllClick(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ContainerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MyStdButton16Click(Sender: TObject);
    procedure MyStdButton1Click(Sender: TObject);
  private  // FRank
    { Private-Deklarationen }
   Fundstellen:tFundstellen;
   lastsuchstr,
   iNrMin,
   iNrMax,
   lastiNr,
   suchstr,
   lastjahr:shortstring;
   Anz,
   LastFilter,
   AktFundstelle,
   suchpos,
   _Start,Ende : Integer;
   Range : TFormatRange;
   URLList : array of String;
   BilderList : Array of String;
//10.04.16   MsgR:tMessageEvent;

   Editor: TRxRichEdit;

   procedure OnPlayClick(Sender:tobject);
   procedure OnStopClick(Sender:tobject);

(*20.08.17 ungenutzt
   procedure InsertBMP(BMP:tBitmap);
   *)
//20.08.17, ungenutzt   procedure InsertButtonRTF(Caption:string;Call:Integer;Link:string);
   procedure KillSuchErgebnisse;//Originalfarben wiederherstellen
   function  Getpage(Const memo: TRxRichEdit; caretpos: integer; var Allpage: integer): integer;//25.07.12
   function  PrintRTFToBitmap(Const ARichEdit : TRxRichEdit; ABitmap : TBitmap) : Longint;
   procedure Cherche;
   procedure DoPanel(Datum,iNr:string;
                                        Call:integer;
                                        Bilder,Video,
                                        Webseite,WebSeiteCaption,Webseite2,Webseite2Caption,
                                        email,emailcaption:String;
                                        wichtig, hervorheben:boolean;
                                        Var TopPos:integer);
  private
   _beta,
   _lokal:boolean;
   AlterLink:ansistring;
   res:tMemorystream;
   //fPlusPanelNews:tPlusPanel;
   inited,
   betahint:boolean;
//10.04.16   procedure AppMessag e(var Msg: TMsg; var Handled: Boolean);
   procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
   function  Load(AllowPreLoad:Boolean=false):boolean;
   procedure Showit(NewFromRessource:boolean{!! Damit wird Editor neu aus Ressource belegt!});
   function  NewsName:shortstring;
  public
    Class Procedure BetaChecked(aBetaCheckBox : TCheckBox);
  end;

procedure ZeigeNeuerungsliste(LOKAL:boolean=false{nimmt news von g!};
                              beta:boolean=false{28.03.16};
                              Filter:Integer=-1//15.04.20
                              );

implementation

{$IFNDEF P367}
{$R *.DFM}
{$ENDIF}

uses
  wdek
, dek
, basis
, wbasis
, speziell
, scaleform
, loadstd
, spez2
, updates
, wRegCode
, SOAPStream
, SOAPIO
, types
, spooler
, Menus
, menuesys
, wleiste
, wgetstrmodal
, Dialogs.Intf
, winsys
, wtools
, wemail
, eAktenSystem.Intf
, eAktenSystem.D2007
, utilitie
, weditor
, wportalget
, Hinweise
;

Const ButtonBreite=140; // FL 14.03.16 100;
      iNrBreite=70;//26.03.17 100;
      HeaderColor=$00CEFFCE;

Function LokalDrive:String;
begin
 //02.09.21:
 if CompanyRights(true) then
  Result:='c:\gv\'
 else
  
  Result:='\\Achim2\g\kunden\';//15.10.19
end;
 
function StreamToString(aStream: TStream): string;
var SS: TStringStream;
begin
 Result := '';
 try
  if aStream <> nil then begin
   SS := TStringStream.Create('');
   try
    SS.CopyFrom(aStream, 0);  // No need to position at 0 nor provide size
    Result := SS.DataString;
   finally
    SS.Free;
   end;
  end;
 except
 end;
end;

Function DecryptStream(InStream : TMemoryStream) : TMemoryStream;

  Procedure Stream2DynArray(S : TMemoryStream;Var Daten : TByteDynArray);
  begin
    S.Position := 0;
    Setlength(Daten,S.Size);
    Move(S.Memory^,Daten[0],length(Daten));
  end;

  Procedure DynArray2Stream(Const Daten : TByteDynArray;Var S : TMemoryStream);
  begin
    S.SetSize(length(Daten));
    S.Position := 0;
    Move(Daten[0],S.Memory^,length(Daten));
    S.Position := 0;
  end;

var
  Data       : TByteDynArray;
  Crypt      : TSOAPIO;
  SOAP       : ISOAPStream;
  MS         : TMemoryStream;
begin
 result:=nil;//13.03.16
  try
    MS := TMemoryStream.Create; // Immer eine Stream damit nicht NIL-Exception
    try
      Crypt := TSOAPIO.Create;
      try
        Stream2DynArray(InStream,Data);

        SOAP := TSOAPStream.Create;

        if SOAP.Assign(Crypt.CallData(Data,false)) then
          begin
            SOAP.Lese(Data);
            DynArray2Stream(Data,MS);
          end;
      finally
        Crypt.Free;
      end;
    finally
      Result := MS;
    end;
  except // fùr Achim :-)
  end;
end;


procedure ZeigeNeuerungsliste;
var f:tfZeigeNeuerungsliste;
    D:datumstyp;
    j,
    i:integer;
begin

 if {P367Mode or} PEVMode or pvbmode then
  TDialogs.mymessagedlg('~Hinweis zur Neuerungsliste'#13+
               'Die Neuerungsliste ist fùr mehrere Produkte erstellt und enthùlt Neuerungen unserer Software fùr Gerichtsvollzieher, fùr Vollziehungsbeamte, zur VAK-Abnahme und zur Forderungsaufstellung.'#13#13+
               'Viele Neuerungen gelten fùr alle Produkte.',
               mtinformation,[mbok],0);

 try
  f:=tfZeigeNeuerungsliste.create(application);
  f.inited:=false;
  f.betahint:=false;
  f.AlterLink:='';
  datumlesen(D);
  f._lokal:=lokal;
  f._beta:=beta;
  f.lastsuchstr:='';
  f.LastFilter:=0;
  f.suchstr:='';
  f.button2.visible:=(User_Status=0);
  f.ShowAll.Visible:=(User_Status=0);//26.04.16
//10.04.16  F.MsgR:=App lication.OnMe ssage;
//10.04.16  Applicat ion.OnMes sage:=F.AppMessag e;
  f.SpeechBtn.Enabled:=IsWin8;

  f.Jahr.Items.Clear;
  //Damit stets mind. Neuerungsliste vom Releasejahr gezeigt wird!
  Val(Copy(ReleaseDate,7,2),j,dek.error);
  j:=j+100;//damit mind. 2016!
  if AktJahr>=j then
   j:=AktJahr;
  for i := j downto AktJahr-1 do //03.01.18 116 do
   f.Jahr.Items.Add(inttostr(1900+i));
  f.Jahr.ItemIndex:=0;
  f.lastjahr:=f.jahr.text;                                                                          { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }

  if Anwaerter or isPensionaer or
    AnfrageKunde or//28.03.16
    (KNr='0099') or//28.03.16 
     P367Mode or PEVMode or PVBMode or
     AbgelaufenNoMsgDlg(D,true)
     then begin
   f.beta.Tag := 1;
   f.beta.checked  :=FALSE;
   f.BETA.Tag := 0;
   f.beta.visible  :=false;
   f.button1.visible:=false;
   f.Label3.visible:=false;
   DIP[97]:=DIP[97] and (255-8);
   WriteOneDip(97);
  end
  else begin // FL 14.09.23
   f.beta.Tag := 1;
   f.beta.checked:=dip[97] and 8=8;
   f.BETA.Tag := 0;
  end;

  f.res:=tMemorystream.create;
  //Nur bei PlusPanel f.fPlusPanelNews:=tPlusPanel.Init(f.NewsPanel,f.NewsContainer,NIL,f.NewsMinus,9999);
  f.inited:=true;
  if f.Load(TRUE) then begin
   f.Showit(true);
   if f.Editor.Lines.Count>0 then begin

    //15.05.20:
    try
     if Filter<>-1 then begin
      f.Filter.ItemIndex:=Filter;
      F.FilternClick(f.Filtern);
     end;
    except
    end;

    f.Show;
   end
   else
    f.Close;
  end
  else
   f.Close;
 except
 end;
end;

procedure TfZeigeNeuerungsliste.Cherche;//doch nicht genutztes Suchsystem
var i:integer;
begin
 try
  suchstr:=ANSIUpperCase(Trim(suchstr));                                                            { TODO 2 -oXEFrank -cS_2_Short : XE }
  if suchstr>'' then begin
   i := Editor.FindText(suchstr,suchpos,Editor.GetTextLen,[]);
   if i <> -1 then begin
    Editor.SelStart := i;
    suchpos:=i+1;
//26.03.17    SearchAgain.Enabled:=(SUCHPOS<Editor.GetTextLen-1);
   end
   else
    WFehler('Keine (weiteren) Eintrùge gefunden...',2);
  end;
 except
 end;
end;

procedure TfZeigeNeuerungsliste.SearchagainClick(Sender: TObject);
begin
 cherche;
end;

procedure TfZeigeNeuerungsliste.SearchClick(Sender: TObject);
var code:VCLChar;
begin
 WinGetStrModal(Code,SUCHSTR,30,'Zu suchender Text',
           'Geben Sie einen Begriff oder das Teil eines Wortes zur Suche ein.',
           'Suche',0,'',FALSE);
 if code<>T_ESC then begin
  suchpos :=  0;
  cherche;
 end;
end;

procedure TfZeigeNeuerungsliste.ShowAllClick(Sender: TObject);
begin
 Showit(true);//Editor neu aus Ressource belegen!
 LastSuchStr:='';
 SuchBtnClick(Sender);
end;

procedure TfZeigeNeuerungsliste.Showit(NewFromRessource:boolean);
var temp:tmemorystream;
Var TopPos:integer;

 procedure AppZeileF( s : string;
                      Size : Integer=0;
                      Style : tFontStyles=[];
                      Color : tColor=clblack;
                      BackColor : tColor=$00F7ECD5//hellblau
                    );
 var i,j:integer;
 begin
  Editor.Lines.Append('%APPEND%');
  j := Editor.SelStart;
  i := Editor.FindText('%APPEND%',0,Editor.GetTextLen,[]);
  if i <> -1 then begin
   Editor.SelStart := i;
   Editor.SelLength:= 8;
   Editor.SelText  := s;
   if Size > 0 then
    Editor.SelAttributes.Size := Size;
   Editor.SelAttributes.Style := Style;
   Editor.SelAttributes.Color := Color;
   Editor.SelAttributes.BackColor:=BackColor;

   Editor.SelStart  := j;
   Editor.SelLength := 0;
  end;
 end;

 procedure Exc;
 var b,e:integer;
     header,
     temp, s:Ansistring; // FL 30.03.16 String

     DontShow,
     neueiNr,
     Wichtig,hervorheben:boolean;
     call:integer;
     Thema,
     urlcaption,url2caption,
     email,
     emailcaption,
     url,url2,
     datum,
     iNr,
     bilder,video:string;

  function weggefiltert:boolean;
  begin
   result:=false;
   if Filter.ItemIndex>0 then
    try
     case Filter.ItemIndex of
       1:Result:=iNr<>'i'+internal;
       2:Result:=not(wichtig);
       3:Result:=(Pos('HINWEIS',Uppercase(Header))=0);
       4:Result:=(Pos('AUùENDIENST',Uppercase(thema))=0) and
                 (Pos('MOBIL',Uppercase(thema))=0) and
                 (Pos('APPLE',Uppercase(thema))=0) and
                 (Pos('ANDROID',Uppercase(thema))=0) and
                 (Pos('MW',Uppercase(thema))=0);
       5:Result:=(Pos('HOMEWORK',Uppercase(thema))=0);
       6:Result:=(Pos('DR I,',Uppercase(thema))=0) and (Pos('DRI,',Uppercase(thema))=0) and
                 (Pos('DR II',Uppercase(thema))=0) and (Pos('DRII',Uppercase(thema))=0);
       7:Result:=(Pos('DR II',Uppercase(thema))=0) and (Pos('DRII',Uppercase(thema))=0);
       8:Result:=(Pos('KB I',Uppercase(thema))=0) and (Pos('KBI',Uppercase(thema))=0);
       9:Result:=(Pos('FORMULAR',Uppercase(thema))=0) and
                 (Pos('BRIEF',Uppercase(thema))=0) and
                 (Pos('BRIEFE',Uppercase(thema))=0) and
                 (Pos('FORMULARE',Uppercase(thema))=0);
      10:Result:=(Pos('VAK',Uppercase(thema))=0);
      11:Result:=(Pos('DRITTSTELLE',Uppercase(thema))=0) and
                 (Pos('SCHNITTSTELLE',Uppercase(thema))=0);
      12:Result:=(Pos('SCHNITTSTELLE',Uppercase(thema))=0);
      13:Result:=(Pos('EGVP',Uppercase(thema))=0) and (Pos('EBO',Uppercase(thema))=0);
      14:Result:=(Pos('FIRMA',Uppercase(thema))=0);
     end;
    except
    end;
   {
0
1 Nur aktuelle Version
2 Wichtiges
3 Hinweise
4 Auùendienst
5 HomeWork
6 DR I
7 DR II
8 KB I / KB II
9 Formulare/Briefe
10 VAK
11 Drittstellen
12 Schnittstellen
13 EGV P
14 Firma 15.02.20
   }
   //WICHTIG: Erst jetzt Thema zur Anzeige verbessern
   if pos(']',thema)>0 then
    thema:=copy(thema,1,pos(']',thema)-1);
  end;

  function Suchen:boolean;
  var suchpos,i,rettb,rettl:integer;
  begin
   result:=true;

   if suchstr>'' then begin

    rettb:=Editor.SelStart;
    rettl:=Editor.Sellength;

    suchpos:=_Start;
    
    //Kein Ergebnis im Absatz
    result:=Editor.FindText(suchstr,suchpos,Ende-suchpos+1,[])<>-1;

    suchpos:=_Start;
    repeat
     i := Editor.FindText(suchstr,suchpos,Ende-suchpos+1,[]);
     if i <> -1 then begin
      Editor.SelStart := i;
      Editor.Sellength:=length(suchstr);
      suchpos:=i+length(suchstr);
      setlength(fundstellen,length(fundstellen)+1);
      fundstellen[high(fundstellen)].Beginn:=i;
      fundstellen[high(fundstellen)].Len   :=length(suchstr);
      //naja: speichert auch nur die Farbe des 1. Eintrages!
      fundstellen[high(fundstellen)].Color  :=Editor.Selattributes.Color;
      fundstellen[high(fundstellen)].BColor :=Editor.Selattributes.Backcolor;
      Editor.Selattributes.Color:=clwhite;
      Editor.Selattributes.BackColor:=clred;
     end;
    until i=-1;

    Editor.SelStart :=rettb;
    Editor.Sellength:=rettl;
   end;
  end;

 begin
  //Alle {} = Headertexte finden und ggf filtern

  //Text vor dem 1. {} wird ignoriert!!!
  iNr:='?';
  lastinr:=inr;                                                                                     { TODO 2 -oXEFrank -cS_2_Short : XE }
  iNrMin:='i9999';
  iNrMax:='i0000';
  Anz:=0;

  repeat
   neueiNr:=false;
   b := Editor.FindText('{',0,Editor.GetTextLen,[]);
   if b <> -1 then begin
    e := Editor.FindText('}',0,Editor.GetTextLen,[]);
    if e <> -1 then begin
     if b<e then begin

      _Start          :=B;
      Editor.SelStart :=B;
      Editor.SelLength:=E-B+1;
      Header          :=copy(Editor.SelText,2,length(Editor.SelText)-2);//=Headertext               { TODO 2 -oXEFrank -cS_2_Ansi : XE }
      if pos('|',header)>0 then
       Editor.SelText  :=copy(Header,1,pos('|',header)-1)
      else
       Editor.SelText  :=Header;
      Editor.SelAttributes.Style    :=[fsbold,fsunderline];
      Editor.SelAttributes.Color    :=clnavy;//26.03.17 clblack;
      Editor.SelAttributes.BackColor:=clwhite;//26.03.17 HeaderColor;
      Editor.SelAttributes.Size := Editor.SelAttributes.Size+2;//26.03.17

      Wichtig    :=false;
      hervorheben:=false;
      call       :=0;
      bilder     :='';
      video      :='';
      email      :='';
      emailcaption:='';
      urlcaption :='';
      url2caption:='';
      url        :='';
      url2       :='';
      thema      :='';
      DontShow   :=false;

      if (copy(Header,1,1)='i') and (Header[2] in ['0'..'9']) then begin
       iNr:=Header;
       neueiNr:=true;
      end
      else
      //Attribute finden:
      if Pos('|',Header)>0 then begin
       s:=Header;
       delete(s,1,Pos('|',s));
       while s>'' do begin
        if Pos('|',s)>0 then begin//weitere Attribute
         temp:=copy(s,1,Pos('|',s)-1);
         delete(s,1,Pos('|',s));
        end
        else begin
         temp:=s;
         s:='';
        end;
        temp:=trim((temp));//KEIN AnsiUppercase!                                                    { TODO 2 -oXEFrank -cS_2_Ansi : XE }

        //Thema:
        if copy(temp,1,1)='[' then begin
         delete(temp,1,1);
         if pos(']',temp)>0 then
          temp:=copy(temp,1,pos(']',temp));//WICHTIG: ] stehen lassen fùr FilterSuche!
         Thema:=temp;
        end
        else

        //04.10.19
        if copy(AnsiUppercase(temp),1,3)='NR:' then begin
         delete(temp,1,6);
         //:=trim(temp);
        end
        else

        if AnsiUppercase(temp)='WICHTIG' then begin
         wichtig:=true;
        end
        else
        if AnsiUppercase(temp)='HERVORHEBEN' then begin
         hervorheben:=true;
        end
        else
        if copy(AnsiUppercase(temp),1,5)='BILD:' then begin
         delete(temp,1,5);
         bilder:=trim(temp);
        end
        else
        if copy(AnsiUppercase(temp),1,7)='BILDER:' then begin
         delete(temp,1,7);
         bilder:=trim(temp);
        end
        else
        if copy(AnsiUppercase(temp),1,6)='VIDEO:' then begin
         delete(temp,1,6);
         video:=trim(temp);
        end
        else
        if copy(AnsiUppercase(temp),1,6)='VIDEOS:' then begin
         delete(temp,1,7);
         video:=trim(temp);
        end
        else
        if copy(AnsiUppercase(temp),1,4)='URL:' then begin
         delete(temp,1,4);
         if URL='' then begin
          urlcaption:=GetSemikolonTxtLong(temp);
          url:=trim(temp);
         end
         else begin
          url2caption:=GetSemikolonTxtLong(temp);
          url2:=trim(temp);
         end;
        end
        else
        if copy(AnsiUppercase(temp),1,5)='LINK:' then begin
         delete(temp,1,5);
         if URL='' then begin
          urlcaption:=GetSemikolonTxtLong(temp);
          url:=trim(temp);
         end
         else begin
          url2caption:=GetSemikolonTxtLong(temp);
          url2:=trim(temp);
         end;
        end
        else
        if copy(AnsiUppercase(temp),1,6)='EMAIL:' then begin
         delete(temp,1,6);
         emailcaption:=GetSemikolonTxtLong(temp);
         email:=trim(temp);
        end
        else
        if copy(AnsiUppercase(temp),1,5)='CALL:' then begin
         Val(copy(temp,6,4),call,dek.error);
         if not(MenuCallAllowed(Call)) then
          call:=0;
         //Button mit Call auf i !!!!
        end
        else
        if copy(AnsiUppercase(temp),1,5)='LAND:' then begin
         delete(temp,1,5);
         if (Pos(strWordNull(land,2),temp)>0) or
            (Pos(OffiziellShortLaender[land],temp)>0) //08.05.20
            then
         else
         if not(ShowAll.checked) then
          DontShow:=true;
        end
        else
         ;
       end;
      end;

      Ende := Editor.FindText('{',0,Editor.GetTextLen,[]);//Nùchster Tag
      if Ende=-1 then
       Ende:=Editor.GetTextLen-1
      else
       Ende:=Ende-1;

      Editor.SelLength:=Ende - _Start;

      //Datum?
      Datum:='';
      //zunùchst nicht unterstùtzen, denn so kann mich jeder ùberwachen und sehen, wann ich im Urlaub bin

      if not(Suchen) then begin
       Editor.SelText:='';
      end
      else

      if Weggefiltert then begin
       Editor.SelText:='';
      end
      else

      //iNr?
      if neueiNr then
      else

      if not(DontShow) then


      //Neue zukùnftigen Neuerungen zeigen?
      //ToDo !es mùsste die hùchste Nummer zum Download gesucht werden!
{ruhig alles zeigen
      if (iNr>'i'+Internal) and not(DIP[9 4] and 128=128) and not(AllowTester) then begin
       Editor.SelText  :='';
      end
      else}

       DoPanel(Datum,iNr,call,bilder,video,
               url,urlcaption,url2,url2caption,
               email,emailcaption,
               wichtig,hervorheben,TopPos);

     end
     else begin
      //b:=-1;
      //Fehler } steht vor { !!
      Editor.SelStart  :=B;
      Editor.SelLength :=1;
      Editor.SelText   :='';
     end;
    end
    else begin
     //Fehlerhafter Tag! Nur Beginn "{" lùschen
     Editor.SelStart  :=B;
     Editor.SelLength :=1;
     Editor.SelText   :='';
    end;
   end;
  until b=-1;
  if Editor.GetTextLen>0 then begin
   //ignorieren!
  end;

  Caption:='Neuerungen und ùberarbeitungen ('+inttostr(Anz)+' Eintrùge von '+iNrMin+' bis '+iNrMax+')';

  if _lokal then
   caption:=caption+' - lokale Version aus '+LokalDrive+'!';

  //Stat.Visible:=true;

 end;


begin//Showit
// Editor.Visible:=FALSE;

 PutWarte('Neuerungsliste wird erstellt ...');
 Panel1.Enabled:=FALSE;
 try

  if NewFromRessource then begin//13.03.16
   Editor.Lines.Clear;
   try
    if _lokal then
     Editor.Lines.LoadFromFile(LokalDrive+NewsName)
    else begin
     temp:=DecryptStream(res);
     Editor.Lines.LoadFromStream(temp);
     temp.free;
    end;

   except
   end;
  end;

  KillSuchErgebnisse;
  setlength(Fundstellen,0);
  AktFundstelle:=0;

  Setlength(URLList,0);
  setlength(BilderList,0);
  try
   While Container.ComponentCount > 0 do
    if Container.Components[0] is TPanel then
     Container.Components[0].Free;
  except
  end;
  TopPos:=0;

  Container.Visible:=FALSE;//Speed...
  Exc;
  Container.Visible:=TRUE;

  if suchstr>'' then begin

   if length(FundStellen)>0 then begin
    SuchErgebnis.Caption:=inttostr(length(fundstellen))+' Fundstellen farblich markiert';
 {
     LastS.Visible:=true;
     NextS.Visible:=true;
     LastS.Enabled:=false;
     NextS.Enabled:=true;
     }
    end
    else begin
     SuchErgebnis.Caption:='Nicht gefunden!';
     LastS.Visible:=false;
     NextS.Visible:=false;
    end;
    SuchErgebnis.Visible:=true;
   end
   else begin
    SuchErgebnis.Visible:=false;
    LastS.Visible:=false;
    NextS.Visible:=false;
   end;

 except
 end;
 Panel1.Enabled:=TRUE;
 DelWarte;
// Editor.Visible:=true;
end;

 procedure TfZeigeNeuerungsliste.SpeechBtnClick(Sender: TObject);
begin
 //15.03.16
 try

  if Voice<>nil then
   if IsSpeaking(Voice) then begin
    Voice.Stopp;
    application.processmessages;
   end;

  Editor.SelStart :=0;
  Editor.Sellength:=Editor.GetTextLen;

  SpeechBtn.Enabled:=false;
  StopSpeechBtn.Visible:=true;
  StopSpeechBtn.caption:='Stopp';
  Application.ProcessMessages;
  SpeechText(Editor.SelText);                                                                       { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  StopSpeechL.Enabled:=True;//Timer
 except
 
 end;
end;

procedure TfZeigeNeuerungsliste.StopSpeechBtnClick(Sender: TObject);
begin
 //10.03.16
 StopSpeechBtn.Visible:=false;
 try
  if StopSpeechBtn.caption='Fortf.' then begin
   StopSpeechBtn.caption:='Stopp';
   Voice.Resume;
  end
  else begin
   StopSpeechBtn.caption:='Fortf.';
   Voice.Pause;
  end;
 except
 end;
 SpeechBtn.Visible    :=true;
end;

procedure TfZeigeNeuerungsliste.StopSpeechLTimer(Sender: TObject);
begin
 //09.03.16:
 SpeechBtn.Enabled    :=not(IsSpeaking(Voice));//12.03.16 InSpeak);
 StopSpeechBtn.Visible:=not(SpeechBtn.Enabled);//12.03.16 InSpeak;
 StopSpeechL.Enabled  :=StopSpeechBtn.Visible;
end;

procedure TfZeigeNeuerungsliste.KillSuchErgebnisse;//Originalfarben wiederherstellen
 var r,i:integer;
 begin
  r:=Editor.SelStart;
  for i := 0 to high(fundstellen) do begin
   Editor.SelStart:=fundstellen[high(fundstellen)].Beginn;
   Editor.SelLength:=fundstellen[high(fundstellen)].Len;
   Editor.Selattributes.Color:=fundstellen[high(fundstellen)].Color;
   Editor.Selattributes.Backcolor:=fundstellen[high(fundstellen)].BColor;
  end;
  Editor.SelStart:=r;
  setlength(Fundstellen,0);
  SuchErgebnis.Visible:=false;
  LastS.Visible:=false;
  NextS.Visible:=false;
  SuchBtn.Enabled:=false;
 end;

procedure TfZeigeNeuerungsliste.SuchBtnClick(Sender: TObject);
//03.11.16 var i, first:integer;
begin
 try
  suchEingabe.text:=trim(suchEingabe.Text);                                                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  suchstr:=suchEingabe.text;

  if suchstr=lastsuchstr then begin
   WFehler('Sie haben denselben Text erneut gesucht.',2);
   exit;
  end;

  (*ist in Showit integriert!!!
  KillSuchErgebnisse;
  if suchstr>'' then begin
   first:=-1;
   setlength(Fundstellen,0);
   repeat
    i := Editor.FindText(suchstr,suchpos,Editor.GetTextLen,[]);
    if i <> -1 then begin
     Editor.SelStart := i;
     if first=-1 then
      first:=Editor.SelStart;
     Editor.Sellength:=length(suchstr);
     suchpos:=i+length(suchstr)-1;
     setlength(fundstellen,length(fundstellen)+1);
     fundstellen[high(fundstellen)].Beginn:=i;
     fundstellen[high(fundstellen)].Len   :=length(suchstr);
     //naja: speichert auch nur die Farbe des 1. Eintrages!
     fundstellen[high(fundstellen)].Color  :=Editor.Selattributes.Color;
     fundstellen[high(fundstellen)].BColor :=Editor.Selattributes.Backcolor;
     Editor.Selattributes.Color:=clwhite;
     Editor.Selattributes.BackColor:=clred;
    end;
   until i=-1;

   if first<>-1 then begin
    AktFundstelle:=0;
    Editor.SelStart:=first;//1. Fundstelle
    SuchErgebnis.Caption:=inttostr(length(fundstellen))+' Fundstellen farblich markiert';
{
    LastS.Visible:=true;
    NextS.Visible:=true;
    LastS.Enabled:=false;
    NextS.Enabled:=true;
    }
   end
   else begin
    SuchErgebnis.Caption:='Nicht gefunden!';
    LastS.Visible:=false;
    NextS.Visible:=false;
   end;
   SuchErgebnis.Visible:=true;
  end
  else begin
   SuchErgebnis.Visible:=false;
   LastS.Visible:=false;
   NextS.Visible:=false;
  end;
  //NEIN! Denn sonst wird Editor ja neu geladen! Load;
  Showit(false);//Damit Panels neu aufgebaut werden, aber NICHT Editor neu belegen!
  *)
  Showit(true);//komplett neu aufbauen!
  BitBtn2.Enabled:=true;
 except
 end;
end;

procedure TfZeigeNeuerungsliste.SuchEingabeChange(Sender: TObject);
begin
 SuchBtn.Enabled:=trim(SuchEingabe.Text)>'';
end;

procedure TfZeigeNeuerungsliste.SuchEingabeClick(Sender: TObject);
begin
 SuchBtn.Enabled:=trim(SuchEingabe.Text)>'';
end;

procedure TfZeigeNeuerungsliste.LastSClick(Sender: TObject);
begin
 if AktFundstelle<=0 then begin
  LastS.Enabled:=false;
 end
 else begin
  try
   dec(AktFundstelle);
   Editor.SelStart:=Fundstellen[AktFundstelle].Beginn;
   LastS.Enabled:=AktFundstelle>0;
  except
  end;
 end;
end;

procedure TfZeigeNeuerungsliste.NextSClick(Sender: TObject);
begin
 if AktFundstelle>=high(Fundstellen) then begin
  NextS.Enabled:=false;
 end
 else begin
  try
   inc(AktFundstelle);
   Editor.SelStart:=Fundstellen[AktFundstelle].Beginn;
   NextS.Enabled:=AktFundstelle<high(Fundstellen);
  except
  end;
 end;
end;

function TfZeigeNeuerungsliste.Load(AllowPreLoad:Boolean=false):boolean;

 function SubLoad:boolean;
 Var NewsNamePack:ShortString;
 begin
  NewsNamePack:=ChangeFileExt(NewsName,'.abp');//06.08.21 case sensitive! '.ABP');//28.09.17        { TODO 2 -oXEFrank -cS_2_Short : XE }
  result:=false;
  PutWarte('Lade Neuerungsliste...'#13'(Dauer je nach Internetverbindung)');
  try

   //13.04.16:
   if not(InternetAvail) then
   else begin

    //Alle DownloadFile Case sensitive:
    //28.03.16:
    if _beta then
     result:=DownloadFileToMem(Produktname, ErsterServer+GetDWLFolderXGVSBETA+'/'+NewsNamePack, res, nil)
    else

     result:=DownloadFileToMem(Produktname, ErsterServer+GetDWLFolderXGVS+'/'+NewsNamePack, res, nil);

    //13.04.16:
    if not(result) or (res.size=0) then
     if _beta then
      result:=DownloadFileToMem(Produktname, ZweiterServer+GetDWLFolderXGVSBETA+'/'+NewsNamePack, res, nil)
     else
      result:=DownloadFileToMem(Produktname, ZweiterServer+GetDWLFolderXGVS+'/'+NewsNamePack, res, nil);

    if not(result) or (res.size=0) then
     if _beta then
      result:=DownloadFileToMem(Produktname, DritterServer+GetDWLFolderXGVSBETA+'/'+NewsNamePack, res, nil)
     else
      result:=DownloadFileToMem(Produktname, DritterServer+GetDWLFolderXGVS+'/'+NewsNamePack, res, nil);

   end;
  except
  end;
  DelWarte;
 end;

begin
 result:=true;
 if _lokal then begin



{  //TEST
  if not(FileExists('g:\kunden\'+NewsName)) then
   MyMessageDlg('1. LOKALE Neuerungsliste g:\kunden\'+NewsName+' nicht verfùgbar.',
                mtinformation,[mbabort],0) else
   MyMessageDlg('1. LOKALE Neuerungsliste g:\kunden\'+NewsName+' verfùgbar.',
                mtinformation,[mbabort],0);
  if not(FileExists('\\achim2\g\kunden\'+NewsName)) then
   MyMessageDlg('2. LOKALE Neuerungsliste \\achim2\g\kunden\'+NewsName+' nicht verfùgbar.',
                mtinformation,[mbabort],0) else
   MyMessageDlg('2. LOKALE Neuerungsliste \\achim2\g\kunden\'+NewsName+' verfùgbar.',
                mtinformation,[mbabort],0);}



  if not(FileExists(LokalDrive+NewsName)) then begin
   TDialogs.MyMessageDlg('LOKALE Neuerungsliste '+LokalDrive+NewsName+' nicht verfùgbar.',
                mtinformation,[mbabort],0);
   result:=false;
  end;
 end
 else
 if not(SubLoad) or (res.Size=0) then begin

  //03.01.18:
  if AllowPreLoad then begin
   Jahr.ItemIndex:=1;
   Result:=Load(false);
  end
  else begin

   TDialogs.MyMessageDlg('~Neuerungsliste '+jahr.text+' nicht verfùgbar.'#13+
                'Versuchen Sie es zu einem spùteren Zeitpunkt erneut. Vielleicht ist nur der Server nicht erreichbar.'#13+
                'Sollten Sie nach 48 Stunden immer noch keinen Zugriff haben, wenden Sie sich bitte an den Service.',
                mtinformation,[mbabort],0);
   result:=false;
  end;

 end
 else begin
 end;
end;

procedure TfZeigeNeuerungsliste.DruckClick(Sender: TObject);
const temp2name='NEWS';
var f : file;
    s : shortstring;
    L:integer;
begin
 try

  DelFile(temppath,TEMP2Name+'.RTF');
  try
   Editor.Lines.SaveToFile(temppath+TEMP2Name+'.RTF');
  except
  end;

  ioresult;
  AssignFile(F,temppath+TEMP2Name+'.RTF');
  Reset(F,1);
  if ioresult = 0 then begin
   OeffneSPOOLER(148,1,GetLPT(148),'Neuerungsliste',DirektDruck,true,false,LeerDR);
   Seek(F,0);
   repeat
    BlockRead(F,S[1],255,L);
    S[0] := Ansichar(L); // FDXE_C
    if S > '' then
     PrintDATAABS(S);
   until l = 0;
   ioresult;
   CloseFile(F);
   ioresult;
   SchliesseSpoolerDat
  end;
  DelFile(temppath,TEMP2Name+'.RTF');

  WFehler('Gedruckt!',2);

 except
 end;
end;

procedure TfZeigeNeuerungsliste.BitBtn1Click(Sender: TObject);
begin
 Disclaimer.Visible:=false;
 Panel2.Visible:=TRUE;
 NEU.Visible:=TRUE;
end;

procedure TfZeigeNeuerungsliste.BitBtn2Click(Sender: TObject);
begin
 bitbtn2.enabled:=false;
 suchstr:='';
 lastsuchstr:='';
 Sucheingabe.Text:='';
 Showit(true);//komplett neu aufbauen!
end;

procedure TfZeigeNeuerungsliste.BitBtn3Click(Sender: TObject);
begin
 bitbtn3.enabled:=false;
 filter.itemindex:=0;
 filterclick(sender);
 Showit(true);//komplett neu aufbauen!
end;

procedure TfZeigeNeuerungsliste.BitBtn4Click(Sender: TObject);
begin
 VideoPanel.Visible:=false;
 Panel2.Visible:=TRUE;
 NEU.Visible:=true;
end;

procedure TfZeigeNeuerungsliste.BitBtn5Click(Sender: TObject);
begin
 BilderPanel.Visible:=false;
 Panel2.Visible:=TRUE;
 NEU.Visible:=true;
end;

procedure TfZeigeNeuerungsliste.MehrClick(Sender: TObject);
begin
// fPlusPanelNews.Change2Panel(Sender);
end;

procedure TfZeigeNeuerungsliste.MyStdButton16Click(Sender: TObject);
begin
 ShowHilfeIdx(HelpContext);
end;

procedure TfZeigeNeuerungsliste.MyStdButton1Click(Sender: TObject);
begin
 if AlterLink>'' then
  CallBrowser(AlterLink);
end;

procedure TfZeigeNeuerungsliste.Button1Click(Sender: TObject);
begin
  ShowBetaHint;
end;

procedure TfZeigeNeuerungsliste.Button2Click(Sender: TObject);
begin
 container.visible:=not(container.visible);
 editor.enabled:=true;
 editor.Align:=alclient;
 editor.visible:=not(container.visible);
end;

procedure TfZeigeNeuerungsliste.Button3Click(Sender: TObject);
begin
 NEU.Visible:=False;
 Panel2.Visible:=FALSE;
 Disclaimer.Align:=alclient;
 Disclaimer.Visible:=true;
end;

class procedure TfZeigeNeuerungsliste.BetaChecked(aBetaCheckBox: TCheckBox);
begin
  if aBetaCheckBox.Tag > 0 then
    exit;

  if aBetaCheckBox.checked then begin
   AskBeta;
   aBetaCheckBox.Tag := 1;
   aBetaCheckBox.Checked:=DIP[97] and 8=8;
   aBetaCheckBox.Tag := 0;
  end
  else begin
   DIP[97]:=DIP[97] and (255-8);
   WriteOneDip(97);
  end;
  TDialogs.mymessagedlg('Bitte das Programm neu starten, damit die ùnderung wirksam wird.',mtinformation,[mbok],0);
end;

procedure TfZeigeNeuerungsliste.BETAClick(Sender: TObject);
begin
  BetaChecked(Beta);
end;

procedure TfZeigeNeuerungsliste.FilterClick(Sender: TObject);
begin
 Filtern.Enabled:=(Filter.ItemIndex>0) or (LastFilter<>Filter.ItemIndex);
end;

procedure TfZeigeNeuerungsliste.FilternClick(Sender: TObject);
begin
 if trim(SuchEingabe.Text)>'' then
  KillSuchErgebnisse;
 //Nein Load;
 Showit(true);//Editor neu aus Ressource belegen!
 LastSuchStr:='';
 SuchBtnClick(Sender);
 LastFilter:=Filter.ItemIndex;
 BitBtn3.Enabled:=Filter.ItemIndex>0;
end;

procedure TfZeigeNeuerungsliste.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 try
   res.free;
 except
 end;
 try
//10.04.16  Appli cation.OnMessag e:=MsgR;
 except
 end; 
 action:=cafree;
end;

procedure TfZeigeNeuerungsliste.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 if StopSpeechBtn.Enabled then
  try
   if IsSpeaking(Voice) then
    Voice.STOPP;
  except
  end;
end;

procedure TfZeigeNeuerungsliste.FormCreate(Sender: TObject);
//03.11.16 var w:tweditor;
begin
  Editor:=Editor2;
  Editor.Perform(EM_SETTYPOGRAPHYOPTIONS, TO_ADVANCEDTYPOGRAPHY, TO_ADVANCEDTYPOGRAPHY);
  Perform(EM_SETTYPOGRAPHYOPTIONS, TO_ADVANCEDTYPOGRAPHY, TO_ADVANCEDTYPOGRAPHY);

{ w:=tweditor.Create(application);
 Editor:=w.Editor;}
end;

procedure TfZeigeNeuerungsliste.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 try
  if ChkENDKeys(Key,Shift) then
 //  OKClick(Sender)
  else
  if Key=vk_escape then begin
   if Disclaimer.Visible then
    bitbtn1click(sender)
   else
   if bilderpanel.visible then
    bitbtn5click(sender)
   else
   if videopanel.visible then
    bitbtn4click(sender)
   else
    NOKClick(Sender);
  end
  else

  //04.11.19:
  if Key=vk_escape then
   NOKClick(Sender)
  else

  if key=vk_next then begin
   if Container.VertScrollBar.Position>Container.VertScrollBar.Range-400 then
    Container.VertScrollBar.Position:=Container.VertScrollBar.Range-400
   else
    Container.VertScrollBar.Position:=Container.VertScrollBar.Position+400
  end
  else
  if (key=vk_prior) then//26.03.17 vk_next
   if Container.VertScrollBar.Position<=400 then
    Container.VertScrollBar.Position:=0
   else
    Container.VertScrollBar.Position:=Container.VertScrollBar.Position-400;
 except
 end;
end;

procedure TfZeigeNeuerungsliste.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
 //10.04.16:
 inherited;
 if PtInRect(Container.ClientRect, Container.ScreenToClient(MousePos)) then begin

  try
   //28.07.20:
   if InComboBox((Self as tForm),handled) then
    exit;

   Container.VertScrollBar.Position := Container.VertScrollBar.Position - WheelDelta;
  except
  end;
  Handled := true;
 end;

end;

procedure TfZeigeNeuerungsliste.NOKClick(Sender: TObject);
begin
 CLOSE;
end;

procedure TfZeigeNeuerungsliste.FormShow(Sender: TObject);
begin
 ScaleFormMy(Self);
 GetMaxWinSize(Self,_1280_2000,false,true);//15.06.21 GetMaxWinSize(Self,_1920,false);//X begrenzt durch Constraints
 Height:=GetClientHeight-20;//Y Maximieren
 top   :=gettop2(height);
 left  :=getleft2(width);
end;

procedure TfZeigeNeuerungsliste.JahrChange(Sender: TObject);
begin
 if inited and (lastjahr<>jahr.text) then begin
  Load;
  Showit(true);
  lastjahr:=jahr.text;                                                                              { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
 end;
end;

Const IMGRand=6;//Damit der Text nicht am Rand klebt!

function TfZeigeNeuerungsliste.PrintRTFToBitmap(Const ARichEdit : TRxRichEdit; ABitmap : TBitmap) : Longint;
 begin
    // Rendering to the same DC we are measuring.
    Range.hdc        := ABitmap.Canvas.handle;
    Range.hdcTarget  := ABitmap.Canvas.Handle;

    // Set up the page.
    Range.rc.left    := 0;
    Range.rc.top     := 0;
    Range.rc.right   := Round(ABitmap.Width * 1440 / Screen.PixelsPerInch);
    Range.rc.Bottom  := Round(ABitmap.Height * 1440 / Screen.PixelsPerInch);

    // format the text
    Result := SendMessage(ARichedit.Handle, EM_FORMATRANGE, 1, Longint(@Range));

    // Free cached information
    SendMessage(ARichEdit.handle, EM_FORMATRANGE, 0,0);
 end;


//measurement:
function TfZeigeNeuerungsliste.Getpage(Const memo: TRxRichEdit; caretpos: integer; var Allpage: integer): integer;//25.07.12
var
    LastChar,
//03.11.16     MaxLen,
    LogX, LogY, OldMap: Integer;
    SaveRect: TRect;
    //03.11.16 TextLenEx: TGetTextLengthEx;
begin
 result:=0;
 Allpage:=0;
 try
  begin
    Range.hdc := 0;             // no target device!
    Range.hdcTarget := 0;
    LogX := Screen.PixelsPerInch;// GetDeviceCaps(dc, LOGPIXELSX);
    LogY := Screen.PixelsPerInch;// GetDeviceCaps(dc, LOGPIXELSY);
    range.rc.left   := 0;
    range.rc.top    := 0;
    range.rc.right  := (Container.{FL 14.03.16 Client }Width - GetScrollbarwidth-10-ButtonBreite-iNrBreite -IMGRand*2) * 1440 div LogX;
    range.rc.bottom := 20000 * 1440 div LogY;
    Range.rcPage := Range.rc;

    SaveRect := Range.rc;

    { ensure printer DC is in text map mode }
    OldMap := SetMapMode(range.hdc, MM_TEXT);
    SendMessage(memo.Handle, EM_FORMATRANGE, 0, 0);    { flush buffer }
    try
      range.rc := SaveRect;
      // only measured:
//13.08.15 laut richprint 1 und nicht 0!      LastChar := SendMessage(memo.Handle, EM_FORMATRANGE, 0, Longint(@Range));
      LastChar := SendMessage(memo.Handle, EM_FORMATRANGE, 1, Longint(@Range));

      if (LastChar < Range.chrg.cpMax) and (Range.rc.bottom > 250000) then
        begin
          Range.chrg.cpMax := LastChar;
          {03.11.16 LastChar :=} SendMessage(Memo.Handle, EM_FORMATRANGE, 1, Longint(@Range));
        end;

    finally
      SendMessage(memo.Handle, EM_FORMATRANGE, 0, 0);  { flush buffer }
      SetMapMode(range.hdc, OldMap);       { restore previous map mode }
    end;

  end;

 except
 end;
end;

procedure TfZeigeNeuerungsliste.ButtonClick(Sender: TObject);
var SpoolHeader : tSpoolHeader;

 procedure DoVideo(link:string);
 begin
  //05.06.18
  CallBrowser(Link);                                                                                { TODO 2 -oXEFrank -cS_2_Ansi : XE }
 end;
 (*05.06.18
 var s:string;
 begin
  try
   NEU.Visible:=False;
   Panel2.Visible:=FALSE;
   VideoPanel.align  :=alclient;
   VideoPanel.Visible:=true;

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

   WFehler('Video wird geladen, was je nach Internetgeschwindigkeit einen Moment dauert',2);
   WB_LoadHTML(WebBrowser1,
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+
'<html xmlns="http://www.w3.org/1999/xhtml">'+
  '<head>'+
    '<meta content="text/html; charset=utf-8" http-equiv="Content-Type" />'+
    '<title>Video zur Neuerungsliste</title>'+
  '</head>'+
  '<body style="background-color: #000000">'+
    '<iframe width="'+inttostr(WebBrowser1.ClientWidth-GetScrollBarWidth)+
         '" height="'+inttostr(WebBrowser1.ClientHeight-GetScrollBarWidth)+
         '" '+

         link+

         ' frameborder="0" allowfullscreen'+
         '></iframe>'+
  '</body>'+
'</html>');
   application.processmessages;

  except
  end;
 end;*)

 procedure DoBild(link:string);
 begin
  try
   AlterLink:=link;                                                                                 { TODO 2 -oXEFrank -cS_2_Ansi : XE }
   NEU.Visible:=False;
   Panel2.Visible:=FALSE;
   VideoPanel.align  :=alclient;
   VideoPanel.Visible:=true;

   if pos('HTTP',uppercase(link))=0 then
    link:=cBilderURL{'http://www.gvi nfo.de/software/bilder/'}+link; // FL 03.08.21 URL als Const

   link:='src="'+link+'"';

   WFehler('Bild wird geladen, was je nach Internetgeschwindigkeit einen Moment dauert',2);
   WB_LoadHTML(WebBrowser1,
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+
'<html xmlns="http://www.w3.org/1999/xhtml">'+
  '<head>'+
    '<meta content="text/html; charset=utf-8" http-equiv="Content-Type" />'+
    '<title>Bild zur Neuerungsliste</title>'+
  '</head>'+
  '<body style="background-color: #000000">'+
    '<iframe width="'+inttostr(WebBrowser1.ClientWidth-GetScrollBarWidth)+
         '" height="'+inttostr(WebBrowser1.ClientHeight-GetScrollBarWidth)+
         '" '+

         link+

         ' frameborder="0" allowfullscreen'+
         '></iframe>'+
  '</body>'+
'</html>');
   application.processmessages;

  except
  end;
 end;

begin
 if (Sender as TColorBitButton).Tag>=6 then //darunter IDs fùr verschiedene Arten wie Link, Bilder
  SubAufruf((Sender as TColorBitButton).Tag)
 else begin
  if (Sender as tColorBitButton).Tag in [4] then begin//E-Mail
   InitSpoolHeader(SpoolHeader, eIgnore, '', 0, 0 );//kein DR etc.
   Wemaile('',LeerDR,False,(Sender as TColorBitButton).Hint,'','','',false,true,false, SpoolHeader);             { TODO 2 -oXEFrank -cS_2_Short : XE }
  end
  else
  if (Sender as tColorBitButton).Tag in [3] then //Videos
   DoVideo((Sender as TColorBitButton).Hint)
  else
  if (Sender as tColorBitButton).Tag in [2] then //Bilder
   DoBild((Sender as TColorBitButton).Hint)
  else
  if (Sender as tColorBitButton).Tag in [1] then //Link,
   CallBrowser((Sender as TColorBitButton).Hint)                                                    { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  else begin
  end;
 end;
end;

procedure TfZeigeNeuerungsliste.DoPanel(Datum,iNr:string;
                                        Call:integer;
                                        Bilder,Video,
                                        Webseite,WebSeiteCaption,Webseite2,Webseite2Caption,
                                        email,emailcaption:String;
                                        wichtig, hervorheben:boolean;
                                        Var TopPos:integer);
var
  A : Integer;
  maxh,
  buttontop,
  W,H : Integer;
//  BMP : TBitmap;
  IMG : TImage;
  Trenn,
  Trenn2,
  IMGBackground,
  Label2_,
  Label_  : TLabel;
  TrennPanel,
  Panel  : TPanel;
  Play_,Stop_:tspeedbutton;
//03.11.16   URL,URLCaption,Bildname : String;
  r:trect;

  procedure CreateBtn(Cap,Hint:string;ID:integer;Call:integer=0);
  var Button : tColorBitButton;
  begin
   Button         := tColorBitButton.Create(Panel);//falls mal geùndert, dann auch typecast suchen! 
   Button.Parent  := Panel;
   Button.Top     := ButtonTop;

   Button.Left    := Pred(IMG.Left+IMG.Width)+IMGRand+4;

   Button.Width   := Container.Width-Button.Left-GetScrollBarWidth-4;

   Button.TabStop := false;
   Button.Font.Assign(StandardFontBtn.Font);

   Button.Color           :=HeaderColor;//$00FFFF80;
   Button.ShadowColor     :=Button.Color;

   Button.PressColor      :=HeaderColor;//$00FAEED8;
   Button.PressShadowColor:=Button.PressColor;

   Button.OverColor       :=HeaderColor;//$00FAEED8;
   Button.OverShadowColor :=Button.OverColor;

   Button.Font.Color      :=clblack;
   Button.PressTextColor  :=clblack;
   Button.Font.Style      :=[fsbold];//25.03.17

   StandardFontBtn.Caption:=Cap;
   if (pos(#13,Cap)>0) or
      (StandardFontBtn.Canvas.TextWidth(Cap)>Button.Width-4) then begin
    Button.Height  := StandardFontBtn.Canvas.TextHeight('A')*2+8;
    Button.WordWrap:=true;
   end
   else
    Button.Height  := StandardFontBtn.Canvas.TextHeight('A')+8;
   Button.Caption := Cap;
   Button.Hint    := Hint;
   if Call<>0 then begin
    Button.Tag      := Call;
    Button.ShowHint := false;
   end
   else
    Button.Tag     := ID;
//   setlength(URLList,length(URLList)+1);
//   URLList[High(URLList)] := Webseite;
//   Button.Tag     := High(URLList);
   Button.OnClick := ButtonClick;

   inc(ButtonTop,button.height+4);
  end;

begin
 DisableRedraw(Container.Handle);
 try
    inc(Anz);

    if Container.Color<>StandardFont.Color then
     Container.Color:=StandardFont.Color;

    if (iNr<>LastiNr) then begin // and (LastiNr<>'?') then begin
     //Yep kein Scheiù 3 D Bevel

     TrennPanel := TPanel.Create(Container);
     TrennPanel.Parent := Container;
     TrennPanel.Top    := TopPos+8;
     TrennPanel.Height := 14;//4 vorher, 2 Breite 4 danach +4
     TrennPanel.Align  := altop;
     TrennPanel.ParentBackground:=false;
     TrennPanel.BorderStyle:=bsnone;
     TrennPanel.BevelInner :=bvnone;
     TrennPanel.BevelOuter :=bvnone;
     TrennPanel.Color  := StandardFont.Color;//clWhite;
     TrennPanel.TabStop:=false;
     TrennPanel.Visible:=true;

     Trenn:=tlabel.create(TrennPanel);
     Trenn.Parent  := TrennPanel;
     Trenn.AutoSize:= false;
     Trenn.Color   := clwhite;
     Trenn.Caption :='';
     Trenn.Transparent:=false;
     
     Trenn2:=tlabel.create(TrennPanel);
     Trenn2.Parent  := TrennPanel;
     Trenn2.AutoSize:= true;
     Trenn2.Top     := 4;
     Trenn2.Left    := iNrBreite;
     Trenn2.Color   := StandardFont.Font.Color;
     Trenn2.Font.Assign(StandardFont.Font);
     Trenn2.Font.Color := clred;//StandardFont.Color;
     Trenn2.Font.Style :=[fsbold];
     Trenn2.Transparent:=false;

     Trenn.Top     := 4 +Trenn2.Canvas.TextHeight('A') div 2;
     Trenn.Left    := 4;
     Trenn.Width   := Container.Width-GetScrollBarWidth-8;
     Trenn.Height  := 2;

     Trenn2.Caption :=' ab hier Version '+iNr+' ';
     TrennPanel.Height := 14+ Trenn2.Canvas.TextHeight('A');//4 vorher, 2 Breite 4 danach +4

     Inc(TopPos,TrennPanel.Height);
    end;
    LastiNr:=iNr;                                                                                   { TODO 2 -oXEFrank -cS_2_Short : XE }
    if iNr>iNrMax then
     iNrMax:=iNr;                                                                                   { TODO 2 -oXEFrank -cS_2_Short : XE }
    if inr<iNrMin then
     iNrMin:=iNr;                                                                                   { TODO 2 -oXEFrank -cS_2_Short : XE }

    Panel := TPanel.Create(Container);
    Panel.Parent := Container;
    Panel.Top    := TopPos+10;
    //Panel.Height := iNrBreite;
    Panel.Align  := altop;
    Panel.Color  := StandardFont.Color;//clWhite;
    Panel.ParentBackground:=false;
    Panel.BorderStyle:=bsnone;
    Panel.BevelInner :=bvnone;
    Panel.BevelOuter :=bvnone;
    Panel.TabStop    :=false;

    Panel.ShowHint:=FALSE;//!!!
    Panel.Hint:=Editor.SelText;

    //iNr
    Label_:=tlabel.create(Panel);
    Label_.Parent := Panel;
    Label_.Top    := 4;
    Label_.Left   := 4;
    Label_.Color  := StandardFont.Color;
    Label_.Font.Assign(StandardFont.Font);
    if Datum>'' then
     Label_.Caption:=Datum+#13+iNr
    else
     Label_.Caption:='Version'+#13+iNr;
    maxh:=Label_.Top+Label_.Height+2;

    //Wichtig
//03.11.16 wegen H2077    Label2_:=nil;
    if wichtig then begin
     Label2_:=tlabel.create(Panel);
     Label2_.Parent :=Panel;
     Label2_.Top    :=Label_.Top+Label_.Height+4;
     Label2_.Left   :=4;
     Label2_.Color  :=clred;
     Label2_.Font.Assign(StandardFont.Font);
     Label2_.Font.Style:=[fsbold];
     Label2_.Font.Color:=clwhite;
     Label2_.Transparent:=false;
     Label2_.Caption:='Wichtig!';
     maxh:=Label2_.Top+Label2_.Height+2;
    end;

    Play_:=tspeedbutton.create(panel);
    Play_.parent :=panel;
    Play_.left   :=4;
    Play_.Top    :=maxh+2;
    Play_.width  :=20;
    Play_.height :=20;
    Play_.Flat   :=true;
    Play_.OnClick:=OnPlayClick;
    Play_.transparent:=true;
    Play_.glyph.assign(Image1.Picture.Bitmap);

    Stop_:=tspeedbutton.create(panel);
    Stop_.parent :=panel;
    Stop_.left   :=pred(Play_.Left+Play_.Width)+4;
    Stop_.Top    :=Play_.Top;
    Stop_.width  :=20;
    Stop_.height :=20;
    Stop_.Flat   :=true;
    Stop_.OnClick:=OnStopClick;
    Stop_.transparent:=true;
    Stop_.glyph.assign(Image2.Picture.Bitmap);

    maxh:=Play_.Top+Play_.Height+2;

    {
    //Land
    if Bundesland>'' then begin
     Label3_:=tlabel.create(Panel);
     Label3_.Parent :=Panel;
     if Label2_<>nil then
      Label3_.Top    :=Label2_.Top+Label2_.Height+2
     else
      Label3_.Top    :=Label_.Top+Label_.Height+2;
     Label3_.Left   :=2;
     Label3_.Color  :=clred;
     Label3_.Font.Assign(StandardFont.Font);
     Label3_.Font.Color:=clwhite;
     Label3_.Transparent:=false;
     Label3_.AutoSize:=false;
     Label3_.Width:=iNrBreite;
     Label3_.Height:=
     Label3_.Caption:='Nur fùr '+Bundesland;
     maxh:=Label3_.Top+Label3_.Height+2;
    end;
    }

    {ToDo
    //Hervorheben
    if hervorheben then
     Panel.Color:=$00BBFFFF;
     }

    FillChar(Range, SizeOf(TFormatRange), 0);
    Range.chrg.cpMin := _Start;
    Range.chrg.cpMax := Ende-1;

    GetPage(Editor,0,A);
    H := Range.RC.Bottom * Screen.PixelsPerInch div 1440;
    W := Range.RC.Right  * Screen.PixelsPerInch div 1440;

    Range.chrg.cpMin := _Start;
    Range.chrg.cpMax := Ende-1;

    //Rand um Text in selber Farbe:
    IMGBackground:=tlabel.create(Panel);
    IMGBackground.Parent  :=Panel;
    IMGBackground.AutoSize:=FALSE;
    IMGBackground.Top     :=4;
    IMGBackground.Left    :=iNrBreite;
    IMGBackground.Width   :=W+IMGRand*2;
    IMGBackground.Transparent:=false;
    if hervorheben then
     IMGBackground.Color  :=CLRED
    else
     IMGBackground.Color  :=clwhite;
    IMGBackground.Caption :='';

    IMG := TImage.Create(Panel);
    IMG.Parent := Panel;
    IMG.Top    := 4+IMGRand;
    IMG.Left   := iNrBreite+IMGRand; // FL 14.03.16 ButtonBreite;
    IMG.Height := H;
    IMG.Width  := W;

// FL 15.11.22 Quark Erzeugt Memory Leak   IMG.Picture.Bitmap := TBitmap.Create;
    IMG.Picture.Bitmap.PixelFormat:=pf24Bit; // FL Mit diesem aufruf wird intern ein TBitmap Erzeugt. //13.08.16 hui, das fehlte
    IMG.Picture.Bitmap.Width  := W;
    IMG.Picture.Bitmap.Height := H;

    IMGBackground.Height:=H+IMGRand*2;

    IMG.Picture.Bitmap.Canvas.Pen.Color   := clwhite;
    IMG.Picture.Bitmap.Canvas.Brush.Color := clwhite;
    IMG.Picture.Bitmap.Canvas.Brush.style := bssolid;
    R.Left   := 0;
    R.Top    := 0;
    R.Right  := IMG.Picture.Bitmap.Width-1;
    R.Bottom := IMG.Picture.Bitmap.Height-1;
    IMG.Picture.Bitmap.Canvas.FillRect(R);

    PrintRTFToBitmap(Editor,IMG.Picture.Bitmap);

    ButtonTop:=4;
    if Call>0 then begin
     CreateBtn('Programmteil'#13'aufrufen','',0,Call);
    end;
    if EMail>'' then begin
     CreateBtn(EMailCaption,EMail,4);
    end;
    if Webseite>'' then begin
     CreateBtn(WebseiteCaption,Webseite,1);
    end;
    if Webseite2>'' then begin
     CreateBtn(Webseite2Caption,Webseite2,1);
    end;
    if Bilder>'' then begin
     CreateBtn('Bild(er)',Bilder,2);
    end;
    if Video>'' then begin
     CreateBtn('Video',Video,3);
    end;

    //Minimum fùr Panel!
    A:=pred(IMG.Top+IMG.Height)+IMGRand*2{In Farbe des IMG} +4{Und noch einmal Abstand und Trenner in Farbe des Containers};
    if maxh>A then
     A:=maxh;
    if buttontop>A then
     A:=buttontop;

    Panel.Height := A;
    inc(TopPos,Panel.Height);

//    IMG.Picture.Bitmap.Assign(BMP);
//    BMP.Free;

    Panel.Visible:=true;

    Container.VertScrollBar.Range:=Panel.Top+Panel.Height+4;

 except
 end;
 EnableRedraw(Container.Handle);
end;

(*//10.04.16
procedure TfZeigeNeuerungsliste.AppMessag e(var Msg: TMsg; var Handled: Boolean);
var i: SmallInt;
begin
 try
  if (Msg.message = WM_MOUSEWHEEL) and active and
     not(Filter.Focused and Filter.DroppedDown)
     then begin
   Msg.message := WM_KEYDOWN;
   Msg.lParam := 0;
   i := HiWord(Msg.wParam);
   if i > 0 then
    Container.VertScrollBar.Position := Container.VertScrollBar.Position - 50
   else
    Container.VertScrollBar.Position := Container.VertScrollBar.position + 50;
   Handled := False;
   if Assigned(MsgR) then
    MsgR(Msg,Handled);
  end;
 except
 end;
end;*)

procedure TfZeigeNeuerungsliste.CMDialogkey;
begin
 try
  with Container.VertScrollBar do begin
   case Msg.CharCode of
    VK_DOWN: Position := Position + Increment; // down arrow key
    VK_Up: Position   := Position - Increment; // up arrow key
    else
     inherited;
   end;
  end;
 except
 end;
end;

procedure TfZeigeNeuerungsliste.ContainerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
 //06.12.16
 try
  if PtInRect(Container.ClientRect, Container.ScreenToClient(Mouse.CursorPos)) then begin

   //28.07.20:
   if InComboBox((Self as tForm),handled) then
    exit;

   Container.VertScrollBar.Position := Container.VertScrollBar.Position - WheelDelta;
   Handled := True;
  end;
 except
 end;
end;

(*
function BitmapToRtf(bmp : TBitmap):string;
var DIBbmi, DIBbits : AnsiString;
InfoHeaderSize, ImageSize : cardinal;
i : integer;
begin
 GetDIBSizes(bmp.Handle, InfoHeaderSize, ImageSize);
 SetLength(DIBbmi,InfoHeaderSize);
 SetLength(DIBbits,ImageSize);
 GetDIB(bmp.Handle, bmp.Palette, PAnsiChar(DIBbmi)^, PAnsiChar(DIBbits)^);
 for i := 1 to InfoHeaderSize do
  Result := Result + IntToHex(Integer(DIBbmi[i]), 2);
 for i := 1 to ImageSize do
  Result := Result + IntToHex(Integer(DIBbits[i]), 2);
 Result := '{\rtf1 {\pict\dibitmap ' + Result + ' }}';
end;

procedure TfZeigeNeuerungsliste.Button1Click(Sender: TObject);
var SS:TStringStream;
bmp : TBitmap;
begin
 bmp := TBitmap.Create;
 bmp.LoadFromFile('Test.bmp');
 SS := TStringStream.Create(BitmapToRtf(bmp));
 Editor.PlainText := false;
 RxRichEdit1.Lines.LoadFromStream(SS);
 RxRichEdit1.Lines.SaveToFile('Test.rtf');
 // Works with WordPad and Word 2010 !!! SS.SaveToFile('TestStringStream.rtf'); // Works with WordPad only !!! SS.Free; bmp.Free; end; - See more at: http://codeverge.com/embarcadero.delphi.vcl.using/trichedit-insert-image-maybe-an/1074072#sthash.1QHfSDAm.dpuf
end;*)

// Stream Callback function
type
  TEditStreamCallBack = function(dwCookie: Longint; pbBuff: PByte;
    cb: Longint; var pcb: Longint): DWORD;
  stdcall;

  TEditStream = record
    dwCookie: Longint;
    dwError: Longint;
    pfnCallback: TEditStreamCallBack;
  end;

// RichEdit Type
type
  TMyRichEdit = TRxRichEdit;

// EditStreamInCallback callback function
function EditStreamInCallback(dwCookie: Longint; pbBuff: PByte;
  cb: Longint; var pcb: Longint): DWORD; stdcall;
  // by P. Below
var
  theStream: TStream;
  dataAvail: LongInt;
begin
  theStream := TStream(dwCookie);
  with theStream do
  begin
    dataAvail := Size - Position;
    Result := 0;
    if dataAvail <= cb then
    begin
      pcb := read(pbBuff^, dataAvail);
      if pcb <> dataAvail then
        Result := UINT(E_FAIL);
    end
    else
    begin
      pcb := read(pbBuff^, cb);
      if pcb <> cb then
        Result := UINT(E_FAIL);
    end;
  end;
end;

// Insert Stream into RichEdit
procedure PutRTFSelection(RichEdit: TMyRichEdit; SourceStream: TStream);
  // by P. Below
var
  EditStream: TEditStream;
begin
  with EditStream do
  begin
    dwCookie := Longint(SourceStream);
    dwError := 0;
    pfnCallback := EditStreamInCallBack;
  end;
  RichEdit.Perform(EM_STREAMIN, SF_RTF or SFF_SELECTION, Longint(@EditStream));
end;

// Convert Bitmap to RTF Code
function BitmapToRTF(pict: TBitmap): string;
// by D3k
var
  bi, bb, rtf: string;
  bis, bbs: Cardinal;
  achar: ShortString;
  hexpict: Ansistring; // FL 30.03.16 String
  I: Integer;
begin
  GetDIBSizes(pict.Handle, bis, bbs);
  SetLength(bi, bis);
  SetLength(bb, bbs);
  GetDIB(pict.Handle, pict.Palette, PChar(bi)^, PChar(bb)^);
  rtf := '{\rtf1 {\pict\dibitmap ';
  SetLength(hexpict, (Length(bb) + Length(bi)) * 2);
  I := 2;
  for bis := 1 to Length(bi) do
  begin
    achar := Format('%x', [Integer(bi[bis])]);                                                      { TODO 2 -oXEFrank -cS_2_Short : XE }
    if Length(achar) = 1 then
      achar := '0' + achar;
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  for bbs := 1 to Length(bb) do
  begin
    achar := Format('%x', [Integer(bb[bbs])]);                                                      { TODO 2 -oXEFrank -cS_2_Short : XE }
    if Length(achar) = 1 then
      achar := '0' + achar;
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  rtf := rtf + hexpict + ' }}';
  Result := rtf;
end;


(*20.08.17 ungenutzt
// Example to insert image from Image1 into RxRichEdit1
procedure TfZeigeNeuerungsliste.InsertBMP(BMP:tBitmap);
var SS: TStringStream;
begin
 try
  SS  := TStringStream.Create(BitmapToRTF(BMP));
  try
    PutRTFSelection(Editor, SS);
  finally
    SS.Free;
  end;
 except
 end;
end;*)

(*//20.08.17
procedure TfZeigeNeuerungsliste.InsertButtonRTF(Caption:string;Call:Integer;Link:string);
var B:tBitmap;
begin
 StandardFont.Caption:=Caption;

 B:=tbitmap.create;
 B.PixelFormat:=pf24Bit;
 B.Width :=StandardFont.Canvas.TextWidth(StandardFont.Caption)+8;
 B.Height:=StandardFont.Canvas.TextHeight(StandardFont.Caption)+8;
// B.OnClick
 InsertBMP(B);
 B.Free;

end;*)

procedure TfZeigeNeuerungsliste.OnPlayClick(Sender:tobject);
begin
 try
//  (Sender as tspeedbutton).Enabled:=false;
//  (Sender as tspeedbutton).Visible:=true;
//  StopSpeechBtn.caption:='Stopp';
//  Application.ProcessMessages;

  if Voice<>nil then
   if IsSpeaking(Voice) then begin
    Voice.Stopp;
    application.processmessages;
   end;

  SpeechText(((Sender as tspeedbutton).parent as tpanel).hint);                                     { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  
//  StopSpeechL.Enabled:=True;//Timer
 except
 end;
end;

procedure TfZeigeNeuerungsliste.OnStopClick(Sender:tobject);
begin
//
 if Voice<>nil then
  try
   Voice.Stopp;
  except
  end;
end;

function TfZeigeNeuerungsliste.NewsName:shortstring;
begin
 //Case sensitive:
 if PEVMode then
  result:='newsEVA_'+jahr.text+'x.rtf'
 else                                                                                               { TODO 2 -oXEFrank -cS_2_Short : XE }
 if P367Mode then
  result:='news367_'+jahr.text+'x.rtf'
 else                                                                                               { TODO 2 -oXEFrank -cS_2_Short : XE }
  result:='news'+jahr.text+'x.rtf';                                                                 { TODO 2 -oXEFrank -cS_2_Short : XE }
end;

end.
