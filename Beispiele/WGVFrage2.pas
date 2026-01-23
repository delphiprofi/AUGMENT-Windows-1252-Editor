unit WGVFrage2;
{$I Globalwarn.inc }
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, dek, ExtCtrls, wownlabel, myform, Grids,XEComp,
  MyColorBitBtn, AdvGlowButton, MyStdButton, UniGrids, UniSortGrid, UniStdCtrls,
  wOwnUniLabel;

type
  TFGVFrage2 = class(TmyForm)
    Label1: TOwnUniLabel;
    Label2: TOwnUniLabel;
    Button1: TMyStdButton;
    Button2: TMyStdButton;
    Button3: TMyStdButton;
    BitBtn1: TMyStdButton;
    KSB: TMyStdButton;
    KST: TOwnUniLabel;
    S1: TMyStdButton;
    T1: TOwnUniLabel;
    S2: TMyStdButton;
    T2: TOwnUniLabel;
    S3: TMyStdButton;
    T3: TOwnUniLabel;
    Hilfe: TMyStdButton;
    T1_2: TOwnUniLabel;
    T2_2: TOwnUniLabel;
    T3_2: TOwnUniLabel;
    S1_2: TMyStdButton;
    S2_2: TMyStdButton;
    S3_2: TMyStdButton;
    T4: TOwnUniLabel;
    T4_2: TOwnUniLabel;
    S4: TMyStdButton;
    S4_2: TMyStdButton;
    OwnLabel1: TOwnUniLabel;
    BitBtn2: TMyStdButton;
    GridAlt: TUniSortGrid;
    GridStd: TUniSortGrid;
    OwnLabel2: TOwnUniLabel;
    Test: TOwnUniLabel;
    BitBtn8: TMyStdButton;
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    MyStdButton2: TMyStdButton;
    MyStdButton1: TMyStdButton;
    FuerAlle: TMyStdButton;
    HinweisText: TOwnUniLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure KSBClick(Sender: TObject);
    procedure S1Click(Sender: TObject);
    procedure S2Click(Sender: TObject);
    procedure S3Click(Sender: TObject);
    procedure HilfeClick(Sender: TObject);
    procedure S1_2Click(Sender: TObject);
    procedure S2_2Click(Sender: TObject);
    procedure S3_2Click(Sender: TObject);
    procedure S4Click(Sender: TObject);
    procedure T4_2Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GridStdSelectCell(Sender: TObject; ACol, ARow: Integer;var CanSelect: Boolean);
    procedure GridAltSelectCell(Sender: TObject; ACol, ARow: Integer;var CanSelect: Boolean);
    procedure GridStdGetCellFormat(Sender: TObject; Col, Row: Integer;State: TGridDrawState; var FormatOptions: TFormatOptions);
    procedure GridAltGetCellFormat(Sender: TObject; Col, Row: Integer;State: TGridDrawState; var FormatOptions: TFormatOptions);
    procedure GridStdDblClick(Sender: TObject);
    procedure GridAltDblClick(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure GridStdClick(Sender: TObject);
    procedure GridAltEnter(Sender: TObject);
    procedure GridStdEnter(Sender: TObject);
    procedure GridAltDrawCell(Sender: TObject; ACol, ARow: Integer;Rect: TRect; State: TGridDrawState);
    procedure GridAltExit(Sender: TObject);
    procedure MyStdButton2Click(Sender: TObject);
    procedure MyStdButton1Click(Sender: TObject);
    procedure FuerAlleClick(Sender: TObject);
  private
   { Private-Deklarationen }
   procedure GetMultiS(ii:integer);//18.09.09:
   procedure GetMultiS3(ii:integer);//22.09.09:
   procedure GetMultiNew(C:AnsiChar;ii:integer);//16.03.17
   //shortstring OK:
   Procedure AddStd(Head:ShortString; A:AdressenTyp;Grid:tUniSortGrid;Col3:ShortString);//18.09.09  // FDXE_S
   procedure DrawSpecial(ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);//21.09.09
  public
   { Public-Deklarationen }
   Liste:Array of AdressenTypReg;
   Gesamt,
   AltAnschrift:integer;
   Accel:AnsiChar;//21.09.09  // FDXE_&C
   StdFocus,//21.09.09
   _DRI:Boolean;
   _DR:Nr_Typ;
     wgkey:VCLchar;
     _GV,_G, _KS,//18.09.09
     _Betreuer,//18.08.14
     A1_2,A2_2,A3_2,
     A4, A4_2,
       A1,
       A2,
       A3:Adressentyp;
  end;

//16.03.17: 200=Gl,201=Vert,202=Gl+Vert+KS,203=S+SV+Betreuer,204=DS,205:S+SV+DS (DS ist dann KS),206=DS
//05.04.18 Result: Wenn "Fùr alle Parteien" ausgewùhlt ist RESULT=TRUE

Function modal_WinGVFrage2( Art:Byte; GV, G, KS : Adressentyp;
                     KSVorgabe:Boolean;//29.09.12 KS vorgeben!
                     KSBez,//29.09.12
                     Txt : shortstring; AdressenArt : tAdressenArt;
                     Var _Out : AdressenTyp;
                     DRI:Boolean;DR:Nr_Typ;//18.09.09
                     FirstDefault:Boolean=false;//16.03.15 Defaultrow ist bei Gesamtschuldnern eben dieser - durch diesen boolean nicht - sinnvoll z.B. bei Ladung
                     AutoIfOnlyOne:Boolean=False;//16.03.17, wenn nur eine Partei existent, dann diese nehmen!
                     AllowFurAlle:Boolean=false;//05.04.18
                     FuerAlleTxt:Shortstring='';
                     HinweisText:Shortstring=''//22.09.22
                      ):boolean;overload;
                    {Art: 0=Gl., 1=Sch, 2=SchV, 9=Sch+Dritts.
                         66=NUR DS bei GV.Name='' 22.09.09 verwendet bei StammDS
                         67=Nur S bei StammZU 12.01.22
                         99=Gl. und diesen trotz Vertr. vorgeben! 09.05.05}

Function modal_WinGVFrage2( Art:Byte; GV, G, KS : Adressentyp;
                     Betreuer:Adressentyp;//18.08.14
                     KSVorgabe:Boolean;//29.09.12 KS vorgeben!
                     KSBez,//29.09.12
                     Txt : shortstring; AdressenArt : tAdressenArt;
                     Var _Out : AdressenTyp;
                     DRI:Boolean;DR:Nr_Typ;//18.09.09
                     FirstDefault:Boolean=false;//16.03.15 Defaultrow ist bei Gesamtschuldnern eben dieser - durch diesen boolean nicht - sinnvoll z.B. bei Ladung
                     AutoIfOnlyOne:Boolean=False;//16.03.17, wenn nur eine Partei existent, dann diese nehmen!
                     AllowFurAlle:Boolean=false;//05.04.18
                     FuerAlleTxt:Shortstring='';
                     HinweisText:Shortstring=''//22.09.22
                      ):boolean;overload;

implementation

uses
  basis, wbasis, emu,  kunden, speziell, spez2, baselib, wRegCode, wemail, wleiste, scaleform, wpopupmodal
, eAktenSystem.Intf
, eAktenSystem.D2007
, xj_SchriftgutObjectHandler    // FL 24.01.23
, Dialogs.Intf
, EGVP.AuftragAblehnen
;

{$IFNDEF P367}
{$R *.DFM}
{$ENDIF}

function modal_WinGVFrage2( Art:Byte; GV, G, KS : Adressentyp;
                     KSVorgabe:Boolean;//29.09.12 KS vorgeben!
                     KSBez,//29.09.12
                     Txt : shortstring; AdressenArt : tAdressenArt;
                     Var _Out : AdressenTyp;
                     DRI:Boolean;DR:Nr_Typ;//18.09.09
                     FirstDefault:Boolean=false;//16.03.15 Defaultrow ist bei Gesamtschuldnern eben dieser - durch diesen boolean nicht - sinnvoll z.B. bei Ladung
                     AutoIfOnlyOne:Boolean=False;//16.03.17, wenn nur eine Partei existent, dann diese nehmen!
                     AllowFurAlle:Boolean=false;//05.04.18
                     FuerAlleTxt:Shortstring='';
                     HinweisText:Shortstring=''//22.09.22
                      ):boolean;overload;
var leer:adressentyp;
begin
 fillchar(leer,sizeof(leer),0);
 result:=modal_WinGVFrage2( Art, GV, G, KS, Leer,
                    KSVorgabe,
                    KSBez,
                    Txt, AdressenArt,
                    _Out,
                    DRI,DR,
                    FirstDefault,
                    AutoIfOnlyOne,
                    AllowFurAlle,
                    FuerAlleTxt,
                    HinweisText
                  );
end;

function modal_WinGVFrage2( Art:Byte; //gruseliges System, dass je nach "Art" unterschiedlich abfragt
                     GV, G, KS : Adressentyp;
                     Betreuer:Adressentyp;//18.08.14
                     KSVorgabe:Boolean;//29.09.12 KS vorgeben!
                     KSBez,//29.09.12
                     Txt : shortstring; AdressenArt : tAdressenArt;
                     Var _Out : AdressenTyp;
                     DRI:Boolean;DR:Nr_Typ;//18.09.09
                     FirstDefault:Boolean=false;//16.03.15 Defaultrow ist bei Gesamtschuldnern eben dieser - durch diesen boolean nicht - sinnvoll z.B. bei Ladung
                     AutoIfOnlyOne:Boolean=False;//16.03.17, wenn nur eine Partei existent, dann diese nehmen!
                     AllowFurAlle:Boolean=false;//05.04.18
                     FuerAlleTxt:Shortstring='';
                     HinweisText:Shortstring=''//22.09.22
                      ):boolean;overload;
var FGVFrage2 : TFGVFrage2;
            L : AnsiString;  // FDXE_S
           ch : VCLChar;
    CombinedAdr3,//05.09.17
    CombinedAdr2,
    CombinedAdr:AdressenTypReg;

   Function Teste(Var A,A1,A2,A3, A1_2,A2_2,A3_2, A4, A4_2:AdressenTyp;ButtonText:ShortString):Boolean;//09.06.09
   //Var D,T:Integer;
   begin

    Result:=TesteMultiAdr(A,A1,A2,A3, A1_2,A2_2,A3_2, A4, A4_2);
    try
                                                                  
     if AllowAufsplittung then begin
      FGVFRage2.Hilfe.Visible    :=TRUE;
      FGVFrage2.BitBtn2.Visible  :=(AllowTester or (InternVersion='19.0')) and (InternVersion<'21.0');
     end;

     if Result and (ButtonText<>'') then begin
      if A1.Name>'' then begin
       FGVFRage2.S1.Visible:=TRUE;
       FGVFRage2.T1.Visible:=TRUE;
       FGVFRage2.S1.Caption:=ButtonText+' (&1)';
       GetAdresseIn1LineLongNotDruck(A1,L,true);
       FGVFRage2.T1.caption := (Crunch(A1.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(A1);

      end;
      if A2.Name>'' then begin
       FGVFRage2.S2.Visible:=TRUE;
       FGVFRage2.T2.Visible:=TRUE;
       FGVFRage2.S2.Caption:=ButtonText+' (&2)';
       GetAdresseIn1LineLongNotDruck(A2,L,true);
       FGVFRage2.T2.caption := (Crunch(A2.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(A2);
      end;
      if A3.Name>'' then begin
       FGVFRage2.S3.Visible:=TRUE;
       FGVFRage2.T3.Visible:=TRUE;
       FGVFRage2.S3.Caption:=ButtonText+' (&3)';
       GetAdresseIn1LineLongNotDruck(A3,L,true);
       FGVFRage2.T3.caption := (Crunch(A3.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(A3);
      end;

      if A1_2.Name>'' then begin
       FGVFRage2.S1_2.Visible:=TRUE;
       FGVFRage2.T1_2.Visible:=TRUE;
       FGVFRage2.S1_2.Caption:=ButtonText+' (&4)';
       GetAdresseIn1LineLongNotDruck(A1_2,L,true);
       FGVFRage2.T1_2.caption := (Crunch(A1_2.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(A1_2);

      end;
      if A2_2.Name>'' then begin
       FGVFRage2.S2_2.Visible:=TRUE;
       FGVFRage2.T2_2.Visible:=TRUE;
       FGVFRage2.S2_2.Caption:=ButtonText+' (&5)';
       GetAdresseIn1LineLongNotDruck(A2_2,L,true);
       FGVFRage2.T2_2.caption := (Crunch(A2_2.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(A2_2);
      end;
      if A3_2.Name>'' then begin
       FGVFRage2.S3_2.Visible:=TRUE;
       FGVFRage2.T3_2.Visible:=TRUE;
       FGVFRage2.S3_2.Caption:=ButtonText+' (&6)';
       GetAdresseIn1LineLongNotDruck(A3_2,L,true);
       FGVFRage2.T3_2.caption := (Crunch(A3_2.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(A3_2);
      end;

      if A4.Name>'' then begin
       FGVFRage2.S4.Visible:=TRUE;
       FGVFRage2.T4.Visible:=TRUE;
       FGVFRage2.S4.Caption:=ButtonText+' (&6)';
       GetAdresseIn1LineLongNotDruck(A4,L,true);
       FGVFRage2.T4.caption := (Crunch(A4.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(A4);
      end;
      if A4_2.Name>'' then begin
       FGVFRage2.S4_2.Visible:=TRUE;
       FGVFRage2.T4_2.Visible:=TRUE;
       FGVFRage2.S4_2.Caption:=ButtonText+' (&7)';
       GetAdresseIn1LineLongNotDruck(A4_2,L,true);
       FGVFRage2.T4_2.caption := (Crunch(A4_2.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(A4_2);
      end;

{      if FGVFRage2.S1.Visible or
         FGVFRage2.S2.Visible or
         FGVFRage2.S3.Visible then}
     end;

     with FGVFrage2 do begin
{26.06.09 klappt, warum auch immer, bei andreas/heinz nicht!!
      T:=S1.Top;
      D:=S2.Top-S1.Top;

      if (S1.Visible) or (S1_2.Visible) then
       Inc(T,D);

      S2.Top  :=T;
      T2.Top  :=T;
      S2_2.Top:=T;
      T2_2.Top:=T;

      if (S2.Visible) or (S2_2.Visible) then
       Inc(T,D);

      S3.Top  :=T;
      T3.Top  :=T;
      S3_2.Top:=T;
      T3_2.Top:=T;

      if (S3.Visible) and (S3_2.Visible) then
       Inc(T,D);

      S4.Top  :=T;
      T4.Top  :=T;
      S4_2.Top:=T;
      T4_2.Top:=T;
}
      if S1.Visible or S2.Visible or
         S3.Visible or S4.Visible or
         S1_2.Visible or S2_2.Visible or
         S3_2.Visible or S4_2.Visible then
       OwnLabel1.Visible:=TRUE;
     end;

    except
     on EXC_:Exception do
      ExceptionHintUser0('03128',EXC_.Message,EXC_.ClassName);
    end;
   end;

    procedure CombineAdr;//22.02.15
    var anz, i:integer;
        n, s:widestring;// shortstring;
        NameGleich:Boolean;

     function Std(var a:adressentypreg):shortstring;//OK, kein Wide nùtig
     begin
      result:=ansiuppercase(a.Anschrift.strasse+' '+a.Anschrift.hausnr+' '+a.Anschrift.plz+' '+a.Anschrift.ort);{ TODO 2 -oXEFrank -cS_2_Short : XE }
     end;

    var W:WideString; 
    begin
     //einfacher Vergleich nur der 1. Anschrift mit weiteren!

     anz:=0;//14.08.17 1;
     NameGleich:=true;
     s:=Std(FGVFRage2.Liste[0]);
     n:=WideUpperCase(FGVFRage2.Liste[0].NameUTF);

     //Gleiche Anschrift:
     for i := 1 to high(FGVFRage2.Liste) do
       if s=Std(FGVFRage2.Liste[i]) then begin
        inc(anz);
        if NameGleich and (WideUpperCase(FGVFRage2.Liste[i].NameUTF)<>n) then
         NameGleich:=false;
       end

       //15.03.15:
       else begin
        if NameGleich and (WideUpperCase(FGVFRage2.Liste[i].NameUTF)<>n) then
         NameGleich:=false;
       end;

     if anz>0{length(Liste)} then begin
      CombinedAdr:=FGVFRage2.Liste[0];//nur AdrReg, also wunderbar keine Geb-daten etc.
      CombinedAdr.RufName:='';
      CombinedAdr.Anrede:='';

      CombinedAdr2:=CombinedAdr;
      CombinedAdr3:=CombinedAdr;

      //*** Lùngenbegrenzung auf HùLFTE des Mùglichen, damit die Anschrift noch ins Adressfeld passt.

       //05.09.17 wie frùher alles zusammengepappt
       CombinedAdr3.nameUTF:='';
       CombinedAdr3.VornameUTF:='';
       for i:=0 to high(FGVFRage2.Liste) do begin
        W:=GetNameOnlyLineWide(AdrReg2Adr(FGVFRage2.Liste[i]));
        if length(CombinedAdr3.NameUTF)+length(W)<((NamenLen div 2)-5{Platz fùr u.a.}) then
         CombinedAdr3.nameUTF:=AddKomma(CombinedAdr3.nameUTF,W)
        else begin
         CombinedAdr3.nameUTF:=CombinedAdr3.nameUTF+' u.a.';
         break;
        end;
       end;
       if CombinedAdr3.nameUTF>'' then begin
        FGVFrage2.AddStd('Gesamt-'#13'schuldner',AdrReg2Adr(CombinedAdr3),FGVFrage2.GridStd,'J');
        //?? if NameGleich then FGVFrage2.Gesamt:=FGVFrage2.GridStd.RowCount-1;
       end;

      if NameGleich then begin
      end
      else begin
       //Verschiedene Namen:
       for i:=1 to high(FGVFRage2.Liste) do
        if length(CombinedAdr.NameUTF)+length(FGVFRage2.Liste[i].NameUTF)<((NamenLen div 2)-5{Platz fùr u.a.}) then
         CombinedAdr.nameUTF:=CombinedAdr.nameUTF+', '+FGVFRage2.Liste[i].NameUTF
        else begin
         CombinedAdr.nameUTF:=CombinedAdr.nameUTF+' u.a.';
         break;
        end;

      end;

      //Verschiedene Vornamen etc.
      for i:=1 to high(FGVFRage2.Liste) do begin
       if s=Std(FGVFRage2.Liste[i]) then begin

        if not(NameGleich) then//05.09.17, sonst kommt da etwas raus wie Hans Ilse Musertammn, Musterfrau
         CombinedAdr.VornameUTF:=''
        else
        if length(CombinedAdr.VornameUTF)+length(FGVFRage2.Liste[i].VornameUTF)<((VornameLen div 2)-5{Platz fùr u.a.}) then
         CombinedAdr.VornameUTF:=CombinedAdr.VornameUTF+', '+FGVFRage2.Liste[i].VornameUTF
        else begin
         CombinedAdr.VornameUTF:=CombinedAdr.VornameUTF+' u.a.';
         break;
        end;

        //Namensvorsatz:
        if Wideuppercase(FGVFRage2.Liste[0].namensvorsatzUTF)<>Wideuppercase(FGVFRage2.Liste[1].namensvorsatzUTF) then
         combinedadr.namensvorsatz:='';
        //Namensnachsatz:
        if Wideuppercase(FGVFRage2.Liste[0].namenszusatzUTF)<>Wideuppercase(FGVFRage2.Liste[1].namenszusatzUTF) then
         combinedadr.namenszusatzUTF:='';
        //Titel:
        if Wideuppercase(FGVFRage2.Liste[0].titelUTF)<>Wideuppercase(FGVFRage2.Liste[1].titelUTF) then
         combinedadr.titelUTF:='';
       end;
      end;

      FGVFrage2.AddStd('Gesamt-'#13'schuldner',AdrReg2Adr(CombinedAdr),FGVFrage2.GridStd,'G');

      if NameGleich then//14.08.17
       FGVFrage2.Gesamt:=FGVFrage2.GridStd.RowCount-1;

     end;

     //mind. eine Partei mit anderer Anschrift:
     if anz<>length(FGVFRage2.Liste) then begin
      if NameGleich then begin
      end
      else begin
       for i:=1 to high(FGVFRage2.Liste) do begin
        if length(CombinedAdr2.NameUTF)+length(FGVFRage2.Liste[i].NameUTF)<((NamenLen div 2)-5{Platz fùr u.a.}) then
         CombinedAdr2.nameUTF:=CombinedAdr2.nameUTF+', '+FGVFRage2.Liste[i].NameUTF
         else begin
          CombinedAdr2.nameUTF:=CombinedAdr2.nameUTF+' u.a.';
          break;
         end;

        //Verschiedene Vornamen etc.
        if not(NameGleich) then//05.09.17, sonst kommt da etwas raus wie Hans Ilse Musertammn, Musterfrau
         CombinedAdr2.VornameUTF:=''
        else
         if length(CombinedAdr2.VornameUTF)+length(FGVFRage2.Liste[i].VornameUTF)<((VornameLen div 2)-5{Platz fùr u.a.}) then
          CombinedAdr2.VornameUTF:=CombinedAdr2.VornameUTF+', '+FGVFRage2.Liste[i].VornameUTF
         else begin
          CombinedAdr2.VornameUTF:=CombinedAdr2.VornameUTF+' u.a.';
          break;
         end;

        //Namensvorsatz:
        if Wideuppercase(FGVFRage2.Liste[0].namensvorsatz)<>Wideuppercase(FGVFRage2.Liste[1].namensvorsatz) then
         combinedadr2.namensvorsatz:='';
        //Namensnachsatz:
        if Wideuppercase(FGVFRage2.Liste[0].namenszusatzUTF)<>Wideuppercase(FGVFRage2.Liste[1].namenszusatzUTF) then
         combinedadr2.namenszusatzUTF:='';
        //Titel:
        if Wideuppercase(FGVFRage2.Liste[0].titel)<>Wideuppercase(FGVFRage2.Liste[1].titel) then
         combinedadr2.titel:='';
       end;
      end;

      fillchar(combinedAdr2,sizeof(combinedAdr2),0);
      combinedAdr2.Anschrift.Strasse:='Anschrift wird erfragt';
      FGVFrage2.AddStd('Gesamt-'#13'schuldner',AdrReg2Adr(CombinedAdr2),FGVFrage2.GridStd,'H');
      if NameGleich then//14.08.17
       FGVFrage2.Gesamt:=FGVFrage2.GridStd.RowCount-1;

     end;

    end;

   procedure MultiS;//18.09.09:
   var i:integer;

   begin//MultiS
    //22.02.15:
    SetLength(FGVFRage2.Liste,1);

//16.03.17: 200=Gl,201=Vert,202=Gl+Vert+KS,203=S+SV+Betreuer,204=DS,205:S+SV+DS (DS ist dann KS),206=DS

    //14.08.17:
    if GV.Name>'' then
     FGVFRage2.Liste[0]:=Adr2AdrReg(GV)//ist hier der Schuldner
    else

     FGVFRage2.Liste[0]:=Adr2AdrReg(G);//ist hier der Schuldner

    i:=1;
    if GetNextAdr(DRI,DR,Adresse,Schuldner,true) then begin
     repeat

      //22.02.15:
      if Adresse.NameUTF>'' then begin
       SetLength(FGVFRage2.Liste,length(FGVFRage2.Liste)+1);
       FGVFRage2.Liste[high(FGVFRage2.Liste)]:=Adr2AdrReg(Adresse);
      end;

      FGVFrage2.AddStd('Schuldner'{14.10.09 DelAccels(FGVFRage2.Button2.Caption)},Adresse,FGVFrage2.GridStd,'S'+StrWordNull(i,4));//18.09.09
      inc(i);
     until not(GetNextAdr(DRI,DR,Adresse,Schuldner,false));
    end;

    if (high(FGVFRage2.Liste)>0) and (FGVFrage2.Liste[0].Name>''){14.08.17} then
     CombineAdr;//22.02.15
//NEIN!!!!    SetLength(FGVFRage2.Liste,0);
   end;

   procedure MultiDS;//22.09.09:
   var i:integer;
   begin
    i:=1;
    if GetNextAdr(DRI,DR,Adresse,Schuldner3,true) then begin
     repeat
      FGVFrage2.AddStd('Drittschuldner'{14.10.09 DelAccels(FGVFRage2.Button2.Caption)},Adresse,FGVFrage2.GridStd,'D'+StrWordNull(i,4));//18.09.09
      inc(i);
     until not(GetNextAdr(DRI,DR,Adresse,Schuldner3,false));
    end;
   end;

   Function TesteMultiS:Boolean;//21.09.09
   begin

    //19.02.15:
{    if length(Adresse.WeitereAnschrift)>0 then
     result:=true
    else} begin

     Result:=FALSE;
     Case Art of
      88 : ;
      66 : Result:=GetNextAdr(DRI,DR,Adresse,Schuldner3,true);//22.09.09
      67,//12.01.22
      19,//20.01.15
       9 : Result:=GetNextAdr(DRI,DR,Adresse,Schuldner,true);
       1 : Result:=GetNextAdr(DRI,DR,Adresse,Schuldner,true) or
                   GetNextAdr(DRI,DR,Adresse,Schuldner3,true);
     end;
    end;
   end;

   procedure DoButton1;//21.09.09:
   begin
    FGVFrage2.AddStd(FGVFRage2.Button1.Caption,GV,FGVFrage2.GridStd,'F1');//18.09.09                { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
    GetAdresseIn1LineLongNotDruck(GV,L,true);
    FGVFRage2.Label1.caption := (Crunch(GV.Anrede+' '+L))+
                               ' '+IstVerzogenTxt(GV);
   end;

   procedure DoButton2;//21.09.09:
   begin
    FGVFrage2.AddStd(FGVFRage2.Button2.Caption,G,FGVFrage2.GridStd,'F2');//18.09.09                 { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
    GetAdresseIn1LineLongNotDruck(G,L,true);
    FGVFRage2.Label2.caption := (Crunch(G.Anrede+' '+L))+
                                ' '+IstVerzogenTxt(G);
   end;

   procedure NewSystem;//16.03.17

    procedure AddMulti(Art:tAdressenArt;C:AnsiChar;Bez:Ansistring; AddListe:Boolean=FALSE{14.08.17} );
    var i:integer;
    begin
     i:=1;
     if GetNextAdr(DRI,DR,Adresse,Art,true) then begin
      repeat

       //14.08.17:
       if AddListe and (Adresse.NameUTF>'') then begin
        SetLength(FGVFRage2.Liste,length(FGVFRage2.Liste)+1);
        FGVFRage2.Liste[high(FGVFRage2.Liste)]:=Adr2AdrReg(Adresse);
       end;

       FGVFrage2.AddStd(bez,Adresse,FGVFrage2.GridStd,'X'+C+StrWordNull(i,4));                      { TODO 2 -oXEFrank -cS_2_Short : XE }
       inc(i);
      until not(GetNextAdr(DRI,DR,Adresse,Art,false));
     end;
    end;

   begin //NewSystem
    //200=Gl,201=Vert,202=Gl+Vert+KS,203=S+SV,204=DS,205:S+SV+DS,
    //Die Button1, Label1 etc. sind nicht mehr in Benutzung!?
    case Art of
     200:begin//G
          FGVFrage2.AddStd(' &Glùubiger',G,FGVFrage2.GridStd,'F2');
          AddMulti(Glaeubiger,'G','Glùubiger');
         end;
     201:begin//GV

          //09.09.17 oje
          if G.Name='' then
           FGVFrage2.AddStd(' Gl.-&Vertreter',GV,FGVFrage2.GridStd,'F1')
          else

           FGVFrage2.AddStd(' Gl.-&Vertreter',G,FGVFrage2.GridStd,'F2'); //F2=G

          AddMulti(GVertreter,'V','Gl.-Vertreter');
         end;
     202:begin//G,GV,KS
          FGVFrage2.AddStd(' Gl.-&Vertreter',GV,FGVFrage2.GridStd,'F1');
          AddMulti(GVertreter,'V','Gl.-Vertreter');
          FGVFrage2.AddStd(' &Glùubiger',G,FGVFrage2.GridStd,'F2');
          AddMulti(Glaeubiger,'G','Glùubiger');
          FGVFrage2.AddStd(' &Kostenbez.',KS,FGVFrage2.GridStd,'F4');
          //AddMulti(_KostSchuld,'K','Kostenbez.');
         end;
     203:begin //Betreuer...

    SetLength(FGVFRage2.Liste,1);
    FGVFRage2.Liste[0]:=Adr2AdrReg(G);//ist hier der Schuldner

          FGVFrage2.AddStd(' &Schuldner',G,FGVFrage2.GridStd,'F2');
          AddMulti(Schuldner,'S','Schuldner',TRUE);
          FGVFrage2.AddStd(' S&ch.-Vertr.',GV,FGVFrage2.GridStd,'F1');
          AddMulti(SchuldnerV,'C','Sch.-Vertr.');

    if (high(FGVFRage2.Liste)>0) and (FGVFrage2.Liste[0].Name>''){14.08.17} then
     CombineAdr;//22.02.15

         end;
     204:begin

          //09.09.17 oje
          if G.Name='' then
           FGVFrage2.AddStd(' &Drittschuldn.',GV,FGVFrage2.GridStd,'F1')//F1=GV
          else

           FGVFrage2.AddStd(' &Drittschuldn.',G,FGVFrage2.GridStd,'F2');//F2=G

          AddMulti(Schuldner3,'D','Drittschuldn.');
         end;
     205:begin
          FGVFrage2.AddStd(' &Drittschuldn.',KS,FGVFrage2.GridStd,'F4');
          AddMulti(Schuldner3,'D','Drittschuldn.');//09.04.20 AddMulti(Schuldner,'D','Drittschuldn.');

    SetLength(FGVFRage2.Liste,1);
    FGVFRage2.Liste[0]:=Adr2AdrReg(G);//ist hier der Schuldner

          FGVFrage2.AddStd(' &Schuldner',G,FGVFrage2.GridStd,'F2');
          AddMulti(Schuldner,'S','Schuldner',TRUE);
          FGVFrage2.AddStd(' S&ch.-Vertr.',GV,FGVFrage2.GridStd,'F1');
          AddMulti(SchuldnerV,'C','Sch.-Vertr.');

    if (high(FGVFRage2.Liste)>0) and (FGVFrage2.Liste[0].Name>''){14.08.17} then
     CombineAdr;//22.02.15

         end;
         //Betreuer nicht vergessen
    end;

    //24.10.17: Wenn nicht ùbergeben, dann nachsehen. Wozu ùberhaupt ùbergeben!?!
    if Art in [203,205] then begin
     if G.KostS>'' then begin
      Betreuer:=GetAdresseF(G.KostS);                     
     end;
    end;

   end;

  Var Buff:^DR_II_Typ;
      BuffI:^DR_I_Typ;
  _1,_2,_3,_1_2,_2_2,_3_2,_4,_4_2:^AdressenTyp;
  ii,yy1:integer;
  AA:spez2.ta;
  code:vclchar;
  lCollector : ISchriftgutCollector; // FL 24.01.23
  lEGVPAuftragAblehnen : IEGVPAuftragAblehnen;
  lEGVPAblehnenCount,
  lEGVPAblehnenIt : integer;
 begin//modal_WinGVFrage2;
  result:=false;
  try
   New(BuffI);
   BuffI^:=DR_In^;
   New(Buff);
   Buff^:=DR_II;

   fillchar(CombinedAdr,sizeof(CombinedAdr),0);
   //05.09.17:
   fillchar(CombinedAdr2,sizeof(CombinedAdr2),0);
   fillchar(CombinedAdr3,sizeof(CombinedAdr3),0);

   New(_1);
   New(_2);
   New(_3);
   New(_4);
   New(_1_2);
   New(_2_2);
   New(_3_2);
   New(_4_2);

   if (Art=0){not(Schuldner)} and
      (GV.NameUTF = '') and
      (KS.NameUTF = '') and //16.09.25
      (DIP[6] and 32 = 0) and
      not(TesteMultiS) and//21.09.09
      not(TesteMultiAdr(G,_1^,_2^,_3^, _1_2^,_2_2^,_3_2^, _4^,_4_2^))//09.06.09, 06.07.09
      //06.07.09      not(Teste(G,_1^,_2^,_3^, _1_2^,_2_2^,_3_2^, _4^,_4_2^, ''))//09.06.09
      then begin
    //09.06.09 !!!!!!!!!!!!!!!!!!!!!!! Test auf multiple Eintrùge...
    Adresse := G;
    _Out    := G;
   end
   else begin

    //22.09.09: z.B. bei 840er Druck mùglich...
    if (Art in [66, 67{12.01.22}] ) and (G.NameUTF='') and not(TesteMultiS) then begin
     P01(ch,Initialisieren,Txt+' '+GetDRStr(DR,DRI,''),False,AdressenArt,False);
    end
    else begin

     FGVFrage2 := tfgvfrage2.create(application);
     try
     SetLength(FGVFrage2.Liste,0);
     FGVFrage2.Caption := GetDRStr(DR,DRI,'')+': '+(TXT);
     fGVFrage2.Gesamt  := -1;

     //15.10.09:
     if User_Status=0 then
      FGVFrage2.Caption:=FGVFrage2.Caption+' ('+inttostr(Art)+')';

     //22.09.22:
     if HinweisText>'' then begin
      FGVFrage2.HinweisText.Caption:=HinweisText;
      FGVFrage2.HinweisText.Visible:=true;
     end;

     //22.04.16     FGVFrage2.Button3.Font.Style:=[fsbold];
     FGVFrage2._DRI:=DRI;
     FGVFrage2._DR :=DR;
     FGVFrage2.Accel:='1';//21.09.09
     FGVFrage2.StdFocus:=TRUE;//21.09.09
     FGVFrage2.GridAlt.DrawSpecial:=FGVFrage2.DrawSpecial;//21.09.09

     fillchar(FGVFrage2.A1,Sizeof(FGVFrage2.A1),0);
     fillchar(FGVFrage2.A2,Sizeof(FGVFrage2.A2),0);
     fillchar(FGVFrage2.A3,Sizeof(FGVFrage2.A3),0);
     fillchar(FGVFrage2.A1_2,Sizeof(FGVFrage2.A1_2),0);
     fillchar(FGVFrage2.A2_2,Sizeof(FGVFrage2.A2_2),0);
     fillchar(FGVFrage2.A3_2,Sizeof(FGVFrage2.A3_2),0);

     FGVFrage2.FuerAlle.Visible:=AllowFurAlle;
     if AllowFurAlle then
      FGVFrage2.FuerAlle.Caption:='Fùr alle &Parteien ein '+FuerAlleTxt;

     //18.09.09:
     try
      FGVFrage2.GridStd.RowCount:=0;
      FGVFrage2.GridAlt.RowCount:=0;
      FGVFrage2.GridStd.DefaultRowHeight:=(FGVFrage2.Test.Canvas.TextHeight('S')+2)*2;
      FGVFrage2.GridAlt.DefaultRowHeight:=FGVFrage2.GridStd.DefaultRowHeight;
     except
      on EXC_:Exception do
       ExceptionHintUser0('03129',EXC_.Message,EXC_.ClassName);
     end;

     //16.03.17:
     if Art in [200..210] then begin //200=Gl,201=Vert,202=Gl+Vert+KS,203=S+SV,204=DS,205:S+SV+DS,
      //Die Button1, Label1 etc. sind nicht mehr in Benutzung!?
      NewSystem
     end
     else begin

      if GV.NameUTF <> '' then begin

       //12.01.22:
       if Art=67 then
       else

       if Art=66 then //15.10.09
       else
       if Art=1{Schuldner} then begin
        FGVFrage2.Button1.Caption := '&Drittschuldner';
        DoButton1;//21.09.09
        MultiDS;//22.09.09
       end
       else
       if Art in [9, 19{20.01.15} ]{Schuldner} then begin
        FGVFrage2.Button1.Caption := '&Schuldner';
        DoButton1;//21.09.09
        MultiS;//18.09.09
       end
       else
       if Art=2 then begin
        FGVFrage2.Button1.Caption := 'S&ch.-Vertr. / Betreuer'; //08.02.11 Accel geùndert
        DoButton1;//21.09.09
       end
       else

       //22.09.09:
       if Art=88 then begin
        FGVFrage2.Button1.Caption := 'Adresse';
        DoButton1;
       end
       else

       if Art=99 then begin
        FGVFRage2.Button1.Caption := '&Glùubiger';
        DoButton1;//21.09.09
       end
       else begin
        FGVFRage2.Button1.Caption := '&Vertreter';
        DoButton1;//21.09.09
       end;

      end //GV.NameUTF>''
      else begin //GV.NameUTF=''
       FGVFRage2.Button1.Visible := FALSE;
       FGVFRage2.Label1.Visible  := FALSE;
      end;

      //20.01.15:
      if Art=19 then begin
       FGVFRage2.Button2.Caption := '&Vertreter';
       DoButton2;//21.09.09
      end
      else

      if Art=9 then begin
       FGVFrage2.Button2.Caption := '&Drittschuldner';
       if G.NameUTF='' then begin
        FGVFRage2.Button2.Visible := FALSE;
        FGVFRage2.Label2.Visible  := FALSE;
       end;
       DoButton2;//21.09.09
       MultiDS;//22.09.09
      end
      else
      if Art=99 then begin
       FGVFRage2.Button2.Caption := '&Vertreter';
       DoButton2;//21.09.09
      end
      else

      //22.09.09:
      if Art=88 then begin
       FGVFrage2.Button1.Caption := 'Adre&sse';
       FGVFrage2.Button2.Caption := 'Adresse';
       DoButton2;
      end
      else

      //12.01.22:
      if Art=67 then begin
       FGVFrage2.Button2.Caption := '&Schuldner';
       DoButton2;
       MultiS;
      end
      else

      if Art=66 then begin
       FGVFrage2.Button2.Caption := '&Drittschuldner';
       DoButton2;//15.10.09 DoButton2
       MultiDS;
      end
      else
      if Art = 100 then begin
        if TEGVPAuftragAblehnenfactory.InUse(lEGVPAuftragAblehnen) then
          begin
            FGVFrage2.Caption := 'Ablehnung eines elektronischen Auftrages';
            FGVFrage2.HinweisText.Visible := true;
            FGVFrage2.HinweisText.Caption := 'Folgende Parteien wurden aus den Prùfvermerken ausgelesen.' + #13#10 + 'Bitte wùhlen Sie den Empfùnger der Ablehnung';
            lEGVPAblehnenCount := 0;
            for lEGVPAblehnenIt := 0 to lEGVPAuftragAblehnen.Count -1 do
              begin
                FGVFrage2.AddStd('Absender',lEGVPAuftragAblehnen.Get(lEGVPAblehnenIt),FGVFrage2.GridStd,'?'+StrWordNull(lEGVPAblehnenCount,4));                      { TODO 2 -oXEFrank -cS_2_Short : XE }
                inc(lEGVPAblehnenCount);
              end;
          end;

      end
      else
      if Art<>0{Schuldner} then begin
       FGVFRage2.Button2.Caption := '&Schuldner';
       DoButton2;//21.09.09
       MultiS;//18.09.09
      end
      else begin
       FGVFRage2.Button2.Caption := '&Glùubiger';
       DoButton2;//21.09.09
      end;

      if KS.NameUTF <> '' then begin

       //29.09.12:
       if KSBez>'' then
        FGVFrage2.KSB.Caption := KSBez
       else

       //20.01.15:
       if Art in [19] then
        FGVFrage2.KSB.Caption := 'S&ch.-Vertr. / Betreuer'
       else

        FGVFrage2.KSB.Caption := '&Kostenbezahler';

       FGVFrage2.AddStd(FGVFrage2.KSB.Caption,KS,FGVFrage2.GridStd,'F4');//18.09.09                 { TODO 2 -oXEFrank -c'TCaption' zu 'ShortString' : XE }
       GetAdresseIn1LineLongNotDruck(KS,L,true);
       FGVFRage2.KST.caption := (Crunch(KS.Anrede+' '+L))+
                                  ' '+IstVerzogenTxt(KS);
      end
      else begin
       FGVFRage2.KSB.Visible := FALSE;
       FGVFRage2.KST.Visible := FALSE;
      end;
     end;


     //18.08.14:
     if Betreuer.NameUTF <> '' then begin
      FGVFrage2.AddStd('Betre&uer',Betreuer,FGVFrage2.GridStd,'F5');//F5 = Betreuer!
     end
     else begin
     end;


     //18.09.09:
     try

      //21.09.09:
      With FGVFrage2 do begin
       if GridStd.RowCount<=4 then
        GridStd.Height:=GridStd.DefaultRowHeight*GridStd.RowCount;//dann auch genug Puffer, da oberste Zeile einspaltig!
      end;

      if FGVFrage2.GridStd.RowCount>1 then

       //29.09.12:
       if KSVorgabe and (KS.NameUTF<>'') then
        FGVFrage2.GridStd.Row:=FGVFrage2.GridStd.RowCount-1
       else begin

        //23.02.15:
        if (FGVFrage2.Gesamt<>-1) and not(FirstDefault) then
         FGVFrage2.GridStd.Row:=FGVFrage2.Gesamt
        else

         FGVFrage2.GridStd.Row:=1;
       end;
 {sonst Fokus nicht zu erkennen!     if FGVFrage2.GridAlt.RowCount>1 then
       FGVFrage2.GridAlt.Row:=1;}
     except
      on EXC_:Exception do
       ExceptionHintUser0('03130',EXC_.Message,EXC_.ClassName);
     end;
//09.09.17     FGVFRage2.Button3.Caption := 'Ad&ress-eingabe';//22.04.16 'Ad&resseingabe';

 //    FGVFrage2.Show;

     FGVFrage2._G :=G;
     FGVFrage2._GV:=GV;
     FGVFrage2._KS:=KS;
     FGVFrage2._Betreuer:=Betreuer;//18.08.14

     //18.09.09:
     With FGvFrage2 do begin
      FGVFrage2.ActiveControl:=GridStd;
      BitBtn8.Visible:=TRUE;
      GridStdClick(FGVFrage2);//21.09.09 Analyse f. 1. Eintrag
      BorderStyle:=bsSizeable;//21.09.09
     end;

     //28.10.12:
     yy1:=0;
     for ii := 0 to fgvfrage2.GridStd.RowCount - 1 do
      yy1:=yy1+fgvfrage2.GridStd.RowHeights[ii]+2;
     if yy1>300 then
      yy1:=300;
     fgvfrage2.GridStd.Height:=yy1+2;

     FGVFrage2.wgkey:=#0;

     //19.07.17:
     if (fgvfrage2.gridstd.RowCount<=1) and
        (fgvfrage2.GridAlt.RowCount<=0) then begin
      fgvfrage2.Button3Click(fgvfrage2.Button3);//Adressabfrage
     end
     else

     //16.03.17:
     if AutoIfOnlyOne and
        (fgvfrage2.gridstd.RowCount=2) and
        (not(fgvfrage2.panel1.visible){11.09.17} or (fgvfrage2.GridAlt.RowCount<=0)) //23.08.17
        then begin//nur eine Adr
      fgvfrage2.StdFocus:=true;
      fgvfrage2.GridStd.Row:=1;
      fgvfrage2.BitBtn8Click(fgvfrage2.BitBtn8);
     end
     else

      MyShowModal(FGVFrage2);//14.08.14 FGVFrage2.S howModal;

     ch:=FGVFrage2.wgkey;

     Case ch of
        '.' : Result:=true;//05.04.18
        '~' : ;//Adresse vorbelegt!!!
      T_ESC : ClrAdresse;
      t_sf1 : Adresse:=FGVFrage2.A1;
      t_sf2 : Adresse:=FGVFrage2.A2;
      t_sf3 : Adresse:=FGVFrage2.A3;
        t_0 : Adresse:=FGVFrage2.A1_2;
        t_1 : Adresse:=FGVFrage2.A2_2;
        t_2 : Adresse:=FGVFrage2.A3_2;
        'x' : Adresse:=FGVFrage2.A4;
        'y' : Adresse:=FGVFrage2.A4_2;
       t_f2 : Adresse := G;
       t_f4 : Adresse := KS;
       t_f5 : Adresse := Betreuer;//18.08.14
       t_f6 : Adresse := AdrReg2Adr(CombinedAdr);

       //05.09.17:
       t_f8 : begin
               Adresse := AdrReg2Adr(CombinedAdr3);
{               preInitPopupmodal(AA);
               for yy1 := 0 to high(FGVFrage2.Liste) do
                AA[yy1+1]:=GetAdrOnlyLineLong(AdrReg2Adr(FGVFrage2.Liste[yy1]));
               yy1 := winPopUpmodal(Code,AA,0,'Zu verwendende Anschrift',1,TRUE,FALSE,0);
               if Code<>T_ESC then begin
                Adresse:=AdrReg2Adr(FGVFrage2.Liste[yy1]);
               end;}
              end;

       t_f7 : begin
               Adresse := AdrReg2Adr(CombinedAdr2);
               preInitPopupmodal(AA);
               for yy1 := 0 to high(FGVFrage2.Liste) do
                AA[yy1+1]:=GetAdrOnlyLineLong(AdrReg2Adr(FGVFrage2.Liste[yy1]));
               yy1 := winPopUpmodal(Code,AA,0,'Zu verwendende Anschrift',1,TRUE,FALSE,0);
               if Code<>T_ESC then begin
                Adresse:=AdrReg2Adr(FGVFrage2.Liste[yy1]);
               end;
              end;
       t_f3 : begin
               Adresse.NameUTF := '';
               //NICHT MODAL, GEFùHRLICH!!! Aber eigentlich durch Buff, BuffI geschùtzt!
               P01(ch,Initialisieren,'Adresseingabe',False,AdressenArt,False);
               if Adresse.NameUTF = '' then
                Adresse := G;
              end;
      else //T_F1
       Adresse := GV;
     end;

     //19.02.15:
     try
{      if (FGVFrage2.AltAnschrift<>-1) and
         (high(Adresse.WeitereAnschrift)>=FGVFrage2.AltAnschrift) then
       Adresse.Anschrift:=Adresse.WeitereAnschrift[FGVFrage2.AltAnschrift];}
     except
      on EXC_:Exception do
       ExceptionHintUser0('03131',EXC_.Message,EXC_.ClassName);
     end;

     SetLength(FGVFrage2.Liste,0);

     finally
 //    fgvfrage2.close;
      FGVFrage2.FREE;
     end;
    end;

    _Out := Adresse;
   end;
   Dispose(_1);
   Dispose(_2);
   Dispose(_3);
   Dispose(_4);
   Dispose(_1_2);
   Dispose(_2_2);
   Dispose(_3_2);
   Dispose(_4_2);

   DR_In^:=BuffI^;//16.03.12
   Dispose(BuffI);
   DR_II:=Buff^;
   Dispose(Buff);
   _OUT:=Adresse;//20.04.11 weil _OUT z.B. DR_II.Schuldner sein kann.
   // FL 24.01.23
   try   // BW 29.01.24 Potentielle Fehler abgefangen
     if TSchriftgutObject.Collecting(lCollector) then
       lCollector.SetEmpfaenger(@Adresse);
   except
   end;
  except
      on EXC_:Exception do
       ExceptionHintUser0('03132',EXC_.Message,EXC_.ClassName);
  end;
 end;

procedure TFGVFrage2.BitBtn1Click(Sender: TObject);
begin
 wgkey:=t_esc;
 CLOSE;
// inWinKey(t_esc);
end;

procedure TFGVFrage2.Button1Click(Sender: TObject);
begin
 wgkey:=t_f1;
 CLOSE;
// inWinkey(T_F1);
end;

procedure TFGVFrage2.Button2Click(Sender: TObject);
begin
 wgkey:=t_f2;
 CLOSE;
// inWinkey(T_F2);
end;

procedure TFGVFrage2.Button3Click(Sender: TObject);
begin
 wgkey:=t_f3;
 CLOSE;
// inWinkey(T_F3);
end;

procedure TFGVFrage2.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 if wgkey=#0 then begin
  wgkey:=t_esc;
 end;
end;

procedure TFGVFrage2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i:integer;
begin

 if ((key = vk_return) or
     ChkENDKeys(Key,Shift)) then begin //04.11.09
  //21.09.09:
  if true then //28.10.12 if Panel1.Visible then
   BitBtn8Click(Sender)
  else

  if button1.Focused then begin
   wgkey := t_f1;
   CLOSE;
  end
  else
  if button2.Focused then begin
   wgkey := t_f2;
   close;
  end
  else
  if button3.Focused then begin
   wgkey := t_f3;
   close;
  end
  else
   ;
 end
 else

 //21.09.09:
 if ((key>=ord('1')) and (key<=ord('9'))) or
    (UpCase(Chr(key)) in ['A'..'Z']) then begin
  try
   i:=1;
   While (i<=GridAlt.RowCount-1) and
         (Pos('&'+Upcase(Chr(key)),GridAlt.Cells[0,i])=0) do
    inc(i);
   if (i<=GridAlt.RowCount-1) then begin
    key:=0;//25.05.11
    GridAlt.Row:=i;
    StdFocus   :=FALSE;
    BitBtn8Click(Sender);
   end
   else begin
    i:=1;
    While (i<=GridStd.RowCount-1) and
          (Pos('&'+Upcase(Chr(key)),GridStd.Cells[0,i])=0) do
     inc(i);
    if (i<=GridStd.RowCount-1) then begin
     key:=0;//25.05.11
     GridStd.Row:=i;
     StdFocus   :=TRUE;
     BitBtn8Click(Sender);
    end
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('03133',EXC_.Message,EXC_.ClassName);
  end;
 end
{ if ((key>=ord('1')) and (key<=ord('9'))) or
    (UpCase(Chr(key)) in ['D','S','G','V','K'])
    then begin
  try
   i:=1;
   While (i<=GridStd.RowCount-1) and
         (Pos('&'+Chr(key),GridStd.Cells[0,i])=0) do
    inc(i);
   if (i<=GridStd.RowCount-1) then begin
    GridStd.Row:=i;
    StdFocus   :=TRUE;
    BitBtn8Click(Sender);
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('03134',EXC_.Message,EXC_.ClassName);
  end;
 end
 else
 if (UpCase(Chr(key)) in ['B','C','D','E','F','I','L','M']) then begin
  try
   i:=1;
   While (i<=GridAlt.RowCount-1) and
         (Pos('&'+Upcase(Chr(key)),GridAlt.Cells[0,i])=0) do
    inc(i);
   if (i<=GridAlt.RowCount-1) then begin
    GridAlt.Row:=i;
    StdFocus   :=FALSE;
    BitBtn8Click(Sender);
   end;
  except
   on EXC_:Exception do
    ExceptionHintUser0('03135',EXC_.Message,EXC_.ClassName);
  end;
 end }

end;

procedure TFGVFrage2.FormShow(Sender: TObject);
begin

 ScaleFormMy(Self);//27.06.10

 GetMaxWinSize(Self,_1600,true,true,false);//23.08.17
 
 Top  := GetTop_modal(height);
 Left := GetLeft_modal(width);

 try
  if true then //28.10.12   if Panel1.Visible then
   GridStd.Setfocus
  else
  if button1.visible then
   Button1.SetFocus
  else
  if button2.visible then
   Button2.SetFocus;
 except
  on EXC_:Exception do
   ExceptionHintUser0('03136',EXC_.Message,EXC_.ClassName);
 end;  
end;

procedure TFGVFrage2.FuerAlleClick(Sender: TObject);
begin
 wgkey:='.';
 Close;
end;

procedure TFGVFrage2.KSBClick(Sender: TObject);
begin
 wgkey:=t_f4;
 CLOSE;
end;

procedure TFGVFrage2.MyStdButton1Click(Sender: TObject);
begin
 TDialogs.mymessagedlg(
'~Adresszusammenfùhrung'#13+
'Etwas ganz feines.'#13+
'Das Programm versucht bei mehreren Schuldnern festzustellen, ob diese gemeinsame Anschriften und ggf. sogar gleiche Nachnamen haben. '#13#13+
'Wurden Gemeinsamkeiten festgestellt, bietet das Programm in Briefen und vielen anderen Programmteilen eine Zusammenfùhrung an. '+
'Vollkommen eigenstùndig versucht das Programm die Parteien zu gruppieren.'#13#13+
'Die Gruppierung macht natùrlich nur Sinn, wenn die Anschriften ùbereinstimmen.'#13#13+

//'Sind weder Namen noch Anschriften gleich, wird zumindest unter der 1. Anschrift ein gemeinsamer Adressat gebildet. Das wird nur in weniger Fùllen hilfreich, aber besser als neu eingeben.'#13#13+ 

'Bsp.:'#13+
'Schuldner 1 = Ise Mustermann, Schuldner 2 = Hans Mustermann. Das Programm bietet sodann neben den beiden Schuldnern eine gemeinsame Partei / einen Adressaten Ilse und Hans Mustermann an.'#13+
'Schuldner 1 = Mustermann, Schuldner 2 = Musterfrau, Anschrift jeweils Sonnenstr. 1.Das Programm bietet eine Partei Mustermann und Musterfrau, Sonnenstr. 1 an!',
 mtinformation,[mbok],0);
end;

procedure TFGVFrage2.MyStdButton2Click(Sender: TObject);
begin
 showhilfeidx(helpcontext);
end;

procedure TFGVFrage2.S1Click(Sender: TObject);
begin
 wgkey:=t_Sf1;
 CLOSE;
end;

procedure TFGVFrage2.S2Click(Sender: TObject);
begin
 wgkey:=t_Sf2;
 CLOSE;
end;

procedure TFGVFrage2.S3Click(Sender: TObject);
begin
 wgkey:=t_Sf3;
 CLOSE;
end;

procedure TFGVFrage2.HilfeClick(Sender: TObject);
begin
 TDialogs.MyMessageDlg('Fùr die Auswahl der Adressaten in Briefen (Glùubiger, Vertreter und Schuldner) versucht ein ùuùerst umfangreiches Analysemodul in Adressen "versteckte" Geschùftsfùhrer '+
              'oder fùr andere Personen handelnde Parteien zu erkennen. '#13#13+//10.08.17 Ebenfalls wird analysiert, ob mehr als eine Person ùber "3 Namen" oder "2 Namen" beim Kunden gespeichert sind.'#13#13+
              'Wenn mehrere Parteien gefunden wurden, werden in der Auswahl des Adressaten weitere Vorschlùge auf Tastendruck angeboten. Dadurch entfùllt die manuelle Korrektur oder die '+
              'Adressneueingabe bzw. Adresssuche, was eine enorme Steigerung des Komforts bedeutet.'#13#13+
              'Bei der Adressatermittlung wird auch die Anrede automatisch gesetzt, falls ein Vorname erkannt wurde. '+

              'U.a. wird fast immer erkannt, ob Parteien z.B. als "Hans Mustermann handelnd fùr Meier GmbH" oder als "Meier GmbH vertr. d.d. GF Hans Meier" erfasst wurden.'#13+

              'Wenn Sie z.B. "Mustermann GmbH vertr. d. d. GF Hans Meier" erfasst haben, '+
              'wird Ihnen fùr den Druck diese Adresse, "Mustermann GmbH", "Herrn Hans Meier" sowie "Herrn Hans Meier als gesetz. Vertr. d. Mustermann GmbH" vorgeschlagen!'#13+

              'Diese Erkennung ist nicht nur auf natùrliche und juristische Personen beschrùnkt. Vormundschaften werden ebenfalls meistens erkannt.'#13#13+
              'Diese Analyse funktioniert auch bei Schuldnern, falls mehrere Personen unter einem Schuldner gespeichert sind!',
              mtinformation,[mbok],0);
 TDialogs.MyMessageDlg('Einschrùnkungen: Das Programm versucht anhand von gewissen Formulierungen bei Firmen etc. die gesetzlichen Vertreter zu ermitteln, '+
              'was jedoch stark von der Art der Eingabe abhùngt. Unter anderem wird versucht, den Vornamen aus der Datenbank der vielen hundert Vornamen '+
              'zu filtern und ggf. den Genus hieraus zu erkennen. Daraus resultiert, dass die weiteren Schaltflùchen ggf. nicht den gewùnschten Inhalt haben. '+
              'Wegen der unzùhligen Eingabemùglichkeiten kùnnen nicht alle Arten der Formulierung erkannt werden. '+
              'Wenn Sie eine bislang nicht erkannt Formulierung gefunden haben, schreiben Sie uns einfach per E-Mail, Fax oder Post die EXAKTE Formulierung. '+
              'Wir versuchen dann, diese ebenfalls in die Erkennung aufzunehmen. '+
              'Ein Test mit mehreren hundert Adressen ergab jedoch eine Quote ùber 95%.'#13#13+

              //19.06.09:
              'FALLS die Adressaufsplittung bei Ihnen nicht funktioniert, erfassen Sie Adressen bitte demnùchst nach folgenden Beispielen:'#13+
              'Name: Mustermann GmbH'#13+
              'Langname: vertr. d.d. GF Hans Mustermann'#13'oder'#13+

              'Name: Ilse Musterfrau'#13+
              'Langname: als GF der Mustermann GmbH'#13'oder'#13+

              'Name: Ilse Musterfrau'#13+
              'Langname: als Betreuerin von Hans Mustermann'

              ,mtinformation,[mbok],0);
end;

procedure TFGVFrage2.S1_2Click(Sender: TObject);
begin
 wgkey:=t_0;
 CLOSE;
end;

procedure TFGVFrage2.S2_2Click(Sender: TObject);
begin
 wgkey:=t_1;
 CLOSE;
end;

procedure TFGVFrage2.S3_2Click(Sender: TObject);
begin
 wgkey:=t_2;
 CLOSE;
end;

procedure TFGVFrage2.S4Click(Sender: TObject);
begin
 wgkey:='x';
 CLOSE;
end;

procedure TFGVFrage2.T4_2Click(Sender: TObject);
begin
 wgkey:='y';
 CLOSE;
end;

procedure TFGVFrage2.BitBtn2Click(Sender: TObject);
var buff:nr_typ;
       T:TextFile;
    SpoolHeader : tSpoolHeader;   
begin
 IOResult;
 AssignFile(T,temppath+'INF.TXT');
 Rewrite(T);
 system.writeln(t,'Infos ùber eine Adressaufsplittung fùr die Programmierung.');
 system.writeln(t,'(Bitte ggf. weitere Hinweise/Bemerkungen hinzufùgen)');
 if Label1.Caption>'' then
  system.Writeln(T,'L1: '+Label1.Caption);
 if Label2.Caption>'' then
  system.Writeln(T,'L2: '+Label2.Caption);
 if KST.Caption>'' then
  system.Writeln(T,'KST: '+KST.Caption);
 if T1.Caption>'' then
  system.Writeln(T,'T1: '+T1.Caption);
 if T1_2.Caption>'' then
  system.Writeln(T,'T1_2: '+T1_2.Caption);
 if T2.Caption>'' then
  system.Writeln(T,'T2: '+T2.Caption);
 if T2_2.Caption>'' then
  system.Writeln(T,'T2_2: '+T2_2.Caption);
 if T3.Caption>'' then
  system.Writeln(T,'T3: '+T3.Caption);
 if T3_2.Caption>'' then
  system.Writeln(T,'T3_2: '+T3_2.Caption);
 if T4.Caption>'' then
  system.Writeln(T,'T4: '+T4.Caption);
 if T4_2.Caption>'' then
  system.Writeln(T,'T4_2: '+T4_2.Caption);
 CloseFile(T);
 IOResult;

 buff.dr :=0;
 buff.jahr:=0;
 InitSpoolHeader(SpoolHeader, eIgnore, 'Info ùber Adressaufsplittung fùr Programmierer', 0, 0 );//kein DR etc.
 Wemaile(temppath+'INF.TXT',Buff,False,
         'baque@gvinfo.de','Info ùber Adressaufsplittung fùr Programmierer',
         temppath+'INF.TXT',
         GetStandardHeader,
         FALSE,TRUE,false, SpoolHeader);
end;

procedure TFGVFrage2.FormResize(Sender: TObject);
var i:integer;
begin
 GridStd.ColWidths[0]:=Test.Canvas.TextWidth('Kostenschuldner..');
 GridStd.ColWidths[1]:=(GridStd.ClientWidth-GridStd.ColWidths[0]-GetScrollBarWidth-10) div 2;
 GridStd.ColWidths[2]:=GridStd.ColWidths[1];
 GridStd.ColWidths[3]:=0;
 GridStd.ColWidths[4]:=0;//AnschriftNr
 for i:=0 to 4 do
  GridAlt.ColWidths[i]:=GridStd.ColWidths[i];
end;

procedure TFGVFrage2.GridStdSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
 if arow = 0 then
  canselect := False
 else
 if acol = 0 then
  canselect := false
 else
  canselect := TRUE;
end;

procedure TFGVFrage2.GridAltSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
 if arow = 0 then
  canselect := False
 else
 if acol = 0 then
  canselect := false
 else
  canselect := TRUE;

{hilft nicht....
 if not(GridAlt.Focused) then
  CanSelect:=FALSE;}
end;

procedure TFGVFrage2.GridStdGetCellFormat(Sender: TObject; Col,
  Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
begin
 formatoptions.alignmentvert := taMultiLine;
 formatoptions.alignmenthorz := taLeftJustify;
end;

procedure TFGVFrage2.GridStdDblClick(Sender: TObject);
begin
//
 BitBtn8Click(Sender);
end;

procedure TFGVFrage2.GridAltDblClick(Sender: TObject);
begin
//
 BitBtn8Click(Sender);
end;

procedure TFGVFrage2.GetMultiS(ii:integer);//18.09.09:
var i:integer;
begin
// i:=1;
 if GetNextAdr(_DRI,_DR,Adresse,Schuldner,true) then begin
  For i:=2 to ii do
   GetNextAdr(_DRI,_DR,Adresse,Schuldner,false);
 end;
end;

procedure TFGVFrage2.GetMultiNew(C:AnsiChar;ii:integer);//16.03.17
var i:integer;
    Art:tAdressenArt;
begin
 case C of
  'G':Art:=Glaeubiger;
  'V':Art:=GVertreter;
  'S':Art:=Schuldner;
  'D':Art:=Schuldner3;
  'C':Art:=SchuldnerV;
  'K':Art:=_KostSchuld;
  else
   Art:=Unknown2;
 end;
 if GetNextAdr(_DRI,_DR,Adresse,Art,true) then begin
  For i:=2 to ii do
   GetNextAdr(_DRI,_DR,Adresse,Art,false);
 end;
end;

procedure TFGVFrage2.GetMultiS3(ii:integer);//18.09.09:
var i:integer;
begin
// i:=1;
 if GetNextAdr(_DRI,_DR,Adresse,Schuldner3,true) then begin
  For i:=2 to ii do
   GetNextAdr(_DRI,_DR,Adresse,Schuldner3,false);
 end;
end;

procedure TFGVFrage2.BitBtn8Click(Sender: TObject);   //OK 18.09.09
var i:integer;

 procedure TestAlt(GridStd:tUniSortGrid);
 begin
  if GridStd.Cells[3,GridStd.Row]='A1' then
   wgkey:=T_SF1
  else
  if GridStd.Cells[3,GridStd.Row]='A2' then
   wgkey:=T_SF2
  else
  if GridStd.Cells[3,GridStd.Row]='A3' then
   wgkey:=T_SF3
  else
  if GridStd.Cells[3,GridStd.Row]='A4' then
   wgkey:='x'
  else
  if GridStd.Cells[3,GridStd.Row]='A1_2' then
   wgkey:=T_0
  else
  if GridStd.Cells[3,GridStd.Row]='A2_2' then
   wgkey:=T_1
  else
  if GridStd.Cells[3,GridStd.Row]='A3_2' then
   wgkey:=T_2
  else
  if GridStd.Cells[3,GridStd.Row]='A4_2' then
   wgkey:='y'
  else
   ;

  Val(GridStd.Cells[4,GridStd.Row],AltAnschrift,dek.error);//8.02.15

  if wgkey>#0 then
   Close;
 end;

 var
 lEGVPAuftragAblehnen : IEGVPAuftragAblehnen;
 lHelpStr : String;

begin //BitBtn8Click
 try
  //erledigt ToDo: hmm Focus ist dann schon weg. Lùsung suchen
  if not(StdFocus) then begin //GridAlt.Focused then

   if GridALT.Cells[1,GridAlt.Row]='-keine-' then begin
    WFehler('Es gibt keine auswùhlbare alternative Adresse!',2);
    Exit;
   end;

   Case GridAlt.Cells[3,GridAlt.Row][1] of
    'A': TestAlt(GridAlt); //Alternative!
   end;
  end
  else begin

   //01.12.17:
   if length(GridStd.Cells[3,GridStd.Row])<1 then begin
    WFehler('Keine Partei gewùhlt',2);
   end
   else
   
   Case GridStd.Cells[3,GridStd.Row][1] of
    'A': TestAlt(GridStd); //Alternative!
    'X':begin //Weitere Parteien 16.03.17
          Val(Copy(GridStd.Cells[3,GridStd.Row],3,4),i,dek.error);
          GetMultiNew(AnsiChar(GridStd.Cells[3,GridStd.Row][2]),i);
          Val(GridStd.Cells[4,GridStd.Row],AltAnschrift,dek.error);//8.02.15
          wgkey:='~';
          Close;
         end;
    'S': begin //Weitere Parteien S
          Val(Copy(GridStd.Cells[3,GridStd.Row],2,4),i,dek.error);
          GetMultiS(i);
          Val(GridStd.Cells[4,GridStd.Row],AltAnschrift,dek.error);//8.02.15
          wgkey:='~';
          Close;
         end;
    'D': begin //Weitere Parteien D3
          Val(Copy(GridStd.Cells[3,GridStd.Row],2,4),i,dek.error);
          GetMultiS3(i);
          Val(GridStd.Cells[4,GridStd.Row],AltAnschrift,dek.error);//8.02.15
          wgkey:='~';
          Close;
         end;
    'G': begin
          wgkey:=t_f6;//22.02.15
          close;
         end;
    'H': begin
          wgkey:=t_f7;//24.02.15
          close;
         end;
    'J': begin
          wgkey:=t_f8;//05.09.17
          close;
         end;
    'F': begin
          Case GridStd.Cells[3,GridStd.Row][2] of
           '1':wgkey:=T_F1;
           '2':wgkey:=T_F2;
           '3':wgkey:=T_F3;
           '4':wgkey:=T_F4;
           '5':wgkey:=T_F5;//kein Problem mit Taschenrechner...
          end;
          Val(GridStd.Cells[4,GridStd.Row],AltAnschrift,dek.error);//8.02.15
          Close;
         end;
    '?' : begin
            if TEGVPAuftragAblehnenfactory.InUse(lEGVPAuftragAblehnen) then
              begin
                lHelpStr := GridStd.Cells[3,GridStd.Row];
                lHelpStr := Copy(lHelpStr,2);
                Dek.Adresse := lEGVPAuftragAblehnen.GetEditor(StrToInt(lHelpStr));
                wgkey := '~';
              end;
            Close;
          end;
   end;

  end;
 except
  on EXC_:Exception do
   ExceptionHintUser0('03137',EXC_.Message,EXC_.ClassName);
 end;
end;

procedure TFGVFrage2.GridStdClick(Sender: TObject);//18.09.09
var i:integer;

 Function Teste(Var A,A1,A2,A3, A1_2,A2_2,A3_2, A4, A4_2:AdressenTyp;ButtonText:ShortString):Boolean;//09.06.09
 var yy2,ii:integer;
 begin
  Result:=TesteMultiAdr(A,A1,A2,A3, A1_2,A2_2,A3_2, A4, A4_2);
  try
   GridAlt.RowCount:=0;
   AddStd('&N',A1,  GridAlt,'A1');
   AddStd('&P',A2,  GridAlt,'A2');
   AddStd('&Q',A3,  GridAlt,'A3');
   AddStd('&X',A4,  GridAlt,'A4');//13.02.19 AddStd('&R',A4,  GridAlt,'A4');
   AddStd('&S',A1_2,GridAlt,'A1_2');
   AddStd('&T',A2_2,GridAlt,'A2_2');
   AddStd('&U',A3_2,GridAlt,'A3_2');
   AddStd('&V',A4_2,GridAlt,'A4_2');
   OwnLabel2.Enabled:=(GridAlt.RowCount>1);

   //28.10.12:
   if (GridAlt.RowCount>1) then begin // GridAlt.Visible then begin
    yy2:=ownlabel2.height+4;
    for ii := 0 to GridAlt.RowCount - 1 do
     yy2:=yy2+GridAlt.RowHeights[ii]+2;
    if yy2>340 then
     yy2:=340;
    Panel1.Height:=yy2;
    Panel1.Visible:=TRUE;
   end
   else
    Panel1.Visible:=FALSE;

  except
   on EXC_:Exception do
    ExceptionHintUser0('03138',EXC_.Message,EXC_.ClassName);
  end;
 end;

begin //Erzeuge alternative Adressen:
 try
  if GridStd.Row<=0 then
   Exit;
  Fillchar(Adresse,Sizeof(Adresse),0);
  Case GridStd.Cells[3,GridStd.Row][1] of
    'X':begin //Weitere Parteien 16.03.17
          Val(Copy(GridStd.Cells[3,GridStd.Row],3,4),i,dek.error);
          GetMultiNew(AnsiChar(GridStd.Cells[3,GridStd.Row][2]),i);
         end;
    'S': begin //Weitere Parteien S
          Val(Copy(GridStd.Cells[3,GridStd.Row],2,4),i,dek.error);
          GetMultiS(i);
         end;
    'D': begin //Weitere Parteien DS
          Val(Copy(GridStd.Cells[3,GridStd.Row],2,4),i,dek.error);
          GetMultiS3(i);
         end;
    'F': begin
          Case GridStd.Cells[3,GridStd.Row][2] of
           '1':Adresse := _GV;
           '2':Adresse := _G;
           '4':Adresse := _KS;
           '5':Adresse := _Betreuer;//18.08.14
          end;
         end;
  end;
  Teste(Adresse,A1,A2,A3,
                A1_2,A2_2,A3_2,
                A4,A4_2,
                '');//16.03.17 'Schuld.');       
 except
  on EXC_:Exception do
   ExceptionHintUser0('03139',EXC_.Message,EXC_.ClassName);
 end;
end;

//****************
//**************** Es wùre so viel besser, die Keys zu speichern, anstatt dùmlich die ganzen Marker!!!!! Zumindest bei den nicht geùnderten Parteien aus Anschriftenzusammenfùhrung etc.
//**************** 
Procedure tfgvfrage2.AddStd(Head:ShortString; A:AdressenTyp;Grid:tUniSortGrid;Col3:ShortString);//18.09.09  // FDXE_S
//var i:integer;

 procedure sub(Col3:Shortstring;AnschriftNr:Integer);
 var //aa:integer;
      s:widestring;//a nsistring;
begin
 try
  if A.NameUTF='' then
   Exit;
  begin
   if Grid.RowCount<=1 then begin
    Grid.RowCount     :=2;
    Grid.FixedRows    :=1;
    Grid.Cells[0,0]   :='';
    Grid.Cells[1,0]   :='Name';
    Grid.Cells[2,0]   :='Anschrift';
    Grid.Cells[3,0]   :='';
    Grid.ColWidths[3] :=0;
    Grid.RowHeights[0]:=Test.Canvas.TextHeight('S')+2;
   end
   else
    Grid.RowCount:=Grid.RowCount+1;

   //21.09.09:
   if Grid = GridStd then begin
    Grid.Cells[0,Grid.RowCount-1]:=(Head);
    if (Pos('&',Head)=0) and (Accel < '9') then begin
     Grid.Cells[0,Grid.RowCount-1]:=Grid.Cells[0,Grid.RowCount-1]+' (&'+Accel+')';
     Accel:=AnsiChar(Ord(Accel)+1); // FDXE_C
    end;
   end
   else

    Grid.Cells[0,Grid.RowCount-1]:=(Head);


   s:=trim(A.VornameUTF);
   if (s>'') and ((A.Anrede>'') or (A.TitelUTF>'')) then
    s:=s+' '+crunch(A.Anrede+' '+A.TitelUTF);
   Grid.Cells[1,Grid.RowCount-1]:=Crunch(A.NamensvorsatzUTF+' '+A.NameUTF+' '+A.NamenszusatzUTF)+#13+
                                   s;
    //24.02.15 doch lieber manuell: GetN ameLineLongNameVoranCanvas(A,Grid.Canvas,Grid.ColWidths[1],aa,Grid.RowCount);

    //19.02.15:
    if AnschriftNr=-1 then
     Grid.Cells[2,Grid.RowCount-1]:={GetAnschriftenArt(A,-1)+':'#13+}GetAdrOnlyLineLong(A)
    else
     Grid.Cells[2,Grid.RowCount-1]:={GetAnschriftenArt(A,AnschriftNr)+':'#13+}GetAdrOnlyLineLong(A,AnschriftNr);

    Grid.Cells[3,Grid.RowCount-1]:=Col3;

    Grid.Cells[4,Grid.RowCount-1]:=inttostr(AnschriftNr);

   end;
  except
  on EXC_:Exception do
   ExceptionHintUser0('03140',EXC_.Message,EXC_.ClassName);
  end;
 end;

begin
 Sub(Col3,-1);
{ //Weitere Anschriften:
 for i := 0 to high(A.WeitereAnschrift) do
  Sub(Col3,i);}
end;

procedure TFGVFrage2.GridAltEnter(Sender: TObject);
begin
 StdFocus:=FALSE;//21.09.09
//fùhrt zu Chaos GridALT.DefaultDrawing:=TRUE;//22.09.09
end;

procedure TFGVFrage2.GridStdEnter(Sender: TObject);
begin
 StdFocus:=TRUE;//21.09.09
end;

procedure TFGVFrage2.GridAltGetCellFormat(Sender: TObject; Col,
  Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
begin
 formatoptions.alignmentvert := taMultiLine;
 formatoptions.alignmenthorz := taLeftJustify;
end;

procedure TFGVFrage2.GridAltDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
//var where:trect;
begin
(*23.08.17
 //22.09.09:
 GridAlt.Font.Assign(GridStd.Font);
 GridAlt.Canvas.Font.Assign(GridStd.Canvas.Font);

 where := rect;
 with GridAlt.Canvas do begin
  Font := Self.Font;

  if not(StdFocus) and
     (gdSelected in State) {and
     not(gdFocused in State)} then begin
   Brush.Color := clblue;//28.10.12 clHighlight;
   Font.Color  := clwhite;//28.10.12 clHighlightText;
  end
  else
   Brush.Color := $00CBFAFE;//28.10.12 clWindow;
  FillRect(Where);
 end;

 GridAlt.MyDrawCell(ACol, ARow, Rect, State);*)
end;

procedure tfgvfrage2.DrawSpecial;
begin
end;

procedure TFGVFrage2.GridAltExit(Sender: TObject);
begin
//fùhrt zu Chaos GridALT.DefaultDrawing:=FALSE;//22.09.09
end;

end.
