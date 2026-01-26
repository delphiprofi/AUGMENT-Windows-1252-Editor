unit DRV;

{$IFDEF FRANK} // DRVTest - inaktive
{.$DEFINE DRVTest} //VORSICHT, IST NUR FùR TESTS!!!!!
{$ENDIF}

interface

Uses
  Dialogs
, dek
, wXJustizKundeneingabe
;

Function  GetDRVTestData : AnsiString; // FL 12.07.20

 //17.02.16:
Procedure Anfrage_MRA(f755802l1:Tf755802l;A:tAnfrage;
                      AGSEdit_:shortstring;//28.02.16
                      var Res:Ansistring//29.02.16
                     );

Procedure Einmal_RegistrierungKBANeu(Filename,Kundennummer,Benutzerkennung,KennwortNeu : AnsiString);

implementation

Uses Windows,Sysutils,xmldom, XMLIntf, msxmldom, XMLDoc, StdCtrls,
     XSBuiltIns, wleiste, wbasis, basis,
     kopfstellekba, eAktenSystem.Intf, eAktenSystem.D2007
, uxJustizVersion
, XML_Defs                   // FL 04.10.22
{$IFDEF FRANK]}
, XMLCheck
{$ENDIF}
, UCodeType
, winsys
, spooler
, wspooler
, xJustiz_GVS_EnumHandler.Intf // FL 28.10.22
, xJustiz_XML_Handler          // FL 02.11.22
, xj_SchriftgutObjectHandler   // FL 06.09.22
, Dialogs.Intf                 // FL 12.02.25
;

Function GetDRVTestData : AnsiString;
begin
  {$IFDEF DRVTest}
  Result := TestFall[TestFallIndex];

  Inc(TestFallIndex);
  if TestFallIndex > high(TestFall) then
    begin
      TestFallIndex := 0;
      TDialogs.MyMessageDlgOK('Das ist der letzte TestDatensatz, danach starten wir wieder beim 1.',mtWarning,[mbOK],0)
    end;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

Procedure MakeNIL(Node : IXMLNode;S:ShortString);
begin
  With Node.AddChild(S) do
    setAttribute('xsi:nil', 'true');
end;

function DoCreateGUID: String;
var
  GUID : TGUID;
begin
  CreateGUID(GUID);
  SetLength(Result, 36);
  StrLFmt(PChar(Result), 38,'%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',[GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;

Procedure Einmal_RegistrierungKBANeu(Filename,Kundennummer,Benutzerkennung,KennwortNeu : AnsiString);
var
  myDOC : IXMLNachricht_Kopfstelle_BenutzerRegistrieren;

  Function FUCK_Reihenfolge(kette,ID : String):String; // FL 22.05.14
  begin
    Result := StringReplace(kette,'<Benutzer','<Nachricht_ID>'+ID+'</Nachricht_ID>'+#10+'<Benutzer',[]);
  end;

  //29.11.17 aus altem Src kopiert
 function  Encode64(S: Ansistring): Ansistring;
 const Codes64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
 var
    i: Integer;
    a: Integer;
    x: Integer;
    b: Integer;
    Count : Integer;
 begin
    Result := '';
    a := 0;
    b := 0;
    Count := length(S);
    for i := 1 to Count do
    begin
      x := Ord(s[i]);
      b := b * 256 + x;
      a := a + 8;
      while a >= 6 do
      begin
        a := a - 6;
        x := b div (1 shl a);
        b := b mod (1 shl a);
        Result := Result + Codes64[x + 1];                                                          { TODO 2 -oXEFrank -cS_2_Ansi : XE }
      end;
    end;
    if a > 0 then
    begin
      x := b shl (6 - a);
      Result := Result + Codes64[x + 1];                                                            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    end;
    case Count mod 3 of
      1 : Result := Result + '==';
      2 : Result := Result + '=';
    end; // of case
 end;

begin
  leiste.XMLDocument3.XML.Text := ''; // FL 27.05.14 Shitt Global!
  leiste.XMLDocument3.Active := true;

  MyDoc := leiste.XMLDocument3.GetDocBinding('Nachricht_Kopfstelle_BenutzerRegistrieren',TXMLNachricht_Kopfstelle_BenutzerRegistrieren) as IXMLNachricht_Kopfstelle_BenutzerRegistrieren;

  // Unnùtzer Shittheader
  leiste.XMLDocument3.Encoding := 'UTF-8';
  MyDoc.DeclareNamespace('xsi','http://www.w3.org/2001/XMLSchema-instance');
  leiste.XMLDocument3.DocumentElement.DeclareNamespace('','http://it.nrw.de/fremdauskunft');
  leiste.XMLDocument3.DocumentElement.Attributes['xsi:schemaLocation'] := 'http://it.nrw.de/fremdauskunft Kopfstelle-kba.xsd';

  MyDoc.Benutzer.Auskunftsstelle := 'KBA';
  {$IFDEF DRVTest}
// 22.05.14  MyDoc.Nachricht_ID             := TDialogs.MyMessageID;    // FL 22.05.14 nach vorne gesetzt
  MyDoc.Benutzer.Kundennummer    := 'Testnutzer GVINFO';
  MyDoc.Benutzer.Benutzerkennung := 'ITNRWTEST6'; /// User ID?
  MyDoc.Benutzer.Kennwort        := Encode64('CB48._MPq605');
  {$ELSE}
// 22.05.14  MyDoc.Nachricht_ID             := TDialogs.MyMessageID;    // FL 22.05.14 nach vorne gesetzt
  MyDoc.Benutzer.Kundennummer    := Kundennummer;
  MyDoc.Benutzer.Benutzerkennung := Benutzerkennung;
//29.11.17  MyDoc.Benutzer.Kennwort        := KennWortNeu;//04.08.17 Encod e64(Kennwort);
  MyDoc.Benutzer.Kennwort        := Encode64(KennwortNeu);//29.11.17
  {$ENDIF}

  leiste.XMLDocument3.XML.Text := Fuck_Reihenfolge(leiste.XMLDocument3.XML.Text,MyMessageID); //FL 22.05.14
  leiste.XMLDocument3.Active := true;

  leiste.XMLDocument3.SaveToFile(Filename);

  leiste.XMLDocument3.Active := false;
  Afterpatch(Filename,PatchDRV); // FL 15.02.16
end;

 //17.02.16:
Procedure Anfrage_MRA(f755802l1:Tf755802l;A:tAnfrage;
                      AGSEdit_:shortstring;//28.02.16
                      var Res:Ansistring//29.02.16
                     );
 const XML='xmeld_anfrage.xml';
 var betreff, s:shortstring;

  function SgO4MRA(Const aFilename,aDocname,aTargetSafeID,aDRNr : String;out Error : String) : boolean; // FL 06.09.22
  var
    lSgOWriter      : ISchriftgutObjectWriter;
    lMySafeID       : String;
  begin
    Result := true;
    try
      lSgOWriter := TSchriftgutObjectWriter.Create(aFilename,NIL);

      if lSgOWriter.GetSafeID(lMySafeID)
        then begin
               lSgOWriter.NachrichtenKopf(TGVSHandler.DoCreateGUID32,'','',lMySafeID,aTargetSafeID,'')
                         .Sachgebiet(cSachgebietMRA,'XMeld Anfrage',cInstancebehoerde,aDRNr) // FL 21.10.22 Sachgebiet fùr MRA FL 06.09.22
                         .AddDokument(dkSonstige,aDocName)
             end
        else begin
               Result := false;
               Error  := 'Eigenes SafeID nicht gefunden! Bitte ùberprùfen Sie Ihre EGVP/eBO Einstellungen!';
             end;
    except
      Error := 'SchriftgutObjekt konnte nicht erstellt werden!';
      Result := false;
    end;
  end;

 var lError : String;
     SpoolHeader : tSpoolHeader;
 begin
  Res:='';
  try
   f755802l1.AnfrageXMeld(NoSignatur {tSignatur(ZenVGTransfer.itemindex)},TempPath+XML,f755802l1.DR_,F755802l1.LocalAdr{hmmm},A.NachrichtenBin,FALSE,  AGSEdit_);

   if FileExists(TempPath+XML) then begin

    //auf keinen Fall f755802l1.SaveAdresse(true);

    DirektEGVP:=false;
    egvpoverride:=-1;
    GlobalOeffneSPOOLERInital:='';

    {$IFDEF DRVTest}
    s:=cTest_XMeldSafeID;
    betreff := cXMeldAnfrage+' an TESTSERVER'; // FL 15.04.22
    {$ELSE}
    //01.03.16:
// BW 14.11.22 nicht nur fùr User_Status=0    if (User_STatus=0) and (f755802l1.Combo1.Visible) and (f755802l1.Combo1.ItemIndex=10) then begin
    if Assigned(f755802l1) and (f755802l1.Combo1.Visible) and (f755802l1.Combo1.ItemIndex=10) then begin
     s:=cTest_XMeldSafeID;                                                                          { TODO 2 -oXEFrank -cS_2_Short : XE }
     betreff := cXMeldAnfrage+' an TESTSERVER'; // FL 15.04.22
    end
    else begin
     s:=cXMeldSafeID;                                                                               { TODO 2 -oXEFrank -cS_2_Short : XE }
     betreff := cXMeldAnfrage; // FL 15.04.22
    end;
    {$ENDIF}

    if SgO4MRA(temppath+'xjustiz_nachricht.xml',XML,s,trim(DRToString(f755802l1.DR_)),lError) // FL 06.09.22
      then begin
        InitSpoolHeader(SpoolHeader, eDRII, betreff, f755802l1.DR_.DR, f755802l1.DR_.Jahr );
        if EGVP_MakeMail(true,s,
                    betreff,
                    '',//Body
                    DRToString(f755802l1.DR_),//Az Abs
                    '',//Az Empf
                    NoSignatur{winsys.tSignatur(ComboBox5.ItemIndex)},
                    TempPath+XML+ ';' + temppath+'xjustiz_nachricht.xml', // FL 06.09.22
                    EGVPMsgTESTONLY,FALSE,f755802l1.DR_,false,
                    false,DLeer,NIL,SpoolHeader) then begin

    {spùter
         //Statistik:
         Inc(AnzMRA);
         WriteStammDaten;}

         if F755802l1.MultiAGS.Checked then
          Res:=f755802l1.MRAInfo+' an EGVP / eBO ùbergeben'
         else
          Res:='An EGVP / eBO ùbergeben!';
        end;
       end
       else begin
        WFehler('!KEINE XMeld-Nachricht!',2); // FL 28.02.16 XMeld!
        Res:='!KEINE XMeld-Nachricht!';
       end;
   end else TDialogs.MyMessageDLG('SchriftgutObject konnte nicht erzeugt werden!'+#1313+lError,mtinformation,[mbok],0);
  except
   on EXC_:Exception do
    ExceptionHintUser0('00421',EXC_.Message,EXC_.ClassName);
  end;
 end;

end.
