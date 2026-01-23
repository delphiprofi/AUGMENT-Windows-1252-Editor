unit ZUHandler;

interface

Uses
  Classes
, Windows
, SysUtils
, Controls
, Dialogs
, Dek
, wBasis
, wmsgdlg
, LoadStd
, RefStream
, GVSToXML.Intf
, GVSToXML
, Zustell
, ProtoEin
, PostZU
, BLCOMPacker
, StreamPacker
, Utilities
, ZUHandler.Intf
, Spooler
, WSpooler
, XECOMP
, ForM  // ja mit M
, wFormularDruck
, WHistorie
, WinSys
, Spez2 // NUR fùr DelFile
, ZipMitPW // BW 14.08.23
, SpoolerHandler
, Feature // BW 18.09.23
, EGVP.Intf  // BW 05.10.23
, EGVP.MakeMail  // BW 05.10.23
, Classes.Helper // BW 06.1023
, DRUtil // BW 09.10.23
, uIndividualLogbuch // BW 25.10.23
, DipHandler.Intf // BW 31.10.23
, Dialogs.Intf
, StrUtils
;

Const
  cZustelldataVersion : Byte = 1;
  cMultiDipEZU        = 27; // Index in Dips.pas
  cZUExt              = '.zup';

Type
  TZuType = (ztUndefined,ztpersoenlich,ztPostZU,ztPostZuPfueb); //,eZU);

  TZustellData = Record
    ZuType        : TZuType;
    Titel         : AnsiString;
    Az            : AnsiString;
    Date          : Datumstyp;
    SchriftStùck  : SchriftstueckArt;
    Aufkleber1,
    Aufkleber2    : boolean;
    Bezirksnummer : AnsiString;

    Constructor Create(aZUType : TZuType;Const aTitel:AnsiString);overload;
    Constructor Create(aZUType : TZuType;Const aTitel:AnsiString;aDate : Datumstyp;aSchriftStùck:SchriftstueckArt;Const aAz : AnsiString;aAufkleber1,aAufkleber2 : Boolean;Const aBezirksnummer : AnsiString);overload;
  end;

  PSchuldnerZU = ^TSchuldnerZU;
  TSchuldnerZU = Record
                   Schuldner   : pAdressenTyp;
                   ZustellData : TZustellData;

                   Procedure SaveToStream(Const S : TStream);
                   Procedure LoadFromStream(Const S : TStream);

                   Constructor Create(aSchuldner : pAdressentyp;Const aZustellData : TZustellData);
                 end;

  ISchuldnerZUHandler = Interface
    ['{536E5352-67C0-4876-B763-D96DCC685755}']
    procedure AddSchuldner(aSchuldner : pAdressenTyp;Const aZustellData : TZustellData);
    function  Inform_LaterPrint(Const aZuInfoText : String) : ISchuldnerZUHandler;
    function  Count : Integer;
    Procedure SaveForID(Const aNID : String);
    Procedure LoadForID(Const aNID : String);
    Procedure SaveToStream(aStream : TStream);
    Procedure LoadFromStream(aStream : TStream);

    Procedure PrintZUs;
  end;

  TGlobalZUInfoCollector = Class abstract
    public
      Class Function Create : ISchuldnerZUHandler;
      Class Function HasAusdruckeForSgO(Const aNID : String) : boolean;
  end;

  TZUHandler = Class
    private
      fIs840er   : boolean;
      fDIP       : Byte;
      fDR        : Nr_Typ;
      fDRI       : Boolean;
      fDRIEntry  : PDR_I_typs;
      fBriefName : String;
      fFilename  : String;
      fPruef     : String; // BW 14.08.23
      fDate      : String; // BW 14.08.23

//      function  CollectSaved(const aGVSToXML: IGVSToXML): boolean;
//      function  CreateCollectName: String;
      function  CreateGVSToXML: IGVSToXML;
      procedure DeInitAll;
      procedure DoFormBrief;
      procedure InitAll;
      Function  MergeZU(const aGVSToXML: IGVSToXML) : boolean;

      Procedure SetFileName;

    public
      Procedure PrintEZU;

      Class Procedure DoEZU(const aBriefName : String; aIs840er : boolean; aDR : Nr_Typ; aDRI : Boolean; aDRIEntry : PDR_I_typs);
      Class Procedure DoGleichzeitige;
  end;

//    Procedure SaveTempInfo(Const aFilename : String;aGVSToXML : IGVSToXML);
//    Function  HasTempZU(Const aFilename : String;Out GVS : IGVSToXML) : boolean;


Var
  GlobalZUInfoCollector : ISchuldnerZUHandler; // FL OMG Global!

implementation

uses eAktenSystem.Intf, eAktenSystem.D2007;

Type
  TSchuldnerZUHandler = Class(TInterfacedObject,ISchuldnerZUHandler)
    private // ISchuldnerZUHandler
      procedure AddSchuldner(aSchuldner : pAdressenTyp;Const aZustellData : TZustellData);
      function  Inform_LaterPrint(Const aZuInfoText : String) : ISchuldnerZUHandler;
      function  Count : Integer;
      Procedure SaveForID(Const aNID : String);
      Procedure LoadForID(Const aNID : String);
      Procedure SaveToStream(aStream : TStream);
      Procedure LoadFromStream(aStream : TStream);
      Procedure PrintZUs;
    private
      fIndex     : Integer;
      fSchuldner : array of TSchuldnerZU;

      procedure Reset;
      function  NextZU(out ZUInfo : PSchuldnerZU) : boolean;
      function  PrintPersùnlich(aZUInfo : PSchuldnerZU) : boolean;
      function  PrintPost(aZUInfo : PSchuldnerZU) : boolean;
    public
      constructor Create;
      destructor  Destroy;override;
  end;

  PGleichzeitigPair = ^TGleichzeitigPair;
  TGleichzeitigPair = record
    DR       : MultiNr_Typ;
    HistText : String;
    Dokumente : array of WideString;

    Procedure AddDocument(const aDocument : WideString);
    Function  GetDocuments(const aDelim : String = ';') : WideString;
    Procedure SaveAllDocumentsToZip(const aZIP : IMemZipPW);
    Function  GetBezeichner : WideString;
  end;

  TGleichzeitigPairs = record
    fPairs : array of TGleichzeitigPair;

    Procedure Add(aDR : MultiNr_Typ; const aHistText : String; const aDokument : WideString);
    Function  Get(aIndex : integer) : PGleichzeitigPair;
    Function  Count : integer;
  end;

{ TSchuldnerZU }

constructor TSchuldnerZU.Create(aSchuldner: pAdressentyp;Const aZustellData : TZustellData);
begin
  New(Schuldner);
  Move(aSchuldner^,Schuldner^,Sizeof(Schuldner^));
  ZustellData := aZustellData;
end;

procedure TSchuldnerZU.LoadFromStream(const S: TStream);
var
  B : Byte;
begin
  if Schuldner = NIL then
    New(Schuldner);

  TAllTypeStream.Lese(S,B);
  TAllTypeStream.Lese(S,Schuldner^,Sizeof(Schuldner^));
  TAllTypeStream.Lese(S,B);
  ZustellData.ZuType := TZuType(B);
  TAllTypeStream.Lese(S,ZustellData.Titel);
  TAllTypeStream.Lese(S,ZustellData.Az);
  TAllTypeStream.Lese(S,ZustellData.Date,Sizeof(ZustellData.Date));
  TAllTypeStream.Lese(S,B);
  ZustellData.SchriftStùck := SchriftstueckArt(B);
  TAllTypeStream.Lese(S,ZustellData.Aufkleber1);
  TAllTypeStream.Lese(S,ZustellData.Aufkleber2);
  TAllTypeStream.Lese(S,ZustellData.Bezirksnummer);
end;

procedure TSchuldnerZU.SaveToStream(const S: TStream);
var
  B : Byte;
begin
  TAllTypeStream.Schreibe(S,cZustelldataVersion);
  TAllTypeStream.Schreibe(S,Schuldner^,Sizeof(Schuldner^));
  B := Byte(ZustellData.ZuType);
  TAllTypeStream.Schreibe(S,B);
  TAllTypeStream.Schreibe(S,ZustellData.Titel);
  TAllTypeStream.Schreibe(S,ZustellData.Az);
  TAllTypeStream.Schreibe(S,ZustellData.Date,Sizeof(ZustellData.Date));
  B := Byte(ZustellData.SchriftStùck);
  TAllTypeStream.Schreibe(S,B);
  TAllTypeStream.Schreibe(S,ZustellData.Aufkleber1);
  TAllTypeStream.Schreibe(S,ZustellData.Aufkleber2);
  TAllTypeStream.Schreibe(S,ZustellData.Bezirksnummer);
end;

{ TZustellData }

constructor TZustellData.Create(aZUType: TZuType; const aTitel: AnsiString);
var
  lDate : Datumstyp;
begin
  Fillchar(lDate,Sizeof(lDate),0);

  Create(aZUType,aTitel,lDate,eDokument,'',false,false,'');
end;

constructor TZustellData.Create(aZUType: TZuType; const aTitel: AnsiString; aDate: Datumstyp; aSchriftStùck: SchriftstueckArt; const aAz: AnsiString; aAufkleber1, aAufkleber2: Boolean;Const aBezirksnummer : AnsiString);
begin
  ZUType        := aZUType;
  Titel         := aTitel;
  Date          := aDate;
  SchriftStùck  := aSchriftStùck;
  Az            := aAz;
  Aufkleber1    := aAufkleber1;
  Aufkleber2    := aAufkleber2;
  Bezirksnummer := aBezirksnummer;
end;

{ TSchuldnerZUHandler }

function TSchuldnerZUHandler.Count: Integer;
begin
  Result := Length(fSchuldner);
end;

constructor TSchuldnerZUHandler.Create;
begin
  Inherited Create;

  fIndex := -1;
  Setlength(fSchuldner,0);
end;

destructor TSchuldnerZUHandler.Destroy;
var
  i : Integer;
begin
  for i:=0 to high(fSchuldner) do
    Dispose(fSchuldner[i].Schuldner);

  inherited;
end;

function TSchuldnerZUHandler.Inform_LaterPrint(Const aZuInfoText : String) : ISchuldnerZUHandler;
begin
  Result := Self;

  if MultiDIP[cMultiDipEZU] = 0 then
    begin
      try
        if TDialogs.AddCustomButton('&Information nicht mehr Anzeigen!',mbAbort)
                   .MyMessageDlg('Der Ausdruck der '+aZuInfoText+' erfolgt nach der Verarbeitung der EGVP/eBO Eingangsbestùtigung!',mtInformation,[mbOK,mbAbort],0) = mrAbort then
          begin
            MultiDIP[cMultiDipEZU] := 1;
            WriteDips;
          end;
      except // Finally
      end;
      CustomButtonCount:=0;
      fillchar(CustomButtons,Sizeof(CustomButtons),0);
    end;
end;

procedure TSchuldnerZUHandler.AddSchuldner(aSchuldner: pAdressenTyp;Const aZustellData : TZustellData);
begin
  Setlength(fSchuldner,length(fSchuldner)+1);
  fSchuldner[High(fSchuldner)] := TSchuldnerZU.Create(aSchuldner,aZustellData);
end;

function TSchuldnerZUHandler.NextZU(out ZUInfo: PSchuldnerZU): boolean;
begin
  inc(fIndex);

  if fIndex <= High(fSchuldner)
    then begin
           ZUInfo := @fSchuldner[fIndex];
           Result := true;
         end
    else Result := false;
end;

procedure TSchuldnerZUHandler.Reset;
begin
  fIndex := -1;
end;

procedure TSchuldnerZUHandler.SaveForID(const aNID : String);
var
  lData  : IDataStream;
begin
  lData := TDataStream.Create;

  SaveToStream(lData.Stream);

  TBLCOMPacker.AsPackedStream(lData).ToFile(EGVPAblage+aNID+cZUExt);
end;

procedure TSchuldnerZUHandler.SaveToStream(aStream: TStream);
Var
  lCount : Int64;
  i      : Integer;
begin
  lCount := Length(fSchuldner);
  TAllTypeStream.Schreibe(aStream,lCount);

  for i:=0 to High(fSchuldner) do
    fSchuldner[i].SaveToStream(aStream);
end;

procedure TSchuldnerZUHandler.LoadFromStream(aStream: TStream);
Var
  lCount : Int64;
  i      : Integer;
begin
  TAllTypeStream.Lese(aStream,lCount);
  Setlength(fSchuldner,lCount);

  for i:=0 to High(fSchuldner) do
    fSchuldner[i].LoadFromStream(aStream);
end;

procedure TSchuldnerZUHandler.LoadForID(const aNID : String);
var
  lData  : IDataStream;
begin
  lData := TDataStream.Create;
  lData.Stream.LoadFromFile(EGVPAblage+aNID+cZUExt);
  lData.Stream.Position := 0;

  TBLCOMPacker.EntpackeDataStream(lData);

  LoadFromStream(lData.Stream);
end;

function TSchuldnerZUHandler.PrintPersùnlich(aZUInfo : PSchuldnerZU) : boolean;
begin
  Result := false;
  try
//    if aIsDRI
//      then DRIZustellPers(aDR,TGVSToXML.DRI_OffsetFromNr(aDR),1,TGVSToXML.CheckEZU(aZUInfo.Schuldner))
//      else DRIIZustellungen(aDR,DR_II.Titel[1].D6.Titel,DR_II.Titel[1],DR_II.Titel[1].ZustellUrkunde,'',aZUInfo.Schuldner^,5{pers},TRUE,'' ,false); //DruckePers( A1+' '+A2 )
  except
    Result := false;
  end;
end;

function TSchuldnerZUHandler.PrintPost(aZUInfo : PSchuldnerZU) : boolean;
begin
  Result := false;
  try
//    DruckeZU_POST(aParteien.Glùubiger^,
//         aZUInfo^.Schuldner^,
//         aParteien.GlùubigerVertreter^,
//         aDR,aIsDRI,FALSE,FALSE,
//         aZUInfo^.ZustellData.Titel,'',
//         aZUInfo^.ZustellData.Date,
//         aZUInfo^.ZustellData.Schriftstùck,
//         aZUInfo^.ZustellData.Az,false,
//        (aZUInfo^.ZustellData.ZuType = PostZuPfueb),
//         aZUInfo^.ZustellData.Aufkleber1,
//         aZUInfo^.ZustellData.Aufkleber1,
//         aZUInfo^.ZustellData.Bezirksnummer);
  except
    Result := false;
  end;
end;


procedure TSchuldnerZUHandler.PrintZUs;
var
 lZUInfo : PSchuldnerZU;
begin
  Reset;

  while NextZU(lZUInfo) do
    begin
      case lZUInfo^.ZustellData.ZuType of
        ztpersoenlich  : PrintPersùnlich(lZUInfo);
        ztPostZU,
        ztPostZuPfueb  : PrintPost(lZUInfo);
      end; // of case
    end;
end;

{ TGlobalZUInfoCollector }

class function TGlobalZUInfoCollector.Create: ISchuldnerZUHandler;
begin
  if cUseZUCollector
    then Result := TSchuldnerZUHandler.Create
    else Result := NIL;
end;

class function TGlobalZUInfoCollector.HasAusdruckeForSgO(const aNID : String): boolean;
begin
  Result := FileExists(EGVPAblage+aNID+cZUExt);
end;

{$WARN GARBAGE OFF}

{ TZUHandler }

class procedure TZUHandler.DoEZU(const aBriefName: String; aIs840er: boolean; aDR: Nr_Typ; aDRI: Boolean; aDRIEntry: PDR_I_typs);
Var
  lHandler : TZUHandler;
begin
  lHandler := TZUHandler.Create;
  try
    lHandler.fBriefName := aBriefName;
    lHandler.fIs840er   := aIs840er;
    lHandler.fDR        := aDR;
    lHandler.fDRI       := aDRI;
    lHandler.fDRIEntry  := aDRIEntry;

    lHandler.PrintEZU;
  finally
    lHandler.Free;
  end;
end;

procedure TZUHandler.InitAll;
begin
  DirektePDF := true;

//  fFilename := TTempFileHandler.UniqueFileName(temppath+cZustellPDFName,'.pdf');
  SetFileName;

  WSpooler.PDFName     := fFileName; // BW 12.05.23                                                 { TODO 2 -oXEFrank -cS_2_Short : XE }
  //13.05.24 WSpooler.PDFFileName := fFilename;
  BereitDok            := fFilename;                                                                { TODO 2 -oXEFrank -cS_2_Short : XE }
  DirekteSammeln       := false;

  fDip    := DIP[60];
  DIP[60] := DIP[60] or 4;
end;

procedure TZUHandler.DeInitAll;
begin
  BereitDok  := '';
  DirektePDF := false;
  DIP[60]    := fDip;
end;

procedure TZUHandler.DoFormBrief;
Var
  ch : VCLChar;
begin

  try
   TIndividualLogbuch.TryAddToLogbuch(lbKostenrechnung,TIndividualLogbuch.CreateLogbuchInhalt
                                                                         .SetUnitName('ZUHandler')
                                                                         .SetMethodenName('DoFormBrief')
                                                                         .AddWert('fBriefName',fBriefName)
                                                                         );
  except
  end;                                     

  if fDRI
    then FormBRIEF(12,fBriefName,0,fDR.DR,0,fDR.Jahr,ch,FALSE,true,true,False,FALSE,1,false)        { TODO 2 -oXEFrank -cS_2_Short : XE }
    else FormBRIEF(12,fBriefName,0,0,fDR.DR,fDR.Jahr,ch,FALSE,true,true,False,FALSE,1,false);       { TODO 2 -oXEFrank -cS_2_Short : XE }
end;

class procedure TZUHandler.DoGleichzeitige;
var
  lEGVPMailParams : TEGVPMailParams;
  lSRNamen : IStringList;
  lAZ,
  lAnlagen,
  lEmpf : String;
  lOK : boolean;
  lDRs : TGleichzeitigPairs;
  lDR : MultiNr_Typ;

  Procedure FillAnlagen;
  var
    i : integer;
  begin
    lAnlagen := '';

    for i := 0 to lDRs.Count-1 do
      lAnlagen := lAnlagen + lDRs.Get(i).GetDocuments;

{    if (FindFirst(SPLR + 'Gleichzeitig\*',faAnyFile,lSR) = 0) then
      begin
        repeat
          if ((lSR.Name <> '.') AND (lSR.Name <> '.') AND (Not SameText(lSR.Name,'Data.dat'))) then
            lAnlagen := lAnlagen + SPLR + 'Gleichzeitig\' + lSR.Name + ';';
        until (FindNext(lSR) <> 0);
        FindClose(lSR);
      end; }
  end;

{  Function GetAZ : String;
  var
    i : integer;
    lList : IStringList;
    lPos1,lPos2 : integer;
    lItem : String;
  begin
    result := '';
    lList               := TIStringList.Create;
    lList.Duplicates    := dupIgnore;
    lList.CaseSensitive := true;

    for i := 0 to lSRNamen.Count-1 do
      begin
        lItem := lSRNamen[i];
        lPos1 := Pos('_',lItem);
        lPos2 := Pos('_',Copy(lItem,lPos1+1));

        lItem := Copy(lItem,lPos1,lPos2);

        lItem := StringReplace(lItem,'-',' ',[]);
        lList.Add(lItem);
      end;

    for i := 0 to lList.Count-1 do
      begin
        if (i = 0)
          then result := result + lList[i]
          else result := result + ', ' + lList[i];
      end;
  end;}

  Procedure FillData;
  var
    lList : IStringList;
    i : integer;
    lFirst,lSecond,lThird : String;
    lPos1,
    lPos2  : integer;

    Procedure Add(var aValue : String; const aWert, aDelimiter : String);
    begin
      if (aValue = '')
        then aValue := aWert
        else if (aWert <> '') then
               aValue := aValue + aDelimiter + aWert;
    end;

  begin
    lAZ := '';
    lEmpf := '';
    lAnlagen := '';
    if DirectoryExists(SPLR + 'Gleichzeitig') then
      begin
        if FileExists(SPLR + 'Gleichzeitig\Data.dat') then
          begin
            lList := TIStringlist.Create;
            lList.LoadFromFile(SPLR + 'Gleichzeitig\Data.dat');

            for i := 0 to lList.Count - 1 do
              begin
                lFirst := lList[i];
                lPos1 := Pos('||',lFirst);
                lPos2 := Pos('||',Copy(lFirst,lPos1+2));

                lThird := Trim(Copy(lFirst,lPos2+1+lPos1+2));
                lSecond := Trim(Copy(lFirst,lPos1+2,lPos2-1));
                lFirst := Trim(Copy(lFirst,1,lPos1-1));

                if lSecond <> '' then
                  begin
                    if lEmpf = ''
                      then lEmpf := lSecond
                      else if not SameText(lEmpf,lSecond) then
                        lOK := false;
                  end;

                if (Pos(lFirst,lAZ) = 0) then
                  Add(lAZ,lFirst,',');
//                Add(lAnlagen,lThird,';');

                InterpretStringAsDR(lFirst,lDR);
                lDRs.Add(lDR,'Gleichzeitige Zustellung',lThird);
              end;

            FillAnlagen;
          end;
      end;
  end;

  Procedure AddHistories;
  var
    i : integer;

    Function AddIfNotEmpty(const aAdd,aVergleich : String) : String;
    begin
      if (Trim(aVergleich) <> '')
        then result := aAdd + aVergleich
        else result := '';
    end;

  begin
    for i := 0 to lDRs.Count-1 do
      AddHistorie(lDRs.Get(i).DR.Nr,lDRs.Get(i).HistText + AddIfNotEmpty(' an ',lEGVPMailParams.Empfaenger),lDRs.Get(i).DR.DRI);
  end;

  Procedure SaveDokumenteToAkte;
  var
    i : integer;
    lZIPName : WideString;
    lZip     : IMemZipPW;
  begin
    for i := 0 to lDRs.Count -1 do
      begin
        lZIPName := FotoDir + lDRs.Get(i).DR.FotoKey(false) + ' ' + lDRs.Get(i).GetBezeichner + '_' + FormatDateTime('dd-mm-yyyy',now) + '_' + lEGVPMailParams.NachrichtenIDForGleichzeitig + '.zip';
        lZIP := TZipMitPW.MemZipper;
        lZIP.PrepareZip(lZipName,'');
        lDRs.Get(i).SaveAllDocumentsToZip(lZIP);
      end;
  end;

begin
  try
    lSRNamen := TIStringList.Create;
    FillChar(lEGVPMailParams,SizeOf(lEGVPMailParams),0);
    lEGVPMailParams.Justiz       := false;
    lEGVPMailParams.SignierePDF  := false;
    lEGVPMailParams.ACK          := true;

    lOK                          := true;
    FillData;

    if not lOK then
      begin
        if (TDialogs.MyMessageDlg('~Unterschiedliche Empfùnger!' + #13 + 'Es wurden unterschiedliche Empfùnger fùr die gleichzeitige Zustellung ermittelt.' + 'Wollen Sie dennoch fortfahren?',mtWarning,mbYesNo,0) = mrYes)
          then AddUltimateLog(8,'Gleichzeitige eZU trotz Unterschiedlicher Empfùnger.')
          else Exit;
      end;

    lEGVPMailParams.AzAbs        := lAZ;                                                            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    lEGVPMailParams.Empfaenger   := lEmpf;                                                          { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    lEGVPMailParams.Anlagen      := lAnlagen;                                                       { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    lEGVPMailParams.SafeIDCaption := 'Safe-ID des Empfùngers';
    lEGVPMailParams.IsGleichzeitig := true;

    if lAZ <> ''
      then lEGVPMailParams.Betreff := 'Zustellung ' + lAZ
      else lEGVPMailParams.Betreff := 'Gleichzeitige eZU';                                          { TODO 2 -oXEFrank -cS_2_Ansi : XE }

    if TEGVP_Mail.Construct(@lEGVPMailParams).Erzeugen then
      begin
        SaveDokumenteToAkte;
        TDialogs.MyMessagedlg('Die gleichzeitige Zustellung wurde erstellt.',mtinformation,[mbok],0);
        DelDIR(SPLR + 'Gleichzeitig',true);
        AddHistories;
      end;
  except

  end;
end;

function TZUHandler.CreateGVSToXML : IGVSToXML;
begin
  if fIs840er
    then Result := TGVSToXML.ConstructFromGlobal(fDRI,fDR,@_ZUEmpfaenger,@_AusgewaehlterSchuldner,true,fDRIEntry)
    else Result := TGVSToXML.ConstructFromGlobal(fDRI,fDR,NIL,@_AusgewaehlterSchuldner,false,fDRIEntry); // FL 11.01.22 aDRIEntry

  if Result.SafeIDAuftraggeber = '' then  // FL 18.11.22 Damit die SafeID generell vorliegt!
    Result.SafeIDAuftraggeber := eZUSafe;
end;

(*
function TZUHandler.CreateCollectName : String;
begin
  Result := TTempFileHandler.UniqueFileName(EGVPAblage + ExtractFilename(fFilename)+'_',cZUExt);
end;

function TZUHandler.CollectSaved(Const aGVSToXML : IGVSToXML) : boolean;
var
  lDataStream : IDataStream;
  lBoolean    : Boolean;
begin
  Result := eZUCollect;

  if Result then
    begin
      lDataStream := TDataStream.Create(CreateCollectName);

      aGVSToXML.SaveToStream(lDataStream.Stream);

      lBoolean := (GlobalZUInfoCollector <> NIL);

      TAllTypeStream.Schreibe(lDataStream.Stream,lBoolean);

      if lBoolean then
        GlobalZUInfoCollector.SaveToStream(lDataStream.Stream);

//      eZUCollect := false;
    end;
end;
*)

function TZUHandler.MergeZU(Const aGVSToXML : IGVSToXML) : boolean;
var
  lDR      : MultiNr_Typ;
  lBetreff : String;
  SpoolHeader : tSpoolHeader;
begin
  lDR := MultiNr_Typ.Create(fDRI,fDR);

  lBetreff := 'Zustellung ' + lDR.ToString(true);

  if lDR.DRI then
   InitSpoolHeader(SpoolHeader, eDRI, lBetreff, lDR.Nr.DR, lDR.Nr.Jahr )
  else
   InitSpoolHeader(SpoolHeader, eDRII, lBetreff, lDR.NR.DR, lDR.Nr.Jahr );
  result := EGVP_MakeMail(false,_ZUEmpfaenger.SafeID,lBetreff,'Siehe Anlage.',lDR.ToString(true),'',
                          NoSignatur,fFileName,EGVPMsgTESTONLY,
                          true,fDR,fDRI,false,DLeer,aGVSToXML,SpoolHeader,true,true);  { TODO 2 -oXEFrank -cS_2_Ansi : XE }

  if result then
    AddHistorie(fDR,'Elektronische Zustellung erstellt',fDRI);

  if FileExists(fFileName) then
    DelFile(fFileName);                                                                             { TODO 2 -oXEFrank -cS_2_Short : XE }

  eZUSafe := '';
end;

procedure TZUHandler.PrintEZU;
Var
  lGVSToXML       : IGVSToXML;
  lData           : IDataStream;
  lEZU            : String;
  lLoadOK         : Boolean;
begin
  InitAll;
  try
    try
      DoFormBrief;

      lData := TDataStream.Create;

      Repeat // FL 18.08.23
        lLoadOK := lData.LoadFromFileSave(fFilename);

        if not(lLoadOK) then
          if TDialogs.MyMessageDlg('Zustelldokumentation konnte nicht an die PDF-Verarbeitung ùbergeben werden! Nochmal versuchen?',mtConfirmation,[mbRetry,mbAbort],0) = mrAbort then
            exit;

      Until lLoadOK;

      lEZU := BoolToString(fIs840er,'eZU840','eZU'); // FL 18.08.23

      lGVSToXML := CreateGVSToXML;

      MergeZU(lGVSToXML);
    except
    end;
  finally
    DeInitAll;
  end;
end;

Procedure TZUHandler.SetFileName;

// Beispiel: Max 90, nur deutsche Buchstaben und Zahlen sowie ù,ù,ù,ù und _ sowie -
// eZU-840_DRII-65555-23_13-07-2023_99959.pdf --> 42 ~> 50
  Function DRString : String;
  begin
    if (Land in DRILaender)
      then begin
             if fDRI
               then result := 'DRI-'
               else result := 'DRII-';
           end
      else result := 'DR-';

    result := result + StringReplace(fDR.DRToString,'/','-',[rfReplaceAll]);
  end;

  Function GetPruef : String;
  begin
    result := FormatDateTime('zzzss',now);

    if FileExists(fFileName+result+'.pdf') then
      result := GetPruef;
  end;

begin
  fFileName := temppath + 'eZU';

  if fIs840er then
    fFileName := fFileName + '-840';

  fDate     := FormatDateTime('dd-mm-yyyy',now);
  fPruef    := GetPruef;
  fFileName := fFileName + '_' + DRString + '_' + fDate + '_' + fPruef + '.pdf';
end;

{ TGleichzeitigPairs }

procedure TGleichzeitigPairs.Add(aDR: MultiNr_Typ; const aHistText: String; const aDokument: WideString);
var
  i : integer;

  Function CheckSame(aDR1,aDR2 : MultiNr_Typ) : boolean;
  begin
    result := (
              (aDR1.DRI = aDR2.DRI) AND
              (aDR1.Nr.DR = aDR2.Nr.DR) AND
              (aDR1.Nr.Jahr = aDR2.Nr.Jahr)
              );
  end;

begin
  for i := 0 to High(fPairs) do
    begin
      if CheckSame(fPairs[i].DR,aDR) then
        begin
          fPairs[i].AddDocument(aDokument);
          exit;
        end;
    end;

  SetLength(fPairs,Length(fPairs)+1);
  fPairs[High(fPairs)].DR.Create(aDR.DRI,aDR.Nr);
  fPairs[High(fPairs)].HistText := aHistText;
  fPairs[High(fPairs)].AddDocument(aDokument);
end;

function TGleichzeitigPairs.Count: integer;
begin
  result := Length(fPairs);
end;

function TGleichzeitigPairs.Get(aIndex: integer): PGleichzeitigPair;
begin
  result := @fPairs[aIndex];
end;

{ TGleichzeitigPair }

procedure TGleichzeitigPair.AddDocument(const aDocument: WideString);
begin
  SetLength(Dokumente,Length(Dokumente)+1);
  Dokumente[High(Dokumente)] := aDocument;
end;

function TGleichzeitigPair.GetBezeichner: WideString;
var
  i : integer;
  lWhat : integer;
  lTemp : String;
begin
  result := '';
  lWhat := 0;

  for i := 0 to High(Dokumente) do
    begin
      lTemp := ExtractFileName(Dokumente[i]);
      if StartsText('eZU_DR',lTemp)
        then lWhat := 1
        else if StartsText('eZU-840_DR',lTemp) then
          lWhat := 2;

      if lWhat > 0 then
        break;
    end;

  case lWhat of
    0 : result := 'Brief';
    1 : result := 'eZU';
    2 : result := 'eZU840';
  end;
end;

function TGleichzeitigPair.GetDocuments(const aDelim: String): WideString;
var
  i : integer;

  Function CheckSignatur(const aDatei : WideString; const aDelim, aExt : String) : WideString;
  begin
    result := '';

    if FileExists(aDatei + aExt) then
      result := aDatei + aExt + aDelim;
  end;

begin
  result := '';

  for i := 0 to High(Dokumente) do
    begin
      result := result + Dokumente[i] + aDelim;

      result := result + CheckSignatur(Dokumente[i],aDelim,'.pkcs7') + CheckSignatur(Dokumente[i],aDelim,'.p7s');
    end;
end;

procedure TGleichzeitigPair.SaveAllDocumentsToZip(const aZIP: IMemZipPW);
var
  i : integer;
  lData : IDataStream;

  Procedure CheckSignatur(const aExt : String);
  begin
    if FileExists(Dokumente[i] + aExt) then
      begin
        lData.Clear;
        lData.Stream.LoadFromFile(Dokumente[i] + aExt);
        aZip.AddStream(lData.Stream,ExtractFileName(Dokumente[i]) + aExt);
      end;
  end;

begin
  lData := TDataStream.Create;
  for i := 0 to High(Dokumente) do
    begin
      lData.Clear;
      lData.Stream.LoadFromFile(Dokumente[i]);
      aZip.AddStream(lData.Stream,ExtractFileName(Dokumente[i]));

      CheckSignatur('.pkcs7');
      CheckSignatur('.p7s');
    end;
end;


end.


