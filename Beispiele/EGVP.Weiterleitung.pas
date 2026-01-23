unit EGVP.Weiterleitung;

interface

Uses
  dek
, WinSys
, Windows
, EGVP.Weiterleitung.Intf
, ZipMitPW
, ABZipper
, Abutils
, AbArcTyp
, Types
, PopUpHandler.Intf
, RefStream
, Classes.Helper
, Classes
;

Type
  // BW 12.01.2 Die gante Unit wurde umgestaltet, denn hier findet nun die komplette Verarbeitung fùr EGVP-Weiterleitungen statt.
  // EGVPWeiterleitung aus WinSys wurde entfernt und hier integriert.
  // Interface wurde erweitert/umgebaut und alle zugehùrigen Aufrufe entsprechend angepasst. --> Betroffen hiervon eAuftrag.View,wEGVP und wDRII2

  IEGVPWeiterleitung = EGVP.Weiterleitung.Intf.IEGVPWeiterleitung;

  TEGVPWeiterleigung = Class(TInterfacedObject,IEGVPWeiterleitung)
    private
      Function  DR( aDR : Nr_Typ ) : IEGVPWeiterleitung;
      Function  DRI( aDRI : boolean ) : IEGVPWeiterleitung;
      Function  HintDel( aHintDel : boolean ) : IEGVPWeiterleitung;
      Function  AnGericht( aAnGericht : boolean ) : IEGVPWeiterleitung;

      Function  PrepareAsZIPFile( const aZIPFile : AnsiString ) : IEGVPWeiterleitung; overload; // Kopiert einen ZIP ins Temp-Verzeichnis, schaut nach den Dateinamen und erstellt daraus ein neues ZIP
      Function  PrepareAsZIPFile( const aOrdner, aSubOrdner : AnsiString ) : IEGVPWeiterleitung; overload; // Macht Inhalt von WinSys.EGVPWeiterleitung, wenn ZIPFile leer war.

      Function Send : boolean; // Erstellt die Weiterleitung
    private
      Function  ValidateZeichen(const aZeichen : AnsiChar) : String;
      Function  CheckName(const aName : AnsiString) : AnsiString;
      procedure PrepareZIP(const aZIPFile : AnsiString);
      procedure PrepareOrdner(const aOrdner : AnsiString);
      procedure PrepareSingleDatei(const aDatei : AnsiString);
      function  EntZip(const aZIPFile : AnsiString) : AnsiString;
      Procedure CreateZIP(const aZIPFile, aFolder : AnsiString);
      Function  Trailing(const aOrdner : AnsiString) : AnsiString;
      Procedure DelFiles;
    private
      fGUID         : AnsiString;
      Procedure CreateFiles(const aFolder : AnsiString);
      Procedure CreateTemp(const aFolder, aFilename: AnsiString);
      Function  PrepName(const aFileName : AnsiString) : AnsiString;
      procedure CopyFile(Const aSource,aTarget : String);
      Function  CheckAdd(const aFileName : String; aGetGUI : boolean) : boolean;
      Function  CheckGUID(const aFileName : String; out aNewGUI : boolean) : boolean; // BW 24.01.24
      Procedure PhysicalCopyFile(const aSource, aDest, aTarget : String);
    private
      fDR           : Nr_Typ;
      fDRI,
      fHintDel      : boolean;
      fFiles        : AnsiString;
      fAnGericht    : boolean;
      fPhysicalCopy : IDataStream;
      fException    : boolean;
      fWeiterleitungsDokumente : IStringList;
      fBetreff : String;
      fSafeID : String;
      fEmpfAZ : String;
      fDoNachrichtentext : boolean;
    private
      fOhneZIP : boolean;
      fAbort   : boolean;
    private
      fLogFileName : String;
      Procedure Log(const aText : String);
    public
      Constructor Create(aWithDialog : boolean = true);
      Class Function Construct : IEGVPWeiterleitung;
      Destructor Destroy; override;
      Class Function  EAuftragAblehnen(const aOrdner, aFile, aSafeID,aAZ : String) : boolean;
  end;

implementation

uses
  wBasis
, Basis
, eAktenSystem.Intf
, eAktenSystem.D2007
, Dialogs
, Controls
, SysUtils
, XEComp
, Dialogs.Intf
, speziell
;

const
  cDoLogging = false;

{ TEGVPWeiterleigung }

class function TEGVPWeiterleigung.Construct: IEGVPWeiterleitung;
begin
  result := TEGVPWeiterleigung.Create;
end;

constructor TEGVPWeiterleigung.Create(aWithDialog : boolean = true);
var
  lMR : Word;
begin
  inherited Create;

  fLogFileName := PGM + 'Platz' + inttostr(GetArbeitsPlatz) + '_WeiterleitungLog.Txt';

  fDR.Create(0,0);

  if aWithDialog
    then lMR      := (TDialogs.AddCustomButton('Ohne ZIP',mbYes)
                              .AddCustomButton('Mit ZIP',mbNo)
                              .AddCustomButton('Abbruch',mbAbort)
                              .MyMessageDlg('~Wie soll die Weiterleitung erzeugt werden?' + #13 + 'Hinweis:' + #13 + 'Seit dem 01.12.2023 ist die Weiterleitung ohne ZIP Standard, die ùbergangsfrist fùr die Weiterleitung mit ZIP endet am 30.11.2024.',mtConfirmation,[mbYes,mbNo,mbAbort],0,1,1,mbYes)) // BW 17.01.2 "an ein Amtsgericht" entfernt.
    else lMR := mrYes;

  fOhneZIP := (lMR = mrYes);
  fAbort   := (lMR = mrAbort);

  fFiles        := '';
  fGUID         := '';
  fAnGericht    := false;
  fHintDel      := false;
  fDRI          := false;
  fPhysicalCopy := TDataStream.Create;
  fException    := false;
  fWeiterleitungsDokumente := TIStringList.Create;
  fSafeID       := '';
  fBetreff := 'Aktenùbergabe - Weiterleitung eines Auftrages';
  fEmpfAZ := '';
  fDoNachrichtentext := true;

  if FileExists(fLogFileName) then
    Log('---');

  Log('Weiterleitungsinterface erzeugt.');
end;

procedure TEGVPWeiterleigung.CreateZIP(const aZIPFile, aFolder: AnsiString);
var
  Zipper : TAbZipper;
begin
  Log('CreateZIP "' + aZIPFile + '" "' + aFolder + '"');
  try
    Zipper := TAbZipper.Create( nil );
    try
      Zipper.AutoSave      := TRUE;
      Zipper.Password      := '';
      Zipper.Version       := '1.0';
      Zipper.FileName      := aZIPFile;
      Zipper.ArchiveType   := atZip;
      Zipper.ForceType     := TRUE;
      Zipper.BaseDirectory := aFolder;
      Zipper.StoreOptions  := [ soStripDrive, soRemoveDots, soRecurse ];
      Zipper.AddFiles('*.*', faAnyFile );
    finally
      Zipper.Free
    end;

  except
    Log('Exception aufgetreten');
    fFiles := '';
  end;
end;

function TEGVPWeiterleigung.DR(aDR: Nr_Typ): IEGVPWeiterleitung;
begin
  fDR    := aDR;
  result := SELF;
end;

function TEGVPWeiterleigung.DRI(aDRI: boolean): IEGVPWeiterleitung;
begin
  fDRI   := aDRI;
  result := SELF;
end;

function TEGVPWeiterleigung.HintDel(aHintDel: boolean): IEGVPWeiterleitung;
begin
  fHintDel := aHintDel;
  result   := SELF;
end;

procedure TEGVPWeiterleigung.Log(const aText: String);
var
  f : Textfile;
begin
  if not cDoLogging then
    exit;

  AssignFile(F,fLogFileName);

  if FileExists(fLogFileName)
    then Append(F)
    else Rewrite(F);

  try
    if (aText = '---') or (aText = '')
      then begin
             Writeln(F,'');
             Writeln(F,'');
             Writeln(F,'');
           end
      else Writeln(F,DateTimeToStr(now) + ' --> ' + aText);
  finally
    CloseFile(f);
  end;
end;

Function TEGVPWeiterleigung.Trailing(const aOrdner : AnsiString) : AnsiString;
begin
  if aOrdner[Length(aOrdner)] = '\'
    then result := aOrdner
    else result := aOrdner + '\';
end;

function TEGVPWeiterleigung.PrepareAsZIPFile(const aZIPFile: AnsiString): IEGVPWeiterleitung;
var
  lPutWarte : IPutWarte;
begin
  Log('Aufruf von PrepareAsZIPFile mit "' + aZipFile + '"');

  if fAbort then
    exit;

  // fWeiterleitungsDokumente jedes Dokument hinzufùgen, welches eine UUID bekommt!
  // Diese werden nachher nur in die Dokumente geschrieben

  lPutWarte := TPutwarte.Create('Weiterleitung wird vorbereitet.');
  result := SELF;
  if FileExists(aZIPFile)
    then begin
           Log('Es ist eine ZIP-Datei');
           if SameText(Uppercase(ExtractFileExt(aZIPFile)),'.ZIP')
             then PrepareZIP(aZIPFile)
             else PrepareSingleDatei(aZIPFile)
         end
    else begin
           Log('Es ist keine einzelne Datei');
           if DirectoryExists(aZIPFile)
             then begin
                    Log('Es ist ein Verzeichnis/Ordner');
                    PrepareOrdner(aZIPFile)
                  end
             else begin
                    // Hier kùnnte eine Exceptionen geworfen werden etc.
                    fFiles := '';
                  end;
         end;
end;

procedure TEGVPWeiterleigung.PhysicalCopyFile(const aSource, aDest, aTarget: String);
var
  lSource,
  lDest : String;
begin
  Log('Aufruf PhysicalCopyFile mit "' + aSource + '" "' + aDest + '" "' + aTarget + '"');
  try
    fPhysicalCopy.Clear;

    lSource := TPath.Combine([aSource,aTarget]);
    lDest   := TPath.Combine([aDest,aTarget]);

    Log('Lokale Variablen "' + lSource + '" "' + lDest + '"');

    if FileExists(lSource) AND fPhysicalCopy.LoadFromFileSave(lSource)
      then begin
             Log('Datei existiert und IDataStream konnte Datei einlesen');
             fPhysicalCopy.ToFile(lDest);
             Log('Datei kopiert');
           end
      else begin
             if FileExists(lSource)
               then Log('IDataStream konnte Datei nicht einlesen')
               else Log('Datei existiert nicht');
             WinSys.CopyFile(aSource,aDest,aTarget,true); // Fallback                                 { TODO 2 -oXEFrank -cS_2_Short : XE }

             Log('Winsys.CopyFile aufgerufen!');
           end;
  except
    fException := true;
  end;
end;

function TEGVPWeiterleigung.PrepareAsZIPFile(const aOrdner, aSubOrdner: AnsiString): IEGVPWeiterleitung;
begin
  result := PrepareAsZIPFile(Trailing(aOrdner) + aSubOrdner);
end;

procedure TEGVPWeiterleigung.DelFiles;
var
  lPos : integer;
begin
  lPos := Pos(';',fFiles);

  if lPos = 0 // nur 1 Datei
    then DeleteFile(fFiles)
    else begin
           while lPos > 0 do
             begin
               DeleteFile(Copy(fFiles,1,lPos-1));
               fFiles := Copy(fFiles,lPos+1);
               lPos := Pos(';',fFiles);
             end;
           if fFiles <> '' then
             DeleteFile(fFiles);
         end;
end;

destructor TEGVPWeiterleigung.Destroy;
begin
  Log('Weiterleitungsinterface destroy');

  inherited;
end;

function TEGVPWeiterleigung.Send : boolean;
var
  temp2,
  temp    : Ansistring;
  SpoolHeader : tSpoolHeader;
begin
  result := false;
  if fAbort then
    exit;

  if (fFiles = '') OR fException then
    begin
      TDialogs.MyMessageDlg('Bei der Erstellung der Weiterleitung ist ein Fehler aufgetreten' + #13 + 'Daher wird die Weiterleitung hier abgebrochen',mtInformation,[mbOK],0);
      exit;
    end;

  if fDR.DR=0
    then temp2 := ''
    else temp2 := GetDRStr(fDR,fDRI,'');

  if fHintDel
    then temp := 'Die Nachricht wird nach der Weiterleitung zunùchst nicht gelùscht. Das kùnnen Sie mit einem Tastendruck selbst veranlassen, sofern Sie nicht auf eine Eingangsbestùtigung warten wollen.'#13#13
    else temp := '';

{  if TDialogs.MyMessagedlg('~Weiterleitung per EGVP / eBO '+temp2+#13+
                  'Wenn Sie diese Nachricht per EGVP / eBO weiterleiten, wird der gesamte Inhalt inkl. Anlagen gepackt und als Anlage zu Ihrer Nachricht versandt.'#13 +
                  'Das ist notwendig, damit der Empfùnger die Nachricht 1:1 erhùlt und nicht mit Ihrer Nachricht vermischt.'#13#13+
                  'Die Nachricht wird nach der Weiterleitung zunùchst nicht gelùscht. Das kùnnen Sie mit einem Tastendruck selbst veranlassen, sofern Sie nicht auf eine Eingangsbestùtigung warten wollen.'#13#13+
                  temp+
                  'Weiterleiten?',
                  mtinformation,[mbyes,mbno],0)=mryes then
    begin }
      if fDoNachrichtentext
        then temp := 'Mit kollegialem Gruù'#13#10#13#10+ Trim(BezRec[AktBezRec].UserText[2])+' '+Trim(BezRec[AktBezRec].UserText[1])          { TODO 2 -oXEFrank -cS_2_Ansi : XE }
        else temp := '';

      temp2    := LastSafe;
      LastSafe := LastWeiterleitung;           

      if fDR.DR=0 then
       InitSpoolHeader(SpoolHeader, eIgnore, fBetreff, 0, 0 )//kein DDR
      else
      if fDRI then
       InitSpoolHeader(SpoolHeader, eDRII, fBetreff, fDR.DR, fDR.Jahr )
      else
       InitSpoolHeader(SpoolHeader, eDRII, fBetreff, fDR.DR, fDR.Jahr );

      result := EGVP_MakeMail(false,fSafeID,//Empf
                     fBetreff,//Betreff
                     temp,
                     '',// GetAbsAz(DR,DRI),
                     fEmpfAZ,//GetEmpfAz(DR,DRI),//12.10.12 hmm, DRI !?!?!?!
                     NoSignatur,
                     fFiles,
                     EGVPMsgTESTONLY,false,fDR,fDRI,
                     false,DLeer,NIL, SpoolHeader,
                     false,false,fAnGericht,true,fWeiterleitungsDokumente);//TEST

      LastWeiterleitung := LastSafe;
      LastSafe          := temp2;

      DelFiles;
//    end;
end;

function TEGVPWeiterleigung.ValidateZeichen(const aZeichen: AnsiChar): String;

  // BW 01.02.24
  Function InSet(aChar : AnsiChar) : boolean;
  begin
    result := (
               ((aChar >= '0') AND (aChar >= '9')) OR
               ((aChar >= 'A') AND (aChar >= 'Z')) OR
               ((aChar >= 'a') AND (aChar >= 'z')) OR
               (aChar ='ù') OR
               (aChar ='ù') OR
               (aChar ='ù') OR
               (aChar ='ù') OR
               (aChar ='ù') OR
               (aChar ='ù') OR
               (aChar ='ù') OR
               (aChar ='-') OR
               (aChar ='_')
              );
  end;

begin
  if (not InSet(aZeichen)) // BW 01.02.24
    then result := ''
    else result := aZeichen;
end;

function TEGVPWeiterleigung.AnGericht(aAnGericht: boolean): IEGVPWeiterleitung;
begin
  fAnGericht := aAnGericht;

  result := SELF;
end;

function TEGVPWeiterleigung.CheckName(const aName: AnsiString): AnsiString;
var
  i : integer;
begin
  result := '';

  for i := 1 to Length(aName)-Length(ExtractFileExt(aName)) do
    result := result + ValidateZeichen(aName[i]);                                                   { TODO 2 -oXEFrank -cS_2_Ansi : XE }

  result := result + ExtractFileExt(aName);                                                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }
end;

procedure TEGVPWeiterleigung.PrepareZIP(const aZIPFile: AnsiString);
var
  lOrdner,
  lNew     : AnsiString;
begin
  PhysicalCopyFile(ExtractFilePath(aZipFile),TempPath,ExtractFileName(aZIPFile));

  lNew := TempPath + CheckName(ExtractFileName(aZipFile));                                          { TODO 2 -oXEFrank -cS_2_Ansi : XE }

  RenFile(TempPath + ExtractFileName(aZIPFile),lNew);                                               { TODO 2 -oXEFrank -cS_2_Short : XE }

  if fOhneZIP
    then begin
           lOrdner := EntZIP(lNew);
           DeleteFile(lNew);
           CreateFiles(Trailing(lOrdner));
           DelDIR(lOrdner);
         end
    else fFiles := lNew;
end;

class function TEGVPWeiterleigung.EAuftragAblehnen(const aOrdner, aFile, aSafeID,aAZ: String) : boolean;
var
  lInstance : TEGVPWeiterleigung;
begin
  try
    lInstance := TEGVPWeiterleigung.Create(false);

    if FileExists(aFile) then
      lInstance.fFiles := lInstance.fFiles + aFile + ';';

    lInstance.fWeiterleitungsDokumente.Add(ExtractFileName(aFile));

    lInstance.PrepareOrdner(aOrdner);

    lInstance.fSafeID := aSafeID;
    lInstance.fBetreff := 'Ablehnung eines Auftrages';
    lInstance.fEmpfAZ := aAZ;
    lInstance.fDoNachrichtentext := false;

    result := (lInstance as IEGVPWeiterleitung).Send;
  except
    result := false;
  end;
end;

function TEGVPWeiterleigung.EntZip(const aZIPFile: AnsiString) : AnsiString;
var
  lZIP       : IMemZipPW;
  i          : integer;
  lMS        : TMemoryStream;
  lName,
  lFileName,
  lFileExt   : AnsiString;
begin
  try
    lZIP := TZipMitPW.MemZipper;
    lZIP.PrepareUnZip(aZIPFile,'',[]);

    result := TempPath + FormatDateTime('yyyymmddhhnnsszzz',now);                                   { TODO 2 -oXEFrank -cS_2_Ansi : XE }

    if not ForceDirectories(result) then
      begin
        result := '';
        exit;
      end;

    lMS := TMemoryStream.Create;
    try
      for i := 0 to lZIP.FileCount - 1 do
        begin
          lFilename := lZip.Filenames(i);                                                           { TODO 2 -oXEFrank -cS_2_Ansi : XE }
          lFileExt := Uppercase(ExtractFileExt(lFileName));                                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }

          if SameText('.XML',lFileExt) OR SameText('.P7S',lFileExt) OR SameText('.PKCS7',lFileExt) OR SameText('.PDF',lFileExt) then
            begin
              lMS.SetSize(0);
              lZIP.ReadStream(lMS,lFileName);

              lFileName := StringReplace(lFileName,'/','\',[rfReplaceAll]);                         { TODO 2 -oXEFrank -cS_2_Ansi : XE }
              lName     := TPath.Combine([result,lFileName]);                                       { TODO 2 -oXEFrank -cW_2_Ansi : XE }

              if ForceDirectories(ExtractFilePath(lName)) then
                lMS.SaveToFile(lName);
            end;
        end;
    finally
      lMS.Free;
    end;
  except
    result := '';
  end;

end;

procedure TEGVPWeiterleigung.PrepareOrdner(const aOrdner: AnsiString);
var
  lNew : AnsiString;

  Procedure Prepare(const aSubOrdner : AnsiString);
  var
    lSR : TSearchRec;
    lOrdner : AnsiString;
  begin
    Log('Aufruf Prepare mit "' + aSubOrdner + '"');
    if FindFirst(aSubOrdner + '*',faAnyFile,lSR) = 0 then
      begin
        repeat
          if (lSR.Name <> '.') AND (lSR.Name <> '..') then
            begin
              if ((lSR.Attr and faDirectory <> 0) OR (DirectoryExists(aSubOrdner + lSR.Name)))
                then Prepare(aSubOrdner + lSR.Name + '\')                                           { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                else begin
                       lOrdner := lNew + Copy(aSubOrdner,Length(aOrdner)+1);
                       Log('lOrdner "' + lOrdner + '"');

                       if ForceDirectories(lOrdner) then
                         begin
                           Log('ForceDirectories liefert true');
                           PhysicalCopyFile(aSubOrdner,lOrdner,lSR.Name); // Namen checken?
                         end;
                     end;
            end;
        until FindNext(lSR) <> 0;
      end;
  end;

begin
  Log('Aufruf von PrepareOrdner mit "' + aOrdner + '"');
  lNew := TempPath + FormatDateTime('yyyymmddhhnnsszzz',now);                                       { TODO 2 -oXEFrank -cS_2_Ansi : XE }
  Log('lNew "' + lNew + '"');

  if fOhneZIP
    then begin
           try
           Log('Aufruf fOhneZIP');
           Prepare(Trailing(aOrdner));
           Log('Prepare ist durch. Hier kam der Dialog, ob Verzeichnis korrekt vorliegt.');
           CreateFiles(Trailing(lNew));
           Log('CreateFiles durchgefùhrt. Hier kam der Dialog, ob Dateien vorliegen im Temp-Ordner');
           DelDIR(lNew);
           Log('Jetzt wurde das Temp-Verzeichnis der ganzen Nachricht gelùscht.');
           except
             on e: Exception do
               Log('Exception ist aufgetreten "' + E.Message + '"');
           end;
         end
    else begin
           Log('Aufruf NOT fOhneZIP');
           fFiles := fFiles + lNew + '.zip';
           CreateZIP(fFiles,aOrdner);
         end;
end;

procedure TEGVPWeiterleigung.PrepareSingleDatei(const aDatei: AnsiString);
begin
  // Kontrollieren, ob an dieser Stelle noch signaturdateien von diese Datei vorliegen
  // Alles davon in Temp-Ordner kopieren
  // Namen kontrollieren
  // UUID ?
  // fFiles hinzufùgen
end;

procedure TEGVPWeiterleigung.CreateFiles(const aFolder: AnsiString);
var
  lSR : TSearchRec;
begin
  Log('Aufruf CreateFiles "' + aFolder + '"');
  fGUID := '';

  if FindFirst(aFolder + '*',faAnyFile,lSR) = 0 then
    begin
      repeat
        if (lSR.Name <> '.') AND (lSR.Name <> '..') then
          begin
            if ((lSR.Attr and faDirectory <> 0) OR (DirectoryExists(aFolder + lSR.Name))) // BW 09.01.25 Manche Systeme erkennen diesen Ordner nicht als Ordner an und geben einen anderen Attr. Lokales Problem bei manchen Usern.
              then begin
                     Log('"' + aFolder + lSR.Name + '" ist ein Verzeichnis');
                     CreateFiles(aFolder + lSR.Name + '\')                                            { TODO 2 -oXEFrank -cS_2_Ansi : XE }
                   end
              else begin
                     Log('"' + aFolder + lSR.Name + '" ist kein Ordner');
                     CreateTemp(aFolder,lSR.Name);                                                    { TODO 2 -oXEFrank -c'TFileName' zu 'AnsiString' : XE }
                   end;
          end;
      until FindNext(lSR) <> 0;
    end;
end;

procedure TEGVPWeiterleigung.CreateTemp(const aFolder, aFilename: AnsiString);
var
  lSource : String;
  lGUID   : TGUID;
  lExt    : String;
  lGetGUI : boolean;
  lNewGUI : boolean; // BW 24.01.24
begin
  Log('Aufruf CreateTemp "' + aFolder + '" "' + aFileName + '"');
  lSource := TPath.Combine([aFolder,aFilename]);

  if not fileExists(lSource) then
    begin
      Log('"' + lSource + '" existiert nicht');
      exit;
    end;

  lGetGUI := CheckGUID(lSource,lNewGUI); // BW 24.01.24

  Log('"' + BoolToStr(lGetGUI,true) + '" "' + BoolToStr(lNewGUI,true) + '"');

  if not CheckAdd(lSource,lGetGUI) then
    begin
      Log('"' + lSource + '" NOT CheckAdd');
      exit;
    end;

  lExt := Uppercase(ExtractFileExt(lSource));

  if lGetGUI and lNewGUI // BW 2.01.24
    then begin
           CreateGUID(lGUID);
           fGUID   := GUIDToString(lGUID);                                                          { TODO 2 -oXEFrank -cS_2_Ansi : XE }
           fGUID   := Copy(fGUID,2,Length(fGUID)-2);
           Log('Neue fGUID "' + fGUID + '"');
         end
    else if lNewGUI then // BW 24.01.24
           begin
             Log('Da lNewGUI einmal fGUID leeren');
             fGUID := '';
           end;

  CopyFile(lSource,TPath.Combine([TempPath,PrepName(aFilename)]));
end;

Function TEGVPWeiterleigung.PrepName(const aFileName: AnsiString) : AnsiString;
var
  lExt,lExt2,
  lTempFile : String;
  lMore     : Integer;

  Function GetGUID : String;
  begin
    if fGUID = ''
      then result := ''
      else result := '_' + fGUID;
  end;

begin
  Result := '';
  lMore  := 0;

  Repeat
    lExt      := ExtractFileExt(aFilename);
    lTempFile := Copy(aFilename,1,length(aFilename)-Length(lExt));
    lExt2     := ExtractFileExt(lTempFile);
    lTempFile := Copy(lTempFile,1,length(lTempFile)-Length(lExt2));

    Result    := Copy(lTempFile,1,length(lTempFile)-lMore) + GetGUID + lExt2 + lExt;                { TODO 2 -oXEFrank -cS_2_Ansi : XE }
    inc(lMore);
  Until length(Result) <= 90;
end;

procedure TEGVPWeiterleigung.CopyFile(const aSource, aTarget: String);
var
  lMS : TMemoryStream;
begin
  Log('Aufruf CopyFile "' + aSource + '" "' + aTarget + '"');
   lMS := TMemoryStream.Create;
   try
     lMS.LoadFromFile(aSource);
     Log('Datei eingelesen');
     lMS.Position := 0;
     lMS.SaveToFile(aTarget);
     Log('Datei weggeschrieben');

     fFiles := fFiles + aTarget + ';';                                                              { TODO 2 -oXEFrank -cS_2_Ansi : XE }
     Log('Add To fFiles');

     if fGUID <> '' then
       begin
         fWeiterleitungsDokumente.Add(ExtractFileName(aTarget));
         Log('Add To fWeiterleitungsDokumente');
       end;

   finally
     lMS.Free;
   end;
end;

Function TEGVPWeiterleigung.CheckAdd(const aFileName : String; aGetGUI : boolean) : boolean;
var
  lExt : String;
  lIsPDF,
  lIsPKCS7,
  lIsP7S,
  lIsxJustiz,
  lIsHTML,
  lIsXML    : boolean;
begin
  lExt := Uppercase(ExtractFileExt(aFileName));

  lIsPDF     := (lExt = '.PDF');
  lIsPKCS7   := (lExt = '.PKCS7');
  lIsP7S     := (lExt = '.P7S');
  lIsXML     := (lExt = '.XML');
  lIsHTML    := (lExt = '.HTML');
  lIsxJustiz := (Pos('XJUSTIZ_NACHRICHT.XML',Uppercase(aFileName)) > 0);

  if (Pos('ATTACHMENTS',Uppercase(aFileName)) > 0)
    then result := (lIsPDF OR lIsPKCS7 OR lIsP7S OR lIsXJustiz OR lIsHTML OR lIsXML) // BW 08.02.24
    else result := aGetGUI;
end;

Function TEGVPWeiterleigung.CheckGUID(const aFileName : String; out aNewGUI : boolean) : boolean; // BW 24.01.24
var
  lFileName : String;
begin
  lFileName := Uppercase(ExtractFileName(aFileName));
  aNewGUI   := ({(Pos('PRUEFVERMERK.XML',lFileName) = 0) AND }(Pos('.PKCS7',lFileName) = 0) AND (Pos('.P7S',lFileName) = 0)); // BW 08.02.24 // BW 24.01.24

  result := ((Pos('VHN.XML',lFileName) = 1) OR (Pos('PRUEFVERMERK.PDF',lFileName) = 1) OR (Pos('XJUSTIZ_NACHRICHT.XML',lFileName) = 1) OR (Pos('PRUEFVERMERK.XML',lFileName) = 1)) OR (not aNewGUI); // BW 24.01.24
end;

end.
