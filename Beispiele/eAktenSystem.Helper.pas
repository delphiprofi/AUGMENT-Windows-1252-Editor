unit eAktenSystem.Helper;

interface

uses
  Classes.Helper
;

Type
  IStringList = Classes.Helper.IStringList;

  TeAktenHelper = class abstract
    private
      Class Function  GetWatcherDir : String;
      Class Function  GetWatcherFilesForSize(aEqualsZero : boolean) : IStringList;
      Class Function  IsAllowed : boolean;
    public
      Class Procedure ToggleSaveToEAkte(aValue : boolean);
      Class Function  IsSaveToEAkte(const aDialogText : String = '') : boolean; // Hier kann ein Text fùr einen Dialog ùbergeben werden, der kommt, wenn der DIP gesetzt ist. Ist der Text leer, dann erscheint kein Dialog!
      Class Procedure AddNachrichtenIdToWatcher(const aNachrichtenID : String);
      Class Procedure CheckNachrichtenIdInWatcher(const aNachrichtenID : String; aIsACK : boolean);
      Class Procedure DeleteNachrichtenIdFromWatcher(const aNachrichtenID : String);
  end;

implementation

uses
  Dialogs
, Controls
, Windows
, SysUtils

, Dialogs.Intf
, Dek // Fùr DIP
, eAktenSystem.Intf
, eAktenSystem.D2007
, LoadStd // Fùr WriteOneDip
;

{ TeAktenHelper }

class procedure TeAktenHelper.AddNachrichtenIdToWatcher(const aNachrichtenID: String);
var
  lWatcherDir,
  lFile        : String;
begin
  if not IsAllowed then
    exit;

  lWatcherDir := GetWatcherDir;
  lFile       := lWatcherDir + aNachrichtenID;

  if not DirectoryExists(eAktenDir) then
    ForceDirectories(eAktenDir);

  if not DirectoryExists(lWatcherDir) then
    ForceDirectories(lWatcherDir);

  if not FileExists(lFile) then
    FileCreate(lFile);
end;

class procedure TeAktenHelper.CheckNachrichtenIdInWatcher(const aNachrichtenID: String; aIsACK: boolean);
var
  lItems         : IStringList;
  i              : integer;
  lNachrichtenID : String;
  lFile : TextFile;
begin
  if not IsAllowed then
    exit;

  lItems := GetWatcherFilesForSize(true);

  for i := 0 to lItems.Count-1 do
    begin
      lNachrichtenID := lItems[i];

      if SameText(lNachrichtenID,aNachrichtenID) then
        begin
          TDialogs.MyMessagedlg('Hier wùrde der Add zur eAkte kommen!',mtconfirmation,[mbyes,mbno],0);
          if aIsACK
            then begin
                   AssignFile(lFile,GetWatcherDir+lNachrichtenID);
                   try
                     Rewrite(lFile);
                     Write(lFile,'Done!');
                   finally
                     CloseFile(lFile);
                   end;
                 end
            else DeleteFile(GetWatcherDir+lNachrichtenID);

          exit;
        end;
    end;
end;

class procedure TeAktenHelper.DeleteNachrichtenIdFromWatcher(const aNachrichtenID: String);
var
  lFile : String;
begin
  if not IsAllowed then
    exit;

  lFile := GetWatcherDir + aNachrichtenID;

  if FileExists(lFile) then
    DeleteFile(lFile);
end;

class function TeAktenHelper.GetWatcherDir: String;
begin
  result := eAktenDir + 'Watcher\';
end;

class function TeAktenHelper.GetWatcherFilesForSize(aEqualsZero: boolean): IStringList;
var
  lSR : TSearchRec;
begin
  result := TIStringList.Create;

  if FindFirst(GetWatcherDir + '*',faAnyFile,lSR) = 0 then
    begin
      repeat
        if (lSR.Name <> '.') and (lSR.Name <> '..') then
          begin
            if aEqualsZero
              then begin
                     if lSR.Size = 0 then
                       result.Add(lSR.Name);
                   end
              else begin
                     if lSR.Size <> 0 then
                       result.Add(lSR.Name);
                   end;
          end;
      until (FindNext(lSR) <> 0);

      FindClose(lSR);
    end;
end;

class function TeAktenHelper.IsAllowed: boolean;
begin
  result := AlloweAkte;

  // Eigene Methode, falls mal ein Dialog oder so kommen soll!
end;

class function TeAktenHelper.IsSaveToEAkte(const aDialogText: String): boolean;
begin
  if not IsAllowed then
    begin
      result := false;
      exit;
    end;

  result := ((DIP[112] and 1) = 1);

  if (result and (aDialogText <> '')) then
    result :=  TDialogs.MyMessagedlg(aDialogText,mtconfirmation,[mbyes,mbno],0)=mryes;
end;

class procedure TeAktenHelper.ToggleSaveToEAkte(aValue: boolean);
begin
  if not IsAllowed then
    exit;

  if aValue
    then DIP[112] := (DIP[112] or 1)
    else DIP[112] := (DIP[112] and (255-1));

  WriteOneDip(112);  
end;

end.
