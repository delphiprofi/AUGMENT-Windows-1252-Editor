Unit StrEditor.Settings;

{***************************************************************************
 * StrEditor Settings
 *
 * Lädt Einstellungen aus StrEditor.ini neben der EXE
 ***************************************************************************}

interface

Uses
  System.SysUtils
, System.IniFiles
, System.IOUtils
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Globale Einstellungen aus StrEditor.ini
  /// </summary>
  {$ENDREGION}
  TStrEditorSettings = class
    strict private
      class var fInstance : TStrEditorSettings;
      class var fLoaded   : Boolean;

    private
      // ChangeReport Settings
      fChangeReportEnabled      : Boolean;
      fChangeReportShowContent  : Boolean;
      fContextLinesBefore       : Integer;
      fContextLinesAfter        : Integer;

      // SessionLog Settings
      fSessionLogEnabled        : Boolean;
      fSessionLogPath           : string;

      procedure LoadFromIni;
      function GetIniPath : string;
      function ReadIniBool( aIni : TIniFile; const aSection, aKey : string; aDefault : Boolean ) : Boolean;

    public
      class function Instance : TStrEditorSettings;
      class procedure ReleaseInstance;

      // ChangeReport
      property ChangeReportEnabled     : Boolean read fChangeReportEnabled;
      property ChangeReportShowContent : Boolean read fChangeReportShowContent;
      property ContextLinesBefore      : Integer read fContextLinesBefore;
      property ContextLinesAfter       : Integer read fContextLinesAfter;

      // SessionLog
      property SessionLogEnabled       : Boolean read fSessionLogEnabled;
      property SessionLogPath          : string  read fSessionLogPath;
  end;

implementation

{ TStrEditorSettings }

class function TStrEditorSettings.Instance : TStrEditorSettings;
begin
  if fInstance = NIL then
    begin
      fInstance := TStrEditorSettings.Create;
      fInstance.LoadFromIni;
    end;

  Result := fInstance;
end;

class procedure TStrEditorSettings.ReleaseInstance;
begin
  FreeAndNIL( fInstance );
  fLoaded := false;
end;

function TStrEditorSettings.GetIniPath : string;
Var
  lExePath : string;
begin
  lExePath := ExtractFilePath( ParamStr( 0 ) );
  Result   := TPath.Combine( lExePath, 'StrEditor.ini' );
end;

function TStrEditorSettings.ReadIniBool( aIni : TIniFile; const aSection, aKey : string; aDefault : Boolean ) : Boolean;
Var
  lValue : string;
begin
  // TIniFile.ReadBool akzeptiert nur 0/1, nicht true/false
  // Diese Funktion akzeptiert: true, false, 1, 0 (case-insensitive)
  if aDefault
    then lValue := aIni.ReadString( aSection, aKey, '1' )
    else lValue := aIni.ReadString( aSection, aKey, '0' );

  Result := SameText( lValue, 'true' ) or ( lValue = '1' );
end;

procedure TStrEditorSettings.LoadFromIni;
Var
  lIni     : TIniFile;
  lIniPath : string;
begin
  // Defaults - ChangeReport
  fChangeReportEnabled     := true;   // Standardmäßig aktiviert
  fChangeReportShowContent := true;   // Zeige Inhalt der Änderungen
  fContextLinesBefore      := 0;      // Keine Kontext-Zeilen vorher (default)
  fContextLinesAfter       := 0;      // Keine Kontext-Zeilen nachher (default)

  // Defaults - SessionLog
  fSessionLogEnabled       := false;  // Standardmäßig deaktiviert
  fSessionLogPath          := '';     // Kein Log-Pfad

  lIniPath := GetIniPath;

  if not FileExists( lIniPath ) then
    Exit;

  lIni := TIniFile.Create( lIniPath );

  try
    // ChangeReport
    fChangeReportEnabled     := ReadIniBool( lIni, 'ChangeReport', 'Enabled', true );
    fChangeReportShowContent := ReadIniBool( lIni, 'ChangeReport', 'ShowContent', true );
    fContextLinesBefore      := lIni.ReadInteger( 'ChangeReport', 'ContextLinesBefore', 0 );
    fContextLinesAfter       := lIni.ReadInteger( 'ChangeReport', 'ContextLinesAfter', 0 );

    // SessionLog
    fSessionLogEnabled       := ReadIniBool( lIni, 'SessionLog', 'Enabled', false );
    fSessionLogPath          := lIni.ReadString( 'SessionLog', 'LogPath', '' );
  finally
    lIni.Free;
  end;

  fLoaded := true;
end;

initialization

finalization
  TStrEditorSettings.ReleaseInstance;

end.

