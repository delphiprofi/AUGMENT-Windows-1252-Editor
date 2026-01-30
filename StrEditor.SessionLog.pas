Unit StrEditor.SessionLog;

{***************************************************************************
 * StrEditor Session Log
 *
 * Protokolliert alle Operationen für Fehleranalyse
 * Format: Timestamp|Type|Base64-Data
 ***************************************************************************}

interface

Uses
  System.SysUtils
, System.Classes
, System.IOUtils
, System.NetEncoding
, System.SyncObjs
;

Type
  TLogType = ( ltView, ltConfig, ltError, ltSuccess, ltWarning );

  {$REGION 'Documentation'}
  /// <summary>
  ///   Thread-safe Session Logger - Singleton
  /// </summary>
  {$ENDREGION}
  TSessionLog = class
    strict private
      class var fInstance : TSessionLog;
      class var fLock     : TCriticalSection;

    private
      fEnabled  : Boolean;
      fLogPath  : string;
      fLogFile  : TStreamWriter;

      procedure OpenLogFile;
      procedure CloseLogFile;
      function GetLogTypeName( aType : TLogType ) : string;
      function EncodeBase64( const aText : string ) : string;

    public
      constructor Create;
      destructor  Destroy; override;

      class function Instance : TSessionLog;
      class procedure ReleaseInstance;

      procedure Log( aType : TLogType; const aData : string );
      procedure LogView( const aFilePath : string; aStartLine, aEndLine : Integer );
      procedure LogConfig( const aJsonConfig : string );
      procedure LogError( const aMessage : string );
      procedure LogSuccess( const aMessage : string );
      procedure LogWarning( const aMessage : string );

      property Enabled : Boolean read fEnabled write fEnabled;
      property LogPath : string  read fLogPath write fLogPath;
  end;

implementation

Uses
  StrEditor.Settings
;

{ TSessionLog }

class function TSessionLog.Instance : TSessionLog;
begin
  if fLock = NIL then
    fLock := TCriticalSection.Create;

  fLock.Enter;

  try
    if fInstance = NIL then
      fInstance := TSessionLog.Create;

    Result := fInstance;
  finally
    fLock.Leave;
  end;
end;

class procedure TSessionLog.ReleaseInstance;
begin
  if fLock <> NIL then
    begin
      fLock.Enter;

      try
        FreeAndNIL( fInstance );
      finally
        fLock.Leave;
      end;

      FreeAndNIL( fLock );
    end;
end;

constructor TSessionLog.Create;
Var
  lSettings : TStrEditorSettings;
begin
  inherited Create;

  fLogFile := NIL;

  // Lade Settings
  lSettings := TStrEditorSettings.Instance;
  fEnabled  := lSettings.SessionLogEnabled;
  fLogPath  := lSettings.SessionLogPath;

  if fEnabled and ( fLogPath <> '' ) then
    OpenLogFile;
end;

destructor TSessionLog.Destroy;
begin
  CloseLogFile;

  inherited Destroy;
end;

procedure TSessionLog.OpenLogFile;
Var
  lFullPath : string;
  lDir      : string;
begin
  if fLogPath = '' then
    Exit;

  // Relativer Pfad -> neben EXE
  if not TPath.IsPathRooted( fLogPath ) then
    lFullPath := TPath.Combine( ExtractFilePath( ParamStr( 0 ) ), fLogPath )
  else
    lFullPath := fLogPath;

  // Verzeichnis erstellen falls nötig
  lDir := ExtractFilePath( lFullPath );

  if ( lDir <> '' ) and not TDirectory.Exists( lDir ) then
    TDirectory.CreateDirectory( lDir );

  // Datei öffnen (append)
  try
    fLogFile := TStreamWriter.Create( lFullPath, true, TEncoding.UTF8 );
  except
    // Fehler beim Öffnen ignorieren - Logging ist optional
  end;
end;

procedure TSessionLog.CloseLogFile;
begin
  FreeAndNIL( fLogFile );
end;

function TSessionLog.GetLogTypeName( aType : TLogType ) : string;
begin
  case aType of
    ltView    : Result := 'VIEW';
    ltConfig  : Result := 'CONFIG';
    ltError   : Result := 'ERROR';
    ltSuccess : Result := 'SUCCESS';
    ltWarning : Result := 'WARNING';
    else        Result := 'UNKNOWN';
  end;
end;

function TSessionLog.EncodeBase64( const aText : string ) : string;
begin
  Result := TNetEncoding.Base64.Encode( aText );
  // Zeilenumbrüche aus Base64 entfernen (bei langen Strings)
  Result := Result.Replace( #13, '' ).Replace( #10, '' );
end;

procedure TSessionLog.Log( aType : TLogType; const aData : string );
Var
  lLine : string;
begin
  if ( not fEnabled ) or ( fLogFile = NIL ) then
    Exit;

  if fLock <> NIL then
    fLock.Enter;

  try
    lLine := FormatDateTime( 'yyyy-mm-dd hh:nn:ss', Now ) + '|' +
             GetLogTypeName( aType ) + '|' +
             EncodeBase64( aData );

    fLogFile.WriteLine( lLine );
    fLogFile.Flush;
  finally
    if fLock <> NIL then
      fLock.Leave;
  end;
end;

procedure TSessionLog.LogView( const aFilePath : string; aStartLine, aEndLine : Integer );
begin
  Log( ltView, aFilePath + '|' + IntToStr( aStartLine ) + '-' + IntToStr( aEndLine ) );
end;

procedure TSessionLog.LogConfig( const aJsonConfig : string );
begin
  Log( ltConfig, aJsonConfig );
end;

procedure TSessionLog.LogError( const aMessage : string );
begin
  Log( ltError, aMessage );
end;

procedure TSessionLog.LogSuccess( const aMessage : string );
begin
  Log( ltSuccess, aMessage );
end;

procedure TSessionLog.LogWarning( const aMessage : string );
begin
  Log( ltWarning, aMessage );
end;

initialization

finalization
  TSessionLog.ReleaseInstance;

end.

