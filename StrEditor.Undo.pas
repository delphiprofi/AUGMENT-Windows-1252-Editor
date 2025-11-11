Unit StrEditor.Undo;

interface

Uses
  System.SysUtils
, System.Classes
, Winapi.Windows
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Undo Helper
  /// </summary>
  {$ENDREGION}
  TUndoHelper = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Stellt die Backup-Datei wieder her
      /// </summary>
      {$ENDREGION}
      class function UndoChanges( const aFilePath : string; const aVerbose : Boolean = false ) : Boolean;
  end;

implementation

class function TUndoHelper.UndoChanges( const aFilePath : string; const aVerbose : Boolean = false ) : Boolean;
Var
  lBackupPath : string;
begin
  Result := false;

  if not FileExists( aFilePath ) then
    begin
      WriteLn( 'Error: File not found: ' + aFilePath );
      Exit;
    end;

  lBackupPath := aFilePath + '.bak';

  if not FileExists( lBackupPath ) then
    begin
      WriteLn( 'Error: Backup file not found: ' + lBackupPath );
      Exit;
    end;

  try
    if aVerbose then
      WriteLn( 'Restoring backup: ' + lBackupPath + ' -> ' + aFilePath );

    System.SysUtils.DeleteFile( aFilePath );
    System.SysUtils.RenameFile( lBackupPath, aFilePath );

    if aVerbose then
      WriteLn( 'Undo successful' );

    Result := true;
  except
    on E : Exception do
      begin
        WriteLn( 'Error: Failed to restore backup: ' + E.Message );
        Result := false;
      end;
  end;
end;

end.

