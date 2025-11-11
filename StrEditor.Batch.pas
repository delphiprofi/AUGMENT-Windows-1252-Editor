Unit StrEditor.Batch;

interface

Uses
  System.SysUtils
, System.Classes
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Batch-Processing Helper
  /// </summary>
  {$ENDREGION}
  TBatchProcessor = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Findet alle Dateien die dem Pattern entsprechen
      /// </summary>
      {$ENDREGION}
      class function FindFiles( const aPattern : string ) : TArray<string>;
  end;

implementation

Uses
  System.IOUtils
;

{ TBatchProcessor }

class function TBatchProcessor.FindFiles( const aPattern : string ) : TArray<string>;
Var
  lPath    : string;
  lPattern : string;
  lFiles   : TArray<string>;
begin
  SetLength( lFiles, 0 );

  if aPattern = '' then
    Exit( lFiles );

  lPath := ExtractFilePath( aPattern );

  if lPath = '' then
    lPath := GetCurrentDir;

  lPattern := ExtractFileName( aPattern );

  if lPattern = '' then
    Exit( lFiles );

  try
    lFiles := TDirectory.GetFiles( lPath, lPattern, TSearchOption.soTopDirectoryOnly );
    Result := lFiles;
  except
    SetLength( lFiles, 0 );
    Result := lFiles;
  end;
end;

end.

