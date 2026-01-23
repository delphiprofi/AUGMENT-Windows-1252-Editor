Unit eAktenSystem.Hook;

interface

{$IF Not(Defined(UNICODE)) or Defined(CONSOLE_TESTRUNNER)}
Uses
  SysUtils
, eAktenSystem.Intf
, eAktenSystem.D2007
, Dek
;

procedure StoreDocumentToEAkte( const aPDFFilename : String;
                                const aHeader      : tSpoolHeader ); overload;

procedure StoreDocumentToEAkte( const aPDFFilename : String;
                                const aNrTypEx     : TNrTypEx ); overload;

procedure StoreDocumentToEAkte( const aPDFFilename : String;
                                      aIsDRI       : Boolean;
                                const aDRNr        : Nr_Typ ); overload;
{$IFEND}

implementation

{$IF Not(Defined(UNICODE)) or Defined(CONSOLE_TESTRUNNER)}
procedure StoreDocumentToEAkte( const aPDFFilename : String;
                                const aHeader      : tSpoolHeader );
begin
  if not AlloweAkte then
    Exit;

  if not FileExists( aPDFFilename ) then
    Exit;

  try
    TeAktenSystem.Speichern( TPDF_To_eAkte.Create( TNrTypEx.Create( aHeader ), aPDFFilename ) );
  except
  end;
end;

procedure StoreDocumentToEAkte( const aPDFFilename : String;
                                const aNrTypEx     : TNrTypEx );
begin
  if not AlloweAkte then
    Exit;

  if not FileExists( aPDFFilename ) then
    Exit;

  if aNrTypEx.Nr <= 0 then
    Exit;

  try
    TeAktenSystem.Speichern( TPDF_To_eAkte.Create( aNrTypEx, aPDFFilename ) );
  except
  end;
end;

procedure StoreDocumentToEAkte( const aPDFFilename : String;
                                      aIsDRI       : Boolean;
                                const aDRNr        : Nr_Typ );
begin
  StoreDocumentToEAkte( aPDFFilename, TNrTypEx.Create( aIsDRI, aDRNr ) );
end;
{$IFEND}

end.
