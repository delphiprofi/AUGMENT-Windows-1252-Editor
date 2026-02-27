Unit StrEditor.Encoding;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

Uses
  System.SysUtils
, System.Classes
, Winapi.Windows
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Encoding-Typ für Dateien
  /// </summary>
  {$ENDREGION}
  TEncodingType = ( etUnknown, etWindows1252, etUTF8 );

  {$REGION 'Documentation'}
  /// <summary>
  ///   Encoding-Erkennung und Lesen/Schreiben mit Encoding-Preservation
  /// </summary>
  {$ENDREGION}
  TEncodingHelper = class
    strict private
      class function HasUTF8BOM( const aBytes : TBytes ) : Boolean;
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Erkennt das Encoding einer Datei anhand des BOM
      /// </summary>
      {$ENDREGION}
      class function DetectEncoding( const aFilePath : string ) : TEncodingType;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Liest eine Datei und gibt die Zeilen sowie das erkannte Encoding zurück
      /// </summary>
      {$ENDREGION}
      class function ReadFile( const aFilePath : string; out aLines : TStringList; out aEncoding : TEncodingType ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Liest eine Datei mit Retry-Mechanismus (3 Versuche, 100ms Delay)
      ///   Fängt File-Access-Errors ab: EInOutError, EFOpenError, EFCreateError
      /// </summary>
      {$ENDREGION}
      class function ReadFileWithRetry( const aFilePath : string; out aLines : TStringList; out aEncoding : TEncodingType ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Schreibt Zeilen in eine Datei mit dem angegebenen Encoding
      /// </summary>
      {$ENDREGION}
      class function WriteFile( const aFilePath : string; const aLines : TStringList; const aEncoding : TEncodingType ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Schreibt eine Datei mit Retry-Mechanismus (3 Versuche, 100ms Delay)
      ///   Fängt File-Access-Errors ab: EInOutError, EFOpenError, EFCreateError
      /// </summary>
      {$ENDREGION}
      class function WriteFileWithRetry( const aFilePath : string; const aLines : TStringList; const aEncoding : TEncodingType ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Konvertiert Windows-1252 Bytes zu String
      /// </summary>
      {$ENDREGION}
      class function Windows1252ToString( const aBytes : TBytes ) : string;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Konvertiert String zu Windows-1252 Bytes
      /// </summary>
      {$ENDREGION}
      class function StringToWindows1252( const aStr : string ) : TBytes;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Konvertiert UTF-8 Bytes zu String
      /// </summary>
      {$ENDREGION}
      class function UTF8ToString( const aBytes : TBytes ) : string;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Konvertiert String zu UTF-8 Bytes (mit BOM)
      /// </summary>
      {$ENDREGION}
      class function StringToUTF8( const aStr : string ) : TBytes;
  end;

implementation

{ TEncodingHelper }

class function TEncodingHelper.HasUTF8BOM( const aBytes : TBytes ) : Boolean;
begin
  Result := ( Length( aBytes ) >= 3 ) and
            ( aBytes[ 0 ] = $EF ) and
            ( aBytes[ 1 ] = $BB ) and
            ( aBytes[ 2 ] = $BF );
end;

class function TEncodingHelper.ReadFileWithRetry( const aFilePath : string; out aLines : TStringList; out aEncoding : TEncodingType ) : Boolean;
const
  cMaxRetries = 3;
  cRetryDelay = 100; // Millisekunden
Var
  lAttempt     : Integer;
  lLastError   : string;
  lIsFileError : Boolean;
begin
  Result := false;

  for lAttempt := 1 to cMaxRetries do
    begin
      try
        Result := ReadFile( aFilePath, aLines, aEncoding );
        Exit; // Erfolg - keine weiteren Versuche nötig
      except
        on E : EInOutError do
          begin
            lIsFileError := true;
            lLastError   := E.Message;
          end;
        on E : EFOpenError do
          begin
            lIsFileError := true;
            lLastError   := E.Message;
          end;
        on E : EFCreateError do
          begin
            lIsFileError := true;
            lLastError   := E.Message;
          end;
        on E : Exception do
          begin
            // Andere Exceptions werden nicht wiederholt
            lIsFileError := false;
            lLastError   := E.Message;
          end;
      end;

      // Wenn es kein File-Error war, nicht wiederholen
      if not lIsFileError then
        begin
          WriteLn( ErrOutput, 'ERROR: ReadFile failed: ' + aFilePath + ': ' + lLastError );
          Exit;
        end;

      // Wenn es der letzte Versuch war, Fehler ausgeben
      if lAttempt = cMaxRetries then
        begin
          WriteLn( ErrOutput, 'ERROR: ReadFile failed: ' + aFilePath + ': ' + lLastError + ' (after ' + IntToStr( cMaxRetries ) + ' retries)' );
          Exit;
        end;

      // Warte vor dem nächsten Versuch
      Sleep( cRetryDelay );
    end;
end;

class function TEncodingHelper.WriteFileWithRetry( const aFilePath : string; const aLines : TStringList; const aEncoding : TEncodingType ) : Boolean;
const
  cMaxRetries = 3;
  cRetryDelay = 100; // Millisekunden
Var
  lAttempt     : Integer;
  lLastError   : string;
  lIsFileError : Boolean;
begin
  Result := false;

  for lAttempt := 1 to cMaxRetries do
    begin
      try
        Result := WriteFile( aFilePath, aLines, aEncoding );

        if Result then
          Exit; // Erfolg - nur bei Erfolg beenden

        // WriteFile hat false zurückgegeben ohne Exception - als retriable behandeln
        lIsFileError := true;
        lLastError   := 'Write operation returned false';
      except
        on E : EInOutError do
          begin
            lIsFileError := true;
            lLastError   := E.Message;
          end;
        on E : EFOpenError do
          begin
            lIsFileError := true;
            lLastError   := E.Message;
          end;
        on E : EFCreateError do
          begin
            lIsFileError := true;
            lLastError   := E.Message;
          end;
        on E : Exception do
          begin
            // Andere Exceptions werden nicht wiederholt
            lIsFileError := false;
            lLastError   := E.Message;
          end;
      end;

      // Wenn es kein File-Error war, nicht wiederholen
      if not lIsFileError then
        begin
          WriteLn( ErrOutput, 'ERROR: WriteFile failed: ' + aFilePath + ': ' + lLastError );
          Exit;
        end;

      // Wenn es der letzte Versuch war, Fehler ausgeben
      if lAttempt = cMaxRetries then
        begin
          WriteLn( ErrOutput, 'ERROR: WriteFile failed: ' + aFilePath + ': ' + lLastError + ' (after ' + IntToStr( cMaxRetries ) + ' retries)' );
          Exit;
        end;

      // Warte vor dem nächsten Versuch
      Sleep( cRetryDelay );
    end;
end;

class function TEncodingHelper.DetectEncoding( const aFilePath : string ) : TEncodingType;
Var
  lFileStream : TFileStream;
  lBytes      : TBytes;
begin
  Result := etUnknown;

  if not FileExists( aFilePath ) then
    Exit;

  lFileStream := NIL;

  try
    try
      lFileStream := TFileStream.Create( aFilePath, fmOpenRead or fmShareDenyWrite );

      if lFileStream.Size >= 3 then
        begin
          SetLength( lBytes, 3 );
          lFileStream.Read( lBytes[ 0 ], 3 );

          if HasUTF8BOM( lBytes )
            then Result := etUTF8
            else Result := etWindows1252;
        end
      else Result := etWindows1252;
    except
      on E : Exception do
        begin
          Result := etUnknown;
          // Exception wird nicht weitergegeben - Caller prüft Result
        end;
    end;
  finally
    // FileStream IMMER freigeben, auch bei Exception
    FreeAndNIL( lFileStream );
  end;
end;

class function TEncodingHelper.ReadFile( const aFilePath : string; out aLines : TStringList; out aEncoding : TEncodingType ) : Boolean;
Var
  lFileStream       : TFileStream;
  lBytes            : TBytes;
  lContent          : string;
  lHasTrailingBreak : Boolean;
begin
  Result    := false;
  aLines    := NIL;
  aEncoding := etUnknown;

  if not FileExists( aFilePath ) then
    Exit;

  aEncoding := DetectEncoding( aFilePath );

  lFileStream := NIL;

  try
    try
      lFileStream := TFileStream.Create( aFilePath, fmOpenRead or fmShareDenyWrite );

      SetLength( lBytes, lFileStream.Size );

      if lFileStream.Size > 0 then
        lFileStream.Read( lBytes[ 0 ], lFileStream.Size );

      if aEncoding = etUTF8
        then lContent := UTF8ToString( lBytes )
        else lContent := Windows1252ToString( lBytes );

      // Prüfe ob Original einen Trailing Line Break hat
      lHasTrailingBreak := ( Length( lContent ) >= 2 ) and
                           ( lContent[ Length( lContent ) - 1 ] = #13 ) and
                           ( lContent[ Length( lContent ) ] = #10 );

      aLines                   := TStringList.Create;
      aLines.TrailingLineBreak := lHasTrailingBreak;
      aLines.Text              := lContent;
      Result                   := true;
    except
      on E : Exception do
        begin
          // Bei Exception: aLines freigeben falls bereits erstellt
          FreeAndNIL( aLines );
          Result := false;
          // Exception wird nicht weitergegeben - Caller prüft Result
        end;
    end;
  finally
    // FileStream IMMER freigeben, auch bei Exception
    FreeAndNIL( lFileStream );
  end;
end;

class function TEncodingHelper.WriteFile( const aFilePath : string; const aLines : TStringList; const aEncoding : TEncodingType ) : Boolean;
Var
  lFileStream : TFileStream;
  lBytes      : TBytes;
  lContent    : string;
begin
  Result := false;

  if aLines = NIL then
    Exit;

  lContent := aLines.Text;

  if aEncoding = etUTF8
    then lBytes := StringToUTF8( lContent )
    else lBytes := StringToWindows1252( lContent );

  lFileStream := NIL;

  try
    // Exceptions werden bewusst NICHT gefangen - WriteFileWithRetry soll sie abfangen und ggf. wiederholen
    lFileStream := TFileStream.Create( aFilePath, fmCreate );

    if Length( lBytes ) > 0 then
      lFileStream.Write( lBytes[ 0 ], Length( lBytes ) );

    Result := true;
  finally
    // FileStream IMMER freigeben, auch bei Exception
    FreeAndNIL( lFileStream );
  end;
end;

class function TEncodingHelper.Windows1252ToString( const aBytes : TBytes ) : string;
Var
  lWideCharCount : Integer;
  lWideStr       : WideString;
begin
  if Length( aBytes ) = 0 then
    begin
      Result := '';
      Exit;
    end;

  lWideCharCount := MultiByteToWideChar( 1252, 0, @aBytes[ 0 ], Length( aBytes ), NIL, 0 );

  SetLength( lWideStr, lWideCharCount );

  MultiByteToWideChar( 1252, 0, @aBytes[ 0 ], Length( aBytes ), PWideChar( lWideStr ), lWideCharCount );

  Result := lWideStr;
end;

class function TEncodingHelper.StringToWindows1252( const aStr : string ) : TBytes;
Var
  lByteCount : Integer;
begin
  if Length( aStr ) = 0 then
    begin
      SetLength( Result, 0 );
      Exit;
    end;

  lByteCount := WideCharToMultiByte( 1252, 0, PWideChar( aStr ), Length( aStr ), NIL, 0, NIL, NIL );

  SetLength( Result, lByteCount );

  WideCharToMultiByte( 1252, 0, PWideChar( aStr ), Length( aStr ), PAnsiChar( @Result[ 0 ] ), lByteCount, NIL, NIL );
end;

class function TEncodingHelper.UTF8ToString( const aBytes : TBytes ) : string;
Var
  lStartIndex : Integer;
  lUTF8Str    : UTF8String;
begin
  if HasUTF8BOM( aBytes )
    then lStartIndex := 3
    else lStartIndex := 0;

  SetLength( lUTF8Str, Length( aBytes ) - lStartIndex );

  if Length( lUTF8Str ) > 0 then
    Move( aBytes[ lStartIndex ], lUTF8Str[ 1 ], Length( lUTF8Str ) );

  Result := UTF8ToUnicodeString( lUTF8Str );
end;

class function TEncodingHelper.StringToUTF8( const aStr : string ) : TBytes;
Var
  lUTF8Str : UTF8String;
begin
  lUTF8Str := UTF8Encode( aStr );

  SetLength( Result, 3 + Length( lUTF8Str ) );
  Result[ 0 ] := $EF;
  Result[ 1 ] := $BB;
  Result[ 2 ] := $BF;

  if Length( lUTF8Str ) > 0 then
    Move( lUTF8Str[ 1 ], Result[ 3 ], Length( lUTF8Str ) );
end;

end.

