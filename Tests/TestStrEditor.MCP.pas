Unit TestStrEditor.MCP;

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, System.JSON
, StrEditor.MCP
, StrEditor.Encoding
;

Type
  [TestFixture]
  TTestMCPDecoding = class
    public
      [Test]
      procedure TestDecode_EmptyBytes;

      [Test]
      procedure TestDecode_AsciiOnly_Unchanged;

      [Test]
      procedure TestDecode_AsciiEscapedUmlaut_AlwaysCorrect;

      [Test]
      procedure TestDecode_RawUtf8Umlaut_ShouldYieldU00E4;

      [Test]
      procedure TestDecode_RawUtf8Umlaut_RoundtripToCp1252Byte;
  end;

implementation

// HINWEIS: Diese Unit ist UTF-8 ohne BOM. Delphi liest sie deshalb als
// CP1252, sodass Umlaute in String-Literalen falsch interpretiert wuerden.
// Daher: alle nicht-ASCII Bytes in String-Konstanten ueber Chr(...) bilden.
// Kommentare sind unkritisch (werden vom Compiler verworfen).

{ TTestMCPDecoding }

procedure TTestMCPDecoding.TestDecode_EmptyBytes;
Var
  lBytes : TBytes;
begin
  SetLength( lBytes, 0 );

  Assert.AreEqual( '', TMCPServer.DecodeIncomingBytes( lBytes ), 'Empty input must yield empty string' );
end;

procedure TTestMCPDecoding.TestDecode_AsciiOnly_Unchanged;
Var
  lBytes  : TBytes;
  lResult : string;
begin
  // {"key":"abc"} - alle Bytes < 0x80, identisch in ASCII / CP1252 / UTF-8
  lBytes := TEncoding.ASCII.GetBytes( '{"key":"abc"}' );

  lResult := TMCPServer.DecodeIncomingBytes( lBytes );

  Assert.AreEqual( '{"key":"abc"}', lResult, 'Pure ASCII must round-trip unchanged' );
end;

procedure TTestMCPDecoding.TestDecode_AsciiEscapedUmlaut_AlwaysCorrect;
Var
  lAsciiJson : string;
  lBytes     : TBytes;
  lJson      : string;
  lValue     : TJSONValue;
  lStr       : string;
begin
  // Client schickt Umlaut als JSON-Escape backslash-u-00e4: alle Pipe-Bytes
  // sind ASCII. Das ist die Variante, die bisher praktisch immer benutzt
  // wurde und deshalb den Decode-Bug nie sichtbar gemacht hat: nach
  // DecodeIncomingBytes steht im String die woertliche Sequenz (6 ASCII-
  // Zeichen), erst der JSON-Parser loest sie zum Codepoint U+00E4 auf.
  //
  // Wir bauen den JSON-Text byte-genau aus ASCII-Codes, damit der
  // Delphi-Compiler keinen Spielraum fuer Encoding-Fehlinterpretation hat.
  lAsciiJson := '{"v":"' + Chr( $5C ) + 'u00e4"}';
  lBytes     := TEncoding.ASCII.GetBytes( lAsciiJson );

  Assert.AreEqual( 14, Length( lBytes ), 'Pre-condition: 14 pure ASCII bytes on the wire' );

  lJson := TMCPServer.DecodeIncomingBytes( lBytes );

  lValue := TJSONObject.ParseJSONValue( lJson );
  try
    Assert.IsNotNull( lValue, 'Escaped-umlaut JSON must parse' );

    lStr := ( lValue as TJSONObject ).GetValue<string>( 'v' );

    Assert.AreEqual( 1,     Length( lStr ),       'Escaped umlaut must decode to exactly one char' );
    Assert.AreEqual( $00E4, Ord( lStr[ 1 ] ),     'Decoded codepoint must be U+00E4' );
  finally
    lValue.Free;
  end;
end;

procedure TTestMCPDecoding.TestDecode_RawUtf8Umlaut_ShouldYieldU00E4;
Var
  lBytes : TBytes;
  lJson  : string;
  lValue : TJSONValue;
  lStr   : string;
begin
  // Client schickt Umlaut als rohe UTF-8-Bytes: 7B 22 76 22 3A 22 C3 A4 22 7D
  // = {"v":"<c3 a4>"} - JSON-Spec erlaubt das ausdruecklich.
  // Erwartung: nach Decode + JSON-Parse muss der String genau EIN Zeichen
  // mit Codepoint U+00E4 enthalten. Heute scheitert das, weil
  // DecodeIncomingBytes per ACP/CP1252 dekodiert und damit aus c3 a4 zwei
  // Codepoints (U+00C3, U+00A4) macht - Mojibake.
  SetLength( lBytes, 10 );
  lBytes[ 0 ] := $7B; // {
  lBytes[ 1 ] := $22; // "
  lBytes[ 2 ] := $76; // v
  lBytes[ 3 ] := $22; // "
  lBytes[ 4 ] := $3A; // :
  lBytes[ 5 ] := $22; // "
  lBytes[ 6 ] := $C3; // UTF-8 Byte 1 fuer U+00E4
  lBytes[ 7 ] := $A4; // UTF-8 Byte 2 fuer U+00E4
  lBytes[ 8 ] := $22; // "
  lBytes[ 9 ] := $7D; // }

  lJson := TMCPServer.DecodeIncomingBytes( lBytes );

  lValue := TJSONObject.ParseJSONValue( lJson );
  try
    Assert.IsNotNull( lValue, 'Raw-UTF-8-umlaut JSON must parse' );

    lStr := ( lValue as TJSONObject ).GetValue<string>( 'v' );

    Assert.AreEqual( 1,     Length( lStr ),       'Raw UTF-8 umlaut must decode to exactly one char, not two (no Mojibake)' );
    Assert.AreEqual( $00E4, Ord( lStr[ 1 ] ),     'Decoded codepoint must be U+00E4, not U+00C3' );
  finally
    lValue.Free;
  end;
end;

procedure TTestMCPDecoding.TestDecode_RawUtf8Umlaut_RoundtripToCp1252Byte;
Var
  lBytes      : TBytes;
  lJson       : string;
  lValue      : TJSONValue;
  lStr        : string;
  lCp1252     : TBytes;
begin
  // End-to-End-Reproduktion des konkreten in-the-wild Symptoms:
  // 1. Client sendet rohe UTF-8 Bytes c3 a4 fuer einen Umlaut
  // 2. Server dekodiert (Bug: per ACP -> zwei Codepoints)
  // 3. Server ruft StringToWindows1252 fuer CP1252-Datei auf
  // 4. Ergebnis: zwei Bytes c3 a4 landen in der CP1252-Datei (= Mojibake)
  //
  // Erwartung nach Fix: ein Byte $E4 in der CP1252-Datei.
  SetLength( lBytes, 10 );
  lBytes[ 0 ] := $7B;
  lBytes[ 1 ] := $22;
  lBytes[ 2 ] := $76;
  lBytes[ 3 ] := $22;
  lBytes[ 4 ] := $3A;
  lBytes[ 5 ] := $22;
  lBytes[ 6 ] := $C3;
  lBytes[ 7 ] := $A4;
  lBytes[ 8 ] := $22;
  lBytes[ 9 ] := $7D;

  lJson  := TMCPServer.DecodeIncomingBytes( lBytes );
  lValue := TJSONObject.ParseJSONValue( lJson );
  try
    Assert.IsNotNull( lValue, 'JSON must parse' );
    lStr := ( lValue as TJSONObject ).GetValue<string>( 'v' );
  finally
    lValue.Free;
  end;

  lCp1252 := TEncodingHelper.StringToWindows1252( lStr );

  Assert.AreEqual( 1,    Length( lCp1252 ),       'CP1252-encoded umlaut must be exactly 1 byte, not 2 (no Mojibake in file)' );
  Assert.AreEqual( $E4,  lCp1252[ 0 ],            'CP1252-encoded umlaut must be byte $E4' );
end;

initialization
  TDUnitX.RegisterTestFixture( TTestMCPDecoding );

end.
