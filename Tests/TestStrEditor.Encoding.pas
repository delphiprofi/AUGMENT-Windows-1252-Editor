Unit TestStrEditor.Encoding;

interface

Uses
  DUnitX.TestFramework
, System.SysUtils
, System.Classes
, StrEditor.Encoding
;

Type
  [TestFixture]
  TTestEncodingHelper = class
    strict private
      fTestFilePath : string;
    public
      [Setup]
      procedure Setup;

      [TearDown]
      procedure TearDown;

      [Test]
      procedure TestDetectEncoding_UTF8;

      [Test]
      procedure TestDetectEncoding_Windows1252;

      [Test]
      procedure TestReadFile_UTF8;

      [Test]
      procedure TestReadFile_Windows1252;

      [Test]
      procedure TestWriteFile_UTF8;

      [Test]
      procedure TestWriteFile_Windows1252;

      [Test]
      procedure TestEncodingPreservation_UTF8;

      [Test]
      procedure TestEncodingPreservation_Windows1252;

      [Test]
      procedure TestUmlautPreservation_Windows1252;

      [Test]
      procedure TestUmlautPreservation_UTF8;
  end;

implementation

{ TTestEncodingHelper }

procedure TTestEncodingHelper.Setup;
begin
  fTestFilePath := 'test_encoding.tmp';
end;

procedure TTestEncodingHelper.TearDown;
begin
  if FileExists( fTestFilePath ) then
    DeleteFile( fTestFilePath );
end;

procedure TTestEncodingHelper.TestDetectEncoding_UTF8;
Var
  lFileStream : TFileStream;
  lBOM        : array[ 0..2 ] of Byte;
  lEncoding   : TEncodingType;
begin
  lFileStream := TFileStream.Create( fTestFilePath, fmCreate );
  try
    lBOM[ 0 ] := $EF;
    lBOM[ 1 ] := $BB;
    lBOM[ 2 ] := $BF;
    lFileStream.Write( lBOM[ 0 ], 3 );
  finally
    lFileStream.Free;
  end;

  lEncoding := TEncodingHelper.DetectEncoding( fTestFilePath );

  Assert.AreEqual( etUTF8, lEncoding, 'UTF-8 BOM should be detected' );
end;

procedure TTestEncodingHelper.TestDetectEncoding_Windows1252;
Var
  lFileStream : TFileStream;
  lData       : Byte;
  lEncoding   : TEncodingType;
begin
  lFileStream := TFileStream.Create( fTestFilePath, fmCreate );
  try
    lData := $41;
    lFileStream.Write( lData, 1 );
  finally
    lFileStream.Free;
  end;

  lEncoding := TEncodingHelper.DetectEncoding( fTestFilePath );

  Assert.AreEqual( etWindows1252, lEncoding, 'Windows-1252 should be detected (no BOM)' );
end;

procedure TTestEncodingHelper.TestReadFile_UTF8;
Var
  lLines    : TStringList;
  lEncoding : TEncodingType;
  lBytes    : TBytes;
begin
  lBytes := TEncodingHelper.StringToUTF8( 'Test Line 1' + #13#10 + 'Test Line 2' );

  Var lFileStream := TFileStream.Create( fTestFilePath, fmCreate );
  try
    lFileStream.Write( lBytes[ 0 ], Length( lBytes ) );
  finally
    lFileStream.Free;
  end;

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'ReadFile should succeed' );
  try
    Assert.AreEqual( etUTF8, lEncoding, 'Encoding should be UTF-8' );
    Assert.AreEqual( 2, lLines.Count, 'Should have 2 lines' );
    Assert.AreEqual( 'Test Line 1', lLines[ 0 ], 'First line should match' );
    Assert.AreEqual( 'Test Line 2', lLines[ 1 ], 'Second line should match' );
  finally
    lLines.Free;
  end;
end;

procedure TTestEncodingHelper.TestReadFile_Windows1252;
Var
  lLines    : TStringList;
  lEncoding : TEncodingType;
  lBytes    : TBytes;
begin
  lBytes := TEncodingHelper.StringToWindows1252( 'Test Line 1' + #13#10 + 'Test Line 2' );

  Var lFileStream := TFileStream.Create( fTestFilePath, fmCreate );
  try
    lFileStream.Write( lBytes[ 0 ], Length( lBytes ) );
  finally
    lFileStream.Free;
  end;

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'ReadFile should succeed' );
  try
    Assert.AreEqual( etWindows1252, lEncoding, 'Encoding should be Windows-1252' );
  finally
    lLines.Free;
  end;
end;

procedure TTestEncodingHelper.TestWriteFile_UTF8;
Var
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  lLines := TStringList.Create;
  try
    lLines.Add( 'Test Line 1' );
    lLines.Add( 'Test Line 2' );

    Assert.IsTrue( TEncodingHelper.WriteFile( fTestFilePath, lLines, etUTF8 ), 'WriteFile should succeed' );
  finally
    lLines.Free;
  end;

  lEncoding := TEncodingHelper.DetectEncoding( fTestFilePath );

  Assert.AreEqual( etUTF8, lEncoding, 'File should have UTF-8 encoding' );
end;

procedure TTestEncodingHelper.TestWriteFile_Windows1252;
Var
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  lLines := TStringList.Create;
  try
    lLines.Add( 'Test Line 1' );
    lLines.Add( 'Test Line 2' );

    Assert.IsTrue( TEncodingHelper.WriteFile( fTestFilePath, lLines, etWindows1252 ), 'WriteFile should succeed' );
  finally
    lLines.Free;
  end;

  lEncoding := TEncodingHelper.DetectEncoding( fTestFilePath );

  Assert.AreEqual( etWindows1252, lEncoding, 'File should have Windows-1252 encoding' );
end;

procedure TTestEncodingHelper.TestEncodingPreservation_UTF8;
Var
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  lLines := TStringList.Create;
  try
    lLines.Add( 'Test Line 1' );
    TEncodingHelper.WriteFile( fTestFilePath, lLines, etUTF8 );
  finally
    lLines.Free;
  end;

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'ReadFile should succeed' );
  try
    lLines[ 0 ] := 'Modified Line 1';
    TEncodingHelper.WriteFile( fTestFilePath, lLines, lEncoding );
  finally
    lLines.Free;
  end;

  lEncoding := TEncodingHelper.DetectEncoding( fTestFilePath );

  Assert.AreEqual( etUTF8, lEncoding, 'Encoding should be preserved as UTF-8' );
end;

procedure TTestEncodingHelper.TestEncodingPreservation_Windows1252;
Var
  lLines    : TStringList;
  lEncoding : TEncodingType;
begin
  lLines := TStringList.Create;
  try
    lLines.Add( 'Test Line 1' );
    TEncodingHelper.WriteFile( fTestFilePath, lLines, etWindows1252 );
  finally
    lLines.Free;
  end;

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'ReadFile should succeed' );
  try
    lLines[ 0 ] := 'Modified Line 1';
    TEncodingHelper.WriteFile( fTestFilePath, lLines, lEncoding );
  finally
    lLines.Free;
  end;

  lEncoding := TEncodingHelper.DetectEncoding( fTestFilePath );

  Assert.AreEqual( etWindows1252, lEncoding, 'Encoding should be preserved as Windows-1252' );
end;

procedure TTestEncodingHelper.TestUmlautPreservation_Windows1252;
Var
  lBytes     : TBytes;
  lLines     : TStringList;
  lEncoding  : TEncodingType;
  lReadBytes : TBytes;
begin
  SetLength( lBytes, 20 );
  lBytes[ 0 ]  := Ord( 'D' );
  lBytes[ 1 ]  := Ord( 'a' );
  lBytes[ 2 ]  := Ord( 't' );
  lBytes[ 3 ]  := Ord( 'e' );
  lBytes[ 4 ]  := Ord( 'n' );
  lBytes[ 5 ]  := Ord( 'r' );
  lBytes[ 6 ]  := $FC;
  lBytes[ 7 ]  := Ord( 'c' );
  lBytes[ 8 ]  := Ord( 'k' );
  lBytes[ 9 ]  := Ord( 's' );
  lBytes[ 10 ] := Ord( 'i' );
  lBytes[ 11 ] := Ord( 'c' );
  lBytes[ 12 ] := Ord( 'h' );
  lBytes[ 13 ] := Ord( 'e' );
  lBytes[ 14 ] := Ord( 'r' );
  lBytes[ 15 ] := Ord( 'u' );
  lBytes[ 16 ] := Ord( 'n' );
  lBytes[ 17 ] := Ord( 'g' );
  lBytes[ 18 ] := $0D;
  lBytes[ 19 ] := $0A;

  Var lFileStream := TFileStream.Create( fTestFilePath, fmCreate );
  try
    lFileStream.Write( lBytes[ 0 ], Length( lBytes ) );
  finally
    lFileStream.Free;
  end;

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lLines, lEncoding ), 'ReadFile should succeed' );
  try
    lLines[ 0 ] := lLines[ 0 ] + ' modified';
    TEncodingHelper.WriteFile( fTestFilePath, lLines, lEncoding );
  finally
    lLines.Free;
  end;

  lFileStream := TFileStream.Create( fTestFilePath, fmOpenRead );
  try
    SetLength( lReadBytes, lFileStream.Size );
    lFileStream.Read( lReadBytes[ 0 ], lFileStream.Size );
  finally
    lFileStream.Free;
  end;

  Var lFound := false;

  for Var i := 0 to Length( lReadBytes ) - 1 do
    begin
      if lReadBytes[ i ] = $FC then
        begin
          lFound := true;
          Break;
        end;
    end;

  Assert.IsTrue( lFound, 'Umlaut ü should be preserved as $FC in Windows-1252' );
end;

procedure TTestEncodingHelper.TestUmlautPreservation_UTF8;
Var
  lLines        : TStringList;
  lReadLines    : TStringList;
  lReadEncoding : TEncodingType;
begin
  lLines := TStringList.Create;
  try
    lLines.Add( 'Datenrücksicherung' );
    TEncodingHelper.WriteFile( fTestFilePath, lLines, etUTF8 );
  finally
    lLines.Free;
  end;

  Assert.IsTrue( TEncodingHelper.ReadFile( fTestFilePath, lReadLines, lReadEncoding ), 'ReadFile should succeed' );
  try
    Assert.AreEqual( etUTF8, lReadEncoding, 'Encoding should be UTF-8' );
    Assert.AreEqual( 'Datenrücksicherung', lReadLines[ 0 ], 'Umlauts should be preserved in UTF-8' );
  finally
    lReadLines.Free;
  end;
end;

end.

