Unit StrEditor.CommandLine;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

Uses
  System.SysUtils
, System.Classes
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Command-Typ
  /// </summary>
  {$ENDREGION}
  TCommandType = ( ctUnknown, ctStrReplace, ctInsert, ctRegexReplace, ctRegexTest, ctUndo, ctHelp, ctVersion );

  {$REGION 'Documentation'}
  /// <summary>
  ///   Case Conversion Type
  /// </summary>
  {$ENDREGION}
  TCaseConversion = ( ccNone, ccUpper, ccLower, ccTitle );

  {$REGION 'Documentation'}
  /// <summary>
  ///   Exit-Codes
  /// </summary>
  {$ENDREGION}
  TExitCode = (
    ecSuccess         = 0,
    ecFileNotFound    = 1,
    ecStringNotFound  = 2,
    ecEncodingError   = 3,
    ecParameterError  = 4,
    ecJSONParseError  = 5
  );

  {$REGION 'Documentation'}
  /// <summary>
  ///   Command-Line Parameter
  /// </summary>
  {$ENDREGION}
  TCommandLineParams = record
    Command           : TCommandType;
    FilePath          : string;
    FilePattern       : string;
    OldStr            : string;
    NewStr            : string;
    StartLine         : Integer;
    EndLine           : Integer;
    InsertAfterLine   : Integer;
    Text              : string;
    JSONFile          : string;
    ConfigFile        : string;
    RegexPattern      : string;
    RegexReplace      : string;
    ConditionPattern  : string;
    CaseInsensitive   : Boolean;
    MultiLine         : Boolean;
    Backup            : Boolean;
    DryRun            : Boolean;
    Diff              : Boolean;
    Stats             : Boolean;
    CaseConversion    : TCaseConversion;
    IndentLevel       : Integer;
    Verbose           : Boolean;
  end;

  {$REGION 'Documentation'}
  /// <summary>
  ///   Command-Line Parameter-Parsing
  /// </summary>
  {$ENDREGION}
  TCommandLineParser = class
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Parst die Command-Line Parameter
      /// </summary>
      {$ENDREGION}
      class function Parse( out aParams : TCommandLineParams ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Zeigt die Hilfe an
      /// </summary>
      {$ENDREGION}
      class procedure ShowHelp;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Zeigt die Version an
      /// </summary>
      {$ENDREGION}
      class procedure ShowVersion;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Zeigt eine Fehlermeldung an
      /// </summary>
      {$ENDREGION}
      class procedure ShowError( const aMessage : string );

    strict private
      class function GetParamValue( const aParamName : string ) : string;
      class function HasParam( const aParamName : string ) : Boolean;
  end;

implementation

{ TCommandLineParser }

class function TCommandLineParser.Parse( out aParams : TCommandLineParams ) : Boolean;
begin
  Result := false;

  aParams.Command         := ctUnknown;
  aParams.FilePath        := '';
  aParams.FilePattern     := '';
  aParams.OldStr          := '';
  aParams.NewStr          := '';
  aParams.StartLine       := 0;
  aParams.EndLine         := 0;
  aParams.InsertAfterLine := 0;
  aParams.Text            := '';
  aParams.JSONFile        := '';
  aParams.RegexPattern    := '';
  aParams.RegexReplace    := '';
  aParams.CaseInsensitive := false;
  aParams.MultiLine       := false;
  aParams.Backup          := false;
  aParams.DryRun          := false;
  aParams.Diff            := false;
  aParams.Verbose         := false;

  if HasParam( '--help' ) or HasParam( '-h' ) then
    begin
      aParams.Command := ctHelp;
      Result          := true;
      Exit;
    end;

  if HasParam( '--version' ) or HasParam( '-v' ) then
    begin
      aParams.Command := ctVersion;
      Result          := true;
      Exit;
    end;

  aParams.ConfigFile := GetParamValue( '--config' );

  if aParams.ConfigFile <> '' then
    begin
      Result := true;
      Exit;
    end;

  if HasParam( '--undo' ) then
    begin
      aParams.Command  := ctUndo;
      aParams.FilePath := GetParamValue( '--file' );
      aParams.Verbose  := HasParam( '--verbose' );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  aParams.FilePath    := GetParamValue( '--file' );
  aParams.FilePattern := GetParamValue( '--files' );

  if ( aParams.FilePath = '' ) and ( aParams.FilePattern = '' ) then
    begin
      ShowError( 'Missing required parameter: --file or --files' );
      Exit;
    end;

  if HasParam( '--command' ) then
    begin
      Var lCommand := GetParamValue( '--command' );

      if lCommand = 'str_replace'
        then aParams.Command := ctStrReplace
        else
      if lCommand = 'insert'
        then aParams.Command := ctInsert
        else
      if lCommand = 'regex_replace'
        then aParams.Command := ctRegexReplace
        else
      if lCommand = 'regex_test'
        then aParams.Command := ctRegexTest
        else begin
               ShowError( 'Unknown command: ' + lCommand );
               Exit;
             end;
    end
  else begin
         if HasParam( '--old-str' )
           then aParams.Command := ctStrReplace
           else
         if HasParam( '--text' )
           then aParams.Command := ctInsert
           else
         if HasParam( '--regex-pattern' ) then
           begin
             if HasParam( '--regex-test' )
               then aParams.Command := ctRegexTest
               else aParams.Command := ctRegexReplace;
           end
           else begin
                  ShowError( 'Missing command or parameters' );
                  Exit;
                end;
       end;

  if aParams.Command = ctStrReplace then
    begin
      aParams.OldStr           := GetParamValue( '--old-str' );
      aParams.NewStr           := GetParamValue( '--new-str' );
      aParams.StartLine        := StrToIntDef( GetParamValue( '--start-line' ), 0 );
      aParams.EndLine          := StrToIntDef( GetParamValue( '--end-line' ), 0 );
      aParams.ConditionPattern := GetParamValue( '--condition-pattern' );

      if aParams.OldStr = '' then
        begin
          ShowError( 'Missing required parameter: --old-str' );
          Exit;
        end;
    end;

  if aParams.Command = ctInsert then
    begin
      aParams.Text            := GetParamValue( '--text' );
      aParams.InsertAfterLine := StrToIntDef( GetParamValue( '--insert-after-line' ), 0 );

      if aParams.Text = '' then
        begin
          ShowError( 'Missing required parameter: --text' );
          Exit;
        end;
    end;

  if ( aParams.Command = ctRegexReplace ) or ( aParams.Command = ctRegexTest ) then
    begin
      aParams.RegexPattern    := GetParamValue( '--regex-pattern' );
      aParams.RegexReplace    := GetParamValue( '--regex-replace' );
      aParams.StartLine       := StrToIntDef( GetParamValue( '--start-line' ), 0 );
      aParams.EndLine         := StrToIntDef( GetParamValue( '--end-line' ), 0 );
      aParams.CaseInsensitive := HasParam( '--case-insensitive' ) or HasParam( '-i' );
      aParams.MultiLine       := HasParam( '--multiline' ) or HasParam( '-m' );

      if aParams.RegexPattern = '' then
        begin
          ShowError( 'Missing required parameter: --regex-pattern' );
          Exit;
        end;

      if ( aParams.Command = ctRegexReplace ) and ( aParams.RegexReplace = '' ) then
        begin
          ShowError( 'Missing required parameter: --regex-replace' );
          Exit;
        end;
    end;

  aParams.JSONFile := GetParamValue( '--json' );
  aParams.Backup   := HasParam( '--backup' );
  aParams.DryRun   := HasParam( '--dry-run' );
  aParams.Diff     := HasParam( '--diff' );
  aParams.Stats    := HasParam( '--stats' );
  aParams.Verbose  := HasParam( '--verbose' );

  Var lCase := GetParamValue( '--case' );

  if lCase <> '' then
    begin
      if SameText( lCase, 'upper' )
        then aParams.CaseConversion := ccUpper
        else
      if SameText( lCase, 'lower' )
        then aParams.CaseConversion := ccLower
        else
      if SameText( lCase, 'title' )
        then aParams.CaseConversion := ccTitle
        else begin
               ShowError( 'Invalid case conversion: ' + lCase + ' (use upper, lower, or title)' );
               Exit;
             end;
    end
  else aParams.CaseConversion := ccNone;

  Var lIndent := GetParamValue( '--indent' );

  if lIndent <> '' then
    begin
      try
        aParams.IndentLevel := StrToInt( lIndent );
      except
        on E : Exception do
          begin
            ShowError( 'Invalid indent level: ' + lIndent + ' (use integer value, e.g. +2 or -2)' );
            Exit;
          end;
      end;
    end
  else aParams.IndentLevel := 0;

  Result := true;
end;

class function TCommandLineParser.GetParamValue( const aParamName : string ) : string;
Var
  i : Integer;
begin
  Result := '';

  for i := 1 to ParamCount do
    begin
      if SameText( ParamStr( i ), aParamName ) then
        begin
          if i < ParamCount
            then Result := ParamStr( i + 1 );

          Exit;
        end;
    end;
end;

class function TCommandLineParser.HasParam( const aParamName : string ) : Boolean;
Var
  i : Integer;
begin
  Result := false;

  for i := 1 to ParamCount do
    begin
      if SameText( ParamStr( i ), aParamName ) then
        begin
          Result := true;
          Exit;
        end;
    end;
end;

class procedure TCommandLineParser.ShowHelp;
begin
  WriteLn( 'StrEditor - String Replace Tool with Encoding Preservation' );
  WriteLn;
  WriteLn( 'Usage:' );
  WriteLn( '  StrEditor.exe --file <file> --old-str <old> --new-str <new> [--start-line <n>] [--end-line <n>]' );
  WriteLn( '  StrEditor.exe --file <file> --text <text> --insert-after-line <n>' );
  WriteLn( '  StrEditor.exe --file <file> --regex-pattern <pattern> --regex-replace <replacement> [-i] [-m]' );
  WriteLn( '  StrEditor.exe --file <file> --regex-pattern <pattern> --regex-test [-i] [-m]' );
  WriteLn( '  StrEditor.exe --help' );
  WriteLn( '  StrEditor.exe --version' );
  WriteLn;
  WriteLn( 'Parameters:' );
  WriteLn( '  --file <file>              File to process' );
  WriteLn( '  --files <pattern>          File pattern for batch processing (e.g. "*.pas")' );
  WriteLn( '  --old-str <old>            String to replace' );
  WriteLn( '  --new-str <new>            Replacement string' );
  WriteLn( '  --start-line <n>           Start line for replacement (optional)' );
  WriteLn( '  --end-line <n>             End line for replacement (optional)' );
  WriteLn( '  --text <text>              Text to insert' );
  WriteLn( '  --insert-after-line <n>    Line number after which to insert' );
  WriteLn( '  --regex-pattern <pattern>  Regular expression pattern' );
  WriteLn( '  --regex-replace <repl>     Replacement string (supports $1, $2, etc.)' );
  WriteLn( '  --regex-test               Test regex without making changes' );
  WriteLn( '  --condition-pattern <pat>  Only replace if line matches this regex pattern' );
  WriteLn( '  -i, --case-insensitive     Case-insensitive regex matching' );
  WriteLn( '  -m, --multiline            Multi-line regex matching' );
  WriteLn( '  --backup                   Create backup file before changes' );
  WriteLn( '  --dry-run                  Show changes without modifying file' );
  WriteLn( '  --diff                     Show differences between original and modified' );
  WriteLn( '  --stats                    Show statistics after operation' );
  WriteLn( '  --case <type>              Case conversion: upper, lower, title' );
  WriteLn( '  --indent <n>               Indent (+n) or outdent (-n) lines' );
  WriteLn( '  --undo                     Restore backup file (requires --file)' );
  WriteLn( '  --config <file>            Load parameters from JSON config file' );
  WriteLn( '  --verbose                  Show detailed information' );
  WriteLn( '  --help, -h                 Show this help' );
  WriteLn( '  --version, -v              Show version' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL"' );
  WriteLn( '  StrEditor.exe --file "test.pas" --text "// Comment" --insert-after-line 10' );
  WriteLn( '  StrEditor.exe --file "test.pas" --regex-pattern "f(\w+)" --regex-replace "l$1" -i' );
  WriteLn( '  StrEditor.exe --file "test.pas" --regex-pattern "procedure\s+(\w+)" --regex-test' );
end;

class procedure TCommandLineParser.ShowVersion;
begin
  WriteLn( 'StrEditor v1.0.0' );
  WriteLn( 'Build: 2025-11-09' );
  WriteLn( 'Delphi String Replace Tool with Encoding Preservation' );
end;

class procedure TCommandLineParser.ShowError( const aMessage : string );
begin
  WriteLn( 'ERROR: ' + aMessage );
  WriteLn;
  WriteLn( 'Use --help for usage information' );
end;

end.

