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
  TCommandType = ( ctUnknown, ctStrReplace, ctInsert, ctRegexReplace, ctRegexTest, ctUndo, ctHelp, ctVersion, ctDetectEncoding, ctShow, ctConvertEncoding, ctReinterpretEncoding, ctDeleteLine, ctDeleteLines, ctReplaceLine );

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
    ecJSONParseError  = 5,
    ecOperationFailed = 6
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
    ReplaceAll        : Boolean;
    Backup            : Boolean;
    DryRun            : Boolean;
    Diff              : Boolean;
    Stats             : Boolean;
    CaseConversion    : TCaseConversion;
    IndentLevel       : Integer;
    Verbose           : Boolean;
    ShowHead          : Integer;
    ShowTail          : Integer;
    ShowRaw           : Boolean;
    ShowLineNumbers   : Boolean;
    TargetEncoding    : string;
    SourceEncoding    : string;
    OldStrIsBase64    : Boolean;
    NewStrIsBase64    : Boolean;
    TextIsBase64      : Boolean;
    RegexIsBase64     : Boolean;
    LineNumber        : Integer;
    LineNumbers       : string;
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
  aParams.ShowHead        := 0;
  aParams.ShowTail        := 0;
  aParams.ShowRaw         := false;
  aParams.ShowLineNumbers := false;
  aParams.LineNumber      := 0;
  aParams.LineNumbers     := '';

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

  if HasParam( '--detect-encoding' ) or HasParam( '--encoding' ) then
    begin
      aParams.Command  := ctDetectEncoding;
      aParams.FilePath := GetParamValue( '--file' );
      aParams.Verbose  := HasParam( '--verbose' );
      Result           := true;
      Exit;
    end;

  if HasParam( '--convert-encoding' ) then
    begin
      aParams.Command        := ctConvertEncoding;
      aParams.FilePath       := GetParamValue( '--file' );
      aParams.TargetEncoding := GetParamValue( '--to' );
      aParams.Backup         := HasParam( '--backup' );
      aParams.DryRun         := HasParam( '--dry-run' );
      aParams.Verbose        := HasParam( '--verbose' );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      if aParams.TargetEncoding = '' then
        begin
          ShowError( 'Missing required parameter: --to (utf8 or windows1252)' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--reinterpret-as' ) then
    begin
      aParams.Command        := ctReinterpretEncoding;
      aParams.FilePath       := GetParamValue( '--file' );
      aParams.SourceEncoding := GetParamValue( '--reinterpret-as' );
      aParams.Backup         := HasParam( '--backup' );
      aParams.DryRun         := HasParam( '--dry-run' );
      aParams.Verbose        := HasParam( '--verbose' );

      if aParams.FilePath = '' then
        begin
          WriteLn( 'ERROR: --file parameter required for --reinterpret-as' );
          Exit;
        end;

      if aParams.SourceEncoding = '' then
        begin
          WriteLn( 'ERROR: --reinterpret-as parameter requires encoding value' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--show' ) or HasParam( '--cat' ) then
    begin
      aParams.Command         := ctShow;
      aParams.FilePath        := GetParamValue( '--file' );
      aParams.Verbose         := HasParam( '--verbose' );
      aParams.ShowLineNumbers := HasParam( '--line-numbers' );
      aParams.ShowRaw         := HasParam( '--raw' );

      if HasParam( '--head' ) or HasParam( '--first' ) or HasParam( '--total-count' ) then
        begin
          if HasParam( '--head' )
            then aParams.ShowHead := StrToIntDef( GetParamValue( '--head' ), 0 )
            else
          if HasParam( '--first' )
            then aParams.ShowHead := StrToIntDef( GetParamValue( '--first' ), 0 )
            else aParams.ShowHead := StrToIntDef( GetParamValue( '--total-count' ), 0 );
        end;

      if HasParam( '--tail' ) or HasParam( '--last' ) then
        begin
          if HasParam( '--tail' )
            then aParams.ShowTail := StrToIntDef( GetParamValue( '--tail' ), 0 )
            else aParams.ShowTail := StrToIntDef( GetParamValue( '--last' ), 0 );
        end;

      if HasParam( '--start-line' ) then
        aParams.StartLine := StrToIntDef( GetParamValue( '--start-line' ), 0 );

      if HasParam( '--end-line' ) then
        aParams.EndLine := StrToIntDef( GetParamValue( '--end-line' ), 0 );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  aParams.ConfigFile := GetParamValue( '--config' );

  if aParams.ConfigFile <> '' then
    begin
      Result := true;
      Exit;
    end;

  if HasParam( '--delete-line' ) then
    begin
      aParams.Command    := ctDeleteLine;
      aParams.FilePath   := GetParamValue( '--file' );
      aParams.LineNumber := StrToIntDef( GetParamValue( '--delete-line' ), 0 );
      aParams.Backup     := HasParam( '--backup' );
      aParams.DryRun     := HasParam( '--dry-run' );
      aParams.Diff       := HasParam( '--diff' );
      aParams.Verbose    := HasParam( '--verbose' );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      if aParams.LineNumber <= 0 then
        begin
          ShowError( 'Invalid line number: ' + GetParamValue( '--delete-line' ) );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--delete-lines' ) then
    begin
      aParams.Command    := ctDeleteLines;
      aParams.FilePath   := GetParamValue( '--file' );
      aParams.LineNumbers := GetParamValue( '--delete-lines' );
      aParams.Backup     := HasParam( '--backup' );
      aParams.DryRun     := HasParam( '--dry-run' );
      aParams.Diff       := HasParam( '--diff' );
      aParams.Verbose    := HasParam( '--verbose' );

      if HasParam( '--start-line' ) then
        aParams.StartLine := StrToIntDef( GetParamValue( '--start-line' ), 0 );

      if HasParam( '--end-line' ) then
        aParams.EndLine := StrToIntDef( GetParamValue( '--end-line' ), 0 );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      if ( aParams.LineNumbers = '' ) and ( aParams.StartLine = 0 ) then
        begin
          ShowError( 'Missing required parameter: --delete-lines or --start-line' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--replace-line' ) then
    begin
      aParams.Command      := ctReplaceLine;
      aParams.FilePath     := GetParamValue( '--file' );
      aParams.LineNumber   := StrToIntDef( GetParamValue( '--replace-line' ), 0 );
      aParams.TextIsBase64 := HasParam( '--with-base64' );
      aParams.Backup       := HasParam( '--backup' );
      aParams.DryRun       := HasParam( '--dry-run' );
      aParams.Diff         := HasParam( '--diff' );
      aParams.Verbose      := HasParam( '--verbose' );

      if aParams.TextIsBase64
        then aParams.Text := GetParamValue( '--with-base64' )
        else aParams.Text := GetParamValue( '--with' );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      if aParams.LineNumber <= 0 then
        begin
          ShowError( 'Invalid line number: ' + GetParamValue( '--replace-line' ) );
          Exit;
        end;

      if aParams.Text = '' then
        begin
          ShowError( 'Missing required parameter: --with or --with-base64' );
          Exit;
        end;

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
         if HasParam( '--old-str' ) or HasParam( '--old-str-base64' )
           then aParams.Command := ctStrReplace
           else
         if HasParam( '--text' ) or HasParam( '--text-base64' )
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
      aParams.OldStrIsBase64 := HasParam( '--old-str-base64' );
      aParams.NewStrIsBase64 := HasParam( '--new-str-base64' );

      if aParams.OldStrIsBase64
        then aParams.OldStr := GetParamValue( '--old-str-base64' )
        else aParams.OldStr := GetParamValue( '--old-str' );

      if aParams.NewStrIsBase64
        then aParams.NewStr := GetParamValue( '--new-str-base64' )
        else aParams.NewStr := GetParamValue( '--new-str' );

      aParams.StartLine        := StrToIntDef( GetParamValue( '--start-line' ), 0 );
      aParams.EndLine          := StrToIntDef( GetParamValue( '--end-line' ), 0 );
      aParams.ConditionPattern := GetParamValue( '--condition-pattern' );

      if aParams.OldStr = '' then
        begin
          ShowError( 'Missing required parameter: --old-str or --old-str-base64' );
          Exit;
        end;
    end;

  if aParams.Command = ctInsert then
    begin
      aParams.TextIsBase64 := HasParam( '--text-base64' );

      if aParams.TextIsBase64
        then aParams.Text := GetParamValue( '--text-base64' )
        else aParams.Text := GetParamValue( '--text' );

      aParams.InsertAfterLine := StrToIntDef( GetParamValue( '--insert-after-line' ), 0 );

      if aParams.Text = '' then
        begin
          ShowError( 'Missing required parameter: --text or --text-base64' );
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

  aParams.JSONFile   := GetParamValue( '--json' );
  aParams.Backup     := HasParam( '--backup' );
  aParams.DryRun     := HasParam( '--dry-run' );
  aParams.Diff       := HasParam( '--diff' );
  aParams.Stats      := HasParam( '--stats' );
  aParams.Verbose    := HasParam( '--verbose' );
  aParams.MultiLine  := HasParam( '--multi-line' );
  aParams.ReplaceAll := HasParam( '--replace-all' );

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
  WriteLn( '  StrEditor.exe --file <file> --delete-line <n> [--backup] [--dry-run] [--diff]' );
  WriteLn( '  StrEditor.exe --file <file> --delete-lines <n,m,k> [--backup] [--dry-run] [--diff]' );
  WriteLn( '  StrEditor.exe --file <file> --delete-lines --start-line <n> --end-line <m> [--backup] [--dry-run]' );
  WriteLn( '  StrEditor.exe --file <file> --replace-line <n> --with <text> [--backup] [--dry-run] [--diff]' );
  WriteLn( '  StrEditor.exe --file <file> --replace-line <n> --with-base64 <base64> [--backup] [--dry-run]' );
  WriteLn( '  StrEditor.exe --file <file> --detect-encoding' );
  WriteLn( '  StrEditor.exe --file <file> --convert-encoding --to <utf8|windows1252> [--backup] [--dry-run]' );
  WriteLn( '  StrEditor.exe --file <file> --reinterpret-as <utf8|windows1252> [--backup] [--dry-run]' );
  WriteLn( '  StrEditor.exe --file <file> --show [--head <n>] [--tail <n>] [--line-numbers] [--raw]' );
  WriteLn( '  StrEditor.exe --help' );
  WriteLn( '  StrEditor.exe --version' );
  WriteLn;
  WriteLn( 'Parameters:' );
  WriteLn( '  --file <file>              File to process' );
  WriteLn( '  --files <pattern>          File pattern for batch processing (e.g. "*.pas")' );
  WriteLn( '  --old-str <old>            String to replace' );
  WriteLn( '  --new-str <new>            Replacement string' );
  WriteLn( '  --old-str-base64 <base64>  String to replace (Base64-encoded)' );
  WriteLn( '  --new-str-base64 <base64>  Replacement string (Base64-encoded)' );
  WriteLn( '  --start-line <n>           Start line for replacement (optional)' );
  WriteLn( '  --end-line <n>             End line for replacement (optional)' );
  WriteLn( '  --text <text>              Text to insert' );
  WriteLn( '  --text-base64 <base64>     Text to insert (Base64-encoded)' );
  WriteLn( '  --insert-after-line <n>    Line number after which to insert' );
  WriteLn( '  --regex-pattern <pattern>  Regular expression pattern' );
  WriteLn( '  --regex-replace <repl>     Replacement string (supports $1, $2, etc.)' );
  WriteLn( '  --regex-test               Test regex without making changes' );
  WriteLn( '  --condition-pattern <pat>  Only replace if line matches this regex pattern' );
  WriteLn( '  -i, --case-insensitive     Case-insensitive regex matching' );
  WriteLn( '  -m, --multiline            Multi-line regex matching' );
  WriteLn( '  --multi-line               Multi-line string replace (search across line boundaries)' );
  WriteLn( '  --replace-all              Replace all occurrences (default: only first)' );
  WriteLn( '  --backup                   Create backup file before changes' );
  WriteLn( '  --dry-run                  Show changes without modifying file' );
  WriteLn( '  --diff                     Show differences between original and modified' );
  WriteLn( '  --stats                    Show statistics after operation' );
  WriteLn( '  --case <type>              Case conversion: upper, lower, title' );
  WriteLn( '  --indent <n>               Indent (+n) or outdent (-n) lines' );
  WriteLn( '  --delete-line <n>          Delete a single line' );
  WriteLn( '  --delete-lines <n,m,k>     Delete multiple lines (comma-separated)' );
  WriteLn( '  --delete-lines --start-line <n> --end-line <m>  Delete line range' );
  WriteLn( '  --replace-line <n>         Replace a complete line' );
  WriteLn( '  --with <text>              Replacement text for --replace-line' );
  WriteLn( '  --with-base64 <base64>     Replacement text (Base64-encoded)' );
  WriteLn( '  --undo                     Restore backup file (requires --file)' );
  WriteLn( '  --config <file>            Load parameters from JSON config file' );
  WriteLn( '  --detect-encoding          Detect and show file encoding (requires --file)' );
  WriteLn( '  --convert-encoding         Convert file encoding (requires --file and --to)' );
  WriteLn( '  --to <encoding>            Target encoding: utf8 or windows1252' );
  WriteLn( '  --reinterpret-as <enc>     Reinterpret file as encoding (repairs broken encodings)' );
  WriteLn( '  --show, --cat              Show file content (encoding-aware)' );
  WriteLn( '  --head <n>                 Show first N lines (aliases: --first, --total-count)' );
  WriteLn( '  --tail <n>                 Show last N lines (alias: --last)' );
  WriteLn( '  --start-line <n>           Start line for show (with --end-line)' );
  WriteLn( '  --end-line <n>             End line for show (with --start-line)' );
  WriteLn( '  --line-numbers             Show line numbers' );
  WriteLn( '  --raw                      Show as single string (no line breaks)' );
  WriteLn( '  --verbose                  Show detailed information' );
  WriteLn( '  --help, -h                 Show this help' );
  WriteLn( '  --version, -v              Show version' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL"' );
  WriteLn( '  StrEditor.exe --file "test.pas" --old-str-base64 "ICAgICB7JElGREVGIFBSRVVOSUNPREV9" --new-str-base64 "ICAgICB7JElGREVGIFVOSUNPREV9"' );
  WriteLn( '  StrEditor.exe --file "test.pas" --text "// Comment" --insert-after-line 10' );
  WriteLn( '  StrEditor.exe --file "test.pas" --text-base64 "eyRJRkRFRiBERUJVR30=" --insert-after-line 10' );
  WriteLn( '  StrEditor.exe --file "test.pas" --regex-pattern "f(\w+)" --regex-replace "l$1" -i' );
  WriteLn( '  StrEditor.exe --file "test.pas" --regex-pattern "procedure\s+(\w+)" --regex-test' );
  WriteLn( '  StrEditor.exe --file "test.pas" --detect-encoding' );
  WriteLn( '  StrEditor.exe --file "test.pas" --convert-encoding --to windows1252 --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --reinterpret-as utf8 --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --show --head 10 --line-numbers' );
  WriteLn( '  StrEditor.exe --file "test.pas" --show --tail 5' );
  WriteLn( '  StrEditor.exe --file "test.pas" --show --start-line 10 --end-line 20' );
  WriteLn( '  StrEditor.exe --file "test.pas" --delete-line 25 --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --delete-lines "1,3,5" --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --delete-lines --start-line 10 --end-line 20 --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --replace-line 25 --with "  WriteLn(''New'');" --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --replace-line 25 --with-base64 "ICBXcml0ZUxuKCdOZXcnKTs=" --backup' );
  WriteLn;
  WriteLn( 'Multi-Line Examples:' );
  WriteLn( '  # Replace 5 lines with 1 line (IFDEF block removal)' );
  WriteLn( '  StrEditor.exe --file "test.pas" --old-str-base64 "BASE64_5_LINES" --new-str-base64 "BASE64_1_LINE" --multi-line --backup' );
  WriteLn;
  WriteLn( '  # Replace all occurrences of multi-line block' );
  WriteLn( '  StrEditor.exe --file "test.pas" --old-str-base64 "BASE64_BLOCK" --new-str-base64 "BASE64_NEW" --multi-line --replace-all' );
end;

class procedure TCommandLineParser.ShowVersion;
begin
  WriteLn( 'StrEditor v1.7.1' );
  WriteLn( 'Build: 2025-11-11' );
  WriteLn( 'Delphi String Replace Tool with Encoding Preservation' );
  WriteLn;
  WriteLn( 'New in v1.7:' );
  WriteLn( '  - Delete Line (--delete-line <n>)' );
  WriteLn( '  - Delete Lines (--delete-lines <n,m,k> or --start-line/--end-line)' );
  WriteLn( '  - Replace Line (--replace-line <n> --with <text>)' );
  WriteLn( '  - Base64 support for replace-line (--with-base64)' );
  WriteLn;
  WriteLn( 'New in v1.6:' );
  WriteLn( '  - Multi-Line String Replace (--multi-line)' );
  WriteLn( '  - Replace all occurrences (--replace-all)' );
  WriteLn( '  - Search and replace across line boundaries' );
  WriteLn;
  WriteLn( 'Previous versions:' );
  WriteLn( '  v1.5: Base64-encoded parameters (--old-str-base64, --new-str-base64, --text-base64)' );
  WriteLn( '  - Fixes PowerShell special character issues (Dollar-Zeichen, Backtick, etc.)' );
  WriteLn;
  WriteLn( '  v1.4: Reinterpret Encoding (--reinterpret-as <utf8|windows1252>)' );
  WriteLn( '  - Repairs broken encodings (e.g., UTF-8 bytes in Windows-1252 files)' );
  WriteLn;
  WriteLn( 'New in v1.3:' );
  WriteLn( '  - Convert Encoding (--convert-encoding --to <utf8|windows1252>)' );
  WriteLn;
  WriteLn( 'New in v1.2:' );
  WriteLn( '  - Show/Cat Command (--show, --cat)' );
  WriteLn( '  - Head/Tail Support (--head, --tail)' );
  WriteLn( '  - Line Range Support (--start-line, --end-line)' );
  WriteLn( '  - Line Numbers (--line-numbers)' );
  WriteLn( '  - Raw Output (--raw)' );
  WriteLn;
  WriteLn( 'Previous versions:' );
  WriteLn( '  v1.1: Encoding Detection, Improved Verbose Output' );
  WriteLn( '  v1.0: Initial Release' );
end;

class procedure TCommandLineParser.ShowError( const aMessage : string );
begin
  WriteLn( 'ERROR: ' + aMessage );
  WriteLn;
  WriteLn( 'Use --help for usage information' );
end;

end.

