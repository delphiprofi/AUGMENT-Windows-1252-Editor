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
  TCommandType = ( ctUnknown, ctStrReplace, ctInsert, ctInsertBefore, ctRegexReplace, ctRegexTest, ctUndo, ctHelp, ctVersion, ctDetectEncoding, ctShow, ctConvertEncoding, ctReinterpretEncoding, ctDeleteLine, ctDeleteLines, ctReplaceLine, ctReplaceLines, ctDocs, ctRepairUmlauts, ctMoveLines, ctIndent, ctUnindent, ctFileCompare );

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
    InsertBeforeLine  : Integer;
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
    ShowHex           : Boolean;
    ShowBase64        : Boolean;
    TargetEncoding    : string;
    SourceEncoding    : string;
    OldStrIsBase64    : Boolean;
    NewStrIsBase64    : Boolean;
    TextIsBase64      : Boolean;
    RegexIsBase64     : Boolean;
    LineNumber        : Integer;
    LineNumbers       : string;
    DocsFile          : string;
    DocsListFiles     : Boolean;
    DocsOpenBrowser   : Boolean;
    // Repair Umlauts
    VCS               : string;       // hg, git, auto
    Revision          : string;       // VCS-Revision (.^ für hg, HEAD~1 für git)
    ReferencePath     : string;       // Alternative: Referenz-Datei statt VCS
    // Move Lines
    FromFile          : string;       // Source file for move-lines
    ToFile            : string;       // Target file for move-lines
    // Indent/Unindent
    IndentSpaces      : Integer;      // Number of spaces for indent/unindent (default: 2)
    // Config Options
    KeepConfig        : Boolean;      // Keep JSON config file after successful execution (default: false = auto-delete)
    // FileCompare
    CompareFile       : string;       // Master file (file2) for --filecompare
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

    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   F5: Liefert alle Namen (Original + Aliase) für einen Parameter
      /// </summary>
      {$ENDREGION}
      class function GetParamAliases( const aParamName : string ) : TArray<string>;

    strict private
      class function GetParamValue( const aParamName : string ) : string;
      class function HasParam( const aParamName : string ) : Boolean;
      {$REGION 'Documentation'}
      /// <summary>
      ///   F4: Zeigt die Hilfe-Übersicht mit Kategorien
      /// </summary>
      {$ENDREGION}
      class procedure ShowHelpOverview;
      class procedure ShowHelpReplace;
      class procedure ShowHelpInsert;
      class procedure ShowHelpDelete;
      class procedure ShowHelpShow;
      class procedure ShowHelpEncoding;
      class procedure ShowHelpConfig;
      class procedure ShowHelpMove;
      class procedure ShowHelpRepair;
      class procedure ShowHelpCompare;
      class procedure ShowHelpAll;
  end;

implementation

{ TCommandLineParser }

class function TCommandLineParser.Parse( out aParams : TCommandLineParams ) : Boolean;
Var
  lRangeValue : string;
  lParts      : TArray<string>;
begin
  Result := false;

  aParams.Command         := ctUnknown;
  aParams.FilePath        := '';
  aParams.FilePattern     := '';
  aParams.OldStr          := '';
  aParams.NewStr          := '';
  aParams.StartLine        := 0;
  aParams.EndLine          := 0;
  aParams.InsertAfterLine  := 0;
  aParams.InsertBeforeLine := 0;
  aParams.Text             := '';
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
  aParams.ShowHex         := false;
  aParams.ShowBase64      := false;
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
      aParams.ShowHex         := HasParam( '--hex' );
      aParams.ShowBase64      := HasParam( '--base64' );

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

      // --range <start>,<end> Parameter (z.B. --range 10,20)
      if HasParam( '--range' ) then
        begin
          lRangeValue := GetParamValue( '--range' );
          lParts      := lRangeValue.Split( [ ',' ] );

          if Length( lParts ) <> 2 then
            begin
              ShowError( 'Invalid --range format. Expected: --range <start>,<end>' );
              Exit;
            end;

          aParams.StartLine := StrToIntDef( Trim( lParts[ 0 ] ), 0 );
          aParams.EndLine   := StrToIntDef( Trim( lParts[ 1 ] ), 0 );

          if ( aParams.StartLine <= 0 ) or ( aParams.EndLine <= 0 ) then
            begin
              ShowError( 'Invalid --range values. Both start and end must be > 0' );
              Exit;
            end;

          if aParams.EndLine < aParams.StartLine then
            begin
              ShowError( 'Invalid --range: end must be >= start' );
              Exit;
            end;
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

  if HasParam( '--repair-umlauts' ) then
    begin
      aParams.Command       := ctRepairUmlauts;
      aParams.FilePath      := GetParamValue( '--file' );
      aParams.VCS           := GetParamValue( '--vcs' );
      aParams.Revision      := GetParamValue( '--revision' );
      aParams.ReferencePath := GetParamValue( '--reference' );
      aParams.Backup        := HasParam( '--backup' );
      aParams.DryRun        := HasParam( '--dry-run' );
      aParams.Verbose       := HasParam( '--verbose' );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--move-lines' ) then
    begin
      aParams.Command   := ctMoveLines;
      aParams.FromFile  := GetParamValue( '--from' );
      aParams.ToFile    := GetParamValue( '--to' );
      aParams.StartLine := StrToIntDef( GetParamValue( '--start-line' ), 0 );
      aParams.EndLine   := StrToIntDef( GetParamValue( '--end-line' ), 0 );
      aParams.Backup    := HasParam( '--backup' );
      aParams.DryRun    := HasParam( '--dry-run' );
      aParams.Diff      := HasParam( '--diff' );
      aParams.Verbose   := HasParam( '--verbose' );

      if HasParam( '--insert-after-line' ) then
        aParams.InsertAfterLine := StrToIntDef( GetParamValue( '--insert-after-line' ), 0 );

      if HasParam( '--insert-before-line' ) then
        aParams.InsertBeforeLine := StrToIntDef( GetParamValue( '--insert-before-line' ), 0 );

      if aParams.FromFile = '' then
        begin
          ShowError( 'Missing required parameter: --from' );
          Exit;
        end;

      if aParams.ToFile = '' then
        begin
          ShowError( 'Missing required parameter: --to' );
          Exit;
        end;

      if aParams.StartLine <= 0 then
        begin
          ShowError( 'Missing or invalid parameter: --start-line' );
          Exit;
        end;

      if aParams.EndLine <= 0 then
        begin
          ShowError( 'Missing or invalid parameter: --end-line' );
          Exit;
        end;

      if aParams.EndLine < aParams.StartLine then
        begin
          ShowError( 'Invalid line range: --end-line must be >= --start-line' );
          Exit;
        end;

      if ( aParams.InsertAfterLine <= 0 ) and ( aParams.InsertBeforeLine <= 0 ) then
        begin
          ShowError( 'Missing required parameter: --insert-after-line or --insert-before-line' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--filecompare' ) then
    begin
      aParams.Command     := ctFileCompare;
      aParams.FilePath    := GetParamValue( '--file' );
      aParams.CompareFile := GetParamValue( '--filecompare' );
      aParams.Verbose     := HasParam( '--verbose' );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file <file1>' );
          Exit;
        end;

      if aParams.CompareFile = '' then
        begin
          ShowError( 'Missing required parameter: --filecompare <master-file>' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--indent-lines' ) then
    begin
      aParams.Command      := ctIndent;
      aParams.FilePath     := GetParamValue( '--file' );
      aParams.StartLine    := StrToIntDef( GetParamValue( '--start-line' ), 0 );
      aParams.EndLine      := StrToIntDef( GetParamValue( '--end-line' ), 0 );
      aParams.IndentSpaces := StrToIntDef( GetParamValue( '--spaces' ), 2 );
      aParams.Backup       := HasParam( '--backup' );
      aParams.DryRun       := HasParam( '--dry-run' );
      aParams.Diff         := HasParam( '--diff' );
      aParams.Verbose      := HasParam( '--verbose' );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      if aParams.StartLine <= 0 then
        begin
          ShowError( 'Missing or invalid parameter: --start-line' );
          Exit;
        end;

      if aParams.EndLine <= 0 then
        begin
          ShowError( 'Missing or invalid parameter: --end-line' );
          Exit;
        end;

      if aParams.EndLine < aParams.StartLine then
        begin
          ShowError( 'Invalid line range: --end-line must be >= --start-line' );
          Exit;
        end;

      if aParams.IndentSpaces <= 0 then
        begin
          ShowError( 'Invalid --spaces value: must be > 0' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--unindent-lines' ) then
    begin
      aParams.Command      := ctUnindent;
      aParams.FilePath     := GetParamValue( '--file' );
      aParams.StartLine    := StrToIntDef( GetParamValue( '--start-line' ), 0 );
      aParams.EndLine      := StrToIntDef( GetParamValue( '--end-line' ), 0 );
      aParams.IndentSpaces := StrToIntDef( GetParamValue( '--spaces' ), 2 );
      aParams.Backup       := HasParam( '--backup' );
      aParams.DryRun       := HasParam( '--dry-run' );
      aParams.Diff         := HasParam( '--diff' );
      aParams.Verbose      := HasParam( '--verbose' );

      if aParams.FilePath = '' then
        begin
          ShowError( 'Missing required parameter: --file' );
          Exit;
        end;

      if aParams.StartLine <= 0 then
        begin
          ShowError( 'Missing or invalid parameter: --start-line' );
          Exit;
        end;

      if aParams.EndLine <= 0 then
        begin
          ShowError( 'Missing or invalid parameter: --end-line' );
          Exit;
        end;

      if aParams.EndLine < aParams.StartLine then
        begin
          ShowError( 'Invalid line range: --end-line must be >= --start-line' );
          Exit;
        end;

      if aParams.IndentSpaces <= 0 then
        begin
          ShowError( 'Invalid --spaces value: must be > 0' );
          Exit;
        end;

      Result := true;
      Exit;
    end;

  if HasParam( '--docs' ) then
    begin
      aParams.Command         := ctDocs;
      aParams.DocsFile        := GetParamValue( '--docs' );
      aParams.DocsListFiles   := HasParam( '--list' );
      aParams.DocsOpenBrowser := HasParam( '--open-in-browser' );
      aParams.ShowLineNumbers := HasParam( '--line-numbers' );

      if HasParam( '--head' ) or HasParam( '--first' ) then
        begin
          if HasParam( '--head' )
            then aParams.ShowHead := StrToIntDef( GetParamValue( '--head' ), 0 )
            else aParams.ShowHead := StrToIntDef( GetParamValue( '--first' ), 0 );
        end;

      if HasParam( '--tail' ) or HasParam( '--last' ) then
        begin
          if HasParam( '--tail' )
            then aParams.ShowTail := StrToIntDef( GetParamValue( '--tail' ), 0 )
            else aParams.ShowTail := StrToIntDef( GetParamValue( '--last' ), 0 );
        end;

      Result := true;
      Exit;
    end;

  aParams.ConfigFile := GetParamValue( '--config' );

  if aParams.ConfigFile <> '' then
    begin
      aParams.KeepConfig := HasParam( '--keep-config' );

      // Deprecation-Warnung für --keep-config
      if aParams.KeepConfig then
        begin
          WriteLn( '' );
          WriteLn( '╔════════════════════════════════════════════════════════════════════════════╗' );
          WriteLn( '║ WARNING: --keep-config is DEPRECATED and makes NO SENSE!                  ║' );
          WriteLn( '║                                                                            ║' );
          WriteLn( '║ JSON config files are TEMPORARY operation instructions, not data files!   ║' );
          WriteLn( '║ They should be auto-deleted after successful execution.                   ║' );
          WriteLn( '║                                                                            ║' );
          WriteLn( '║ This parameter will be REMOVED in a future version.                       ║' );
          WriteLn( '╚════════════════════════════════════════════════════════════════════════════╝' );
          WriteLn( '' );
        end;

      aParams.Backup     := HasParam( '--backup' );
      aParams.DryRun     := HasParam( '--dry-run' );
      aParams.Verbose    := HasParam( '--verbose' );
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
         if HasParam( '--text' ) or HasParam( '--text-base64' ) then
           begin
             if HasParam( '--insert-before-line' )
               then aParams.Command := ctInsertBefore
               else aParams.Command := ctInsert;
           end
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

  if aParams.Command = ctInsertBefore then
    begin
      aParams.TextIsBase64 := HasParam( '--text-base64' );

      if aParams.TextIsBase64
        then aParams.Text := GetParamValue( '--text-base64' )
        else aParams.Text := GetParamValue( '--text' );

      aParams.InsertBeforeLine := StrToIntDef( GetParamValue( '--insert-before-line' ), 0 );

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
  i          : Integer;
  lNextParam : string;
  lAliases   : TArray<string>;
  lAlias     : string;
begin
  Result := '';
  lAliases := GetParamAliases( aParamName );

  for i := 1 to ParamCount do
    begin
      for lAlias in lAliases do
        begin
          if SameText( ParamStr( i ), lAlias ) then
            begin
              if i < ParamCount then
                begin
                  lNextParam := ParamStr( i + 1 );

                  if ( Length( lNextParam ) > 0 ) and ( lNextParam[ 1 ] <> '-' )
                    then Result := lNextParam;
                end;

              Exit;
            end;
        end;
    end;
end;

class function TCommandLineParser.HasParam( const aParamName : string ) : Boolean;
Var
  i        : Integer;
  lAliases : TArray<string>;
  lAlias   : string;
begin
  Result := false;
  lAliases := GetParamAliases( aParamName );

  for i := 1 to ParamCount do
    begin
      for lAlias in lAliases do
        begin
          if SameText( ParamStr( i ), lAlias ) then
            begin
              Result := true;
              Exit;
            end;
        end;
    end;
end;

class function TCommandLineParser.GetParamAliases( const aParamName : string ) : TArray<string>;
begin
  // F5: Definiert Kurzaliase für Parameter
  // Format: Original → [Original, Alias1, Alias2, ...]

  if SameText( aParamName, '--insert-before-line' ) then
    Result := [ '--insert-before-line', '--ib' ]
  else
  if SameText( aParamName, '--insert-after-line' ) then
    Result := [ '--insert-after-line', '--ia' ]
  else
  if SameText( aParamName, '--delete-line' ) then
    Result := [ '--delete-line', '--dl' ]
  else
  if SameText( aParamName, '--replace-line' ) then
    Result := [ '--replace-line', '--rl' ]
  else
  if SameText( aParamName, '--old-str-base64' ) then
    Result := [ '--old-str-base64', '--ob64' ]
  else
  if SameText( aParamName, '--new-str-base64' ) then
    Result := [ '--new-str-base64', '--nb64' ]
  else
    Result := [ aParamName ];
end;

class procedure TCommandLineParser.ShowHelp;
Var
  lCategory : string;
begin
  // F4: Kategorisierte Help - prüfe ob eine Kategorie angegeben wurde
  lCategory := '';

  if ParamCount >= 2 then
    begin
      // --help <category> oder -h <category>
      if SameText( ParamStr( 1 ), '--help' ) or SameText( ParamStr( 1 ), '-h' ) then
        lCategory := LowerCase( ParamStr( 2 ) );
    end;

  // Zeige kategoriespezifische Hilfe oder Übersicht
  if lCategory = '' then
    ShowHelpOverview
  else
  if ( lCategory = 'replace' ) or ( lCategory = 'str' ) then
    ShowHelpReplace
  else
  if ( lCategory = 'insert' ) or ( lCategory = 'add' ) then
    ShowHelpInsert
  else
  if ( lCategory = 'delete' ) or ( lCategory = 'del' ) then
    ShowHelpDelete
  else
  if ( lCategory = 'show' ) or ( lCategory = 'cat' ) or ( lCategory = 'view' ) then
    ShowHelpShow
  else
  if ( lCategory = 'encoding' ) or ( lCategory = 'enc' ) then
    ShowHelpEncoding
  else
  if ( lCategory = 'config' ) or ( lCategory = 'json' ) then
    ShowHelpConfig
  else
  if ( lCategory = 'move' ) then
    ShowHelpMove
  else
  if ( lCategory = 'repair' ) or ( lCategory = 'umlaut' ) then
    ShowHelpRepair
  else
  if ( lCategory = 'compare' ) or ( lCategory = 'filecompare' ) then
    ShowHelpCompare
  else
  if ( lCategory = 'all' ) then
    ShowHelpAll
  else
    begin
      WriteLn( 'Unknown help category: ' + lCategory );
      WriteLn;
      ShowHelpOverview;
    end;
end;

class procedure TCommandLineParser.ShowHelpOverview;
begin
  WriteLn( 'StrEditor - String Replace Tool with Encoding Preservation (v1.9.0)' );
  WriteLn;
  WriteLn( 'Quick Start:' );
  WriteLn( '  StrEditor.exe --file <file> --old-str <old> --new-str <new>     # Replace string' );
  WriteLn( '  StrEditor.exe --file <file> --text <text> --insert-after-line <n>  # Insert line' );
  WriteLn( '  StrEditor.exe --file <file> --delete-line <n>                   # Delete line' );
  WriteLn( '  StrEditor.exe --file <file> --show --line-numbers               # Show file' );
  WriteLn( '  StrEditor.exe --config <file.json>                              # Use JSON config' );
  WriteLn;
  WriteLn( 'Help Categories (use: --help <category>):' );
  WriteLn( '  replace   String and regex replacement operations' );
  WriteLn( '  insert    Insert text before/after lines' );
  WriteLn( '  delete    Delete lines (single, multiple, range)' );
  WriteLn( '  show      Display file content' );
  WriteLn( '  encoding  Detect, convert, reinterpret encodings' );
  WriteLn( '  config    JSON config file usage' );
  WriteLn( '  move      Move lines between files' );
  WriteLn( '  repair    Repair corrupted umlauts' );
  WriteLn( '  compare   Compare files for broken special characters' );
  WriteLn( '  all       Show complete help' );
  WriteLn;
  WriteLn( 'Parameter Aliases (v1.8):' );
  WriteLn( '  --ib   = --insert-before-line' );
  WriteLn( '  --ia   = --insert-after-line' );
  WriteLn( '  --dl   = --delete-line' );
  WriteLn( '  --rl   = --replace-line' );
  WriteLn( '  --ob64 = --old-str-base64' );
  WriteLn( '  --nb64 = --new-str-base64' );
  WriteLn;
  WriteLn( 'Common Options:' );
  WriteLn( '  --backup      Create backup file (.bak)' );
  WriteLn( '  --dry-run     Preview changes without modifying' );
  WriteLn( '  --diff        Show differences' );
  WriteLn( '  --verbose     Detailed output' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --help replace    # Show replace help' );
  WriteLn( '  StrEditor.exe --help config     # Show JSON config help' );
  WriteLn( '  StrEditor.exe --docs            # Show full documentation' );
end;

class procedure TCommandLineParser.ShowHelpReplace;
begin
  WriteLn( 'StrEditor - REPLACE Operations' );
  WriteLn( '================================' );
  WriteLn;
  WriteLn( 'String Replace:' );
  WriteLn( '  --file <file> --old-str <old> --new-str <new>' );
  WriteLn( '  --file <file> --old-str-base64 <b64> --new-str-base64 <b64>  # Base64 encoded' );
  WriteLn( '  --file <file> --replace-line <n> --with <text>              # Replace whole line' );
  WriteLn( '  --file <file> --replace-line <n> --with-base64 <b64>        # Base64 encoded' );
  WriteLn;
  WriteLn( 'Options:' );
  WriteLn( '  --start-line <n>        Limit to lines from n' );
  WriteLn( '  --end-line <n>          Limit to lines up to n' );
  WriteLn( '  --replace-all           Replace ALL occurrences (default: first only)' );
  WriteLn( '  --multi-line            Search across line boundaries' );
  WriteLn( '  --condition-pattern <p> Only replace if line matches pattern' );
  WriteLn;
  WriteLn( 'Regex Replace:' );
  WriteLn( '  --file <file> --regex-pattern <pattern> --regex-replace <replacement>' );
  WriteLn( '  -i, --case-insensitive  Case-insensitive matching' );
  WriteLn( '  -m, --multiline         Multi-line mode (^ and $ match line boundaries)' );
  WriteLn( '  --regex-test            Test pattern without changes' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL"' );
  WriteLn( '  StrEditor.exe --file "test.pas" --replace-line 25 --with "  NewCode;" --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --regex-pattern "f(\w+)" --regex-replace "l$1" -i' );
  WriteLn;
  WriteLn( 'Aliases: --rl = --replace-line, --ob64 = --old-str-base64, --nb64 = --new-str-base64' );
end;

class procedure TCommandLineParser.ShowHelpInsert;
begin
  WriteLn( 'StrEditor - INSERT Operations' );
  WriteLn( '==============================' );
  WriteLn;
  WriteLn( 'Usage:' );
  WriteLn( '  --file <file> --text <text> --insert-after-line <n>' );
  WriteLn( '  --file <file> --text <text> --insert-before-line <n>' );
  WriteLn( '  --file <file> --text-base64 <b64> --insert-after-line <n>   # Base64 encoded' );
  WriteLn;
  WriteLn( 'JSON Config with text-lines (v1.8):' );
  WriteLn( '  {' );
  WriteLn( '    "file": "test.pas",' );
  WriteLn( '    "command": "insert-after",' );
  WriteLn( '    "insert-after-line": 10,' );
  WriteLn( '    "text-lines": ["  // Line 1", "  // Line 2", "  // Line 3"]' );
  WriteLn( '  }' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --text "// Comment" --insert-after-line 10' );
  WriteLn( '  StrEditor.exe --file "test.pas" --text "// Header" --insert-before-line 1 --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --text-base64 "eyRJRkRFRn0=" --ia 10' );
  WriteLn;
  WriteLn( 'Aliases: --ia = --insert-after-line, --ib = --insert-before-line' );
end;

class procedure TCommandLineParser.ShowHelpDelete;
begin
  WriteLn( 'StrEditor - DELETE Operations' );
  WriteLn( '==============================' );
  WriteLn;
  WriteLn( 'Single Line:' );
  WriteLn( '  --file <file> --delete-line <n>' );
  WriteLn;
  WriteLn( 'Multiple Lines:' );
  WriteLn( '  --file <file> --delete-lines <n,m,k>                     # Comma-separated' );
  WriteLn( '  --file <file> --delete-lines --start-line <n> --end-line <m>  # Range' );
  WriteLn;
  WriteLn( 'Options:' );
  WriteLn( '  --backup     Create backup before deletion' );
  WriteLn( '  --dry-run    Preview without deleting' );
  WriteLn( '  --diff       Show what would be deleted' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --delete-line 25 --backup' );
  WriteLn( '  StrEditor.exe --file "test.pas" --delete-lines "1,3,5" --dry-run' );
  WriteLn( '  StrEditor.exe --file "test.pas" --delete-lines --start-line 10 --end-line 20 --backup' );
  WriteLn;
  WriteLn( 'Alias: --dl = --delete-line' );
end;

class procedure TCommandLineParser.ShowHelpShow;
begin
  WriteLn( 'StrEditor - SHOW/VIEW Operations' );
  WriteLn( '=================================' );
  WriteLn;
  WriteLn( 'Usage:' );
  WriteLn( '  --file <file> --show' );
  WriteLn( '  --file <file> --cat           # Alias for --show' );
  WriteLn;
  WriteLn( 'Options:' );
  WriteLn( '  --head <n>           Show first N lines' );
  WriteLn( '  --tail <n>           Show last N lines' );
  WriteLn( '  --range <start>,<end>  Show lines from start to end (e.g., --range 10,20)' );
  WriteLn( '  --start-line <n>     Show from line n' );
  WriteLn( '  --end-line <n>       Show until line n' );
  WriteLn( '  --line-numbers       Show line numbers' );
  WriteLn( '  --raw             Output as single string' );
  WriteLn( '  --hex             Show raw bytes as hex dump (16 bytes per line)' );
  WriteLn( '  --base64          Output file content as Base64 string' );
  WriteLn;
  WriteLn( 'Documentation:' );
  WriteLn( '  --docs                 Show README.md' );
  WriteLn( '  --docs <file>          Show specific doc file' );
  WriteLn( '  --docs --list          List available docs' );
  WriteLn( '  --docs --open-in-browser  Open in browser' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --show --head 10 --line-numbers' );
  WriteLn( '  StrEditor.exe --file "test.pas" --show --range 50,60' );
  WriteLn( '  StrEditor.exe --file "test.pas" --show --start-line 50 --end-line 60' );
  WriteLn( '  StrEditor.exe --file "test.pas" --show --hex --tail 32' );
  WriteLn( '  StrEditor.exe --file "test.pas" --show --base64' );
  WriteLn( '  StrEditor.exe --docs CHANGELOG.md --tail 20' );
end;

class procedure TCommandLineParser.ShowHelpEncoding;
begin
  WriteLn( 'StrEditor - ENCODING Operations' );
  WriteLn( '================================' );
  WriteLn;
  WriteLn( 'Detect Encoding:' );
  WriteLn( '  --file <file> --detect-encoding' );
  WriteLn;
  WriteLn( 'Convert Encoding:' );
  WriteLn( '  --file <file> --convert-encoding --to <utf8|windows1252>' );
  WriteLn;
  WriteLn( 'Reinterpret (Repair Broken):' );
  WriteLn( '  --file <file> --reinterpret-as <utf8|windows1252>' );
  WriteLn( '  # Use when UTF-8 bytes were saved as Windows-1252 or vice versa' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --detect-encoding' );
  WriteLn( '  StrEditor.exe --file "test.pas" --convert-encoding --to utf8 --backup' );
  WriteLn( '  StrEditor.exe --file "broken.pas" --reinterpret-as utf8 --backup' );
end;

class procedure TCommandLineParser.ShowHelpConfig;
begin
  WriteLn( 'StrEditor - JSON CONFIG Operations' );
  WriteLn( '===================================' );
  WriteLn;
  WriteLn( 'Usage:' );
  WriteLn( '  --config <file.json>              # JSON is AUTO-DELETED on success!' );
  WriteLn( '  --config <file.json> --keep-config  # Keep JSON file after success' );
  WriteLn;
  WriteLn( 'IMPORTANT: JSON config file is AUTOMATICALLY DELETED after successful execution!' );
  WriteLn( '           Use --keep-config to preserve the file.' );
  WriteLn;
  WriteLn( 'Single Operation JSON:' );
  WriteLn( '  {' );
  WriteLn( '    "file": "test.pas",' );
  WriteLn( '    "old-str": "nil",' );
  WriteLn( '    "new-str": "NIL"' );
  WriteLn( '  }' );
  WriteLn;
  WriteLn( 'Multiple Operations JSON:' );
  WriteLn( '  {' );
  WriteLn( '    "operations": [' );
  WriteLn( '      { "file": "a.pas", "command": "delete-line", "line": 25 },' );
  WriteLn( '      { "file": "b.pas", "command": "insert-after", "insert-after-line": 10,' );
  WriteLn( '        "text-lines": ["Line 1", "Line 2"] }' );
  WriteLn( '    ]' );
  WriteLn( '  }' );
  WriteLn;
  WriteLn( 'text-lines Array (v1.8):' );
  WriteLn( '  Instead of: "text": "Line1\r\nLine2"  (problematic)' );
  WriteLn( '  Use:        "text-lines": ["Line1", "Line2"]  (recommended)' );
  WriteLn;
  WriteLn( 'Available Commands:' );
  WriteLn( '  str-replace, insert, insert-after, insert-before, regex-replace,' );
  WriteLn( '  delete-line, delete-lines, replace-line, replace-lines, move-lines,' );
  WriteLn( '  indent, unindent' );
  WriteLn;
  WriteLn( 'Example:' );
  WriteLn( '  StrEditor.exe --config "ops.json"              # JSON auto-deleted' );
  WriteLn( '  StrEditor.exe --config "ops.json" --keep-config  # JSON preserved' );
end;

class procedure TCommandLineParser.ShowHelpMove;
begin
  WriteLn( 'StrEditor - MOVE LINES Operations' );
  WriteLn( '==================================' );
  WriteLn;
  WriteLn( 'Move lines from one file to another:' );
  WriteLn( '  --move-lines --from <src> --to <dst> --start-line <n> --end-line <m> --insert-after-line <l>' );
  WriteLn( '  --move-lines --from <src> --to <dst> --start-line <n> --end-line <m> --insert-before-line <l>' );
  WriteLn;
  WriteLn( 'Options:' );
  WriteLn( '  --backup     Create backup of both files' );
  WriteLn( '  --dry-run    Preview without moving' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  # Move lines 50-100 from UnitA.pas after line 200 in UnitB.pas' );
  WriteLn( '  StrEditor.exe --move-lines --from "UnitA.pas" --to "UnitB.pas" \\' );
  WriteLn( '    --start-line 50 --end-line 100 --insert-after-line 200' );
  WriteLn;
  WriteLn( '  # Move lines 10-20 before line 50' );
  WriteLn( '  StrEditor.exe --move-lines --from "Src.pas" --to "Dst.pas" \\' );
  WriteLn( '    --start-line 10 --end-line 20 --insert-before-line 50 --backup' );
end;

class procedure TCommandLineParser.ShowHelpRepair;
begin
  WriteLn( 'StrEditor - REPAIR UMLAUTS Operations' );
  WriteLn( '======================================' );
  WriteLn;
  WriteLn( 'Repair corrupted umlauts (9D bytes) using version control:' );
  WriteLn( '  --file <file> --repair-umlauts [--vcs hg|git] [--revision <rev>]' );
  WriteLn;
  WriteLn( 'Or using a reference file:' );
  WriteLn( '  --file <file> --repair-umlauts --reference <good-file>' );
  WriteLn;
  WriteLn( 'Options:' );
  WriteLn( '  --vcs <hg|git>     Version control (default: auto-detect)' );
  WriteLn( '  --revision <rev>   VCS revision (default: .^ for hg, HEAD~1 for git)' );
  WriteLn( '  --reference <f>    Use reference file instead of VCS' );
  WriteLn( '  --verbose          Show detailed repair information' );
  WriteLn( '  --dry-run          Preview without repairing' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --repair-umlauts --verbose' );
  WriteLn( '  StrEditor.exe --file "test.pas" --repair-umlauts --vcs hg --revision ".^"' );
  WriteLn( '  StrEditor.exe --file "broken.pas" --repair-umlauts --reference "original.pas"' );
end;

class procedure TCommandLineParser.ShowHelpAll;
begin
  ShowHelpOverview;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpReplace;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpInsert;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpDelete;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpShow;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpEncoding;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpConfig;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpMove;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpRepair;
  WriteLn;
  WriteLn( String.Create( '=', 60 ) );
  WriteLn;
  ShowHelpCompare;
end;

class procedure TCommandLineParser.ShowHelpCompare;
begin
  WriteLn( 'StrEditor - FILE COMPARE Operations' );
  WriteLn( '=====================================' );
  WriteLn;
  WriteLn( 'Compare file against master for broken special characters:' );
  WriteLn( '  --file <file1> --filecompare <master-file>' );
  WriteLn;
  WriteLn( 'Logic:' );
  WriteLn( '  1. Detect encoding of both files' );
  WriteLn( '  2. If encoding differs -> exit with code 1' );
  WriteLn( '  3. Read master file, collect lines with special chars' );
  WriteLn( '  4. Check file1 for matching lines using substring matching' );
  WriteLn( '  5. Report broken/missing special characters' );
  WriteLn;
  WriteLn( 'Special characters checked: ' + Chr( $F6 ) + Chr( $E4 ) + Chr( $FC ) + Chr( $D6 ) + Chr( $C4 ) + Chr( $DC ) + Chr( $DF ) + Chr( $E9 ) + Chr( $A7 ) );
  WriteLn;
  WriteLn( 'Exit Codes:' );
  WriteLn( '  0 = OK, all special characters match (or binary identical)' );
  WriteLn( '  1 = Encoding mismatch between files' );
  WriteLn( '  2 = Special characters broken (at least one wrong)' );
  WriteLn( '  3 = Line not found (manual review needed)' );
  WriteLn( '  4 = BOM difference only (content identical, only BOM differs)' );
  WriteLn;
  WriteLn( 'Options:' );
  WriteLn( '  --verbose    Show detailed comparison output' );
  WriteLn;
  WriteLn( 'Examples:' );
  WriteLn( '  StrEditor.exe --file "test.pas" --filecompare "master.pas"' );
  WriteLn( '  StrEditor.exe --file "test.pas" --filecompare "master.pas" --verbose' );
end;

class procedure TCommandLineParser.ShowVersion;
begin
  WriteLn( 'StrEditor v1.9.1' );
  WriteLn( 'Build: 2026-02-26' );
  WriteLn( 'Delphi String Replace Tool with Encoding Preservation' );
  WriteLn;
  WriteLn( 'New in v1.9.1:' );
  WriteLn( '  - Retry-Mechanismus für File-Access-Errors (E/A-Fehler 232)' );
  WriteLn( '  - 3 Versuche mit 100ms Delay bei EInOutError, EFOpenError, EFCreateError' );
  WriteLn( '  - Verhindert Race-Conditions bei parallelen Augment-Aufrufen' );
  WriteLn;
  WriteLn( 'Previous version v1.9.0:' );
  WriteLn( '  - Tolerant JSON-Config validation (Postel''s Law: accept + warn)' );
  WriteLn( '  - Accept "action"/"operation" as alias for "command" with warning' );
  WriteLn( '  - Accept JSON array [{...}] format for multiple operations' );
  WriteLn( '  - Accept top-level "file" as fallback for operations without "file"' );
  WriteLn( '  - Accept "old"/"new" as alias for "old-str"/"new-str" with warning' );
  WriteLn( '  - Warn on unescaped backslashes in file paths' );
  WriteLn;
  WriteLn( 'Previous versions:' );
  WriteLn( '  v1.8.7: --filecompare, Fragment-based matching' );
  WriteLn( '  v1.8.6: --range for --show, --keep-config deprecation warning' );
  WriteLn( '  v1.8.5: Bug Fix: --dry-run with JSON config, Command-line flag priority' );
  WriteLn( '  v1.8.4: ChangeReport, ContextLines, SessionLog, INI-Config' );
  WriteLn( '  v1.8.3: Auto-Delete JSON Config, --keep-config' );
  WriteLn( '  v1.8.2: Indent/Unindent Lines (--indent-lines, --unindent-lines)' );
  WriteLn( '  v1.8.1: Hex-Dump (--hex), Base64 (--base64), Original Line Numbers' );
  WriteLn( '  v1.8.0: text-lines Array, replace-lines Command' );
  WriteLn( '  v1.7.x: Delete/Replace Lines, Move Lines, JSON Config' );
  WriteLn;
  WriteLn( 'New in v1.7.5:' );
  WriteLn( '  - Move Lines (--move-lines --from <src> --to <dst> --start-line <n> --end-line <m>)' );
  WriteLn( '  - Move code between files with encoding preservation' );
  WriteLn( '  - Supports --insert-after-line and --insert-before-line' );
  WriteLn( '  - JSON-Config support for move-lines' );
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
