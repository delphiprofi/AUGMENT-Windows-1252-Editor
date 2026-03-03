Unit StrEditor.MCP;

interface

Uses
  System.SysUtils
, System.Classes
, System.JSON
, StrEditor.CommandLine
, StrEditor.Operations
, StrEditor.Encoding
, StrEditor.BatchProcessor
, StrEditor.Config
, StrEditor.Settings
, StrEditor.ChangeReport
, StrEditor.Regex
, StrEditor.Undo
, StrEditor.FileCompare
, StrEditor.Repair
, StrEditor.SessionLog
;

Type
  TMCPServer = class
    strict private
      fRunning       : Boolean;
      fWorkspaceRoot : string;

      function ResolvePath( const aPath : string ) : string;
      function HandleInitialize( const aId : TJSONValue; const aParams : TJSONObject ) : TJSONObject;
      function HandleToolsList( const aId : TJSONValue ) : TJSONObject;
      function HandleToolsCall( const aId : TJSONValue; const aParams : TJSONObject ) : TJSONObject;

      function BuildToolsArray : TJSONArray;

      function HandleStrReplace( const aArgs : TJSONObject ) : TJSONObject;
      function HandleEditFile( const aArgs : TJSONObject ) : TJSONObject;
      function HandleShowFile( const aArgs : TJSONObject ) : TJSONObject;
      function HandleDetectEncoding( const aArgs : TJSONObject ) : TJSONObject;
      function HandleRegexReplace( const aArgs : TJSONObject ) : TJSONObject;
      function HandleRegexTest( const aArgs : TJSONObject ) : TJSONObject;
      function HandleMoveLines( const aArgs : TJSONObject ) : TJSONObject;
      function HandleIndentLines( const aArgs : TJSONObject ) : TJSONObject;
      function HandleUnindentLines( const aArgs : TJSONObject ) : TJSONObject;
      function HandleConvertEncoding( const aArgs : TJSONObject ) : TJSONObject;
      function HandleRepairUmlauts( const aArgs : TJSONObject ) : TJSONObject;
      function HandleFileCompare( const aArgs : TJSONObject ) : TJSONObject;
      function HandleUndo( const aArgs : TJSONObject ) : TJSONObject;
      function HandleCommentLines( const aArgs : TJSONObject ) : TJSONObject;
      function HandleUncommentLines( const aArgs : TJSONObject ) : TJSONObject;
      function HandleRestartServer( const aArgs : TJSONObject ) : TJSONObject;

      function MakeResult( const aId : TJSONValue; const aResult : TJSONValue ) : TJSONObject;
      function MakeError( const aId : TJSONValue; aCode : Integer; const aMessage : string ) : TJSONObject;
      function MakeToolResult( const aText : string; aIsError : Boolean = false ) : TJSONObject;

      function MakeInputSchema( const aProperties : TJSONObject; const aRequired : TJSONArray ) : TJSONObject;

      procedure SendResponse( const aResponse : TJSONObject );
      function  ReadRequest : TJSONObject;
    public
      procedure Run;
  end;

implementation

Uses
  System.Math
, System.Generics.Collections
, System.NetEncoding
, Winapi.Windows
;

Const
  cRestartEventName = 'Local\StrEditorRestart';

// Watchdog-Thread: wartet auf das Named Event und beendet den Prozess sauber
function RestartWatchdogThread( aParam : Pointer ) : DWORD; stdcall;
Var
  lEvent : THandle;
begin
  Result := 0;

  lEvent := THandle( aParam );

  // Blockiert bis das Event gesetzt wird (oder der Prozess endet)
  if WaitForSingleObject( lEvent, INFINITE ) = WAIT_OBJECT_0 then
    begin
      TSessionLog.Instance.LogSuccess( 'MCP server shutdown: restart signal received' );
      // Prozess sauber beenden — ReadLn blockiert, daher ExitProcess
      ExitProcess( 0 );
    end;
end;

procedure TMCPServer.Run;
Var
  lRequest      : TJSONObject;
  lMethod       : string;
  lId           : TJSONValue;
  lParams       : TJSONObject;
  lResponse     : TJSONObject;
  lRestartEvent : THandle;
  lThreadId     : DWORD;
  lThread       : THandle;
begin
  fRunning := true;

  // Named Event erstellen (ManualReset=TRUE, InitialState=FALSE)
  // Alle MCP-Instanzen teilen sich dieses Event über den Namen
  lRestartEvent := CreateEvent( NIL, TRUE, FALSE, cRestartEventName );

  // Watchdog-Thread starten der auf das Event wartet
  lThread := 0;

  if lRestartEvent <> 0 then
    lThread := CreateThread( NIL, 0, @RestartWatchdogThread, Pointer( lRestartEvent ), 0, lThreadId );

  try
    // MCP Server läuft als stdio-Loop

    while fRunning do
      begin
        lRequest := ReadRequest;

        if lRequest = NIL then
          begin
            fRunning := false;
            Break;
          end;

        try
          lMethod := lRequest.GetValue<string>( 'method', '' );
          lId     := lRequest.GetValue( 'id' );

          if lMethod = 'initialize' then
            lResponse := HandleInitialize( lId, lRequest.GetValue<TJSONObject>( 'params' ) )

          else
          if lMethod = 'initialized' then
            Continue

          else
          if lMethod = 'tools/list' then
            lResponse := HandleToolsList( lId )

          else
          if lMethod = 'tools/call' then
            begin
              lParams   := lRequest.GetValue<TJSONObject>( 'params' );
              lResponse := HandleToolsCall( lId, lParams );
            end

          else
          if lMethod = 'shutdown' then
            begin
              fRunning  := false;
              lResponse := MakeResult( lId, TJSONObject.Create );
            end

          else
            lResponse := MakeError( lId, -32601, 'Method not found: ' + lMethod );

          SendResponse( lResponse );
          lResponse.Free;
        finally
          lRequest.Free;
        end;
      end;

  finally
    if lThread <> 0 then
      CloseHandle( lThread );

    if lRestartEvent <> 0 then
      CloseHandle( lRestartEvent );
  end;
end;

function TMCPServer.ReadRequest : TJSONObject;
Var
  lLine : string;
begin
  Result := NIL;

  // MCP verwendet standardmäßig JSON-RPC über stdio
  // Jede Nachricht ist eine einzelne JSON-Zeile
  try
    if EOF( Input ) then
      Exit;

    ReadLn( lLine );

    lLine := Trim( lLine );

    if lLine = '' then
      Exit;

    Result := TJSONObject.ParseJSONValue( lLine ) as TJSONObject;
  except
    on E : Exception do
      begin
        WriteLn( ErrOutput, 'ERROR reading request: ' + E.Message );
        Result := NIL;
      end;
  end;
end;

procedure TMCPServer.SendResponse( const aResponse : TJSONObject );
begin
  WriteLn( aResponse.ToJSON );
  Flush( Output );
end;

function TMCPServer.MakeResult( const aId : TJSONValue; const aResult : TJSONValue ) : TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair( 'jsonrpc', '2.0' );

  if aId <> NIL
    then Result.AddPair( 'id', aId.Clone as TJSONValue )
    else Result.AddPair( 'id', TJSONNull.Create );

  Result.AddPair( 'result', aResult );
end;

function TMCPServer.MakeError( const aId : TJSONValue; aCode : Integer; const aMessage : string ) : TJSONObject;
Var
  lError : TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair( 'jsonrpc', '2.0' );

  if aId <> NIL
    then Result.AddPair( 'id', aId.Clone as TJSONValue )
    else Result.AddPair( 'id', TJSONNull.Create );

  lError := TJSONObject.Create;
  lError.AddPair( 'code', TJSONNumber.Create( aCode ) );
  lError.AddPair( 'message', aMessage );
  Result.AddPair( 'error', lError );
end;

function TMCPServer.MakeToolResult( const aText : string; aIsError : Boolean = false ) : TJSONObject;
Var
  lContent : TJSONObject;
  lArray   : TJSONArray;
begin
  lContent := TJSONObject.Create;
  lContent.AddPair( 'type', 'text' );
  lContent.AddPair( 'text', aText );

  lArray := TJSONArray.Create;
  lArray.AddElement( lContent );

  Result := TJSONObject.Create;
  Result.AddPair( 'content', lArray );

  if aIsError then
    Result.AddPair( 'isError', TJSONBool.Create( true ) );
end;

function TMCPServer.MakeInputSchema( const aProperties : TJSONObject; const aRequired : TJSONArray ) : TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair( 'type', 'object' );
  Result.AddPair( 'properties', aProperties );
  Result.AddPair( 'required', aRequired );
end;

function TMCPServer.ResolvePath( const aPath : string ) : string;
begin
  if aPath = '' then
    Exit( '' );

  // Absoluter Pfad → direkt verwenden
  if ( Length( aPath ) >= 2 ) and ( aPath[ 2 ] = ':' ) then
    Exit( aPath );

  if ( Length( aPath ) >= 1 ) and ( aPath[ 1 ] = '\' ) then
    Exit( aPath );

  // Relativer Pfad → gegen Workspace-Root auflösen
  if fWorkspaceRoot <> '' then
    Result := IncludeTrailingPathDelimiter( fWorkspaceRoot ) + aPath
  else
    Result := aPath;
end;

function TMCPServer.HandleInitialize( const aId : TJSONValue; const aParams : TJSONObject ) : TJSONObject;
Var
  lResult       : TJSONObject;
  lCapabilities : TJSONObject;
  lServerInfo   : TJSONObject;
  lRoots        : TJSONArray;
  lRootObj      : TJSONObject;
  lUri          : string;
begin
  // Workspace-Root aus initialize-Params auslesen
  fWorkspaceRoot := '';

  if aParams <> NIL then
    begin
      // MCP 2024-11-05: roots Array
      if aParams.FindValue( 'roots' ) is TJSONArray
        then lRoots := aParams.FindValue( 'roots' ) as TJSONArray
        else lRoots := NIL;

      if ( lRoots <> NIL ) and ( lRoots.Count > 0 ) then
        begin
          lRootObj := lRoots.Items[ 0 ] as TJSONObject;
          lUri     := lRootObj.GetValue<string>( 'uri', '' );

          // file:///C:/path → C:\path
          if lUri.StartsWith( 'file:///' ) then
            begin
              fWorkspaceRoot := lUri.Substring( 8 ).Replace( '/', '\' );

              // URL-Decoding für %20 etc.
              fWorkspaceRoot := TNetEncoding.URL.Decode( fWorkspaceRoot );
            end;
        end;

      // Fallback: workspaceFolders (älteres Format)
      if fWorkspaceRoot = '' then
        begin
          if aParams.FindValue( 'workspaceFolders' ) is TJSONArray
            then lRoots := aParams.FindValue( 'workspaceFolders' ) as TJSONArray
            else lRoots := NIL;

          if ( lRoots <> NIL ) and ( lRoots.Count > 0 ) then
            begin
              lRootObj := lRoots.Items[ 0 ] as TJSONObject;
              lUri     := lRootObj.GetValue<string>( 'uri', '' );

              if lUri.StartsWith( 'file:///' ) then
                begin
                  fWorkspaceRoot := lUri.Substring( 8 ).Replace( '/', '\' );
                  fWorkspaceRoot := TNetEncoding.URL.Decode( fWorkspaceRoot );
                end;
            end;
        end;
    end;



  lCapabilities := TJSONObject.Create;
  lCapabilities.AddPair( 'tools', TJSONObject.Create );

  lServerInfo := TJSONObject.Create;
  lServerInfo.AddPair( 'name', 'StrEditor' );
  lServerInfo.AddPair( 'version', '1.10.2' );

  lResult := TJSONObject.Create;
  lResult.AddPair( 'protocolVersion', '2024-11-05' );
  lResult.AddPair( 'capabilities', lCapabilities );
  lResult.AddPair( 'serverInfo', lServerInfo );

  Result := MakeResult( aId, lResult );
end;

function TMCPServer.HandleToolsList( const aId : TJSONValue ) : TJSONObject;
Var
  lResult : TJSONObject;
begin
  lResult := TJSONObject.Create;
  lResult.AddPair( 'tools', BuildToolsArray );
  Result := MakeResult( aId, lResult );
end;

function TMCPServer.HandleToolsCall( const aId : TJSONValue; const aParams : TJSONObject ) : TJSONObject;

  procedure ResolvePathParam( const aArgs : TJSONObject; const aKey : string );
  Var
    lValue    : string;
    lResolved : string;
  begin
    lValue := aArgs.GetValue<string>( aKey, '' );

    if lValue <> '' then
      begin
        lResolved := ResolvePath( lValue );

        if lResolved <> lValue then
          begin
            aArgs.RemovePair( aKey );
            aArgs.AddPair( aKey, lResolved );
          end;
      end;
  end;

Var
  lToolName   : string;
  lArgs       : TJSONObject;
  lToolResult : TJSONObject;
begin
  lToolName := aParams.GetValue<string>( 'name', '' );
  lArgs     := aParams.GetValue<TJSONObject>( 'arguments' );

  TSessionLog.Instance.LogSuccess( 'MCP tool call: ' + lToolName );

  if lArgs = NIL then
    lArgs := TJSONObject.Create;

  // Relative Pfade gegen Workspace-Root auflösen
  ResolvePathParam( lArgs, 'file' );
  ResolvePathParam( lArgs, 'file1' );
  ResolvePathParam( lArgs, 'file2' );
  ResolvePathParam( lArgs, 'from_file' );
  ResolvePathParam( lArgs, 'to_file' );
  ResolvePathParam( lArgs, 'reference' );

  try
    if lToolName = 'str_replace' then
      lToolResult := HandleStrReplace( lArgs )

    else
    if lToolName = 'edit_file' then
      lToolResult := HandleEditFile( lArgs )

    else
    if lToolName = 'show_file' then
      lToolResult := HandleShowFile( lArgs )

    else
    if lToolName = 'detect_encoding' then
      lToolResult := HandleDetectEncoding( lArgs )

    else
    if lToolName = 'regex_replace' then
      lToolResult := HandleRegexReplace( lArgs )

    else
    if lToolName = 'regex_test' then
      lToolResult := HandleRegexTest( lArgs )

    else
    if lToolName = 'move_lines' then
      lToolResult := HandleMoveLines( lArgs )

    else
    if lToolName = 'indent_lines' then
      lToolResult := HandleIndentLines( lArgs )

    else
    if lToolName = 'unindent_lines' then
      lToolResult := HandleUnindentLines( lArgs )

    else
    if lToolName = 'convert_encoding' then
      lToolResult := HandleConvertEncoding( lArgs )

    else
    if lToolName = 'repair_umlauts' then
      lToolResult := HandleRepairUmlauts( lArgs )

    else
    if lToolName = 'file_compare' then
      lToolResult := HandleFileCompare( lArgs )

    else
    if lToolName = 'undo' then
      lToolResult := HandleUndo( lArgs )

    else
    if lToolName = 'comment_lines' then
      lToolResult := HandleCommentLines( lArgs )

    else
    if lToolName = 'uncomment_lines' then
      lToolResult := HandleUncommentLines( lArgs )

    else
    if lToolName = 'restart_server' then
      lToolResult := HandleRestartServer( lArgs )

    else
      lToolResult := MakeToolResult( 'Unknown tool: ' + lToolName, true );

    Result := MakeResult( aId, lToolResult );
  except
    on E : Exception do
      begin
        TSessionLog.Instance.LogError( 'MCP tool exception [' + lToolName + ']: ' + E.ClassName + ': ' + E.Message );
        Result := MakeResult( aId, MakeToolResult( 'Exception: ' + E.ClassName + ': ' + E.Message, true ) );
      end;
  end;
end;

function TMCPServer.BuildToolsArray : TJSONArray;

  function StringProp( const aDesc : string ) : TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.AddPair( 'type', 'string' );
    Result.AddPair( 'description', aDesc );
  end;

  function IntProp( const aDesc : string ) : TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.AddPair( 'type', 'integer' );
    Result.AddPair( 'description', aDesc );
  end;

  function BoolProp( const aDesc : string ) : TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.AddPair( 'type', 'boolean' );
    Result.AddPair( 'description', aDesc );
  end;

  function ArrayProp( const aDesc : string ) : TJSONObject;
  Var
    lItems : TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.AddPair( 'type', 'array' );
    Result.AddPair( 'description', aDesc );
    lItems := TJSONObject.Create;
    lItems.AddPair( 'type', 'object' );
    Result.AddPair( 'items', lItems );
  end;

  function ReqArray( const aValues : array of string ) : TJSONArray;
  begin
    Result := TJSONArray.Create;

    for Var lVal in aValues do
      Result.Add( lVal );
  end;

Var
  lTool  : TJSONObject;
  lProps : TJSONObject;
begin
  Result := TJSONArray.Create;

  // Tool 1: str_replace
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file to edit' ) );
  lProps.AddPair( 'old_str', StringProp( 'String to search for' ) );
  lProps.AddPair( 'new_str', StringProp( 'Replacement string' ) );
  lProps.AddPair( 'start_line', IntProp( 'Start line (optional, 1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'End line (optional, 1-based)' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview changes without writing (default: false)' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create backup file (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'str_replace' );
  lTool.AddPair( 'description', 'Replace a string in a file. Preserves Windows-1252 and UTF-8 encoding. No Base64 needed - pass strings directly.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file', 'old_str', 'new_str' ] ) ) );
  Result.AddElement( lTool );

  // Tool 2: edit_file
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Default file path (used if operations do not specify their own)' ) );
  lProps.AddPair( 'operations', ArrayProp( 'Array of operations. Each: {command, file?, line?, start-line?, end-line?, text?, text-lines?, insert-after-line?, insert-before-line?, old-str?, new-str?}. Commands: delete-line, delete-lines, replace-line, replace-lines, insert-after, insert-before, str-replace. All line numbers refer to ORIGINAL file state.' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview changes without writing (default: false)' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create backup file (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'edit_file' );
  lTool.AddPair( 'description', 'Execute batch operations on a file (delete, insert, replace lines). All line numbers refer to ORIGINAL file state - no offset calculation needed. Operations are auto-sorted highest-line-first.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'operations' ] ) ) );
  Result.AddElement( lTool );

  // Tool 3: show_file
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file to show' ) );
  lProps.AddPair( 'start_line', IntProp( 'Start line (optional, 1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'End line (optional, 1-based)' ) );
  lProps.AddPair( 'head', IntProp( 'Show first N lines (optional)' ) );
  lProps.AddPair( 'tail', IntProp( 'Show last N lines (optional)' ) );
  lProps.AddPair( 'line_numbers', BoolProp( 'Show line numbers (default: true)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'show_file' );
  lTool.AddPair( 'description', 'Show file contents with correct encoding (Windows-1252 / UTF-8). Use this instead of cat/type for Delphi files to preserve umlauts.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file' ] ) ) );
  Result.AddElement( lTool );

  // Tool 4: detect_encoding
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file to check' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'detect_encoding' );
  lTool.AddPair( 'description', 'Detect file encoding (Windows-1252 or UTF-8 with BOM)' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file' ] ) ) );
  Result.AddElement( lTool );

  // Tool 5: regex_replace
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file to edit' ) );
  lProps.AddPair( 'pattern', StringProp( 'Regex pattern to search for' ) );
  lProps.AddPair( 'replacement', StringProp( 'Replacement string (supports $1, $2 etc.)' ) );
  lProps.AddPair( 'start_line', IntProp( 'Start line (optional, 1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'End line (optional, 1-based)' ) );
  lProps.AddPair( 'case_insensitive', BoolProp( 'Case insensitive matching (default: false)' ) );
  lProps.AddPair( 'multi_line', BoolProp( 'Multi-line mode (default: false)' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview without writing (default: false)' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create backup (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'regex_replace' );
  lTool.AddPair( 'description', 'Replace text using regex pattern. Supports capture groups ($1, $2).' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file', 'pattern', 'replacement' ] ) ) );
  Result.AddElement( lTool );

  // Tool 6: regex_test
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file to search' ) );
  lProps.AddPair( 'pattern', StringProp( 'Regex pattern to test' ) );
  lProps.AddPair( 'start_line', IntProp( 'Start line (optional, 1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'End line (optional, 1-based)' ) );
  lProps.AddPair( 'case_insensitive', BoolProp( 'Case insensitive matching (default: false)' ) );
  lProps.AddPair( 'multi_line', BoolProp( 'Multi-line mode (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'regex_test' );
  lTool.AddPair( 'description', 'Test if regex pattern matches in file. Returns match count without modifying the file.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file', 'pattern' ] ) ) );
  Result.AddElement( lTool );

  // Tool 7: move_lines
  lProps := TJSONObject.Create;
  lProps.AddPair( 'from_file', StringProp( 'Source file path' ) );
  lProps.AddPair( 'to_file', StringProp( 'Destination file path' ) );
  lProps.AddPair( 'start_line', IntProp( 'First line to move (1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'Last line to move (1-based)' ) );
  lProps.AddPair( 'insert_after_line', IntProp( 'Insert after this line in target (0 = beginning)' ) );
  lProps.AddPair( 'insert_before_line', IntProp( 'Insert before this line in target (alternative to insert_after_line)' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview without writing (default: false)' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create backup (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'move_lines' );
  lTool.AddPair( 'description', 'Move lines from one file to another (or within same file). Lines are removed from source and inserted at target position.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'from_file', 'to_file', 'start_line', 'end_line' ] ) ) );
  Result.AddElement( lTool );

  // Tool 8: indent_lines
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file' ) );
  lProps.AddPair( 'start_line', IntProp( 'First line to indent (1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'Last line to indent (1-based)' ) );
  lProps.AddPair( 'spaces', IntProp( 'Number of spaces to add (default: 2)' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview without writing (default: false)' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create backup (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'indent_lines' );
  lTool.AddPair( 'description', 'Add indentation (spaces) to a range of lines.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file', 'start_line', 'end_line' ] ) ) );
  Result.AddElement( lTool );

  // Tool 9: unindent_lines
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file' ) );
  lProps.AddPair( 'start_line', IntProp( 'First line to unindent (1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'Last line to unindent (1-based)' ) );
  lProps.AddPair( 'spaces', IntProp( 'Number of spaces to remove (default: 2)' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview without writing (default: false)' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create backup (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'unindent_lines' );
  lTool.AddPair( 'description', 'Remove indentation (spaces) from a range of lines.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file', 'start_line', 'end_line' ] ) ) );
  Result.AddElement( lTool );

  // Tool 10: convert_encoding
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file' ) );
  lProps.AddPair( 'target_encoding', StringProp( 'Target encoding: "utf8" or "windows-1252"' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview without writing (default: false)' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create backup (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'convert_encoding' );
  lTool.AddPair( 'description', 'Convert file encoding between UTF-8 (with BOM) and Windows-1252.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file', 'target_encoding' ] ) ) );
  Result.AddElement( lTool );

  // Tool 11: repair_umlauts
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file with broken umlauts' ) );
  lProps.AddPair( 'reference', StringProp( 'Path to reference file with correct umlauts (optional)' ) );
  lProps.AddPair( 'vcs', StringProp( 'VCS type: "hg" or "git" (default: "hg", used if no reference file)' ) );
  lProps.AddPair( 'revision', StringProp( 'VCS revision to use as reference (optional)' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview without writing (default: false)' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create backup (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'repair_umlauts' );
  lTool.AddPair( 'description', 'Repair broken umlauts in a file by comparing with a reference file or VCS version.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file' ] ) ) );
  Result.AddElement( lTool );

  // Tool 12: file_compare
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file1', StringProp( 'Path to first file (master/reference)' ) );
  lProps.AddPair( 'file2', StringProp( 'Path to second file (to check)' ) );
  lProps.AddPair( 'verbose', BoolProp( 'Verbose output (default: false)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'file_compare' );
  lTool.AddPair( 'description', 'Compare two files for encoding differences and broken special characters (umlauts etc.).' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file1', 'file2' ] ) ) );
  Result.AddElement( lTool );

  // Tool 13: undo
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file to undo (restores from .bak)' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'undo' );
  lTool.AddPair( 'description', 'Undo last change by restoring from .bak backup file.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file' ] ) ) );
  Result.AddElement( lTool );

  // Tool 14: comment_lines
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file' ) );
  lProps.AddPair( 'start_line', IntProp( 'First line to comment (1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'Last line to comment (1-based)' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview only, do not modify file' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create .bak backup before modifying' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'comment_lines' );
  lTool.AddPair( 'description', 'Comment out lines by prepending // at the beginning of each line. Empty lines and already commented lines also get //. Preserves encoding.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file', 'start_line', 'end_line' ] ) ) );
  Result.AddElement( lTool );

  // Tool 15: uncomment_lines
  lProps := TJSONObject.Create;
  lProps.AddPair( 'file', StringProp( 'Path to the file' ) );
  lProps.AddPair( 'start_line', IntProp( 'First line to uncomment (1-based)' ) );
  lProps.AddPair( 'end_line', IntProp( 'Last line to uncomment (1-based)' ) );
  lProps.AddPair( 'dry_run', BoolProp( 'Preview only, do not modify file' ) );
  lProps.AddPair( 'backup', BoolProp( 'Create .bak backup before modifying' ) );

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'uncomment_lines' );
  lTool.AddPair( 'description', 'Uncomment lines by removing leading // from each line. Tolerant: removes "// " (with space) or "//" (without space). Preserves encoding.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [ 'file', 'start_line', 'end_line' ] ) ) );
  Result.AddElement( lTool );

  // Tool 16: restart_server
  lProps := TJSONObject.Create;

  lTool := TJSONObject.Create;
  lTool.AddPair( 'name', 'restart_server' );
  lTool.AddPair( 'description', 'Shutdown the MCP server so it can be updated and restarted by the host. Use after deploying a new StrEditor.exe.' );
  lTool.AddPair( 'inputSchema', MakeInputSchema( lProps, ReqArray( [] ) ) );
  Result.AddElement( lTool );
end;

function TMCPServer.HandleStrReplace( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath : string;
  lOldStr   : string;
  lNewStr   : string;
  lStart    : Integer;
  lEnd      : Integer;
  lDryRun   : Boolean;
  lBackup   : Boolean;
  lResult   : TOperationResult;
begin
  lFilePath := aArgs.GetValue<string>( 'file', '' );
  lOldStr   := aArgs.GetValue<string>( 'old_str', '' );
  lNewStr   := aArgs.GetValue<string>( 'new_str', '' );
  lStart    := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd      := aArgs.GetValue<Integer>( 'end_line', 0 );
  lDryRun   := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup   := aArgs.GetValue<Boolean>( 'backup', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  if lOldStr = '' then
    Exit( MakeToolResult( 'ERROR: old_str parameter required', true ) );

  lResult := TStringOperations.StrReplace( lFilePath, lOldStr, lNewStr, lStart, lEnd, lDryRun, lBackup, false, ccNone, 0, '', false, false, false, false, false );

  if lResult.Success
    then Result := MakeToolResult( 'OK: ' + IntToStr( lResult.LinesChanged ) + ' line(s) changed in ' + lFilePath )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleEditFile( const aArgs : TJSONObject ) : TJSONObject;
Var
  lDefaultFile : string;
  lOpsArray    : TJSONArray;
  lOperations  : TArray<TCommandLineParams>;
  lDryRun      : Boolean;
  lBackup      : Boolean;
  lFilePath    : string;
  lOpObj       : TJSONObject;
  lParams      : TCommandLineParams;
  lCommand     : string;
  i            : Integer;
  lTextLines   : TJSONArray;
  lTextParts   : TArray<string>;
begin
  lDefaultFile := aArgs.GetValue<string>( 'file', '' );
  lOpsArray    := aArgs.GetValue<TJSONArray>( 'operations' );
  lDryRun      := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup      := aArgs.GetValue<Boolean>( 'backup', false );

  if ( lOpsArray = NIL ) or ( lOpsArray.Count = 0 ) then
    Exit( MakeToolResult( 'ERROR: operations array required', true ) );

  SetLength( lOperations, lOpsArray.Count );
  lFilePath := '';

  for i := 0 to lOpsArray.Count - 1 do
    begin
      lOpObj := lOpsArray.Items[ i ] as TJSONObject;

      // Command
      lCommand := lOpObj.GetValue<string>( 'command', '' );

      if lCommand = '' then
        Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ': Missing required parameter "command"', true ) );

      // File - aus Operation oder Default
      lParams := Default( TCommandLineParams );
      lParams.FilePath := lOpObj.GetValue<string>( 'file', lDefaultFile );
      lParams.DryRun   := lDryRun;
      lParams.Backup   := lBackup;

      if ( lFilePath = '' ) and ( lParams.FilePath <> '' ) then
        lFilePath := lParams.FilePath;

      // Command-Mapping with required parameter validation
      if ( lCommand = 'delete-line' ) then
        begin
          lParams.Command    := ctDeleteLine;
          lParams.LineNumber := lOpObj.GetValue<Integer>( 'line', 0 );

          if lParams.LineNumber <= 0 then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (delete-line): Missing or invalid required parameter "line" (must be >= 1)', true ) );
        end

      else
      if ( lCommand = 'delete-lines' ) then
        begin
          lParams.Command   := ctDeleteLines;
          lParams.StartLine := lOpObj.GetValue<Integer>( 'start-line', 0 );
          lParams.EndLine   := lOpObj.GetValue<Integer>( 'end-line', 0 );

          if lParams.StartLine <= 0 then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (delete-lines): Missing or invalid required parameter "start-line" (must be >= 1)', true ) );

          if lParams.EndLine <= 0 then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (delete-lines): Missing or invalid required parameter "end-line" (must be >= 1)', true ) );
        end

      else
      if ( lCommand = 'replace-line' ) then
        begin
          lParams.Command    := ctReplaceLine;
          lParams.LineNumber := lOpObj.GetValue<Integer>( 'line', 0 );
          lParams.Text       := lOpObj.GetValue<string>( 'text', '' );

          if lParams.LineNumber <= 0 then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (replace-line): Missing or invalid required parameter "line" (must be >= 1)', true ) );
        end

      else
      if ( lCommand = 'replace-lines' ) then
        begin
          lParams.Command   := ctReplaceLines;
          lParams.StartLine := lOpObj.GetValue<Integer>( 'start-line', 0 );
          lParams.EndLine   := lOpObj.GetValue<Integer>( 'end-line', 0 );

          if lParams.StartLine <= 0 then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (replace-lines): Missing or invalid required parameter "start-line" (must be >= 1)', true ) );

          if lParams.EndLine <= 0 then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (replace-lines): Missing or invalid required parameter "end-line" (must be >= 1)', true ) );
        end

      else
      if ( lCommand = 'insert-after' ) then
        begin
          lParams.Command        := ctInsert;
          lParams.InsertAfterLine := lOpObj.GetValue<Integer>( 'insert-after-line', 0 );

          if lParams.InsertAfterLine <= 0 then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (insert-after): Missing or invalid required parameter "insert-after-line" (must be >= 1)', true ) );
        end

      else
      if ( lCommand = 'insert-before' ) then
        begin
          lParams.Command         := ctInsertBefore;
          lParams.InsertBeforeLine := lOpObj.GetValue<Integer>( 'insert-before-line', 0 );

          if lParams.InsertBeforeLine <= 0 then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (insert-before): Missing or invalid required parameter "insert-before-line" (must be >= 1)', true ) );
        end

      else
      if ( lCommand = 'str-replace' ) then
        begin
          lParams.Command := ctStrReplace;
          lParams.OldStr  := lOpObj.GetValue<string>( 'old-str', '' );
          lParams.NewStr  := lOpObj.GetValue<string>( 'new-str', '' );

          if lParams.OldStr = '' then
            Exit( MakeToolResult( 'ERROR: Operation ' + IntToStr( i + 1 ) + ' (str-replace): Missing required parameter "old-str"', true ) );
        end

      else
        Exit( MakeToolResult( 'ERROR: Unknown command "' + lCommand + '" in operation ' + IntToStr( i + 1 ), true ) );

      // text-lines Array → Text mit CRLF (FindValue statt GetValue um EJSONException zu vermeiden)
      if ( lOpObj.FindValue( 'text-lines' ) <> NIL ) and ( lOpObj.FindValue( 'text-lines' ) is TJSONArray )
        then lTextLines := TJSONArray( lOpObj.FindValue( 'text-lines' ) )
        else lTextLines := NIL;

      if lTextLines <> NIL then
        begin
          SetLength( lTextParts, lTextLines.Count );

          for Var k := 0 to lTextLines.Count - 1 do
            lTextParts[ k ] := lTextLines.Items[ k ].Value;

          lParams.Text := String.Join( #13#10, lTextParts );
        end;

      lOperations[ i ] := lParams;
    end;

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: No file path specified', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  // BatchProcessor nutzen
  if TBatchProcessor.HasLineOperations( lOperations ) then
    begin
      if not TBatchProcessor.ProcessLineOperations( lFilePath, lOperations, NIL ) then
        Exit( MakeToolResult( 'ERROR: ProcessLineOperations failed for ' + lFilePath, true ) );

      Result := MakeToolResult( 'OK: ' + IntToStr( Length( lOperations ) ) + ' operation(s) executed on ' + lFilePath );
    end
  else
    Result := MakeToolResult( 'ERROR: No line operations found in operations array', true );
end;

function TMCPServer.HandleShowFile( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath    : string;
  lStart       : Integer;
  lEnd         : Integer;
  lHead        : Integer;
  lTail        : Integer;
  lLineNumbers : Boolean;
  lEncoding    : TEncodingType;
  lLines       : TStringList;
  lOutput      : TStringBuilder;
  lLineStart   : Integer;
  lLineEnd     : Integer;
  lMaxDigits   : Integer;
begin
  lFilePath    := aArgs.GetValue<string>( 'file', '' );
  lStart       := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd         := aArgs.GetValue<Integer>( 'end_line', 0 );
  lHead        := aArgs.GetValue<Integer>( 'head', 0 );
  lTail        := aArgs.GetValue<Integer>( 'tail', 0 );
  lLineNumbers := aArgs.GetValue<Boolean>( 'line_numbers', true );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  lEncoding := TEncodingHelper.DetectEncoding( lFilePath );
  lLines    := TStringList.Create;
  lOutput   := TStringBuilder.Create;
  try
    if lEncoding = etUTF8
      then lLines.LoadFromFile( lFilePath, TEncoding.UTF8 )
      else lLines.LoadFromFile( lFilePath, TEncoding.GetEncoding( 1252 ) );

    if lLines.Count = 0 then
      Exit( MakeToolResult( '' ) );

    lLineStart := 1;
    lLineEnd   := lLines.Count;

    if lHead > 0 then
      lLineEnd := System.Math.Min( lHead, lLines.Count );

    if lTail > 0 then
      lLineStart := System.Math.Max( 1, lLines.Count - lTail + 1 );

    if lStart > 0 then
      lLineStart := System.Math.Max( lStart, 1 );

    if lEnd > 0 then
      lLineEnd := System.Math.Min( lEnd, lLines.Count );

    lMaxDigits := Length( IntToStr( lLineEnd ) );

    for Var i := lLineStart - 1 to lLineEnd - 1 do
      begin
        if i > lLineStart - 1 then
          lOutput.Append( #13#10 );

        if lLineNumbers then
          begin
            Var lNum := IntToStr( i + 1 );

            while Length( lNum ) < lMaxDigits do
              lNum := ' ' + lNum;

            lOutput.Append( lNum + ': ' );
          end;

        lOutput.Append( lLines[ i ] );
      end;

    Result := MakeToolResult( lOutput.ToString );
  finally
    lLines.Free;
    lOutput.Free;
  end;
end;

function TMCPServer.HandleDetectEncoding( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath     : string;
  lEncoding     : TEncodingType;
  lEncodingName : string;
begin
  lFilePath := aArgs.GetValue<string>( 'file', '' );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  lEncoding := TEncodingHelper.DetectEncoding( lFilePath );

  case lEncoding of
    etUTF8        : lEncodingName := 'UTF-8 with BOM';
    etWindows1252 : lEncodingName := 'Windows-1252 (no BOM)';
    else            lEncodingName := 'Unknown';
  end;

  Result := MakeToolResult( 'File: ' + lFilePath + #13#10 + 'Encoding: ' + lEncodingName );
end;

function TMCPServer.HandleRegexReplace( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath        : string;
  lPattern         : string;
  lReplacement     : string;
  lStart           : Integer;
  lEnd             : Integer;
  lCaseInsensitive : Boolean;
  lMultiLine       : Boolean;
  lDryRun          : Boolean;
  lBackup          : Boolean;
  lResult          : TRegexOperationResult;
begin
  lFilePath        := aArgs.GetValue<string>( 'file', '' );
  lPattern         := aArgs.GetValue<string>( 'pattern', '' );
  lReplacement     := aArgs.GetValue<string>( 'replacement', '' );
  lStart           := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd             := aArgs.GetValue<Integer>( 'end_line', 0 );
  lCaseInsensitive := aArgs.GetValue<Boolean>( 'case_insensitive', false );
  lMultiLine       := aArgs.GetValue<Boolean>( 'multi_line', false );
  lDryRun          := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup          := aArgs.GetValue<Boolean>( 'backup', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  if lPattern = '' then
    Exit( MakeToolResult( 'ERROR: pattern parameter required', true ) );

  lResult := TRegexOperations.RegexReplace( lFilePath, lPattern, lReplacement, lStart, lEnd, lCaseInsensitive, lMultiLine, lDryRun, lBackup );

  if lResult.Success
    then Result := MakeToolResult( 'OK: ' + IntToStr( lResult.MatchCount ) + ' match(es), ' + IntToStr( lResult.LinesChanged ) + ' line(s) changed in ' + lFilePath )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleRegexTest( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath        : string;
  lPattern         : string;
  lStart           : Integer;
  lEnd             : Integer;
  lCaseInsensitive : Boolean;
  lMultiLine       : Boolean;
  lResult          : TRegexOperationResult;
begin
  lFilePath        := aArgs.GetValue<string>( 'file', '' );
  lPattern         := aArgs.GetValue<string>( 'pattern', '' );
  lStart           := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd             := aArgs.GetValue<Integer>( 'end_line', 0 );
  lCaseInsensitive := aArgs.GetValue<Boolean>( 'case_insensitive', false );
  lMultiLine       := aArgs.GetValue<Boolean>( 'multi_line', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  if lPattern = '' then
    Exit( MakeToolResult( 'ERROR: pattern parameter required', true ) );

  lResult := TRegexOperations.RegexTest( lFilePath, lPattern, lStart, lEnd, lCaseInsensitive, lMultiLine );

  if lResult.Success
    then Result := MakeToolResult( 'Pattern found: ' + IntToStr( lResult.MatchCount ) + ' match(es) in ' + lFilePath )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleMoveLines( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFromFile        : string;
  lToFile          : string;
  lStart           : Integer;
  lEnd             : Integer;
  lInsertAfter     : Integer;
  lInsertBefore    : Integer;
  lDryRun          : Boolean;
  lBackup          : Boolean;
  lResult          : TOperationResult;
begin
  lFromFile     := aArgs.GetValue<string>( 'from_file', '' );
  lToFile       := aArgs.GetValue<string>( 'to_file', '' );
  lStart        := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd          := aArgs.GetValue<Integer>( 'end_line', 0 );
  lInsertAfter  := aArgs.GetValue<Integer>( 'insert_after_line', 0 );
  lInsertBefore := aArgs.GetValue<Integer>( 'insert_before_line', 0 );
  lDryRun       := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup       := aArgs.GetValue<Boolean>( 'backup', false );

  if lFromFile = '' then
    Exit( MakeToolResult( 'ERROR: from_file parameter required', true ) );

  if lToFile = '' then
    Exit( MakeToolResult( 'ERROR: to_file parameter required', true ) );

  if not FileExists( lFromFile ) then
    Exit( MakeToolResult( 'ERROR: Source file not found: ' + lFromFile, true ) );

  lResult := TStringOperations.MoveLines( lFromFile, lToFile, lStart, lEnd, lInsertAfter, lInsertBefore, lDryRun, lBackup );

  if lResult.Success
    then Result := MakeToolResult( 'OK: Moved lines ' + IntToStr( lStart ) + '-' + IntToStr( lEnd ) + ' from ' + lFromFile + ' to ' + lToFile )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleIndentLines( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath : string;
  lStart    : Integer;
  lEnd      : Integer;
  lSpaces   : Integer;
  lDryRun   : Boolean;
  lBackup   : Boolean;
  lResult   : TOperationResult;
begin
  lFilePath := aArgs.GetValue<string>( 'file', '' );
  lStart    := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd      := aArgs.GetValue<Integer>( 'end_line', 0 );
  lSpaces   := aArgs.GetValue<Integer>( 'spaces', 2 );
  lDryRun   := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup   := aArgs.GetValue<Boolean>( 'backup', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  lResult := TStringOperations.IndentLines( lFilePath, lStart, lEnd, lSpaces, lDryRun, lBackup );

  if lResult.Success
    then Result := MakeToolResult( 'OK: Indented lines ' + IntToStr( lStart ) + '-' + IntToStr( lEnd ) + ' by ' + IntToStr( lSpaces ) + ' spaces in ' + lFilePath )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleUnindentLines( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath : string;
  lStart    : Integer;
  lEnd      : Integer;
  lSpaces   : Integer;
  lDryRun   : Boolean;
  lBackup   : Boolean;
  lResult   : TOperationResult;
begin
  lFilePath := aArgs.GetValue<string>( 'file', '' );
  lStart    := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd      := aArgs.GetValue<Integer>( 'end_line', 0 );
  lSpaces   := aArgs.GetValue<Integer>( 'spaces', 2 );
  lDryRun   := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup   := aArgs.GetValue<Boolean>( 'backup', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  lResult := TStringOperations.UnindentLines( lFilePath, lStart, lEnd, lSpaces, lDryRun, lBackup );

  if lResult.Success
    then Result := MakeToolResult( 'OK: Unindented lines ' + IntToStr( lStart ) + '-' + IntToStr( lEnd ) + ' by ' + IntToStr( lSpaces ) + ' spaces in ' + lFilePath )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleConvertEncoding( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath : string;
  lTarget   : string;
  lDryRun   : Boolean;
  lBackup   : Boolean;
  lResult   : TOperationResult;
begin
  lFilePath := aArgs.GetValue<string>( 'file', '' );
  lTarget   := aArgs.GetValue<string>( 'target_encoding', '' );
  lDryRun   := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup   := aArgs.GetValue<Boolean>( 'backup', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  if lTarget = '' then
    Exit( MakeToolResult( 'ERROR: target_encoding parameter required (utf8 or windows-1252)', true ) );

  lResult := TStringOperations.ConvertEncoding( lFilePath, lTarget, lBackup, lDryRun, false );

  if lResult.Success
    then Result := MakeToolResult( 'OK: Converted ' + lFilePath + ' to ' + lTarget )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleRepairUmlauts( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath  : string;
  lReference : string;
  lVCS       : string;
  lRevision  : string;
  lDryRun    : Boolean;
  lBackup    : Boolean;
  lResult    : TRepairResult;
begin
  lFilePath  := aArgs.GetValue<string>( 'file', '' );
  lReference := aArgs.GetValue<string>( 'reference', '' );
  lVCS       := aArgs.GetValue<string>( 'vcs', 'hg' );
  lRevision  := aArgs.GetValue<string>( 'revision', '' );
  lDryRun    := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup    := aArgs.GetValue<Boolean>( 'backup', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  if lReference <> '' then
    lResult := TUmlautRepair.RepairFromReference( lFilePath, lReference, lDryRun, false, lBackup )
  else
    lResult := TUmlautRepair.RepairFromVCS( lFilePath, TUmlautRepair.StringToVCS( lVCS ), '', lRevision, lDryRun, false, lBackup );

  if lResult.Success then
    begin
      if lResult.BytesRepaired > 0
        then Result := MakeToolResult( 'OK: Repaired ' + IntToStr( lResult.BytesRepaired ) + ' umlaut(s) in ' + lFilePath )
        else Result := MakeToolResult( 'OK: No broken umlauts found in ' + lFilePath );
    end
  else
    Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleFileCompare( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFile1   : string;
  lFile2   : string;
  lVerbose : Boolean;
  lResult  : TCompareResult;
  lOutput  : string;
begin
  lFile1   := aArgs.GetValue<string>( 'file1', '' );
  lFile2   := aArgs.GetValue<string>( 'file2', '' );
  lVerbose := aArgs.GetValue<Boolean>( 'verbose', false );

  if lFile1 = '' then
    Exit( MakeToolResult( 'ERROR: file1 parameter required', true ) );

  if lFile2 = '' then
    Exit( MakeToolResult( 'ERROR: file2 parameter required', true ) );

  if not FileExists( lFile1 ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFile1, true ) );

  if not FileExists( lFile2 ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFile2, true ) );

  lResult := TFileCompare.Compare( lFile1, lFile2, lVerbose );
  try
    lOutput := 'Result: ' + lResult.Message + #13#10 +
               'Total checked: ' + IntToStr( lResult.TotalChecked ) + #13#10 +
               'Broken: ' + IntToStr( lResult.BrokenCount ) + #13#10 +
               'Not found: ' + IntToStr( lResult.NotFoundCount );

    if ( lResult.Details <> NIL ) and ( lResult.Details.Count > 0 ) then
      lOutput := lOutput + #13#10 + #13#10 + 'Details:' + #13#10 + lResult.Details.Text;

    Result := MakeToolResult( lOutput, lResult.ExitCode <> ceOK );
  finally
    if lResult.Details <> NIL then
      lResult.Details.Free;
  end;
end;

function TMCPServer.HandleUndo( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath : string;
begin
  lFilePath := aArgs.GetValue<string>( 'file', '' );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  if not FileExists( lFilePath + '.bak' ) then
    Exit( MakeToolResult( 'ERROR: Backup file not found: ' + lFilePath + '.bak', true ) );

  if TUndoHelper.UndoChanges( lFilePath )
    then Result := MakeToolResult( 'OK: Restored ' + lFilePath + ' from backup' )
    else Result := MakeToolResult( 'ERROR: Failed to restore backup for ' + lFilePath, true );
end;

function TMCPServer.HandleCommentLines( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath : string;
  lStart    : Integer;
  lEnd      : Integer;
  lDryRun   : Boolean;
  lBackup   : Boolean;
  lResult   : TOperationResult;
begin
  lFilePath := aArgs.GetValue<string>( 'file', '' );
  lStart    := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd      := aArgs.GetValue<Integer>( 'end_line', 0 );
  lDryRun   := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup   := aArgs.GetValue<Boolean>( 'backup', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  lResult := TStringOperations.CommentLines( lFilePath, lStart, lEnd, lDryRun, lBackup );

  if lResult.Success
    then Result := MakeToolResult( 'OK: Commented lines ' + IntToStr( lStart ) + '-' + IntToStr( lEnd ) + ' (' + IntToStr( lResult.LinesChanged ) + ' lines) in ' + lFilePath )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleUncommentLines( const aArgs : TJSONObject ) : TJSONObject;
Var
  lFilePath : string;
  lStart    : Integer;
  lEnd      : Integer;
  lDryRun   : Boolean;
  lBackup   : Boolean;
  lResult   : TOperationResult;
begin
  lFilePath := aArgs.GetValue<string>( 'file', '' );
  lStart    := aArgs.GetValue<Integer>( 'start_line', 0 );
  lEnd      := aArgs.GetValue<Integer>( 'end_line', 0 );
  lDryRun   := aArgs.GetValue<Boolean>( 'dry_run', false );
  lBackup   := aArgs.GetValue<Boolean>( 'backup', false );

  if lFilePath = '' then
    Exit( MakeToolResult( 'ERROR: file parameter required', true ) );

  if not FileExists( lFilePath ) then
    Exit( MakeToolResult( 'ERROR: File not found: ' + lFilePath, true ) );

  lResult := TStringOperations.UncommentLines( lFilePath, lStart, lEnd, lDryRun, lBackup );

  if lResult.Success
    then Result := MakeToolResult( 'OK: Uncommented lines ' + IntToStr( lStart ) + '-' + IntToStr( lEnd ) + ' (' + IntToStr( lResult.LinesChanged ) + ' lines) in ' + lFilePath )
    else Result := MakeToolResult( 'ERROR: ' + lResult.ErrorMessage, true );
end;

function TMCPServer.HandleRestartServer( const aArgs : TJSONObject ) : TJSONObject;
begin

  fRunning := false;
  Result   := MakeToolResult( 'OK: Server shutting down. Host will restart automatically.' );
end;

end.

