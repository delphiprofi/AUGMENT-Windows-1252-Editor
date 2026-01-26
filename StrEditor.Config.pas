Unit StrEditor.Config;

interface

Uses
  System.SysUtils
, System.Classes
, System.JSON
, System.IOUtils
, System.Generics.Collections
, StrEditor.CommandLine
;

Type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Config File Helper
  /// </summary>
  {$ENDREGION}
  TConfigHelper = class
    private
      {$REGION 'Documentation'}
      /// <summary>
      ///   F3: Prüft Parameter auf literal \r\n (4 Zeichen) und gibt Warnung aus
      /// </summary>
      {$ENDREGION}
      class procedure CheckForLiteralBackslashRN( const aParams : TCommandLineParams );
    public
      {$REGION 'Documentation'}
      /// <summary>
      ///   Lädt Parameter aus einer JSON-Config-Datei
      /// </summary>
      {$ENDREGION}
      class function LoadFromJSON( const aConfigPath : string; Var aParams : TCommandLineParams ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Lädt mehrere Operationen aus einer JSON-Config-Datei
      /// </summary>
      {$ENDREGION}
      class function LoadMultipleOperations( const aConfigPath : string; out aOperations : TArray<TCommandLineParams> ) : Boolean;

      {$REGION 'Documentation'}
      /// <summary>
      ///   Prüft, ob die Config-Datei mehrere Operationen enthält
      /// </summary>
      {$ENDREGION}
      class function IsMultipleOperationsConfig( const aConfigPath : string ) : Boolean;
  end;

implementation

class function TConfigHelper.LoadFromJSON( const aConfigPath : string; Var aParams : TCommandLineParams ) : Boolean;
Var
  lJSON       : TJSONObject;
  lJSONString : string;
  lCommand    : string;
begin
  Result := false;

  if not FileExists( aConfigPath ) then
    begin
      WriteLn( 'Error: Config file not found: ' + aConfigPath );
      Exit;
    end;

  try
    // JSON ist per Definition UTF-8 kodiert - explizit angeben!
    lJSONString := TFile.ReadAllText( aConfigPath, TEncoding.UTF8 );
    lJSON       := TJSONObject.ParseJSONValue( lJSONString ) as TJSONObject;

    if lJSON = NIL then
      begin
        WriteLn( 'Error: Invalid JSON in config file' );
        Exit;
      end;

    try
      aParams.FilePath         := lJSON.GetValue<string>( 'file', '' );
      aParams.FilePattern      := lJSON.GetValue<string>( 'files', '' );
      aParams.OldStr           := lJSON.GetValue<string>( 'old-str', '' );
      aParams.NewStr           := lJSON.GetValue<string>( 'new-str', '' );
      aParams.StartLine        := lJSON.GetValue<Integer>( 'start-line', -1 );
      aParams.EndLine          := lJSON.GetValue<Integer>( 'end-line', -1 );
      aParams.Text             := lJSON.GetValue<string>( 'text', '' );
      aParams.InsertAfterLine  := lJSON.GetValue<Integer>( 'insert-after-line', -1 );
      aParams.InsertBeforeLine := lJSON.GetValue<Integer>( 'insert-before-line', -1 );
      aParams.RegexPattern     := lJSON.GetValue<string>( 'regex-pattern', '' );
      aParams.RegexReplace     := lJSON.GetValue<string>( 'regex-replace', '' );
      aParams.ConditionPattern := lJSON.GetValue<string>( 'condition-pattern', '' );
      aParams.CaseInsensitive  := lJSON.GetValue<Boolean>( 'case-insensitive', false );
      aParams.MultiLine        := lJSON.GetValue<Boolean>( 'multiline', false );
      aParams.ReplaceAll       := lJSON.GetValue<Boolean>( 'replace-all', false );
      aParams.OldStrIsBase64   := lJSON.GetValue<Boolean>( 'old-str-base64-encoded', false );
      aParams.NewStrIsBase64   := lJSON.GetValue<Boolean>( 'new-str-base64-encoded', false );
      aParams.TextIsBase64     := lJSON.GetValue<Boolean>( 'text-base64-encoded', false );
      aParams.Backup           := lJSON.GetValue<Boolean>( 'backup', false );
      aParams.DryRun           := lJSON.GetValue<Boolean>( 'dry-run', false );
      aParams.Diff             := lJSON.GetValue<Boolean>( 'diff', false );
      aParams.Stats            := lJSON.GetValue<Boolean>( 'stats', false );
      aParams.Verbose          := lJSON.GetValue<Boolean>( 'verbose', false );
      aParams.IndentLevel      := lJSON.GetValue<Integer>( 'indent', 0 );
      aParams.LineNumber       := lJSON.GetValue<Integer>( 'line', -1 );
      aParams.LineNumbers      := lJSON.GetValue<string>( 'lines', '' );
      // Indent/Unindent parameters
      aParams.IndentSpaces     := lJSON.GetValue<Integer>( 'spaces', 2 );
      // Move-Lines parameters
      aParams.FromFile         := lJSON.GetValue<string>( 'from-file', '' );
      aParams.ToFile           := lJSON.GetValue<string>( 'to-file', '' );

      lCommand := lJSON.GetValue<string>( 'command', 'str-replace' );

      if SameText( lCommand, 'str-replace' )
        then aParams.Command := ctStrReplace
        else
      if SameText( lCommand, 'insert' ) or SameText( lCommand, 'insert-after' )
        then aParams.Command := ctInsert
        else
      if SameText( lCommand, 'insert-before' )
        then aParams.Command := ctInsertBefore
        else
      if SameText( lCommand, 'regex-replace' )
        then aParams.Command := ctRegexReplace
        else
      if SameText( lCommand, 'regex-test' )
        then aParams.Command := ctRegexTest
        else
      if SameText( lCommand, 'undo' )
        then aParams.Command := ctUndo
        else
      if SameText( lCommand, 'delete-line' )
        then aParams.Command := ctDeleteLine
        else
      if SameText( lCommand, 'delete-lines' )
        then aParams.Command := ctDeleteLines
        else
      if SameText( lCommand, 'replace-line' )
        then aParams.Command := ctReplaceLine
        else
      if SameText( lCommand, 'move-lines' )
        then aParams.Command := ctMoveLines
        else
      if SameText( lCommand, 'indent' ) or SameText( lCommand, 'indent-lines' )
        then aParams.Command := ctIndent
        else
      if SameText( lCommand, 'unindent' ) or SameText( lCommand, 'unindent-lines' )
        then aParams.Command := ctUnindent
        else begin
               WriteLn( 'Error: Invalid command in config file: ' + lCommand );
               Exit;
             end;

      Var lCase := lJSON.GetValue<string>( 'case', '' );

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
                   WriteLn( 'Error: Invalid case conversion in config file: ' + lCase );
                   Exit;
                 end;
        end
      else aParams.CaseConversion := ccNone;

      // F3: Prüfe auf literal \r\n und gib Warnung aus
      CheckForLiteralBackslashRN( aParams );

      Result := true;
    finally
      lJSON.Free;
    end;
  except
    on E : Exception do
      begin
        WriteLn( 'Error: Failed to parse config file: ' + E.Message );
        Result := false;
      end;
  end;
end;

class function TConfigHelper.LoadMultipleOperations( const aConfigPath : string; out aOperations : TArray<TCommandLineParams> ) : Boolean;
Var
  lJSON         : TJSONObject;
  lJSONString   : string;
  lOperations   : TJSONArray;
  lOperation    : TJSONValue;
  lParams       : TCommandLineParams;
  lCommand      : string;
  lCase         : string;
  i             : Integer;
begin
  Result := false;
  SetLength( aOperations, 0 );

  if not FileExists( aConfigPath ) then
    begin
      WriteLn( 'Error: Config file not found: ' + aConfigPath );
      Exit;
    end;

  try
    // JSON ist per Definition UTF-8 kodiert - explizit angeben!
    lJSONString := TFile.ReadAllText( aConfigPath, TEncoding.UTF8 );
    lJSON       := TJSONObject.ParseJSONValue( lJSONString ) as TJSONObject;

    if lJSON = NIL then
      begin
        WriteLn( 'Error: Invalid JSON in config file' );
        Exit;
      end;

    try
      lOperations := lJSON.GetValue<TJSONArray>( 'operations' );

      if lOperations = NIL then
        begin
          WriteLn( 'Error: No "operations" array found in config file' );
          Exit;
        end;

      SetLength( aOperations, lOperations.Count );

      for i := 0 to lOperations.Count - 1 do
        begin
          lOperation := lOperations.Items[ i ];

          if not ( lOperation is TJSONObject ) then
            begin
              WriteLn( 'Error: Operation ' + IntToStr( i + 1 ) + ' is not a JSON object' );
              Exit;
            end;

          lParams.FilePath         := TJSONObject( lOperation ).GetValue<string>( 'file', '' );
          lParams.FilePattern      := TJSONObject( lOperation ).GetValue<string>( 'files', '' );
          lParams.OldStr           := TJSONObject( lOperation ).GetValue<string>( 'old-str', '' );
          lParams.NewStr           := TJSONObject( lOperation ).GetValue<string>( 'new-str', '' );
          lParams.StartLine        := TJSONObject( lOperation ).GetValue<Integer>( 'start-line', -1 );
          lParams.EndLine          := TJSONObject( lOperation ).GetValue<Integer>( 'end-line', -1 );
          lParams.Text             := TJSONObject( lOperation ).GetValue<string>( 'text', '' );

          if lParams.Text = '' then
            lParams.Text := TJSONObject( lOperation ).GetValue<string>( 'with', '' );

          // F1: text-lines Array - Join array elements with CRLF
          Var lTextLinesValue := TJSONObject( lOperation ).FindValue( 'text-lines' );

          if ( lTextLinesValue <> NIL ) and ( lTextLinesValue is TJSONArray ) then
            begin
              Var lTextLines := TJSONArray( lTextLinesValue );
              Var lTextParts : TArray<string>;
              SetLength( lTextParts, lTextLines.Count );

              for Var k := 0 to lTextLines.Count - 1 do
                lTextParts[k] := lTextLines.Items[k].Value;

              lParams.Text := String.Join( #13#10, lTextParts );
            end;

          lParams.InsertAfterLine  := TJSONObject( lOperation ).GetValue<Integer>( 'insert-after-line', -1 );
          lParams.InsertBeforeLine := TJSONObject( lOperation ).GetValue<Integer>( 'insert-before-line', -1 );
          lParams.RegexPattern     := TJSONObject( lOperation ).GetValue<string>( 'regex-pattern', '' );
          lParams.RegexReplace     := TJSONObject( lOperation ).GetValue<string>( 'regex-replace', '' );
          lParams.ConditionPattern := TJSONObject( lOperation ).GetValue<string>( 'condition-pattern', '' );
          lParams.CaseInsensitive  := TJSONObject( lOperation ).GetValue<Boolean>( 'case-insensitive', false );
          lParams.MultiLine        := TJSONObject( lOperation ).GetValue<Boolean>( 'multiline', false );
          lParams.ReplaceAll       := TJSONObject( lOperation ).GetValue<Boolean>( 'replace-all', false );
          lParams.OldStrIsBase64   := TJSONObject( lOperation ).GetValue<Boolean>( 'old-str-base64-encoded', false );
          lParams.NewStrIsBase64   := TJSONObject( lOperation ).GetValue<Boolean>( 'new-str-base64-encoded', false );
          lParams.TextIsBase64     := TJSONObject( lOperation ).GetValue<Boolean>( 'text-base64-encoded', false );
          lParams.Backup           := TJSONObject( lOperation ).GetValue<Boolean>( 'backup', false );
          lParams.DryRun           := TJSONObject( lOperation ).GetValue<Boolean>( 'dry-run', false );
          lParams.Diff             := TJSONObject( lOperation ).GetValue<Boolean>( 'diff', false );
          lParams.Stats            := TJSONObject( lOperation ).GetValue<Boolean>( 'stats', false );
          lParams.Verbose          := TJSONObject( lOperation ).GetValue<Boolean>( 'verbose', false );
          lParams.IndentLevel      := TJSONObject( lOperation ).GetValue<Integer>( 'indent', 0 );
          lParams.LineNumber       := TJSONObject( lOperation ).GetValue<Integer>( 'line', -1 );
          lParams.LineNumbers      := TJSONObject( lOperation ).GetValue<string>( 'lines', '' );
          // Indent/Unindent parameters
          lParams.IndentSpaces     := TJSONObject( lOperation ).GetValue<Integer>( 'spaces', 2 );
          // Move-Lines parameters
          lParams.FromFile         := TJSONObject( lOperation ).GetValue<string>( 'from-file', '' );
          lParams.ToFile           := TJSONObject( lOperation ).GetValue<string>( 'to-file', '' );

          lCommand := TJSONObject( lOperation ).GetValue<string>( 'command', 'str-replace' );

          if SameText( lCommand, 'str-replace' )
            then lParams.Command := ctStrReplace
            else
          if SameText( lCommand, 'insert' ) or SameText( lCommand, 'insert-after' )
            then lParams.Command := ctInsert
            else
          if SameText( lCommand, 'insert-before' )
            then lParams.Command := ctInsertBefore
            else
          if SameText( lCommand, 'regex-replace' )
            then lParams.Command := ctRegexReplace
            else
          if SameText( lCommand, 'regex-test' )
            then lParams.Command := ctRegexTest
            else
          if SameText( lCommand, 'undo' )
            then lParams.Command := ctUndo
            else
          if SameText( lCommand, 'delete-line' )
            then lParams.Command := ctDeleteLine
            else
          if SameText( lCommand, 'delete-lines' )
            then lParams.Command := ctDeleteLines
            else
          if SameText( lCommand, 'replace-line' )
            then lParams.Command := ctReplaceLine
            else
          if SameText( lCommand, 'replace-lines' )
            then lParams.Command := ctReplaceLines
            else
          if SameText( lCommand, 'move-lines' )
            then lParams.Command := ctMoveLines
            else
          if SameText( lCommand, 'indent' ) or SameText( lCommand, 'indent-lines' )
            then lParams.Command := ctIndent
            else
          if SameText( lCommand, 'unindent' ) or SameText( lCommand, 'unindent-lines' )
            then lParams.Command := ctUnindent
            else begin
                   WriteLn( 'Error: Invalid command in operation ' + IntToStr( i + 1 ) + ': ' + lCommand );
                   Exit;
                 end;

          lCase := TJSONObject( lOperation ).GetValue<string>( 'case', '' );

          if lCase <> '' then
            begin
              if SameText( lCase, 'upper' )
                then lParams.CaseConversion := ccUpper
                else
              if SameText( lCase, 'lower' )
                then lParams.CaseConversion := ccLower
                else
              if SameText( lCase, 'title' )
                then lParams.CaseConversion := ccTitle
                else begin
                       WriteLn( 'Error: Invalid case conversion in operation ' + IntToStr( i + 1 ) + ': ' + lCase );
                       Exit;
                     end;
            end
          else lParams.CaseConversion := ccNone;

          // F3: Prüfe auf literal \r\n und gib Warnung aus
          CheckForLiteralBackslashRN( lParams );

          aOperations[ i ] := lParams;
        end;

      Result := true;
    finally
      lJSON.Free;
    end;
  except
    on E : Exception do
      begin
        WriteLn( 'Error: Failed to parse config file: ' + E.Message );
        Result := false;
      end;
  end;
end;

class function TConfigHelper.IsMultipleOperationsConfig( const aConfigPath : string ) : Boolean;
Var
  lJSON       : TJSONObject;
  lJSONString : string;
begin
  Result := false;

  if not FileExists( aConfigPath ) then
    Exit;

  try
    // JSON ist per Definition UTF-8 kodiert - explizit angeben!
    lJSONString := TFile.ReadAllText( aConfigPath, TEncoding.UTF8 );
    lJSON       := TJSONObject.ParseJSONValue( lJSONString ) as TJSONObject;

    if lJSON = NIL then
      Exit;

    try
      Result := lJSON.GetValue( 'operations' ) <> NIL;
    finally
      lJSON.Free;
    end;
  except
    Result := false;
  end;
end;

class procedure TConfigHelper.CheckForLiteralBackslashRN( const aParams : TCommandLineParams );
Const
  cLiteralBackslashRN = '\r\n';  // 4 Zeichen: \, r, \, n
Var
  lWarnings : TArray<string>;
begin
  SetLength( lWarnings, 0 );

  // Prüfe Text-Parameter
  if Pos( cLiteralBackslashRN, aParams.Text ) > 0 then
    begin
      SetLength( lWarnings, Length( lWarnings ) + 1 );
      lWarnings[High( lWarnings )] := 'text';
    end;

  // Prüfe OldStr-Parameter
  if Pos( cLiteralBackslashRN, aParams.OldStr ) > 0 then
    begin
      SetLength( lWarnings, Length( lWarnings ) + 1 );
      lWarnings[High( lWarnings )] := 'old-str';
    end;

  // Prüfe NewStr-Parameter
  if Pos( cLiteralBackslashRN, aParams.NewStr ) > 0 then
    begin
      SetLength( lWarnings, Length( lWarnings ) + 1 );
      lWarnings[High( lWarnings )] := 'new-str';
    end;

  // Ausgabe nach stderr wenn Warnings gefunden
  if Length( lWarnings ) > 0 then
    begin
      WriteLn( ErrOutput, 'WARNING: Detected literal \r\n (4 characters) in parameter: ' + String.Join( ', ', lWarnings ) );
      WriteLn( ErrOutput, '  This is probably NOT what you want!' );
      WriteLn( ErrOutput, '  For real line breaks, use JSON with "text-lines" array or base64 encoding.' );
      WriteLn( ErrOutput, '  Example: "text-lines": ["Line 1", "Line 2"]' );
    end;
end;

end.

