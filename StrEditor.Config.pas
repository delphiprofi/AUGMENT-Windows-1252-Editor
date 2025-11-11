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
    lJSONString := TFile.ReadAllText( aConfigPath );
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

      lCommand := lJSON.GetValue<string>( 'command', 'str-replace' );

      if SameText( lCommand, 'str-replace' )
        then aParams.Command := ctStrReplace
        else
      if SameText( lCommand, 'insert' )
        then aParams.Command := ctInsert
        else
      if SameText( lCommand, 'regex-replace' )
        then aParams.Command := ctRegexReplace
        else
      if SameText( lCommand, 'regex-test' )
        then aParams.Command := ctRegexTest
        else
      if SameText( lCommand, 'undo' )
        then aParams.Command := ctUndo
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
    lJSONString := TFile.ReadAllText( aConfigPath );
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
          lParams.InsertAfterLine  := TJSONObject( lOperation ).GetValue<Integer>( 'insert-after-line', -1 );
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

          lCommand := TJSONObject( lOperation ).GetValue<string>( 'command', 'str-replace' );

          if SameText( lCommand, 'str-replace' )
            then lParams.Command := ctStrReplace
            else
          if SameText( lCommand, 'insert' )
            then lParams.Command := ctInsert
            else
          if SameText( lCommand, 'regex-replace' )
            then lParams.Command := ctRegexReplace
            else
          if SameText( lCommand, 'regex-test' )
            then lParams.Command := ctRegexTest
            else
          if SameText( lCommand, 'undo' )
            then lParams.Command := ctUndo
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
    lJSONString := TFile.ReadAllText( aConfigPath );
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

end.

