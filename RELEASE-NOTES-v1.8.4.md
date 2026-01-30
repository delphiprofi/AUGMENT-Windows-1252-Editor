# StrEditor v1.8.4

## New Features

### ChangeReport - Shows What Was Changed
After each operation, StrEditor now displays what was changed with context lines:

```
DELETE line 7:        
       5: procedure Test1;       <- Context before
       6: procedure Test2;       <- Context before  
  -    7: procedure Test3;       <- Deleted line
       8: procedure Test4;       <- Context after
       9: procedure Test5;       <- Context after
```

### SessionLog - Logs All Operations
For analyzing AI agent misuse patterns:
- Logs VIEW requests, JSON configs, errors, and successes
- Base64-encoded to avoid CRLF issues
- Format: `Timestamp|Type|Base64-Data`

Example log entries:
```
2026-01-30 17:14:55|VIEW|QzpcVGVtcFxTZXNzaW9uTG9nVGVzdC5wYXN8MS01
2026-01-30 17:15:08|CONFIG|eyJvcGVyYXRpb25zIjogWy4uLl19
2026-01-30 17:15:08|ERROR|U3RyaW5nIG5vdCBmb3VuZA==
```

### INI-Config - Configuration via StrEditor.ini
New configuration file `StrEditor.ini` (placed next to StrEditor.exe):

```ini
[ChangeReport]
Enabled=true
ShowContent=true
ContextLinesBefore=2
ContextLinesAfter=2

[SessionLog]
Enabled=true
LogPath=StrEditor.log
```

## Bug Fixes

### TIniFile.ReadBool Bug
Delphi's `TIniFile.ReadBool` only accepts `0` or `1`, not `true` or `false` strings.
Implemented `ReadIniBool()` helper function that accepts both formats.

## Important Notes

⚠️ **INI File Encoding**: The INI file must be created **without UTF-8 BOM**! 
Delphi's `TIniFile` cannot correctly read UTF-8 BOM files. Use ANSI/Windows-1252 or UTF-8 without BOM.

## New Files
- `StrEditor.ChangeReport.pas` - Change reporting functionality
- `StrEditor.SessionLog.pas` - Session logging with Base64 encoding
- `StrEditor.Settings.pas` - INI configuration singleton
- `Tests/TestStrEditor.ChangeReport.pas` - Unit tests for ChangeReport

## Tests
- 8 new unit tests for ChangeReport feature
- **191 total tests passing**

## Downloads
- `StrEditor-v1.8.4-Windows-x86.zip` - Windows 32-bit executable with documentation

