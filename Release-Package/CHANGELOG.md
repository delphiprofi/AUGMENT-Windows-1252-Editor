# Changelog

All notable changes to StrEditor will be documented in this file.

---

## [1.9.3] - 2026-02-27

### Fixed
- **WriteFileWithRetry: Retry-Mechanismus war komplett wirkungslos**
  - `WriteFile` fing alle Exceptions intern ab und gab nur `false` zurück → keine Exception kam je bei `WriteFileWithRetry` an
  - `WriteFileWithRetry` hatte nach `Result := WriteFile(...)` ein unbedingtes `Exit` → Retry-Schleife wurde nie erreicht, auch wenn `Result = false`
  - Fix: `WriteFile` propagiert Exceptions jetzt nach oben (`try/except` → `try/finally`)
  - Fix: `WriteFileWithRetry` verlässt die Schleife nur noch bei `Result = true`
  - Retry funktioniert jetzt korrekt: 3 Versuche mit 100ms Pause bei transienten Dateisperren

- **replace-line mit mehrzeiligem Text korrupte Datei** (Workaround)
  - `replace-line` + `text-lines` mit mehreren Einträgen bettete CRLF in eine TStringList-Zeile ein
  - Beim Schreiben entstanden dadurch mehr Zeilen als erwartet → nachfolgende Operationen arbeiteten mit falschen Zeilennummern
  - Fix: Text wird automatisch aufgeteilt und als mehrere Zeilen korrekt eingefügt
  - Warning auf stderr: `please use replace-lines with start-line/end-line/text-lines`

### Added
- **Alias `"line"` für `"insert-after-line"`** in JSON-Config (mit Warning auf stderr)

---

## [1.9.2] - 2026-02-27

### Fixed
- **ChangeReport: "Lines: X -> 0 (-X)" Ausgabe korrigiert**
  - `fFinalLineCount` wurde nach der Operation nie gesetzt und blieb immer 0
  - `GenerateReport` liest jetzt die Datei nach der Operation neu ein um die korrekte Zeilenzahl zu ermitteln
  - Bedingung von `or` auf `and` geändert: Lines-Zeile wird nur ausgegeben wenn beide Werte bekannt sind

---

## [1.9.1] - 2026-02-26

### Fixed
- **Retry-Mechanismus für File-Access-Errors** (E/A-Fehler 232, EFOpenError, EFCreateError)
  - Neue Funktionen: `ReadFileWithRetry()` und `WriteFileWithRetry()` in `StrEditor.Encoding.pas`
  - 3 Versuche mit 100ms Delay bei File-Access-Errors
  - Detaillierte Fehlermeldungen mit Dateiname und Retry-Info
  - Alle 26 File-Access-Aufrufe in `StrEditor.Operations.pas` verwenden jetzt Retry-Mechanismus
  - Verhindert Race-Conditions und Handle-Leaks bei parallelen Augment-Aufrufen

### Technical Details
- Exception-Handling für `EInOutError` (E/A-Fehler 232 = "The pipe is being closed")
- Exception-Handling für `EFOpenError` und `EFCreateError`
- Andere Exceptions werden nicht wiederholt (z.B. JSON-Parse-Errors)
- Fehlerausgabe auf `stderr` mit "after 3 retries" Info
- Basierend auf Log-Analyse: E/A-Fehler 232 war häufigster Fehler (24x) seit 2026-02-12

---

## [1.9.0] - 2026-02-15

### Added
- **Tolerant JSON-Config Validation** (Postel's Law: "Be liberal in what you accept")
  - Accept `"action"` and `"operation"` as alias for `"command"` with stderr warning
  - Accept `"replace"` as alias for `"str-replace"` command with warning
  - Accept JSON array format `[{...}, {...}]` instead of `{"operations": [...]}` with warning
  - Accept top-level `"file"` as fallback for operations that don't specify their own `"file"` with warning
  - Accept `"old"`/`"new"` as alias for `"old-str"`/`"new-str"` with warning
  - Accept `"search"`/`"replace"` as alias for `"old-str"`/`"new-str"` with warning
  - Accept `"line-number"` as alias for `"insert-after-line"` with warning
  - Warn when file paths contain control characters (from unescaped JSON backslashes)

### Technical Details
- All tolerance warnings go to `stderr` (via `WriteLn(ErrOutput, ...)`) so they don't interfere with normal stdout output
- If a recognized alias is found, the operation executes successfully with a warning instead of failing
- This reduces the JSON-Config error rate from ~48% to near zero for common AI-generated config patterns
- Based on analysis of 55 failed JSON-Config operations from production log data

---

## [1.8.7] - 2026-02-09

### Added
- **--filecompare**: Compare file against master copy for broken special characters
  - Detects encoding mismatch between files (exit code 1)
  - Identifies broken/corrupted special characters (exit code 2)
  - Reports missing lines that need manual review (exit code 3)
  - Fragment-based substring matching: splits context at special character positions
  - Robust detection even when surrounding context also contains broken characters
  - Special characters checked: öäüÖÄÜßé§•
  - Usage: `StrEditor.exe --file "test.pas" --filecompare "master.pas" --verbose`
  - New help category: `--help compare`

### Technical Details
- New unit `StrEditor.FileCompare.pas` with `TFileCompare` class
- `TCompareExitCode` enum: ceOK(0), ceEncodingMismatch(1), ceCharsBroken(2), ceLineNotFound(3)
- Context split into fragments at special char positions for reliable matching
- 20-character context window before and after each special character

---

## [1.8.6] - 2026-02-05

### Added
- **--range Parameter for --show**: Show specific line ranges with `--range <start>,<end>`
  - Example: `StrEditor.exe --file "test.pas" --show --range 10,20 --line-numbers`
  - Validates format (must be `<start>,<end>`)
  - Validates values (both must be > 0, end >= start)
  - Works with all other --show options (--line-numbers, --verbose, etc.)

### Changed
- **Deprecation Warning for --keep-config**: Parameter now shows prominent warning
  - Displays warning box on stderr when `--keep-config` is used
  - Explains that JSON config files are temporary operation instructions
  - Warns that parameter will be removed in future version
  - Reason: JSON configs should be auto-deleted after successful execution

### Documentation
- Updated README.md and README.de.md with new features
- Distributed updated str-replace-editor.md to 12 project locations

---

## [1.8.5] - 2026-02-01

### Fixed
- **--dry-run Bug with JSON Config**: The `--dry-run` parameter now works correctly when used with `--config`
  - Previously, `--dry-run` was ignored when loading JSON config files
  - Root cause: `LoadFromJSON` was overwriting command-line flags with JSON defaults (false)
  - Fix: Command-line flags are now preserved and have priority over JSON defaults
  - Affected flags: `--dry-run`, `--backup`, `--verbose`, `--diff`, `--stats`

### Technical Details
- Modified `StrEditor.Config.pas`: Added command-line flag preservation in `LoadFromJSON`
- Modified `StrEditor.dpr`: Added flag transfer for multiple operations configs

### Tests
- 191 total tests passing

---

## [1.8.4] - 2026-01-30

### Added
- **ChangeReport**: Shows what was changed after each operation
  - Displays old and new content with line numbers
  - Configurable via `StrEditor.ini`
  - Enable/disable with `[ChangeReport] Enabled=true`

- **ContextLines**: Shows context lines before/after changes in ChangeReport
  - `ContextLinesBefore=2` - Lines before the change
  - `ContextLinesAfter=2` - Lines after the change
  - Makes it easier to understand where changes occurred

- **SessionLog**: Logs all operations for error analysis
  - Logs VIEW requests, JSON configs, errors, and successes
  - Base64-encoded to avoid CRLF issues
  - Format: `Timestamp|Type|Base64-Data`
  - Enable with `[SessionLog] Enabled=true` and `LogPath=StrEditor.log`

- **INI-Config**: New `StrEditor.ini` configuration file
  - Located next to StrEditor.exe
  - Supports `true/false` and `1/0` for boolean values

### Fixed
- **TIniFile.ReadBool Bug**: Delphi's `TIniFile.ReadBool` only accepts `0/1`, not `true/false`
  - Implemented `ReadIniBool()` helper that accepts both formats

### Configuration Example
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

### Tests
- 191 total tests passing

---

## [1.8.3] - 2026-01-29

### Changed (BREAKING)
- **Auto-Delete JSON Config**: JSON config files are now **automatically deleted** after successful execution
  - Previous behavior: Config files were kept unless `--delete-config-on-success` was specified
  - New behavior: Config files are deleted by default, use `--keep-config` to preserve them
  - Rationale: AI agents often forgot to clean up config files, leading to workspace clutter

### Added
- **`--keep-config` Parameter**: Preserves JSON config file after successful execution
  - Use for debugging or when you need to re-run the same config
  - Example: `StrEditor.exe --config "ops.json" --keep-config`

### Removed
- **`--delete-config-on-success` Parameter**: No longer needed (now default behavior)

### Documentation
- **AGENT-RULES-STREDITOR.md**: New strict rules file for AI agents
  - Prohibits sequential operations on same file
  - Requires JSON-Config for multiple operations
  - Requires `text-lines` array for multi-line text (no Base64)
  - Copy-paste templates for common operations
- **str-replace-editor.md**: Added critical warning section at top

### Tests
- 183 total tests passing (+1 new test for `--keep-config`)

---

## [1.8.2] - 2026-01-26

### Added
- **Indent Lines (`--indent-lines`)**: Add spaces at the beginning of lines
  - Parameters: `--file`, `--start-line`, `--end-line`, `--spaces` (default: 2)
  - Supports: `--backup`, `--dry-run`, `--diff`, `--verbose`
  - Empty lines are skipped (no spaces added to empty lines)
  - JSON config: `"command": "indent"` or `"command": "indent-lines"`

- **Unindent Lines (`--unindent-lines`)**: Remove spaces from the beginning of lines
  - Parameters: `--file`, `--start-line`, `--end-line`, `--spaces` (default: 2)
  - Supports: `--backup`, `--dry-run`, `--diff`, `--verbose`
  - Partial unindent: If line has fewer spaces than requested, removes only available spaces
  - JSON config: `"command": "unindent"` or `"command": "unindent-lines"`

- **BatchProcessor Support**: Indent/Unindent work in batch mode with original line numbers

### Fixed
- **Build-StrEditor-Release.bat**: Now actually copies to `C:\Delphi XE16\bin\` (was only announced but not executed)

### Tests
- 13 new unit tests for indent/unindent features
- 182 total tests passing

---

## [1.8.1] - 2026-01-26

### Added
- **Hex-Dump Output (`--hex`)**: Display file content as hex dump
  - Format: `OFFSET: HH HH HH HH HH HH HH HH  HH HH HH HH HH HH HH HH  ASCII`
  - 16 bytes per line with space after 8th byte for readability
  - ASCII representation shows printable characters (32-126), others as `.`
  - Use case: Encoding debugging (e.g., `F6` = Windows-1252 ö, `C3 B6` = UTF-8 ö)

- **Base64 Output (`--base64`)**: Display file content as Base64 string
  - Use case: Transport file content for tests or copy/paste
  - Use case: Embed binary data in JSON configs

- **Byte-based Head/Tail**: In hex and base64 modes, `--head` and `--tail` operate on **byte offsets**, not line numbers
  - Example: `--show --hex --head 64` shows first 64 bytes
  - Example: `--show --base64 --tail 32` shows last 32 bytes as Base64

### Fixed
- **TrailingLineBreak Preservation**: Files without trailing CRLF are now correctly preserved
  - Previously, `TStringList.Text` always added CRLF at end
  - Now detects original state and preserves it

- **Original Line Numbers in BatchProcessor**: All line numbers in JSON configs now refer to the **original file state**
  - BatchProcessor internally tracks offsets and adjusts line numbers
  - Makes complex refactoring configs much easier to create

- **UTF-8 JSON Config Reading**: JSON configs are now explicitly read as UTF-8
  - Fixes issues with umlauts in JSON config files

### Tests
- 10 new unit tests for hex/base64 features
- 169 total tests passing

---

## [1.8.0] - 2026-01-23

### Added
- **`text-lines` Array**: Multi-line text in JSON without escaping issues
  - Use `"text-lines": ["Line 1", "Line 2"]` instead of `"text": "Line1\r\nLine2"`
  - Lines are automatically joined with CRLF
  - Recommended for all multi-line insertions

- **`replace-lines` Command**: Replace line ranges in JSON config
  - New command type for atomic line range replacement

- **Parameter Aliases**: Shorter parameter names for common operations
  - `--ib` = `--insert-before-line`
  - `--ia` = `--insert-after-line`
  - `--dl` = `--delete-line`
  - `--rl` = `--replace-line`
  - `--ob64` = `--old-str-base64`
  - `--nb64` = `--new-str-base64`

- **Categorized Help**: Help system organized by categories
  - `--help` shows compact overview with categories
  - `--help <category>` shows focused help (replace, insert, delete, show, encoding, config, move, repair, all)

- **Warning for `\r\n` Literal**: Detects when user accidentally uses literal `\r\n` (4 characters) instead of real CRLF
  - Shows warning on stderr with recommendation to use `text-lines` or base64

### Documentation
- New Agent Cookbook: `DOC/AGENT-COOKBOOK.md` with practical recipes
- Removed duplicate `DOC/AUGMENT-RULES.md`
- Updated `.augment/rules/str-replace-editor.md` with decision checklist

### Tests
- 23 new unit tests for v1.8.0 features

---

## [1.7.7] - 2026-01-20

### Fixed
- **Bugfix: Dry-Run Mode respected**: `--delete-config-on-success` now correctly respects `--dry-run` mode
  - Config file is **NOT deleted** when using `--dry-run` (no actual changes made)
  - Clear message: "Dry-run mode: Config file NOT deleted"

---

## [1.7.6] - 2026-01-20

### Added
- **Delete Config on Success**: New parameter `--delete-config-on-success`
  - Automatically deletes JSON config file after successful execution
  - Only deletes if all operations completed successfully (ExitCode = 0)
  - Useful for automated scripts and CI/CD pipelines
  - Works with both single-operation and multi-operation configs

### Examples
```bash
# Execute config and delete it on success
StrEditor.exe --config operations.json --delete-config-on-success

# With verbose output
StrEditor.exe --config operations.json --delete-config-on-success --verbose

# Dry-run does NOT delete config (no actual changes made)
StrEditor.exe --config operations.json --delete-config-on-success --dry-run
```

### Tests
- 3 new unit tests for delete-config-on-success functionality

---

## [1.7.5] - 2026-01-17

### Added
- **Move Lines Feature**: Move source code lines from one file to another
  - `--move-lines`: Move lines between files while preserving encoding
  - `--from <file>`: Source file to move lines from
  - `--to <file>`: Target file to move lines to
  - `--start-line <n>`: First line to move (1-based)
  - `--end-line <n>`: Last line to move (1-based)
  - `--insert-after-line <n>`: Insert moved lines after this line in target
  - `--insert-before-line <n>`: Insert moved lines before this line in target
  - **Encoding Preservation**: Both source and target files keep their original encoding
  - **Backup Support**: Use `--backup` to create .bak files for both files
  - **Safe Mode**: Use `--dry-run` to preview changes without modifying files
  - **JSON-Config Support**: Can be used in batch operations via JSON config

### Implementation
- Added `MoveLines()` method in `StrEditor.Operations.pas`
- Extended `StrEditor.CommandLine.pas` with `ctMoveLines` command type
- Added `FromFile` and `ToFile` parameters to `TCommandLineParams`
- Extended `StrEditor.Config.pas` for JSON-Config support
- Integrated move-lines feature into main program

### Tests
- 8 new unit tests for move-lines functionality
- All move-lines tests passing (DUnitX)

### Examples
```bash
# Move lines 50-100 from UnitA.pas to UnitB.pas after line 200
StrEditor.exe --move-lines --from "UnitA.pas" --to "UnitB.pas" --start-line 50 --end-line 100 --insert-after-line 200

# Move lines with backup
StrEditor.exe --move-lines --from "UnitA.pas" --to "UnitB.pas" --start-line 10 --end-line 20 --insert-before-line 50 --backup

# Preview changes (dry-run)
StrEditor.exe --move-lines --from "UnitA.pas" --to "UnitB.pas" --start-line 10 --end-line 20 --insert-after-line 100 --dry-run
```

### JSON-Config Example
```json
{
  "operations": [
    {
      "command": "move-lines",
      "from-file": "UnitA.pas",
      "start-line": 50,
      "end-line": 100,
      "to-file": "UnitB.pas",
      "insert-after-line": 200
    }
  ]
}
```

---

## [1.7.4] - 2026-01-11

### Added
- **Repair Umlauts Feature**: Automatically repair broken umlauts in Delphi source files
  - `--repair-umlauts`: Scan and repair broken umlaut sequences (Ã¤ → ä, Ã¶ → ö, etc.)
  - `--reference <file>`: Use a reference file instead of VCS to determine original encoding
  - **VCS Integration**: Automatically uses Mercurial (hg) or Git to get the original file content
  - **Smart Detection**: Detects broken UTF-8 bytes in Windows-1252 files
  - **Detailed Output**: With `--verbose` shows all detected and repaired umlauts
  - **Safe Mode**: Use `--dry-run` to preview changes without modifying files

### Implementation
- Created umlaut repair logic in `StrEditor.Repair.pas`
- Added `TUmlautRepair.RepairUmlauts()` method for scanning and repairing
- Added `TUmlautRepair.GetOriginalFromVCS()` for VCS integration (hg/git)
- Added `TUmlautRepair.GetOriginalFromReference()` for reference file support
- Extended `StrEditor.CommandLine.pas` with `ctRepairUmlauts` command type
- Integrated repair feature into main program

### Tests
- All tests passing (DUnitX)
- Manual testing with broken umlaut files

### Examples
```bash
# Repair umlauts using VCS (Mercurial/Git)
StrEditor.exe --file "broken.pas" --repair-umlauts --backup --verbose

# Preview changes without modifying (dry-run)
StrEditor.exe --file "broken.pas" --repair-umlauts --dry-run --verbose

# Use reference file instead of VCS
StrEditor.exe --file "broken.pas" --repair-umlauts --reference "original.pas" --verbose
```

---

## [1.7.3] - 2025-11-17

### Added
- **Documentation Viewer**: View documentation files with `--docs [<file>]` command
- **Browser Integration**: Open documentation in default browser with `--open-in-browser` flag
- **Documentation Listing**: List all available documentation files with `--docs --list`
- **Head/Tail Support**: Show first/last N lines of documentation with `--head <n>` / `--tail <n>`
- **Line Numbers**: Display line numbers in documentation with `--line-numbers`

### Implementation
- Created `StrEditor.Documentation.pas` module with documentation display functionality
- Added `ShowDocs()` method for displaying documentation with filtering options
- Added `OpenInBrowser()` method using `ShellExecute()` to open files in default browser
- Added `GetDocumentationPath()` for resolving documentation files in exe directory
- Added `ListDocumentationFiles()` to enumerate all .md files in exe directory
- Extended `StrEditor.CommandLine.pas` with `ctDocs` command type and parameter parsing
- Fixed `GetParamValue()` to ignore flag parameters (starting with `-`)
- Integrated documentation feature into main program

### Tests
- 112/112 tests passing (DUnitX)
- Added 9 new tests for documentation feature (7 for display, 2 for browser open)

### Examples
```bash
# View default documentation (README.md)
StrEditor.exe --docs

# View specific documentation file
StrEditor.exe --docs INTEGRATION.md

# Open documentation in browser
StrEditor.exe --docs --open-in-browser

# Show first 20 lines with line numbers
StrEditor.exe --docs --head 20 --line-numbers

# List all available documentation files
StrEditor.exe --docs --list
```

---

## [1.7.2] - 2025-11-12

### Added
- **Insert Before Line Feature**: Insert text before a specific line with `--insert-before-line <n>`
- **JSON Config Support**: `insert-before` command in JSON config files
- **1-based Line Numbering**: User-friendly line numbering (line 1 = first line)

### Implementation
- Added `TStringOperations.InsertBefore()` method in `StrEditor.Operations.pas`
- Added `InsertTextBefore()` helper function with 1-based line validation
- Extended `StrEditor.CommandLine.pas` with `ctInsertBefore` command type
- Extended `StrEditor.Config.pas` to parse `insert-before-line` from JSON
- Added `ctInsertBefore` case handler in main program

### Tests
- 103/103 tests passing (DUnitX)
- Added 3 new tests for `--insert-before-line` feature

---

## [1.7.1] - 2025-11-11

### Added
- **JSON Config Support for Line Manipulation**: Load line operations from JSON config files
- **Batch Mode for Line Operations**: Multiple line operations are automatically sorted and executed in optimal order
- **Automatic Sorting**: Operations are executed from highest to lowest line number to prevent index shifting
- **Mixed Operations Support**: Combine delete-line, delete-lines, and replace-line in one JSON config

### Changed
- Extended `StrEditor.Config.pas` to support new line manipulation commands in JSON
- Created `StrEditor.BatchProcessor.pas` for automatic batch processing and sorting
- Integrated batch processor into main program for automatic detection and execution
- Updated documentation (DOC/INTEGRATION.md, DOC/AUGMENT-RULES.md) with JSON config examples

### Tests
- 100/100 tests passing (DUnitX)
- Added 8 new tests for JSON config and batch processor

---

## [1.7.0] - 2025-11-11

### Added
- **Delete Line Feature**: Delete a single line with `--delete-line <n>`
- **Delete Lines Feature**: Delete multiple lines with `--delete-lines <n,m,k>` or `--start-line/--end-line`
- **Replace Line Feature**: Replace a complete line with `--replace-line <n> --with <text>`
- **Base64 Support for Replace Line**: Use `--with-base64` for special characters
- **Direct Line Manipulation**: Simpler and more efficient than string replacement

### Changed
- Added `TStringOperations.DeleteLine` method
- Added `TStringOperations.DeleteLines` method (two overloads: range and comma-separated)
- Added `TStringOperations.ReplaceLine` method with Base64 support
- Updated command-line parser to support new line manipulation commands
- Updated help text with new commands and examples

### Tests
- 92/92 tests passing (DUnitX)
- Added 16 new tests for line manipulation features

---

## [1.6.0] - 2025-11-11

### Added
- **Multi-Line String Replace**: Replace strings spanning multiple lines with `--multi-line` flag
- **TStringList Optimization**: Better performance for large files
- **Documentation**: Added DOC/INTEGRATION.md and DOC/AUGMENT-RULES.md
- **English README**: Professional English README.md for GitHub

### Changed
- Improved string replacement algorithm for multi-line strings
- Updated command-line parser to support multi-line mode

### Tests
- 76/76 tests passing (DUnitX)
- Added tests for multi-line string replacement

---

## [1.5.0] - 2025-11-10

### Added
- **Base64-Parameter Feature**: Solve PowerShell special character problems
  - `--old-str-base64`: Base64-encoded old string
  - `--new-str-base64`: Base64-encoded new string
  - `--text-base64`: Base64-encoded text to insert
- **Automatic Encoding/Decoding**: Parameters are automatically decoded

### Tests
- 72/72 tests passing (DUnitX)
- Added tests for Base64 parameter encoding/decoding

---

## [1.4.0] - 2025-11-09

### Added
- **Reinterpret Encoding Feature**: Fix files with wrong encoding interpretation
  - `--reinterpret-as`: Reinterpret file as Windows-1252 or UTF-8
- **Repair Broken Characters**: Repair broken umlauts and special characters

### Tests
- 68/68 tests passing (DUnitX)
- Added tests for encoding reinterpretation

---

## [1.3.0] - 2025-11-08

### Added
- **Convert Encoding Feature**: Convert between Windows-1252 and UTF-8
  - `--convert-encoding`: Convert file encoding
- **Preserve Line Endings**: Line endings are preserved during conversion

### Tests
- 58/58 tests passing (DUnitX)
- Added tests for encoding conversion

---

## [1.2.0] - 2025-11-07

### Added
- **Show/Cat Command**: Display file content with encoding awareness
  - `--show`: Display file content
  - `--cat`: Alias for `--show`
- **Line Number Display**: Show line numbers with `--show-line-numbers`
- **Syntax Highlighting**: Basic syntax highlighting support with `--syntax`

### Tests
- 54/54 tests passing (DUnitX)
- Added tests for show/cat command

---

## [1.1.0] - 2025-11-06

### Added
- **Batch Processing**: Process multiple files at once with `--batch`
- **Diff Output**: Show differences before/after with `--diff`
- **Statistics**: Show replacement statistics with `--stats`
- **Case Conversion**: Convert case (upper/lower/title)
  - `--case-upper`: Convert to uppercase
  - `--case-lower`: Convert to lowercase
  - `--case-title`: Convert to title case
- **Indent/Outdent**: Indent or outdent lines
  - `--indent`: Indent lines
  - `--outdent`: Outdent lines
- **Undo Support**: Restore previous version with `--undo`
- **Config File Support**: Load operations from JSON files with `--config`
- **Multiple Operations**: Chain multiple operations with `--multi-op`
- **Conditional Replacements**: Replace only in lines matching a pattern with `--if-line-contains`

### Tests
- 48/48 tests passing (DUnitX)
- Added tests for all new features

---

## [1.0.0] - 2025-11-05

### Added
- **Encoding Detection & Preservation**: Automatically detects and preserves Windows-1252 and UTF-8 encoding
- **String Replace**: Replace exact strings with optional line range support
  - `--file`: File to edit
  - `--old-str`: String to replace
  - `--new-str`: Replacement string
  - `--start-line`: Start line for replacement (optional)
  - `--end-line`: End line for replacement (optional)
- **Insert Text**: Insert text after a specific line
  - `--text`: Text to insert
  - `--insert-after-line`: Line number after which to insert
- **Regex Replace**: Pattern-based replacement with capture groups
  - `--regex-pattern`: Regex pattern
  - `--regex-replace`: Replacement string with capture groups (`$1`, `$2`, etc.)
  - `-i`, `--case-insensitive`: Case-insensitive matching
  - `-m`, `--multiline`: Multi-line matching
- **Macro Expansion**: Variables like `{{LINE_NUMBER}}`, `{{FILE_NAME}}`, `{{DATE}}`, `{{TIME}}`
- **Dry-Run Mode**: Test changes without modifying files with `--dry-run`
- **Backup Support**: Create backups before editing with `--backup`
- **Verbose Output**: Detailed output for debugging with `--verbose`
- **Exit Codes**: Clear exit codes for automation
  - 0: Success
  - 1: String not found
  - 2: Error (e.g., file not found)
  - 10: Invalid parameter combination

### Tests
- 38/38 tests passing (DUnitX)
- Tests for encoding detection, string replace, insert, regex, and macros

---

## Links

- **GitHub Repository**: https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor
- **Documentation**: [DOC/INTEGRATION.md](DOC/INTEGRATION.md)
- **Augment Rules**: [DOC/AUGMENT-RULES.md](DOC/AUGMENT-RULES.md)

