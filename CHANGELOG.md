# Changelog

All notable changes to StrEditor will be documented in this file.

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

