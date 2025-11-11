# Changelog

All notable changes to StrEditor will be documented in this file.

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

