# StrEditor - Integration Guide

**Version:** 1.6.0  
**Last Updated:** 2025-11-11

---

## Overview

StrEditor is a command-line tool for editing Delphi source files while preserving encoding (Windows-1252 and UTF-8). It is designed to replace the internal `str-replace-editor` tool used by Augment/VSCode.

---

## Features

### Core Features (v1.0)
- **Encoding Detection & Preservation**: Automatically detects and preserves Windows-1252 and UTF-8 encoding
- **String Replace**: Replace exact strings with optional line range support
- **Insert Text**: Insert text after a specific line
- **Regex Replace**: Pattern-based replacement with capture groups (`$1`, `$2`, etc.)
- **Macro Expansion**: Variables like `{{LINE_NUMBER}}`, `{{FILE_NAME}}`, `{{DATE}}`, `{{TIME}}`
- **Dry-Run Mode**: Test changes without modifying files (`--dry-run`)
- **Backup Support**: Create backups before editing (`--backup`)
- **Verbose Output**: Detailed output for debugging (`--verbose`)

### Enhanced Features (v1.1)
- **Batch Processing**: Process multiple files at once (`--batch`)
- **Diff Output**: Show differences before/after (`--diff`)
- **Statistics**: Show replacement statistics (`--stats`)
- **Case Conversion**: Convert case (upper/lower/title) (`--case-upper`, `--case-lower`, `--case-title`)
- **Indent/Outdent**: Indent or outdent lines (`--indent`, `--outdent`)
- **Undo Support**: Restore previous version (`--undo`)
- **Config File Support**: Load operations from JSON files (`--config`)
- **Multiple Operations**: Chain multiple operations (`--multi-op`)
- **Conditional Replacements**: Replace only in lines matching a pattern (`--if-line-contains`)

### Show/Cat Feature (v1.2)
- **Show/Cat Command**: Display file content with encoding awareness (`--show`, `--cat`)
- **Line Number Display**: Show line numbers (`--show-line-numbers`)
- **Syntax Highlighting**: Basic syntax highlighting support (`--syntax`)

### Convert Encoding Feature (v1.3)
- **Convert Encoding**: Convert between Windows-1252 and UTF-8 (`--convert-encoding`)
- **Preserve Line Endings**: Line endings are preserved during conversion

### Reinterpret Encoding Feature (v1.4)
- **Reinterpret Encoding**: Fix files with wrong encoding interpretation (`--reinterpret-as`)
- **Repair Broken Characters**: Repair broken umlauts and special characters

### Base64-Parameter Feature (v1.5)
- **Base64-Parameter**: Solve PowerShell special character problems
  - `--old-str-base64`: Base64-encoded old string
  - `--new-str-base64`: Base64-encoded new string
  - `--text-base64`: Base64-encoded text to insert
- **Automatic Encoding/Decoding**: Parameters are automatically decoded

### Multi-Line String Replace Feature (v1.6)
- **Multi-Line String Replace**: Replace strings spanning multiple lines (`--multi-line`)
- **TStringList Optimization**: Better performance for large files

---

## Installation

### Option 1: Download Pre-built Binary
Download the latest release from the [Releases](https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/releases) page.

### Option 2: Build from Source
1. Clone the repository:
   ```bash
   git clone https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor.git
   ```

2. Open `StrEditor.dproj` in Delphi (tested with Delphi 13/XE16)

3. Build the project (Release configuration recommended)

4. The executable will be in `Win32\Release\StrEditor.exe`

---

## Basic Usage

### Show Help
```bash
StrEditor.exe --help
```

### Show Version
```bash
StrEditor.exe --version
```

### Simple String Replace
```bash
StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL"
```

### String Replace with Line Range
```bash
StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL" --start-line 10 --end-line 20
```

### Insert Text
```bash
StrEditor.exe --file "test.pas" --text "// Comment" --insert-after-line 10
```

### Regex Replace
```bash
# Simple Regex Replace
StrEditor.exe --file "test.pas" --regex-pattern "f(\w+)" --regex-replace 'l$1' --verbose

# Case-insensitive
StrEditor.exe --file "test.pas" --regex-pattern "procedure" --regex-replace "function" -i
```

**IMPORTANT for PowerShell:** Use **single quotes** `'...'` for `--regex-replace` when using Capture Groups (`$1`, `$2`, etc.)!

---

## Advanced Features

### Base64-Parameter (v1.5)
Solve PowerShell special character problems by encoding parameters as Base64:

```bash
# Encode the string "ä ö ü ß" as Base64
$oldStr = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes("ä ö ü ß"))
$newStr = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes("Ä Ö Ü ẞ"))

# Use Base64-encoded parameters
StrEditor.exe --file "test.pas" --old-str-base64 $oldStr --new-str-base64 $newStr
```

### Multi-Line String Replace (v1.6)
Replace strings spanning multiple lines:

```bash
# Replace multi-line string
StrEditor.exe --file "test.pas" --old-str "begin\n  WriteLn('Hello');\nend" --new-str "start\n  Print('Hi');\nstop" --multi-line
```

---

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | String not found |
| 2 | Error (e.g., file not found) |
| 10 | Invalid parameter combination |

---

## Limitations

- **Regex Patterns**: Only single-line patterns are supported (no `\n` in patterns)
- **File Size**: Large files (>100 MB) may be slow
- **Encoding**: Only Windows-1252 and UTF-8 are supported

---

## Links

- **GitHub Repository**: https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor
- **Augment Rules**: [DOC/AUGMENT-RULES.md](AUGMENT-RULES.md)
- **Changelog**: [CHANGELOG.md](../CHANGELOG.md)

