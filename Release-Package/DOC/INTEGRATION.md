# StrEditor - Integration Guide

**Version:** 1.7.7
**Last Updated:** 2026-01-20

---

## 📌 TL;DR - Quick Start

1. **Use `StrEditor.exe`** instead of `str-replace-editor` for Delphi files
2. **For MULTIPLE operations** → Always use **JSON-Config** (not multiple single calls!)
3. **Path:** `C:\Delphi XE16\bin\StrEditor.exe` (in PATH)

### Quick Example: JSON-Config (Preferred Method)
```json
{
  "operations": [
    {"command": "replace-line", "file": "MyUnit.pas", "line": 25, "text": "  NewCode;"},
    {"command": "delete-line", "file": "MyUnit.pas", "line": 30},
    {"command": "insert-before", "file": "MyUnit.pas", "line": 10, "text": "  // Comment"}
  ]
}
```
```bash
StrEditor.exe --config operations.json --backup --verbose
```

---

## Overview

StrEditor is a command-line tool for editing Delphi source files while preserving encoding (Windows-1252 and UTF-8). It is designed to replace the internal `str-replace-editor` tool used by Augment/VSCode.

---

## ⚠️ IMPORTANT: Use JSON-Config for Multiple Operations!

**Why JSON-Config?**
| Single Calls (❌ Inefficient) | JSON-Config (✅ Efficient) |
|-------------------------------|---------------------------|
| File read/written N times | File read/written 1 time |
| Each call is isolated | All operations atomic |
| Manual line number tracking | Automatic sorting (high→low) |
| Escaping issues in PowerShell | No escaping needed |

**Rule:** If you need more than 2 operations on a file → Use JSON-Config!

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

### Line Manipulation Features (v1.7)
- **Delete Line**: Delete a single line (`--delete-line <n>`)
- **Delete Lines**: Delete multiple lines (`--delete-lines <n,m,k>` or `--start-line/--end-line`)
- **Replace Line**: Replace a complete line (`--replace-line <n> --with <text>`)
- **Base64 Support**: Use `--with-base64` for special characters in replace-line
- **Direct Line Manipulation**: Simpler and more efficient than string replacement

### JSON Config & Batch Mode (v1.7.1)
- **JSON Config Support**: Load line manipulation operations from JSON files
- **Batch Mode**: Multiple line operations are automatically sorted and executed in optimal order
- **Automatic Sorting**: Operations are executed from highest to lowest line number to avoid index shifting
- **Mixed Operations**: Combine delete-line, delete-lines, and replace-line in one JSON config

### Insert Before Line (v1.7.2)
- **Insert Before Line**: Insert text before a specific line with `--insert-before-line <n>`
- **1-based Line Numbering**: User-friendly line numbering (line 1 = first line)
- **JSON Config Support**: Use `"command": "insert-before"` in JSON config files
- **Macro Support**: Full macro expansion support (`{{LINE_NUMBER}}`, `{{FILE_NAME}}`, etc.)

### Documentation Viewer (v1.7.3)
- **Documentation Viewer**: View documentation files with `--docs [<file>]` command
- **Browser Integration**: Open documentation in default browser with `--open-in-browser` flag
- **Documentation Listing**: List all available documentation files with `--docs --list`

### Repair Umlauts (v1.7.4)
- **Repair Umlauts**: Automatically repair broken umlauts in Delphi source files (`--repair-umlauts`)
- **VCS Integration**: Automatically uses Mercurial (hg) or Git to get original file content
- **Reference File**: Use a reference file instead of VCS (`--reference <file>`)
- **Smart Detection**: Detects broken UTF-8 bytes in Windows-1252 files (Ã¤ → ä, Ã¶ → ö, etc.)

### Move Lines (v1.7.5)
- **Move Lines**: Move source code lines from one file to another (`--move-lines`)
- **From/To Files**: Specify source (`--from <file>`) and target (`--to <file>`) files
- **Line Range**: Specify lines to move (`--start-line <n>`, `--end-line <n>`)
- **Insert Position**: Insert after (`--insert-after-line <n>`) or before (`--insert-before-line <n>`) a line
- **Encoding Preservation**: Both source and target files keep their original encoding
- **Backup Support**: Create backups for both files with `--backup`
- **JSON Config Support**: Can be used in batch operations via JSON config

### Delete Config on Success (v1.7.6)
- **Auto-Cleanup**: Automatically delete JSON config file after successful execution (`--delete-config-on-success`)
- **Only on Success**: Config is only deleted if all operations completed successfully (ExitCode = 0)
- **Dry-Run Safe**: Config is NOT deleted when using `--dry-run` (no actual changes made)
- **Use Case**: Useful for automated scripts and CI/CD pipelines

```bash
# Execute config and delete it on success
StrEditor.exe --config operations.json --delete-config-on-success --verbose
```

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

### Insert Text After Line
```bash
StrEditor.exe --file "test.pas" --text "// Comment" --insert-after-line 10
```

### Insert Text Before Line (v1.7.2)
```bash
# Insert before line 1 (at the beginning)
StrEditor.exe --file "test.pas" --text "// Header Comment" --insert-before-line 1

# Insert before line 10
StrEditor.exe --file "test.pas" --text "// New Comment" --insert-before-line 10

# With Base64 encoding
StrEditor.exe --file "test.pas" --text-base64 "Ly8gQ29tbWVudA==" --insert-before-line 5
```

### Repair Umlauts (v1.7.4)
```bash
# Repair umlauts using VCS (Mercurial/Git)
StrEditor.exe --file "broken.pas" --repair-umlauts --backup --verbose

# Preview changes without modifying (dry-run)
StrEditor.exe --file "broken.pas" --repair-umlauts --dry-run --verbose

# Use reference file instead of VCS
StrEditor.exe --file "broken.pas" --repair-umlauts --reference "original.pas" --verbose
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

### Line Manipulation (v1.7)
Direct line manipulation is simpler and more efficient than string replacement:

```bash
# Delete a single line
StrEditor.exe --file "test.pas" --delete-line 25 --backup

# Delete multiple lines (comma-separated)
StrEditor.exe --file "test.pas" --delete-lines "1,3,5,7" --backup

# Delete line range
StrEditor.exe --file "test.pas" --delete-lines --start-line 10 --end-line 20 --backup

# Replace a complete line
StrEditor.exe --file "test.pas" --replace-line 25 --with "  WriteLn('New');" --backup

# Replace line with Base64-encoded text (for special characters)
$text = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes("  WriteLn('Ä Ö Ü');"))
StrEditor.exe --file "test.pas" --replace-line 25 --with-base64 $text --backup

# Dry-run mode (test without modifying)
StrEditor.exe --file "test.pas" --delete-line 25 --dry-run

# Show diff
StrEditor.exe --file "test.pas" --replace-line 25 --with "New Line" --diff
```

### JSON Config & Batch Mode (v1.7.1)
Load multiple line operations from a JSON config file:

```json
{
  "operations": [
    {
      "command": "delete-line",
      "file": "test.pas",
      "line": 10
    },
    {
      "command": "delete-lines",
      "file": "test.pas",
      "lines": "5,15,25"
    },
    {
      "command": "delete-lines",
      "file": "test.pas",
      "start-line": 30,
      "end-line": 40
    },
    {
      "command": "replace-line",
      "file": "test.pas",
      "line": 50,
      "text": "  WriteLn('New Line');"
    },
    {
      "command": "replace-line",
      "file": "test.pas",
      "line": 60,
      "text": "ICBXcml0ZUxuKCdOZXcgTGluZScpOw==",
      "text-base64-encoded": true
    },
    {
      "command": "insert-before",
      "file": "test.pas",
      "insert-before-line": 1,
      "text": "// Header Comment"
    },
    {
      "command": "insert-before",
      "file": "test.pas",
      "insert-before-line": 70,
      "text": "Ly8gTmV3IENvbW1lbnQ=",
      "text-base64-encoded": true
    }
  ]
}
```

Execute the JSON config:
```bash
StrEditor.exe --config operations.json --verbose
```

**Automatic Batch Mode:**
- When multiple line operations are detected, StrEditor automatically enters batch mode
- Operations are sorted by highest line number first
- Execution order: highest → lowest line number
- This prevents index shifting issues when deleting/modifying lines

**Example:**
```bash
# User specifies: delete line 5, delete line 20, delete line 10
# StrEditor executes: delete line 20, then line 10, then line 5
# Result: All correct lines are deleted!
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

