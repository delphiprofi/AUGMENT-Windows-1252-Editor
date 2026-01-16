# StrEditor - Augment Agent Integration Rules

**Version:** 1.7.4
**Last Updated:** 2026-01-11

---

## 📌 THE 3 MOST IMPORTANT RULES

1. **🚫 NEVER** use the internal `str-replace-editor` for `.pas` files → Broken umlauts!
2. **✅ ALWAYS** use `StrEditor.exe` for Delphi files
3. **🎯 FOR MULTIPLE OPERATIONS** → **Use JSON-Config!** (not multiple single calls)

---

## ⚠️ CRITICAL: JSON-Config is the PREFERRED Method!

**Why JSON-Config instead of multiple single calls?**
- **Efficiency:** File is read/written only ONCE (not N times)
- **Atomicity:** All operations execute together or none
- **Auto-Sorting:** Line operations are sorted high→low (prevents index shifting)
- **No Escaping:** No PowerShell escaping issues with special characters

### ❌ WRONG: Multiple Single Calls
```bash
# DON'T DO THIS! Inefficient!
StrEditor.exe --file "MyUnit.pas" --replace-line 25 --with "Text1"
StrEditor.exe --file "MyUnit.pas" --replace-line 30 --with "Text2"
StrEditor.exe --file "MyUnit.pas" --delete-line 35
```

### ✅ CORRECT: One JSON-Config
```json
{
  "operations": [
    {"command": "replace-line", "file": "MyUnit.pas", "line": 25, "text": "Text1"},
    {"command": "replace-line", "file": "MyUnit.pas", "line": 30, "text": "Text2"},
    {"command": "delete-line", "file": "MyUnit.pas", "line": 35}
  ]
}
```
```bash
StrEditor.exe --config operations.json --backup --verbose
```

---

## Overview

This document contains the rules for integrating StrEditor with Augment Agent. StrEditor replaces the internal `str-replace-editor` tool used by Augment/VSCode.

---

## Basic Usage (Single Operations Only)

```bash
# Simple string replace
StrEditor.exe --file "path\to\file.pas" --old-str "oldString" --new-str "newString"

# String replace with line range
StrEditor.exe --file "path\to\file.pas" --old-str "oldString" --new-str "newString" --start-line 10 --end-line 20

# Insert text after line
StrEditor.exe --file "path\to\file.pas" --text "// Comment" --insert-after-line 10

# Insert text before line (v1.7.2)
StrEditor.exe --file "path\to\file.pas" --text "// Header" --insert-before-line 1

# Multi-line string replace (v1.6)
StrEditor.exe --file "path\to\file.pas" --old-str "begin\n  WriteLn('Hello');\nend" --new-str "start\n  Print('Hi');\nstop" --multi-line

# Delete a single line (v1.7)
StrEditor.exe --file "path\to\file.pas" --delete-line 25 --backup

# Delete multiple lines (v1.7)
StrEditor.exe --file "path\to\file.pas" --delete-lines "1,3,5,7" --backup

# Replace a complete line (v1.7)
StrEditor.exe --file "path\to\file.pas" --replace-line 25 --with "  WriteLn('New');" --backup

# JSON Config with multiple operations (v1.7.1)
StrEditor.exe --config "operations.json" --verbose

# Repair broken umlauts (v1.7.4)
StrEditor.exe --file "path\to\file.pas" --repair-umlauts --backup --verbose
```

---

## Most Important Rules

### 1. Encoding Preservation
- StrEditor automatically detects and preserves file encoding (Windows-1252 or UTF-8)
- **DO NOT** manually specify encoding unless using `--convert-encoding` or `--reinterpret-as`

### 2. Line Endings
- StrEditor preserves Windows line endings (CRLF)
- **DO NOT** convert line endings to LF

### 3. PowerShell Special Characters
- Use **single quotes** `'...'` for `--regex-replace` when using capture groups (`$1`, `$2`, etc.)
- For complex strings with special characters, use `--old-str-base64` and `--new-str-base64` (v1.5)

### 4. Multi-Line Replacements
- Use `--multi-line` flag for strings spanning multiple lines (v1.6)
- Use `\n` to represent line breaks in `--old-str` and `--new-str`

### 4a. Line Manipulation (v1.7) - RECOMMENDED
- **PREFER** line manipulation over string replacement when possible
- Use `--delete-line <n>` to delete a single line
- Use `--delete-lines <n,m,k>` to delete multiple lines (comma-separated)
- Use `--delete-lines --start-line <n> --end-line <m>` to delete a line range
- Use `--replace-line <n> --with <text>` to replace a complete line
- Use `--with-base64` for special characters in replace-line
- **ADVANTAGES**: Simpler, faster, more precise than string replacement

### 5. Line Range
- Use `--start-line` and `--end-line` to limit replacements to specific lines
- Line numbers are 1-based (first line is 1, not 0)

### 6. Dry-Run Mode
- Use `--dry-run` to test changes without modifying files
- Always use `--verbose` with `--dry-run` to see what would be changed

### 7. Backup
- Use `--backup` to create a backup before editing
- Backups are saved with `.bak` extension

### 8. Exit Codes
- Check exit code to determine success/failure
- Exit code 0 = success, 1 = string not found, 2 = error, 10 = invalid parameters

### 9. Regex Patterns
- Use `--regex-pattern` and `--regex-replace` for pattern-based replacements
- Use `-i` for case-insensitive matching
- **DO NOT** use `\n` in regex patterns (only single-line patterns are supported)

### 10. Macro Expansion
- Use macros like `{{LINE_NUMBER}}`, `{{FILE_NAME}}`, `{{DATE}}`, `{{TIME}}` in replacement strings
- Macros are expanded automatically

### 11. Verbose Output
- Use `--verbose` to see detailed output for debugging
- Verbose output shows encoding detection, line numbers, and replacement details

### 12. JSON Config & Batch Mode (v1.7.1)
- Use JSON config files to load multiple line operations
- **Automatic Batch Mode**: Multiple line operations are automatically sorted and executed in optimal order
- **Sorting**: Operations are executed from highest to lowest line number
- **Advantage**: Prevents index shifting issues when deleting/modifying multiple lines
- **Supported Commands**: `delete-line`, `delete-lines`, `replace-line`
- **Example**: See [DOC/INTEGRATION.md](INTEGRATION.md) for JSON config examples

### 13. Repair Umlauts (v1.7.4)
- Use `--repair-umlauts` to automatically repair broken umlauts in Delphi files
- **VCS Integration**: Automatically uses Mercurial (hg) or Git to get original content
- **Reference File**: Use `--reference <file>` to specify a known-good reference file
- **Broken Sequences**: Detects and repairs Ã¤→ä, Ã¶→ö, Ã¼→ü, Ã→ß, Ã„→Ä, Ã–→Ö, Ãœ→Ü
- **Use Case**: Repair files damaged by `str-replace-editor` saving as UTF-8 without BOM
- **Example**: `StrEditor.exe --file "broken.pas" --repair-umlauts --backup --verbose`

---

## Checklist Before Each Use

1. ✅ **File Path**: Is the file path correct and enclosed in quotes?
2. ✅ **Old String**: Is the old string exactly as it appears in the file?
3. ✅ **New String**: Is the new string correct?
4. ✅ **Line Range**: Do I need to limit the replacement to specific lines?
5. ✅ **Multi-Line**: Does the string span multiple lines? Use `--multi-line`
6. ✅ **Special Characters**: Do I need to use Base64 encoding for special characters?
7. ✅ **Dry-Run**: Should I test the change first with `--dry-run`?
8. ✅ **Backup**: Should I create a backup with `--backup`?
9. ✅ **Verbose**: Do I need detailed output with `--verbose`?
10. ✅ **Exit Code**: Will I check the exit code to determine success/failure?

---

## Examples

### Example 1: Simple String Replace
```bash
StrEditor.exe --file "MyUnit.pas" --old-str "nil" --new-str "NIL"
```

### Example 2: String Replace with Line Range
```bash
StrEditor.exe --file "MyUnit.pas" --old-str "nil" --new-str "NIL" --start-line 50 --end-line 100
```

### Example 3: Insert Comment
```bash
StrEditor.exe --file "MyUnit.pas" --text "  // TODO: Implement this" --insert-after-line 10
```

### Example 4: Multi-Line String Replace (v1.6)
```bash
StrEditor.exe --file "MyUnit.pas" --old-str "begin\n  WriteLn('Hello');\nend" --new-str "start\n  Print('Hi');\nstop" --multi-line
```

### Example 5: Base64-Parameter (v1.5)
```powershell
$oldStr = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes("ä ö ü ß"))
$newStr = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes("Ä Ö Ü ẞ"))
StrEditor.exe --file "MyUnit.pas" --old-str-base64 $oldStr --new-str-base64 $newStr
```

### Example 6: Regex Replace with Capture Groups
```bash
StrEditor.exe --file "MyUnit.pas" --regex-pattern "f(\w+)" --regex-replace 'l$1' --verbose
```

### Example 7: Dry-Run with Verbose
```bash
StrEditor.exe --file "MyUnit.pas" --old-str "nil" --new-str "NIL" --dry-run --verbose
```

### Example 8: JSON Config with Multiple Line Operations (v1.7.1)
```json
{
  "operations": [
    {
      "command": "delete-line",
      "file": "MyUnit.pas",
      "line": 10
    },
    {
      "command": "replace-line",
      "file": "MyUnit.pas",
      "line": 25,
      "text": "  WriteLn('New Line');"
    },
    {
      "command": "delete-lines",
      "file": "MyUnit.pas",
      "lines": "5,15,20"
    }
  ]
}
```

Execute:
```bash
StrEditor.exe --config "operations.json" --verbose
```

**Note**: Operations are automatically sorted and executed from highest to lowest line number!

---

## Links

- **Full Documentation**: [DOC/INTEGRATION.md](INTEGRATION.md)
- **GitHub Repository**: https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor
- **Changelog**: [CHANGELOG.md](../CHANGELOG.md)

