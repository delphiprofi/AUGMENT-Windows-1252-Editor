# AUGMENT-Windows-1252-Editor
This is a replacement for the built-in str-replace-editor, if you are dealing with legacy code that is not UTF-8 and cannot be migrated!

**Version:** 1.10.2
**Last Updated:** 2026-03-03
**License:** MIT License - see [LICENSE.md](LICENSE.md)

---

## 🚀 Quick Start

```bash
# Download the latest release
# Extract StrEditor.exe to a directory in your PATH (e.g., C:\Delphi XE16\bin)

# Replace a string in a Delphi file
StrEditor.exe --file "MyUnit.pas" --old-str "TObject" --new-str "TInterfacedObject"

# Insert text before line 10
StrEditor.exe --file "MyUnit.pas" --text "  fNewField : string;" --insert-before-line 10

# Delete a line
StrEditor.exe --file "MyUnit.pas" --delete-line 25 --backup

# Show file content (encoding-aware)
StrEditor.exe --file "MyUnit.pas" --show --head 20
```

**For Augment Agent users:** StrEditor automatically replaces the built-in `str-replace-editor` when installed in your PATH. See [DOC/INTEGRATION.md](DOC/INTEGRATION.md) for details.

**For MCP users:** See [DOC/QUICKSTART.md](DOC/QUICKSTART.md) for the English MCP quick start and [DOC/QUICKSTART.de.md](DOC/QUICKSTART.de.md) for the German version.

---

## 📝 Description

StrEditor is a command-line tool for replacing and inserting text in Delphi source files with **Encoding-Preservation**.

### ✨ Main Features

- ✅ **Signal-Restart (`--signal-restart`)**: Gracefully shut down all running MCP server instances via Named Event **[NEW in v1.10.2]**
- ✅ **Comment/Uncomment Lines**: Add/remove `//` comments by line range (`--comment-lines`, `--uncomment-lines`) **[NEW in v1.10.2]**
- ✅ **MCP Server (`--mcp`)**: Native JSON-RPC 2.0 server over stdio — 350-4000x faster than CLI **[NEW in v1.10.0]**
- ✅ **Parameter Validation**: edit_file validates all required parameters — no more silent defaults **[FIXED in v1.10.2]**
- ✅ **WriteFile Retry**: Reliable file writes with 3 retries and 100ms delay on transient locks **[FIXED in v1.9.3]**
- ✅ **Tolerant replace-line**: Auto-splits multi-line text instead of corrupting the file (with stderr warning) **[NEW in v1.9.3]**
- ✅ **JSON Alias `"line"`**: Accepted as alias for `"insert-after-line"` (with stderr warning) **[NEW in v1.9.3]**
- ✅ **File-Access Retry**: 3 retries with 100ms delay for read/write on locked files **[NEW in v1.9.1]**
- ✅ **File Compare**: Compare file against master for broken special characters (`--filecompare`) **[NEW in v1.8.7]**
- ✅ **Encoding-Preservation**: Windows-1252 and UTF-8 are correctly detected and preserved
- ✅ **Encoding Detection**: Detect file encoding (`--detect-encoding`) **[NEW in v1.1]**
- ✅ **Encoding Conversion**: Convert between Windows-1252 and UTF-8 (`--convert-encoding`) **[NEW in v1.3]**
- ✅ **Reinterpret Encoding**: Fix broken encodings (`--reinterpret-as`) **[NEW in v1.4]**
- ✅ **Base64-Parameter**: Solve PowerShell special character problems (`--old-str-base64`) **[NEW in v1.5]**
- ✅ **Multi-Line String Replace**: Replace strings spanning multiple lines (`--multi-line`) **[NEW in v1.6]**
- ✅ **Line Manipulation**: Delete and replace complete lines (`--delete-line`, `--replace-line`) **[NEW in v1.7]**
- ✅ **JSON Config & Batch Mode**: Load multiple line operations from JSON with automatic sorting **[NEW in v1.7.1]**
- ✅ **Insert Before Line**: Insert text before a specific line (`--insert-before-line`) **[NEW in v1.7.2]**
- ✅ **Documentation Viewer**: View documentation files with `--docs` command **[NEW in v1.7.3]**
- ✅ **Repair Umlauts**: Automatically repair broken umlauts (`--repair-umlauts`) **[NEW in v1.7.4]**
- ✅ **Move Lines**: Move source code lines between files (`--move-lines`) **[NEW in v1.7.5]**
- ✅ **ChangeReport**: Shows what was changed after each operation **[NEW in v1.8.4]**
- ✅ **SessionLog**: Logs all operations for error analysis **[NEW in v1.8.4]**
- ✅ **INI-Config**: Configure ChangeReport and SessionLog via StrEditor.ini **[NEW in v1.8.4]**
- ✅ **Auto-Delete Config**: JSON config is auto-deleted on success (use `--keep-config` to preserve) **[CHANGED in v1.8.3]**
- ✅ **Indent Lines**: Add spaces at line beginnings (`--indent-lines`) **[NEW in v1.8.2]**
- ✅ **Unindent Lines**: Remove spaces from line beginnings (`--unindent-lines`) **[NEW in v1.8.2]**
- ✅ **Hex-Dump Output**: Display file as hex dump for encoding debugging (`--hex`) **[NEW in v1.8.1]**
- ✅ **Base64 Output**: Display file as Base64 string (`--base64`) **[NEW in v1.8.1]**
- ✅ **Original Line Numbers**: JSON config line numbers refer to original file state **[NEW in v1.8.1]**
- ✅ **text-lines Array**: Multi-line text in JSON without escaping **[NEW in v1.8.0]**
- ✅ **replace-lines Command**: Replace line ranges atomically in JSON **[NEW in v1.8.0]**
- ✅ **Literal \r\n Warning**: Warns when agent uses \r\n incorrectly **[NEW in v1.8.0]**
- ✅ **Categorized Help**: Compact help with `--help [category]` **[NEW in v1.8.0]**
- ✅ **Parameter Aliases**: Short names: `--ib`, `--ia`, `--dl`, `--rl`, `--ob64`, `--nb64` **[NEW in v1.8.0]**
- ✅ **Show/Cat Command**: Display file content with encoding awareness (`--show`) **[NEW in v1.2]**
- ✅ **Umlaut-Support**: German umlauts (ü, ö, ä, ß) are correctly handled
- ✅ **String-Replace**: Exact string replacement with optional line ranges
- ✅ **Insert**: Insert text after specific line
- ✅ **Regex-Replace**: Pattern-based replacement with Capture Groups (`$1`, `$2`, etc.)
- ✅ **Macro-Expansion**: Variables like `{{LINE_NUMBER}}`, `{{FILE_NAME}}`, `{{DATE}}`, `{{TIME}}`
- ✅ **Conditional Replacements**: Replace only in lines matching a pattern
- ✅ **Case Conversion**: Convert case (upper/lower/title)
- ✅ **Indent/Outdent**: Indent or outdent lines
- ✅ **Backup & Undo**: Create backups and restore them
- ✅ **Dry-Run Mode**: Test changes without modifying files
- ✅ **Batch Processing**: Process multiple files at once
- ✅ **JSON Config**: Load operations from JSON files
- ✅ **Line-Ending-Preservation**: CRLF (Windows) is preserved
- ✅ **Exit-Codes**: Clear exit codes for automation
- ✅ **Verbose Mode**: Detailed output for debugging
- ✅ **Range Parameter**: Show specific line ranges with `--range <start>,<end>` **[NEW in v1.8.6]**

---

## 🚀 Installation

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

## 📖 Usage

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

### Verbose Mode

```bash
StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL" --verbose
```

### Regex Replace

```bash
# Simple Regex Replace
StrEditor.exe --file "test.pas" --regex-pattern "f(\w+)" --regex-replace 'l$1' --verbose

# Case-insensitive
StrEditor.exe --file "test.pas" --regex-pattern "procedure" --regex-replace "function" -i
```

**IMPORTANT for PowerShell:** Use **single quotes** `'...'` for `--regex-replace` when using Capture Groups (`$1`, `$2`, etc.)! PowerShell interprets `$1` in double quotes as a variable.

### Macro-Expansion

```bash
# Macros in Replacement-Strings
StrEditor.exe --file "test.pas" --old-str "interface" --new-str "interface // Line {{LINE_NUMBER}} in {{FILE_NAME}}" --verbose
```

**Available Macros:**
- `{{LINE_NUMBER}}` - Current line number
- `{{FILE_NAME}}` - File name (without path)
- `{{DATE}}` - Current date (yyyy-mm-dd)
- `{{TIME}}` - Current time (hh:nn:ss)

---

## 📚 Documentation

For complete documentation, see:
- [DOC/QUICKSTART.md](DOC/QUICKSTART.md) - MCP quick start (English)
- [DOC/QUICKSTART.de.md](DOC/QUICKSTART.de.md) - MCP quick start (Deutsch)
- [DOC/INTEGRATION.md](DOC/INTEGRATION.md) - Full integration guide
- [DOC/AGENT-COOKBOOK.md](DOC/AGENT-COOKBOOK.md) - Practical recipes for common tasks **[NEW in v1.8.0]**
- [CHANGELOG.md](CHANGELOG.md) - Version history

---

## 🎯 Exit-Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | String not found |
| 2 | Error (e.g., file not found) |
| 10 | Invalid parameter combination |

---

## 🧪 Testing

The project includes comprehensive unit tests:

**Test Results (v1.8.4):**
- ✅ 191 tests implemented
- ✅ 191 tests passing
- ✅ 0 tests failing

---

## 📁 Project Structure

```
DelphiStrEditor/
├── StrEditor.dpr                    # Main program
├── StrEditor.dproj                  # Project file
├── StrEditor.Encoding.pas           # Encoding support
├── StrEditor.Operations.pas         # String operations
├── StrEditor.CommandLine.pas        # Command-line & help system
├── StrEditor.Config.pas             # JSON config support
├── StrEditor.BatchProcessor.pas     # Batch/JSON operations (v1.7+)
├── StrEditor.Repair.pas             # Umlaut repair (v1.7.4+)
├── Tests/
│   ├── Unittests.dpr                # Unit test program
│   ├── TestStrEditor.Operations.pas # Operations tests
│   ├── TestStrEditor.V180.pas       # v1.8.0 feature tests
│   └── ...                          # More test files
├── DOC/
│   ├── INTEGRATION.md               # Full integration guide
│   └── AGENT-COOKBOOK.md            # Practical recipes (v1.8.0)
├── .augment/rules/
│   └── str-replace-editor.md        # Augment Agent rules
├── CHANGELOG.md                     # Version history
└── README.md                        # This file
```

---

## 🔒 Encoding Details

### Windows-1252

- **Detection**: No BOM present
- **Umlauts**: ü=$FC, ö=$F6, ä=$E4, ß=$DF, Ü=$DC, Ö=$D6, Ä=$C4
- **Conversion**: WinAPI `MultiByteToWideChar` / `WideCharToMultiByte` with Code Page 1252

### UTF-8

- **Detection**: BOM present ($EF $BB $BF)
- **Conversion**: Delphi `UTF8Encode` / `UTF8ToUnicodeString`

---

## 📄 License

MIT License - Copyright (c) 2025-2026 Frank Lauter (DelphiProfi)

See [LICENSE.md](LICENSE.md) for full license text.

---

## 🤝 Contact

For questions or issues, please create an issue on GitHub:
https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/issues

**Author:** Frank Lauter (DelphiProfi)
**Website:** http://www.delphiprofi.com

