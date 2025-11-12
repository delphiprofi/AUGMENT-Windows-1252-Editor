# AUGMENT-Windows-1252-Editor
This is a replacement for the built-in str-replace-editor, if you are dealing with legacy code that is not UTF-8 and cannot be migrated!

**Version:** 1.7.1
**Last Updated:** 2025-11-11

---

## 📝 Description

StrEditor is a command-line tool for replacing and inserting text in Delphi source files with **Encoding-Preservation**.

### ✨ Main Features

- ✅ **Encoding-Preservation**: Windows-1252 and UTF-8 are correctly detected and preserved
- ✅ **Encoding Detection**: Detect file encoding (`--detect-encoding`) **[NEW in v1.1]**
- ✅ **Encoding Conversion**: Convert between Windows-1252 and UTF-8 (`--convert-encoding`) **[NEW in v1.3]**
- ✅ **Reinterpret Encoding**: Fix broken encodings (`--reinterpret-as`) **[NEW in v1.4]**
- ✅ **Base64-Parameter**: Solve PowerShell special character problems (`--old-str-base64`) **[NEW in v1.5]**
- ✅ **Multi-Line String Replace**: Replace strings spanning multiple lines (`--multi-line`) **[NEW in v1.6]**
- ✅ **Line Manipulation**: Delete and replace complete lines (`--delete-line`, `--replace-line`) **[NEW in v1.7]**
- ✅ **JSON Config & Batch Mode**: Load multiple line operations from JSON with automatic sorting **[NEW in v1.7.1]**
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
- [DOC/INTEGRATION.md](DOC/INTEGRATION.md) - Full integration guide
- [DOC/AUGMENT-RULES.md](DOC/AUGMENT-RULES.md) - Rules for Augment Agent integration
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

**Test Results (v1.6.0):**
- ✅ 76 tests implemented
- ✅ 76 tests passing
- ✅ 0 tests failing

---

## 📁 Project Structure

```
DelphiStrEditor/
├── StrEditor.dpr                    # Main program
├── StrEditor.dproj                  # Project file
├── StrEditor.Encoding.pas           # Encoding support
├── StrEditor.Operations.pas         # String operations
├── StrEditor.CommandLine.pas        # Command-line interface
├── StrEditor.CaseConversion.pas     # Case conversion (v1.1)
├── StrEditor.Conditional.pas        # Conditional replacements (v1.1)
├── StrEditor.Config.pas             # JSON config support (v1.1)
├── StrEditor.Indent.pas             # Indent/outdent (v1.1)
├── StrEditor.Undo.pas               # Undo support (v1.1)
├── Tests/
│   ├── Unittests.dpr                # Unit test program
│   ├── Unittests.dproj              # Unit test project
│   ├── TestStrEditor.Encoding.pas   # Encoding tests
│   └── TestStrEditor.Operations.pas # Operations tests
├── DOC/
│   ├── INTEGRATION.md               # Full integration guide
│   └── AUGMENT-RULES.md             # Augment Agent rules
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

© 2025 Frank Lauter ( DelphiProfi ) 
http://www.delphiprofi.com

---

## 🤝 Contact

For questions or issues, please create an issue on GitHub:
https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/issues

