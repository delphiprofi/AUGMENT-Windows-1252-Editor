# StrEditor v1.7.2 - Insert Before Line Feature

## 🎉 New Features

- **Insert Before Line**: Insert text before a specific line with `--insert-before-line <n>`
- **1-based Line Numbering**: User-friendly line numbering (line 1 = first line)
- **JSON Config Support**: `insert-before` command in JSON config files
- **MIT License**: Added MIT License to the project
- **Quick Start**: Added Quick Start section to README

## 📦 What's Included

- `StrEditor.exe` - Command-line tool (Windows x86)
- `README.md` - Full documentation
- `LICENSE.md` - MIT License
- `CHANGELOG.md` - Version history
- `DOC/` - Integration guides and rules

## 🚀 Installation

1. Download `StrEditor-v1.7.2-Windows-x86.zip`
2. Extract `StrEditor.exe` to a directory in your PATH (e.g., `C:\Delphi XE16\bin`)
3. Run `StrEditor.exe --version` to verify installation

## 📝 Usage Examples

```bash
# Insert text before line 1 (at the beginning)
StrEditor.exe --file "test.pas" --text "// Header Comment" --insert-before-line 1

# Insert text before line 10
StrEditor.exe --file "test.pas" --text "  fNewField : string;" --insert-before-line 10

# With Base64 encoding
StrEditor.exe --file "test.pas" --text-base64 "Ly8gQ29tbWVudA==" --insert-before-line 5
```

## 🧪 Tests

- **103/103 tests passing** (DUnitX)
- Added 3 new tests for `--insert-before-line` feature

## 📚 Full Documentation

- [README.md](https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/blob/main/README.md)
- [CHANGELOG.md](https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/blob/main/CHANGELOG.md)
- [DOC/INTEGRATION.md](https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/blob/main/DOC/INTEGRATION.md)

## 🔗 Links

- **GitHub Repository**: https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor
- **Author**: Frank Lauter (DelphiProfi)
- **Website**: http://www.delphiprofi.com
- **License**: MIT License

## 📋 Implementation Details

### Added Files
- `LICENSE.md` - MIT License
- `StrEditor.BatchProcessor.pas` - Batch processing for line operations
- `Tests/TestStrEditor.Config.pas` - Tests for JSON config parsing

### Modified Files
- `StrEditor.Operations.pas` - Added `InsertBefore()` method and `InsertTextBefore()` helper
- `StrEditor.CommandLine.pas` - Added `ctInsertBefore` command type
- `StrEditor.Config.pas` - Added JSON parsing for `insert-before-line`
- `StrEditor.dpr` - Added `ctInsertBefore` case handler
- `Tests/TestStrEditor.Operations.pas` - Added 3 new tests
- `README.md` - Added Quick Start section and license info
- `README.de.md` - Added Schnellstart section and license info
- `CHANGELOG.md` - Added v1.7.2 section
- `DOC/INTEGRATION.md` - Updated to v1.7.2
- `DOC/AUGMENT-RULES.md` - Updated to v1.7.2
- `.augment/rules/str-replace-editor.md` - Updated to v1.7.2

## 🔄 Version History

- **v1.7.2** (2025-11-12) - Insert Before Line Feature
- **v1.7.1** (2025-11-11) - JSON Config & Batch Mode
- **v1.7.0** (2025-11-11) - Line Manipulation (delete-line, replace-line)
- **v1.6.0** (2025-11-11) - Multi-Line String Replace
- **v1.5.0** (2025-11-11) - Base64 Parameters
- **v1.4.0** (2025-11-11) - Reinterpret Encoding
- **v1.3.0** (2025-11-11) - Encoding Conversion
- **v1.2.0** (2025-11-11) - Show/Cat Command
- **v1.1.0** (2025-11-11) - Encoding Detection & Verbose Mode
- **v1.0.0** (2025-11-10) - Initial Release

