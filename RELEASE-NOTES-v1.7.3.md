# StrEditor v1.7.3 - Release Notes

**Release Date:** 2025-11-17
**Build:** 2025-11-17

---

## 🎉 What's New in v1.7.3

### 📚 Documentation Viewer Feature

StrEditor now includes a built-in documentation viewer that allows you to view documentation files directly from the command line!

#### Key Features:

1. **View Documentation Files**
   - View README.md (default) or any other .md file in the exe directory
   - Encoding-aware display (UTF-8 support)
   - Command: `StrEditor.exe --docs [<file>]`

2. **Open in Browser**
   - Open documentation files in your default browser
   - Perfect for viewing formatted Markdown
   - Command: `StrEditor.exe --docs --open-in-browser`

3. **List Available Documentation**
   - See all available .md files in the exe directory
   - Command: `StrEditor.exe --docs --list`

4. **Head/Tail Filtering**
   - Show first N lines: `--head <n>`
   - Show last N lines: `--tail <n>`
   - Useful for quick previews

5. **Line Numbers**
   - Display line numbers with `--line-numbers`
   - Helpful for referencing specific sections

---

## 📋 Usage Examples

```bash
# View default documentation (README.md)
StrEditor.exe --docs

# View specific documentation file
StrEditor.exe --docs INTEGRATION.md

# Open documentation in browser
StrEditor.exe --docs --open-in-browser
StrEditor.exe --docs INTEGRATION.md --open-in-browser

# Show first 20 lines
StrEditor.exe --docs --head 20

# Show last 30 lines with line numbers
StrEditor.exe --docs --tail 30 --line-numbers

# List all available documentation files
StrEditor.exe --docs --list
```

---

## 🔧 Technical Details

### New Files:
- `StrEditor.Documentation.pas` - Documentation display module
- `Tests/TestStrEditor.Documentation.pas` - Unit tests for documentation feature

### Modified Files:
- `StrEditor.CommandLine.pas` - Added `ctDocs` command type and parameter parsing
- `StrEditor.dpr` - Integrated documentation feature into main program
- `Tests/Unittests.dpr` - Added documentation tests

### Bug Fixes:
- Fixed `GetParamValue()` to ignore flag parameters (starting with `-`)
  - Previously, `--docs --head 20` would incorrectly return `--head` as the value for `--docs`
  - Now correctly returns empty string when next parameter is a flag

---

## ✅ Quality Assurance

- **All 112 tests passing** (DUnitX)
- **9 new tests added** for documentation feature
- **No breaking changes** - fully backward compatible

---

## 📦 Installation

1. Download `StrEditor-v1.7.3-Windows-x86.zip`
2. Extract `StrEditor.exe` to a directory in your PATH (e.g., `C:\Delphi XE16\bin`)
3. Copy documentation files (.md) to the same directory as StrEditor.exe
4. Run `StrEditor.exe --docs` to verify installation

---

## 🔗 Links

- **GitHub Repository:** https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor
- **Documentation:** See `DOC/INTEGRATION.md` for Augment Agent integration
- **License:** MIT License - see `LICENSE.md`

---

## 📝 Full Changelog

See [CHANGELOG.md](CHANGELOG.md) for complete version history.

---

**Previous Version:** v1.7.2 (2025-11-12)
**Next Version:** TBD

