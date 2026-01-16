# StrEditor v1.7.4 - Release Notes

**Release Date:** 2026-01-11
**Build:** 2026-01-11

---

## 🎉 What's New in v1.7.4

### 🔧 Repair Umlauts Feature

StrEditor now includes an automatic umlaut repair feature! When the built-in `str-replace-editor` accidentally saves a Windows-1252 file as UTF-8 without BOM, umlauts get corrupted (ä → Ã¤). This feature detects and repairs these broken sequences automatically.

#### Key Features:

1. **Automatic Repair**
   - Detects broken UTF-8 byte sequences in Windows-1252 files
   - Repairs: Ã¤→ä, Ã¶→ö, Ã¼→ü, Ã→ß, Ã„→Ä, Ã–→Ö, Ãœ→Ü
   - Command: `StrEditor.exe --file "broken.pas" --repair-umlauts`

2. **VCS Integration**
   - Automatically uses Mercurial (hg) or Git to get original file content
   - Compares original encoding with current file
   - Works out-of-the-box with no configuration

3. **Reference File Support**
   - Use a known-good file as reference instead of VCS
   - Command: `StrEditor.exe --file "broken.pas" --repair-umlauts --reference "original.pas"`

4. **Safe Mode (Dry-Run)**
   - Preview changes without modifying files
   - Command: `StrEditor.exe --file "broken.pas" --repair-umlauts --dry-run --verbose`

5. **Detailed Output**
   - With `--verbose` shows all detected and repaired umlauts
   - Shows position and character of each repair

---

## 📋 Usage Examples

```bash
# Repair umlauts using VCS (Mercurial/Git) - RECOMMENDED
StrEditor.exe --file "broken.pas" --repair-umlauts --backup --verbose

# Preview changes without modifying (dry-run)
StrEditor.exe --file "broken.pas" --repair-umlauts --dry-run --verbose

# Use reference file instead of VCS
StrEditor.exe --file "broken.pas" --repair-umlauts --reference "original.pas" --verbose

# Repair without backup (use with caution!)
StrEditor.exe --file "broken.pas" --repair-umlauts
```

---

## 🔧 Technical Details

### New Files:
- `StrEditor.Repair.pas` - Umlaut repair module

### Modified Files:
- `StrEditor.CommandLine.pas` - Added `ctRepairUmlauts` command type and parameter parsing
- `StrEditor.dpr` - Integrated repair feature into main program

### Supported Umlaut Repairs:
| Broken Sequence | Repaired Character |
|-----------------|-------------------|
| Ã¤ (C3 A4) | ä |
| Ã¶ (C3 B6) | ö |
| Ã¼ (C3 BC) | ü |
| Ã (C3 9F) | ß |
| Ã„ (C3 84) | Ä |
| Ã– (C3 96) | Ö |
| Ãœ (C3 9C) | Ü |
| Ã© (C3 A9) | é |

---

## ✅ Quality Assurance

- **All tests passing** (DUnitX)
- **No breaking changes** - fully backward compatible
- **Tested with**: Mercurial and Git repositories

---

## 📦 Installation

1. Download `StrEditor-v1.7.4-Windows-x86.zip`
2. Extract `StrEditor.exe` to a directory in your PATH (e.g., `C:\Delphi XE16\bin`)
3. Copy documentation files (.md) to the same directory as StrEditor.exe
4. Run `StrEditor.exe --version` to verify installation

---

## 🔗 Links

- **GitHub Repository:** https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor
- **Documentation:** See `DOC/INTEGRATION.md` for Augment Agent integration
- **License:** MIT License - see `LICENSE.md`

---

## 📝 Full Changelog

See [CHANGELOG.md](CHANGELOG.md) for complete version history.

---

**Previous Version:** v1.7.3 (2025-11-17)
**Next Version:** TBD

