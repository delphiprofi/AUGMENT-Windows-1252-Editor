# StrEditor - Delphi String Replace Tool

**Version:** 1.0.0  
**Build:** 2025-11-09  
**Autor:** Frank Lauter

---

## 📝 Beschreibung

StrEditor ist ein Kommandozeilen-Tool zum Ersetzen und Einfügen von Text in Delphi-Quelldateien mit **Encoding-Preservation**.

### ✨ Hauptfeatures

- ✅ **Encoding-Preservation**: Windows-1252 und UTF-8 werden korrekt erkannt und erhalten
- ✅ **Umlaut-Support**: Deutsche Umlaute (ü, ö, ä, ß) werden korrekt behandelt
- ✅ **String-Replace**: Exakte String-Ersetzung mit optionalen Zeilenbereichen
- ✅ **Insert**: Text nach bestimmter Zeile einfügen
- ✅ **Regex-Replace**: Pattern-basierte Ersetzung mit Capture Groups (`$1`, `$2`, etc.)
- ✅ **Regex-Test**: Regex-Patterns testen ohne Änderungen
- ✅ **Macro-Expansion**: Variablen wie `{{LINE_NUMBER}}`, `{{FILE_NAME}}`, `{{DATE}}`, `{{TIME}}`
- ✅ **Line-Ending-Preservation**: CRLF (Windows) wird beibehalten
- ✅ **Exit-Codes**: Klare Exit-Codes für Automatisierung
- ✅ **Verbose Mode**: Detaillierte Ausgaben für Debugging

---

## 🚀 Installation

1. Kompiliere das Projekt mit Delphi 13 (XE16):
   ```bash
   Build-StrEditor.bat
   ```

2. Die ausführbare Datei befindet sich in:
   ```
   Win32\AI\StrEditor.exe
   ```

---

## 📖 Verwendung

### Hilfe anzeigen

```bash
StrEditor.exe --help
```

### Version anzeigen

```bash
StrEditor.exe --version
```

### String ersetzen

```bash
StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL"
```

### String ersetzen mit Zeilenbereich

```bash
StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL" --start-line 10 --end-line 20
```

### Text einfügen

```bash
StrEditor.exe --file "test.pas" --text "// Comment" --insert-after-line 10
```

### Verbose Mode

```bash
StrEditor.exe --file "test.pas" --old-str "nil" --new-str "NIL" --verbose
```

### Regex Replace

```bash
# Einfaches Regex Replace
StrEditor.exe --file "test.pas" --regex-pattern "f(\w+)" --regex-replace 'l$1' --verbose

# Case-insensitive
StrEditor.exe --file "test.pas" --regex-pattern "procedure" --regex-replace "function" -i

# Multi-line
StrEditor.exe --file "test.pas" --regex-pattern "begin.*end" --regex-replace "start.*stop" -m
```

**WICHTIG für PowerShell:** Verwende **einfache Anführungszeichen** `'...'` für `--regex-replace`, wenn Capture Groups (`$1`, `$2`, etc.) verwendet werden! PowerShell interpretiert `$1` in doppelten Anführungszeichen als Variable.

### Regex Test

```bash
# Pattern testen ohne Änderungen
StrEditor.exe --file "test.pas" --regex-pattern "f(\w+)" --regex-test --verbose
```

### Macro-Expansion

```bash
# Makros in Replacement-Strings
StrEditor.exe --file "test.pas" --old-str "interface" --new-str "interface // Line {{LINE_NUMBER}} in {{FILE_NAME}}" --verbose

# Makros mit Regex und Capture Groups
StrEditor.exe --file "test.pas" --regex-pattern "f(\w+)" --regex-replace 'l$1 // Line {{LINE_NUMBER}} - {{DATE}}' --verbose
```

**Verfügbare Makros:**
- `{{LINE_NUMBER}}` - Aktuelle Zeilennummer
- `{{FILE_NAME}}` - Dateiname (ohne Pfad)
- `{{DATE}}` - Aktuelles Datum (yyyy-mm-dd)
- `{{TIME}}` - Aktuelle Uhrzeit (hh:nn:ss)

---

## 🔧 Parameter

| Parameter | Beschreibung | Erforderlich |
|-----------|--------------|--------------|
| `--file <file>` | Datei zum Bearbeiten | Ja |
| `--old-str <old>` | String zum Ersetzen | Ja (für Replace) |
| `--new-str <new>` | Ersetzungs-String | Nein (leer = löschen) |
| `--start-line <n>` | Start-Zeile für Ersetzung | Nein |
| `--end-line <n>` | End-Zeile für Ersetzung | Nein |
| `--text <text>` | Text zum Einfügen | Ja (für Insert) |
| `--insert-after-line <n>` | Zeile nach der eingefügt wird | Ja (für Insert) |
| `--regex-pattern <pattern>` | Regex-Pattern | Ja (für Regex) |
| `--regex-replace <replacement>` | Ersetzungs-String mit Capture Groups | Ja (für Regex Replace) |
| `--regex-test` | Pattern testen ohne Änderungen | Nein |
| `--case-insensitive, -i` | Case-insensitive Regex | Nein |
| `--multiline, -m` | Multi-line Regex | Nein |
| `--verbose` | Detaillierte Ausgaben | Nein |
| `--help, -h` | Hilfe anzeigen | Nein |
| `--version, -v` | Version anzeigen | Nein |

---

## 🎯 Exit-Codes

| Code | Bedeutung |
|------|-----------|
| 0 | Erfolg |
| 1 | Datei nicht gefunden |
| 2 | String nicht gefunden |
| 3 | Encoding-Fehler |
| 4 | Parameter-Fehler |

---

## 📊 Beispiele

### Beispiel 1: Parameter-Präfix ändern

```bash
# Ändere alle "fBar" zu "fMyBar"
StrEditor.exe --file "MyUnit.pas" --old-str "fBar" --new-str "fMyBar"
```

### Beispiel 2: Keywords case-fixen

```bash
# Ändere "nil" zu "NIL" nur in Zeilen 50-100
StrEditor.exe --file "MyUnit.pas" --old-str "nil" --new-str "NIL" --start-line 50 --end-line 100
```

### Beispiel 3: Kommentar einfügen

```bash
# Füge Kommentar nach Zeile 10 ein
StrEditor.exe --file "MyUnit.pas" --text "  // TODO: Implement this" --insert-after-line 10
```

### Beispiel 4: Text löschen

```bash
# Lösche " with text" aus allen Zeilen
StrEditor.exe --file "MyUnit.pas" --old-str " with text" --new-str ""
```

---

## 🧪 Testing

Das Projekt enthält umfangreiche Unit-Tests:

```bash
Build-Unittests.bat
Tests\Win32\AI\Unittests.exe
```

**Test-Ergebnisse:**
- ✅ 20 Tests implementiert
- ✅ 20 Tests bestanden
- ✅ 0 Tests fehlgeschlagen

---

## 🏗️ Build-System

### Build-Batches

- `Build-StrEditor.bat`: Baut Hauptprojekt
- `Build-Unittests.bat`: Baut Unittests
- `Build-All.bat`: Baut beide Projekte

### Konfiguration

- **Delphi Version**: Delphi 13 (XE16)
- **Build-Konfiguration**: AI (ohne TESTINSIGHT)
- **Output-Pfad**: `Win32\AI`

---

## 📁 Projekt-Struktur

```
DelphiStrEditor/
├── StrEditor.dpr                    # Hauptprogramm
├── StrEditor.dproj                  # Projekt-Datei
├── StrEditor.Encoding.pas           # Encoding-Support
├── StrEditor.Operations.pas         # String-Operationen
├── StrEditor.CommandLine.pas        # Command-Line Interface
├── Tests/
│   ├── Unittests.dpr                # Unittest-Programm
│   ├── Unittests.dproj              # Unittest-Projekt
│   ├── TestStrEditor.Encoding.pas   # Encoding-Tests
│   └── TestStrEditor.Operations.pas # Operations-Tests
├── Build-StrEditor.bat              # Build-Batch Hauptprojekt
├── Build-Unittests.bat              # Build-Batch Unittests
├── Build-All.bat                    # Build-Batch Alle
├── AUGMENT/
│   ├── DelphiStrReplace-Specification.md
│   ├── DelphiStrReplace-Tasks.md
│   └── Implementation-Summary.md
└── README.md                        # Diese Datei
```

---

## 🔒 Encoding-Details

### Windows-1252

- **Erkennung**: Keine BOM vorhanden
- **Umlaute**: ü=$FC, ö=$F6, ä=$E4, ß=$DF, Ü=$DC, Ö=$D6, Ä=$C4
- **Konvertierung**: WinAPI `MultiByteToWideChar` / `WideCharToMultiByte` mit Code Page 1252

### UTF-8

- **Erkennung**: BOM vorhanden ($EF $BB $BF)
- **Konvertierung**: Delphi `UTF8Encode` / `UTF8ToUnicodeString`

---

## 📄 Lizenz

© 2025 Frank Lauter

---

## 🤝 Kontakt

Bei Fragen oder Problemen bitte ein Issue erstellen.

