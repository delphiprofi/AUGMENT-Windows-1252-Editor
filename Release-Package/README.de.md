# AUGMENT-Windows-1252-Editor
Dies ist ein Ersatz für den eingebauten str-replace-editor, wenn Sie mit Legacy-Code arbeiten, der nicht UTF-8 ist und nicht migriert werden kann!

**Version:** 1.9.3
**Letztes Update:** 2026-02-27
**Lizenz:** MIT License - siehe [LICENSE.md](LICENSE.md)

---

## 🚀 Schnellstart

```bash
# Lade die neueste Version herunter
# Extrahiere StrEditor.exe in ein Verzeichnis im PATH (z.B. C:\Delphi XE16\bin)

# String in einer Delphi-Datei ersetzen
StrEditor.exe --file "MyUnit.pas" --old-str "TObject" --new-str "TInterfacedObject"

# Text vor Zeile 10 einfügen
StrEditor.exe --file "MyUnit.pas" --text "  fNewField : string;" --insert-before-line 10

# Zeile löschen
StrEditor.exe --file "MyUnit.pas" --delete-line 25 --backup

# Dateiinhalt anzeigen (encoding-aware)
StrEditor.exe --file "MyUnit.pas" --show --head 20
```

**Für Augment Agent Benutzer:** StrEditor ersetzt automatisch den eingebauten `str-replace-editor`, wenn es im PATH installiert ist. Siehe [DOC/INTEGRATION.md](DOC/INTEGRATION.md) für Details.

**Für MCP-Benutzer:** Siehe [DOC/QUICKSTART.de.md](DOC/QUICKSTART.de.md) für den deutschen MCP-Schnellstart und [DOC/QUICKSTART.md](DOC/QUICKSTART.md) für die englische Version.

---

## 📝 Beschreibung

StrEditor ist ein Kommandozeilen-Tool zum Ersetzen und Einfügen von Text in Delphi-Quelldateien mit **Encoding-Preservation**.

### ✨ Hauptfeatures

- ✅ **WriteFile Retry**: Zuverlässiges Schreiben mit 3 Versuchen und 100ms Pause bei transienten Sperren **[BEHOBEN in v1.9.3]**
- ✅ **Tolerantes replace-line**: Teilt mehrzeiligen Text automatisch auf statt Datei zu beschädigen (mit stderr-Warnung) **[NEU in v1.9.3]**
- ✅ **JSON-Alias `"line"`**: Wird als Alias für `"insert-after-line"` akzeptiert (mit stderr-Warnung) **[NEU in v1.9.3]**
- ✅ **Datei-Zugriff Retry**: 3 Versuche mit 100ms Pause bei gesperrten Dateien **[NEU in v1.9.1]**
- ✅ **Dateivergleich**: Datei gegen Masterkopie auf kaputte Sonderzeichen prüfen (`--filecompare`) **[NEU in v1.8.7]**
- ✅ **Encoding-Preservation**: Windows-1252 und UTF-8 werden korrekt erkannt und erhalten
- ✅ **Encoding-Erkennung**: Datei-Encoding erkennen (`--detect-encoding`) **[NEU in v1.1]**
- ✅ **Encoding-Konvertierung**: Zwischen Windows-1252 und UTF-8 konvertieren (`--convert-encoding`) **[NEU in v1.3]**
- ✅ **Reinterpret Encoding**: Fehlerhafte Encodings reparieren (`--reinterpret-as`) **[NEU in v1.4]**
- ✅ **Base64-Parameter**: PowerShell-Sonderzeichen-Probleme lösen (`--old-str-base64`) **[NEU in v1.5]**
- ✅ **Multi-Line String Replace**: Strings über mehrere Zeilen ersetzen (`--multi-line`) **[NEU in v1.6]**
- ✅ **Zeilen-Manipulation**: Komplette Zeilen löschen und ersetzen (`--delete-line`, `--replace-line`) **[NEU in v1.7]**
- ✅ **JSON Config & Batch-Modus**: Mehrere Zeilen-Operationen aus JSON laden mit automatischer Sortierung **[NEU in v1.7.1]**
- ✅ **Insert Before Line**: Text vor einer bestimmten Zeile einfügen (`--insert-before-line`) **[NEU in v1.7.2]**
- ✅ **Dokumentations-Viewer**: Dokumentationsdateien anzeigen mit `--docs` Befehl **[NEU in v1.7.3]**
- ✅ **Umlaute reparieren**: Kaputte Umlaute automatisch reparieren (`--repair-umlauts`) **[NEU in v1.7.4]**
- ✅ **Zeilen verschieben**: Quellcode-Zeilen zwischen Dateien verschieben (`--move-lines`) **[NEU in v1.7.5]**
- ✅ **ChangeReport**: Zeigt was geändert wurde nach jeder Operation **[NEU in v1.8.4]**
- ✅ **SessionLog**: Protokolliert alle Operationen für Fehleranalyse **[NEU in v1.8.4]**
- ✅ **INI-Config**: ChangeReport und SessionLog via StrEditor.ini konfigurieren **[NEU in v1.8.4]**
- ✅ **Auto-Delete Config**: JSON-Config wird bei Erfolg automatisch gelöscht (mit `--keep-config` behalten) **[GEÄNDERT in v1.8.3]**
- ✅ **Zeilen einrücken**: Spaces am Zeilenanfang hinzufügen (`--indent-lines`) **[NEU in v1.8.2]**
- ✅ **Zeilen ausrücken**: Spaces am Zeilenanfang entfernen (`--unindent-lines`) **[NEU in v1.8.2]**
- ✅ **Hex-Dump Ausgabe**: Datei als Hex-Dump für Encoding-Debugging anzeigen (`--hex`) **[NEU in v1.8.1]**
- ✅ **Base64 Ausgabe**: Datei als Base64-String ausgeben (`--base64`) **[NEU in v1.8.1]**
- ✅ **Original-Zeilennummern**: JSON-Config Zeilennummern beziehen sich auf Original-Zustand **[NEU in v1.8.1]**
- ✅ **text-lines Array**: Mehrzeiliger Text in JSON ohne Escaping **[NEU in v1.8.0]**
- ✅ **replace-lines Befehl**: Zeilenbereiche atomar ersetzen in JSON **[NEU in v1.8.0]**
- ✅ **Kategorisierte Hilfe**: Kompakte Hilfe mit `--help [category]` **[NEU in v1.8.0]**
- ✅ **Parameter-Aliase**: Kurznamen: `--ib`, `--ia`, `--dl`, `--rl`, `--ob64`, `--nb64` **[NEU in v1.8.0]**
- ✅ **Show/Cat-Befehl**: Dateiinhalt mit Encoding-Awareness anzeigen (`--show`) **[NEU in v1.2]**
- ✅ **Umlaut-Support**: Deutsche Umlaute (ü, ö, ä, ß) werden korrekt behandelt
- ✅ **String-Replace**: Exakte String-Ersetzung mit optionalen Zeilenbereichen
- ✅ **Insert**: Text nach bestimmter Zeile einfügen
- ✅ **Regex-Replace**: Musterbasierte Ersetzung mit Capture Groups (`$1`, `$2`, etc.)
- ✅ **Macro-Expansion**: Variablen wie `{{LINE_NUMBER}}`, `{{FILE_NAME}}`, `{{DATE}}`, `{{TIME}}`
- ✅ **Conditional Replacements**: Nur in Zeilen ersetzen, die einem Muster entsprechen
- ✅ **Case Conversion**: Groß-/Kleinschreibung konvertieren (upper/lower/title)
- ✅ **Indent/Outdent**: Zeilen einrücken oder ausrücken
- ✅ **Backup & Undo**: Backups erstellen und wiederherstellen

---

## 🚀 Installation

### Option 1: Vorkompilierte Binary (empfohlen)

1. Download `StrEditor.exe` aus dem [Releases](https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/releases)-Bereich
2. Kopiere `StrEditor.exe` in ein Verzeichnis deiner Wahl (z.B. `C:\Tools\StrEditor\`)
3. Füge das Verzeichnis zum PATH hinzu (optional)

### Option 2: Aus Quellcode kompilieren

**Voraussetzungen:**
- Delphi 13 (XE16) oder höher
- DUnitX (für Tests)

**Build-Schritte:**
```bash
# Klone das Repository
git clone https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor.git
cd AUGMENT-Windows-1252-Editor

# Kompiliere mit MSBuild
msbuild StrEditor.dproj /p:Config=Release /p:Platform=Win32

# Oder öffne StrEditor.dproj in Delphi IDE und kompiliere
```

---

## 💡 Verwendung

### Grundlegende Syntax

```bash
StrEditor.exe --file <file> [options]
```

### Wichtigste Befehle

#### 1. String Replace
```bash
StrEditor.exe --file "MyUnit.pas" --old-str "fBar" --new-str "fMyBar"
```

#### 2. Multi-Line String Replace (NEU in v1.6)
```bash
StrEditor.exe --file "MyUnit.pas" --old-str "Line1\nLine2" --new-str "NewLine" --multi-line
```

#### 3. Insert Text
```bash
StrEditor.exe --file "MyUnit.pas" --text "  // TODO: Implement" --insert-after-line 10
```

#### 4. Regex Replace
```bash
StrEditor.exe --file "MyUnit.pas" --regex-pattern "f([A-Z]\w+)" --regex-replace "m$1"
```

#### 5. Show/Cat (NEU in v1.2)
```bash
StrEditor.exe --file "MyUnit.pas" --show
```

#### 6. Convert Encoding (NEU in v1.3)
```bash
StrEditor.exe --file "MyUnit.pas" --convert-encoding "UTF-8"
```

#### 7. Reinterpret Encoding (NEU in v1.4)
```bash
StrEditor.exe --file "MyUnit.pas" --reinterpret-as "Windows-1252"
```

#### 8. Base64-Parameter (NEU in v1.5)
```bash
StrEditor.exe --file "MyUnit.pas" --old-str-base64 "ZkJhcg==" --new-str "fMyBar"
```

---

## 📚 Dokumentation

Für vollständige Dokumentation siehe:
- [DOC/QUICKSTART.de.md](DOC/QUICKSTART.de.md) - MCP-Schnellstart (Deutsch)
- [DOC/QUICKSTART.md](DOC/QUICKSTART.md) - MCP quick start (English)
- [DOC/INTEGRATION.md](DOC/INTEGRATION.md) - Vollständiger Integrationsleitfaden
- [DOC/AUGMENT-RULES.md](DOC/AUGMENT-RULES.md) - Regeln für Augment Agent Integration
- [CHANGELOG.md](CHANGELOG.md) - Versionshistorie

---

## 🎯 Exit-Codes

| Code | Bedeutung |
|------|-----------|
| 0 | Erfolg |
| 1 | String nicht gefunden |
| 2 | Fehler (z.B. Datei nicht gefunden) |
| 10 | Ungültige Parameterkombination |

---

## 🧪 Testing

Das Projekt enthält umfangreiche Unit-Tests:

**Test-Ergebnisse (v1.8.4):**
- ✅ 191 Tests implementiert
- ✅ 191 Tests bestanden
- ✅ 0 Tests fehlgeschlagen

---

## 📁 Projekt-Struktur

```
DelphiStrEditor/
├── StrEditor.dpr                    # Hauptprogramm
├── StrEditor.dproj                  # Projekt-Datei
├── StrEditor.Encoding.pas           # Encoding-Support
├── StrEditor.Operations.pas         # String-Operationen
├── StrEditor.CommandLine.pas        # Command-Line Interface
├── StrEditor.CaseConversion.pas     # Case Conversion (v1.1)
├── StrEditor.Conditional.pas        # Conditional Replacements (v1.1)
├── StrEditor.Config.pas             # JSON Config Support (v1.1)
├── StrEditor.Indent.pas             # Indent/Outdent (v1.1)
├── StrEditor.Undo.pas               # Undo Support (v1.1)
├── Tests/
│   ├── Unittests.dpr                # Unit-Test-Programm
│   ├── Unittests.dproj              # Unit-Test-Projekt
│   ├── TestStrEditor.Encoding.pas   # Encoding-Tests
│   └── TestStrEditor.Operations.pas # Operations-Tests
├── DOC/
│   ├── INTEGRATION.md               # Vollständiger Integrationsleitfaden
│   └── AUGMENT-RULES.md             # Augment Agent Regeln
├── CHANGELOG.md                     # Versionshistorie
└── README.md                        # Diese Datei (English)
└── README.de.md                     # Diese Datei (Deutsch)
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

© 2025 Frank Lauter ( DelphiProfi ) 
http://www.delphiprofi.de

---

## 📄 Lizenz

MIT License - Copyright (c) 2025 Frank Lauter (DelphiProfi)

Siehe [LICENSE.md](LICENSE.md) für den vollständigen Lizenztext.

---

## 🤝 Kontakt

Bei Fragen oder Problemen bitte ein Issue auf GitHub erstellen:
https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/issues

**Autor:** Frank Lauter (DelphiProfi)
**Website:** http://www.delphiprofi.com

