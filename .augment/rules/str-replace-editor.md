---
type: "agent_requested"
description: "Example description"
---

## 🔧 StrEditor Integration für Delphi-Dateien

**Version:** 1.7.2 (2025-11-12)

### Wichtig: Verwende IMMER StrEditor statt str-replace-editor

Für die Bearbeitung von Delphi-Dateien (`.pas`, `.dpr`, `.dpk`, `.dfm`, `.fmx`) verwende **AUSSCHLIESSLICH** das Tool `StrEditor.exe`.

**Pfad:** `C:\Delphi XE16\bin\StrEditor.exe` (liegt im PATH, kann ohne Pfad aufgerufen werden)

**Vollständige Dokumentation:** `C:\Delphi XE16\bin\AUGMENT-INTEGRATION.md`

---

### Grundlegende Verwendung

**Einfacher String Replace:**
```
StrEditor.exe --file "MyUnit.pas" --old-str "TObject" --new-str "TInterfacedObject"
```

**String Replace mit Zeilenbereich:**
```
StrEditor.exe --file "MyUnit.pas" --old-str "Integer" --new-str "Int64" --start-line 10 --end-line 50
```

**Insert nach bestimmter Zeile:**
```
StrEditor.exe --file "MyUnit.pas" --text "  fNewField : string;" --insert-after-line 20
```

**Insert vor bestimmter Zeile:** 🆕 **[NEU in v1.7.2]**
```
# Insert vor Zeile 1 (am Anfang der Datei)
StrEditor.exe --file "MyUnit.pas" --text "// Header Comment" --insert-before-line 1

# Insert vor Zeile 10
StrEditor.exe --file "MyUnit.pas" --text "  fNewField : string;" --insert-before-line 10
```

**Regex Replace mit Capture Groups:**
```
StrEditor.exe --file "MyUnit.pas" --regex-pattern "function\s+(\w+)\(\s*a\s*:" --regex-replace "function $1( aValue :"
```

**Conditional Replace (nur in Zeilen mit bestimmtem Muster):**
```
StrEditor.exe --file "MyUnit.pas" --old-str "Integer" --new-str "Int64" --condition-pattern "function"
```

**Encoding Detection:** 🆕 **[NEU in v1.1]**
```
StrEditor.exe --file "MyUnit.pas" --detect-encoding --verbose
```
- Zeigt das Encoding der Datei an (Windows-1252 oder UTF-8 with BOM)
- Mit `--verbose` werden zusätzliche Details angezeigt (Dateigröße, erste 3 Bytes, BOM-Status)
- Perfekt für Debugging von Encoding-Problemen

**Verbose Debug-Ausgabe:** 🔧 **[VERBESSERT in v1.1]**
```
StrEditor.exe --file "MyUnit.pas" --old-str "Integer" --new-str "Int64" --verbose --dry-run
```
- Zeigt alle geprüften Zeilen
- Zeigt Match-Status für jede Zeile (MATCH found / No match)
- Zeigt Position des Matches
- Perfekt für Debugging von String-Not-Found-Problemen

**Convert Encoding (Encoding konvertieren):** 🆕 **[NEU in v1.3]**
```
# UTF-8 → Windows-1252 (mit Backup)
StrEditor.exe --file "MyUnit.pas" --convert-encoding --to windows1252 --backup --verbose

# Windows-1252 → UTF-8 (mit Backup)
StrEditor.exe --file "MyUnit.pas" --convert-encoding --to utf8 --backup

# Dry-Run (Test ohne Änderung)
StrEditor.exe --file "MyUnit.pas" --convert-encoding --to windows1252 --dry-run --verbose
```
- Konvertiert zwischen UTF-8 (mit BOM) und Windows-1252 (ohne BOM)
- `--backup` erstellt automatisch `.bak` Datei
- `--dry-run` zeigt nur, was geändert würde
- Umlaute werden korrekt konvertiert (ä, ö, ü, ß, Ä, Ö, Ü)
- Byte-identische Rück-Konvertierung garantiert

**Reinterpret Encoding (Kaputte Encodings reparieren):** 🆕 **[NEU in v1.4]**
```
# UTF-8 Bytes in Windows-1252 Datei reparieren (Ã¤ → ä)
StrEditor.exe --file "MyUnit.pas" --reinterpret-as utf8 --backup --verbose

# Windows-1252 Bytes in UTF-8 Datei reparieren
StrEditor.exe --file "MyUnit.pas" --reinterpret-as windows1252 --backup --verbose

# Dry-Run (Test ohne Änderung)
StrEditor.exe --file "MyUnit.pas" --reinterpret-as utf8 --dry-run --verbose
```
- Repariert kaputte Encodings (z.B. UTF-8 Bytes in Windows-1252 Dateien)
- Liest Datei als angegebenes Encoding und speichert als Ziel-Encoding
- Unterstützt alle Sonderzeichen: ä ö ü ß Ä Ö Ü é © und mehr
- `--backup` erstellt automatisch `.bak` Datei
- `--dry-run` zeigt nur, was geändert würde
- **Anwendungsfall:** Datei wurde mit falschem Encoding gespeichert und zeigt jetzt `Ã¤` statt `ä`

**Base64-Parameter (PowerShell Sonderzeichen-Probleme lösen):** 🆕 **[NEU in v1.5]**
```
# String mit Dollar-Zeichen ersetzen (PowerShell-sicher)
StrEditor.exe --file "MyUnit.pas" --old-str-base64 "ICAgICB7JElGREVGIFBSRVVOSUNPREV9" --new-str-base64 "ICAgICB7JElGREVGIFVOSUNPREV9" --verbose

# Text mit Dollar-Zeichen einfügen (PowerShell-sicher)
StrEditor.exe --file "MyUnit.pas" --text-base64 "eyRJRkRFRiBERUJVR30=" --insert-after-line 10 --verbose

# Base64-Encoding in PowerShell:
$text = "     {$IFDEF PREUNICODE}"
$bytes = [System.Text.Encoding]::UTF8.GetBytes($text)
$base64 = [Convert]::ToBase64String($bytes)
Write-Host $base64
```
- Löst PowerShell-Probleme mit Sonderzeichen (Dollar-Zeichen, Backtick, Anfuehrungszeichen, At-Zeichen, Pipe, Ampersand)
- Parameter werden als Base64-String übergeben und automatisch dekodiert
- Unterstützt: `--old-str-base64`, `--new-str-base64`, `--text-base64`
- **Anwendungsfall:** String enthält Dollar-Zeichen (z.B. `{$IFDEF}`), Backtick, Anfuehrungszeichen oder andere PowerShell-Sonderzeichen
- **Alternative:** JSON-Config-Datei verwenden (siehe unten)

**Show/Cat (Datei anzeigen):** 🆕 **[NEU in v1.2]**
```
# Ganze Datei anzeigen (encoding-aware!)
StrEditor.exe --file "MyUnit.pas" --show

# Erste 10 Zeilen mit Zeilennummern
StrEditor.exe --file "MyUnit.pas" --show --head 10 --line-numbers

# Letzte 5 Zeilen
StrEditor.exe --file "MyUnit.pas" --show --tail 5

# Zeilenbereich (Zeilen 10-20)
StrEditor.exe --file "MyUnit.pas" --show --start-line 10 --end-line 20

# Mit Encoding-Info
StrEditor.exe --file "MyUnit.pas" --show --verbose
```
- **WICHTIG:** Verwende `--show` statt PowerShell `Get-Content` für Delphi-Dateien!
- Umlaute werden korrekt angezeigt (Windows-1252 und UTF-8)
- Keine PowerShell Code Page Probleme
- Aliases: `--cat`, `--first`, `--last`, `--total-count`

**Multi-Line String Replace (mehrzeilige Strings ersetzen):** 🆕 **[NEU in v1.6]**
```
# IFDEF-Block entfernen (5 Zeilen → 1 Zeile)
StrEditor.exe --file "MyUnit.pas" --old-str-base64 "ICAgICAgeyRJRkRFRiBQUkVVTklDT0RFfQ0K..." --new-str-base64 "ICAgICAgV1cgOj0gTXlXaWRlQ2FudmFzVGV4dFdpZHRo..." --multi-line --verbose

# Alle Vorkommen ersetzen
StrEditor.exe --file "MyUnit.pas" --old-str "Line 1\r\nLine 2" --new-str "New Line" --multi-line --replace-all

# Dry-Run (Test ohne Änderung)
StrEditor.exe --file "MyUnit.pas" --old-str-base64 "..." --new-str-base64 "..." --multi-line --dry-run --verbose
```
- **WICHTIG:** Mit `--multi-line` wird der gesamte Datei-Inhalt als ein String behandelt
- Ermöglicht das Ersetzen von Strings über mehrere Zeilen hinweg
- Perfekt für IFDEF-Block-Entfernung oder Refactoring von mehrzeiligen Konstrukten
- `--replace-all` ersetzt alle Vorkommen (Standard: nur erstes Vorkommen)
- **Einschränkung:** `--multi-line` kann nicht mit `--condition-pattern` kombiniert werden (Exit-Code 10)
- **Tipp:** Verwende Base64-Parameter für Strings mit `#13#10` (CRLF)

**Exit-Codes für Multi-Line:**
- `0` - Erfolg (String gefunden und ersetzt)
- `1` - String nicht gefunden
- `2` - Fehler (z.B. Datei nicht gefunden)
- `10` - Ungültige Parameter-Kombination (z.B. `--multi-line` + `--condition-pattern`)

**Line Manipulation (Zeilen löschen und ersetzen):** 🆕 **[NEU in v1.7]**
```
# Einzelne Zeile löschen
StrEditor.exe --file "MyUnit.pas" --delete-line 25 --backup

# Mehrere Zeilen löschen (Liste)
StrEditor.exe --file "MyUnit.pas" --delete-lines "1,3,5,7" --backup

# Zeilenbereich löschen
StrEditor.exe --file "MyUnit.pas" --delete-lines --start-line 10 --end-line 20 --backup

# Zeile ersetzen
StrEditor.exe --file "MyUnit.pas" --replace-line 25 --with "  WriteLn('New');" --backup

# Zeile ersetzen mit Base64 (für Sonderzeichen)
StrEditor.exe --file "MyUnit.pas" --replace-line 25 --with-base64 "ICBXcml0ZUxuKCdOZXcnKTs=" --backup
```
- **WICHTIG:** Zeilennummern sind 1-basiert (erste Zeile = 1)
- Perfekt für präzise Änderungen an bekannten Zeilen
- `--backup` erstellt automatisch `.bak` Datei
- `--dry-run` zeigt nur, was geändert würde
- `--diff` zeigt Unterschiede vor/nach der Änderung

**JSON Config & Batch Mode (mehrere Operationen):** 🆕 **[NEU in v1.7.1]**
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
      "line": 50,
      "text": "  WriteLn('New Line');"
    },
    {
      "command": "insert-before",
      "file": "MyUnit.pas",
      "insert-before-line": 1,
      "text": "// Header Comment"
    }
  ]
}
```
```
# JSON Config ausführen
StrEditor.exe --config operations.json --verbose
```
- **Automatische Sortierung:** Operationen werden von höchster zu niedrigster Zeile ausgeführt
- **Verhindert Index-Shifting:** Keine manuellen Zeilennummern-Anpassungen nötig
- **Gemischte Operationen:** `delete-line`, `delete-lines`, `replace-line`, `insert-before` kombinierbar
- **Batch-Modus:** Mehrere Dateien und Operationen in einem Aufruf

---

### Wichtigste Regeln für Augment Agent

1. **🚫 NIEMALS PowerShell Get-Content für Delphi-Dateien verwenden!** 🆕 **[v1.2]**
   - ❌ DON'T: `Get-Content "MyUnit.pas"`
   - ❌ DON'T: `Get-Content "MyUnit.pas" -TotalCount 10`
   - ❌ DON'T: `Get-Content "MyUnit.pas" -Tail 10`
   - ✅ DO: `StrEditor.exe --file "MyUnit.pas" --show`
   - ✅ DO: `StrEditor.exe --file "MyUnit.pas" --show --head 10`
   - ✅ DO: `StrEditor.exe --file "MyUnit.pas" --show --tail 10`
   - **Grund:** PowerShell zeigt Umlaute in Windows-1252 Dateien falsch an (`�` statt `ü`)
   - **Lösung:** StrEditor ist encoding-aware und zeigt Umlaute korrekt an

2. **Pfade IMMER in Anführungszeichen einschließen**
   - ✅ DO: `StrEditor.exe --file "C:\My Projects\MyUnit.pas"`
   - ❌ DON'T: `StrEditor.exe --file C:\My Projects\MyUnit.pas`

3. **Encoding wird automatisch erkannt**
   - Windows-1252 (ohne BOM) und UTF-8 (mit BOM) werden automatisch erkannt
   - Umlaute (ä, ö, ü, ß) werden automatisch korrekt behandelt
   - Keine manuellen Encoding-Parameter erforderlich

4. **Line-Endings (CRLF) werden automatisch erhalten**
   - Windows Line-Endings (CR+LF) werden automatisch preserviert
   - Keine manuellen Line-Ending-Parameter erforderlich

5. **Exit-Code IMMER prüfen**
   - Exit-Code 0 = Erfolg
   - Exit-Code != 0 = Fehler (siehe Dokumentation für Details)
   - Bei Fehler: Fehler an Benutzer melden

6. **Bei komplexen Operationen: Dry-Run verwenden**
   - Erst mit `--dry-run` testen, dann ohne ausführen
   - Beispiel: `StrEditor.exe --file "MyUnit.pas" --old-str "Integer" --new-str "Int64" --dry-run`

7. **Bei kritischen Dateien: Backup verwenden**
   - Mit `--backup` wird automatisch ein Backup erstellt
   - Mit `--undo` kann das Backup wiederhergestellt werden
   - Beispiel: `StrEditor.exe --file "MyUnit.pas" --old-str "Integer" --new-str "Int64" --backup`

8. **Bei Problemen: Verbose verwenden** 🔧 **[VERBESSERT in v1.1]**
   - Mit `--verbose` werden detaillierte Informationen angezeigt
   - Zeigt alle geprüften Zeilen und Match-Status
   - Zeigt Position des Matches
   - Perfekt für Debugging von String-Not-Found-Problemen
   - Beispiel: `StrEditor.exe --file "MyUnit.pas" --old-str "Integer" --new-str "Int64" --verbose`

9. **Bei Encoding-Problemen: Encoding Detection verwenden** 🆕 **[NEU in v1.1]**
   - Mit `--detect-encoding` wird das Encoding der Datei angezeigt
   - Mit `--verbose` werden zusätzliche Details angezeigt
   - Beispiel: `StrEditor.exe --file "MyUnit.pas" --detect-encoding --verbose`

---

### Erweiterte Features

**Batch Processing (mehrere Dateien):**
```
StrEditor.exe --files "Unit1.pas,Unit2.pas,Unit3.pas" --old-str "TObject" --new-str "TInterfacedObject"
```

**Multiple Operations via JSON Config:**
```json
{
  "file": "MyUnit.pas",
  "operations": [
    {"old_str": "TObject", "new_str": "TInterfacedObject"},
    {"old_str": "Integer", "new_str": "Int64", "start_line": 10, "end_line": 50}
  ]
}
```
```
StrEditor.exe --config "operations.json"
```

**Case Conversion:**
```
StrEditor.exe --file "MyUnit.pas" --old-str "interface" --new-str "interface" --case upper
```

**Indent/Outdent:**
```
StrEditor.exe --file "MyUnit.pas" --start-line 10 --end-line 20 --indent 2
```

---

### Workflow-Empfehlung für Augment Agent

**Für einfache Operationen:**
1. Führe Operation direkt aus
2. Prüfe Exit-Code
3. Bei Fehler: Melde Fehler an Benutzer

**Für komplexe Operationen:**
1. Führe Operation mit `--dry-run` aus
2. Prüfe Ausgabe
3. Führe Operation ohne `--dry-run` aus (mit `--backup`)
4. Prüfe Exit-Code
5. Bei Fehler: Verwende `--undo` und melde Fehler an Benutzer

**Für kritische Dateien:**
1. Führe Operation mit `--backup` aus
2. Prüfe Exit-Code
3. Bei Fehler: Verwende `--undo` und melde Fehler an Benutzer

**Für Strings mit PowerShell-Sonderzeichen:** 🆕 **[NEU in v1.5]**
1. **Prüfe, ob String Sonderzeichen enthält:**
   - Dollar-Zeichen (z.B. `{$IFDEF}`, `$Variable`)
   - Backtick (z.B. `` `Test` ``)
   - Anfuehrungszeichen (z.B. `"Text"`)
   - At-Zeichen (z.B. `@Array`, `test@example.com`)
   - Pipe (z.B. `A | B`)
   - Ampersand (z.B. `A & B`)
2. **Wenn Sonderzeichen vorhanden:**
   - **Option A:** Verwende Base64-Parameter (`--old-str-base64`, `--new-str-base64`, `--text-base64`)
   - **Option B:** Verwende JSON-Config-Datei (`--config`)
3. **Wenn keine Sonderzeichen:**
   - Verwende normale Parameter (`--old-str`, `--new-str`, `--text`)

---

### Wichtige Einschränkungen

1. **Regex ist Single-Line**
   - Regex-Patterns matchen nur innerhalb einer Zeile
   - Workaround: Verwende mehrere Regex-Operationen für jede Zeile

3. **Backup überschreibt vorheriges Backup**
   - Nur das letzte Backup wird gespeichert
   - Workaround: Verwende externes Backup-System (z.B. Mercurial)

---

### Checkliste vor jeder Verwendung

- [ ] Pfad in Anführungszeichen?
- [ ] Encoding-Awareness berücksichtigt?
- [ ] Line-Ending-Preservation berücksichtigt?
- [ ] Exit-Code wird geprüft?
- [ ] Bei komplexen Operationen: Dry-Run verwendet?
- [ ] Bei kritischen Dateien: Backup verwendet?
- [ ] Bei Problemen: Verbose verwendet?
- [ ] Bei Encoding-Problemen: Encoding Detection verwendet? 🆕 **[v1.1]**
- [ ] Zum Lesen von Dateien: `--show` statt `Get-Content` verwendet? 🆕 **[v1.2]**

---

**Vollständige Dokumentation mit allen Features, Beispielen und Troubleshooting:**
`C:\Delphi XE16\bin\AUGMENT-INTEGRATION.md`

---

## 🆕 Neue Features in Version 1.1 (2025-11-09)

### 1. Encoding Detection
**Problem:** String wird nicht gefunden, obwohl er in der Datei existiert
**Lösung:** Prüfe zuerst das Encoding der Datei

```bash
# Schritt 1: Encoding prüfen
StrEditor.exe --file "MyUnit.pas" --detect-encoding --verbose

# Schritt 2: String Replace mit Verbose
StrEditor.exe --file "MyUnit.pas" --old-str "Integer" --new-str "Int64" --verbose --dry-run
```

**Ausgabe Encoding Detection:**
```
File: MyUnit.pas
Encoding: Windows-1252 (no BOM)

--- Details ---
File size: 12824 bytes
First 3 bytes (hex): 70 72 6F
No BOM detected (Windows-1252)
```

### 2. Verbesserte Verbose-Ausgabe
**Problem:** String wird nicht gefunden, aber du weißt nicht warum
**Lösung:** Verwende `--verbose` bei String Replace

```bash
StrEditor.exe --file "MyUnit.pas" --old-str "Integer" --new-str "Int64" --verbose --dry-run
```

**Ausgabe:**
```
Searching for: "Integer"
In lines 1 to 100 (100 lines)

Line 1: "Unit MyUnit;"
  -> No match
Line 2: "Var i : Integer;"
  -> MATCH found at position 9
Line 3: "begin"
  -> No match
...
```

**Vorteile:**
- ✅ Zeigt alle geprüften Zeilen
- ✅ Zeigt Match-Status für jede Zeile
- ✅ Zeigt Position des Matches
- ✅ Perfekt für Debugging

### 3. Workflow für Encoding-Probleme

**Wenn String nicht gefunden wird:**
1. Prüfe Encoding: `StrEditor.exe --file "MyUnit.pas" --detect-encoding --verbose`
2. Teste mit Verbose: `StrEditor.exe --file "MyUnit.pas" --old-str "..." --new-str "..." --verbose --dry-run`
3. Prüfe Ausgabe: Welche Zeilen werden geprüft? Wird der String gefunden?
4. Bei Erfolg: Führe ohne `--dry-run` aus (mit `--backup`)

---

## 🆕 Neue Features in Version 1.2 (2025-11-10)

### 1. Show/Cat Command (Encoding-aware File Display)

**Problem:** PowerShell `Get-Content` zeigt Umlaute in Windows-1252 Dateien falsch an (`�` statt `ü`)
**Lösung:** Verwende `StrEditor.exe --show` statt `Get-Content`

#### **Basis-Verwendung:**
```bash
# Ganze Datei anzeigen
StrEditor.exe --file "MyUnit.pas" --show

# Alias: --cat
StrEditor.exe --file "MyUnit.pas" --cat
```

#### **Head (Erste N Zeilen):**
```bash
# Erste 10 Zeilen
StrEditor.exe --file "MyUnit.pas" --show --head 10

# Aliases: --first, --total-count
StrEditor.exe --file "MyUnit.pas" --show --first 10
StrEditor.exe --file "MyUnit.pas" --show --total-count 10
```

#### **Tail (Letzte N Zeilen):**
```bash
# Letzte 10 Zeilen
StrEditor.exe --file "MyUnit.pas" --show --tail 10

# Alias: --last
StrEditor.exe --file "MyUnit.pas" --show --last 10
```

#### **Zeilenbereich:**
```bash
# Zeilen 10-20
StrEditor.exe --file "MyUnit.pas" --show --start-line 10 --end-line 20
```

#### **Mit Zeilennummern:**
```bash
StrEditor.exe --file "MyUnit.pas" --show --head 10 --line-numbers
```

#### **Raw Output (ein String):**
```bash
StrEditor.exe --file "MyUnit.pas" --show --head 3 --raw
```

#### **Mit Encoding-Info:**
```bash
StrEditor.exe --file "MyUnit.pas" --show --verbose
```

**Ausgabe:**
```
File: MyUnit.pas
Encoding: Windows-1252 (no BOM)
Total lines: 150

Unit MyUnit;
...
```

### 2. PowerShell Get-Content Replacement

**WICHTIG:** Verwende `--show` statt PowerShell `Get-Content` für Delphi-Dateien!

| PowerShell | StrEditor |
|------------|-----------|
| `Get-Content "file.pas"` | `StrEditor.exe --file "file.pas" --show` |
| `Get-Content -TotalCount 10` | `StrEditor.exe --show --head 10` |
| `Get-Content -Tail 10` | `StrEditor.exe --show --tail 10` |
| `Get-Content -Raw` | `StrEditor.exe --show --raw` |

**Vorteile:**
- ✅ Umlaute werden korrekt angezeigt (Windows-1252 und UTF-8)
- ✅ Keine PowerShell Code Page Probleme
- ✅ Encoding-aware (automatische Erkennung)
- ✅ Konsistent mit StrEditor Replace/Insert

### 3. Workflow für Datei-Inspektion

**Wenn du eine Delphi-Datei lesen möchtest:**
1. Verwende `--show` statt `Get-Content`
2. Bei Bedarf: `--verbose` für Encoding-Info
3. Bei Bedarf: `--line-numbers` für Zeilennummern
4. Bei Bedarf: `--head` oder `--tail` für Ausschnitte

**Beispiel:**
```bash
# Schritt 1: Encoding prüfen
StrEditor.exe --file "MyUnit.pas" --show --verbose --head 0

# Schritt 2: Erste 20 Zeilen mit Zeilennummern
StrEditor.exe --file "MyUnit.pas" --show --head 20 --line-numbers

# Schritt 3: Bestimmten Bereich anzeigen
StrEditor.exe --file "MyUnit.pas" --show --start-line 50 --end-line 70
```

---

## 🆕 Neue Features in Version 1.6 (2025-11-11)

### 1. Multi-Line String Replace (Mehrzeilige Strings ersetzen)

**Problem:** Standardmäßig arbeitet StrEditor zeilenweise und kann keine mehrzeiligen Strings ersetzen.
**Lösung:** Mit `--multi-line` wird der gesamte Datei-Inhalt als ein String behandelt.

#### **Basis-Verwendung:**
```bash
# Einfacher Multi-Line Replace
StrEditor.exe --file "MyUnit.pas" --old-str "Line 1\r\nLine 2" --new-str "New Line" --multi-line

# Mit Base64 (empfohlen für CRLF)
StrEditor.exe --file "MyUnit.pas" --old-str-base64 "TGluZSAxDQpMaW5lIDI=" --new-str "New Line" --multi-line
```

#### **IFDEF-Block entfernen (5 Zeilen → 1 Zeile):**
```bash
# Beispiel: IFDEF-Block entfernen
StrEditor.exe --file "MyUnit.pas" \
  --old-str-base64 "ICAgICAgeyRJRkRFRiBQUkVVTklDT0RFfQ0KICAgICAgV1cgOj0gTXlXaWRlQ2FudmFzVGV4dFdpZHRoKFNvcnRHcmlkMS5jYW52YXMsQW5zaU9yVVRGOCgoQVtpaWldKycuLicpKSk7DQogICAgICB7JEVMU0V9DQogICAgICBXVyA6PSBTb3J0Z3JpZDEuY2FudmFzLnRleHRXaWR0aChBW2lpaV0rJy4uJyk7DQogICAgICB7JEVORElGfQ==" \
  --new-str-base64 "ICAgICAgV1cgOj0gTXlXaWRlQ2FudmFzVGV4dFdpZHRoKFNvcnRHcmlkMS5jYW52YXMsQW5zaU9yVVRGOCgoQVtpaWldKycuLicpKSk7" \
  --multi-line \
  --verbose
```

#### **Alle Vorkommen ersetzen:**
```bash
# Ersetzt alle Vorkommen des mehrzeiligen Strings
StrEditor.exe --file "MyUnit.pas" --old-str "Line 1\r\nLine 2" --new-str "New Line" --multi-line --replace-all
```

#### **Dry-Run (Test ohne Änderung):**
```bash
StrEditor.exe --file "MyUnit.pas" --old-str-base64 "..." --new-str-base64 "..." --multi-line --dry-run --verbose
```

#### **Mit JSON-Config:**

**Beispiel 1: Einfacher Multi-Line Replace**
```json
{
  "file": "MyUnit.pas",
  "old-str": "Line 1\r\nLine 2",
  "new-str": "New Line",
  "multiline": true,
  "verbose": true
}
```

**Beispiel 2: Multi-Line Replace mit Replace-All**
```json
{
  "file": "MyUnit.pas",
  "old-str": "AAA\r\nBBB",
  "new-str": "XXX",
  "multiline": true,
  "replace-all": true,
  "verbose": true
}
```

**Beispiel 3: Multi-Line Replace mit Base64** 🆕 **[NEU in v1.6]**
```json
{
  "file": "MyUnit.pas",
  "old-str": "QUFBDQpCQkI=",
  "new-str": "WFhY",
  "old-str-base64-encoded": true,
  "new-str-base64-encoded": true,
  "multiline": true,
  "replace-all": true,
  "verbose": true
}
```

**Verwendung:**
```bash
StrEditor.exe --config "operations.json"
```

**JSON-Config Parameter für Multi-Line:**
- `multiline: true` - Aktiviert Multi-Line-Modus
- `replace-all: true` - Ersetzt alle Vorkommen (Standard: `false`)
- `old-str-base64-encoded: true` - Old-String ist Base64-kodiert 🆕 **[NEU in v1.6]**
- `new-str-base64-encoded: true` - New-String ist Base64-kodiert 🆕 **[NEU in v1.6]**
- `text-base64-encoded: true` - Text ist Base64-kodiert (für Insert) 🆕 **[NEU in v1.6]**

### 2. Exit-Codes für Multi-Line

- `0` - Erfolg (String gefunden und ersetzt)
- `1` - String nicht gefunden
- `2` - Fehler (z.B. Datei nicht gefunden)
- `10` - Ungültige Parameter-Kombination (z.B. `--multi-line` + `--condition-pattern`)

### 3. Wichtige Hinweise

**Einschränkungen:**
- `--multi-line` kann **nicht** mit `--condition-pattern` kombiniert werden (Exit-Code 10)
- `--multi-line` kann **nicht** mit `--start-line` / `--end-line` kombiniert werden (Exit-Code 10)

**Empfehlungen:**
- ✅ Verwende Base64-Parameter für Strings mit `#13#10` (CRLF)
- ✅ Verwende `--verbose` für Debugging
- ✅ Verwende `--dry-run` zum Testen
- ✅ Verwende `--backup` für kritische Dateien
- ✅ Verwende JSON-Config für komplexe Operationen

**Anwendungsfälle:**
- IFDEF-Block entfernen (5 Zeilen → 1 Zeile)
- Mehrzeilige Kommentare entfernen
- Mehrzeilige Konstrukte refactoren
- Code-Blöcke ersetzen

### 4. Workflow für Multi-Line Replace

**Wenn du einen mehrzeiligen String ersetzen möchtest:**
1. Erstelle Base64-String für `old-str` und `new-str` (siehe unten)
2. Teste mit `--dry-run` und `--verbose`
3. Führe ohne `--dry-run` aus (mit `--backup`)
4. Prüfe Exit-Code

**Base64-Encoding in PowerShell:**
```powershell
# String mit CRLF
$oldStr = "Line 1" + [char]13 + [char]10 + "Line 2"
$bytes = [System.Text.Encoding]::GetEncoding(1252).GetBytes($oldStr)
$base64 = [Convert]::ToBase64String($bytes)
Write-Host $base64
```

**Beispiel:**
```bash
# Schritt 1: Base64 erstellen (siehe oben)
# Schritt 2: Dry-Run
StrEditor.exe --file "MyUnit.pas" --old-str-base64 "..." --new-str-base64 "..." --multi-line --dry-run --verbose

# Schritt 3: Ausführen (mit Backup)
StrEditor.exe --file "MyUnit.pas" --old-str-base64 "..." --new-str-base64 "..." --multi-line --backup

# Schritt 4: Exit-Code prüfen
if ($LASTEXITCODE -eq 0) {
  Write-Host "SUCCESS"
} else {
  Write-Host "ERROR: Exit-Code $LASTEXITCODE"
}
```

---

