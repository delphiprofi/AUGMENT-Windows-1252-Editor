---
type: "agent_requested"
description: "Example description"
---

# 🚨🚨🚨 KRITISCHE WARNUNG FÜR AI-AGENTEN 🚨🚨🚨

## ⛔ NIEMALS SEQUENTIELLE OPERATIONEN AUF DIESELBE DATEI!

```powershell
# ❌ VERBOTEN - NIEMALS SO!
StrEditor.exe --file "test.pas" --delete-lines "5,6,7"
StrEditor.exe --file "test.pas" --insert-after 4 --text "Neue Zeile"
```

**WARUM?** Nach dem ersten Befehl stimmen die Zeilennummern nicht mehr!
- Zeile 5 ist nach dem Delete jetzt Zeile 4!
- Der zweite Befehl fügt an der FALSCHEN Stelle ein!
- **RESULTAT: KORRUPTE DATEI!**

## ✅ RICHTIG: JSON-Config verwenden!

```json
{
  "file": "test.pas",
  "operations": [
    { "command": "delete-line", "line": 5 },
    { "command": "delete-line", "line": 6 },
    { "command": "delete-line", "line": 7 },
    { "command": "insert-after", "insert-after-line": 4, "text": "Neue Zeile" }
  ]
}
```

**Alle Zeilennummern = ORIGINAL-Zustand! Kein Nachrechnen nötig!**

---

# 🚀 JSON-CONFIG MIT ZEILENOPERATIONEN - DIE BESTE METHODE!

## ⭐ WARUM JSON-CONFIG SO GENIAL IST:

**Alle Zeilennummern beziehen sich auf den ORIGINAL-Zustand der Datei!**

Das bedeutet: Du schaust in die Datei, siehst Zeile 735, schreibst `"line": 735`. **FERTIG.**
Du musst **NIEMALS** nachrechnen, wie sich Zeilennummern durch vorherige Operationen verschieben!

### Beispiel - So einfach ist es:

**Original-Datei:**
```
1: #ZEILE1
2: #ZEILE2
3: #ZEILE3
4: #ZEILE4
5: #ZEILE5
6: #ZEILE6
```

**Ich will:**
- Zeile 2 löschen
- Zeile 5 löschen
- Zeile 3 durch 2 Zeilen ersetzen
- Vor Zeile 2 etwas einfügen
- Nach Zeile 4 etwas einfügen

**JSON-Config (alle Zeilennummern = Original!):**
```json
{
  "file": "test.pas",
  "operations": [
    { "command": "delete-line", "line": 2 },
    { "command": "delete-line", "line": 5 },
    { "command": "replace-line", "line": 3, "text-lines": ["#ZEILE3a", "#ZEILE3b"] },
    { "command": "insert-before", "line": 2, "text": "#ZEILE1a" },
    { "command": "insert-after", "line": 4, "text": "#ZEILE4a" }
  ]
}
```

**Ergebnis:**
```
1: #ZEILE1
2: #ZEILE1a    ← eingefügt vor Original-Zeile 2
3: #ZEILE3a    ← Original-Zeile 3 ersetzt
4: #ZEILE3b
5: #ZEILE4
6: #ZEILE4a    ← eingefügt nach Original-Zeile 4
7: #ZEILE6
```
(#ZEILE2 und #ZEILE5 wurden gelöscht)

### ❌ OHNE diese Funktion wäre es UNMÖGLICH:

Bei einer 10.000-Zeilen-Datei mit 20 Operationen müsste ich bei JEDER Operation nachrechnen:
- "Ich habe Zeile 100 gelöscht, also ist Zeile 200 jetzt Zeile 199..."
- "Ich habe 3 Zeilen bei Zeile 50 eingefügt, also ist Zeile 199 jetzt Zeile 202..."
- 🤯 **UNMÖGLICH!**

### ✅ MIT dieser Funktion:

Ich schaue in die Datei, notiere die Zeilennummern die ich sehe, fertig.
Der BatchProcessor rechnet intern alles um.

---

## 📌 TL;DR - Die 3 wichtigsten Regeln

1. **🚫 NIEMALS** den internen `str-replace-editor` für `.pas` Dateien verwenden → Kaputte Umlaute!
2. **✅ IMMER** `StrEditor.exe` verwenden für Delphi-Dateien
3. **🎯 BEI MEHREREN OPERATIONEN** → **JSON-Config mit Zeilenoperationen verwenden!**
   - Alle Zeilennummern = Original-Zustand
   - Kein Nachrechnen von Offsets nötig
   - Automatische Sortierung (höchste Zeile zuerst)

---

## ✅ Entscheidungs-Checkliste: Wann JSON-Config?

**JSON-Config mit Zeilenoperationen verwenden, wenn:**
- [x] Mehrere Zeilen ändern/löschen/einfügen
- [x] Komplexes Refactoring (z.B. IFDEF-Blöcke entfernen und neu einfügen)
- [x] Mehrzeiliger Text → Nutze `"text-lines": ["Zeile 1", "Zeile 2"]`
- [x] Sonderzeichen im Text (`$`, `` ` ``, `"` etc.)

**Direkt aufrufen nur wenn:**
- [ ] Nur 1 einfache Operation
- [ ] Keine Sonderzeichen
- [ ] Kein mehrzeiliger Text

---

## 🚨 KRITISCHE REGEL: NIEMALS str-replace-editor für .pas Dateien verwenden!

**⚠️ WARNUNG:** Der interne `str-replace-editor` speichert IMMER als UTF-8 ohne BOM!
Delphi-Dateien ohne BOM werden als **Windows-1252** interpretiert → **KAPUTTE UMLAUTE!**

### Bei versehentlicher Verwendung von str-replace-editor:

**Symptom:** Umlaute werden als `Ã¤` `Ã¶` `Ã¼` `Ã` `Ã„` `Ã–` `Ãœ` angezeigt

**Sofortige Reparatur:**
```powershell
# MUSS SOFORT nach versehentlicher Bearbeitung ausgeführt werden!
StrEditor.exe --file "DATEINAME.pas" --reinterpret-as utf8 --backup --verbose
```

**Erklärung:**
- `--reinterpret-as utf8` liest die Datei als UTF-8 (wie sie vom Editor gespeichert wurde)
- Speichert sie dann als Windows-1252 (wie Delphi sie erwartet)
- `Ã¤` (UTF-8 Bytes für ä) wird zu `ä` (Windows-1252)

---

## 🔧 StrEditor Integration für Delphi-Dateien

**Version:** 1.9.0 (2026-02-15)

### Wichtig: Verwende IMMER StrEditor statt str-replace-editor

Für die Bearbeitung von Delphi-Dateien (`.pas`, `.dpr`, `.dpk`, `.dfm`, `.fmx`) verwende **AUSSCHLIESSLICH** das Tool `StrEditor.exe`.

**Pfad:** `C:\Delphi XE16\bin\StrEditor.exe` (liegt im PATH, kann ohne Pfad aufgerufen werden)

**Vollständige Dokumentation:** `C:\Delphi XE16\bin\AUGMENT-INTEGRATION.md`

---

## 🎯 WICHTIG: JSON-Config ist die BEVORZUGTE Methode!

### ⚠️ REGEL: Bei MEHREREN Operationen IMMER JSON-Config verwenden!

**Warum JSON-Config?**
1. **Effizienz:** Eine Datei wird nur EINMAL gelesen und geschrieben (statt N mal bei Einzelaufrufen)
2. **Atomarität:** Alle Operationen werden zusammen ausgeführt oder keine
3. **Automatische Sortierung:** Zeilenoperationen werden automatisch von unten nach oben sortiert (verhindert Index-Shifting)
4. **Keine Encoding-Probleme:** JSON ist UTF-8, aber Strings werden korrekt behandelt
5. **Keine PowerShell-Escaping-Probleme:** Sonderzeichen sind in JSON sicher

### 📋 JSON-Config Quick-Start

**Schritt 1:** Erstelle eine JSON-Datei (z.B. `operations.json`):
```json
{
  "operations": [
    {
      "command": "replace-line",
      "file": "MyUnit.pas",
      "line": 25,
      "text": "  WriteLn('Neue Zeile');"
    },
    {
      "command": "delete-line",
      "file": "MyUnit.pas",
      "line": 30
    },
    {
      "command": "insert-before",
      "file": "MyUnit.pas",
      "line": 10,
      "text": "  // Neuer Kommentar"
    }
  ]
}
```

**Schritt 2:** Führe aus:
```bash
StrEditor.exe --config operations.json --backup --verbose
```

### ❌ FALSCH: Mehrere Einzelaufrufe
```bash
# NICHT SO! Ineffizient und fehleranfällig!
StrEditor.exe --file "MyUnit.pas" --replace-line 25 --with "Text1"
StrEditor.exe --file "MyUnit.pas" --replace-line 30 --with "Text2"
StrEditor.exe --file "MyUnit.pas" --delete-line 35
```

### ✅ RICHTIG: Eine JSON-Config
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

## 🆕 v1.8.0 Neue Features

### 1. `text-lines` Array (EMPFOHLEN für mehrzeiligen Text!)

**Problem:** `\r\n` in JSON wird oft falsch interpretiert
**Lösung:** Nutze `"text-lines"` Array statt `"text"` String

```json
{
  "file": "test.pas",
  "command": "insert-after",
  "insert-after-line": 10,
  "text-lines": [
    "  {$IFDEF DEBUG}",
    "  WriteLn('Debug: ' + IntToStr(i));",
    "  {$ENDIF}"
  ]
}
```

### 2. Parameter-Aliase (Kurzformen)

| Kurz | Lang |
|------|------|
| `--ib` | `--insert-before-line` |
| `--ia` | `--insert-after-line` |
| `--dl` | `--delete-line` |
| `--rl` | `--replace-line` |
| `--ob64` | `--old-str-base64` |
| `--nb64` | `--new-str-base64` |

**Beispiel:**
```bash
StrEditor.exe --file "test.pas" --ia 10 --text "// Kommentar"
```

### 3. Kategorisierte Hilfe

Statt 100+ Zeilen Help → kompakte Übersicht mit Kategorien:
```bash
StrEditor.exe --help              # Übersicht
StrEditor.exe --help config       # JSON-Config Hilfe
StrEditor.exe --help insert       # Insert-Operationen
StrEditor.exe --help replace      # Replace-Operationen
StrEditor.exe --help all          # Komplette Hilfe
```

### 4. Warning bei `\r\n` Literal

Wenn du aus Versehen `\r\n` als 4 Zeichen (Backslash-r-Backslash-n) verwendest statt echtem CRLF, erscheint eine Warnung:
```
WARNING: Detected literal \r\n (4 characters) in parameter: text
  This is probably NOT what you want!
  For real line breaks, use JSON with "text-lines" array or base64 encoding.
```

---

### Grundlegende Verwendung (Einzeloperationen)

**Einfacher String Replace (nur für EINE Operation):**
```
StrEditor.exe --file "MyUnit.pas" --old-str "TObject" --new-str "TInterfacedObject"
```

**String Replace mit Zeilenbereich:
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

**🔥 JSON-Config Praxis-Beispiele:**

**Beispiel 1: Mehrere Zeilen in einer Datei ändern**
```json
{
  "operations": [
    {"command": "replace-line", "file": "MyUnit.pas", "line": 5, "text": "Unit MyNewUnit;"},
    {"command": "replace-line", "file": "MyUnit.pas", "line": 10, "text": "  fValue : Integer;"},
    {"command": "delete-line", "file": "MyUnit.pas", "line": 15},
    {"command": "insert-before", "file": "MyUnit.pas", "line": 20, "text": "  // Neuer Kommentar"}
  ]
}
```

**Beispiel 2: Mehrere Dateien in einem Aufruf**
```json
{
  "operations": [
    {"command": "replace-line", "file": "Unit1.pas", "line": 1, "text": "Unit Unit1_Renamed;"},
    {"command": "replace-line", "file": "Unit2.pas", "line": 1, "text": "Unit Unit2_Renamed;"},
    {"command": "replace-line", "file": "Unit3.pas", "line": 1, "text": "Unit Unit3_Renamed;"}
  ]
}
```

**Beispiel 3: String-Replace via JSON (für Sonderzeichen)**
```json
{
  "file": "MyUnit.pas",
  "old-str": "{$IFDEF DEBUG}",
  "new-str": "{$IFDEF RELEASE}",
  "verbose": true,
  "backup": true
}
```

**⚠️ MERKE:** Wenn du mehr als 2 Operationen an einer Datei durchführen willst → JSON-Config!

**Repair Umlauts (kaputte Umlaute reparieren):** 🆕 **[NEU in v1.7.4]**
```
# Umlaute reparieren mit VCS (Mercurial/Git)
StrEditor.exe --file "MyUnit.pas" --repair-umlauts --backup --verbose

# Dry-Run (Test ohne Änderung)
StrEditor.exe --file "MyUnit.pas" --repair-umlauts --dry-run --verbose

# Mit Referenz-Datei statt VCS
StrEditor.exe --file "MyUnit.pas" --repair-umlauts --reference "original.pas" --verbose
```
- Repariert kaputte UTF-8 Byte-Sequenzen in Windows-1252 Dateien
- Erkennt und repariert: Ã¤→ä, Ã¶→ö, Ã¼→ü, Ã→ß, Ã„→Ä, Ã–→Ö, Ãœ→Ü
- VCS-Integration: Verwendet automatisch Mercurial (hg) oder Git
- **Anwendungsfall:** Datei wurde versehentlich mit `str-replace-editor` bearbeitet

**Move Lines (Zeilen zwischen Dateien verschieben):** 🆕 **[NEU in v1.7.5]**
```
# Zeilen 50-100 von UnitA.pas nach UnitB.pas verschieben (nach Zeile 200)
StrEditor.exe --move-lines --from "UnitA.pas" --to "UnitB.pas" --start-line 50 --end-line 100 --insert-after-line 200

# Zeilen verschieben mit Backup
StrEditor.exe --move-lines --from "UnitA.pas" --to "UnitB.pas" --start-line 10 --end-line 20 --insert-before-line 50 --backup

# Dry-Run (Test ohne Änderung)
StrEditor.exe --move-lines --from "UnitA.pas" --to "UnitB.pas" --start-line 10 --end-line 20 --insert-after-line 100 --dry-run
```
- Verschiebt Zeilen von einer Datei in eine andere
- Erhält das Encoding beider Dateien (Source und Target)
- `--insert-after-line <n>`: Fügt nach Zeile n ein
- `--insert-before-line <n>`: Fügt vor Zeile n ein
- **Anwendungsfall:** Refactoring, Code zwischen Units verschieben

**Move Lines mit JSON-Config:** 🆕 **[NEU in v1.7.5]**
```json
{
  "operations": [
    {
      "command": "move-lines",
      "from-file": "UnitA.pas",
      "start-line": 50,
      "end-line": 100,
      "to-file": "UnitB.pas",
      "insert-after-line": 200
    }
  ]
}
```
```bash
StrEditor.exe --config operations.json --backup --verbose
```

**Auto-Delete Config (Standard seit v1.8.3):** 🆕 **[GEÄNDERT in v1.8.3]**
```bash
# JSON-Config wird bei Erfolg AUTOMATISCH GELÖSCHT (Standard!)
StrEditor.exe --config operations.json --verbose

# Config BEHALTEN (für Debugging)
StrEditor.exe --config operations.json --keep-config --verbose

# Dry-Run löscht Config NICHT (keine echten Änderungen)
StrEditor.exe --config operations.json --dry-run
# Output: "Dry-run mode: Config file NOT deleted"
```
- **WICHTIG:** Seit v1.8.3 wird die JSON-Config bei Erfolg AUTOMATISCH gelöscht!
- **--keep-config:** Verwenden wenn die Config-Datei erhalten bleiben soll
- **Dry-Run:** Bei `--dry-run` wird die Config NICHT gelöscht
- **Bei Fehler:** Config wird NICHT gelöscht (für Debugging)

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

#### **Hex-Dump (Byte-Ansicht):** 🆕 **[NEU in v1.8.1]**
```bash
# Ganze Datei als Hex-Dump
StrEditor.exe --file "MyUnit.pas" --show --hex

# Erste 64 Bytes als Hex-Dump
StrEditor.exe --file "MyUnit.pas" --show --hex --head 64

# Letzte 32 Bytes als Hex-Dump
StrEditor.exe --file "MyUnit.pas" --show --hex --tail 32
```
**Ausgabe:**
```
00000000: 70 72 6F 67 72 61 6D 20  53 74 72 45 64 69 74 6F  program StrEdito
00000010: 72 3B 0D 0A 0D 0A 7B 24  41 50 50 54 59 50 45 20  r;....{$APPTYPE
```
- **Format:** `OFFSET: HH HH HH HH HH HH HH HH  HH HH HH HH HH HH HH HH  ASCII`
- **16 Bytes pro Zeile** mit Leerzeichen nach dem 8. Byte
- **ASCII-Darstellung:** Druckbare Zeichen (32-126), sonst `.`
- **Anwendungsfall:** Encoding-Debugging (z.B. `F6` = Windows-1252 ö, `C3 B6` = UTF-8 ö)

#### **Base64-Ausgabe:** 🆕 **[NEU in v1.8.1]**
```bash
# Ganze Datei als Base64
StrEditor.exe --file "MyUnit.pas" --show --base64

# Erste 100 Bytes als Base64
StrEditor.exe --file "MyUnit.pas" --show --base64 --head 100

# Letzte 50 Bytes als Base64
StrEditor.exe --file "MyUnit.pas" --show --base64 --tail 50
```
- **Anwendungsfall:** Dateiinhalt für Copy/Paste oder Tests transportieren
- **Hinweis:** `--head` und `--tail` arbeiten im Hex/Base64-Modus auf **Byte-Ebene**, nicht Zeilen-Ebene

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

## 🆕 Neue Features in Version 1.9.0 (2026-02-15)

### Tolerante JSON-Config Validierung (Postel's Law)

**Problem:** AI-Agenten erzeugen häufig JSON-Configs mit leicht abweichenden Feldnamen (`"action"` statt `"command"`, `"old"` statt `"old-str"`) oder Formaten (Array `[{...}]` statt `{"operations": [...]}`). Diese schlugen bisher fehl (~48% Fehlerquote bei JSON-Configs).

**Lösung:** StrEditor akzeptiert jetzt gängige Varianten tolerant und gibt eine **Warnung auf stderr** aus:

| Geschrieben | Erwartet | Warnung |
|-------------|----------|---------|
| `"action": "insert"` | `"command": "insert"` | ✅ akzeptiert + WARNING |
| `"operation": "insert"` | `"command": "insert"` | ✅ akzeptiert + WARNING |
| `"replace"` (command) | `"str-replace"` | ✅ akzeptiert + WARNING |
| `[{...}, {...}]` | `{"operations": [...]}` | ✅ akzeptiert + WARNING |
| `"file"` auf Top-Level | `"file"` in jeder Operation | ✅ Fallback + WARNING |
| `"old"` / `"new"` | `"old-str"` / `"new-str"` | ✅ akzeptiert + WARNING |
| `"search"` / `"replace"` (Felder) | `"old-str"` / `"new-str"` | ✅ akzeptiert + WARNING |
| `"line-number"` | `"insert-after-line"` | ✅ akzeptiert + WARNING |
| Backslash in Pfaden | Doppel-Backslash | ⚠️ WARNING wenn Control-Chars erkannt |

**Alle Warnungen gehen auf stderr**, so dass stdout sauber bleibt.

**Beispiel:** Wenn die Config `"action": "insert"` enthält:
```
WARNING: [Op 1] Used "action" instead of "command" - accepted, but please use "command"
```

---

## 🆕 Neue Features in Version 1.8.7 (2026-02-09)

### 1. --filecompare: Dateivergleich auf kaputte Sonderzeichen

**Problem:** Nach Encoding-Konvertierungen oder fehlerhaften Dateioperationen können Sonderzeichen (Umlaute, ß, é, §, •) kaputt gehen. Man braucht einen automatischen Vergleich gegen eine Masterkopie.

**Lösung:** `--filecompare <master-file>` vergleicht eine Datei gegen die Masterkopie.

```bash
# Einfacher Vergleich
StrEditor.exe --file "test.pas" --filecompare "master.pas"

# Mit detaillierter Ausgabe
StrEditor.exe --file "test.pas" --filecompare "master.pas" --verbose
```

**Logik:**
1. Encoding beider Dateien prüfen → bei Unterschied: Exit Code 1
2. Master lesen, alle Sonderzeichen-Positionen mit 20-Zeichen-Kontext extrahieren
3. Kontext an Sonderzeichen-Positionen in Fragmente aufteilen (robust gegen kaputte Zeichen im Kontext)
4. File1 zeilenweise durchsuchen per Fragment-Matching
5. Pro Position: OK / BROKEN / NOT FOUND

**Exit Codes:**
- `0` = Alle Sonderzeichen stimmen überein
- `1` = Encoding unterschiedlich (Windows-1252 vs UTF-8)
- `2` = Sonderzeichen kaputt (mindestens eins falsch)
- `3` = Zeile nicht gefunden (manuell prüfen)

**Geprüfte Sonderzeichen:** öäüÖÄÜßé§•

**Hilfe:** `StrEditor.exe --help compare`

---

## 🆕 Neue Features in Version 1.8.6 (2026-02-05)

### 1. --range Parameter für --show

**Problem:** Manchmal will man nur einen bestimmten Zeilenbereich anzeigen, ohne --start-line und --end-line zu verwenden.
**Lösung:** `--range <start>,<end>` ist eine kompakte Alternative.

```bash
# Zeige Zeilen 10-20
StrEditor.exe --file "test.pas" --show --range 10,20 --line-numbers

# Zeige Zeilen 1-5 mit Hex-Dump
StrEditor.exe --file "test.pas" --show --range 1,5 --hex
```

**Validierung:**
- Format muss `<start>,<end>` sein (mit Komma)
- Beide Werte müssen > 0 sein
- End muss >= Start sein

### 2. Deprecation-Warnung für --keep-config

**Problem:** `--keep-config` macht keinen Sinn - JSON-Configs sind temporäre Operationsanweisungen, keine Datendateien!
**Lösung:** Parameter zeigt jetzt deutliche Warnung und wird in Zukunft entfernt.

```bash
# Zeigt Warnung:
StrEditor.exe --config "ops.json" --keep-config

╔════════════════════════════════════════════════════════════════════════════╗
║ WARNING: --keep-config is DEPRECATED and makes NO SENSE!                  ║
║                                                                            ║
║ JSON config files are TEMPORARY operation instructions, not data files!   ║
║ They should be auto-deleted after successful execution.                   ║
║                                                                            ║
║ This parameter will be REMOVED in a future version.                       ║
╚════════════════════════════════════════════════════════════════════════════╝
```

---

## 🆕 Neue Features in Version 1.8.4 (2026-01-30)

### 1. ChangeReport - Zeigt was geändert wurde

Nach jeder Operation zeigt StrEditor jetzt an, was geändert wurde:

```
DELETE line 7:
       5: procedure Test1;       <- Kontext vorher
       6: procedure Test2;       <- Kontext vorher
  -    7: procedure Test3;       <- gelöschte Zeile
       8: procedure Test4;       <- Kontext nachher
       9: procedure Test5;       <- Kontext nachher
```

### 2. SessionLog - Protokolliert alle Operationen

Für die Analyse von Fehlbedienungen durch AI-Agenten:
- Loggt VIEW-Anfragen, JSON-Configs, Fehler und Erfolge
- Base64-kodiert (keine CRLF-Probleme)
- Format: `Timestamp|Type|Base64-Data`

### 3. INI-Config - Konfiguration via StrEditor.ini

Neue Konfigurationsdatei `StrEditor.ini` (neben StrEditor.exe):

```ini
[ChangeReport]
Enabled=true
ShowContent=true
ContextLinesBefore=2
ContextLinesAfter=2

[SessionLog]
Enabled=true
LogPath=StrEditor.log
```

⚠️ **WICHTIG:** INI-Datei ohne UTF-8 BOM erstellen! Delphi's TIniFile kann das nicht korrekt lesen.

---

## 🆕 Neue Features in Version 1.8.3 (2026-01-29)

### 1. Auto-Delete JSON Config (BREAKING CHANGE!)

**Vorher (v1.7.6 - v1.8.2):** JSON-Config blieb nach Ausführung erhalten, `--delete-config-on-success` löschte sie.

**Jetzt (v1.8.3):** JSON-Config wird bei Erfolg **AUTOMATISCH GELÖSCHT**!

```bash
# Standard: JSON wird nach Erfolg gelöscht
StrEditor.exe --config "ops.json"
# Output: "Config file deleted successfully"

# Config behalten (für Debugging)
StrEditor.exe --config "ops.json" --keep-config
# Output: "Config file preserved (--keep-config)"
```

**Warum diese Änderung?**
- AI-Agenten vergessen oft `--delete-config-on-success`
- JSON-Dateien sammeln sich im Workspace an
- Der saubere Weg sollte der DEFAULT sein!

---

## 🆕 Neue Features in Version 1.8.1 (2026-01-26)

### 1. Hex-Dump Ausgabe (`--hex`)

**Problem:** Für Encoding-Debugging musste man bisher PowerShell verwenden, was zu Interpretationsproblemen führen konnte.
**Lösung:** `--hex` zeigt die rohen Bytes direkt, wie StrEditor sie liest.

```bash
# Hex-Dump der ersten 64 Bytes
StrEditor.exe --file "MyUnit.pas" --show --hex --head 64
```

**Ausgabe:**
```
00000000: 70 72 6F 67 72 61 6D 20  53 74 72 45 64 69 74 6F  program StrEdito
00000010: 72 3B 0D 0A 0D 0A 7B 24  41 50 50 54 59 50 45 20  r;....{$APPTYPE
```

**Anwendungsfälle:**
- Encoding-Debugging: `F6` = Windows-1252 ö, `C3 B6` = UTF-8 ö
- BOM-Erkennung: `EF BB BF` = UTF-8 BOM
- CRLF vs LF: `0D 0A` = CRLF, `0A` = LF

### 2. Base64-Ausgabe (`--base64`)

**Problem:** Dateiinhalte für Tests oder Copy/Paste transportieren.
**Lösung:** `--base64` gibt den Inhalt als Base64-String aus.

```bash
# Ganze Datei als Base64
StrEditor.exe --file "MyUnit.pas" --show --base64

# Erste 100 Bytes als Base64
StrEditor.exe --file "MyUnit.pas" --show --base64 --head 100
```

**Anwendungsfälle:**
- Dateiinhalt für Unit-Tests
- Binärdaten in JSON-Configs einbetten
- Exakte Byte-Sequenzen transportieren

### 3. Byte-basierte Head/Tail

Im Hex- und Base64-Modus arbeiten `--head` und `--tail` auf **Byte-Ebene**, nicht Zeilen-Ebene:

```bash
# Erste 32 Bytes (nicht Zeilen!)
StrEditor.exe --file "MyUnit.pas" --show --hex --head 32

# Letzte 16 Bytes
StrEditor.exe --file "MyUnit.pas" --show --base64 --tail 16
```

---


# ⚠️ HÄUFIGE FEHLER (aus Log-Analysen 2026-02-02 und 2026-02-06)

## ❌ FEHLER 1: Falsche Verwendung von "replace-line" für mehrere Zeilen

**PROBLEM:** Verwendung von `"command": "replace-line"` (SINGULAR) mit `"text-lines"` Array

**WICHTIG:** Es gibt ZWEI verschiedene Commands:
- `replace-line` (SINGULAR) - Für EINZELNE Zeile
- `replace-lines` (PLURAL) - Für MEHRERE Zeilen

**FALSCH:**
```json
{
  "operations": [
    {
      "file": "test.pas",
      "command": "replace-line",           // ❌ SINGULAR ist FALSCH für mehrere Zeilen!
      "replace-line": 100,                 // ❌ Falscher Feldname!
      "text-lines": [                      // ❌ text-lines passt nicht zu replace-line!
        "Zeile 1",
        "Zeile 2",
        "Zeile 3"
      ]
    }
  ]
}
```

**FEHLER:** `ProcessLineOperations failed` - Falsche Kombination von Command und Parametern!

**RICHTIG:**
```json
{
  "operations": [
    {
      "file": "test.pas",
      "command": "replace-lines",          // ✅ RICHTIG für mehrere Zeilen!
      "start-line": 100,                   // ✅ Start-Zeile
      "end-line": 102,                     // ✅ End-Zeile
      "text-lines": [
        "Zeile 1",
        "Zeile 2",
        "Zeile 3"
      ]
    }
  ]
}
```

**ODER (sicherer):** delete-lines + insert-after
```json
{
  "operations": [
    {
      "file": "test.pas",
      "command": "delete-lines",
      "start-line": 100,
      "end-line": 102
    },
    {
      "file": "test.pas",
      "command": "insert-after",
      "insert-after-line": 99,
      "text-lines": [
        "Zeile 1",
        "Zeile 2",
        "Zeile 3"
      ]
    }
  ]
}
```

**MERKE:** Beide Commands existieren, aber für unterschiedliche Zwecke!

**✅ replace-line (SINGULAR) - Für EINZELNE Zeile:**
```json
{
  "operations": [
    {
      "file": "test.pas",
      "command": "replace-line",           // ✅ SINGULAR für EINZELNE Zeile
      "line": 100,                         // ✅ "line" nicht "replace-line"
      "text": "Einzelne Zeile"             // ✅ "text" nicht "text-lines"
    }
  ]
}
```

**✅ replace-lines (PLURAL) - Für MEHRERE Zeilen:**
```json
{
  "operations": [
    {
      "file": "test.pas",
      "command": "replace-lines",          // ✅ PLURAL für MEHRERE Zeilen
      "start-line": 100,                   // ✅ start-line
      "end-line": 102,                     // ✅ end-line
      "text-lines": [                      // ✅ text-lines Array
        "Zeile 1",
        "Zeile 2",
        "Zeile 3"
      ]
    }
  ]
}
```

---

## ❌ FEHLER 2: Falscher Feldname "operation" oder "method"

**PROBLEM:** Verwendung von `"operation"` oder `"method"` statt `"command"`

**FALSCH:**
```json
{
  "operations": [
    {
      "file": "test.pas",
      "operation": "delete-line",          // ❌ FALSCH!
      "line": 100
    }
  ]
}
```

**ODER:**
```json
{
  "operations": [
    {
      "file": "test.pas",
      "method": "delete-line",             // ❌ FALSCH!
      "line": 100
    }
  ]
}
```

**RICHTIG:**
```json
{
  "operations": [
    {
      "file": "test.pas",
      "command": "delete-line",            // ✅ RICHTIG!
      "line": 100
    }
  ]
}
```

**MERKE:** Der Feldname ist IMMER `"command"`, niemals `"operation"` oder `"method"`!

---

## ❌ FEHLER 3: Fehlendes "operations" Array

**PROBLEM:** Einzelne Operation ohne `"operations"` Array

**FALSCH:**
```json
{
  "file": "test.pas",
  "command": "delete-line",
  "line": 100
}
```

**RICHTIG:**
```json
{
  "operations": [                          // ✅ Array erforderlich!
    {
      "file": "test.pas",
      "command": "delete-line",
      "line": 100
    }
  ]
}
```

**WICHTIG:** StrEditor erwartet IMMER ein `"operations"` Array, auch wenn nur eine Operation ausgeführt wird!

---

## ❌ FEHLER 4: Falscher Parameter-Name bei insert-after (NEU 2026-02-06)

**PROBLEM:** Verwendung von `"line"` statt `"insert-after-line"` bei `insert-after` command

**FALSCH:**
```json
{
  "operations": [
    {
      "file": "TreeViewEx.pas",
      "command": "insert-after",
      "line": 153,                             // ❌ FALSCH! Muss "insert-after-line" heißen!
      "text-lines": [
        "    procedure CMHintShow( var Message : TCMHintShow ); message CM_HINTSHOW;"
      ]
    }
  ]
}
```

**FEHLER:** `ProcessLineOperations failed: TreeViewEx.pas`

**RICHTIG:**
```json
{
  "operations": [
    {
      "file": "TreeViewEx.pas",
      "command": "insert-after",
      "insert-after-line": 153,                // ✅ RICHTIG!
      "text-lines": [
        "    procedure CMHintShow( var Message : TCMHintShow ); message CM_HINTSHOW;"
      ]
    }
  ]
}
```

**MERKE:**
- Bei `insert-after` → `"insert-after-line": <n>`
- Bei `insert-before` → `"insert-before-line": <n>`
- Bei `delete-line` → `"line": <n>`
- Bei `replace-line` → `"line": <n>`

---

## ❌ FEHLER 5: text-lines bei replace-line (NEU 2026-02-06)

**PROBLEM:** Verwendung von `"text-lines"` Array bei `replace-line` (SINGULAR)

**FALSCH:**
```json
{
  "operations": [
    {
      "file": "eAktenSystem.Frame.pas",
      "command": "replace-line",               // ❌ replace-line akzeptiert nur "text" (String)!
      "line": 1371,
      "text-lines": [                          // ❌ FALSCH! text-lines ist ein Array!
        "      if not fViewModel.StoreMetadata( @lNrTypEx, lMetadata, lGUID ) then",
        "        begin",
        "          AddUltimateLog( log_eAkte, 'StoreMetadata FEHLER: ' + fViewModel.LastError );",
        "        end;"
      ]
    }
  ]
}
```

**FEHLER:** `ProcessLineOperations failed: eAktenSystem.Frame.pas`

**RICHTIG (Methode 1): delete-line + insert-after**
```json
{
  "operations": [
    {
      "file": "eAktenSystem.Frame.pas",
      "command": "delete-line",
      "line": 1371
    },
    {
      "file": "eAktenSystem.Frame.pas",
      "command": "insert-after",
      "insert-after-line": 1370,
      "text-lines": [
        "      if not fViewModel.StoreMetadata( @lNrTypEx, lMetadata, lGUID ) then",
        "        begin",
        "          AddUltimateLog( log_eAkte, 'StoreMetadata FEHLER: ' + fViewModel.LastError );",
        "        end;"
      ]
    }
  ]
}
```

**RICHTIG (Methode 2): replace-lines (PLURAL)**
```json
{
  "operations": [
    {
      "file": "eAktenSystem.Frame.pas",
      "command": "replace-lines",              // ✅ PLURAL für mehrere Zeilen!
      "start-line": 1371,
      "end-line": 1371,                        // Auch wenn nur 1 Zeile ersetzt wird
      "text-lines": [
        "      if not fViewModel.StoreMetadata( @lNrTypEx, lMetadata, lGUID ) then",
        "        begin",
        "          AddUltimateLog( log_eAkte, 'StoreMetadata FEHLER: ' + fViewModel.LastError );",
        "        end;"
      ]
    }
  ]
}
```

**MERKE:**
- `replace-line` (SINGULAR) → nur `"text"` (String), NICHT `"text-lines"`!
- Um EINE Zeile durch MEHRERE zu ersetzen → `delete-line` + `insert-after` ODER `replace-lines`

---

## ❌ FEHLER 6: Umlaute ohne Base64-Encoding (NEU 2026-02-06)

**PROBLEM:** Umlaute (ü, ö, ä) direkt in JSON führen zu Encoding-Problemen

**FALSCH:**
```json
{
  "operations": [
    {
      "file": "eAkte.pas",
      "command": "replace-line",
      "line": 42,
      "text": "    class Function  IsSaveToEAkte(const aDialogText : String = '') : boolean; // Hier kann ein Text für einen Dialog übergeben werden..."
    }
  ]
}
```

**FEHLER:** Umlaute werden falsch kodiert (ü → ?, ö → ?, ä → ?)

**RICHTIG:**
```json
{
  "operations": [
    {
      "file": "eAkte.pas",
      "command": "replace-line",
      "line": 42,
      "text": "ICAgIGNsYXNzIEZ1bmN0aW9uICBJc1NhdmVUb0VBa3RlKGNvbnN0IGFEaWFsb2dUZXh0IDogU3RyaW5nID0gJycpIDogYm9vbGVhbjsgLy8gSGllciBrYW5uIGVpbiBUZXh0IGb8ciBlaW5lbiBEaWFsb2cg/GJlcmdlYmVuIHdlcmRlbi4uLg==",
      "text-base64-encoded": true              // ✅ WICHTIG!
    }
  ]
}
```

**MERKE:** Bei Umlauten IMMER Base64-Encoding verwenden mit `"text-base64-encoded": true`

---

## ✅ ERFOLGREICHE PATTERNS (aus echten Projekten)

### Pattern 1: Zeilen ersetzen (SICHER)
```json
{
  "operations": [
    {
      "file": "MyUnit.pas",
      "command": "delete-lines",
      "start-line": 100,
      "end-line": 105
    },
    {
      "file": "MyUnit.pas",
      "command": "insert-after",
      "insert-after-line": 99,
      "text-lines": [
        "Neue Zeile 1",
        "Neue Zeile 2",
        "Neue Zeile 3"
      ]
    }
  ]
}
```

### Pattern 2: Mehrere einzelne Zeilen löschen
```json
{
  "operations": [
    { "file": "MyUnit.pas", "command": "delete-line", "line": 38 },
    { "file": "MyUnit.pas", "command": "delete-line", "line": 40 },
    { "file": "MyUnit.pas", "command": "delete-line", "line": 110 },
    { "file": "MyUnit.pas", "command": "delete-line", "line": 114 }
  ]
}
```

### Pattern 3: Einzelne Zeile ersetzen
```json
{
  "operations": [
    {
      "file": "MyUnit.pas",
      "command": "replace-line",
      "line": 905,
      "text": "  if not ( fEntry.BlockType in [ btPDF, btXML, btHTML ] ) then"
    }
  ]
}
```

---

## 📋 CHECKLISTE: Vor dem Ausführen prüfen!

- [ ] Feldname ist `"command"` (nicht `"operation"` oder `"method"`)
- [ ] `"operations"` Array vorhanden
- [ ] Für mehrere Zeilen: `"command": "replace-lines"` mit `"start-line"` und `"end-line"`
- [ ] Für einzelne Zeile: `"command": "replace-line"` mit `"line"` und `"text"`
- [ ] **NEU:** Bei `insert-after` → `"insert-after-line"` (NICHT `"line"`!)
- [ ] **NEU:** Bei `insert-before` → `"insert-before-line"` (NICHT `"line"`!)
- [ ] **NEU:** Bei `replace-line` → nur `"text"` (String), NICHT `"text-lines"` (Array)!
- [ ] **NEU:** Bei Umlauten → `"text-base64-encoded": true` verwenden
- [ ] Alle Zeilennummern beziehen sich auf ORIGINAL-Datei
- [ ] Bei mehreren Operationen: Alle im selben `"operations"` Array

---
