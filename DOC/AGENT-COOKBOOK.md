# StrEditor Agent Cookbook

**Version:** 1.8.0  
**Für:** AI Agents (Augment, Cursor, etc.)

Praktische Rezepte für häufige Delphi-Bearbeitungsaufgaben.

---

## 📖 Rezept 1: Uses-Unit hinzufügen

**Aufgabe:** Eine neue Unit zur Uses-Klausel hinzufügen

```json
{
  "file": "MyUnit.pas",
  "command": "insert-after",
  "insert-after-line": 5,
  "text": ", NewUnit"
}
```

**Tipp:** Suche zuerst die Zeile mit dem letzten Unit-Namen vor dem Semikolon.

---

## 📖 Rezept 2: Property hinzufügen

**Aufgabe:** Property mit Getter/Setter zu einer Klasse hinzufügen

```json
{
  "operations": [
    {
      "file": "MyClass.pas",
      "command": "insert-before",
      "insert-before-line": 25,
      "text-lines": [
        "  private",
        "    fNewProp : string;",
        "    function GetNewProp : string;",
        "    procedure SetNewProp( const aValue : string );"
      ]
    },
    {
      "file": "MyClass.pas",
      "command": "insert-before",
      "insert-before-line": 40,
      "text-lines": [
        "  public",
        "    property NewProp : string read GetNewProp write SetNewProp;"
      ]
    }
  ]
}
```

---

## 📖 Rezept 3: Methode hinzufügen (Interface + Implementation)

**Aufgabe:** Neue Methode mit Deklaration und Implementation

```json
{
  "operations": [
    {
      "file": "MyUnit.pas",
      "command": "insert-after",
      "insert-after-line": 30,
      "text": "    procedure DoNewThing( const aParam : string );"
    },
    {
      "file": "MyUnit.pas",
      "command": "insert-before",
      "insert-before-line": 200,
      "text-lines": [
        "",
        "procedure TMyClass.DoNewThing( const aParam : string );",
        "begin",
        "  // TODO: Implementation",
        "end;",
        ""
      ]
    }
  ]
}
```

---

## 📖 Rezept 4: IFDEF-Block einfügen

**Aufgabe:** Bedingten Compiler-Block einfügen

```json
{
  "file": "MyUnit.pas",
  "command": "insert-after",
  "insert-after-line": 50,
  "text-lines": [
    "  {$IFDEF DEBUG}",
    "  WriteLn( 'Debug: Value = ' + IntToStr( lValue ) );",
    "  {$ENDIF}"
  ]
}
```

---

## 📖 Rezept 5: Mehrere Zeilen ersetzen

**Aufgabe:** Zeilen 25-27 durch neuen Code ersetzen

```json
{
  "operations": [
    {"file": "MyUnit.pas", "command": "delete-line", "delete-line": 25},
    {"file": "MyUnit.pas", "command": "delete-line", "delete-line": 26},
    {"file": "MyUnit.pas", "command": "delete-line", "delete-line": 27},
    {
      "file": "MyUnit.pas",
      "command": "insert-after",
      "insert-after-line": 24,
      "text-lines": [
        "  // Neue Implementation",
        "  Result := ProcessData( aInput );",
        "  ValidateResult( Result );"
      ]
    }
  ]
}
```

**Hinweis:** StrEditor sortiert automatisch von unten nach oben!

---

## 📖 Rezept 6: Kommentar-Header einfügen

**Aufgabe:** Region mit Dokumentation einfügen

```json
{
  "file": "MyUnit.pas",
  "command": "insert-before",
  "insert-before-line": 15,
  "text-lines": [
    "  {$REGION 'Documentation'}",
    "  /// <summary>",
    "  ///   Beschreibung der Klasse/Methode",
    "  /// </summary>",
    "  {$ENDREGION}"
  ]
}
```

---

## 📖 Rezept 7: String mit Sonderzeichen ersetzen

**Aufgabe:** String mit `$`, Quotes, etc. ersetzen

**Option A: Base64**
```bash
# Encode: echo "Text mit $Dollar" | base64
StrEditor.exe --file "test.pas" --ob64 "VGV4dCBtaXQgJERvbGxhcg==" --new-str "Ersatz"
```

**Option B: JSON (empfohlen)**
```json
{
  "file": "test.pas",
  "old-str": "Text mit $Dollar",
  "new-str": "Neuer Text"
}
```

---

## 🔧 Workflow-Tipps

1. **Immer `--dry-run` zuerst** bei komplexen Operationen
2. **`--backup` verwenden** für wichtige Dateien
3. **`--delete-config-on-success`** für Cleanup nach erfolgreicher Ausführung
4. **JSON für 3+ Operationen** - Atomarität und keine Index-Probleme

---

## ⚡ Quick Reference

| Aufgabe | Befehl |
|---------|--------|
| Zeile löschen | `--dl 25` oder `--delete-line 25` |
| Nach Zeile einfügen | `--ia 10 --text "..."` |
| Vor Zeile einfügen | `--ib 10 --text "..."` |
| Zeile ersetzen | `--rl 25 --with "..."` |
| Mit JSON | `--config ops.json --delete-config-on-success` |

