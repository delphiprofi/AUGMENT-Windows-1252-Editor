# StrEditor v1.8.1 Release Notes

## Neue Features

### 🔍 Hex-Dump Ausgabe (`--hex`)

Zeigt den Dateiinhalt als Hex-Dump an - ideal für Encoding-Debugging:

```bash
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

### 📦 Base64-Ausgabe (`--base64`)

Gibt den Dateiinhalt als Base64-String aus:

```bash
StrEditor.exe --file "MyUnit.pas" --show --base64
```

**Anwendungsfälle:**
- Dateiinhalt für Unit-Tests transportieren
- Binärdaten in JSON-Configs einbetten
- Exakte Byte-Sequenzen kopieren

### 📏 Byte-basierte Head/Tail

Im Hex- und Base64-Modus arbeiten `--head` und `--tail` auf **Byte-Ebene**:

```bash
# Erste 32 Bytes als Hex-Dump
StrEditor.exe --file "MyUnit.pas" --show --hex --head 32

# Letzte 16 Bytes als Base64
StrEditor.exe --file "MyUnit.pas" --show --base64 --tail 16
```

## Vorteile gegenüber PowerShell

- **Exakte Byte-Ansicht** - Keine Interpretation durch Encoding
- **Konsistent** - Zeigt genau was StrEditor liest
- **Keine PowerShell Code Page Probleme**

## Installation

1. ZIP entpacken
2. `StrEditor.exe` nach `C:\Delphi XE16\bin\` kopieren
3. Fertig!

## Alle 169 Unit-Tests bestanden ✅

