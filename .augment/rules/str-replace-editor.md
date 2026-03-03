---
type: "agent_requested"
description: "StrEditor MCP-Server Regeln"
---

# 🚨 PFLICHT: MCP-Server `StrEditor` verwenden!

**Der StrEditor ist als MCP-Server registriert (`StrEditor`).**
Die MCP-Tools (`str_replace_StrEditor`, `edit_file_StrEditor`, `show_file_StrEditor` etc.)
dokumentieren sich selbst über das MCP-Protokoll (Tool-Beschreibungen, Parameter-Schemas).

**Verwende AUSSCHLIESSLICH die MCP-Tools. NIEMALS `StrEditor.exe` direkt aufrufen!**

---

## ⛔ VERBOTE

1. **NIEMALS** das interne `str-replace-editor` Tool für `.pas` Dateien verwenden!
   - Speichert UTF-8 ohne BOM → **zerstört Umlaute** (Windows-1252 Dateien)
   - Gilt für: `c:\gv`, `c:\comp`, `c:\Delphi\Kunden`, `k:\ncs\Source\Winneu`

2. **NIEMALS** `StrEditor.exe` direkt über die Kommandozeile aufrufen!
   - Der MCP-Server ist schneller (TSR-Modus), braucht kein Base64-Encoding
   - Keine JSON-Config-Dateien auf der Platte nötig

---

## ✅ REGELN

1. **IMMER** den MCP-Server `StrEditor` verwenden
2. Die MCP-Tools erklären ihre Parameter selbst — keine zusätzliche Doku nötig
3. Bei versehentlicher `str-replace-editor` Nutzung sofort reparieren:
   - MCP-Tool `reinterpret_encoding_StrEditor` mit `reinterpret_as: "utf8"` aufrufen

---

## 🛑 STOP-REGELN

**SOFORT STOPPEN und User fragen, wenn:**
1. Ein MCP-Tool-Aufruf fehlschlägt
2. Die Datei nach der Änderung "komisch" aussieht
3. Du unsicher bist, welches Tool korrekt ist

**NIEMALS "schnell mal reparieren" versuchen!**
