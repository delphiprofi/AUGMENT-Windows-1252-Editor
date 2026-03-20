# StrEditor MCP Schnellstart

Dieser Schnellstart zeigt den schnellsten Weg, `StrEditor.exe --mcp` mit gängigen MCP-Clients zu verbinden.

## 1. StrEditor bereitstellen

1. `StrEditor.exe` herunterladen oder selbst kompilieren
2. In ein festes Verzeichnis legen, zum Beispiel `C:\Tools\StrEditor\StrEditor.exe`
3. Start testen:

```bash
StrEditor.exe --version
```

## 2. Client auswählen

| Client | Bedeutung | Konfigurationsdatei | Geltungsbereich |
|--------|-----------|---------------------|------------------|
| Claude Desktop | Klassische Chat-App | `%APPDATA%\Claude\claude_desktop_config.json` | Anwendungsweit |
| Claude Code CLI | Coding-CLI | `%USERPROFILE%\.claude.json` oder `.mcp.json` | Benutzer / Projekt |
| Claude Desktop Code-Tab | Code-Oberfläche in Claude Desktop | `%USERPROFILE%\.claude.json` oder `.mcp.json` | Benutzer / Projekt |

### Claude Desktop (klassische Chat-App)

Verwende `%APPDATA%\Claude\claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "streditor": {
      "command": "C:\\Tools\\StrEditor\\StrEditor.exe",
      "args": ["--mcp"]
    }
  }
}
```

### Claude Code CLI / Code-Tab in Claude Desktop

Wichtig: Das verwendet **nicht** `claude_desktop_config.json`.

Verwende stattdessen entweder:

- `%USERPROFILE%\.claude.json` für eine benutzerweite Konfiguration
- `.mcp.json` für eine projektlokale Konfiguration

Beispiel für `.mcp.json`:

```json
{
  "mcpServers": {
    "streditor": {
      "command": "C:\\Tools\\StrEditor\\StrEditor.exe",
      "args": ["--mcp"]
    }
  }
}
```

### Augment (VS Code)

- `Settings > MCP Servers > Add MCP` öffnen
- Command: `C:\Tools\StrEditor\StrEditor.exe --mcp`

### OpenAI Codex CLI

Verwende `~/.codex/config.toml`:

```toml
[mcp_servers.streditor]
command = "C:\\Tools\\StrEditor\\StrEditor.exe"
args = ["--mcp"]
```

## 3. Client neu starten

Nach einer Konfigurationsänderung den Client vollständig neu starten, damit der neue MCP-Server geladen wird.

## 4. Kurzer Funktionstest

Prüfe, ob der Client den Server `streditor` sieht, und rufe danach testweise ein einfaches Read-Only-Tool auf, zum Beispiel:

- `show_file`
- `detect_encoding`

## 5. Vollständige Referenz

Weitere Details, Beispiele und Protokollhinweise stehen in [../MCP-SERVER.md](../MCP-SERVER.md).