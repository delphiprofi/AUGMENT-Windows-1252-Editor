# StrEditor MCP Server

Native Delphi MCP Server (JSON-RPC 2.0 over stdio) for encoding-aware file editing.
No Node.js, no Python, no dependencies - just a single EXE.

Quick start guides:
- English: [DOC/QUICKSTART.md](DOC/QUICKSTART.md)
- Deutsch: [DOC/QUICKSTART.de.md](DOC/QUICKSTART.de.md)

## Client configuration overview

| Client | Config file |
|--------|-------------|
| Claude Desktop (chat app) | `%APPDATA%\Claude\claude_desktop_config.json` |
| Claude Code CLI | `%USERPROFILE%\.claude.json` or `.mcp.json` |
| Claude Desktop Code tab | `%USERPROFILE%\.claude.json` or `.mcp.json` |
| Augment | MCP settings in the extension / UI |
| Codex CLI | `~/.codex/config.toml` |

## Features

- **14 Tools** for file editing, encoding detection, regex operations, and more
- **Encoding-aware**: Handles Windows-1252 and UTF-8 (with BOM) transparently
- **TSR mode**: Starts once, stays in memory, handles requests via stdin/stdout
- **Zero dependencies**: Single native Windows EXE

## Tools

| Tool | Description |
|------|-------------|
| `str_replace` | Replace a string in a file (encoding-aware) |
| `edit_file` | Batch operations (delete, insert, replace lines) |
| `show_file` | Show file contents with correct encoding |
| `detect_encoding` | Detect file encoding (Windows-1252 / UTF-8) |
| `regex_replace` | Replace text using regex with capture groups |
| `regex_test` | Test regex pattern (read-only, returns match count) |
| `move_lines` | Move lines between files or within same file |
| `indent_lines` | Add indentation to a range of lines |
| `unindent_lines` | Remove indentation from a range of lines |
| `convert_encoding` | Convert between UTF-8 and Windows-1252 |
| `repair_umlauts` | Repair broken umlauts using VCS or reference file |
| `file_compare` | Compare two files for encoding differences |
| `undo` | Restore from .bak backup file |
| `restart_server` | Shutdown server for update (host restarts automatically) |

## Installation

### Augment (VSCode)

Settings > MCP Servers > Add MCP:
- **Command:** `C:\mysys\StrEditor.exe --mcp`

### Claude Desktop (Chat-App)

Add to `%APPDATA%\Claude\claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "streditor": {
      "command": "C:\\path\\to\\StrEditor.exe",
      "args": ["--mcp"]
    }
  }
}
```

### Claude Code (CLI / Desktop Code-Tab)

`Claude Code` does **not** use `claude_desktop_config.json`.
The MCP configuration for `Claude Code CLI` and the `Code` tab inside Claude Desktop is separate.

Use one of these files instead:

- User-wide: `%USERPROFILE%\.claude.json`
- Project-local: `.mcp.json`

Example:

```json
{
  "mcpServers": {
    "streditor": {
      "command": "C:\\path\\to\\StrEditor.exe",
      "args": ["--mcp"]
    }
  }
}
```

> **Note:** `claude_desktop_config.json` is only for the classic Claude Desktop chat client.
> If you want to use this MCP server in `Claude Code CLI` or in the Claude Desktop `Code` tab,
> configure it via `.claude.json` or `.mcp.json`.

### OpenAI Codex CLI

Add to `~/.codex/config.toml`:

```toml
[mcp_servers.streditor]
command = "C:\\path\\to\\StrEditor.exe"
args = ["--mcp"]
```

### Any MCP-compatible client

- **Command:** `StrEditor.exe`
- **Args:** `["--mcp"]`
- **Transport:** `stdio`

## Protocol

The server implements MCP (Model Context Protocol) version `2024-11-05` using JSON-RPC 2.0 over stdio.

Each request/response is a single JSON line on stdin/stdout.

### Example

```
→ {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test"}}}
← {"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}},"serverInfo":{"name":"streditor","version":"1.10.0"}}}

→ {"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"detect_encoding","arguments":{"file":"C:\\src\\MyUnit.pas"}}}
← {"jsonrpc":"2.0","id":2,"result":{"content":[{"type":"text","text":"File: C:\\src\\MyUnit.pas\r\nEncoding: Windows-1252 (no BOM)"}]}}
```

## Updating

1. Build new `StrEditor.exe`
2. Call `restart_server` tool (server shuts down, host restarts it)
3. Copy new EXE to deployment path
4. Server auto-restarts with new version

## Legacy CLI Mode

The EXE also supports traditional command-line usage:

```
StrEditor.exe --file "test.pas" --old-str "foo" --new-str "bar"
StrEditor.exe --config operations.json
StrEditor.exe --version
```

The `--mcp` flag activates the MCP server mode.

