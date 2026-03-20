# StrEditor MCP Quick Start

This quick start shows the fastest way to connect `StrEditor.exe --mcp` to common MCP clients.

## 1. Prepare StrEditor

1. Download or build `StrEditor.exe`
2. Put it in a stable location, for example `C:\Tools\StrEditor\StrEditor.exe`
3. Verify it starts:

```bash
StrEditor.exe --version
```

## 2. Choose your client

| Client | What it is | Config file | Scope |
|--------|------------|-------------|-------|
| Claude Desktop | Classic chat app | `%APPDATA%\Claude\claude_desktop_config.json` | App-wide |
| Claude Code CLI | Coding CLI | `%USERPROFILE%\.claude.json` or `.mcp.json` | User / project |
| Claude Desktop Code tab | Code UI inside Claude Desktop | `%USERPROFILE%\.claude.json` or `.mcp.json` | User / project |

### Claude Desktop (classic chat app)

Use `%APPDATA%\Claude\claude_desktop_config.json`:

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

### Claude Code CLI / Claude Desktop Code tab

Important: this does **not** use `claude_desktop_config.json`.

Use either:

- `%USERPROFILE%\.claude.json` for a user-wide setup
- `.mcp.json` for a project-local setup

Example `.mcp.json`:

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

- Open `Settings > MCP Servers > Add MCP`
- Command: `C:\Tools\StrEditor\StrEditor.exe --mcp`

### OpenAI Codex CLI

Use `~/.codex/config.toml`:

```toml
[mcp_servers.streditor]
command = "C:\\Tools\\StrEditor\\StrEditor.exe"
args = ["--mcp"]
```

## 3. Restart the client

After changing the config, fully restart the client so it picks up the new MCP server.

## 4. Smoke test

Check that the client sees the `streditor` server, then try a simple read-only tool call such as:

- `show_file`
- `detect_encoding`

## 5. Full reference

For more details, examples, and protocol notes, see [../MCP-SERVER.md](../MCP-SERVER.md).