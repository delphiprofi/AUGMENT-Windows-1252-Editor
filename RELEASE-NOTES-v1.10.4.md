# StrEditor v1.10.4

## Bug Fixes

### OpenAI Codex MCP compatibility
Codex sends MCP startup messages differently than Claude/Augment. This release fixes compatibility:

- **JSON-RPC batch requests**: `ReadRequest` now accepts both single JSON-RPC objects and JSON-RPC batch arrays. Queued batch elements are processed one-by-one in the main loop. Prevents `Ungültige Typumwandlung` / transport-close on hosts that send batched startup messages.
- **Initialized notification**: Accepts both `initialized` and `notifications/initialized`.
- **Version consistency**: `initialize.result.serverInfo.version` now uses shared `cStrEditorVersion`.

## Documentation update

- Added bilingual MCP quick starts: English and German
- Clarified the difference between `Claude Desktop` (chat app) and `Claude Code` / Claude Desktop `Code` tab
- Added a small release checklist and updated packaging so the release ZIP now includes the new MCP docs consistently

## Downloads
- `StrEditor-v1.10.4-Windows-x86.zip` - Windows 32-bit executable with documentation

