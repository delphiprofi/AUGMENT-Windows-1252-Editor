# StrEditor v1.10.4

## Bug Fixes

### OpenAI Codex MCP compatibility
Codex sends MCP startup messages differently than Claude/Augment. This release fixes compatibility:

- **JSON-RPC batch requests**: `ReadRequest` now accepts both single JSON-RPC objects and JSON-RPC batch arrays. Queued batch elements are processed one-by-one in the main loop. Prevents `Ungültige Typumwandlung` / transport-close on hosts that send batched startup messages.
- **Initialized notification**: Accepts both `initialized` and `notifications/initialized`.
- **Version consistency**: `initialize.result.serverInfo.version` now uses shared `cStrEditorVersion`.

## Downloads
- `StrEditor-v1.10.4-Windows-x86.zip` - Windows 32-bit executable with documentation

