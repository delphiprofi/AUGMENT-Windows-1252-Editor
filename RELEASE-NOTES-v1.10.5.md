# StrEditor v1.10.5

## Bug Fixes

### UTF-8 input on MCP stdin (Mojibake in CP1252 files)

When an MCP client sent JSON tool arguments containing raw UTF-8 multibyte sequences (e.g. the two bytes `c3 a4` for `ä`), the server interpreted the incoming bytes through the system ANSI code page (CP1252 on German Windows) instead of UTF-8. This corrupted Non-ASCII characters before they ever reached the JSON parser and resulted in Mojibake being written to CP1252 source files (e.g. `Ã¤` instead of `ä`).

**Root cause**: `ReadRequest` read stdin via Delphi's `TextFile`-style `ReadLn(Input,…)` / `Read(Input,…)`. That path implicitly converts incoming bytes using `DefaultSystemCodePage`. JSON-RPC over MCP stdio is UTF-8 by spec, so this is wrong whenever the client serializes Non-ASCII as raw UTF-8 (instead of `\uXXXX` escapes, which is also legal JSON and was the historical default).

**Why it stayed latent**: Older MCP clients (Anthropic SDK defaults, several `ensure_ascii=true`-style serializers) escape Non-ASCII as `ä` — pure ASCII on the wire, which decodes identically under any single-byte code page. The bug only triggers for clients that send raw UTF-8 in JSON strings. Newer clients (including the Codex-style hosts that v1.10.4 was made compatible with) do this.

**Fix**: `ReadRequest` now reads stdin as raw bytes from `STD_INPUT_HANDLE` (via two new helpers `ReadLineBytesFromHandle` / `ReadExactBytesFromHandle`) and converts them to a Delphi string via `TEncoding.UTF8.GetString`. This is symmetric to the existing write side in `SendResponse`, which already used `TEncoding.UTF8.GetBytes`. Both ASCII-escape and raw-UTF-8 client behaviors now produce the correct codepoint.

End-to-end effect: rohe UTF-8-Bytes `c3 a4` im Tool-Argument → korrekter Codepoint U+00E4 nach Parse → genau ein Byte `$E4` in einer CP1252-Datei (statt zwei Mojibake-Bytes).

## Tests

- New test fixture `TTestMCPDecoding` in `Tests\TestStrEditor.MCP.pas` with 5 cases:
  - Empty input
  - Pure ASCII pass-through
  - ASCII-escaped umlaut `ä` (regression guard for the historically-working path)
  - Raw UTF-8 umlaut → must yield single codepoint U+00E4
  - Raw UTF-8 umlaut → end-to-end roundtrip to single CP1252 byte `$E4`
- 196/196 tests pass on the fix; the two raw-UTF-8 tests fail on the pre-fix code, confirming the bug repros and the fix lands exactly the missing decode step.

## New Files

- `Tests\TestStrEditor.MCP.pas` — MCP decoder regression tests
- `build_tests.bat` — convenience wrapper to build `Tests\Unittests.dproj`

## Changed Files

- `StrEditor.MCP.pas`
  - new public `class function DecodeIncomingBytes(TBytes) : string` (UTF-8)
  - new private helpers `ReadLineBytesFromHandle` / `ReadExactBytesFromHandle`
  - `ReadRequest` reworked to read raw bytes from `STD_INPUT_HANDLE` instead of `Input`
- `StrEditor.Version.pas` — bumped to `1.10.5`

## Downloads
- `StrEditor-v1.10.5-Windows-x86.zip` — Windows 32-bit executable with documentation
