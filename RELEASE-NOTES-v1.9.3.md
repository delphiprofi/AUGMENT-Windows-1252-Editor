# StrEditor v1.9.3

## Bug Fixes

### WriteFileWithRetry - Retry-Mechanismus war komplett wirkungslos
The retry mechanism for file writes has been completely broken since v1.9.1 when it was introduced.
Two root causes were identified and fixed:

1. **`WriteFile` swallowed all exceptions** - The internal `try/except` caught every exception and just returned `false`,
   so no exception ever reached `WriteFileWithRetry`.
2. **`WriteFileWithRetry` had an unconditional `Exit`** - After calling `WriteFile`, an unconditional `Exit` was placed
   regardless of the return value. This meant the retry loop was never reached even when `WriteFile` returned `false`.

**Fix:** `WriteFile` now propagates exceptions (`try/except` → `try/finally`).
`WriteFileWithRetry` now only exits early on success (`if Result then Exit`).

**Practical impact:** When a file is transiently locked (Windows Indexer, Antivirus, etc.),
StrEditor now correctly retries 3 times with 100ms delay.

### replace-line with multi-line text caused file corruption (Workaround)
Using `replace-line` with a `text-lines` array caused CRLF sequences to be embedded in a single
TStringList entry. When written to disk, this produced more lines than expected, causing all subsequent
operations to work on wrong line numbers.

**Fix:** The text is automatically split into multiple lines with a stderr warning:
`please use replace-lines with start-line/end-line/text-lines`

## New Features

### JSON Alias `"line"` for `"insert-after-line"`
The JSON config now accepts `"line"` as an alias for `"insert-after-line"` (Postel's Law).
A warning is emitted to stderr to encourage correct usage.

## Downloads
- `StrEditor-v1.9.3-Windows-x86.zip` - Windows 32-bit executable with documentation

