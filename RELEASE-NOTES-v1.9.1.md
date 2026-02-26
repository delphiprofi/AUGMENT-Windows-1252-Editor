# StrEditor v1.9.1

## Bug Fixes

### Retry-Mechanismus für File-Access-Errors

**Problem:** E/A-Fehler 232 ("The pipe is being closed") trat häufig auf, wenn Augment mehrere StrEditor-Instanzen parallel aufrief. Dies führte zu Race-Conditions und Handle-Leaks.

**Lösung:**
- Neue Funktionen `ReadFileWithRetry()` und `WriteFileWithRetry()` in `StrEditor.Encoding.pas`
- 3 Versuche mit 100ms Delay bei File-Access-Errors
- Exception-Handling für:
  - `EInOutError` (E/A-Fehler 232)
  - `EFOpenError` (Datei kann nicht geöffnet werden)
  - `EFCreateError` (Datei kann nicht erstellt werden)
- Alle 26 File-Access-Aufrufe in `StrEditor.Operations.pas` verwenden jetzt Retry-Mechanismus
- Detaillierte Fehlermeldungen mit Dateiname und "after 3 retries" Info auf stderr

**Basierend auf:** Log-Analyse vom 2026-02-26 - E/A-Fehler 232 war häufigster Fehler (24x) seit 2026-02-12

## Technical Details

- **Modified Files:**
  - `StrEditor.Encoding.pas` - Retry-Funktionen hinzugefügt
  - `StrEditor.Operations.pas` - Alle File-Access-Aufrufe auf Retry umgestellt
  - `StrEditor.CommandLine.pas` - Version auf 1.9.1 erhöht
  - `CHANGELOG.md` - Version 1.9.1 dokumentiert

- **Build Info:**
  - Compiler: Embarcadero Delphi for Win32 version 37.0
  - Lines: 9520
  - Build Time: 0.19 seconds
  - Code Size: 1737572 bytes

## Downloads

- `StrEditor-v1.9.1-Windows-x86.zip` - Windows 32-bit executable with documentation

## Previous Version

For changes in v1.9.0, see [CHANGELOG.md](CHANGELOG.md)

