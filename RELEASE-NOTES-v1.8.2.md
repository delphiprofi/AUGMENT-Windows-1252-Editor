# StrEditor v1.8.2

## New Features

### Indent Lines (`--indent-lines`)
Add spaces at the beginning of lines in a specified range.

```bash
StrEditor.exe --indent-lines --file "test.pas" --start-line 4 --end-line 8 --spaces 2
```

**Parameters:**
- `--file`: Target file
- `--start-line`: First line to indent (1-based)
- `--end-line`: Last line to indent (1-based)
- `--spaces`: Number of spaces to add (default: 2)
- `--backup`, `--dry-run`, `--diff`, `--verbose`: Standard options

**JSON Config:**
```json
{
  "command": "indent",
  "file": "test.pas",
  "start-line": 4,
  "end-line": 8,
  "spaces": 2
}
```

### Unindent Lines (`--unindent-lines`)
Remove spaces from the beginning of lines in a specified range.

```bash
StrEditor.exe --unindent-lines --file "test.pas" --start-line 4 --end-line 8 --spaces 2
```

**Features:**
- Partial unindent: If a line has fewer spaces than requested, only available spaces are removed
- Empty lines are handled gracefully

## Bug Fixes
- **Build-StrEditor-Release.bat**: Fixed documentation (Release config builds directly to `C:\Delphi XE16\bin\`)

## Tests
- 13 new unit tests for indent/unindent features
- **182 total tests passing**

## Downloads
- `StrEditor-v1.8.2-Windows-x86.zip` - Windows 32-bit executable with documentation
