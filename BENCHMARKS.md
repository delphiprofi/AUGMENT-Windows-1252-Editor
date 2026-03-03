# StrEditor Benchmarks & Test Results

> **🇩🇪 [Deutsche Version weiter unten](#deutsche-version)**

---

## Disclaimer

The benchmark results presented in this document are **approximate values** measured on a specific development system. Results may vary significantly on different hardware, operating systems, or under different system loads. We make **no guarantees** regarding the accuracy or reproducibility of these results. They are intended to provide a general sense of relative performance characteristics, not absolute measurements.

**Test system:** Windows 11, Intel i7, SSD, Delphi XE 16 (32-bit build)

---

## 1. CLI vs MCP Server — Basic Operations (100 iterations)

**Test setup:** A small 6-line Delphi `.pas` file. Each operation was executed 100 times. CLI launches a new `StrEditor.exe` process per call; MCP keeps a single server process running and communicates via JSON-RPC over stdin/stdout.

| Operation | CLI (EXE) | MCP Server | Speedup |
|-----------|-----------|------------|---------|
| `detect_encoding` | **17.5 ms** avg | **0.05 ms** avg | ~350× |
| `show_file` | **19.1 ms** avg | **0.01 ms** avg | ~1,900× |
| `str_replace` | **40.5 ms** avg | **0.01 ms** avg | ~4,000× |

### Explanation

The CLI times are dominated by **process startup overhead** (~17ms on Windows for each EXE invocation). The MCP server pays this cost only once at startup. After that, each operation is pure in-memory work communicated through a pipe — resulting in sub-millisecond response times.

---

## 2. 10,000-Line File — 5 Replacements

**Test setup:** A generated Delphi unit with ~10,000 lines of realistic code. Five `str_replace` operations were performed at lines 14, 2016, 5018, 7500, and 9981 (spread across the entire file).

| Method | Time | Factor |
|--------|------|--------|
| **CLI: 5× StrEditor.exe** (separate processes) | **122 ms** | 1× (baseline) |
| **MCP: 5× str_replace** (separate tool calls) | **119 ms** | ~1× |
| **MCP: 1× edit_file** (5 ops batched) | **0.03 ms** | **~4,000×** |
| **Built-in str-replace-editor** (5 ops batched) | ~2–5 sec* | Slowest |

*\* The 2–5 seconds for the built-in str-replace-editor are due to the API round-trip (Cloud → Agent → Tool → back), not the local file operation itself.*

### Explanation

- **CLI** launches a new process for each replacement — 5 process starts = ~120ms total.
- **MCP separate calls** avoids process startup but still has 5 round-trips through the JSON-RPC pipe.
- **MCP edit_file batch** reads the file once, performs all 5 replacements in memory, writes once — resulting in ~0.03ms.
- **Built-in str-replace-editor** can also batch 5 operations in one call, but the bottleneck is the cloud API round-trip, not the local operation.

---

## 3. edit_file Batch — Detailed Measurement (20 iterations)

**Test setup:** Same 10,000-line file, same 5 replacements. The file was reset to its original state before each run. Measured 20 consecutive runs via `edit_file` batch call.

| Run | Time |
|-----|------|
| 1 (cold) | 0.59 ms |
| 2–20 (warm) | 0.03–0.04 ms |
| **Average** | **0.06 ms** |
| **Min** | **0.03 ms** |
| **Max** | **0.59 ms** |

### Explanation

The first run is slightly slower due to initial file I/O caching. Subsequent runs benefit from the OS file cache, resulting in consistent ~0.03ms per batch operation. This demonstrates that the MCP server's in-process architecture eliminates virtually all overhead.

---

## 4. Comment/Uncomment Lines — Functional Tests

**Test setup:** A 14-line Delphi unit with procedures, empty lines, and indented code.

| Test Case | Input | Result | Status |
|-----------|-------|--------|--------|
| Comment lines 6–11 | `procedure DoSomething;` | `//procedure DoSomething;` | ✅ |
| Comment empty line | *(empty)* | `//` | ✅ |
| Comment indented line | `  WriteLn('Hello');` | `//  WriteLn('Hello');` | ✅ |
| Uncomment → Round-trip | `//  WriteLn('Hello');` | `  WriteLn('Hello');` | ✅ |
| Double comment | `  WriteLn('Hello');` → 2× comment | `////  WriteLn('Hello');` | ✅ |
| Uncomment double | `////  WriteLn('Hello');` → 1× uncomment | `//  WriteLn('Hello');` | ✅ |

### Explanation

- **Comment** always prepends `//` at position 1 (column 0), preserving all existing indentation.
- **Uncomment** always removes exactly `//` (2 characters), ensuring a perfect round-trip.
- Empty lines receive `//` when commented (no special-casing).
- Already commented lines receive another `//` (no skip) — this is intentional to support nested commenting.

---

## Key Takeaways

1. **MCP Server eliminates process startup overhead** — the single biggest performance win (~17ms saved per call on Windows).
2. **Batching via `edit_file`** is the fastest approach — one file read, multiple in-memory operations, one file write.
3. **The real bottleneck for AI agents is the API round-trip**, not the local file operation. The MCP server minimizes the local portion to near-zero.
4. **Encoding preservation** (Windows-1252) is the primary functional advantage over the built-in str-replace-editor, which only handles UTF-8.

---
---

# <a name="deutsche-version"></a>🇩🇪 Deutsche Version

---

## Haftungsausschluss

Die in diesem Dokument dargestellten Benchmark-Ergebnisse sind **Näherungswerte**, die auf einem bestimmten Entwicklungssystem gemessen wurden. Die Ergebnisse können auf anderer Hardware, anderen Betriebssystemen oder unter anderer Systemlast **erheblich abweichen**. Wir übernehmen **keine Garantie** für die Richtigkeit oder Reproduzierbarkeit dieser Ergebnisse. Sie dienen lediglich dazu, ein allgemeines Gefühl für die relativen Leistungsmerkmale zu vermitteln, nicht als absolute Messungen.

**Testsystem:** Windows 11, Intel i7, SSD, Delphi XE 16 (32-Bit Build)

---


## 1. CLI vs MCP-Server — Basis-Operationen (100 Iterationen)

**Testaufbau:** Eine kleine 6-zeilige Delphi `.pas`-Datei. Jede Operation wurde 100-mal ausgeführt. CLI startet pro Aufruf einen neuen `StrEditor.exe`-Prozess; MCP hält einen einzelnen Server-Prozess und kommuniziert über JSON-RPC via stdin/stdout.

| Operation | CLI (EXE) | MCP-Server | Beschleunigung |
|-----------|-----------|------------|----------------|
| `detect_encoding` | **17,5 ms** Ø | **0,05 ms** Ø | ~350× |
| `show_file` | **19,1 ms** Ø | **0,01 ms** Ø | ~1.900× |
| `str_replace` | **40,5 ms** Ø | **0,01 ms** Ø | ~4.000× |

### Erklärung

Die CLI-Zeiten werden vom **Prozess-Start-Overhead** dominiert (~17ms unter Windows pro EXE-Aufruf). Der MCP-Server zahlt diesen Preis nur einmal beim Start. Danach ist jede Operation reine In-Memory-Arbeit über eine Pipe — mit Sub-Millisekunden-Antwortzeiten.

---

## 2. 10.000-Zeilen-Datei — 5 Ersetzungen

**Testaufbau:** Eine generierte Delphi-Unit mit ~10.000 Zeilen realistischem Code. Fünf `str_replace`-Operationen wurden an den Zeilen 14, 2016, 5018, 7500 und 9981 durchgeführt (über die gesamte Datei verteilt).

| Methode | Zeit | Faktor |
|---------|------|--------|
| **CLI: 5× StrEditor.exe** (separate Prozesse) | **122 ms** | 1× (Basis) |
| **MCP: 5× str_replace** (einzelne Tool-Calls) | **119 ms** | ~1× |
| **MCP: 1× edit_file** (5 Ops gebatcht) | **0,03 ms** | **~4.000×** |
| **Interner str-replace-editor** (5 Ops gebatcht) | ~2–5 Sek.* | Langsamster |

*\* Die 2–5 Sekunden beim internen str-replace-editor entstehen durch den API-Roundtrip (Cloud → Agent → Tool → zurück), nicht durch die lokale Datei-Operation.*

### Erklärung

- **CLI** startet für jede Ersetzung einen neuen Prozess — 5 Prozess-Starts = ~120ms gesamt.
- **MCP einzelne Calls** vermeidet den Prozess-Start, hat aber 5 Roundtrips durch die JSON-RPC-Pipe.
- **MCP edit_file Batch** liest die Datei einmal, führt alle 5 Ersetzungen im Speicher durch, schreibt einmal — Ergebnis: ~0,03ms.
- **Interner str-replace-editor** kann ebenfalls 5 Operationen in einem Call bündeln, aber der Flaschenhals ist der Cloud-API-Roundtrip, nicht die lokale Operation.

---

## 3. edit_file Batch — Detailmessung (20 Iterationen)

**Testaufbau:** Gleiche 10.000-Zeilen-Datei, gleiche 5 Ersetzungen. Die Datei wurde vor jedem Durchlauf auf den Originalzustand zurückgesetzt. 20 aufeinanderfolgende Durchläufe über `edit_file`-Batch-Call gemessen.

| Durchlauf | Zeit |
|-----------|------|
| 1 (kalt) | 0,59 ms |
| 2–20 (warm) | 0,03–0,04 ms |
| **Durchschnitt** | **0,06 ms** |
| **Minimum** | **0,03 ms** |
| **Maximum** | **0,59 ms** |

### Erklärung

Der erste Durchlauf ist durch initiales Datei-I/O-Caching etwas langsamer. Nachfolgende Durchläufe profitieren vom OS-Datei-Cache und liefern konstant ~0,03ms pro Batch-Operation. Dies zeigt, dass die In-Process-Architektur des MCP-Servers praktisch jeden Overhead eliminiert.

---

## 4. Comment/Uncomment Lines — Funktionstests

**Testaufbau:** Eine 14-zeilige Delphi-Unit mit Prozeduren, Leerzeilen und eingerücktem Code.

| Testfall | Eingabe | Ergebnis | Status |
|----------|---------|----------|--------|
| Zeilen 6–11 kommentieren | `procedure DoSomething;` | `//procedure DoSomething;` | ✅ |
| Leerzeile kommentieren | *(leer)* | `//` | ✅ |
| Eingerückte Zeile kommentieren | `  WriteLn('Hello');` | `//  WriteLn('Hello');` | ✅ |
| Uncomment → Round-Trip | `//  WriteLn('Hello');` | `  WriteLn('Hello');` | ✅ |
| Doppelt kommentieren | `  WriteLn('Hello');` → 2× comment | `////  WriteLn('Hello');` | ✅ |
| Doppelt uncomment | `////  WriteLn('Hello');` → 1× uncomment | `//  WriteLn('Hello');` | ✅ |

### Erklärung

- **Comment** fügt `//` immer an Position 1 (Spalte 0) ein und bewahrt die bestehende Einrückung.
- **Uncomment** entfernt immer genau `//` (2 Zeichen), was einen perfekten Round-Trip gewährleistet.
- Leerzeilen erhalten beim Kommentieren `//` (keine Sonderbehandlung).
- Bereits kommentierte Zeilen erhalten ein weiteres `//` (kein Überspringen) — dies ist beabsichtigt, um verschachteltes Kommentieren zu unterstützen.

---

## Kernaussagen

1. **Der MCP-Server eliminiert den Prozess-Start-Overhead** — der größte Performance-Gewinn (~17ms gespart pro Aufruf unter Windows).
2. **Batching über `edit_file`** ist der schnellste Ansatz — ein Datei-Lesen, mehrere In-Memory-Operationen, ein Datei-Schreiben.
3. **Der eigentliche Flaschenhals für KI-Agenten ist der API-Roundtrip**, nicht die lokale Datei-Operation. Der MCP-Server minimiert den lokalen Anteil auf nahezu Null.
4. **Encoding-Erhaltung** (Windows-1252) ist der primäre funktionale Vorteil gegenüber dem internen str-replace-editor, der nur UTF-8 verarbeitet.