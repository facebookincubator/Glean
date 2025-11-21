# scip-symbol-parse

A CLI tool for parsing SCIP symbols and outputting them as JSON.

## Overview

This tool parses SCIP (SCIP Code Intelligence Protocol) symbols according to the specification and outputs the parsed structure as JSON. It uses the `scip_symbol` library for parsing logic.

## Usage

### Parse a single symbol

```bash
scip-symbol-parse "rust-analyzer cargo std v1.0 io/IsTerminal#"
```

### Parse from stdin

```bash
echo "local myVar" | scip-symbol-parse --stdin
```

### Pretty-print JSON output

```bash
scip-symbol-parse --pretty "rust-analyzer cargo std v1.0 io/IsTerminal#"
```

### Process multiple symbols from a file

```bash
cat symbols.txt | scip-symbol-parse --stdin
```

## Options

- `<SYMBOL>` - The SCIP symbol to parse (positional argument)
- `--stdin, -s` - Read symbols from stdin (one per line)
- `--pretty, -p` - Pretty-print JSON output

## Examples

### Global symbol

```bash
$ scip-symbol-parse "rust-analyzer cargo std v1.0 io/IsTerminal#"
{"type":"global","scheme":"rust-analyzer","package":{"manager":"cargo","name":"std","version":"v1.0"},"descriptors":[{"name":"io","kind":"namespace"},{"name":"IsTerminal","kind":"type"}]}
```

### Local symbol

```bash
$ scip-symbol-parse "local myVar"
{"type":"local","id":"myVar"}
```

### Pretty-printed output

```bash
$ scip-symbol-parse --pretty "scip . . . MyClass#myMethod()."
{
  "type": "global",
  "scheme": "scip",
  "package": {
    "manager": null,
    "name": null,
    "version": null
  },
  "descriptors": [
    {
      "name": "MyClass",
      "kind": "type"
    },
    {
      "name": "myMethod",
      "kind": {
        "kind": "method",
        "disambiguator": null
      }
    }
  ]
}
```

## Building

```bash
buck2 build //fbcode/glean/lang/scip/indexer/scip_to_glean/scip_symbol_parse:scip-symbol-parse
```

## Testing

Run the test suite directly:

```bash
./test.sh
```

Or run via Buck:

```bash
buck2 test //fbcode/glean/lang/scip/indexer/scip_to_glean/scip_symbol_parse:test
```

Each test validates the complete JSON output to ensure correctness.

Requirements:
- `buck2` (for building)

## SCIP Symbol Format

This is documented in scip.proto: https://github.com/sourcegraph/scip/blob/main/scip.proto#L147-L188
