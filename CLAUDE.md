# Glean

Glean is a system for collecting, storing, and querying facts about source code at scale. It is primarily written in Haskell with a C++17 runtime.

## Build System

The build uses Cabal orchestrated through a Makefile. The `glean.cabal` file is **generated** from `glean.cabal.in` via m4 — never edit `glean.cabal` directly.

```bash
# Full build (generates thrift, schema code, bytecode, then builds)
make

# Generate glean.cabal from glean.cabal.in
make glean.cabal

# If you modify schema code (glean/schema/source) or the schema generator
# in glean/schema/gen:
make gen-schema thrift-schema-hs

# If you modify any .thrift files:
make thrift-glean-hs

# Build just the glean binary
cabal build exe:glean

# Build and run all tests
cabal test glean:tests

# Build Glass (code navigation service)
cabal build glass-server
```

## Testing

```bash
# Run all tests
cabal test glean:tests

# Run a single test suite by name (names defined in glean.cabal.in)
cabal test angle-test-angle
cabal test schematest
cabal test api
```

Test suite names are defined as `test-suite` entries in `glean.cabal.in` (67 test suites total). Examples: `angle-test-angle`, `schematest`, `api`, `lifecycle`, `glass-regression-hack`, `glean-snapshot-flow`.

## Linting

Haskell code is checked with `hlint`. The project-specific `.hlint.yaml` ignores several common suggestions (LambdaCase, Eta reduce, point-free style, etc.) and warns against `foldl` (use `foldl'`) and `fromJust`. All packages compile with `-Werror` and `-Wall`.

## Architecture

See roadmap in `glean/ROADMAP.md` for main components and dependencies.

### Schema System (Angle)

Glean's schema language is called **Angle**. Schema source files live in `glean/schema/source/*.angle` and define predicates (fact types) and derived queries for each supported language. The `gen-schema` make target processes these into Thrift definitions (`glean/schema/thrift/`) and Haskell bindings (`glean/schema/hs/`).

### Key Components

- **`glean/rts/`** — C++17 runtime: fact storage, binary encoding, bytecode VM, ownership tracking, query execution
- **`glean/hs/`** — Haskell FFI bindings to the C++ runtime, plus bytecode generation
- **`glean/db/`** — Database layer: storage backends (RocksDB, LMDB), lifecycle, janitor, write path
- **`glean/angle/`** — Angle query language parser, type checker, and compiler
- **`glean/if/`** — Thrift service definitions (the Glean API)
- **`glean/glass/`** — Glass: a code navigation service built on Glean
- **`glean/lang/`** — Language-specific indexers (C++/Clang, Haskell, Flow, LSIF/SCIP adapters, etc.)
- **`glean/client/`** — Client libraries (Haskell, Swift)
- **`hsthrift/`** — Git submodule: Haskell Thrift compiler and runtime (builds first)

### Code Generation Pipeline

Several layers of code are auto-generated and must be regenerated when their inputs change:

1. **Thrift** (`make thrift`) — Compiles `.thrift` files into Haskell bindings. The Thrift compiler itself is built from `hsthrift/`.
2. **Schema** (`make gen-schema`) — Generates Thrift and Haskell from `.angle` schema files.
3. **Thrift-Schema** (`make thrift-schema-hs`) — Compiles the generated schema Thrift into Haskell.
4. **Bytecode** (`make gen-bytecode`) — Generates bytecode instruction definitions from `glean/bytecode/`.

Generated code is written to `.build/<mode>/codegen/` then rsynced with `--checksum` to preserve timestamps for unchanged files.

### C++ Build Modes

C++ libraries can be built via make (`CXX_MODE=make`, faster, parallel) or via Cabal (for Hackage compatibility). Library definitions are in `mk/cxx.mk`.

## Conventions

- **Haskell style**: 80-column line width. Code must be `-Wall` clean. Use `foldl'` not `foldl`. Avoid `fromJust`. See `.hlint.yaml` for the full style policy.
- **C++ style**: C++17, compiled with Clang or GCC. Use `-fno-omit-frame-pointer`.
- **Haskell extensions**: The `fb-haskell` common stanza in `glean.cabal.in` enables a standard set of extensions (OverloadedStrings, ScopedTypeVariables, GADTs, LambdaCase, etc.). Use LANGUAGE pragmas for anything beyond this set.
- **Thrift**: Service interfaces are defined in `glean/if/`. Schema-derived Thrift goes in `glean/schema/thrift/`. The Thrift compiler comes from `hsthrift/`.
- **Schema changes**: Edit `.angle` files in `glean/schema/source/`, then run `make gen-schema thrift-schema-hs` to regenerate.
- **Testing snapshots**: Indexer tests use snapshot-based regression testing (e.g., `glean-snapshot-flow`). To update snapshots after intentional changes: `cabal run glean-snapshot-<lang> -- --replace-all`.
