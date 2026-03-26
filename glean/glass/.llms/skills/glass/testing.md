# Testing Glass Changes

## Component Architecture

```
glass CLI  <-->  glass-server  <-->  glean
```

## Deciding How to Run Each Component

### Decision 1: Run locally or use the production service?

```
Changed .angle schema files?
  YES → run Glean locally
  NO  → use prod Glean
        ↓
Changed Glass Haskell code?
  YES → run glass-server locally
  NO  → use prod glass-server (or skip manual testing)
        ↓
Changed CLI code?
  YES → build CLI from source
  NO  → use prod glass CLI
```

Note: if you run Glean locally, you must also run glass-server locally (to point it at local Glean).

### Decision 2: If running locally, use prod binary or build from source?

| Method | What it runs | When to use |
|--------|-------------|-------------|
| `glass` / `glean` (bare command) | **prod fbpkg** (pre-built binary) | Querying prod, or comparing prod behavior as baseline |
| `buck run @fbcode//mode/opt fbcode//glean/glass/facebook:glass-server` | **Your local code** | Testing your changes |

When testing Glass server changes, use `buck run` for both the server and client CLI to test end-to-end.

## Automated Tests

```bash
buck test fbcode//glean/glass/test:               # unit tests
buck test fbcode//glean/glass/test/facebook:       # internal tests
buck test fbcode//glean/glass/test/regression:     # snapshot regression (all languages)
buck test fbcode//glean/glass/test/regression:cpp  # just one language
buck test fbcode//glean/glass/facebook/test/perf:  # perf attribute tests
buck test fbcode//glean/lang/codemarkup/tests/...  # schema/codemarkup (if you changed .angle)
buck test fbcode//glean/glass/facebook/test:glass-snapshot-integration-test  # e2e
```

Regenerate golden files after intentional output changes:
```bash
buck run @//mode/opt fbcode//glean/glass/test/regression:cpp -- --replace-all
buck run fbcode//glean/glass/facebook/test/perf:perf -- --replace-all
buck run //glean/lang/codemarkup/tests/flow:tests -- --replace-all  # schema tests
```

### Recommended test workflow

1. Run unit tests: `buck test fbcode//glean/glass/test:` and `fbcode//glean/glass/test/facebook:`
2. Run regression tests if your change affects query output: `fbcode//glean/glass/test/regression:`
3. Run perf tests if you touched attributes: `fbcode//glean/glass/facebook/test/perf:`
4. Run schema tests if you changed codemarkup: `fbcode//glean/lang/codemarkup/tests/...`
5. Manual testing with a local server (see below)
6. `arc lint` before submitting

## Running a Local Glass Server

### Against production Glean (no schema changes)

```bash
buck run @fbcode//mode/opt fbcode//glean/glass/facebook:glass-server
```

Server prints its port on startup (e.g. `glass: port 26073`).

### Against a local Glean server (schema changes)

```bash
mkdir -p $HOME/local/gleandbs
buck run fbcode//glean/server:server -- \
  --db-root=$HOME/local/gleandbs \
  --schema=$HOME/fbsource/fbcode/glean/schema/source

# Note the port, then start Glass against it
buck run @fbcode//mode/opt fbcode//glean/glass/facebook:glass-server -- \
  +RTS -N16 -A128m -RTS \
  --service localhost:$GLEAN_PORT \
  --use-shards=no
```

> **Warning:** Only use `--use-shards=no` with a local Glean server, never against prod.

### Populating a local Glean with databases

```bash
# 1. Find a database to download. Use one of:
glean list
glean list <db-name>
glean list '<GLOB>'

# 2. Restore it locally, e.g.
glean --db-root=$HOME/local/gleandbs restore --repo=www.flow/<HASH>

# 3. Verify
glean list --db-root=$HOME/local/gleandbs
```

To test local schema changes, stack a new DB on top:
```bash
glean --db-root=$HOME/local/gleandbs \
  --schema=fbsource/fbcode/glean/schema/source \
  create --update-schema-for-stacked \
  --stacked=www.flow/<HASH> \
  --repo=www.flow/v2

glean --db-root=$HOME/local/gleandbs \
  --schema=fbsource/fbcode/glean/schema/source \
  derive --repo=www.flow/v2 flow.DeclarationUses
```

## Glass CLI Manual Testing

Path format: `fbsource/fbcode/path/to/file.py`
Symbol IDs: `fbsource/py/fbcode/<module.path.SymbolName>` (get from `search`, `symbol-index`, or `list` output)

Use `<glass-cli>` as shorthand below — substitute one of:
- `glass --service localhost:26073` (prod fbpkg)
- `buck run @fbcode//mode/opt fbcode//glean/glass/facebook/cli:glass -- --service localhost:26073` (local code)

```bash
<glass-cli> list fbsource/fbcode/glean/client/py3/__init__.py
<glass-cli> search GleanConfig --limit 5
<glass-cli> describe <SYMBOL_ID>
<glass-cli> find-references <SYMBOL_ID> --limit 10
<glass-cli> find-related <SYMBOL_ID> --callers --limit 5
```

Add `--debug` / `-d` to any command for timing and debug info.

> The first query may be slow as Glean loads the database. Subsequent queries will be fast.

### Typical manual test flow

1. `search` for a symbol to get its symbol ID
2. `describe` the symbol to check metadata
3. `find-references` to verify cross-references
4. `list` a file to check document symbol listing
5. `find-related --callers` / `--subclasses` to verify relation queries

## Performance Smoke Test (glass-test)

```bash
buck run fbcode//glean/glass/tools/facebook:glass-test -- --service localhost:$GLASS_PORT

# Target a specific repo/file
buck run fbcode//glean/glass/tools/facebook:glass-test -- \
  --service localhost:$GLASS_PORT --repo fbsource --file path/to/file.cpp

# Only run document-symbols phase
buck run fbcode//glean/glass/tools/facebook:glass-test -- \
  --service localhost:$GLASS_PORT --document-symbols
```

Phases: list-symbols → find-references → symbol-location → symbol-describe.

## Testing CodeHub Integration

Start a local Glass server, then on your WWW on-demand override `genGleanGlassClient()`:
```php
->setHost("localhost", 26073)
```
Use the IP address from the Glass startup log if Glass and CodeHub are on different machines.

## Documenting Tests in Diffs

Capture before/after CLI output, upload to Phabricator Paste, and link in your Test Plan:

```bash
arc paste --title "Glass before/after: <description>" < /tmp/glass-diff.txt
```

Example Test Plan:
```
## Automated
- `buck test fbcode//glean/glass/test:` — pass
- `buck test fbcode//glean/glass/test/regression:` — pass

## Manual
Before/after diff: P1234567
```
