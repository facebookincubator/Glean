# Glass Server & CLI Architecture

Glass (Glean Language Agnostic Symbol Service) is a Thrift service and CLI that provides language-agnostic code navigation (symbol search, definitions, references, document symbols, related symbols) by querying Glean databases.

It is fundamentally a layer on top of Glean: most request fulfillment is done via Angle queries, often through the `codemarkup` schema, which unifies language-specific schemas.

## 1. High-Level Architecture

Flow at a high level:

1. Clients (`glass` CLI, IDE integrations, CodeHub) call Thrift RPCs.
2. `glassHandler` in [`Main.hs`](Glean/Glass/Main.hs) dispatches to request handlers.
3. Handlers resolve repo/language/file/symbol context and choose one or more Glean DBs.
4. Query modules build typed Angle queries (often codemarkup predicates).
5. Results are post-processed (attributes, symbol encoding, optional fallbacks) and returned.

Core files:

- `fbsource/fbcode/glean/glass/if/glass.thrift`
- `fbsource/fbcode/glean/glass/Glean/Glass/Main.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Env.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Documents.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Symbols.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Cxx.hs`

## 2. Service API (Thrift)

API definition:

- `fbsource/fbcode/glean/glass/if/glass.thrift`

Important RPCs include:

- Document-level:
  - `documentSymbolListX`
  - `documentSymbolIndex`
- Symbol navigation:
  - `findReferenceRanges`
  - `describeSymbol`
  - `symbolLocation`
  - `resolveSymbols`
- Search/relations:
  - `searchSymbol`
  - `searchRelated`
  - `searchRelatedNeighborhood`
- C++/USR-specific:
  - `fileIncludeLocations`
  - `clangUSRToDefinition`
  - `usrToDefinition`

## 3. Server Bootstrapping and Request Plumbing

Entrypoints and startup:

- `fbsource/fbcode/glean/glass/server/Server.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Main.hs`

Key startup/runtime functions in `Main.hs` include:

- `mainWith`
- `withEnv`
- `runGlass`
- `glassHandler`

Environment container:

- `fbsource/fbcode/glean/glass/Glean/Glass/Env.hs`

Shared request helpers:

- `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Utils.hs`
  - `withRequest`
  - `withRepoFile`
  - `withRepoLanguage`
  - `withSymbol`
  - `dbChooser`

These helpers standardize request parsing, DB selection policy, error handling behavior, and handler execution.

## 4. Glean DB Selection (“best/closest DB”)

This is one of Glass’s key logic layers.

Primary files:

- `fbsource/fbcode/glean/glass/Glean/Glass/Repos.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Base.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Utils.hs`
- `fbsource/fbcode/glean/glass/facebook/Glean/Glass/RepoMapping.hs`

Important strategy type (in practice):

- `ChooseLatest`
- `ChooseExactOrLatest Revision`
- `ChooseNearest RepoName Revision`

Common pipeline:

1. Expand candidate DB names from repo/language mapping.
2. Filter candidates for the current request context (repo, branch/revision constraints, file language).
3. Use cached latest DB metadata (periodically refreshed).
4. Choose concrete DB repos:
   - latest if no revision requested,
   - exact-or-latest when exact is requested,
   - nearest-by-generation when closest revision is preferred.

Key functions to inspect:

- `selectGleanDBs`
- `chooseGleanDBs`
- `getLatestRepos`
- `updateLatestRepos`
- `fromSCSRepo`

## 5. Query Layer and `codemarkup`

Unifying schema:

- `fbsource/fbcode/glean/schema/source/codemarkup.angle`

Glass query wrappers:

- `fbsource/fbcode/glean/glass/Glean/Glass/Query.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Query/Cxx.hs`

Representative codemarkup-backed query helpers in `Query.hs`:

- `fileEntityLocations`
- `fileEntityXRefsGenEntities`
- `findReferenceRangeSpan`
- `findReferenceEntities`
- `entityLocation`
- `symbolToEntity`
- `symbolKind`

Representative codemarkup predicates used by Glass include:

- `FileEntityLocations`
- `FileXRefsGenericEntities`
- `EntityReferences`
- `EntityLocation`
- `EntityKind`
- `SymbolToEntity`
- `SearchRelatedEntities`
- `GeneratedEntityToIdlEntity`

This is the core reason Glass can expose language-agnostic APIs while still serving many language backends.

## 6. Main Handler Flows

Document flows:

- `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Documents.hs`
  - `documentSymbolListX`
  - `documentSymbolIndex`

Symbol/search flows:

- `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Symbols.hs`
  - `findReferenceRanges`
  - `describeSymbol`
  - `symbolLocation`
  - `resolveSymbols`
  - `searchSymbol`
  - `searchRelated`
  - `searchRelatedNeighborhood`

C++/USR flows:

- `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Cxx.hs`
  - `fileIncludeLocations`
  - `clangUSRToDefinition`
  - `usrToDefinition`

## 7. Source Control and Revision-Aware Behavior

Source-control abstraction:

- `fbsource/fbcode/glean/glass/Glean/Glass/SourceControl.hs`

Important integration points include:

- generation lookup for nearest-revision DB selection,
- file hash/diff logic for revision compatibility decisions,
- branch ancestry checks when filtering DB choices.

## 8. CLI Architecture and Command Mapping

CLI entrypoint and modules:

- `fbsource/fbcode/glean/glass/facebook/cli/src/main.rs`
- `fbsource/fbcode/glean/glass/facebook/cli/src/commands/mod.rs`
- `fbsource/fbcode/glean/glass/facebook/cli/src/commands/`

Representative command modules are in `fbsource/fbcode/glean/glass/facebook/cli/src/commands`.

Typical mapping:

- `glass list` → `documentSymbolListX`
- `glass symbol-index` → `documentSymbolIndex`
- `glass find-references` → `findReferenceRanges`
- `glass describe` → `describeSymbol`
- `glass location` → `symbolLocation`
- `glass search` → `searchSymbol`
- `glass find-related` / `glass call-hierarchy` → `searchRelated`-driven flows
- `glass usr-to-definition` → `usrToDefinition`

## 9. Additional Logic Worth Knowing

Beyond the baseline request/response path, there are useful behaviors to understand:

- Snapshot backend integration for some document-symbol workloads.
- Fallback rules between snapshot and live Glean query paths.
- Cross-language and generated-to-IDL reference mapping paths.
- Strict-mode error behavior controlled via request options.
- Performance controls via batching, limits, and timeouts.

Useful files:

- `fbsource/fbcode/glean/glass/Glean/Glass/SnapshotBackend.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/NameSearch.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/SearchRelated.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/Neighborhood.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/SymbolId.hs`
- `fbsource/fbcode/glean/glass/Glean/Glass/XRefs.hs`

## 10. Quick Code Pointer Index

- API: `fbsource/fbcode/glean/glass/if/glass.thrift`
- Server dispatch: `fbsource/fbcode/glean/glass/Glean/Glass/Main.hs`
- Env/runtime: `fbsource/fbcode/glean/glass/Glean/Glass/Env.hs`
- Request utilities: `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Utils.hs`
- Document handlers: `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Documents.hs`
- Symbol handlers: `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Symbols.hs`
- C++ handlers: `fbsource/fbcode/glean/glass/Glean/Glass/Handler/Cxx.hs`
- DB selection: `fbsource/fbcode/glean/glass/Glean/Glass/Repos.hs`
- Repo mapping: `fbsource/fbcode/glean/glass/facebook/Glean/Glass/RepoMapping.hs`
- Query layer: `fbsource/fbcode/glean/glass/Glean/Glass/Query.hs`
- C++ query layer: `fbsource/fbcode/glean/glass/Glean/Glass/Query/Cxx.hs`
- codemarkup schema: `fbsource/fbcode/glean/schema/source/codemarkup.angle`
- Source control abstraction: `fbsource/fbcode/glean/glass/Glean/Glass/SourceControl.hs`
- CLI entrypoint: `fbsource/fbcode/glean/glass/facebook/cli/src/main.rs`
- CLI commands: `fbsource/fbcode/glean/glass/facebook/cli/src/commands/`
