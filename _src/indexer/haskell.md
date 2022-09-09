---
id: haskell
title: Haskell
sidebar_label: Haskell
---

To index [Haskell](https://haskell.org) Glean consumes `.hie` files produced by the GHC compiler (>=8.8) with the flag `-fwrite-ide-info`.

# Run the indexer

The indexer is run via the main `glean` CLI tool.

```
BUILD --ghc-options=-fwrite-ide-info
glean --db-root DBDIR index haskell ROOT --db NAME/INSTANCE
```

where

* `BUILD` is a build command such that GHC is called with `-fwrite-ide-info`
* `DBDIR` is the directory where the Glean db will be created
* `ROOT` is the root directory containing the build artifacts generated with the `fwrite-ide-info` flag (e.g. `dist` if a Cabal project)
* `name/hash` is the name of the repository to create

## Schema

The schema is in <SrcFile file="glean/schema/source/hs.angle" />
