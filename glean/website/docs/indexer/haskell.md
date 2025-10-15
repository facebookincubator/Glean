---
id: haskell
title: Haskell
sidebar_label: Haskell
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Haskell](https://haskell.org) Glean consumes `.hie` files produced by the GHC compiler (>=8.8) with the flag `-fwrite-ide-info`.

# Run the indexer

The Haskell indexer uses the `.hie` files produced when the Haskell
code is compiled with GHC's `-fwrite-ide-info` option, so first you
need to compile your Haskell code with that flag. For example, using
Cabal you can do

```
cabal build --ghc-options=-fwrite-ide-info
```

To enable `-fwrite-ide-info` more permanently, you can add it to your
`cabal.project` like this:

```
program-options
    ghc-options: -fwrite-ide-info
```

Ensure that you have [built and installed Glean](../building.md) and
the `glean` executable is on your `PATH`.  Then you can index your
Haskell code with:

```
glean --db-root DBDIR index haskell ROOT --db NAME/INSTANCE
```

where

* `BUILD` is a build command such that GHC is called with `-fwrite-ide-info`
* `DBDIR` is the directory where the Glean db will be created
* `ROOT` is the root directory containing the build artifacts generated with the `fwrite-ide-info` flag (e.g. `dist-newstyle` if a Cabal project)
* `NAME/INSTANCE` is the name of the repository to create

## Schema

The schema is in <SrcFile file="glean/schema/source/hs.angle" />
