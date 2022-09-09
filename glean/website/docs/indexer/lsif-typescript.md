---
id: lsif-typescript
title: TypeScript
sidebar_label: TypeScript
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [TypeScript](https://www.typescriptlang.org/) we use SourceGraph's [LSIF indexer for TypeScript](https://github.com/sourcegraph/lsif-node). [LSIF](https://lsif.dev) is a new format for tools to share information about code. Releases of [lsif-tsc](https://github.com/sourcegraph/lsif-node/releases/tag/v0.7.4) can be installed with `yarn` or `npm` and used as indexers for LSIF, which Glean will accept. The indexer itself requires a [node.js](https://nodejs.org/en/download/) runtime.

## Run the indexer

The indexer is run via the main `glean` CLI tool.

```
> cabal build exe:glean
```

And index your TypeScript repository with:
```
glean index typescript DIR --db NAME/INSTANCE
```

where

* `DIR` is the root directory containing the TypeScript project
* `name/hash` is the name of the repository to create

Provide the usual `--db-root` and `--schema` or `--service` arguments
to `glean`

To index very large TypeScript repositories, it may be necessary to use more heap memory in node.js (or break up the targets into subdirectories). Setting `export NODE_OPTIONS="--max-old-space-size=8192"` in the environment in which the indexer runs may help.

## In the shell

TypeScript source can also be indexed directly from the Glean shell:

```
:index typescript DIR
```

The shell will pick a DB name and hash for you based on `DIR`.

## Schema

The schema is in <SrcFile file="glean/schema/source/lsif.angle" />
