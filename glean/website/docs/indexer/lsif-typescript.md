---
id: lsif-typescript
title: TypeScript
sidebar_label: TypeScript
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [TypeScript](https://www.typescriptlang.org/) we use SourceGraph's [LSIF indexer for TypeScript](https://github.com/sourcegraph/lsif-node). [LSIF](https://lsif.dev) is a new format for tools to share information about code. Releases of [lsif-tsc](https://github.com/sourcegraph/lsif-node/releases/tag/v0.7.4) can be installed with `yarn` or `npm` and used as indexers for LSIF, which Glean will accept.

## Run the indexer

Build the glean-lsif indexer, which wraps the lsif-tsc indexer.

```
> cabal build glean-lsif
```

And index your TypeScript repository with:
```
glean-lsif --language typescript --o JSON DIR
```

where

* `DIR` is the root directory containing the TypeScript project
* `JSON` is the directory in which to write the output `.json` files after converting from lsif

The generated files can be ingested into a Glean database using [`glean
create`](../cli.md#glean-create), or added to Glean with `:load` in the shell.

## In the shell

TypeScript source can also be indexed directly from the Glean shell:

```
:index lsif/typescript DIR
```

## Schema

The schema is in <SrcFile file="glean/schema/source/lsif.angle" />
