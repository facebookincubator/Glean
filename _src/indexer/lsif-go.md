---
id: lsif-go
title: Go
sidebar_label: Go
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Go](https://go.dev/) we use SourceGraph's [LSIF indexer for Go](https://github.com/sourcegraph/lsif-go). [LSIF](https://lsif.dev) is a new format for tools to share information about code. Binary releases of [lsif-go](https://github.com/sourcegraph/lsif-go/releases) are available ffor x86 Linux which will work as Glean indexers. The LSIF indexer uses a recent (>1.15) [version of Go](https://go.dev/dl/).

## Run the indexer

Build the glean-lsif indexer, which wraps the lsif-go indexer.

```
> cabal build glean-lsif
```

And index your Go repository with:
```
glean-lsif --language go --o JSON DIR
```

where

* `DIR` is the root directory containing the Go project
* `JSON` is the directory in which to write the output `.json` files after converting from lsif

The generated files can be ingested into a Glean database using [`glean create`](../cli.md#glean-create), or added to Glean with `:load` in the shell.

## In the shell

Go source can also be indexed directly from the Glean shell:

```
:index lsif/go DIR
```

## Schema

The schema is in <SrcFile file="glean/schema/source/lsif.angle" />
