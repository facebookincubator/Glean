---
id: lsif-go
title: Go
sidebar_label: Go
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Go](https://go.dev/) we use SourceGraph's [LSIF indexer for Go](https://github.com/sourcegraph/lsif-go). [LSIF](https://lsif.dev) is a new format for tools to share information about code. Binary releases of [lsif-go](https://github.com/sourcegraph/lsif-go/releases) are available ffor x86 Linux which will work as Glean indexers. The LSIF indexer uses a recent (>1.15) [version of Go](https://go.dev/dl/).

## Run the indexer

Ensure that you have [built and installed Glean](../building.md) and
the `glean` executable is on your `PATH`.

Index your Go repository with:

```
glean index go DIR --db-root DB --db NAME/INSTANCE
```

where

* `DB` is the directory where you want to store your Glean DBs
* `DIR` is the root directory containing the Go project
* `NAME/INSTANCE` is the name of the repository to create

## In the shell

Go source can also be indexed directly from the [Glean shell](../shell.md):

```
:index go DIR
```

The shell will pick a DB name and hash for you based on `DIR`.

## Schema

The schema is in <SrcFile file="glean/schema/source/lsif.angle" />
