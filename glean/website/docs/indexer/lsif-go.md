---
id: lsif-go
title: Go
sidebar_label: Go
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Go](https://go.dev/) we use SourceGraph's [SCIP indexer for Go](https://github.com/sourcegraph/scip-go). [SCIP](https://about.sourcegraph.com/blog/announcing-scip) is a format for tools to share information about code.

## Prerequisites

You will need:

* [scip-go](https://github.com/sourcegraph/scip-go), with binary releases available for x86 Linux
* [scip-to-glean](python-scip.md#installing-scip-to-glean), the SCIP-to-Glean converter
* A recent (>1.15) [version of Go](https://go.dev/dl/)

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

The schema is in <SrcFile file="glean/schema/source/scip.angle" />
