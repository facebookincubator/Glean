---
id: scip-dotnet
title: Dotnet
sidebar_label: Dotnet
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Dotnet](https://dotnet.microsoft.com/) we use SourceGraph's [SCIP indexer for dotnet](https://github.com/sourcegraph/scip-dotnet). [SCIP](https://about.sourcegraph.com/blog/announcing-scip) is a new format for tools to share information about code. Releases of [scip-dotnet](https://github.com/sourcegraph/scip-dotnet) can be installed with `dotnet tools` and used as indexers for SCIP, which Glean will accept. The indexer itself requires a [dotnet](https://dotnet.microsoft.com/) runtime environment.

## Run the indexer

Ensure that you have [built and installed Glean](../building.md) and
the `glean` executable is on your `PATH`.

You can index a Dotnet repository with:

```
glean index dotnet-scip DIR --db-root DB --db NAME/INSTANCE
```

where

* `DB` is the directory you want to store your Glean DBs
* `DIR` is the root directory containing the Dotnet project
* `NAME/INSTANCE` is the name of the repository to create

## In the shell

Dotnet source can also be indexed directly from the [Glean shell](../shell.md):

```
:index dotnet-scip DIR
```

The shell will pick a DB name and hash for you based on `DIR`.

## Schema

The schema is in <SrcFile file="glean/schema/source/scip.angle" />
