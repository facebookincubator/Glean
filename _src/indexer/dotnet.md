---
id: scip-dotnet
title: Dotnet
sidebar_label: Dotnet
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Dotnet](https://dotnet.microsoft.com/) we use SourceGraph's [SCIP indexer for dotnet](https://github.com/sourcegraph/scip-dotnet). [SCIP](https://about.sourcegraph.com/blog/announcing-scip) is a new format for tools to share information about code. Releases of [scip-dotnet](https://github.com/sourcegraph/scip-dotnet) can be installed with `dotnet tools` and used as indexers for SCIP, which Glean will accept. The indexer itself requires a [dotnet](https://dotnet.microsoft.com/) runtime environment.

## Run the indexer

The indexer is run via the main `glean` CLI tool.

```
> cabal build exe:glean
```

And index your Dotnet repository with:
```
glean index dotnet-scip DIR --db NAME/INSTANCE
```

where

* `DIR` is the root directory containing the Dotnet project
* `name/hash` is the name of the repository to create

Provide the usual `--db-root` and `--schema` or `--service` arguments
to `glean`

## In the shell

Dotnet source can also be indexed directly from the Glean shell:

```
:index dotnet-scip DIR
```

The shell will pick a DB name and hash for you based on `DIR`.

## Schema

The schema is in <SrcFile file="glean/schema/source/scip.angle" />
