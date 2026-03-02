---
id: lsif-typescript
title: TypeScript
sidebar_label: TypeScript
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [TypeScript](https://www.typescriptlang.org/) we use SourceGraph's [SCIP indexer for TypeScript](https://github.com/sourcegraph/scip-typescript). [SCIP](https://about.sourcegraph.com/blog/announcing-scip) is a format for tools to share information about code.

## Prerequisites

You will need:

* [scip-typescript](https://github.com/sourcegraph/scip-typescript), installable with `npm install -g @sourcegraph/scip-typescript`
* [scip-to-glean](python-scip.md#installing-scip-to-glean), the SCIP-to-Glean converter
* A [Node.js](https://nodejs.org/en/download/) runtime

## Run the indexer

Ensure that you have [built and installed Glean](../building.md) and
the `glean` executable is on your `PATH`.

Index your TypeScript repository with:
```
glean index typescript DIR --db-root DB --db NAME/INSTANCE
```

where

* `DB` is the directory where you want to store your Glean DBs
* `DIR` is the root directory containing the TypeScript project
* `NAME/INSTANCE` is the name of the repository to create

To index very large TypeScript repositories, it may be necessary to use more heap memory in Node.js. Setting `export NODE_OPTIONS="--max-old-space-size=8192"` in the environment may help.

## In the shell

TypeScript source can also be indexed directly from the [Glean shell](../shell.md):

```
:index typescript DIR
```

The shell will pick a DB name and hash for you based on `DIR`.

## Schema

The schema is in <SrcFile file="glean/schema/source/scip.angle" />
