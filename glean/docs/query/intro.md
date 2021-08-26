---
id: intro
title: Querying Glean
sidebar_label: Overview
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import Daiquery from './fb/daiquery.md';
import {SrcFile,SrcFileLink} from '@site/utils';

Glean's query language is called **Angle**.  Read the [Angle
Guide](../angle/guide) to learn about how to write Angle queries.

The [shell](../shell) can be used for testing queries and schema
changes, and exploring the data.

<FbInternalOnly>

To run the REPL to try queries interactively:

```lang=shell
buck run glean/shell:shell
```

To make a single query:

```lang=shell
buck run glean/shell:shell -- <query>
```

</FbInternalOnly>

<Daiquery />

## Query APIs

<FbInternalOnly>

There are APIs in [Hack](api/fb/hack.md), [Python](api/fb/python.md),
[Rust](api/fb/rust.md), and [Haskell](api/haskell.md).

</FbInternalOnly>

<OssOnly>

There is currently only a [Haskell](api/haskell.md) API; APIs in other
languages are coming soon.

</OssOnly>

All client layers are wrappers around the [Thrift API](#thrift).

The results of queries will be returned as instances of the Thrift
types in `glean/schema/thrift` that are generated automatically from
the [schema](../schema/basic.md).

The client layers provide a few useful things over the raw Thrift API:
* Connecting to the most recent database for a given repository;
* Sharding to ensure we connect to a suitable server for that database;
* In some cases, an API for building queries programmatically.

### Thrift

Glean exposes a <SrcFileLink file="glean/if/glean.thrift">Thrift
API</SrcFileLink>.

The two methods for querying are

* `userQuery` - general queries returning an arbitrary number of results
* `userQueryFacts` - fetch the definition of a single fact given its ID

Queries are in Angle syntax, and results are encoded as JSON or Thrift
compact which you can encode/decode into the Thrift types.

<FbInternalOnly>

## VS Code syntax highlighter

We have a VS Code plugin for Angle syntax highlighting in VS Code: [Glean development Support](https://www.internalfb.com/fb-vscode/marketplace/extension/nuclide.fb-glean/overview?leftNavTab=manage) the code lives [here](https://www.internalfb.com/code/fbsource/[master]/xplat/vscode/vscode-extensions/fb-glean/)

</FbInternalOnly>
