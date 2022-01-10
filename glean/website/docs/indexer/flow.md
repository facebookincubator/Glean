---
id: flow
title: JavaScript (Flow)
sidebar_label: JavaScript (Flow)
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile, SrcFileLink} from '@site/utils';

The JavaScript/Flow indexer is built into the
[Flow](https://flow.org/) system.  It's also included
in the [Glean demo Docker image](../trying.md) to try out.

## Run the indexer

```
flow glean DIR --output-dir JSON --write-root PREFIX
```

where

* `DIR` is the root directory containing the JavaScript/Flow files
* `JSON` is the directory in which to write the output `.json` files
* `PREFIX` is a prefix to add to the files in the Glean index (this
  can be empty if you don't need a prefix)

The generated files can be ingested into a Glean database using [`glean create`](../cli.md#glean-create).

## Derived predicates

Several predicates should be derived after indexing. For each `stored` predicate in the [schema](#schema) you should [`glean derive`](../cli.md#glean-derive) the predicate.

## In the shell

Flow source can also be indexed directly from the Glean shell:

```
:index flow DIR
```

## Schema

The schema is in <SrcFile file="glean/schema/source/flow.angle" />
