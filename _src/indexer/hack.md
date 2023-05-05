---
id: hack
title: Hack
sidebar_label: Hack
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

The [Hack](https://hacklang.org/) indexer is built into the [Hack typechecker](https://github.com/facebook/hhvm/tree/master/hphp/hack). Stable and nightly binaries of the Hack indexer [are available](https://docs.hhvm.com/hhvm/installation/linux).

## Run the indexer

The indexer is run via the main `glean` CLI tool.

```
> cabal build exe:glean
```

And index your Hack repository with:
```
glean index hack DIR --db NAME/INSTANCE
```

where

* `DIR` is the root directory containing the Hack project (with `.hhconfig`)
* `name/hash` is the name of the repository to create

Provide the usual `--db-root` and `--schema` or `--service` arguments
to `glean`

## In the shell

Hack source can also be indexed directly from the Glean shell:

```
:index hack DIR
```

## Run the indexer (manually)

```
hh_server DIR --write-symbol-info JSON \
  --config symbol_write_include_hhi=false \
  --config symbolindex_search_provider=NoIndex \
  --config lazy_decl=true \
  --config lazy_parse=true \
  --config lazy_init2=true \
```

where

* `DIR` is the root directory containing the `.php` files
* `JSON` is the directory in which to write the output `.json` files
* We need several config flags to instantiate hh_server for indexing

The generated files can be ingested into a Glean database using [`glean create`](../cli.md#glean-create).

## Derived predicates

Several predicates should be derived after indexing. For each `stored` predicate in the [schema](#schema) you should [`glean derive`](../cli.md#glean-derive) the predicate.

## Schema

The schema is in <SrcFile file="glean/schema/source/hack.angle" />
