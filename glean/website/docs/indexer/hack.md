---
id: hack
title: Hack
sidebar_label: Hack
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

The [Hack](https://hacklang.org/) indexer is built into the [Hack
typechecker](https://github.com/facebook/hhvm/tree/master/hphp/hack).

## Run the indexer

```
hh_server DIR --write-symbol-info JSON
```

where

* `DIR` is the root directory containing the `.php` files
* `JSON` is the directory in which to write the output `.json` files

The generated files can be ingested into a Glean database using [`glean create`](../cli.md#glean-create).

## Derived predicates

Several predicates should be derived after indexing. For each `stored` predicate in the [schema](#schema) you should [`glean derive`](../cli.md#glean-derive) the predicate.

## In the shell

Hack source can also be indexed directly from the Glean shell:

```
:index hack DIR
```

## Schema

The schema is in <SrcFile file="glean/schema/source/hack.angle" />
