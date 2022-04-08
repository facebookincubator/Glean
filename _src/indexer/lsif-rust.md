---
id: lsif-rust
title: Rust
sidebar_label: Rust
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Rust](https://www.rust-lang.org/) we use [rust-analyzer](https://rust-analyzer.github.io/) in LSIF mode. Pre-built binaries of [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer/releases) can be used as indexers that emit LSIF from Rust source.

## Run the indexer

Build the glean-lsif indexer, which wraps rust-analyzer.

```
> cabal build glean-lsif
```

And index your Rust repository with:
```
glean-lsif --language rust --o JSON DIR
```

where

* `DIR` is the root directory containing the TypeScript project
* `JSON` is the directory in which to write the output `.json` files after converting from lsif

The generated files can be ingested into a Glean database using [`glean
create`](../cli.md#glean-create), or added to Glean with `:load` in the shell.

## In the shell

Rust source can also be indexed directly from the Glean shell:

```
:index lsif/rust DIR
```

## Schema

The schema is in <SrcFile file="glean/schema/source/lsif.angle" />
