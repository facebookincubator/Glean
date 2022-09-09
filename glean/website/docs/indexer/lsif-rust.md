---
id: lsif-rust
title: Rust
sidebar_label: Rust
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Rust](https://www.rust-lang.org/) we use [rust-analyzer](https://rust-analyzer.github.io/) in LSIF mode. Pre-built binaries of [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer/releases) can be used as indexers that emit LSIF from Rust source.

## Run the indexer

The indexer is run via the main `glean` CLI tool.

```
> cabal build exe:glean
```

And index your Rust repository with:
```
glean index rust-lsif DIR --db NAME/INSTANCE
```

where

* `DIR` is the root directory containing the Rust project
* `name/hash` is the name of the repository to create

Provide the usual `--db-root` and `--schema` or `--service` arguments
to `glean`

## In the shell

Rust source can also be indexed directly from the Glean shell:

```
:index rust-lsif DIR
```

The shell will pick a DB name and hash for you based on `DIR`.

## Schema

The schema is in <SrcFile file="glean/schema/source/lsif.angle" />
