---
id: lsif-rust
title: Rust
sidebar_label: Rust
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Rust](https://www.rust-lang.org/) we use [rust-analyzer](https://rust-analyzer.github.io/) in SCIP mode.

## Prerequisites

You will need:

* [rust-analyzer](https://rust-analyzer.github.io/), available via `rustup component add rust-analyzer`
* [scip-to-glean](python-scip.md#installing-scip-to-glean), the SCIP-to-Glean converter

## Run the indexer

Ensure that you have [built and installed Glean](../building.md) and
the `glean` executable is on your `PATH`.

Index your Rust repository with:
```
glean index rust-scip DIR --db-root DB --db NAME/INSTANCE
```

where

* `DB` is the directory where you want to store your Glean DBs
* `DIR` is the root directory containing the Rust project
* `NAME/INSTANCE` is the name of the repository to create

## In the shell

Rust source can also be indexed directly from the [Glean shell](../shell.md):

```
:index rust-scip DIR
```

The shell will pick a DB name and hash for you based on `DIR`.

## Schema

The schema is in <SrcFile file="glean/schema/source/scip.angle" />
