---
id: scip-python
title: Python
sidebar_label: Python
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index [Python](https://www.python.org) we use SourceGraph's [SCIP indexer for Python](https://github.com/sourcegraph/scip-python). [SCIP](https://about.sourcegraph.com/blog/announcing-scip) is a format for tools to share information about code.

## Prerequisites

You will need:

* [scip-python](https://github.com/sourcegraph/scip-python), installable with `npm install -g @sourcegraph/scip-python`
* [scip-to-glean](#installing-scip-to-glean), the SCIP-to-Glean converter

## Run the indexer

Ensure that you have [built and installed Glean](../building.md) and
the `glean` executable is on your `PATH`.

You can index a Python repository with:

```
glean index python-scip DIR --db-root DB --db NAME/INSTANCE
```

where

* `DB` is the directory where you want to store your Glean DBs
* `DIR` is the root directory containing the Python project
* `NAME/INSTANCE` is the name of the repository to create

## In the shell

Python source can also be indexed directly from the [Glean shell](../shell.md):

```
:index python-scip DIR
```

The shell will pick a DB name and hash for you based on `DIR`.

## Installing scip-to-glean

All SCIP-based indexers require the `scip-to-glean` converter, which
converts SCIP protobuf output into Glean facts. Install it with:

```
cargo install --git https://github.com/facebookincubator/Glean \
  --path glean/lang/scip/indexer/scip_to_glean/cli
```

This installs the `scip-to-glean` binary into `~/.cargo/bin`. Make
sure this is on your `PATH`.

## Schema

The schema is in <SrcFile file="glean/schema/source/scip.angle" />
