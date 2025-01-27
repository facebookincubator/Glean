---
id: introduction
title: Introduction
sidebar_label: Introduction
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

<FbInternalOnly>

This is the Glean documentation. It describes all aspects of the
system, including how to write queries and schemas, and how
to use the included command-line tools. Use the navigation on the left
to find all the sections.

If you found yourself here by mistake, go back to the
  main [Glean Wiki](https://www.internalfb.com/wiki/Glean/).


</FbInternalOnly>

## Overview

Glean is a system for working with facts about source code. It is
designed for collecting and storing detailed information about code
structure, and providing access to the data to power tools and
experiences from online IDE features to offline code analysis.

For example, Glean could answer all the questions you'd expect your
IDE to answer, accurately and efficiently on a large-scale
codebase. Things like:

* *Where is the definition of this method?*
* *Where are all the callers of this function?*
* *Who inherits from this class?*
* *What are all the declarations in this file?*

But Glean isn't limited to storing particular kinds of data, or
answering particular queries. Glean comes with indexers and schemas
for some languages which support queries like the examples above, but
you can also define your own schemas and store whatever data you like,
perhaps augmenting the data that existing indexers collect.  So, for
example, you could store test coverage data or profiling data.

Glean's powerful query language means that you can build tools around
complex queries of the underlying data. For example, you could search
for dead code, write code linters, API migration tools or refactoring
tools, all by using Glean queries instead of a compiler API to inspect
the code structure.

## Components

Glean consists of the following:

* An **efficient storage backend** built on
  [RocksDB](https://rocksdb.org/), for storing facts. Facts are
  immutable terms described by user-defined schemas, and form a
  DAG. Facts are automatically de-duplicated by the storage
  backend. Think of it as being able to store and query the AST of
  your code, efficiently and with full type-safety[^1].

* A query engine implementing our **[declarative query language
  Angle](./angle/intro.md)**.  Angle is a logic language with similarities
  to Datalog, but with extensions that make it suitable for building
  complex queries over Glean data[^2].  Like in Datalog, Glean can
  derive new facts automatically by [defining rules](derived.md) using
  Angle.

* A **[server](server.md)** that manages multiple databases on disk, and
  serves requests from clients to create, write, and query
  databases. The server currently uses Thrift, but there's no reason
  there couldn't also be servers exposing other protocols in the
  future.  The server is designed to be deployed at scale, serving
  replicated databases to large numbers of clients.

* An **[interactive shell](shell.md)** where you can type queries and explore
  the data.

* A **[command-line tool](cli.md)** for creating, writing, and querying
  databases, either directly or by connecting to the server.

* Several **<SrcFileLink file="glean/schema/source">example schemas</SrcFileLink>** for common programming languages, and
  **[indexers](indexer/intro.md)** for some of those.  Note that Glean
  doesn't force all the data into a single schema; there can be
  arbitrary amounts of language-specific detail in the schema for each
  language. Language-neutral abstractions can be built by <SrcFileLink
  file="glean/schema/source/codemarkup.angle">deriving facts using Angle</SrcFileLink>.

[^1]: while we could in principle store the full AST, for efficiency
reasons we typically store only the parts we need for the clients we
want to support. Usually that means things like the locations of
definitions and cross-references, but not expressions.

[^2]: If you're familiar with Datalog, it's worth noting that
currently Angle is limited to non-recursive queries only.
