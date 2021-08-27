---
id: introduction
title: Introduction
sidebar_label: Introduction
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

Glean is a system for working with facts about source code. It is
designed for collecting and storing detailed information about code
structure, and providing access to the data to power tools and
experiences from online IDE features to offline code analysis.

Glean consists of the following components:

* An **efficient storage backend** built on
  [RocksDB](https://rocksdb.org/), for storing facts. Facts are
  immutable terms described by user-defined schemas, and form a
  DAG. Facts are automatically de-duplicated by the storage
  backend. Think of it as being able to store and query the AST of
  your code, efficiently and with full type-safety[^1].

* A query engine implementing our **[declarative query language
  Angle](angle/intro)**.  Angle is a logic language with similarities
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
