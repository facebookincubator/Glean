---
id: databases
title: Glean Databases
sidebar_label: Glean Databases
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import Lifecycle from './fb/lifecycle.md';

Glean stores facts in databases.

A Glean database has:
* A **name**. This is often (but not always) the name of the source code repository from which the facts in the database were collected. Indeed, for historical reasons we sometimes say "repository" or "repo" when we mean "database".
* A **hash**. This is just an arbitrary string, used to distinguish different versions of databases with the same name. For source code repositories it can be the revision of the repository that was indexed.
* Some **properties**. A database has a set of key-value pairs associated with it. Some of these are created by Glean itself, and others can be set by a client when the DB is created, or while writing facts. Properties can be used to store arbitrary metadata about the DB, and can be accessed cheaply.
* A **schema**. The schema is stored in the DB, so that a DB knows the structure of the data it stores. You can query the DB using any schema that is [compatible](schema/changing.md#compatibility) with the DB's schema.

<FbInternalOnly>

At Facebook, we have adopted an informal convention for database names: `<repo>.<use-case>`. For example, test coverage data for the `www` repo uses the database name `www.tcc`. We sometimes omit the use case if the database is for primary source code indexing of the repo, so for example the index of Hack code in `www` is called just "`www`".

</FbInternalOnly>

The name and hash together uniquely identify a database. This is written `<name>/<hash>`, and it is how you refer to a database in most cases when working with Glean. For example, in the shell's `:db` command, or the `--db` argument to the command-line tools.

## Working with local databases

Most Glean tools (in particular the [shell](shell.md) and the [CLI
tool](cli.md)) can work either by talking to a [Glean server](server.md) or
accessing databases on the local filesystem directly. The access
method is chosen by these command-line flags:

* `--service <tier>` or `--service <host>:<port>`  Connect to a remote Glean server.
* `--db-root <dir>`  Use databases stored locally in the directory `<dir>`
* `--db-tmp` Create a temporary directory to store DBs and delete it
  when the program exits.
* `--db-memory` Store databases in memory rather than on disk.

These flags are accepted by all the Glean command-line tools,
including `glean` and `glean-server`.

<FbInternalOnly>

The default for all commands is `--service glean.query.prod`, so all requests go to the production Glean query service.

</FbInternalOnly>

To use the [shell](shell.md) with local databases, you can do:

```lang=sh
mkdir /tmp/glean
glean shell --db-root /tmp/glean
```

To run a server, see [Running the Glean Server](server.md).

## Lifecycle of a database

A database will typically be created by an automatic periodic job to
index the current state of a source repository.  The process works
like this:

<Lifecycle />

<OssOnly>

* The job invokes `glean create --service <write-server> <args>` to create the database.

* At this point the database is in the **Incomplete** state. Queries
are supported in this state, and always reflect the current contents.

* Facts are written to the database using the methods described in [Writing data to Glean](write.md), and finally the database is closed by invoking `glean finish --service <write-server> <args>` or the appropriate Thrift method.

* The database is now in the **Complete** state.

* If backups are allowed for this database, then:
  * the write server uploads the database to backup storage.
  * servers that are configured to restore databases automatically can download the DB from backup storage, and use it to serve queries from clients.

:::note

There are currently no backup backends implemented for open-source Glean.

:::

</OssOnly>
