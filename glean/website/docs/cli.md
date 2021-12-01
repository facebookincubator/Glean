---
id: cli
title: The Glean CLI tool
sidebar_label: The Glean CLI tool
---

The Glean CLI tool (`glean`) can create and write data into databases,
perform a variety of admin tasks on databases, and also do one-off
queries.

The `glean` tool accepts all the [common
options](./running.md#common-options) to specify how to connect to access
the databases.

The available commands are as follows:

### `glean create`

Create a new database.

* `--repo NAME/HASH` or `--repo-name NAME --repo-hash HASH`<br />
Specifies the name and hash of the database
* `--finish`<br />
Also mark the DB as complete
* `--stacked REPO`<br />
Create a stacked database on top of `REPO`.
* `--property NAME=VALUE`<br />
Set properties when creating a DB
* `FILE..`<br />
File(s) of facts to write into the database (JSON). See [Writing data
to Glean](./write.md).

### `glean write`

Write facts to a database.

* `--repo NAME/HASH` or `--repo-name NAME --repo-hash HASH`<br />
Specifies the name and hash of the database
* `FILE..`<br />
File(s) of facts to write into the database (JSON). See [Writing data
to Glean](./write.md).
* `--finish`<br />
Also mark the DB as complete

### `glean finish`

Notify server that a database is complete.

* `--repo NAME/HASH` or `--repo-name NAME --repo-hash HASH`<br />
Specifies the name and hash of the database

### `glean dump`

Dump the contents of the specified database into a file.

* `--repo NAME/HASH` or `--repo-name NAME --repo-hash HASH`<br />
Specifies the name and hash of the database
* `FILE`<br />
File to write the facts into

### `glean delete`

Delete a database.

* `--repo NAME/HASH` or `--repo-name NAME --repo-hash HASH`<br />
Specifies the name and hash of the database

### `glean derive`

Derive and store a predicate. See [Derived Predicates](derived.md).

* `--page-bytes BYTES`<br />
Maximum number of bytes per page

* `--page-facts FACTS`<br />
Maximum number of facts per page

* `PREDICATE`<br />
Predicates to derive

### `glean query`

Execute an Angle query and print the results, or write them to a file.

* `--repo NAME/HASH` or `--repo-name NAME`<br />
Specifies the database to query

* `--page-bytes BYTES`<br />
Maximum number of bytes per page

* `--page-facts FACTS`<br />
Maximum number of facts per page

* `--recursive`<br />
Fetch nested facts (slower)

* `--limit FACTS`<br />
Maximum number of facts to query

* `-o,--output FILE`<br />
Output the facts to a file

* `--stats FILE`<br />
Output stats to a file ('-' for stdout)

* `--timeout MILLISECONDS`
Override the default query timeout

* `QUERY`
query to execute (`@file` to read from file, `-` for stdin)

* `--omit-results`
Don't print results; use with `--stat` to get a count of results

### `glean restore`

Restore a database from backup.

* `LOCATOR`<br />
DB location, see `:list-all` in glean shell.

Alternatively the DB to restore can be specified by:

* `--repo NAME/HASH` or `--repo-name NAME` and (`--repo-hash HASH` or `--date YYY-MM-DD`)

### `glean validate`

Perform checks on the internal integrity of a database. This is for
testing and debugging Glean itself.

 a local database

* `--repo NAME/HASH` or `--repo-name NAME --repo-hash HASH`<br />
Specifies the name and hash of the database

* `--no-typecheck`<br />
Don't typecheck facts.

* `--no-keys`<br />
Don't verify key uniqueness

* `--limit N`<br />
Only validate the first N facts

### `glean validate-schema`

Validate a schema. Checks that a new schem does not modify any of the
predicates in the existing schema, which could lead to problems.

* `FILE`<br />
Name of schema file

### `glean stats`

Get fact counts and sizes. Like the `:statistics` command in the shell.

* `--repo NAME/HASH` or `--repo-name NAME --repo-hash HASH`<br />
Specifies the name and hash of the database

### `glean unfinish`

Unfinish a local database (turn it from complete to incomplete
state). This is for testing and development and not for routine use:
once a database is marked complete it could be replicated, so we
shouldn't be modifying it.

* `--repo NAME/HASH` or `--repo-name NAME --repo-hash HASH`<br />
Specifies the name and hash of the database
