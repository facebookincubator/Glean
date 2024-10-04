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

### `glean list`

List the available databases.

* `[DBNAME]` <br />
List only databases that match `DBNAME`
* `--format (tty|plain|json|compact-json)` <br />
Various different formats for the output. JSON is useful for
processing and filtering the result in a script, e.g. using `jq`.
* `-v` / `--verbose` <br />
Show the full metadata associated with each database.
* `--include-backups` <br />
Also list databases in backup storage (note: may be slow).

### `glean create`

Create a new database.

* `--db NAME/INSTANCE` or `--db-name NAME --db-instance INSTANCE`<br />
Specifies the name and instance of the database
* `--finish`<br />
Also mark the DB as complete
* `--stacked DB`<br />
Create a stacked database on top of `DB`.
* `--property NAME=VALUE`<br />
Set properties when creating a DB
* `--update-schema-for-stacked`<br />
When creating a stacked DB, the schema is taken from the base DB. This
option specifies that the current schema should be used instead. When
using this option, creation will fail if the current schema has
a different definition for any predicate in the base DB schema;
therefore predicates may only be added or removed relative to the base DB.
* `FILE..`<br />
File(s) of facts to write into the database. Accepts JSON or compressed (zstd) JSONs.
See [Writing data to Glean](./write.md).

The schema for the new DB is given by:

* the property `glean.schema_id` if specified, or

* if `--stacked` or `--incremental`, then
  * if `--update-schema-for-stacked` is specified, then the default
    schema (or the one given by the `--schema` option),
  * otherwise, the schema from the base DB.

* otherwise the default schema, or the one given by the `--schema`
  flag.

Note that when creating a stacked DB, it is an error if the schema
does not agree with the schema in the base DB for any predicate that
has facts. That is, you cannot change the schema in a stacked DB for
existing facts in the base DB.

### `glean write`

Write facts to a database.

* `--db NAME/INSTANCE` or `--db-name NAME --db-instance INSTANCE`<br />
Specifies the name and instance of the database
* `FILE..`<br />
File(s) of facts to write into the database. Accepts JSON or compressed (zstd) JSONs.
See [Writing data to Glean](./write.md).
* `--finish`<br />
Also mark the DB as complete

### `glean finish`

Notify server that a database is complete.

* `--db NAME/INSTANCE` or `--db-name NAME --db-instance INSTANCE`<br />
Specifies the name and instance of the database

### `glean dump`

Dump the contents of the specified database into a file.

* `--db NAME/INSTANCE` or `--db-name NAME --db-instance INSTANCE`<br />
Specifies the name and instance of the database
* `FILE`<br />
File to write the facts into

### `glean delete`

Delete a database.

* `--db NAME/INSTANCE` or `--db-name NAME --db-instance INSTANCE`<br />
Specifies the name and instance of the database

### `glean derive`

Derive and store a predicate. See [Derived Predicates](derived.md).

* `--page-bytes BYTES`<br />
Maximum number of bytes per page

* `--page-facts FACTS`<br />
Maximum number of facts per page

* `PREDICATE`<br />
Predicates to derive

### `glean index`

Index some source code using one of the known indexers.

The form of the command in general is

```
glean index LANGUAGE DIR --db NAME/INSTANCE
```

There may also be additional options accepted for each `LANGUAGE`; try
`glean index LANGUAGE --help` to find out.

For information on each indexer, see [Indexers](./indexer/intro.md).

### `glean query`

Execute an Angle query and print the results, or write them to a file.

* `--db NAME/INSTANCE` or `--db-name NAME`<br />
Specifies the database to query

* `--page-bytes BYTES`<br />
Maximum number of bytes per page

* `--page-facts FACTS`<br />
Maximum number of facts per page

* `--expand`<br />
Fetch nested facts (slower)

* `--limit FACTS`<br />
Maximum number of facts to query (default: no limit)

* `-o,--output FILE`<br />
Output the facts to a file

* `--stats FILE`<br />
Output stats to a file ('-' for stdout)

* `--profile`<br />
Get full profiling information; use with `--stats` to include facts_searched

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

* `--db NAME/INSTANCE` or `--db-name NAME` and (`--db-instance INSTANCE` or `--date YYY-MM-DD` or `--latest`)

### `glean validate`

Perform checks on the internal integrity of a database. This is for
testing and debugging Glean itself.

 a local database

* `--db NAME/INSTANCE` or `--db-name NAME --db-instance INSTANCE`<br />
Specifies the name and instance of the database

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

* `--db NAME/INSTANCE` or `--db-name NAME --db-instance INSTANCE`<br />
Specifies the name and instance of the database

### `glean unfinish`

Unfinish a local database (turn it from complete to incomplete
state). This is for testing and development and not for routine use:
once a database is marked complete it could be replicated, so we
shouldn't be modifying it.

* `--db NAME/INSTANCE` or `--db-name NAME --db-instance INSTANCE`<br />
Specifies the name and instance of the database
