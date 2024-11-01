---
id: shell
title: Using the Shell
sidebar_label: Using the Shell
---

The shell is an interactive tool in which you can

* Experiment with [Angle queries](angle/guide.md)
* Explore the data in a Glean database
* Experiment with schema changes and [derived predicates](derived.md)
* Create experimental databases and query them

## Invoking the shell

<FbInternalOnly>

```
glean shell
```

will start the shell, connect to the Glean query service and load up
the `fbsource` database.

</FbInternalOnly>

<OssOnly>

```
glean shell --service HOST:PORT
```

to connect to a server, or

```
glean shell --db-root DIR
```

to use local databases from directory `DIR`.

</OssOnly>

See [shell options](#shell-options) for more command-line options.

## Quick start

* List the available databases with `:list`
* Select a database with `:db NAME`
* See the contents of the database with `:stat`
* Type queries in [Angle](angle/guide.md) to see the results.

## Shell options

The shell accepts all the [common options](./running.md#common-options). Additionally:

* `QUERY` or `:COMMAND`<br />
Perform the given `QUERY` or `:COMMAND` and then exit. If multiple
commands or queries are given on the command line, they will be
performed in left-to-right order.
* `--db NAME` or `--db NAME/HASH`<br />
Load the database `NAME` or `NAME/HASH`.
* `--limit N`<br />
Set the default limit for queries; equivalent to the `:limit` command.
* `--width N`<br />
Set the terminal width for pretty-printing results.
* `--pager`<br />
Enable automatic paging of results longer than a page.
* `-v|--verbose N`<br />
Enabled verbosity at level `N`

# Commands

Note that you can abbreviate commands as long as the abbreviation is
unique. For example, `:edit` can be abbreviated as `:e`.

* `:database NAME` or `:database NAME/HASH`<br/>
Use database `NAME` or `NAME/HASH`.
* `:list NAME`<br />
List available databases which match `NAME`.
* `:list-all NAME`<br />
List available databases, and restorable backups, which match `NAME`.
* `:index LANGUAGE DIR`<br/>
Index some source code for `LANGUAGE` in directory `DIR`, creating a
new database. This command is only available with the `--db-root`
option. Currently the only supported languages are `flow` and `hack`.
* `:debug off|[-]ir|[-]bytecode|all`<br/>
Enable query debugging; `:debug ir` shows the intermediate
representation of the query after optimisation; `:debug
bytecode` shows the compiled bytecode.
* `:describe NAME`<br />
Like `:list`, but show more details
* `:describe-all NAME`<br />
Like `:list-all`, but show more details
* `:schema [PREDICATE|TYPE]`<br />
Show schema for the given `PREDICATE` or `TYPE`, or the whole schema
if no predicate or type is given.
* `:use-schema [current|stored|<schema-id>]` <br />
With no arguments, shows the available schemas and the current setting.
With an argument, selects the schema to use for queries and inspecting with the `:schema`
command:
  * `current`: selects the current schema.
  * `stored`: selects the schema stored in the current DB.
  * `<schema-id>`: selects the specified schema.
* `:edit`<br />
Edit a query in an external editor. Set the `EDITOR` environment
variable to choose an editor.
* `:limit N`<br />
Set limit on the number of query results. If there are more results
than the limit, then you can type `:more` to fetch the next `N`
results.
* `:load [<file> | <db>/<hash> <file> ...]` <br />
Create a new DB, loading facts from the given file(s).
  * `<file>` is a file of facts in JSON format
  * `<db>/<hash>` optionally the name of the DB to create
* `:create [<db>/<hash>]` <br />
Create a new empty DB.
  * `<db>/<hash>` optionally the name of the DB to create
Set the query time budget. If the time limit expires, the results so
far are returned, and you can type `:more` to see more results.
* `:expand off|on`<br />
Recursively expand nested facts in the response. `on` by default.
* `:pager off|on`<br />
Enable/disable result paging.
* `:count QUERY`<br />
Show only a count of query results, not the results themselves
* `:more`<br />
Fetch more results from the previous query
* `:profile [off|summary|full]`<br />
Show query profiling information; see [Query Debugging](angle/debugging.md).
* `:reload`<br />
Reload the schema (when using `--schema`). This command is useful when
making changes to the schema, including [derived
predicates](./derived.md). Edit the schema source files, `:reload` and then
test your changes.
* `:statistics [PREDICATE]`<br />
Show statistics for the current database.
* `:quit`<br />
Leave the shell.
