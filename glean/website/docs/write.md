---
id: write
title: Writing data to Glean
sidebar_label: Writing data to Glean
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import Backup from './fb/backup.md';
import {SrcFile,SrcFileLink} from '@site/utils';

This page describes the various ways in which data gets into Glean.

<FbInternalOnly>

For a complete walkthrough of the steps necessary to write an indexer, see [How to write a Glean indexer](https://www.internalfb.com/intern/wiki/Glean/How_to:_write_a_Glean_indexer/).

</FbInternalOnly>

A database can be created by a client using any of these methods:

1. Programmatically, using one of the APIs listed in [APIs for Writing](#apis-for-writing).
2. On the command line: invoke the `glean` command-line tool to send data in JSON format, see [ Creating a database using the command line](#creating-a-database-using-the-command-line).
3. In the shell, use `glean shell --db-root=<dir>` and then use the command `:load` to create a DB from a JSON file. See [Loading a DB from JSON in the shell](#loading-a-db-from-json-in-the-shell).

<FbInternalOnly>

After the data is ingested by the write tier (`glean.write`), it is backed up and copied to the read tier (`glean`) for efficient access. For newly created DB names, check out [the section below](https://www.internalfb.com/intern/wiki/Glean/Write/#configuring-db-backup-an) for configuring this behavior.

</FbInternalOnly>

## APIs for writing

<FbInternalOnly>

* The C++ writing API is the most performant. It is used by the clang-based indexer for C++  and Objective C code.  See [glean/cpp/glean.h](https://phabricator.intern.facebook.com/diffusion/FBS/browse/master/fbcode/glean/cpp/glean.h)
* In Hack, [genKickOff](https://www.internalfb.com/intern/codex/symbol/php/Glean/genKickOffForHandle/) and the various functions for writing facts.

</FbInternalOnly>

* The Haskell API for writing
   * Example: <SrcFile file="glean/client/hs/example/ExampleWriter.hs" />

If none of the above work for you, the Thrift API enable basic write
access to the database.

* `kickOff` can be used to create a new DB
* `sendJsonBatch` is for sending facts in JSON-serialized form
* `finishBatch` exposes the result of a previously sent JSON batch
* `finish` closes a DB

A rough outline of a client looks like:

```
glean = make_glean_thrift_client()
glean.kickOff(my_repo)
for json_batch in json_batches:
    handle = glean.sendJsonBatch(json_batch)
    result = glean.finishBatch(handle)
    # handle result
glean.finish(my_repo)
```
## Writing from the command line

### JSON format

The JSON format for Glean data is described in [Thrift and JSON](schema/thrift.md).

Here's an example of JSON data for writing to Glean:

```
[
  { "predicate": "cxx1.Name.1",          # define facts for cxx1.Name.1
    "facts": [
      { "id": 1, "key": "abc" },         # define a fact with id 1
      { "id": 2, "key": "def" }
    ]
  },
  { "predicate": "cxx1.FunctionName.1",  # define facts for cxx1.FunctionName.1
    "facts": [
      { "id": 3,
        "key": {
          "name": { "id": 1 }}}          # reference to fact with id 1
    ]
  },
  { "predicate": "cxx1.FunctionQName.1", # define facts for cxx1.FunctionQName.1
    "facts": [
      { "key": {
        "name": 3,                       # 3 is shorthand for { "id": 3 }
        "scope": { "global_": {} } } },
      { "key": {
        "name": {
          "key": {                       # define a nested fact directly
            "name": {
              "key": "ghi" }}},         # another nested fact
        "scope": {
          "namespace_": {
            "key": {
              "name": {
                "key": "std" }}}}}
    ]
  }
]
```
The rules of the game are:

* Predicate names must include versions, i.e. `cxx1.Name.1` rather than `cxx1.Name`.
* The `id` field when defining a fact is optional. The `id` numbers in the input file will *not* be the final `id` numbers assigned to the facts in the database.
* There are no restrictions on `id` values (any 64-bit integer will do) but an `id` value may not be reused within a file.
* Later facts may refer to earlier ones using either `{ "id": N }` or just `N`.
* It is only possible to refer to `id`s from facts in the same file, if you are writing multiple files using `glean write` or via the `sendJsonBatch` API.
* a nested facts can be defined inline, instead of defining it with an `id` first and then referencing it.
* an inline nested fact can be given an `id` and referred to later.

### Loading a DB from JSON in the shell

The shell is useful for experimenting with creating a DB from JSON data directly. Let's try loading the data above into a DB in the shell:

```
$ mkdir /tmp/glean
$ glean shell --db-root /tmp/glean
Glean Shell, dev mode
type :help for help.
no fbsource database availabe
> :load test/0 /home/smarlow/test
I0514 01:19:37.137109 3566745 Work.hs:184] test/16: database complete
```
Let's see what facts we loaded:

```
test> :stat
1
  count: 72
  size:  5988
cxx1.FunctionName.1
  count: 2
  size:  66
cxx1.FunctionQName.1
  count: 2
  size:  70
cxx1.Name.1
  count: 4
  size:  148
cxx1.NamespaceQName.1
  count: 1
  size:  35
test>
```
Note that there were 4 `cxx1.Name.1` facts - some of those were defined as inline nested facts in the JSON. We can query them all:

```
test> cxx1.Name _
4 results, 1 queries, 4 facts, 0.22ms, 44296 bytes

{ "id": 1096, "key": "abc" }
{ "id": 1097, "key": "def" }
{ "id": 1100, "key": "ghi" }
{ "id": 1102, "key": "std" }
```
Note that the `id` values here do not correspond to the `id` values in the input file.

### Creating a database using the command line

The `glean` command-line tool can be used to create a database directly on the server.

<FbInternalOnly>

There is a default retention policy for databases created this way; for details and to discuss your requirements, talk to the Glean team before creating databases.

</FbInternalOnly>

To create a database from a single file of JSON facts:

```
glean create --service <write-server> --finish --db <name>/<instance> <filename>
```
where

<FbInternalOnly>

* `<write-server>` can be `glean.write.test` for testing. `glean.write` is the production write tier.

</FbInternalOnly>

<OssOnly>

* `<write-server>` is the `host:port` of the Glean server

</OssOnly>

* `<name>` is the name for your DB. For indexing repositories we normally use the name of the repository, but it's just a string, so you can use whatever you want.
* `<hash>` identifies this particular instance of your database. For repositories we normally use the revision hash, but, again, it's just a string.
* `<filename>` the file containing the JSON facts.

If the file is more than, say, 100MB, this operation will probably time out sending the data to the server. To send large amounts of data you need to batch it up into multiple files, and then send it like this:

```
glean create --service <write-server> --db <name>/<hash>
glean write --service <write-server> --db <name>/<hash> <filename1>
glean write --service <write-server> --db <name>/<hash> <filename2>
...
glean finish --service <write-server> --db <name>/<hash>
```
To find out if your DB made it:

```
glean shell --service <write-server> :list
```
This will list the DBs available on the write server.


<Backup />
