---
id: all
title: The special "all" schema
sidebar_label: The special "all" schema
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

<FbInternalOnly>

There is a special schema known as the `all` schema, defined in <SrcFile file="glean/schema/source/facebook/schema.angle" />.

</FbInternalOnly>

<OssOnly>

There is a special schema known as the `all` schema, defined in <SrcFile file="glean/schema/source/schema.angle" />.

</OssOnly>

The `all` schema is used to resolve names to particular predicates and
types when the version in omitted. This matters in a few places:

* When Thrift types are generated from the schema, the types and
  predicates from the `all` schema determine which names are
  unversioned in the generated Thrift. For example, if the `all`
  schema contains `src.1`, then the predicate `src.File.1` will be
  simply called `File` in the generated Thrift; otherwise it would be
  called `File_1`.

* When an [Angle query](../../angle/intro) mentions an unversioned predicate or type, the
  name is resolved to a particular version of that predicate or type
  using the current `all` schema.

* When deriving a predicate with the CLI tool, like `glean derive
  python.TargetUses`.

The `all` schema is defined like this:

```
schema all.6 :
  src.1,
  python.3,
  code.18
```

## Multiple versions of `all`

If you had to bump a schema version because you modified it, then you
can often just bump the schema version in the `all` schema too,
without creating a new version of `all`.

The exception to this is when you have clients that are using
raw Angle queries without versions. Imagine you have:

```
schema src.1 {
   predicate File {
     path : string
   }
}
```

and you add

```
schema src.2 {
   predicate File {
     path : string,
     is_generated: bool
   }
}

schema src.2 evolves src.1
```

and we update `all`:

```
schema all.6 :
  src.2,
  ...
```

Now, a client that makes a query like `src.File { path = "X" }` on an
old database with the `src.File.1` facts will get no results, because
`src.File` is resolved to `src.File.2` by the `all` schema.

Note that versioned clients--those clients using a query API that
automatically adds versions, like the Haskell DSL--can avoid this
problem. Clients that are already deployed when the change is made
will continue to work on both the old DBs and the new DBs, thanks to
the `evolves` declaration. However, after recompiling the client
against the new schema, the new client will only work with the new
DBs. So be careful to deploy the new DBs before recompiling and
deploying the new clients.

Back to the problem of unversioned clients. To ensure that these
clients can continue to work after we've added `src.2` and started to
create DBs that contain it, we need to first make a new version of
`all` that contains `src.2`. Keep the original version of `all`, like
this:

```
schema all.6 :
  src.1,
  ...

schema all.7 :
  src.2,
  ...
```

Next, ensure that queries are resolved by the appropriate version of
`all`: the old one for old DBs, and the new one for new DBs. We do
that by setting the `glean.schema_version` property in the DB when it
is created. Setting the property can be done with the CLI tool:

```
glean set-property --repo <name>/<hash> glean.schema_version=7
```

or it can be done via the API when creating a DB programmatically.

To be precise, the version of `all` used to resolve a name is chosen
as follows:

* For generating the Thrift definitions from the schema, it is the
  `--version` flag to the `gen-schema` tool.

* For an Angle query, the `all` version is chosen, in order of
  priority, by:
  * the optional `schema_version` field in the `UserQuery` struct from
    the query request itself; or
  * the `--schema-version` flag to the server; or
  * the `glean.schema_version` property in the database being
    queried.

When creating a database, it's recommended to set the
`glean.schema_version` property to the version of the schema
corresponding to the data being stored, so that clients using
unversioned queries will automatically resolve to the right versions.
