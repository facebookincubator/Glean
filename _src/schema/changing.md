---
id: changing
title: How do I change a schema?
sidebar_label: Changing a schema
---

Glean supports modifying the schema directly, while providing
backwards compatibility between existing clients and data across the
schema change.

### Basic rules

To preserve compatibility between clients and data, the schema can
only be changed in **compatible** ways. This means:

* Adding or removing a field from a record, if the field has a **defaultable** type (see [Default values](#default-values))
* Adding or removing an alternative from a sum type
* Adding or removing a predicate or type declaration
* Changing a list to a set or vice versa

An example of an *incompatible* change would be changing the type of a
field, for example from `nat` to `bool`.

Of course it's fine to make arbitrary changes to a schema that you're
working on; the compatibility rules only come into effect when the
schema is landed. From that point there might be existing clients and
databases in use, so Glean will reject any incompatible schema changes.

<FbInternalOnly>
The compatibility check shows up in CI as `sync-cf-main-diff` and is
performed on any diff that changes a schema. It checks that the new
schema is compatible with the existing schema and all previous
versions of the schema still in use, so it will catch things like
removing a field and then re-adding it with a different type.
</FbInternalOnly>

### Compatibility

When the schema is changed, Glean supports any combination of old or
new clients (that is, clients built against the old or new version of
the schema) with old or new data.

* If the client requests a field that doesn't exist in the data, the field will be filled with its default value.
* If a query matches on an alternative that doesn't exist in the schema, the match will fail.
* If the data contains an alternative that doesn't exist in the client's schema, then the client will receive an `unknown` value (this is represented as an `EMPTY` constructor in the Thrift-generated datatypes).

### Default values

A **defaultable** type is any type that is not a predicate. So for
example, if `P` is a predicate type, then we could add a field `f :
maybe P` but not a field `f : P`. The reason for this restriction is
that Glean must be able to substitute a default value for the
field when a client is using the new schema but the data is using the
old schema, and there cannot be a default value for a predicate.

Default values for missing fields are determined according to the
following table:

| Type | Default value |
|-------|-------|
| <code>nat</code> | <code>0</code> |
| <code>byte</code> | <code>0</code> |
| <code>string</code> |  <code>""</code> |
| <code>[T]</code> | <code>[]</code> |
| <code> set T</code> | the empty set |
| <code>{ field₁ : T₁, ..., fieldₙ : Tₙ }</code> | <code>{ field₁ = default(T₁), ..., fieldₙ = default(Tₙ) }</code> |
| <code>{ field₁ : T₁ &#124; ... &#124; fieldₙ : Tₙ }</code> | <code>{ field₁ = default(T₁) }</code> |
| <code>bool</code> | <code>false</code> |
| <code>maybe T</code> | <code>nothing</code> |
| <code>enum { name₁ &#124; ... &#124; nameₙ }</code> | <code>name₁</code> |


### What if my schema changes are incompatible?

If you don't care about backwards compatibility, then an easy
workaround is to just bump the version of the whole schema, so for
example if you have

```
schema graphql.1 {
...
}
```

then change it to

```
schema graphql.2 {
...
}
```

and make whatever changes you need. The new version of the schema is
entirely separate from the old version as far as Glean is concerned,
so there are no restrictions on what changes can be made.

If you want to retain some backwards compatibility, then you can add
new predicates representing the new data. You have the option of

1. Writing both the old and the new data to the database
2. Producing two databases, one with the old data and one with the new data. The databases can be distinguished by different names or different properties.

With approach (1), you can migrate clients incrementally and then eventually
stop producing the old data. But the downside of this approach is that
the database may contain a lot more data than necessary; writing may
take a lot longer, and so on.

With approach (2), you first have to ensure clients select the old
database. Then as you migrate clients, change them to use the new data
and select the new database at the same time.

### How does it work?

A particular instance of the schema is identified by a
`SchemaId`. This is a hash value computed from the full contents of
the schema. The `SchemaId` of the current schema is available through
the `schema_id` value exported by the `builtin` schema (the
`Glean.Schema.Builtin.Types` module in Haskell).

The server keeps track of multiple instances of the schema in a
**schema index**. Each instance of the schema is identified by its
`SchemaId`. Each database also contains the schema that it was written
with. The schema index is manipulated using the `gen-schema` tool in
the Glean repository; to use a particular index we can use `--schema
indexfile:FILE` or `--schema indexconfig:CONFIG`.

A client sends its `SchemaId` to the server in the `schema_id` field
of the query. For Haskell clients this is done automatically: the
client passes its `SchemaId` when initialising the Glean client
library, and this `SchemaId` is passed on to the server for every
query.

The server knows which schema the client is using, so it can translate
the data in the database into the client's schema automatically.

When a database is created, the schema used by the database is chosen
by the `glean.schema_id` property set by the client creating the
database. Again for Haskell clients this happens automatically.

### Evolving schemas

Glean also allows backwards-compatibility between co-existing schemas,
which can be useful if you want to perform schema changes in a more
explicit way, or to rename schemas.

The feature is enabled using a top-level directive

```
schema my_schema.2 evolves my_schema.1
```

This declaration has the effect of treating queries for `my_schema.1` predicates as if they were for `my_schema.2`. That is the query results will be retrieved from the database in the shape of a `my_schema.2` fact and transformed into a fact of the equivalent `my_schema.1` predicate specified in the query.

The new schema must contain all the predicates of the old schema, either with new versions or old versions, and their definitions must be backwards compatible. We can achieve this by copying the entire content of the old schema into the new one and modifying it there.

Now what should Glean do when a client asks for a fact from an old schema?
- Answer with db facts from the old schema
- Answer with db facts from the new schema transformed into the old ones.

If there are no facts of the old schema in in the database we will take option 2.
If the database has any fact at all of the old schema we choose option 1.

That is, schema evolutions only take effect if there are no facts of the old schema in the database; it is ignored otherwise.

As an example suppose we start with the following schemas:

```
schema src.1 {
   predicate File {
     path : string
   }
}

schema os.1 {
  import src.1

  predicate Permissions {
    file : File,
    permissions : nat
  }
}

schema info.1 {
  import src.1

  predicate IsTemporary {
    file : File
  } F where F = src.File { path = "/tmp".. }
}
```

Now we want to make a backward-compatible change to `src.File` and add an `extension` field. We could add this to the file:

```
schema src.2 {
   predicate File {
     path : string,
     extension : string
   }
}

schema src.2 evolves src.1
```

Now if the indexer is still producing only `src.1` facts, all other predicates will work as before and queries for `src.File.2` will return no results.

Once the indexer is changed to produce only `src.2` facts queries like `src.File.1 _` will be fulfilled using data from the `src.2` schema, converting the `src.File.2` results to the shape of `src.File.1` before returning to the client.

This is also the case in the derivation query of `info.IsTemporary`. Although `info` imports `src.1`, the query will be transformed to use `src.2` facts.

On the other hand, `os.Permissions` will be empty. This must be the case because its first field references a `src.File.1` fact, of which there is none in the database. For this predicate to continue being available we must evolve its schema as well.

```
schema os.2 {             # changed
  import src.2            # changed

  predicate Permissions {
    file : File,
    permissions : nat
  }
}

schema os.2 evolves os.1    # changed
```
