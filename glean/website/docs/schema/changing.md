---
id: changing
title: How do I change a schema?
sidebar_label: Changing a schema
---

:::important

Predicates are never modified. We can only make new versions of a
predicate, or delete an old version of a predicate when we no longer
need to read or write data using it.

:::

A schema is a contract between indexer, client and server about
the shape of facts. Schemas are used during the compilation of some clients to
generate code to build queries and decode facts. Because it is not possible to
update all running application clients and issue new databases in one atomic
operation, if we change the shape of a predicate type clients will suddenly
begin to create type-incorrect queries and become unable to decode facts.

Because of that you can only add new predicates or new versions of predicates, and
delete old ones. This is to ensure compatibilty between different
versions of clients and databases: adding new predicates to the schema
doesn't break existing clients or indexers.

To be specific, *modifying* a predicate means changing its type in any way. To modify a predicate you need to:

* Add a new version of the predicate, creating a new schema version at the same time if necessary.
  * This may entail adding new versions of other predicates too, because predicates that depended on the old version of the predicate must now be copied so that they can point to the new predicate you created.
* Update and recompile clients and indexers as necessary to use your new version. Most of the time we don't use explicit versions in client code, so usually updating a client is just a recompile after the schema update.

Changing the schema can present a tricky migration problem: there are indexers generating the data, clients reading the data, and existing databases that can contain either the old schema or the new schema. Glean provides features to make smooth migrations possible, see [Derived Predicates for Schema Migration](../derived.md#derived-predicates-for-schema-migration) and [Schema migrations with backward compatible changes](#schema-migrations-with-backward-compatible-changes)

:::note

if you're just changing the derivation of a derived predicate, there's no need to create a new predicate version. The new derivation will take effect, for both old and new databases, as soon as the schema change is deployed.

:::

### Adding new predicates

If you're just adding new predicates or types, then you don't need to add a new schema version.

### Deleting predicates

In most cases it's safe to delete predicates from the schema, provided you have no existing client code using them.

## Schema migrations with backward compatible changes

One of the challenges of migrations is that once you start producing databases with a new schema, clients which specify predicate versions in their queries will stop receiving results until they are updated to request the latest available version. If the client is updated first we have a similar problem; it will not receive results until databases with the latest schema are produced.

To allow old clients which specify predicate versions to still receive results when schemas are updated Glean supports **schema evolution**, a feature where facts of a new schema can be automatically transformed into facts of an older schema to be returned to old clients.

To use schema evolutions, all changes made in the new schema must be backward compatible. The following are the supported backward compatible changes:

- Add a field to a predicate/type
- Change field order in a predicate/type
- Change alternative order in a sum type or enum
- Add a predicate
- Remove a predicate

Changes that are not backward compatible are not supported, such as:
- Remove a field
- Change the type of a field

### Evolving schemas

The feature is enabled using a top-level directive

```
schema my_schema.2 evolves my_schema.1
```

This declaration has the effect of treating queries for `my_schema.1` predicates as if they were for `my_schema.2`. That is the query results will be retrieved from the database in the shame of a `my_schema.2` fact and transformed into a fact of the equivalent `my_schema.1` predicate specified in the query.

The new schema must contain all the predicates of the old schema, either with new versions or old versions, and their definitions must be backwards compatible. We can achieve this by copying the entire content of the old schema into the new one and modifying it there.

Schema evolutions only take effect if there are no facts of the old schema in the database; it is ignored otherwise.

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

