---
id: migration
title: How do I migrate a schema?
sidebar_label: Migrating a schema
---

import ImgBefore from '@site/static/img/migration-before.png';
import ImgAfter from '@site/static/img/migration-after.png';

# Migrating schemas

TL;DR:

* Make only backwards compatible changes in schema migrations.
* Always update the indexer and schema at the same time.
* Use the `evolves` feature to guarantee clients don’t break during the migration.

* * *
Migrating schemas in Glean doesn’t need to be hard, but to ensure there will be no disruption of live services you need to consider multiple parts of the system.

## Guiding example

This guide will walk through an example migration and explain the main points to be taken into account in the process. It assumes you are familiar with the [`evolves` feature.](https://www.internalfb.com/intern/staticdocs/glean/docs/schema/changing/#schema-migrations-with-backward-compatible-changes)

To start suppose you use the following schema to represent classes and their methods.

```
schema code.1 {
    predicate Class : { name: string }
    predicate Method : { class : Class, name : string }
}
schema all.1 : code.1 {}
```

The goal will be to augment the representation to support storing whether a method is static or not.

To do that you will need to add a field to the `Method` predicate. This change requires you to create a new version of the schema (see [How do I change a schema](https://www.internalfb.com/intern/staticdocs/glean/docs/schema/changing/)). You will also need a new version of the `all` schema (see ***Version Resolution***).

```
schema code.2 {
    predicate Class : { name: string }
    predicate Method : { class : Class, name : string, static: bool }
}
schema code.2 evolves code.1
schema all.2 : code.2 {}
```

Keep in mind that `code.2 { predicate Method { class: Class, ..}}` is just syntactic sugar for `predicate code.Method.2 { class: code.Class.2, ...}` so when we say updating a schema, what we really mean is updating all the individual predicates in a schema. Each predicate is independent

## Changing the schema files

This `code` schema might live in some path in the project like `/schema/source/code.angle`.
It is worth having an archive folder such as `/schema/source/archive/` where old versions are moved to. We cannot remove an old schema version while there may still be clients using it.
By having an archive directory the original file can contain only the most current version of the schema and the changes between versions are shown much more clearly in version control.

## Deploying changes

You need a plan to deploy this update. Let’s start by looking at the initial state of the system.

<img src={ImgBefore} alt="System diagram - before migration"/>

The *Indexer* generates facts and sends them to the *Glean Write Server*. The write server will use the *Schemas* to determine the shape each saved fact should have and save them to a *Database*. The *Schemas* used during the creation of the *Database* will be saved into the database itself and will determine how the Query Server should interpret client queries.

In the diagram we can see that there are two types of clients:

* Clients that make versioned queries, that is with predicate versions (e.g. `code.Method.1 _`).
    * These are usually using a DSL such as the Hack or Haskell ones and use Thrift or a binary encoding of the results. If there is any unexpected bit in the result decoding will fail.
    * If a specific version is requested the server will only return facts with exactly the expected shape.
    * Although one can omit predicate versions in the Haskell and Hack DSLs these libraries will still create queries with explicit version numbers. The libraries use code generation and the version numbers are determined at compilation time.
* Clients that make unversioned queries, without predicate versions (e.g. `code.Method _`).
    * These are applications using clients without a DSL (like the Python client) or constructing their queries using strings, like in `glean shell`.
    * They receive results in JSON.
    * The server will determine the version to be used based on the standard ***Version Resolution*** procedure.

When you create a new schema version you also create new versions of all predicates inside that schema. If we are not careful in the process of updating the indexers and clients we could end up in a situation where clients are expecting predicates of one version and the server is operating on predicates of a different one.

This could lead to two problems when deploying changes:

1. Clients breaking with unexpected response content.
    1. A type changes in a backwards incompatible way in a schema migration and clients issuing unversioned queries start getting the new type with the unexpected shape.
2. Clients getting empty responses.
    1. Client requests `code.Method.1` but the db only has `code.Method.2` and `code.2` doesn’t evolve `code.1`
    2. Client requests `code.Method.2` but the database only has `code.Method.1` facts.

The way to avoid problem 1 is to never create backwards incompatible changes. If you need such a change it is best to create a schema with a different name and have the indexer create facts for both old and new schemas while all clients are being updated.

Problem number 2 can be eliminated by deploying new schemas, indexers and clients in the following order and using the `evolves` feature to guarantee that all clients work during the transition:

1. Stop running indexers using your schema.
2. Publish the new schema.
3. Deploy the updated indexer
4. Enable indexing using your schema again.
5. Wait until a database with the new schema is created.
6. Update clients.

Step 5 ensures that we don’t update clients before the server is using a database capable of answering their new queries.

The diagram below shows the intermediate state of the system after step 5 but before we have updated all clients. Changes are in bold. The blue text represents facts from the new schema transformed through the `evolves`  feature into the shape of facts from the old schema to answer versioned queries from non-updated clients.

<img src={ImgAfter} alt="System diagram - after migration"/>

Now that the server side is done clients can be updated at their leisure.

## Schema Dependencies

There might be many schemas that depend on `code.1`. If you need to update them to import `code.2` instead you will need to create new versions of them as well. You will need to update dependant schemas if:

* A schema uses predicates/types from the old schema in a predicate/type.

```
schema inheritance.1 {
   import code.1
   predicate Inherits : { child: code.Class, parent: code.Class }
}
```

* A schema use predicates/types from the old schema in the derivation of a stored predicate.

```
schema inheritance.roots.1 {
   import code.1
   predicate IsRoot : Class
        stored C where
            C = code.Class _ ;
            !(Inherits { child = C });
}
```

If a dependant schema only uses types from the old schema in the derivation of non-stored predicates there is no need to create a new schema version.

If you create a new version of a dependant schema you may also need to create new versions for the schemas that depend on it and so on. The criterion for whether that is needed is the same as the one above.

New versions of dependant schemas should evolve the old dependant schema version and should be added to the new `all` schema.

Let’s now look in a bit more detail into how clients issuing unversioned queries are affected by the migration.

## Version Resolution

To answer queries without explicit versions the server must determine a version to be used for each predicate and type in the query.

Each predicate has a version, which is derived from the name of its schema.

```
schema src.1 {
    predicate File : string
}
```

The schema above defines the predicate `src.File.1`.

When a query is sent to a Glean server it may or may not include the version of the predicate it wants.

* `src.File.1 _` - Query to fetch all facts from this exact predicate.
* `src.File _` - Omit predicate version number and let the server determine what version should be used.

### The `all` schema

To determine what predicate to return when the server receives an unversioned query Glean uses a special schema named `all`.

```
schema all.1 : src.1 {}
```

The statement above means that with `all.1` any unversioned predicate with the `src` qualification should be interpreted as being part of the `src.1` schema. That is, the query `src.File _` should be interpreted as `src.File.1 _`.

When there are multiple versions of the `all` schema available, the Glean server will apply these rules (in this order) to establish which one should be used:

* The version specified in the `schema_version` field of the query request. By default clients don’t specify this field.
* The latest version of `all` at the time the database was created. This is stored in the database property `glean.schema_version`, which can be seen with the command `:describe` in the shell.

Here is a full example. Suppose we create a database using a schema file with the following content:

```
schema src.1 {
    predicate File : string
}
schema src.2 {
    predicate File : { name : string, executable : bool }
}
schema all.1 : src.1 {}
schema all.2 : src.2 {}
```

The database will use `all.2` as its schema for version resolution and this is the outcome of the following queries:

* `src.File _` - Will yield results of type `src.File.2`.
* `src.File.2 _` - Will yield results of type `src.File.2`.
* `src.File "/tools"..` - Error. Because it resolves to `src.File.2` this is a type mismatch as the predicate content is a record and not a string.

### Conflict resolution

If we had

```
schema all.2 : src.1, src.2 {}
```

What would `src.File _` resolve to? When multiple versions are available in the `all` schema, the type or predicate with the highest version number is selected. In this case `src.File.2`.

