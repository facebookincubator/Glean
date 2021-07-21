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

You can only add new predicates or new versions of predicates, and
delete old ones. This is to ensure compatibilty between different
versions of clients and databases: adding new predicates to the schema
doesn't break existing clients or indexers.

To be specific, *modifying* a predicate means changing its type in any way. To modify a predicate you need to:

* Add a new version of the predicate, creating a new schema version at the same time if necessary.
  * This may entail adding new versions of other predicates too, because predicates that depended on the old version of the predicate must now be copied so that they can point to the new predicate you created.
* Update and recompile clients and indexers as necessary to use your new version. Most of the time we don't use explicit versions in client code, so usually updating a client is just a recompile after the schema update.

Changing the schema can present a tricky migration problem: there are indexers generating the data, clients reading the data, and existing databases that can contain either the old schema or the new schema. Glean provides features to make smooth migration possible, see [Derived Predicates for Schema Migration](../derived.md#derived-predicates-for-schema-migration)

:::note

if you're just changing the derivation of a derived predicate, there's no need to create a new predicate version. The new derivation will take effect, for both old and new databases, as soon as the schema change is deployed.

:::

### Adding new predicates

If you're just adding new predicates or types, then you don't need to add a new schema version.

### Deleting predicates

In most cases it's safe to delete predicates from the schema, provided you have no existing client code using them.
