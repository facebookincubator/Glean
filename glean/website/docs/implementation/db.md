---
id: db
title: Database Representation
sidebar_label: Database Representation
---

Glean can use any embeddable key/value store, as long as it provides the following:

* multiple tables (in RocksDB this is "column families", in LMDB it is DBs within an "environment")
* table scanning, i.e. seek to the first key with a given prefix and iterate through subsequent keys in lexicographic order, both forwards and backwards
* concurrent reading and writing, and multiple concurrent readers. Multi-threaded writing is not required.
* saving and restoring the whole DB, while the DB is open for reading. This operation should be *fast*, not much more expensive than just copying the files. This is used by Glean to support backup and restore, and automatic fetching of DBs from remote storage.
* (nice to have) read-only access.
* (nice to have) data compression.

Glean currently supports two DB backends:

* [RocksDB](https://github.com/facebook/rocksdb/)
* [LMDB](https://www.symas.com/mdb)

The rest of this page goes into more detail about exactly how Glean uses the key/value store to store its data.

# Facts

A stored fact comprises

* A **Fact ID**

* A **Predicate ID**. Each stored predicate in the schema has a unique ID. These are assigned by Glean when the DB is created, and stored in the DB itself along with the full schema source.

* A **Key** and a **Value**, binary serialized representations of the fact's key and value.

# DB Tables

The important DB tables are

* **entities**, which maps fact IDs to (key,value) pairs. The encoding of fact IDs is designed so that iterating in lexicographic order returns facts in numeric order.
* **keys**, which maps fact keys to fact IDs.

There are various other tables:

* **admin**, stores various admin values about the DB, such as the DB version and the next unused fact ID.
* **meta**, arbitrary key/value store, used for example to store the schema.
* **stats**, keeps track of the amount of data stored for each predicate

And a number of tables used to keep track of fact ownership, which are described in [Incrementality](incrementality.md).

# Key sizes

Key/value stores typically have a limit on the size of a key. In RocksDB this is very large, so we don't worry about it. For LMDB the limit is much smaller - currently about 2kB - so we have to consider what to do in our *keys* table when the key is larger than the limit.

Let's look at a small example, suppose we have the following facts (ignoring predicate IDs for simplicity):

```
Fact ID   Key

1         abc
2         abcd
3         abcde
4         abcdef
5         abcdefg
6         abcdefgh
```

With RocksDB, our *keys* and *entities* tables would look like this:

```
Keys table

abc        -> 1
abcd       -> 2
abcde      -> 3
abcdef     -> 4
abcdefg    -> 5
abcdefgh   -> 6

Entities table

1 -> abc
2 -> abcd
3 -> abcde
4 -> abcdef
5 -> abcdefg
6 -> abcdefgh
```

Note that for each fact, the key is stored twice: once in the *keys* table and once in the *entities* table (although the *value* is stored only once). So this representation is rather wasteful of space, particularly for facts that have large keys.

Suppose the key size is restricted. We'll use a max key size of 4 for this example. Then our tables will look like this:

```
Keys table

abc        -> 1
abcd       -> 2  -.
abcd       -> 3   |
abcd       -> 4   | sorted by fact ID
abcd       -> 5   |
abcd       -> 6  -`

Entities

1 -> abc
2 -> abcd
3 -> abcde
4 -> abcdef
5 -> abcdefg
6 -> abcdefgh
```

For this representation to work, the DB must support storing multiple entries with the same key (LMDB does, given a particular flag). If the DB doesn't support this, then you can append the fact ID to the key to distinguish facts with the same key prefix.

Note that while small keys are stored twice, for larger keys only the prefix up to the DB's max key size is duplicated. This goes a long way towards addressing the earlier space wastage, so we might want to adopt this representation *even when the DB supports larger keys*.

Let's look at the operations we need to support and see how they map to this representation:

## Lookup by key

Glean needs to be able to retrieve a particular fact ID given its full key: `idByKey(key)`.

If the key we're looking up is larger than the max key size, then we'll need to do a linear search of all the keys with the same prefix, and for each entry in the keys table we look up the fact ID in the *entities* table to find the full key, and do a comparison. So this ends up being O(n). However, the claim here is that `n` is usually 1, or at least very small:

Claim:
 * `key[0..maxkey-1]` often uniquely determines the rest of the key, or
   narrows it down to a small number.
 * looking up by full key is uncommon during querying, it mostly happens during writing

What do we typically use large keys for?

 * `src.FileLines` - basically `(src.File, [nat])`, so usually the first field uniquely determines the fact.
 * `src.File : string`
   * looking up pathnames could exceed the key size, pathnames sometimes get long
   * but this probably  isn't a big issue in general.

## Iterators

An iterator works by starting from a key prefix: `seek(prefix)`

If `prefix` is larger than the max key size, then the iterator will traverse all the entries with the same `prefix[0..maxkey-1]`. For each entry we lookup the full key in the *entities* table, and check to see whether the key matches `prefix`, ignoring those entries that don't match.

Again, this case will typically be rare because key prefixes are usually small.

The other thing to consider here is that the iterator will not necessarily return facts in lexicographic order. Glean used to rely on this property, but we removed that requirement in order to support limited key sizes; see [this PR](https://github.com/facebookincubator/Glean/pull/647/).

Iterators for a stacked DB use an "append iterator" which iterates through the base DB in the stack first and then the stacked DB, so the iterator will first return all the facts from the base DB that match the prefix, followed by facts from the stacked DB.

## Restarting a fact iterator

For resuming queries, we need to be able to save an iterator and restart it when the query is resumed.

A saved iterator is `(Pid, Fact ID)`. This uniquely determines where to restart from, and we can find the start point fast by searching for the pair `(key, fact ID)` in the keys table. LMDB supports this operation with good performance, because it keeps multiple entries with the same key sorted by value and we can seek by both key and value. For a DB that doesn't support this, you could append the fact ID to the key as described above.

For a stacked DB, we know whether the fact ID is in the lower or the upper DB because we know the fact ID boundary between the two DBs.
