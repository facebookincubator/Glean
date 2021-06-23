---
id: basic
title: Basic Concepts
sidebar_label: Basic Concepts
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

This section explains the concepts behind how Glean stores data, and
how to define a schema to describe your data.

<FbInternalOnly>

For a more formal description, see [Glean, a little formally](https://fb.workplace.com/notes/roman-leshchinskiy/glean-a-little-formally/504151247034061/).

</FbInternalOnly>

A Glean Database consists of a set of ***Facts***. Facts are unique;
each fact is stored only once.

The ***Schema*** describing the database is a set of
***Predicates***. You can think of the predicates as the types of the
facts. Each fact is an instance of one predicate.

A predicate looks like this:

```
predicate P : KeyType -> ValueType
```
where

* `P` is the name of the predicate. e.g. `src.File`
* `KeyType` is the *key type*
* `ValueType` is the *value type*

`KeyType` and `ValueType` are types; the type language is described in
[Built-in Types](types).

Every *fact* in predicate P has:

* A ***Fact ID***: a unique 64-bit integer that identifies the fact
* A ***Key***, a term of type `KeyType`
* A ***Value***, a term of type `ValueType`

(we use "term" here to avoid confusion with the use of "value" in a predicate).

Often the value is empty, represented by the empty record `{}`. Most
predicates have all their information in the key.

You can think of Glean as like a key/value store: we can look up a key
`K` in predicate `P` and get back value `V.` We can also query for
*patterns* that match multiple keys, and get back all the facts that
match the pattern. More about this when we talk about [Angle](angle)
queries.
