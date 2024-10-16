---
id: advanced
title: Advanced Query Features
sidebar_label: Advanced Query Features
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

## Types and signatures

Angle queries are *strongly typed*: the server will check your query for type-safety before executing it. Type-checking ensures that the query makes sense; that it's not trying to pattern-match strings against integers, or look for a field in a record that doesn't exist for example.

Angle's type-checker isn't very clever, though. It mostly doesn't do type *inference*, it checks that expressions have the intended type. When it doesn't know the intended type of an expression, it uses a dumb inference mode that can only infer the type when it's really obvious: like a fact match, or a string.

```lang=angle
facts> P where C = { name = "Fish" }; example.Parent { C, P }
can't infer the type of: {name = "Fish"}
    try adding a type annotation like ({name = "Fish"} : T)
    or reverse the statement (Q = P instead of P = Q)
```

In cases like this, Angle's type-checker needs a bit of help. We can use a *type signature* to supply more information about the type:

```lang=angle
facts> P where C = { name = "Fish" } : example.Class; example.Parent { C, P }
{ "id": 1024, "key": { "name": "Pet", "line": 10 } }
```

Here we used `{ name = "Fish" } : example.Class` to tell Angle the expected type of the pattern. You should read the colon as "has type", and the type can be any valid Angle type, for details see [Built-in types](../schema/types.md).

## Explicit fact IDs

Every fact has an ID, which is a 64-bit integer that uniquely identifies the fact in a particular database. You've probably noticed these fact IDs in the query results: every result has an `id` field with the fact ID, and a `key` field with the fact key.

Most Angle queries don't need to mention fact IDs explicitly, but sometimes it's useful. For example, you might need to perform a query to fetch some results, do some custom filtering on the results and then query Glean again using some of the fact IDs from the first query.

WARNING: a fact ID only makes sense in the context of a particular database, so make sure that your query that mentions fact IDs is being made on the same database that you obtained the fact ID from originally.

Glean has a syntax for referring to fact IDs directly; for example

```lang=angle
facts> $1026 : example.Class
{ "id": 1026, "key": { "name": "Fish", "line": 30 } }
```

the syntax is `$<fact ID>`, but you will often want to use it with a [type signature](#types-and-signatures), as `$<fact ID> : <predicate>`.

If you get the predicate wrong, Glean will complain:

```lang=angle
facts> $1026 : example.Parent
*** Exception: fact has the wrong type
```

The type can be omitted only if it is clear from the context, for example

```lang=angle
facts> example.Parent { child = $1026 }
{ "id": 1029, "key": { "child": { "id": 1026 }, "parent": { "id": 1024 } } }
```

Sometimes you might want to use multiple fact IDs in a query. Choice (`|`) comes in handy here:

```lang=angle
facts> example.Parent { child = $1026 | $1027 }
```

## Functional predicates

All the predicates we've seen so far have been key-only predicates. A predicate can also have a *value*; we call these *functional predicates* or *key-value predicates*.

For example, we might model a reference to a class in our example schema like this:

```lang=angle
predicate Reference :
  { file : string, line : nat, column : nat } -> Class
```

This says that for a given (file,line,column) there can be at most one reference to a Class.  This uniqueness is the important property of a key-value predicate: for each key there is at most one value.

We query for key-value predicates using this syntax:

```lang=angle
facts> C where example.Reference { file = "x", line = 1, column = 2 } -> C
```

The pattern after the `->` matches the value. It can be an arbitrary pattern, just like the key. Note that facts cannot be efficiently searched by value, so the pattern that matches the value is a filter only.
