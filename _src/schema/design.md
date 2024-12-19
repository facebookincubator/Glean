---
id: design
title: Schema Design
sidebar_label: Design
---

import {SrcFile,SrcFileLink} from '@site/utils';

There are usually multiple ways to design a schema, and which one is
best will depend on multiple factors. Specifically, we usually want to
represent data so that it

* can be stored compactly,
* is convenient to generate,
* is convenient and efficient to query,
* and it supports incremental indexing.

In the following sections we'll go through some of the common choices
that you'll encounter when designing a schema and offer some advice.

Note: here we're only concerned with stored facts. The considerations
here don't apply to [On-demand derived predicates](../../derived/#on-demand-derived-predicates), because they aren't stored.

## Should we reference predicates directly?

For example, you could write a predicate for a class like this: **(1)**

```
predicate Class :
  {
    name : string,
    methods : [Method]
  }
```

or like this: **(2)**

```
predicate Class :
  {
    name : string,
  }

predicate ClassMethod :
  {
    class_ : Class,
    method : Method
  }
```

Which of these is best?

* **Functionality**
  * (1) retains the order of the methods, which might be
    important. Otherwise they're equivalent.
  * With (1) we have to know the methods when we generate the `Class`
    fact, whereas with (2) we can generate the facts about the methods
    separately and in any order. This might not matter much with
    something small like a class definition, but for larger facts
    (e.g. the definitions of a file) it could be important.

* **Data representation**
  * (1) has fewer facts per class, so is more compact (see [Using Arrays](#using-arrays) below).

* **Query performance**
  * it's faster to fetch the methods of a class with
    (1), because (2) requires searching two predicates.

* **Incrementality**
  * with (1), changing one method requires changing the
    whole `Class` fact, which might force changes to other facts. With
    (2) we would only need to replace the `ClassToMethod` fact.

## Use key-value predicates

We often have a choice between using key-only or key-value (also known as [Functional predicates](../../angle/advanced#functional-predicates)): **(1)**

```
predicate FunctionType : { function : Function, type_ : Type }
```

and: **(2)**

```
predicate FunctionType : Function -> Type
```

There are several tradeoffs here:

* **Functionality**
  * (1) is a relation, whereas (2) is a function. In practical terms,
    with (1) you can have many types for the same function, but with
    (2) that is an error (Glean will complain if you try to
    insert two facts with the same key and different values).

* **Data representation**
  * (2) is much more efficient to store. In particular the value is
    stored only once. If the value (`Type` in the above example) is large,
    you should strongly consider using a key-value predicate.

* **Query performance**
  * Both (1) and (2) support efficient querying by the key (`Function`
    in the example), and they both support slow filtering by the value
    (`Type`).

* **Incrementality**
  * These two alternatives are equivalent with respect to incrementality.

## Using arrays, sets or separate facts

If you're choosing between arrays and separate facts, then consider:

* Arrays are ordered lists, whereas facts are just sets. If the order
  of your items is important - because you're representing something
  that has an order, such as function arguments - then an array is the
  right choice.

* Conversely, if the order is *not* important, then sets are the natural
  choice. Using an array is
  a poor choice because you will be forced to choose an order when
  generating your data. If you don't have a deterministic way to pick
  the order, then your data representation is non-deterministic which
  leads to spurious differences in things like test outputs, which can
  be annoying.

* Arrays and sets are much more compact than multiple facts. There can be a
  huge difference in storage overhead; it's worth measuring this for
  your schema.

* When a client fetches an array or a set as part of the result of a query,
  they will get the whole array/set. If it is large, that may be a
  lot of data to send over the wire, and it might even result in an
  allocation limit error on the server, preventing the client from
  fetching the data at all. Facts tend to support incremental querying
  better compared with arrays and sets.

* Facts with large arrays/sets are also slower to search through in a query
  than smaller facts.

## Increase sharing

If there is duplication in the data stored in our facts, we can often
extract the common data into a predicate to increase sharing. One
example of this was described in [What is the difference between a predicate and a type?](schema/syntax.md#what-is-the-difference-between-a-predicate-and-a-type).

Choosing to use sets instead of arrays can increase sharing because sets have
a canonical representation.

## How to experiment with schema design

* Generate some data and see how large it is, using `:stat` in the shell.

* Write some example queries against your data, and check how much
  searching they do using `:profile` in the shell (see [Query
  Debugging](angle/debugging.md)).
