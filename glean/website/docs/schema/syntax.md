---
id: syntax
title: Syntax
sidebar_label: Syntax
---

Schemas are written in Glean's **Angle** language.

## Named schemas

We usually create a new named schema for a family of related types and
predicates. For example `cxx` is the schema for facts about C/C++ and
Objective C code, `java` is the schema for facts about Java code,
`python` is the schema for facts about Python code and so on.

```
schema java.1 {
# your types and predicates go here
}
```

:::note

**What does the .1 suffix mean?** This is the *version* of the
  schema. Versions are a somewhat legacy feature and you should
  normally leave everything at version 1.

:::

The name of the schema will be also used as the *namespace* for the
generated Thrift types. You'll notice that each schema gets a separate
generated [Thrift file](thrift.md) for working with the data in your code.

## Predicates

A predicate definition looks like this:

```
predicate P : KeyType
```
where

* `P` is the name of the predicate. e.g. `src.File`
* `KeyType` is the *key type*

For example, the example schema that we saw in the [Introduction](../introduction.md)
contains the `Class` predicate:

```
schema example.1 {
predicate Class :
  {
     name : string,
     line : nat,
  }
}
```

This defines:
* A predicate called `example.Class.1` (you can usually refer to it by its unversioned name, `example.Class`).
* With a key type that is a record, consisting of
   * a field `name`, that has type `string`
   * a field `line`, of type `nat`

A predicate can also have a value:

```
predicate P : KeyType -> ValueType
```

We sometimes call these key-value predicates. Most predicates you
encounter will only have a key.

## Referring to other predicates

A key feature of Glean is the ability to refer directly to other types
to build up nested structures.

To refer to a predicate defined in the same schema, we just use its
unqualified name:

```
schema example.1 {
predicate Class :
  {
     name : string,
     line : nat,
  }

predicate Parent :
  {
     child : Class,
     parent : Class,
  }
}
```

We can also refer to predicates defined in other schemas, but we have
to bring them into scope using an `import` declaration.  For example,
the `src` schema contains a `File` predicate:

```
schema src.1 {
predicate File : string
}
```

If we want to extend our `Class` predicate with the file that contains
it, we can do it like this:

```
schema example.1 {
import src.1

predicate Class :
  {
     name : string,
     line : nat,
     file : src.File
  }
}
```

Note that to refer to a predicate from another schema, we have to
qualify it with the name of its schema, as in `src.File` above. We
don't have to add the version though - there can only be one version
of any given predicate in scope at a time.

## Named types

It's useful to be able to name a type so that it can be used in multiple places. The form of a type definition is similar to a predicate:

```
type <name> = <type>
```

For example, the `src` schema has a type for source locations:

```lang=python
schema src.1 {
predicate File : string

# Common source code location type
type Loc = {
  file : File,
  line : nat,
  column : nat,
}
}
```

### What is the difference between a predicate and a type?

Predicates describe facts that are stored in the DB. A type has no
facts, it's just a name for a type. Types are expanded everywhere
they're mentioned.

Predicates are useful for increasing sharing. For example, if we have

```
predicate P: { a : string, b : [nat] }
```

and we notice that many `P` facts have the same array of `nat` for the
`b` field, we could store the data more compactly by making it a predicate:

```
predicate List: [nat]

predicate P: { a : string, b : List }
```

Now there will only be one fact for each unique value of `List` stored
in the DB. Using a `type` would not have the same effect:

```
type List: [nat]

predicate P: { a : string, b : List }
```

This is exactly equivalent to the original version; every `P` fact
will contain the full `[nat]`.

Finally, `type` declarations cannot be recursive or mutually
recursive. If you want a recursive type, the cycle must go through at
least one predicate. For more details, see [Recursion](recursion.md).

## Importing and inheriting

There are two ways for schemas to refer to each other:

### Importing

```
schema example.1 {
import src.1
...
}
```

The `import` declaration brings into scope all the predicates and
types defined by the named schema. Those predicates and types are
referred to by qualified names, e.g. `src.File` for the `File`
predicate defined by the schema `src`. The version number is dropped;
each schema exports a single version of a predicate or type, so the
unversioned name is unambiguous.

### Inheritance and revising schemas

:::warning

Schema inheritance is a legacy feature and may be removed in the
future. The process for safely changing schemas is described in
[Changing the Schema](changing.md).

:::

```
schema example.2 : example.1 {
predicate Class :
  {
       # new definition of Class
  }
}
```

Inheritance is useful for making changes to a schema by creating a new schema version:
* Inheriting from a schema brings into scope all the types and predicates of that schema, both qualified *and unqualified*.
* The new schema also exports all the types and predicates defined in the schemas it inherits from, *except those that are re-defined*.

Specifically, in the above example:
* We can `import example.2` anywhere and get all the predicates defined in `example.1`, except that we'll get the new `Class` defined in `example.2`.
* We can still `import example.1` and get the old version of the schema.

Note that if you have predicates that depend on a predicate that was revised in this way, you must also copy those predicates to the new schema, because the existing predicates will refer to the old version of the predicate you revised. (In due course Glean will probably provide a convenient way to do this; in the meantime you have to copy & paste. Not a big deal because you'll usually delete the old one at some point, and you can't modify it anyway.)

Named schemas can *not* form cycles through their `import` or
inheritance declarations.

## Naming rules and conventions

Names take the form of a dot-separated sequence of alphanumeric words. For example, `sys.Blob`, `clang.File`, or `cxx.objc.Name`. The words up to the last dot are the *namespace*, the final word is the *name*.

See [Names](../angle/reference.md#names) for full details.

Briefly:
* Namespaces (schema names) are dot-separated sequences of identifiers each beginning with a lower-case letter
* Names and namespaces can contain only alphanumeric characters, '_', or '.' (namespaces only)
* There is a set of reserved words that can't be used for names, e.g. `class`. Syncing the schema will fail with an error if you use a reserved word.
