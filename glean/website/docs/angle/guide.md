---
id: guide
title: Angle Guide
sidebar_label: Guide
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';


The following guide will explain Angle from first principles, leading you through from simple queries to more complex ones.

If you want to try the examples for yourself, or experiment with
changes to the example schema, you should first follow the
instructions in [Walkthrough](../walkthrough.md) to get set up.

<FbInternalOnly>

There are also [examples of using Angle](https://www.internalfb.com/intern/wiki/Glean/Query/Angle/Angle_examples/) to query real data.

</FbInternalOnly>

## Just the facts

Data in Glean is described by a *schema*, which we normally put in a file with the extension `angle`. For the purposes of this guide we’ll use the example schema in `example.angle`. Full details about defining schemas can be found in [Schemas](../schema/basic.md). The `example.angle` file contains a schema definition like this:

```lang=angle
schema example.1 {

# definitions go here

}
```

This says we’re defining a schema called `example`, with version 1.

The schema contains definitions for *predicates*. A predicate is the type of *facts*, which are the individual pieces of information that Glean stores.  Our example schema models a simplified class hierarchy for an object-oriented language, starting with a predicate for a `Class`:

```lang=angle
predicate Class :
  {
     name : string,
     line : nat,
  }
```

This says that the facts of `Class` are records with two fields, a `name` field which contains a `string`, and a `line` field which contains a `nat` (“nat” is short for “natural number”, which is limited to 64 bits in Glean).

The simplest type of Angle query is one that just selects facts from
the database that match a pattern.  For our first Angle query, let’s
find a class by its name:

```
facts> example.Class { name = "Pet" }
{ "id": 1024, "key": { "name": "Pet", "line": 10 } }

1 results, 1 facts, 4.61ms, 117632 bytes, 677 compiled bytes
```

(The last line contains statistics about query performance from Glean; I’ll leave this out in the rest of the examples.)

What’s going on here?

* The query consists of the *predicate name* `example.Class` followed by a *pattern* `{ name = "Pet" }`
* Note that when we refer to a predicate in a query, the name is *qualified* by prefixing the schema name, so it’s `example.Class` rather than just `Class`.
* The query returns all the facts of `example.Class` that match the pattern


The shell shows results in JSON format. When you’re making Glean
queries from code, the results will normally be decoded into native
data types that you can manipulate directly in whatever language
you’re using; for more details see [Thrift and
JSON](../schema/thrift.md).

Note that each fact has a unique `id`. This is how Glean identifies facts in its database. As a user you normally won’t have to worry about fact `id`s; you can think of them like memory addresses.

The pattern specifies which facts to return. In the example above, our pattern is matching a record type and specifying a subset of the fields: just the `name` field. We could match the `line` field instead:

```lang=sh
facts> example.Class { line = 20 }
{ "id": 1025, "key": { "name": "Lizard", "line": 20 } }
```

:::note

Your patterns should normally match fields at the *beginning* of the
record, because facts in the database are indexed by a prefix of the
fields. Matching a field in the middle of the record works by scanning
all the facts, which could be expensive. We’ll get into this in more
detail in [Query Efficiency](efficiency.md).

:::

What other kinds of patterns can we use? Well, the simplest patterns are the wildcard, “_”, which matches anything, and "never", which always fails to match.

```lang=angle
facts> example.Class _
{ "id": 1026, "key": { "name": "Fish", "line": 30 } }
{ "id": 1027, "key": { "name": "Goldfish", "line": 40 } }
{ "id": 1025, "key": { "name": "Lizard", "line": 20 } }
{ "id": 1024, "key": { "name": "Pet", "line": 10 } }
facts> example.Class never
(no results)
```

We’ll introduce more kinds of pattern in the following sections. The full list of patterns can be found in [Angle Reference](reference.md).

## Matching nested facts

The real power of Glean comes from relationships between facts. Facts can refer directly to other facts, and we can write queries that directly match on these connections.

Our example schema has a predicate that expresses the inheritance relationship between classes:

```lang=angle
predicate Parent :
  {
     child : Class,
     parent : Class,
  }
```

Let’s find what `Fish` inherits from:

```lang=angle
facts> example.Parent { child = { name = "Fish" }}
{
  "id": 1029,
  "key": { "child": { "id": 1026, "key": { "name": "Fish", "line": 30 } }, "parent": { "id": 1024, "key": { "name": "Pet", "line": 10 } } }
}
```

Let’s break this down.

* `{ child = { name = "Fish" }}` is a pattern that matches the key type of `Parent`
* So, looking at the schema, `{ name = "Fish" }` is a pattern that should match the `Class` in the field `child`.

By default Angle queries recursively expand facts in the results.  We can see in the above result that the `child` and `parent` fields contain the full facts they point to.  If we want the result to be “shallow”, meaning it contains just the facts that match and not the nested facts, we can ask Glean to not expand the content of those references. In the shell this is done by running the command `:expand off`:
```lang=angle
facts> :expand off
facts> example.Parent { child = { name = "Fish" }}
{ "id": 1029, "key": { "child": { "id": 1026 }, "parent": { "id": 1024 } } }
```

We can of course go the other way and find all the children of a class:

```lang=angle
facts> example.Parent { parent = { name = "Pet" }}
{
  "id": 1028,
  "key": {
    "child": { "id": 1025, "key": { "name": "Lizard", "line": 20 } },
    "parent": { "id": 1024, "key": { "name": "Pet", "line": 10 } }
  }
}
{
  "id": 1029,
  "key": {
    "child": { "id": 1026, "key": { "name": "Fish", "line": 30 } },
    "parent": { "id": 1024, "key": { "name": "Pet", "line": 10 } }
  }
}
```

But as before, note that this would be an inefficient query if we had a lot of data because the pattern is matching on the second field of `Parent` (namely `parent`). Later we’ll see how to make these queries more efficient using a derived predicate.

##  Union types

Our examples so far have dealt with record types. Glean also supports *union types*, also called *sum types*, which are used to express multiple alternatives. For example, let’s expand our schema to include class members which can be either a method or a variable:

```lang=angle
predicate Has :
  {
    class_ : Class,
    has : Member,
    access : enum { public_ | private_ },
  }

predicate Member :
  {
    method : { name : string, doc : maybe string } |
    variable : { name : string }
  }
```

The predicate `Has` maps a `Class` to a `Member` (with a `public_` or `private_` annotation), and a `Member` is either `method` or `variable`, with some associated data. Note that a `Class` might have more than one `Member`, which is fine: there can be multiple `Has` facts for a given `Class`.

:::note

The schema uses `class_` rather than `class` as a field name, because
`class` is a reserved word in Angle. Similarly, we added trailing
underscores to `public_` and`private_` for the same reason.

There are many such reserved words, which are reserved not because Angle uses them, but because they cause problems for code that is automatically generated from the schema. To avoid having too many ad-hoc language-specific naming rules, Glean prevents certain problematic names from being used in the schema. The Angle compiler will tell you if you try to use a reserved word.

:::

Let’s find classes that have a variable called `fins`:

```lang=angle
facts> example.Has { has = { variable = { name = "fins" }}}
{
  "id": 1036,
  "key": {
    "class_": { "id": 1026, "key": { "name": "Fish", "line": 30 } },
    "has": { "id": 1035, "key": { "variable": { "name": "fins" } } },
    "access": 1
  }
}
```

The key thing here is that we matched on `Member` which is a union type, using the pattern `{ variable = { name = "fins" }}`. A pattern to match a union type looks very much like a record pattern, but it can have only a single field, in this case either `variable` or `method`.

## Maybe

Glean has one built-in union type called `maybe`, which is useful when we want to have optional values in the data. It's used in our example schema to attach optional documentation to a class member:

```lang=angle
predicate Member :
  {
    method : { name : string, doc : maybe string } |
    variable : { name : string }
  }
```

The type `maybe string` behaves exactly as if it were defined as the union type `{ nothing | just : string }`.  That means we can write a pattern that matches it, exactly as we would write a pattern for `{ nothing | just : string }`:

Methods without documentation:

```
facts> example.Member { method = { doc = nothing } }
```

Methods with documentation:

```
facts> example.Member { method = { doc = {  just = _ }}}
```

## Choice

In a pattern we can express multiple alternatives by separating patterns with a vertical bar `|`.

For example, we can find classes on lines 20 or 30:

```lang=angle
facts> example.Class { line = 20 | 30 }
{ "id": 1025, "key": { "name": "Lizard", "line": 20 } }
{ "id": 1026, "key": { "name": "Fish", "line": 30 } }
```

Or we can find all the classes that have either a `method` called `feed` or a `variable` with any name:

```lang=angle
facts> example.Has { has = { method = { name = "feed" }} | { variable = _ }}

(results omitted)
```

## Variables and more complex queries

So far we’ve seen how to query for facts by matching patterns, including matching nested facts.  In this section we’ll see how to construct more complex queries that combine matching facts from multiple predicates.

Suppose we want to find all the parents of classes that have a variable called `fins`. We need to build a query that will

* find the classes with a variable called `fins` using `example.Has` as we did above
* find their parents using `example.Parent`

We can combine these two as follows:

```lang=angle
example.Has
  {
    class_ = C,
    has = { variable = { name = "fins" }}
  };
example.Parent { child = C }
```

:::note

I’ve written this on several lines with indentation to illustrate it
better, to do this in the shell you will need to use the `:edit`
command to put the query in a temporary file.

:::

The key thing here is that we used a *variable* `C` to stand for the `class_` field when matching facts of `example.Has`, and then we searched for `example.Parent` facts with the same value of `C` for the `child` field.

Note that variables must *always* begin with an upper-case letter, while schema names (`example)` and field names (`child`) begin with a lower-case letter.

The semicolon separates multiple *statements* in a query. When there are multiple statements the results of the query are the facts that match the last statement, in this case the `example.Parent`.  Let’s try it:

```lang=angle
facts> example.Has { class_ = C, has = { variable = { name = "fins" }}}; example.Parent { child = C }
{
  "id": 1029,
  "key": {
    "child": { "id": 1026, "key": { "name": "Fish", "line": 30 } },
    "parent": { "id": 1024, "key": { "name": "Pet", "line": 10 } }
  }
}
```

 Suppose we don’t care too much about the child here, we only care about getting a list of the parents. We can avoid returning the redundant information by specifying explicitly what it is we want to return from the query:

```lang=angle
P where
    example.Has
      {
        class_ = C,
        has = { variable = { name = "fins" }}
      };
    example.Parent { child = C, parent = P }
```

The general form of the query is *`expression`* `where` *`statements`*, where *`expression`* is an arbitrary expression and each statement is a pattern that matches some facts. The results of the query are the distinct values of *`expression`* for which all the statements match facts in the database.

```lang=angle
facts> P where example.Has { class_ = C, has = { variable = { name = "fins" }}}; example.Parent { child = C, parent = P }
{ "id": 1024, "key": { "name": "Pet", "line": 10 } }
```

## Statements

In general, a statement can be of the form *A = B.* For example, if we write

```lang=angle
C = example.Class { name = "Fish" };
example.Parent { child = C }
```

that’s the same as

```lang=angle
example.Parent { child = { name = "Fish" }}
```

A statement can have a pattern on either side, for example

```lang=angle
C where
  C = example.Class { name = N };
  N = "Fish" | "Goldfish"
```

A statement can itself be a set of alternatives separated by a vertical bar `|`. For example, we can find classes that are either a parent of the `Goldfish` or have a `feed` method:

```lang=angle
C where
  example.Parent { child = { name = "Goldfish" }, parent = C } |
  example.Has { class_ = C, has = { method = { name = "feed" }}}
```

## Dot syntax

So far we've been extracting fields from records by writing patterns,
but we can also extract fields from records using the traditional "dot
syntax". For example, instead of

```
example.Parent { child = { name = "Fish" }}
```

we could write

```
example.Parent P where P.child.name = "Fish"
```

Here
* `example.Parent P` selects facts of `example.Parent` and binds the key to the variable `P`
* `P.child.name = "Fish"` is a constraint on the `name` field of the `child` field of `P`
* the query returns all facts of `example.Parent` that satisfy the constraint

Dot syntax tends to be more concise than patterns when there are
deeply-nested records, because it avoids all the nested braces.

### Dot syntax for union types

Matching union types can also be achieved using dot syntax. For
example, earlier we had

```
example.Has { has = { variable = { name = "fins" }}}
```

using dot syntax this would be

```
example.Has H where H.has.variable?.name = "fins"
```

Note that when selecting a union type we add a '?' suffix,
as with `.variable?` in the example above. This makes it more obvious
that we're doing something conditional: if `X.has` is not a
`variable`, then `X.has.variable?` has no values.

Selecting from union types works nicely with choice (`|`):

```
Name where
  example.Has H;
  Name = (H.has.variable?.name | H.has.method?.name)
```

returns all the names of variables and methods.

### Extracting the key of a fact

It's sometimes useful to be able to extract the key of a fact. If we
have a variable `X` of some predicate type, then we can extract the
key of `X` with `X.*`. If `X` has type `P` and `P` is a predicate with
key type `K`, then `X.*` has type `K`.

Usually we don't need to extract the key explicitly because Angle does
it automatically. For example if `X : example.Class`, then `X.name` is
shorthand for `X.*.name`. But sometimes we just want the key without
selecting a field, or perhaps the key isn't a record. In those cases,
`X.*` can be useful.

## If-then-else

We can conditionally match patterns using `if then else`.

Variables matched in the condition will be available in the `then` branch.

Whilst a choice will always evaluate both of its branches, the `else` branch of an if will
never be evaluated if the condition succeeds at least once.

For example, we could get all child classes if inheritance is being used in the codebase, or
retrieve all classes if it isn't.
```
facts > if (example.Parent { child = X }) then X else example.Class _
  { "id": 1025, "key": { "name": "Lizard", "line": 20 } }
  { "id": 1026, "key": { "name": "Fish", "line": 30 } }
  { "id": 1027, "key": { "name": "Goldfish", "line": 40 } }
```

Please note that `if` cannot be used in stored derived predicates. This
is the case because they require the use of negation, which is disallowed in
stored predicates.

## Arrays

When the schema uses an array, we need to be able to write queries that traverse the elements of the array. For example, a common use of an array is to represent the list of declarations in a source file. Our example schema defines the `FileClasses` predicate:

```lang=angle
predicate FileClasses :
  {
    file : string,
    classes : [Class]
  }
```

The goal here is to map efficiently from a filename to the list of classes defined in that file. Suppose we want to write a query that finds all the classes called `Goldfish` in the file `petshop.example`, we could do it like this:

```lang=angle
example.FileClasses { file = "petshop.example", classes = Cs };
{ name = "Goldfish" } = Cs[..]
```

The second line is the interesting one: `{ name = "Goldfish" } = Cs[..]` means

* on the right-hand side, `Cs[..]` means “each element of the array `Cs`”
* the left-hand side is a pattern, filtering only those `Class` facts that match `{ name = "Goldfish" }`


We can also match the whole array with a pattern of the form `[ p1, p2, .. ]`

```lang=angle
facts> X where [_,X,_] = [1,2,3]
{ "id": 1040, "key": 2 }
```

Or if we don't care about the length of the array:

```lang=angle
facts> X where [_,X, ..] = [1,2,3]
{ "id": 1040, "key": 2 }
```

## Sets

Sets are similar to arrays but helpful when the order of the elements are not important and duplicates are also irrelevant.
A common example is when storing cross references. For instance, the python schema has a predicate which contains all
name cross references in a file. The cross references are currently stored in an array but it could be stored in a set as below.

```lang=angle
predicate XRefsViaNameByFile:
    {
        file: src.File,
        xrefs: set XRefViaName,
    }
```

If we want to know for a particular file and a particular name, where it is used we could write the following query:

```lang=angle
XRefsViaNameByFile { file = "foo.py", xrefs = XRefs };
{ target = { name = "Bar" } } = elements XRefs
```

The second line uses the construct `elements` which is similar to the `[..]` syntax for arrays.

We can also create new sets from the results of a query. This is done using the `all` construct. For instance
`all (1 | 2 | 3)` is a set containing the number `1`, `2`, and `3`.

The `all` construct can be used in combination with the `elements` construct to, for instance, map over a set
of elements and transform them. In the example below, the second line takes each element of the `StringSet` and
applies the primitive `prim.toLower` to it. The result is a set where all the strings are lowercase.

```lang=angle
StringSet = all ("Foo" | "Bar" | "Baz" );
all (String = elements StringSet; prim.toLower String)
```

## String prefix

We’ve seen many examples of patterns that match strings. Glean also supports matching strings by *prefix*; for example:

```lang=angle
facts> example.Class { name = "F".. }
{ "id": 1026, "key": { "name": "Fish", "line": 30 } }
```

The syntax `"F"..` means *strings beginning with the prefix* `”F"`.

:::note

Why only prefix and not substring matching in general? Prefix matching can be supported efficiently by Glean’s prefix-tree representation of the fact database. Other kinds of string matching could be supported, but they wouldn’t be able to exploit the database representation so there’s little advantage to implementing them in Angle compared with filtering on the client-side.

:::

## Tuples

A *tuple* is just a a way of writing a record without the field names.  So for example, instead of

```
example.Parent { child = C }
```

we could write

```
example.Parent { C, _ }
```

When using a tuple you have to list *all* the fields, in the same order as they are declared in the schema.  That's why `{ child = C }` becomes `{ C, _ }` when written as a tuple.

There are upsides and downsides to using the tuple notation:
* Pro: more concise
* Con: brittle and sensitive to changes in the schema. If we add a field, then tuple patterns will break whereas record patterns won't.

As a rule of thumb we tend to use tuple syntax in cases where the predicate is "obviously" a relation, such as `example.Parent`, but we wouldn't use tuple syntax for more complex records.

## Enums and bool

An `enum` type is a set of named constants. In the `Has` predicate we used an `enum` type to indicate whether a class member is `public_` or `private_`:

```lang=angle
predicate Has :
  {
    class_ : Class,
    has : Member,
    access : enum { public_ | private_ },
  }
```

To match an `enum` we just use the appropriate identifier, in this case `public_` or `private`:

```lang=angle
facts> example.Has { access = private_ }
{ "id": 1036, "key": { "class_": { "id": 1026 }, "has": { "id": 1035 }, "access": 1 } }
```

Note that in the JSON format results, an `enum` is represented by an integer. When you make queries in code, the `enum` will be represented by an appropriate type, such as a `data` type in Haskell.

The boolean type `bool` is a special case of an `enum`, defined like this:

```lang=angle
type bool = enum { false | true }
```

:::note

Normally the constants of an `enum` should begin with a lower case
letter. You can use constants beginning with an upper-case letter, but
to distinguish the constant from a variable name you may sometimes
need to provide a type signature, e.g. `Constant : Type` rather than
just `Constant`.

:::

## Negation

If we want results that do not match a certain criterion, we can use `!` to
specify a subquery that should fail. A subquery fails if it doesn't return any
result.

For example, we can find classes that don't have methods

```lang=angle
facts> C where C = example.Class _; !(example.Has { class_ = C, has = { method = _ } })
{ "id": 1026, "key": { "name": "Fish", "line": 30 } }
{ "id": 1027, "key": { "name": "Goldfish", "line": 40 } }
{ "id": 1025, "key": { "name": "Lizard", "line": 20 } }
```

Or we could find the maximum element in an array

```lang=angle
facts> X where Values = [5,1,2,3]; X = Values[..]; !(Y = Values[..]; Y > X)
{ "id": 1091, "key": 5 }
```

The query asks for the `X` for which given all values of `Y` *none* is greater
than it.  If `Y = Values[..]` were outside of the negation, the meaning would
be give me all `X` for which there is *at least one* `Y` that is not greater
than it. The answer to that would be all elements.
