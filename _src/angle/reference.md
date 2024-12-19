---
id: reference
title: Angle Reference
sidebar_label: Reference
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

## Queries

A query produces a set of values. At the outermost level, the values
returned are always *facts*, which are returned to the client making
the query.

In general, a Glean query takes the form:

*query* ::= [ *term* `where` ] *statement₀* ; ...; *statementₙ*

You can think of this declaratively, as in

> For each substitution of the variables in the query such that *statement₀*..*statementₙ* holds, produce the value of *term*

Or, we can think of it more operationally, which helps with query optimisation:

> for each value of *statement₀*<br />
> ...<br />
> for each value of *statementₙ*<br />
> produce the value of *term*

If *term* `where` is omitted, then the query produces the values of the final statement. For example, a query `src.File "foo/bar"` is equivalent to `F where F = src.File "foo/bar"`.

Note that a query corresponds to a nested loop, where *statement₀* is the outermost loop, and *statementₙ* is the innermost. The ordering of the statements can therefore have a significant effect on performance.

## Statements

*statement* ::= [ *term₁* `=` ] *term₂*

> match all values of **term₁** against all values of **term₂**

The order is mostly irrelevant; `A = B` is equivalent to `B = A`, except that type inference works by inferring the right-hand-side before checking the left-hand-side so this may influence which order you want. You can also use a type signature (`A = B : type`) to help the type checker.

## Names

Glean uses the following classes of names:
* A *schema name*, e.g. `example.schema`, of the form *name*[.*name*]*. By convention, the components of a schema name begin with a lower-case letter.
* A *predicate name*, e.g. `example.schema.Predicate.1` of the form *schema*.*predicate*[.*version*]. By convention, *predicate* begins with an upper-case letter. The version can often be omitted, in which case it defaults depending on the context: in a query it defaults to the most recent version, in a schema there is always only one version of a predicate visible in any given scope.
* A *field name*, e.g. `declaration`, used to identify fields of a record, or alternatives of a sum type or enumeration.  A field name **must begin with a lower-case letter**.
* A *variable*, e.g. `X`. Variables **must begin with an upper-case letter** to distinguish them from field names.

There is a set of reserved words that can't be used for names. Mostly this is because those words would clash with reserved keywords in code that we generate from the schema, and we don't want to have to do any automatic translation of names that might be confusing. Typically the convention for avoiding these reserved words is to add an underscore to the name, e.g. `class_`.


## Term

A term may be fully defined, like `{ true, 123 }` (a value that we could insert in the database), or it can be partially defined, like `{ A, "b", _ }`.

A term is often matched against something that will instantiate its unknown variables. For example, in `cxx.Name X`, we're instantitating the variable `X` to each of the keys of the predicate `cxx.Name`.

Ultimately the result of a query must be terms that are fully defined, though. If this isn't the case, Glean's query engine will report an error.  For example, a query like `X where 123` doesn't make sense, because we haven't matched `X` with anything.

Terms have the following forms:

*term* ::=<br />
&nbsp;&nbsp;    *variable* <br />

> A **variable** names the terms that match at this position in the query. The variable can be mentioned elsewhere in the query; it doesn't usually make sense for a variable to be mentioned only once, since then you might as well just use a wildcard, see below.

&nbsp;&nbsp;  `_`<br />

> A wildcard; matches anything

&nbsp;&nbsp;  `never`<br />

> A pattern that always fails to match.

&nbsp;&nbsp;*predicate*&nbsp;*term* [ `->` *term* ] <br />

> All the facts of **predicate** with keys that match the first **term** (and values that match the second **term** if appropriate)

&nbsp;&nbsp;`(` *query* `)`

> All the values of **query**. Note in particular that **query** can just be a simple term, but it can also be something like **term** `where` **statements**.

&nbsp;&nbsp;*term* `[..]`

> All the elements of the array **term**

&nbsp;&nbsp;*term₁* `|` *term₂*

> When used as a pattern, matches **term₁** or **term₂**. When used as an expression, generates all values of **term₁** and all values of **term₂**.

> Note: variables mentioned in **term₁** and **term₂** are local to those terms, and may have different types, but only if the variable is not mentioned elsewhere.

&nbsp;&nbsp;`elements` *term*

> All the elements of the set **term**

&nbsp;&nbsp;`all` *query*

> Construct a set of all the results of **query**.

&nbsp;&nbsp;`!` *term*

> The negation of a term. Fails if the term matches anything and succeeds otherwise.

&nbsp;&nbsp;`if` *term₁* `then` *term₂* `else` *term₃*

> If *term₁* has matches the `then` branch is taken and *term₃* is never matched against. If *term₁* has no matches then the `else` branch is taken and *term₂* is never matched against.

&nbsp;&nbsp;  *[0-9]+*<br />

> a number matches a value of type `nat` or `byte`

&nbsp;&nbsp;  *string*<br />

> a string matches a value of type `string`

&nbsp;&nbsp; *string* `..`<br />

> matches strings with the given prefix

&nbsp;&nbsp; *string* `..` *term*<br />

> matches a prefix and the rest of the string

&nbsp;&nbsp; `{` *field* `=` *term* `,` ... `}`<br />

> matches a record with the given fields

&nbsp;&nbsp; `{` *field* `=` *term* `}`

> matches a sum type with an alternative **field**

&nbsp;&nbsp; *field*

> when matching a sum type, shorthand for `{` *field* `= _ }`

&nbsp;&nbsp; *enumerator*

> matches an value of an enumerated type

&nbsp;&nbsp; `{ just =` *term* `}`<br />
&nbsp;&nbsp; `nothing`

> matches a `maybe` type

&nbsp;&nbsp; `true`<br />
&nbsp;&nbsp; `false`

> matches a `boolean`

&nbsp;&nbsp; *term*&nbsp;:&nbsp;*type*<br />

> (a *type signature*) interpret **term** as having type **type**, where **type** is any valid Angle type.

&nbsp;&nbsp; `$` [0-9]+<br />

> matches a literal fact ID. The only reason to use these would be if you did a previous query, extracted some fact IDs, and want to do a subsequent query incorporating them. Literal fact IDs are not allowed in derived predicates (it wouldn't make any sense).

## Primitives

Angle supports a few primitive operations. The argument(s) to a primitive operation must always be fully defined; they cannot be patterns or wildcards.

&nbsp;&nbsp;`prim.toLower` (S : string) : string

> Converts its string argument to lower case

&nbsp;&nbsp;`prim.length` (A : [_]) : nat

> Equal to the number of elements in its array argument

&nbsp;&nbsp;`prim.size` (A : set _) : nat

> Equal to the number of elements in the set

&nbsp;&nbsp;*term* `>` *term* <br />
&nbsp;&nbsp;*term* `>=` *term* <br />
&nbsp;&nbsp;*term* `<` *term* <br />
&nbsp;&nbsp;*term* `<=` *term* <br />
&nbsp;&nbsp;*term* `!==` *term* <br />

> Standard numerical comparisons. These work on values of type `nat` only, and they have value `{}` if the comparison succeeds, otherwise they fail (in the same way as a predicate match fails if there are no facts that match the pattern).

&nbsp;&nbsp;*term* `!=` *term*

> Standard comparison between two terms of any type. It has a value of `{}` if the comparison succeeds, otherwise it fails in the same way as a predicate match fails if there are no facts that match the pattern.

&nbsp;&nbsp;`zip` (A : [a]) (B : [b]) : [{a,b}]

> Takes two arrays and zips them together pairwise into a new array of tuples.
If the arrays have different length, the result has the same length as the shorter input array.

&nbsp;&nbsp;`concat` (A : [a]) (B : [a]) : [a]

> Concatenates two arrays together

&nbsp;&nbsp;`reverse` (S : string) : string

> Reverses a string
