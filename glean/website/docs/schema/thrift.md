---
id: thrift
title: Thrift and JSON
sidebar_label: Thrift and JSON
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

The Glean schema is automatically translated into a set of Thrift type
definitions by the `gen-schema` tool (see [Workflow](workflow.md)).
These Thrift definitions can be used to work with Glean data in your
client, as native data types in whatever language you're using, either
for querying data or for writing facts.

The Thrift types also have a JSON representation, which can be read
and written directly. When you perform queries in the
[shell](../shell.md), the results are printed as JSON-encoded Thrift;
when you [write data to Glean](../write.md) it can be in the form of
JSON-encoded Thrift.

<FbInternalOnly>

Facebook internal: the Thrift types for the schema are automatically
generated into
[fbcode/glean/schema](https://phabricator.intern.facebook.com/diffusion/FBS/browse/master/fbcode/glean/schema), and those files are automatically sync'd to
www too.

</FbInternalOnly>

The relationship between schema types and Thrift/JSON is given by the following table:

|  Schema type | Thrift type | JSON |
|-------|-------|-------|
| `nat` | `Nat` (`i64`) | 123 |
| `byte` | `Byte` (`i8`) | 123 |
| `string` | `string` | "abc" |
| `bool` | `bool` | `true` or `false` |
| `[byte]` | `binary` | base-64 encoded string <sup>*1</sup> |
| `[T]` | `list<T>` |  [...] |
| `set T` | `list<T>` | [...] |
|`{`<br/>&nbsp;&nbsp;`f₁ : T₁,`<br/>&nbsp;&nbsp`...,`<br/>&nbsp;&nbsp;`fₙ : Tₙ`<br/>`}` | `struct Foo {`<br/>&nbsp;&nbsp;`1: T₁ f₁;`<br/>&nbsp;&nbsp;`...`<br/>&nbsp;&nbsp;`n: Tₙ fₙ;`<br/>`}` |  `{`<br/>&nbsp;&nbsp;`"f₁" : q₁,`<br/>&nbsp;&nbsp;`...`<br/>&nbsp;&nbsp;`"fₙ" : qₙ`<br/>`}` |
| `{`<br/>&nbsp;&nbsp;`f₁ : T₁ `<code>&vert;</code><br/>&nbsp;&nbsp;`... `<code>&vert;</code><br/>&nbsp;&nbsp;`fₙ : Tₙ`<br/>`}` | `union Foo {`<br/>&nbsp;&nbsp;`1: T₁ f₁;`<br/>&nbsp;&nbsp;`...`<br/>&nbsp;&nbsp;`n: Tₙ fₙ;`<br/>`}` |  `{ "f" : t }`<br/>for one of the fields `f₁`..`fₙ` |
| `maybe T` | In a record field:<br/> `optional T f` | `f : t`<br/> if the value is present |
| `enum {`<br/>&nbsp;&nbsp;`L₁`<code>&vert;</code><br/>&nbsp;&nbsp;`...`<code>&vert;</code><br/>&nbsp;&nbsp;`Lₙ`<br/>`}` | `enum Foo { `<br/>&nbsp;&nbsp;`L₁ = 1,`<br/>&nbsp;&nbsp;`...`<br/>&nbsp;&nbsp;`Lₙ = n`<br/>`}` | the index of the value,<br/> e.g. 12 |
| `predicate P : K -> V` | `struct P {`<br/>&nbsp;&nbsp;`1: Id id`<br/>&nbsp;&nbsp;`2: optional K key`<br/>&nbsp;&nbsp;`3: optional V value`<br/>`}`<br/>note<sup>*2</sup> | refer to fact N:<br/>`N` or `{ "id": N }`<br/>define a fact:<br/>`{ "id" : N,`<br/>&nbsp;&nbsp;&nbsp;`"key" : t }` or<br/>`{ "key": t }` or<br/>`{ "key": t,`<br/>&nbsp;&nbsp;&nbsp;&nbsp;`"value" : v }` |
| `type N = T` | depending on T: <br/>`struct N { .. }`<br/> `union N {...}`<br/> `enum N {...}`<br/>`typedef T N;` | same as type T |

1. The Thrift encoding of a binary field in JSON is a base-64-encoded string. However, not all Thrift implementations respect this. At the time of writing, the Python Thrift implementation doesn't base-64-encode binary values. For this reason we provide an option in the Glean Thrift API to disable base-64 encoding for binary if your client doesn't support it. The Glean Shell also uses this option to make it easier to work with binary.

2. the `key` is optional - a nested fact may be expanded in place or represented by a reference to the fact ID only. When querying Glean data the query specifies which nested facts should be expanded in the result, and when writing data to Glean using Thrift or JSON, we can optionally specify the value of nested facts inline.
