---
id: types
title: Built-in Types
sidebar_label: Built-in Types
---

| Type | Meaning |
|-------|-------|
| <code>nat</code> | 64-bit natural numbers |
| <code>byte</code> | 8-bit natural numbers |
| <code>string</code> |  UTF-8 encoded strings |
| <code>[T]</code> | lists of elements of type T |
| <code>set T </code> | set of elements of type T |
| <code>{ field₁ : T₁, ..., fieldₙ : Tₙ }</code> | a record with zero or more named fields |
| <code>{ field₁ : T₁ &#124; ... &#124; fieldₙ : Tₙ }</code> | a sum (union) type with one or more named alternatives |
| <code>P</code> | a reference to a fact of predicate P |
| <code>bool</code> | the boolean type with values **true** and **false** |
| <code>maybe T</code> | an optional value of type T |
| <code>enum { name₁ &#124; ... &#124; nameₙ }</code> | exactly one of the symbols name₁..nameₙ |
