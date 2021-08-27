---
id: types
title: Built-in Types
sidebar_label: Built-in Types
---

## Built-in Types

| Type | Meaning |
|-------|-------|
| nat | 64-bit natural numbers |
| byte | 8-bit natural numbers |
| string |  UTF-8 encoded strings |
| [T] | lists of elements of type T |
| { field₁ : T₁, ..., fieldₙ : Tₙ } | a record with zero or more named fields |
| <code>{ field₁ : T₁ &#124; ... &#124; fieldₙ : Tₙ }</code> | a sum (union) type with one or more named alternatives |
| P | a reference to a fact of predicate P |
| bool | the boolean type with values **true** or **false** |
| maybe T | an optional value of type T |
| enum { name₁ &#124; ... &#124; nameₙ } | exactly one of the symbols name₁..nameₙ |
