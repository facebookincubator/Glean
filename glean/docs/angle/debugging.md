---
id: debugging
title: Debugging
sidebar_label: Debugging
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

Typically you want to do most of your debugging in the
[shell](../shell.md), where you can experiment with queries quickly and
easily.

If you're writing particularly complex queries, then consider using [Derived Predicates](../derived.md) to structure your query and to allow parts of the query to be re-used. To iterate on derived predicates, see [How do I write and test a derived predicate?](../derived/#how-do-i-write-and-test)

## Debugging a slow query

Performance debugging can be tricky, because Angle is a very declarative language. There are often many ways to write the query that are correct, but not all of them will be fast.

The shell provides a few facilities to help with this.

```
> :profile full
```

Turning on query profiling allows you to see how many facts of each predicate are being searched by your query. For example:

```
fbsource> search.cxx.SearchByNameAndScope { name = "Future" }
...
Facts searched:
                cxx1.RecordDeclaration.1 : 103
             cxx1.TypeAliasDeclaration.2 : 11
                            cxx1.QName.1 : 8
              cxx1.VariableDeclaration.2 : 7
                  cxx1.EnumDeclaration.1 : 7
                             cxx1.Name.1 : 1
```

If your query is expensive, then likely you will see some large numbers next to one or more predicates. This is a sign that you probably want to reorder the statements in your query, or lift out some nested queries into statements so that you can control the ordering more precisely.

```
> :debug on
```

## Showing the internals

The shell provides ways to show what Glean's query engine is doing internally. This is mostly useful for those working on the query engine itself, but it might also be helpful when debugging queries.

:::warning

We provide no guarantees about this functionality and it might change
without warning.

:::

 ```lang=sh
 > :debug ir
 ```

Shows the internal representation of the query after parsing, name resolution, type checking, and various transformations to simplify it.  In particular, all the nesting has been flattened at this stage, so you can see the exact order of the searches on each predicate, which might help with performance debugging.

```lang=sh
> :debug bytecode
```

Shows the compiled bytecode for the query. This is what Glean's virtual machine (VM) will execute to perform the query. Probably not all that useful for debugging queries.
