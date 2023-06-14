---
id: all
title: The special "all" schema
sidebar_label: The special "all" schema
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

<FbInternalOnly>

There is a special schema known as the `all` schema, defined in <SrcFile file="glean/schema/source/facebook/schema.angle" />.

</FbInternalOnly>

<OssOnly>

There is a special schema known as the `all` schema, defined in <SrcFile file="glean/schema/source/schema.angle" />.

</OssOnly>

The `all` schema is used to resolve names to particular predicates and
types when the version in omitted. This matters in a few places:

* When Thrift types are generated from the schema, the types and
  predicates from the `all` schema determine which names are
  unversioned in the generated Thrift. For example, if the `all`
  schema contains `src.1`, then the predicate `src.File.1` will be
  simply called `File` in the generated Thrift; otherwise it would be
  called `File_1`.

* When an [Angle query](../../angle/intro) mentions an unversioned predicate or type, the
  name is resolved to a particular version of that predicate or type
  using the current `all` schema.

* When deriving a predicate with the CLI tool, like `glean derive
  python.TargetUses`.

The `all` schema is defined like this:

```
schema all.6 :
  src.1,
  python.4,
  code.18
```

:::note

**Multiple versions of `all`**

This is a rather specialised scenario, but it might be necessary to
have multiple versions of the `all` schema during a schema
migration. We can code-generate the Thrift types from each version of
`all` separately, and clients can select at build time which version
they want to use. This enables incremental migration of code from one
schema to another schema.

:::
