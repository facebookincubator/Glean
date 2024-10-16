---
id: intro
title: Angle Introduction
sidebar_label: Introduction
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

Angle is Glean’s schema and query language.  It is a declarative query
language based on logic programming and Datalog, and is designed to be
particularly suited for finding and extracting data from Glean.

To give you a flavour of the query language, here is how we could
return the names of all the member declarations defined in a
JavaScript file `project/myfile.js`:

```
D.declaration.memberDecl?.name
  where
    D : flow.FileDeclaration;
    D.file = src.File "project/myfile.js"
```

To learn about Angle, start with the [Guide](guide.md).
