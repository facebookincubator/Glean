---
id: style
title: Angle Style Guide
sidebar_label: Style Guide
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

Typical Angle style uses the following rules:

* 2-column indentation
* trailing commas
* open/close braces on a line by themselves
* camel case for record field names

e.g.

```lang=angle
# Named parameter
type Parameter =
  {
    name : Name,
    type : Type,
    isVariadic : bool,
  }
```

This uses quite a lot of vertical space, but it's clear and works well with source control.

It's OK to put things on a single line if they fit:

```lang=angle
type Access = enum { public | protected | private }
```
