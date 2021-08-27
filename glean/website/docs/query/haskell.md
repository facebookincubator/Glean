---
id: haskell
title: Haskell Query API
sidebar_label: Haskell Query API
---

import {SrcFile,SrcFileLink} from '@site/utils';

To use the Haskell API:

```haskell
import Glean
import Glean.Angle
```

Or import qualified if you prefer:

```haskell
import qualified Glean
import qualified Glean.Angle as Angle
```

 * <SrcFileLink file="glean/hs/Glean/Query/Thrift.hs">Haskell query API</SrcFileLink>
 * <SrcFileLink file="glean/hs/Glean/Query/Angle.hs">Angle DSL</SrcFileLink> - a library for building type-safe Angle queries
 * <SrcFileLink file="glean/client/hs/example/Example.hs">Simple example client</SrcFileLink>
 * <SrcFileLink file="glean/demo/Hyperlink.hs">Example: Hyperlink: browse hyperlinked code</SrcFileLink>
 * <SrcFileLink file="glean/lib/Glean/Search/Search.hs">Example: Search for definitions by name</SrcFileLink>
