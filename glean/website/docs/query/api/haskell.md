---
id: haskell
title: Haskell Query API
sidebar_label: Haskell
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

### Using Haxl

Glean comes with a [Haxl](https://github.com/facebook/Haxl/) API for performing queries. The advantages of using the Haxl API are:

* Queries can be performed concurrently without using explicit threads
  or `Control.Concurrent.Async`. Applicative combinators such as
  `sequence` or `mapM` are performed concurrently in the `Haxl` monad,
  and GHC's `ApplicativeDo` extension can be used to enable implicit
  concurrency within a sequence of statements in a `do` expression.

* When traversing the results of a query, instead of using the
  `expand` query modifier to recursively fetch the entire result,
  Haxl provides a family of `get` operations to iteratively fetch
  nested facts in the result.  When performed concurrently in the
  `Haxl` monad, multiple calls to `get` are batched into a single
  request to Glean. This makes it efficient to do shallow queries and
  then selectively traverse and expand the results as needed.

To use the API, import <SrcFileLink
file="glean/client/hs/Glean/Haxl.hs">Glean.Haxl</SrcFileLink>. The
implementation of the API is in <SrcFileLink
file="glean/haxl/Haxl/DataSource/Glean.hs">glean/haxl/Haxl/DataSource/Glean.hs</SrcFileLink>.
