{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- |
-- Small domain-specific language for building Angle queries
-- programmatically.
--
module Glean.Angle (query, module Glean.Query.Angle) where

import Glean.Query.Angle
import Glean.Query.Thrift.Internal (query)
