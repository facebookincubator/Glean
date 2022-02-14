{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module Glean.Glass.Config
  (
    defaultConfigKey,
    defaultPort,
    defaultServiceName,
    defaultRefreshFreq,
  ) where

import Data.Text (Text)
import Glean.Util.Time ( DiffTimePoints, minutes )

defaultPort :: Int
defaultPort = 26073

defaultConfigKey :: Text
defaultConfigKey = "glean/glass/glass_server"

defaultServiceName :: Text
defaultServiceName = "glean_glass"

defaultRefreshFreq :: DiffTimePoints
defaultRefreshFreq = minutes 5
