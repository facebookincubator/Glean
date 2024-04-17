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
    defaultListDatabasesRetry,
    defaultWelcomeMessage
  ) where

import Data.Text (Text)
import Glean.Util.Time
import Glean.Glass.Env
import Util.Text (textShow)

defaultPort :: Int
defaultPort = 26073

defaultConfigKey :: Text
defaultConfigKey = "glean/glass/glass_server"

defaultServiceName :: Text
defaultServiceName = "glean_glass"

defaultRefreshFreq :: DiffTimePoints
defaultRefreshFreq = minutes 5

defaultListDatabasesRetry :: Int
defaultListDatabasesRetry = 5

defaultWelcomeMessage :: Config a -> Text
defaultWelcomeMessage Config{..} = mconcat
          [ "glass"
          , ": port " <> textShow listenPort
          , ", config " <> configKey
          ]
