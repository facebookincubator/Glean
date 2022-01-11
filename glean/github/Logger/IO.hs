{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Logger.IO (module Logger.IO) where

import Glean.Impl.ConfigProvider

data Logger = Logger

withLogger :: ConfigAPI -> (Logger -> IO a) -> IO a
withLogger _ f = f Logger
