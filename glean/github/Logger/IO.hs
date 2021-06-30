-- Copyright (c) Facebook, Inc. and its affiliates.

module Logger.IO (module Logger.IO) where

import Glean.Impl.ConfigProvider

data Logger = Logger

withLogger :: ConfigAPI -> (Logger -> IO a) -> IO a
withLogger _ f = f Logger
