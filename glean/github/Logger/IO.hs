module Logger.IO (module Logger.IO) where

import Configerator

data Logger = Logger

withLogger :: ConfigAPI -> (Logger -> IO a) -> IO a
withLogger _ f = f Logger
