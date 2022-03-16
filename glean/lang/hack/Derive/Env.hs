-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.


module Derive.Env
  ( Env(..)
  , withEnv
  , withEnvWriter
  ) where

import Glean (Backend)
import qualified Glean.Schema.Hack as Hack
import Glean.Util.Some
import Glean.Write.Async
import Glean.Write.SimpleAsync

import Derive.Types

data Env = Env
  { envConfig :: Config
  , envBackend :: Some Backend
  , envSender :: Sender
  , envWriterSettings :: WriterSettings
  }

withEnv :: Backend e => Config -> e -> (Env -> IO a) -> IO a
withEnv cfg be action =
  withSimpleSender be (cfgRepo cfg) [Hack.allPredicates] (cfgSendQueue cfg)
    $ \sender -> action Env
    { envConfig = cfg
    , envBackend = Some be
    , envSender = sender
    , envWriterSettings = cfgWriter cfg
    }

withEnvWriter :: Env -> (Writer -> IO a) -> IO a
withEnvWriter env = withSimpleWriter (envSender env) (envWriterSettings env)
