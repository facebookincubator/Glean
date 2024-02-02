{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module Derive.Env
  ( Env(..)
  , withEnv
  , withEnvWriter
  , withEnvWriters
  , queryEnv
  )
where

import Glean
import Glean.Remote
import Glean.Util.Some
import Glean.Write.SimpleAsync

import Derive.Types

data Env = Env
  { envConfig :: Config
  , envBackend :: Some Backend
  , envSender :: Sender
  , envWriterSettings :: WriterSettings
  }

withEnv
  :: Backend e
  => Config
  -> [SchemaPredicates]
  -> e
  -> (Env -> IO a)
  -> IO a
withEnv cfg allPredicates be action =
  withSimpleSender be (cfgRepo cfg) allPredicates (cfgSendQueue cfg)
    $ \sender -> action Env
    { envConfig = cfg
    , envBackend = Some (backendRetryReads be defaultRetryPolicy)
    , envSender = sender
    , envWriterSettings = cfgWriter cfg
    }

withEnvWriter :: Env -> (Writer -> IO a) -> IO a
withEnvWriter env = withSimpleWriter (envSender env) (envWriterSettings env)

withEnvWriters :: Env -> Int -> ([Writer] -> IO a) -> IO a
withEnvWriters env = withSimpleWriters (envSender env) (envWriterSettings env)

queryEnv :: forall p. Predicate p => Env -> Query p
queryEnv env =
  maybe id limit (cfgMaxQueryFacts (envConfig env)) $
  limitBytes (cfgMaxQuerySize (envConfig env))
  allFacts
