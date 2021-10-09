-- Copyright (c) Facebook, Inc. and its affiliates.

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
import Glean.FFI (withMany)
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
    , envBackend = Some be
    , envSender = sender
    , envWriterSettings = cfgWriter cfg
    }

withEnvWriter :: Env -> (Writer -> IO a) -> IO a
withEnvWriter env = withSimpleWriter (envSender env) (envWriterSettings env)

withEnvWriters :: Env -> Int -> ([Writer] -> IO a) -> IO a
withEnvWriters env n = withMany withEnvWriter (replicate n env)

queryEnv :: forall p. Predicate p => Env -> Query p
queryEnv env =
  maybe id limit (cfgMaxQueryFacts (envConfig env)) $
  limitBytes (cfgMaxQuerySize (envConfig env))
  allFacts
