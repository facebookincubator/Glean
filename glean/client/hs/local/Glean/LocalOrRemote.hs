{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

--
-- | Connecting to a Glean server or a local database store.
--
-- Client code that wants to be able to use either a remote
-- Glean server or a local database store should look like:
--
-- > import Glean hiding (options)
-- > import Glean.LocalOrRemote
-- > import Glean.Util.ConfigProvider
-- > import Glean.Impl.ConfigProvider
-- > import Util.EventBase
-- >
-- > main :: IO ()
-- > main =
-- >   withConfigOptions options $ \(service, cfgOpts) ->
-- >   withEventBaseDataplane $ \evb ->
-- >   withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
-- >   withBackendWithDefaultOptions evb cfgAPI service $ \backend -> do
-- >     ...
--
module Glean.LocalOrRemote
  (
    -- * Connecting to a remote server or a local database store
    options
  , withBackendWithDefaultOptions
  , defaultClientConfigSource
  , Service(..)
  , Logging(..)
  , LocalOrRemote(..)
  , LoggingBackend(..)
  , BackendKind(..)

  -- * Misc
  , validate
  , Validate(..)
  , computeOwnership
  , dumpJsonToFile
  , finalize
  , serializeInventory
  , sendJsonBatch
  ) where

import Control.Monad.Extra
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import System.IO
import Text.Printf

import qualified Glean
import Glean.Backend
import Glean.Dump
import Glean.Database.Validate
import Glean.Database.Work (finalizeWait)
import Glean.Write.JSON (syncWriteJsonBatch)

-- | Write facts to a file in JSON format suitable for parsing using
-- 'parseJsonFactBatches'.
--
dumpJsonToFile
  :: Backend b
  => b
  -> Glean.Repo
  -> FilePath
  -> IO ()
dumpJsonToFile backend repo file =
  withFile file WriteMode $ \hdl -> do
    notFirst <- newIORef False
    hPutStrLn hdl "["
    dump backend repo (withBatch hdl notFirst)
    hPutStrLn hdl "]"
  where
    withBatch hdl notFirst Glean.JsonFactBatch{..} = do
      whenM (readIORef notFirst) $ hPutStr hdl ","
      writeIORef notFirst True
      let Glean.PredicateRef{..} = jsonFactBatch_predicate
      hPrintf hdl "{ \"predicate\": \"%s.%d\", \"facts\": [\n"
        predicateRef_name predicateRef_version
      BC.hPutStrLn hdl (BC.intercalate ",\n" jsonFactBatch_facts)
      hPutStrLn hdl "]}"

finalize :: LocalOrRemote backend => backend -> Glean.Repo -> IO ()
finalize backend repo =
  case backendKind backend of
    -- finalizeWait is faster than polling if we have local DBs.
    BackendEnv env -> finalizeWait env repo
    _ -> Glean.finalize backend repo

-- | Version of sendJsonBatch that uses a synchronous write rather
-- than polling when we're using a local backend.
sendJsonBatch
  :: LocalOrRemote be
  => be
  -> Glean.Repo
  -> [Glean.JsonFactBatch]
  -> Maybe Glean.SendJsonBatchOptions
  -> IO ()
sendJsonBatch backend repo batches opts = do
  case backendKind backend of
    -- syncWriteJsonBatch is faster than polling if we have local DBs.
    BackendEnv env -> syncWriteJsonBatch env repo batches opts
    _ -> void $ Glean.sendJsonBatch backend repo batches opts
