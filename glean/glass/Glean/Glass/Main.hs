{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Glean.Glass.Main
  ( main

  -- * Useful helpers
  , withEnv
  ) where


import Facebook.Service ( runFacebookService' )
import Facebook.Fb303 ( fb303Handler, withFb303 )
import Thrift.Channel (Header)
#ifdef FBTHRIFT
import qualified Thrift.Server.CppServer as Thrift
#else
import qualified Thrift.Server.HTTP as Thrift
#endif
import Util.EventBase ( withEventBaseDataplane )
import Util.Log.Text ( logInfo )
import Util.Text ( textShow )
import Logger.IO (withLogger)

import Control.Exception (SomeException, fromException)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)

import qualified Glean
import Glean.Init ( withOptions )
import Glean.LocalOrRemote as Glean
  ( Service(..),
    LocalOrRemote(..),
    BackendKind(..),
    withBackendWithDefaultOptions )
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Util.ConfigProvider
    ( ConfigProvider(defaultConfigOptions, withConfigProvider) )
import Glean.Util.Some ( Some(Some) )
import Glean.Util.Time ( DiffTimePoints )

import Glean.Glass.RepoMapping -- site-specific
import qualified Glean.Glass.Env as Glass
import Glean.Glass.Repos (withLatestRepos)
import qualified Glean.Glass.Options as Glass
import qualified Glean.Glass.Handler as Handler
import Glean.Glass.GlassService.Service ( GlassServiceCommand(..) )

import Glean.Glass.SnapshotBackend ( snapshotBackend, SnapshotTier )
import Glean.Glass.Types (RevisionNotAvailableException(..))

kThriftCacheNoCache :: Text
kThriftCacheNoCache = "nocache"

-- | Ok, go.
main :: IO ()
main = withGlass runGlass

type Server = Glass.Env -> Glass.Config -> IO ()

-- | Set up the resources we'll need
withGlass :: Server -> IO ()
withGlass f =
  withOptions Glass.options $ \config@Glass.Config{..} ->
  withEnv serviceName gleanService snapshotTier
      configKey refreshFreq listDatabasesRetry Nothing $ \env -> f env config

-- | Construct a server environment
withEnv
  :: Text
  -> Glean.Service
  -> SnapshotTier
  -> Text
  -> DiffTimePoints
  -> Maybe Int
  -> Maybe Glean.Repo
  -> (Glass.Env -> IO a)
  -> IO a
withEnv name service snapshotTier _ refreshFreq listDatabasesRetry gleanDB f =
  withEventBaseDataplane $ \evp ->
  withConfigProvider defaultConfigOptions $ \cfgapi ->
  withLogger cfgapi $ \logger ->
  withFb303 name $ \fb303 ->
  withBackendWithDefaultOptions evp cfgapi service (Just schema_id)
    $ \backend ->
  withLatestRepos backend (Just logger)
    (if isRemote service then listDatabasesRetry else Nothing) refreshFreq
    $ \latestGleanRepos repoScmRevisions -> do
      repoMapping <- getRepoMapping
      f Glass.Env
        { gleanBackend = Some backend
        , gleanIndexBackend = indexBackend backend
        , snapshotBackend = snapshotBackend snapshotTier
        , gleanDB = gleanDB
        , ..
        }

-- | Some of the retry/logging logic only applies to remote services
isRemote :: Service -> Bool
isRemote service = case service of
  Remote{} -> True
  _ -> False

-- | Construct a backend to call the indexing endpoint
indexBackend :: LocalOrRemote b => b -> Glass.IndexBackend
indexBackend b = Glass.IndexBackend $ case backendKind b of
  BackendEnv _ -> Nothing
  BackendThrift b -> Just b

-- | Kick off the server
runGlass :: Server
runGlass res@Glass.Env{..} conf@Glass.Config{..} = do

  logInfo =<< welcomeMessage conf

  let options = Thrift.defaultOptions
        { Thrift.desiredPort = Just listenPort
        , Thrift.numWorkerThreads = numWorkerThreads
        }
  runFacebookService' fb303 (glassHandler res) assignHeaders options

assignHeaders :: GlassServiceCommand r -> Either SomeException r -> Header
assignHeaders _ (Left (fromException -> Just RevisionNotAvailableException{})) =
  [ (encodeUtf8 kThriftCacheNoCache, "1")]
assignHeaders _ _ = []

welcomeMessage :: Glass.Config -> IO Text
welcomeMessage Glass.Config{..} = do
  return $ Text.concat
    [ "glass"
    , ": port " <> textShow listenPort
    , ", config " <> configKey
    ]

-- | Perform an operation with the latest RepoMapping
withCurrentRepoMapping :: Glass.Env -> (Glass.Env -> IO a) -> IO a
withCurrentRepoMapping env0 fn = do
  current <- getRepoMapping
  fn (env0 { Glass.repoMapping = current })

-- Actual glass service handler, types from glass.thrift
-- TODO: snapshot the env, rather than passing in the mutable fields
--
glassHandler :: Glass.Env -> GlassServiceCommand r -> IO r
glassHandler env0 cmd = withCurrentRepoMapping env0 $ \env -> case cmd of
  SuperFacebookService r -> fb303Handler (Glass.fb303 env) r

  -- Listing symbols in files
  DocumentSymbolListX r opts -> Handler.documentSymbolListX env r opts
  DocumentSymbolIndex r opts -> Handler.documentSymbolIndex env r opts

  -- Navigating
  FindReferences r opts -> Handler.findReferences env r opts
  FindReferenceRanges r opts -> Handler.findReferenceRanges env r opts

  -- Resolving symbol information
  ResolveSymbolRange r opts -> Handler.resolveSymbolRange env r opts

  -- Symbol info
  DescribeSymbol r opts -> Handler.describeSymbol env r opts

  -- Search for symbols
  -- by string
  SearchSymbol r opts -> Handler.searchSymbol env r opts

  -- by relationship
  SearchRelated r opts req -> Handler.searchRelated env r opts req
  SearchRelatedNeighborhood r opts req ->
    Handler.searchRelatedNeighborhood env r opts req


  -- deprecated
  SearchBySymbolId r opts -> Handler.searchBySymbolId env r opts

  -- Create an incremental database
  Index r -> Handler.index env r

  -- C++/LSP specific
  FileIncludeLocations r opts -> Handler.fileIncludeLocations env r opts
  ClangUSRToDefinition r opts -> fst <$> Handler.clangUSRToDefinition env r opts
