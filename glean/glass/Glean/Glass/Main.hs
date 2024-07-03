{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Glean.Glass.Main
  ( mainWith

  -- * Useful helpers
  , withEnv
  ) where

import Control.Concurrent (myThreadId)
import Control.Trace (traceMsg, (>$<))
import Data.Hashable (hash)
import Data.List (stripPrefix)
import Facebook.Service ( runFacebookService' )
import Facebook.Fb303 ( fb303Handler, withFb303 )
import System.Timeout
import TextShow
import Thrift.Channel (Header)
import Thrift.Protocol.ApplicationException.Types as Thrift
#ifdef FBTHRIFT
import qualified Thrift.Server.CppServer as Thrift
#else
import qualified Thrift.Server.HTTP as Thrift
#endif
import Util.EventBase ( withEventBaseDataplane )
import Util.Log.Text ( logInfo )
import Logger.IO (withLogger)

import Control.Exception (SomeException, fromException, throwIO)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative (Parser)

import qualified Glean
import Glean.Init ( withOptions )
import Glean.LocalOrRemote as Glean
    ( Service(Remote), withBackendWithDefaultOptions )
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Util.ConfigProvider
    ( ConfigProvider(defaultConfigOptions, withConfigProvider) )
import Glean.Util.Some ( Some(Some) )

import Glean.Glass.RepoMapping -- site-specific
import qualified Glean.Glass.Env as Glass
import Glean.Glass.Repos (withLatestRepos)
import qualified Glean.Glass.Options as Glass
import qualified Glean.Glass.Handler as Handler
import Glean.Glass.GlassService.Service ( GlassServiceCommand(..) )

import Glean.Glass.Types
  ( GlassException (GlassException, glassException_reasons),
    GlassExceptionReason (..))
import Glean.Glass.Env (Env'(tracer), Env)
import Glean.Glass.Tracer ( isTracingEnabled )
import Glean.Glass.Handler.Cxx as Cxx
import Glean.Glass.Tracing
  (GlassTrace(TraceCommand), GlassTraceWithId (GlassTraceWithId))

kThriftCacheNoCache :: Text
kThriftCacheNoCache = "nocache"

-- | Ok, go.
mainWith
  :: Parser (Glass.Config GlassTraceWithId -> Glass.Config GlassTraceWithId)
  -> IO ()
mainWith = withGlass runGlass

type Server trace = Glass.Env' trace -> Glass.Config trace -> IO ()

-- | Set up the resources we'll need
withGlass
  :: TextShow trace
  => Server trace -> Parser (Glass.Config trace -> Glass.Config trace) -> IO ()
withGlass f backendsP =
  withOptions (Glass.options backendsP) $ \config@Glass.Config{..} ->
  withEnv config Nothing $ \env -> f env config

-- | Construct a server environment
withEnv
  :: TextShow trace
  => Glass.Config trace
  -> Maybe Glean.Repo
  -> (Glass.Env' trace -> IO a)
  -> IO a
withEnv Glass.Config{..} gleanDB f =
  withEventBaseDataplane $ \evp ->
  withConfigProvider defaultConfigOptions $ \cfgapi ->
  withLogger cfgapi $ \logger ->
  withFb303 serviceName $ \fb303 ->
  withBackendWithDefaultOptions evp cfgapi gleanService (Just schema_id)
    $ \backend -> do
  scm <- sourceControl evp
  snapshotBackend <- snapshotBackend evp
  hst <- haxlState evp
  withLatestRepos backend scm (Just logger)
    (if isRemote gleanService then listDatabasesRetry else Nothing) refreshFreq
    $ \latestGleanRepos -> do
      repoMapping <- getRepoMapping
      f Glass.Env
        { gleanBackend = Some backend
        , gleanDB = gleanDB
        , snapshotBackend = snapshotBackend
        , sourceControl = scm
        , haxlState = hst
        , ..
        }

-- | Some of the retry/logging logic only applies to remote services
isRemote :: Service -> Bool
isRemote service = case service of
  Remote{} -> True
  _ -> False

-- | Kick off the server
runGlass :: Server GlassTraceWithId
runGlass res@Glass.Env{fb303, evp} conf@Glass.Config{..} = do

  logInfo =<< welcomeMessage evp conf

  let options = Thrift.defaultOptions
        { Thrift.desiredPort = Just listenPort
        , Thrift.numWorkerThreads = numWorkerThreads
        }
  runFacebookService' fb303 (glassHandler res) assignHeaders options

assignHeaders :: GlassServiceCommand r -> Either SomeException r -> Header
assignHeaders _ (Left e) | isRevisionNotAvailableException e =
  [ (encodeUtf8 kThriftCacheNoCache, "1")]
  where
    isRevisionNotAvailableException e = case fromException e of
      Just GlassException{glassException_reasons} ->
        all isRevisionNotAvailable glassException_reasons
      _ -> False
    isRevisionNotAvailable e = case e of
      GlassExceptionReason_exactRevisionNotAvailable{} -> True
      GlassExceptionReason_matchingRevisionNotAvailable{} -> True
      _ -> False
assignHeaders _ _ = []

-- | Perform an operation with the latest RepoMapping
withCurrentRepoMapping :: Glass.Env -> (Glass.Env -> IO a) -> IO a
withCurrentRepoMapping env0 fn = do
  current <- getRepoMapping
  fn (env0 { Glass.repoMapping = current })

withRequestTracing :: Env' GlassTraceWithId -> (Env -> IO b) -> IO b
withRequestTracing env k = do
  enabled <- isTracingEnabled
  if enabled
    then do
      -- Fix the trace id for this request traces.
      -- Since every request is handled by a fresh new thread,
      -- we reuse thread ids as trace ids
      tid <- tidToIndex <$> myThreadId
      k env{tracer = GlassTraceWithId tid >$< tracer env}
    else k env { tracer = mempty }
  where
    tidToIndex tid = case stripPrefix "ThreadId " (show tid) of
      Just n | [(i, "")] <- reads n -> i
      _ -> hash tid -- should be unreachable

-- Actual glass service handler, types from glass.thrift
-- TODO: snapshot the env, rather than passing in the mutable fields
--
glassHandler :: Glass.Env' GlassTraceWithId -> GlassServiceCommand r -> IO r
glassHandler env0 cmd =
  Glass.withAllocationLimit env0 $
  withRequestTracing env0 $ \env1 ->
  withCurrentRepoMapping env1 $ \env ->
  tracing env $
  withTimeout $ case cmd of
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

  -- C++/LSP specific
  FileIncludeLocations r opts -> Cxx.fileIncludeLocations env r opts
  ClangUSRToDefinition r opts -> fst <$> Cxx.clangUSRToDefinition env r opts

  where
    tracing env
      | SuperFacebookService{} <- cmd = id
      | otherwise = traceMsg (tracer env) (TraceCommand cmd)

    withTimeout act = do
      -- workaround for lack of request cancellation on client-side timeout
      result <- timeout clientTimeoutMs act
      case result of
        Nothing ->
          throwIO $
            Thrift.ApplicationException
              "glass server timeout"
                Thrift.ApplicationExceptionType_Timeout
        Just r -> return r

    clientTimeoutMs =
      -- TODO get the client timeout from the Thrift request
      30 * 1000000
