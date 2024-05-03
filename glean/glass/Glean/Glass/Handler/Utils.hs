{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
module Glean.Glass.Handler.Utils (
    -- * Handler wrappers: handlers should call one of these
    withRepoFile,
    withRepoLanguage,
    withSymbol,
    withRequest,

    -- * deprecated handler wrappers
    withGleanDBs,

    -- * Utils for building handlers
    GleanBackend(..),
    backendRunHaxl,

    RevisionSpecifier(..),
    revisionSpecifierError,

    firstOrErrors,
    allOrError,
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch ( throwM )
import Data.Either
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty, toList )
import Data.Text ( Text )
import qualified Data.Text as Text

import Haxl.DataSource.Glean (HasRepo)
import Logger.GleanGlass ( GleanGlassLogger )
import Util.Logger ( loggingAction )
import Util.STM
import qualified Logger.GleanGlass as Logger
import qualified Logger.GleanGlassErrors as ErrorsLogger

import qualified Glean
import Glean.Haxl.Repos as Glean
import Glean.Util.Some

import Glean.Glass.Base
import Glean.Glass.Logging
import Glean.Glass.Repos
import Glean.Glass.SourceControl
import Glean.Glass.SymbolId
import Glean.Glass.Tracing
import Glean.Glass.Types

import qualified Glean.Glass.Env as Glass

-- | Given a query, run it on all available repos and return the first success
--   plus an explainer
firstOrErrors
  :: (HasRepo u => ReposHaxl u w (Either GlassExceptionReason a))
  -> ReposHaxl u w
      (Either (NonEmpty GlassExceptionReason) (a, QueryEachRepoLog))
firstOrErrors act = do
  results <- queryEachRepo $ do
    repo <- Glean.haxlRepo
    result <- act
    return $ (repo,) <$> result
  let (fail, success) = partitionEithers results
  return $ case success of
    (repo, x) : rest ->
      Right (x, FoundSome (repo :| fmap fst rest))
    [] -> Left $ fromMaybe (error "unreachable") $ nonEmpty fail

allOrError :: NonEmpty (Either a1 a2) -> Either a1 (NonEmpty a2)
allOrError = go (Right [])
  where
    go
      :: Either err [ok]
      -> NonEmpty (Either err ok)
      -> Either err (NonEmpty ok)
    go (Left err) _ = Left err
    go (Right _es) ((Left err) :| _) = Left err
    go (Right res) ((Right ok) :| []) =
      Right (ok :| res)
    go (Right res) ((Right ok) :| (x:xs)) =
      go (Right (ok:res)) (x :| xs)

-- | Bundle of glean db handle resources
data GleanBackend b =
  GleanBackend {
    gleanBackend :: b,
    gleanDBs :: NonEmpty (GleanDBName, Glean.Repo),
    tracer :: GlassTracer
  }

backendRunHaxl
  :: Glean.Backend b => GleanBackend b -> (forall u. ReposHaxl u w a) -> IO a
backendRunHaxl GleanBackend{..} haxl =
  traceSpan tracer "glean" $
    runHaxlAllRepos gleanBackend (fmap snd gleanDBs) haxl

-- | Whether the user requires the exact revision specified
data RevisionSpecifier = ExactOnly Revision | AnyRevision
  deriving Show

revisionSpecifierError :: RevisionSpecifier -> Text
revisionSpecifierError AnyRevision = "AnyRevision"
revisionSpecifierError (ExactOnly (Revision rev))= "Requested exactly " <> rev

dbChooser :: RepoName -> RequestOptions -> ChooseGleanDBs
dbChooser repo opts =
  case requestOptions_revision opts of
    Nothing -> ChooseLatest
    Just rev
      | Just True <- nearest, not exact -> ChooseNearest repo rev
      | otherwise -> ChooseExactOrLatest rev
 where
 nearest = requestOptions_feature_flags opts >>= featureFlags_nearest_revision
 exact = requestOptions_exact_revision opts

withGleanDBs
  :: (LogError a, LogRequest a, LogResult b)
  => Text
  -> Glass.Env
  -> RequestOptions
  -> a
  -> RepoName
  -> NonEmpty GleanDBName
  -> (NonEmpty (GleanDBName, Glean.Repo)
        -> GleanDBInfo -> IO (b, Maybe ErrorLogger))
  -> IO (b, Maybe ErrorLogger)
withGleanDBs method env@Glass.Env{..} opts req repo dbNames fn = do
  dbInfo <- readTVarIO latestGleanRepos
  dbs <- getSpecificGleanDBs tracer sourceControl dbInfo (dbChooser repo opts) dbNames
  withLog method env opts req $
    withLogDB dbs $
      fn dbs dbInfo

-- | Top-level wrapper for all requests.
--
-- * Snapshots the GleanDBInfo
-- * Implements strict vs. non-strict error handling
-- * Logs the request and result
--
withRequest
  :: (LogRequest req, LogError req, LogResult res)
  => Text
  -> Glass.Env
  -> req
  -> RequestOptions
  -> (GleanDBInfo -> IO (res, GleanGlassLogger, Maybe ErrorLogger))
  -> IO res
withRequest method env@Glass.Env{..} req opts fn = do
  dbInfo <- readTVarIO latestGleanRepos
  withStrictErrorHandling dbInfo opts $
    withLog method env opts req $
      fn dbInfo

-- | Run an action that provides a repo and maybe a language, log it
withRepoLanguage
  :: (LogError a, LogRequest a, LogResult b)
  => Text
  -> Glass.Env
  -> a
  -> RepoName
  -> Maybe Language
  -> RequestOptions
  -> (  NonEmpty (GleanDBName,Glean.Repo)
     -> GleanDBInfo
     -> Maybe Language
     -> IO (b, Maybe ErrorLogger))
  -> IO b
withRepoLanguage method env@Glass.Env{..} req repo mlanguage opts fn =
  withRequest method env req opts $ \dbInfo -> do
    dbs <- getGleanRepos tracer sourceControl repoMapping dbInfo repo
      mlanguage (dbChooser repo opts) gleanDB
    withLogDB dbs $
      fn dbs dbInfo mlanguage

-- | Run an action that provides a repo and filepath, log it
withRepoFile
  :: (LogError a, LogRequest a, LogResult b)
  => Text
  -> Glass.Env
  -> RequestOptions
  -> a
  -> RepoName
  -> Path
  -> (  NonEmpty (GleanDBName,Glean.Repo)
     -> GleanDBInfo
     -> Maybe Language
     -> IO (b, Maybe ErrorLogger))
  -> IO b
withRepoFile method env opts req repo file fn = do
  withRepoLanguage method env req repo (filetype file) opts fn

-- | Run an action that provides a symbol id, log it
withSymbol
  :: LogResult c
  => Text
  -> Glass.Env
  -> RequestOptions
  -> SymbolId
  -> (  NonEmpty (GleanDBName, Glean.Repo)
     -> GleanDBInfo
     -> (RepoName, Language, [Text])
     -> IO (c, Maybe ErrorLogger))
  -> IO c
withSymbol method env@Glass.Env{..} opts sym fn =
  withRequest method env sym opts $ \dbInfo ->
    case symbolTokens sym of
      Left err -> throwM $ ServerException err
      Right req@(repo, lang, _toks) -> do
        dbs <- getGleanRepos tracer sourceControl repoMapping dbInfo repo
          (Just lang) (dbChooser repo opts) gleanDB
        withLogDB dbs $ fn dbs dbInfo req

withStrictErrorHandling
  :: GleanDBInfo
  -> RequestOptions
  -> IO (res, Maybe ErrorLogger)
  -> IO res
withStrictErrorHandling dbInfo opts action = do
  (res, merr) <- action
  case merr of
    Just err
      | requestOptions_strict opts
      , not $ null $ errorTy err
      -> do
          let getFirstRevision repo =
                case HashMap.lookup repo (scmRevisions dbInfo) of
                  Just m | rev : _ <- HashMap.elems m ->
                    Just (scmRevision rev)
                  _ -> Nothing
          throwM $ GlassException
            (errorTy err)
            (mapMaybe getFirstRevision (errorGleanRepo err))
    _ -> return res


withLog
  :: (LogRequest req, LogError req, LogResult res)
  => Text
  -> Glass.Env
  -> RequestOptions
  -> req
  -> IO (res, GleanGlassLogger, Maybe ErrorLogger)
  -> IO (res, Maybe ErrorLogger)
withLog cmd env opts req action = do
  let requestLogs =
        Logger.setMethod cmd <>
        logRequest req <>
        logRequest opts
  (res, _) <- loggingAction
    (Logger.runLog (Glass.logger env) . (requestLogs <>))
    logResult
    (do
      (res, log, merr) <- action
      forM_ merr $ \e -> runErrorLog env cmd (e <> logError req)
      return ((res, merr), log))
  return res

runErrorLog :: Glass.Env -> Text -> ErrorLogger -> IO ()
runErrorLog env cmd err = ErrorsLogger.runLog (Glass.logger env) $
  errorsLogger err <> ErrorsLogger.setMethod cmd

-- | Wrapper to enable perf logging, log the db names, and stats for
-- intermediate steps, and internal errors.
withLogDB
  :: (LogError dbs, LogRepo dbs)
  => dbs
  -> IO (res, Maybe ErrorLogger)
  -> IO (res, GleanGlassLogger, Maybe ErrorLogger)
withLogDB dbs fn = do
  (res, merr) <- fn
  let err = fmap (<> logError dbs) merr
  return (res, logRepo dbs, err)

-- | Given an SCS repo name, and a candidate path, find latest Glean dbs or
-- throw. Returns the chosen db name and Glean repo handle.
-- If a Glean.Repo is given, use it instead.
getGleanRepos
  :: GlassTracer
  -> Some SourceControl
  -> RepoMapping
  -> GleanDBInfo
  -> RepoName
  -> Maybe Language
  -> ChooseGleanDBs
  -> Maybe Glean.Repo
  -> IO (NonEmpty (GleanDBName,Glean.Repo))
getGleanRepos tracer scm repoMapping dbInfo scsrepo mlanguage chooser mGleanDB =
  case mGleanDB of
    Nothing ->
      case fromSCSRepo repoMapping scsrepo mlanguage of
        [] ->  throwIO $ ServerException $ "No repository found for: " <>
          unRepoName scsrepo <>
            maybe "" (\x -> " (" <> toShortCode x <> ")") mlanguage
        (x:xs) ->
          getSpecificGleanDBs tracer scm dbInfo chooser (x :| xs)
    Just gleanDB@Glean.Repo{repo_name} ->
      return ((GleanDBName repo_name, gleanDB) :| [])

-- | If you already know the set of dbs you need, just get them.
getSpecificGleanDBs
  :: GlassTracer
  -> Some SourceControl
  -> GleanDBInfo
  -> ChooseGleanDBs
  -> NonEmpty GleanDBName
  -> IO (NonEmpty (GleanDBName,Glean.Repo))
getSpecificGleanDBs tracer scm dbInfo chooser gleanDBNames = do
  dbs <- chooseGleanDBs tracer scm dbInfo chooser (toList gleanDBNames)
  case dbs of
    [] -> throwIO $ ServerException $ "No Glean dbs found for: " <>
            Text.intercalate ", " (map unGleanDBName $ toList gleanDBNames)
    db:dbs -> return (db :| dbs)
