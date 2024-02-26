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
    getLatestAttrDB,

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

import Glean.Glass.Base
import Glean.Glass.Logging
import Glean.Glass.Repos
import Glean.Glass.SymbolId
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
    gleanDBs :: NonEmpty (GleanDBName, Glean.Repo)
  }

backendRunHaxl
  :: Glean.Backend b => GleanBackend b -> (forall u. ReposHaxl u w a) -> IO a
backendRunHaxl GleanBackend{..} =
  runHaxlAllRepos gleanBackend (fmap snd gleanDBs)

-- | Whether the user requires the exact revision specified
data RevisionSpecifier = ExactOnly Revision | AnyRevision
  deriving Show

revisionSpecifierError :: RevisionSpecifier -> Text
revisionSpecifierError AnyRevision = "AnyRevision"
revisionSpecifierError (ExactOnly (Revision rev))= "Requested exactly " <> rev

-- | Get glean db for an attribute type
getLatestAttrDB
  :: RepoMapping
  -> GleanDBInfo
  -> RequestOptions
  -> GleanDBName
  -> IO (Maybe (Glean.Repo, GleanDBAttrName))
getLatestAttrDB repoMapping dbInfo opts gleanDBName =
  case firstAttrDB repoMapping gleanDBName of
    Nothing -> return Nothing
    Just attrDBName -> atomically $ do
      let dbs = chooseGleanDBs dbInfo (selectRevision opts)
            [gleanAttrDBName attrDBName]
      return $ case dbs of
        [] -> Nothing
        db:_ -> Just (snd db, attrDBName)

withGleanDBs
  :: (LogError a, LogRequest a, LogResult b)
  => Text
  -> Glass.Env
  -> RequestOptions
  -> a
  -> NonEmpty GleanDBName
  -> (NonEmpty (GleanDBName, Glean.Repo)
        -> GleanDBInfo -> IO (b, Maybe ErrorLogger))
  -> IO (b, Maybe ErrorLogger)
withGleanDBs method env@Glass.Env{..} opts req dbNames fn = do
  dbInfo <- readTVarIO latestGleanRepos
  dbs <- getSpecificGleanDBs dbInfo (selectRevision opts) dbNames
  withLog method env req $ \log ->
    withLogDB dbs log $
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
  -> (GleanDBInfo -> GleanGlassLogger -> IO (res, GleanGlassLogger, Maybe ErrorLogger))
  -> IO res
withRequest method env@Glass.Env{..} req opts fn = do
  dbInfo <- readTVarIO latestGleanRepos
  withStrictErrorHandling dbInfo opts $
    withLog method env req $
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
  withRequest method env req opts $ \dbInfo logger -> do
    dbs <- getGleanRepos repoMapping dbInfo
      repo mlanguage (selectRevision opts) gleanDB
    withLogDB dbs logger $
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
  withRequest method env sym opts $ \dbInfo log ->
    case symbolTokens sym of
      Left err -> throwM $ ServerException err
      Right req@(repo, lang, _toks) -> do
        dbs <- getGleanRepos repoMapping dbInfo repo
          (Just lang) (selectRevision opts) gleanDB
        withLogDB dbs log $ fn dbs dbInfo req

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
                    Just (Revision rev)
                  _ -> Nothing
          throwM $ GlassException
            (errorTy err)
            (mapMaybe getFirstRevision (errorGleanRepo err))
    _ -> return res


withLog
  :: (LogRequest req, LogError req, LogResult res)
  => Text
  -> Glass.Env
  -> req
  -> (GleanGlassLogger -> IO (res, GleanGlassLogger, Maybe ErrorLogger))
  -> IO (res, Maybe ErrorLogger)
withLog cmd env req action = do
  (res, _) <- loggingAction
    (runLog env cmd)
    logResult
    (do
      (res, log, merr) <- action $ logRequest req
      forM_ merr $ \e -> runErrorLog env cmd (e <> logError req)
      return ((res, merr), log))
  return res

runLog :: Glass.Env -> Text -> GleanGlassLogger -> IO ()
runLog env cmd log = Logger.runLog (Glass.logger env) $
  log <> Logger.setMethod cmd

runErrorLog :: Glass.Env -> Text -> ErrorLogger -> IO ()
runErrorLog env cmd err = ErrorsLogger.runLog (Glass.logger env) $
  errorsLogger err <> ErrorsLogger.setMethod cmd

-- | Wrapper to enable perf logging, log the db names, and stats for
-- intermediate steps, and internal errors.
withLogDB
  :: (LogError dbs, LogRepo dbs)
  => dbs
  -> GleanGlassLogger
  -> IO (res, Maybe ErrorLogger)
  -> IO (res, GleanGlassLogger, Maybe ErrorLogger)
withLogDB dbs log fn = do
  (res, merr) <- fn
  let err = fmap (<> logError dbs) merr
  return (res, log <> logRepo dbs, err)

-- | Revision to use when selecting Glean DBs.
--     Just rev -> select DBs that match rev, or latest otherwise
--     Nothing -> always choose the latest
selectRevision :: RequestOptions -> Maybe Revision
selectRevision RequestOptions{..}
  | Just FeatureFlags { featureFlags_use_revision = Just True }
      <- requestOptions_feature_flags = requestOptions_revision
  | otherwise = Nothing

-- | Given an SCS repo name, and a candidate path, find latest Glean dbs or
-- throw. Returns the chosen db name and Glean repo handle.
-- If a Glean.Repo is given, use it instead.
getGleanRepos
  :: RepoMapping
  -> GleanDBInfo
  -> RepoName
  -> Maybe Language
  -> Maybe Revision
  -> Maybe Glean.Repo
  -> IO (NonEmpty (GleanDBName,Glean.Repo))
getGleanRepos repoMapping dbInfo scsrepo mlanguage mRevision mGleanDB =
  case mGleanDB of
    Nothing ->
      case fromSCSRepo repoMapping scsrepo mlanguage of
        [] ->  throwIO $ ServerException $ "No repository found for: " <>
          unRepoName scsrepo <>
            maybe "" (\x -> " (" <> toShortCode x <> ")") mlanguage
        (x:xs) ->
          getSpecificGleanDBs dbInfo mRevision (x :| xs)
    Just gleanDB@Glean.Repo{repo_name} ->
      return ((GleanDBName repo_name, gleanDB) :| [])

-- | If you already know the set of dbs you need, just get them.
getSpecificGleanDBs
  :: GleanDBInfo
  -> Maybe Revision
  -> NonEmpty GleanDBName
  -> IO (NonEmpty (GleanDBName,Glean.Repo))
getSpecificGleanDBs dbInfo mRevision gleanDBNames =
  case chooseGleanDBs dbInfo mRevision (toList gleanDBNames) of
    [] -> throwIO $ ServerException $ "No Glean dbs found for: " <>
            Text.intercalate ", " (map unGleanDBName $ toList gleanDBNames)
    db:dbs -> return (db :| dbs)
