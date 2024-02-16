{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
module Glean.Glass.Handler.Utils where

import Control.Exception
import Control.Monad
import Control.Monad.Catch ( throwM )
import Data.Either
import qualified Data.Foldable as Foldable
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
import qualified Glean.Repo as Glean
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
    (_, x) : rest -> Right
      ( x
      , case nonEmpty rest of
          Nothing -> FoundOne
          Just otherSuccesses ->  FoundMultiple (fmap fst otherSuccesses)
      )
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
  -> TVar GleanDBInfo
  -> Maybe Revision
  -> GleanDBName
  -> IO (Maybe (Glean.Repo, GleanDBAttrName))
getLatestAttrDB repoMapping allRepos mRevision gleanDBName =
  case firstAttrDB repoMapping gleanDBName of
    Nothing -> return Nothing
    Just attrDBName -> atomically $ do
      dbs <- lookupLatestRepos allRepos mRevision [gleanAttrDBName attrDBName]
      return $ case dbs of
        [] -> Nothing
        db:_ -> Just (snd db, attrDBName)

withGleanDBs
  :: (LogError a, LogRequest a, LogResult b)
  => Text
  -> Glass.Env
  -> Maybe Revision
  -> a
  -> NonEmpty GleanDBName
  -> (NonEmpty (GleanDBName, Glean.Repo)
        -> ScmRevisions -> IO (b, Maybe ErrorLogger))
  -> IO (b, Maybe ErrorLogger)
withGleanDBs method env@Glass.Env{..} mRevision req dbNames fn = do
  withLogDB method env req
    (getSpecificGleanDBs latestGleanRepos mRevision dbNames)
    Nothing
    (\(dbs,revs) _mlang -> fn dbs revs)

-- | Run an action that provides a repo and maybe a language, log it
withRepoLanguage
  :: (LogError a, LogRequest a, LogResult b)
  => Text
  -> Glass.Env
  -> a
  -> RepoName
  -> Maybe Language
  -> Maybe Revision
  -> (  (NonEmpty (GleanDBName,Glean.Repo), ScmRevisions)
     -> Maybe Language
     -> IO (b, Maybe ErrorLogger))
  -> IO (b, Maybe ErrorLogger)
withRepoLanguage method env@Glass.Env{..} req repo mlanguage mRevision fn = do
  withLogDB method env req
    (getGleanRepos repoMapping latestGleanRepos
      repo mlanguage mRevision gleanDB)
    mlanguage
    fn

-- | Run an action that provides a repo and filepath, log it
withRepoFile :: (LogError a, LogRequest a, LogResult b) => Text
  -> Glass.Env
  -> Maybe Revision
  -> a
  -> RepoName
  -> Path
  -> (  (NonEmpty (GleanDBName,Glean.Repo), ScmRevisions)
     -> Maybe Language
     -> IO (b, Maybe ErrorLogger))
  -> IO (b, Maybe ErrorLogger)
withRepoFile method env mRev req repo file fn = do
  withRepoLanguage method env req repo (filetype file) mRev fn

-- | Run an action that provides a symbol id, log it
withSymbol
  :: LogResult c
  => Text
  -> Glass.Env
  -> Maybe Revision
  -> SymbolId
  -> ((NonEmpty (GleanDBName, Glean.Repo),
        ScmRevisions, (RepoName, Language, [Text]))
  -> IO (c, Maybe ErrorLogger))
  -> IO (c, Maybe ErrorLogger)
withSymbol method env@Glass.Env{..} mRevision sym fn =
  withLogDB method env sym
    (case symbolTokens sym of
      Left err -> throwM $ ServerException err
      Right req@(repo, lang, _toks) -> do
        (dbs, revs) <-
          getGleanRepos repoMapping latestGleanRepos repo
            (Just lang) mRevision gleanDB
        return (dbs, revs, req))
    Nothing
    (\db _mlang -> fn db)

withStrictErrorHandling
  :: Glean.Backend b
  => b
  -> RequestOptions
  -> IO (res, Maybe ErrorLogger)
  -> IO res
withStrictErrorHandling backend opts action = do
  (res, merr) <- action
  case merr of
    Just err
      | requestOptions_strict opts
      , not $ null $ errorTy err
      -> do
          revisionsByScm <-
            mapM (Glean.getSCMrevisions backend) (errorGleanRepo err)
          let getFirstRevision scmRevisions =
                case Foldable.toList scmRevisions of
                  rev : _ -> Just (Revision rev)
                  _ -> Nothing
          throwM $ GlassException
            (errorTy err)
            (mapMaybe getFirstRevision revisionsByScm)
    _ -> return res

runLog :: Glass.Env -> Text -> GleanGlassLogger -> IO ()
runLog env cmd log = Logger.runLog (Glass.logger env) $
  log <> Logger.setMethod cmd

runErrorLog :: Glass.Env -> Text -> ErrorLogger -> IO ()
runErrorLog env cmd err = ErrorsLogger.runLog (Glass.logger env) $
  errorsLogger err <> ErrorsLogger.setMethod cmd

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

-- | Wrapper to enable perf logging, log the db names, and stats for
-- intermediate steps, and internal errors.
withLogDB
  :: (LogRequest req, LogError req, LogError dbs, LogRepo dbs, LogResult res)
  => Text
  -> Glass.Env
  -> req
  -> IO dbs
  -> Maybe Language
  -> (dbs -> Maybe Language -> IO (res, Maybe ErrorLogger))
  -> IO (res, Maybe ErrorLogger)
withLogDB cmd env req fetch mlanguage run =
  withLog cmd env req $ \log -> do
    dbs <- fetch
    (res,merr) <- run dbs mlanguage
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
  -> TVar GleanDBInfo
  -> RepoName
  -> Maybe Language
  -> Maybe Revision
  -> Maybe Glean.Repo
  -> IO (NonEmpty (GleanDBName,Glean.Repo), ScmRevisions)
getGleanRepos repoMapping latestGleanDBs scsrepo
    mlanguage mRevision mGleanDB = do
  case mGleanDB of
    Nothing ->
      case fromSCSRepo repoMapping scsrepo mlanguage of
        [] ->  throwIO $ ServerException $ "No repository found for: " <>
          unRepoName scsrepo <>
            maybe "" (\x -> " (" <> toShortCode x <> ")") mlanguage
        (x:xs) ->
          getSpecificGleanDBs latestGleanDBs mRevision (x :| xs)
    Just gleanDB@Glean.Repo{repo_name} -> do
      scmRevs <- fmap scmRevisions $ atomically $ do readTVar latestGleanDBs
      return ((GleanDBName repo_name, gleanDB) :| [], scmRevs)

-- | If you already know the set of dbs you need, just get them.
getSpecificGleanDBs
  :: TVar GleanDBInfo
  -> Maybe Revision
  -> NonEmpty GleanDBName
  -> IO (NonEmpty (GleanDBName,Glean.Repo), ScmRevisions)
getSpecificGleanDBs latestGleanDBs mRevision gleanDBNames = do
  (dbs, scmRevs) <- atomically $ do
    dbs <- lookupLatestRepos latestGleanDBs mRevision $ toList gleanDBNames
    scmRevs <- scmRevisions <$> readTVar latestGleanDBs
    pure (dbs, scmRevs)
  case dbs of
    [] -> throwIO $ ServerException $ "No Glean dbs found for: " <>
            Text.intercalate ", " (map unGleanDBName $ toList gleanDBNames)
    db:dbs -> return (db :| dbs, scmRevs)
