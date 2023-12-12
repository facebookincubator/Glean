{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

module Glean.Glass.Repos
  (
  -- * Types
  Language(..)
  , GleanDBName(..)
  , GleanDBAttrName(..)
  , ScmRevisions(..)

  -- * Mappings
  , fromSCSRepo
  , filetype
  , firstAttrDB

  -- * Operation on a pool of latest repos
  , withLatestRepos
  , lookupLatestRepos
  , updateLatestRepos

  -- * Misc
  , toRepoName
  , findLanguages
  , findRepos
  , selectGleanDBs
  , getRepoHash
  , getRepoHashForLocation
  ) where

import Data.List (nub)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Control.Concurrent.Async ( withAsync, forConcurrently )
import Control.Exception ( uninterruptibleMask_ )
import Data.Maybe ( catMaybes )
import Data.Set ( Set )
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text ( Text, intercalate )
import qualified Logger.GleanGlassErrors as Errors

import Logger.IO (Logger)
import qualified Util.Log as LocalLog
import Glean.Util.Periodic ( doPeriodicallySynchronised )
import Util.Text ( textShow )
import Glean.Util.Time
import qualified Glean
import Util.STM ( readTVar, writeTVar, atomically, newTVarIO, TVar, STM )
import qualified Glean.Repo as Glean

import Glean.Glass.Base ( GleanDBAttrName(..), GleanDBName(..),
  RepoMapping(..) )
import Glean.Glass.SymbolId ( toShortCode )
import Glean.Glass.Types
    ( Path(Path),
      RepoName(RepoName),
      Language(..),
      SymbolId(SymbolId),
      unRepoName,
      Revision(..),
      LocationRange(..)
    )
import qualified Glean.Glass.RepoMapping as Mapping -- site-specific
import Glean.Repo (LatestRepos)

-- mapping from scm repo to hash for each db
newtype ScmRevisions = ScmRevisions
  { scmRevisions :: HashMap Glean.Repo (HashMap Text Text)
  }

-- return a RepoName if indexed by glean
toRepoName :: RepoMapping -> Text -> Maybe RepoName
toRepoName RepoMapping{..} repo =
  case Map.lookup repoName gleanIndices of
    Just _ -> Just repoName
    Nothing -> Nothing
  where
    repoName = RepoName repo

-- | Additional metadata about files and methods in attribute dbs
firstAttrDB :: RepoMapping -> GleanDBName -> Maybe GleanDBAttrName
firstAttrDB RepoMapping{..} dbName
  | Just (db:_) <- Map.lookup dbName gleanAttrIndices = Just db
  | otherwise = Nothing

-- | Expand generic string search request parameters into a set of candidate
-- GleanDBs, grouped by logical SCM repo or corpus.
--
-- (We need to know which repo each GleanDB was discovered under to construct
-- symbol ids later on, which use SCM repo names).
--
-- Glean databases and language filters implied by the query.
--
-- Inputs are:
-- - an scm repo (like "www") or corpus (like "fbsource")
-- - language filters
--
-- Examples:
--
-- > 'www / *' selects [(www,hack), (www,flow)]
-- > '* / *' selects all
-- > '* / hack' selects www and fbsource
--
-- The special case of 'test / _' selects only the test db/lang pairs
--
-- Note:
-- - Selection does not preserve the order of dbs in a repo.
-- - Overlapping dbs (e.g. fbsource and fbsource.arvr.cxx) are de-duped
--
selectGleanDBs
  :: RepoMapping
  -> Maybe RepoName
  -> Set Language
  -> Either Text (Map.Map RepoName (Set GleanDBName))
selectGleanDBs repoMapping mRepoName langs0 =
  case map flatten (filter matches candidates) of
    [] -> Left err
    dbs -> Right $ Map.fromListWith Set.union dbs
  where
    candidates = listGleanIndices repoMapping isTestOnly
    langs = Set.map normalizeLanguages langs0

    flatten (repo,(dbname,_)) = (repo, Set.singleton dbname)

    -- if client requests tests only, search expansion is limited to the test db
    isTestOnly = mRepoName == Just (RepoName "test")

    matches :: (RepoName, (GleanDBName, Language)) -> Bool
    matches (r, (_, l)) = case (mRepoName, Set.null langs) of
      (Nothing, True)  -> True
      (Just rr, False) -> r == rr && l `Set.member` langs
      (Nothing, False) -> l `Set.member` langs
      (Just rr, True)  -> r == rr

    err = case (mRepoName, Set.toList langs) of
      (Nothing, []) -> "Empty index: no repos or languages found"
      (Just r, []) -> "Unknown repository: " <> unRepoName r
      (Nothing, ll) -> "No repository for " <>
        intercalate "," (map toShortCode ll)
      (Just r, ll) -> "Unknown repo/lang combination: " <> unRepoName r
        <> "(" <> intercalate "," (map toShortCode ll) <> ")"

-- | We don't distinguish between flavors of distinct related languages
-- notably, objective-c and objective C++ are treated like cpp by clang
normalizeLanguages :: Language -> Language
normalizeLanguages Language_ObjectiveC = Language_Cpp
normalizeLanguages l = l

-- | Select universe of glean repo,(db/language) pairs.
-- Either just the test dbs, or all the non-test dbs.
listGleanIndices :: RepoMapping -> Bool -> [(RepoName, (GleanDBName, Language))]
listGleanIndices RepoMapping{..} testsOnly
  | not testsOnly = concatMap flatten $ -- only non-test repos
      Map.toList (Map.delete testRepo gleanIndices)
  | otherwise = map (testRepo,) $ -- just the test repos
      Map.findWithDefault [] testRepo gleanIndices
  where
    testRepo = RepoName "test"

    flatten (repo,langs) = map (repo,) langs

-- Do something simple to map SCS repo to Glean repos
-- Names from configerator/scm/myles/service as a start
-- This should be in a config or SV to make onboarding simple, or from Glean
-- properties?
fromSCSRepo :: RepoMapping -> RepoName -> Maybe Language -> [GleanDBName]
fromSCSRepo RepoMapping{..} r hint
  | Just rs <- Map.lookup r gleanIndices
  = nub $ map fst $ case hint of
      Nothing -> rs
      Just h -> filter ((== h) . snd) rs
  | otherwise = []

-- | Used to minimize the choice of Glean db when looking for a file
-- This could be in DB properties if it becomes important
--
-- When onboarding a language, you should register the filetype, for
-- any src.Files we have xrefs for
--
filetype :: Path -> Maybe Language
filetype (Path file)
  | is ".c"  = Just Language_Cpp
  | is ".cc"  = Just Language_Cpp
  | is ".cpp"  = Just Language_Cpp
  | is ".cu"  = Just Language_Cpp -- CUDA
  | is ".cxx"  = Just Language_Cpp
  | is ".c++"  = Just Language_Cpp
  | is ".h"  = Just Language_Cpp
  | is ".hh"  = Just Language_Cpp
  | is ".hip"  = Just Language_Cpp
  | is ".hpp"  = Just Language_Cpp
  | is ".hxx"  = Just Language_Cpp
  | is ".h++"  = Just Language_Cpp
  | is ".mm"  = Just Language_Cpp
  | is ".m"  = Just Language_Cpp
  | is ".tcc"  = Just Language_Cpp

  | is ".flow"  = Just Language_JavaScript
  | is ".js"  = Just Language_JavaScript

  | is ".java"  = Just Language_Java
  | is ".kt"  = Just Language_Kotlin

  | is ".hhi"  = Just Language_Hack
  | is ".php"  = Just Language_Hack

  | is ".hs"  = Just Language_Haskell

  | is ".py"  = Just Language_Python
  | is ".cinc"  = Just Language_Python
  | is ".cconf"  = Just Language_Python
  | is ".mcconf"  = Just Language_Python
  | is ".ctest"  = Just Language_Python
  | is ".thrift-cvalidator"  = Just Language_Python

  | is ".thrift"  = Just Language_Thrift

  | is ".rs"  = Just Language_Rust
  | is ".erl"  = Just Language_Erlang
  | is ".go"  = Just Language_Go
  | is ".ts"  = Just Language_TypeScript

  | is "TARGETS"  = Just Language_Buck
  | is "BUCK"  = Just Language_Buck
  | is ".bzl"  = Just Language_Buck

  | is ".cs"  = Just Language_CSharp

  | otherwise = Nothing

  where
    is a =  a `Text.isSuffixOf` file

--
-- Operating on the latest repo statea
--

-- | Fetch all latest dbs we care for
getLatestRepos
  :: Glean.Backend b
  => b
  -> Maybe Logger
  -> Maybe Int
  -> IO Glean.LatestRepos
getLatestRepos backend mlogger mretry = go mretry
  where
    go :: Maybe Int -> IO Glean.LatestRepos
    go n = do
      latest <- Glean.getLatestRepos backend $ \name ->
        GleanDBName name `Set.member` Mapping.allGleanRepos
      let advertised = Map.keysSet (Glean.latestRepos latest)
      if required `Set.isSubsetOf` advertised
        then return latest
        else do
          -- some required dbs are missing! this is transient? and bad
          -- in prod/full service mode this would be bad
          let missing = required `Set.difference` advertised
          logIt mlogger latest missing n backend
          case n of
            Just n
              | n > 1 -> do {- i.e. try more than 1 time -}
                delay (seconds 1) >> go (Just (n-1))

            _ -> return latest -- if no retries allowed, give up

    required = Set.map unGleanDBName Mapping.gleanRequiredIndices

-- | Log an entry in glean_glass_server_error_events if a logger is available,
-- and locally (e.g. to stderr). Do not log otherwise (e.g. in test mode).
logIt
  :: Glean.Backend b
  => Maybe Logger -> LatestRepos -> Set Text -> Maybe Int -> b -> IO ()
logIt mlogger latest missing attempt backend = do
  case mlogger of
    Just logger -> do
      LocalLog.logError $ mconcat [
        "Error listing databases: ",
        Text.unpack missingDBNames,
        " not found. ",
        Text.unpack errorMsg
       ]
      Errors.runLog logger $ mconcat [
        Errors.setErrorType errorTy,
        Errors.setError errorMsg,
        Errors.setRepoName missingDBNames, -- these are the ones not found
        Errors.setMethod "getLatestRepos" -- fake thrift method is required
       ]
    Nothing -> return () -- don't log unless Just attempt
  where
    missingDBNames = Text.intercalate "," (Set.toList missing)
    errorTy = "ListDatabases"
    errorMsg =
      let remaining = subtract 1 <$> attempt
          reposListed = Text.unlines
            [ "Instances listed for " <> db <> " : " <> intercalate ","
              [ Glean.repo_hash repo
              | Just repos <- [Map.lookup db (Glean.allLatestRepos latest)]
              , repo <- repos
              ]
              | db <- Set.toList missing
            ]

      in case remaining of
        Just 0 -> Text.concat [ -- final attempt failed
                "Failed finding required databases in listDatabases",
                " (no more attempts possible!)",
                ". Using " <> formatBackend backend,
                ". \n",
                reposListed
                ]
        _ -> Text.concat [
                "Missing some required databases in listDatabases",
                " (attempts remaining " <> maybe "unknown" textShow remaining,
                "). Using " <> formatBackend backend
            ]


-- This is the full glean backend details so should have some useful stuff
formatBackend :: Glean.Backend b => b -> Text
formatBackend = Text.pack . Glean.displayBackend

getScmRevisions :: Glean.Backend b => b -> Glean.LatestRepos -> IO ScmRevisions
getScmRevisions backend repos = ScmRevisions . HashMap.fromList <$>
    forConcurrently repoList getScmRevision
  where
    repoList = Map.elems $ Glean.latestRepos repos
    getScmRevision repo = (repo,) <$> Glean.getSCMrevisions backend repo

-- | Introduce a latest repo cache.
-- TODO: this should pass the configured repo list through
withLatestRepos
  :: Glean.Backend b
   => b
   -> Maybe Logger
   -> Maybe Int
   -> DiffTimePoints
   -> (TVar Glean.LatestRepos -> TVar ScmRevisions -> IO a)
   -> IO a
withLatestRepos backend mlogger mretry freq f = do
  repos <- getLatestRepos backend mlogger mretry
  scmRevisions <- getScmRevisions backend repos
  tvRepos <- newTVarIO repos
  tvRevs <- newTVarIO scmRevisions
  withAsync (worker tvRepos tvRevs) $ \_async -> f tvRepos tvRevs
  where
    worker tvRepos tvRevs =
      doPeriodicallySynchronised freq $
      uninterruptibleMask_ $
        -- prevents the update from being cancelled while in progress
        -- which can cause memory leaks if the process exits
        -- immediately. This is benign, but can lead to ASAN test
        -- failures.
      updateLatestRepos backend mlogger mretry tvRepos tvRevs

-- | Update a TVar with the latest repos
-- TODO: should take latest configuration repo list
updateLatestRepos
  :: Glean.Backend b
  => b
  -> Maybe Logger
  -> Maybe Int
  -> TVar Glean.LatestRepos -> TVar ScmRevisions -> IO ()
updateLatestRepos backend mlogger mretry tvRepos tvRevs = do
  repos <- getLatestRepos backend mlogger mretry
  revs <- getScmRevisions backend repos
  atomically $ do
    writeTVar tvRepos repos
    writeTVar tvRevs revs

-- | Lookup latest repo in the cache
lookupLatestRepos
  :: TVar Glean.LatestRepos -> [GleanDBName] -> STM [(GleanDBName, Glean.Repo)]
lookupLatestRepos tv repoNames = do
  repos <- Glean.latestRepos <$> readTVar tv
  return $ catMaybes
    [ (dbName,) <$> mrepo
    | dbName@(GleanDBName name) <- repoNames
    , let mrepo = Map.lookup name repos
    ]

-- | Symbols of the form "/repo/lang/" where pRepo is a prefix of repo
findRepos :: RepoMapping -> Text -> [SymbolId]
findRepos RepoMapping{..} pRepo =
  let repos = Map.toList $ Map.filterWithKey
        (const . Text.isPrefixOf pRepo . unRepoName) gleanIndices
  in concatMap (\(RepoName repo, repolangs) ->
      map (\(_, lang) -> SymbolId $ repo <> "/" <> toShortCode lang <> "/")
      repolangs) repos

-- | Symbols of the form "/repo/lang/" where pLang is a prefix of lang
findLanguages :: RepoMapping -> RepoName -> Text -> [SymbolId]
findLanguages RepoMapping{..} repoName@(RepoName repo) pLang =
  let allLangs = maybe [] (map (toShortCode . snd)) $
        Map.lookup repoName gleanIndices
      langs = filter (Text.isPrefixOf pLang) allLangs
  in map (\lang -> SymbolId $ repo <> "/" <> lang <> "/") langs


-- TODO (T122759515): Get repo revision from db properties
getRepoHash :: Glean.Repo -> Revision
getRepoHash repo = Revision (Text.take 40 (Glean.repo_hash repo))

getRepoHashForLocation
  :: LocationRange -> ScmRevisions -> Glean.Repo -> Revision
getRepoHashForLocation LocationRange{..} scmRevs repo =
  maybe (getRepoHash repo) Revision $ do
    scmRepoToHash <- HashMap.lookup repo $ scmRevisions scmRevs
    HashMap.lookup (unRepoName locationRange_repository) scmRepoToHash
