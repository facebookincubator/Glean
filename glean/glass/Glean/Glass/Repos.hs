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
  , GleanDBInfo(..)
  , ScmRevisions
  , ScmRevisionInfo(..)

  -- * Mappings
  , fromSCSRepo
  , filetype

  -- * Operation on a pool of latest repos
  , withLatestRepos
  , ChooseGleanDBs(..)
  , chooseGleanDBs
  , updateLatestRepos

  -- * Misc
  , toRepoName
  , findLanguages
  , findRepos
  , selectGleanDBs
  , getRepoHash
  , getRepoHashForLocation
  , getDBRevision
  , getLatestRepos
  ) where

import Control.Concurrent.Stream
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (nub)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Control.Applicative
import Control.Concurrent.Async ( withAsync )
import Control.Exception ( uninterruptibleMask_ )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Set ( Set )
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text ( Text, intercalate )
import qualified Logger.GleanGlassErrors as Errors
import Text.Printf

import Util.Log
import Logger.IO (Logger)
import qualified Util.Log as LocalLog
import Glean.Util.Some
import Glean.Util.Periodic ( doPeriodicallySynchronised )
import Util.Text ( textShow )
import Util.Time
import Util.Timing
import qualified Glean
import Util.STM
import qualified Glean.Repo as Glean

import Glean.Glass.Base
import Glean.Glass.SourceControl
import Glean.Glass.SymbolId ( toShortCode )
import Glean.Glass.Types
import qualified Glean.Glass.RepoMapping as Mapping -- site-specific
import Glean.Repo (LatestRepos)
import Glean.Glass.Tracing (GlassTracer, traceSpan)

-- | mapping from Glean DB to info about the scm repositories it covers
type ScmRevisions = HashMap Glean.Repo (HashMap RepoName ScmRevisionInfo)

-- | Information about a source control revision
data ScmRevisionInfo = ScmRevisionInfo
  { scmRevision :: Revision
  , scmGeneration :: Maybe ScmGeneration
  } deriving Show

-- | mapping from revision to Glean DB. We assume that revisions are
-- globally unique hashes, so there's no need to include the
-- repository in the key.
type DbByRevision = HashMap Revision Glean.Repo

-- | mapping from scm repo to (generation -> DB mapping)
type DbByGeneration = HashMap RepoName (IntMap Glean.Repo)

-- | info about which Glean DBs are currently available
data GleanDBInfo = GleanDBInfo
  { latestRepos :: Glean.LatestRepos
  , scmRevisions :: ScmRevisions
  , dbByRevision :: HashMap GleanDBName (DbByRevision, DbByGeneration)
  }

-- return a RepoName if indexed by glean
toRepoName :: RepoMapping -> Text -> Maybe RepoName
toRepoName RepoMapping{..} repo =
  case Map.lookup repoName gleanIndices of
    Just _ -> Just repoName
    Nothing -> Nothing
  where
    repoName = RepoName repo

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
listGleanIndices RepoMapping{..} testsOnly =
  let testRepos = [RepoName "test", RepoName "test-xlang"]
      flatten (repo,langs) = map (repo,) langs
      flattened = concatMap flatten $ Map.toList gleanIndices
      isTest = \(repo, _) -> elem repo testRepos in
    filter (if testsOnly then isTest else not . isTest) flattened

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
  | is ".pyi"  = Just Language_Python
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
  | is ".tsx"  = Just Language_TypeScript

  | is "TARGETS"  = Just Language_Buck
  | is "BUCK"  = Just Language_Buck
  | is ".bzl"  = Just Language_Buck

  | is ".cs"  = Just Language_CSharp
  | is ".graphql"  = Just Language_GraphQL

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
  -> Some SourceControl
  -> Maybe Logger
  -> Maybe Int
  -> IO GleanDBInfo
getLatestRepos backend scm mlogger mretry = go mretry
  where
    go :: Maybe Int -> IO GleanDBInfo
    go n = do
      latest <- Glean.getLatestRepos backend $ \name ->
        GleanDBName name `Set.member` Mapping.allGleanRepos

      -- Filter unavailable DBs using hasDatabase. Glean.getLatestRepos does
      -- this for latestRepos but not allLatestRepos, in that case it's up to
      -- the client (us). Since we're fetching DBs periodically and caching
      -- them, it's no problem to check availability for the whole set.
      let available = Glean.hasDatabase backend . Glean.database_repo
      (time, _, dbs) <- timeIt $ do
        avail <- mapM (filterM available) (Glean.allLatestRepos latest)
        return latest { Glean.allLatestRepos = avail }
      let numDBs = sum (fmap length (Glean.allLatestRepos latest))
          numAvailableDBs = sum (fmap length (Glean.allLatestRepos dbs))
      logInfo $ printf "filtered available DBs in %s, %d/%d unavailable"
        (showTime time) (numDBs - numAvailableDBs) numDBs

      scmRevisions <- getScmRevisions scm dbs
      let advertised = Map.keysSet (Glean.latestRepos dbs)
          info = GleanDBInfo
            { latestRepos = dbs
            , scmRevisions = scmRevisions
            , dbByRevision = getDbByRevision dbs scmRevisions
            }
      if required `Set.isSubsetOf` advertised
        then return info
        else do
          -- some required dbs are missing! this is transient? and bad
          -- in prod/full service mode this would be bad
          let missing = required `Set.difference` advertised
          logIt mlogger dbs missing n backend
          case n of
            Just n
              | n > 1 -> do {- i.e. try more than 1 time -}
                delay (seconds 1) >> go (Just (n-1))

            _ -> return info -- if no retries allowed, give up

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
              [ Glean.repo_hash (Glean.database_repo repo)
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

getScmRevisions
  :: Some SourceControl
  -> Glean.LatestRepos
  -> IO ScmRevisions
getScmRevisions scm repos = do
  let all = Map.toList (Glean.allLatestRepos repos)
  (time, _, r) <- timeIt $ fmap HashMap.fromList $
    forConcurrently_unordered 16 (concatMap snd all) $ \db -> do
      revmap <- revMap db
      return (Glean.database_repo db, revmap)
  logInfo $ printf "fetched commit generations for %d DBs in %s"
    (HashMap.size r) (showTime time)
  return r
  where
  revMap db =
    fmap HashMap.fromList $ forM revs $ \(repo, r) -> do
      let repoName = RepoName repo
          rev = Revision r
      gen <- getGeneration scm repoName rev
      return (repoName, ScmRevisionInfo rev gen)
    where
    revs = HashMap.toList (Glean.scmRevisionsOfDatabase db)

getDbByRevision
  :: Glean.LatestRepos
  -> ScmRevisions
  -> HashMap GleanDBName (DbByRevision, DbByGeneration)
getDbByRevision repos scmRevs =
  HashMap.fromList
    [ (GleanDBName name, (revmap, genmap))
    | (name, dbs) <- Map.toList (Glean.allLatestRepos repos)
    , let
        revmap = HashMap.fromList
          [ (scmRevision info, Glean.database_repo db)
          | db <- dbs
          , (_repo, info) <- getDBRevisions db
          ]
        genmap = HashMap.fromListWith IntMap.union
          [ (repo, IntMap.fromList [(fromIntegral gen, Glean.database_repo db)])
          | db <- dbs
          , (repo, ScmRevisionInfo { scmGeneration =
              Just (ScmGeneration gen) }) <- getDBRevisions db
          ]
    ]
    where
      getDBRevisions db =
        maybe [] HashMap.toList $
          HashMap.lookup (Glean.database_repo db) scmRevs

-- | Introduce a latest repo cache.
-- TODO: this should pass the configured repo list through
withLatestRepos
  :: Glean.Backend b
   => b
   -> Some SourceControl
   -> Maybe Logger
   -> Maybe Int
   -> DiffTimePoints
   -> (TVar GleanDBInfo -> IO a)
   -> IO a
withLatestRepos backend scm mlogger mretry freq f = do
  repos <- getLatestRepos backend scm mlogger mretry
  tvRepos <- newTVarIO repos
  withAsync (worker tvRepos) $ \_async -> f tvRepos
  where
    worker tvRepos =
      doPeriodicallySynchronised freq $
      uninterruptibleMask_ $
        -- prevents the update from being cancelled while in progress
        -- which can cause memory leaks if the process exits
        -- immediately. This is benign, but can lead to ASAN test
        -- failures.
      updateLatestRepos backend scm mlogger mretry tvRepos

-- | Update a TVar with the latest repos
-- TODO: should take latest configuration repo list
updateLatestRepos
  :: Glean.Backend b
  => b
  -> Some SourceControl
  -> Maybe Logger
  -> Maybe Int
  -> TVar GleanDBInfo -> IO ()
updateLatestRepos backend scm mlogger mretry tvRepos = do
  repos <- getLatestRepos backend scm mlogger mretry
  atomically $ writeTVar tvRepos repos

data ChooseGleanDBs
  = ChooseLatest
  | ChooseExactOrLatest Revision
  | ChooseNearest RepoName Revision

-- | Choose DBs for the given DB names and ChooseGleanDBs spec
chooseGleanDBs
  :: GlassTracer
  -> Some SourceControl
  -> GleanDBInfo
  -> ChooseGleanDBs
  -> [GleanDBName]
  -> IO [(GleanDBName, Glean.Repo)]
chooseGleanDBs _ _ dbInfo ChooseLatest repoNames =
  return $ catMaybes
    [ (dbName,) <$> Map.lookup name (Glean.latestRepos (latestRepos dbInfo))
    | dbName@(GleanDBName name) <- repoNames
    ]
chooseGleanDBs _ _ dbInfo (ChooseExactOrLatest rev) repoNames =
  return $ catMaybes
    [ (dbName,) <$> (dbForRevision <|> latestDb)
    | dbName@(GleanDBName name) <- repoNames
    , let
        latestDb = Map.lookup name (Glean.latestRepos (latestRepos dbInfo))
        dbForRevision =
          HashMap.lookup dbName (dbByRevision dbInfo) >>= \(byrev, _) ->
          HashMap.lookup rev byrev
    ]
chooseGleanDBs tracer scm dbInfo (ChooseNearest repo rev) repoNames = do
  maybeGen <- traceSpan tracer "getGeneration" $ getGeneration scm repo rev
  case maybeGen of
    Nothing ->
      chooseGleanDBs tracer scm dbInfo (ChooseExactOrLatest rev) repoNames
    Just (ScmGeneration gen) -> do
      return $ catMaybes
        [ (dbName,) <$> (dbForRevision <|> latestDb)
        | dbName@(GleanDBName name) <- repoNames
        , let
            latestDb = Map.lookup name (Glean.latestRepos (latestRepos dbInfo))
            dbForRevision =
              HashMap.lookup dbName (dbByRevision dbInfo) >>= \(_, repos) ->
              HashMap.lookup repo repos >>= best gen
        ]
  where
  -- find the DB with the closest generation number
  best gen bygen =
   case (low, high) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just (_, db)) -> Just db
      (Just (_, db), Nothing) -> Just db
      (Just (lowgen, lowdb), Just (highgen, highdb))
        | gen' - lowgen < highgen - gen' -> Just lowdb
        | otherwise -> Just highdb
     where
      gen' = fromIntegral gen
      low = IntMap.lookupLE gen' bygen
      high = IntMap.lookupGT gen' bygen


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
getRepoHashForLocation LocationRange{..} scmRevs gleanDB =
  getDBRevision scmRevs gleanDB locationRange_repository

getDBRevision :: ScmRevisions -> Glean.Repo -> RepoName -> Revision
getDBRevision scmRevs gleanDB repo =
  fromMaybe (getRepoHash gleanDB) $ do
    scmRepoToHash <- HashMap.lookup gleanDB scmRevs
    scmRevision <$> HashMap.lookup repo scmRepoToHash
