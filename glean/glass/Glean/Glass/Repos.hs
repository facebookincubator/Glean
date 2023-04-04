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

import Glean.Util.Periodic ( doPeriodicallySynchronised )
import Glean.Util.Time ( DiffTimePoints )
import qualified Glean
import Util.STM ( readTVar, writeTVar, atomically, newTVarIO, TVar, STM )
import qualified Glean.Repo as Glean

import Glean.Glass.Base ( GleanDBAttrName(..), GleanDBName(..) )
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
import Glean.Glass.RepoMapping  -- site-specific

-- mapping from scm repo to hash for each db
newtype ScmRevisions = ScmRevisions
  { scmRevisions :: HashMap Glean.Repo (HashMap Text Text)
  }

-- return a RepoName if indexed by glean
toRepoName :: Text -> Maybe RepoName
toRepoName repo = case Map.lookup repoName gleanIndices of
    Just _ -> Just repoName
    Nothing -> Nothing
  where
    repoName = RepoName repo

-- | Additional metadata about files and methods in attribute dbs
firstAttrDB :: GleanDBName -> Maybe GleanDBAttrName
firstAttrDB dbName
  | Just (db:_) <- Map.lookup dbName gleanAttrIndices = Just db
  | otherwise = Nothing

-- | All the Glean db repo names we're aware of
-- We will only be able to queries members of this set.
allGleanRepos :: Set GleanDBName
allGleanRepos = Set.fromList $
  map fst (concat (Map.elems gleanIndices)) ++
  concatMap (map gleanAttrDBName) (Map.elems gleanAttrIndices)

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
  :: Maybe RepoName
  -> Set Language
  -> Either Text (Map.Map RepoName (Set GleanDBName))
selectGleanDBs mRepoName langs0 =
  case map flatten (filter matches candidates) of
    [] -> Left err
    dbs -> Right $ Map.fromListWith Set.union dbs
  where
    candidates = listGleanIndices isTestOnly
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
listGleanIndices :: Bool -> [(RepoName, (GleanDBName, Language))]
listGleanIndices testsOnly
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
fromSCSRepo :: RepoName -> Maybe Language -> [GleanDBName]
fromSCSRepo r hint
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
  | ".h" `Text.isSuffixOf` file = Just Language_Cpp
  | ".cpp" `Text.isSuffixOf` file = Just Language_Cpp
  | ".cxx" `Text.isSuffixOf` file = Just Language_Cpp
  | ".cc" `Text.isSuffixOf` file = Just Language_Cpp
  | ".c" `Text.isSuffixOf` file = Just Language_Cpp
  | ".hh" `Text.isSuffixOf` file = Just Language_Cpp
  | ".hpp" `Text.isSuffixOf` file = Just Language_Cpp
  | ".hxx" `Text.isSuffixOf` file = Just Language_Cpp
  | ".mm" `Text.isSuffixOf` file = Just Language_Cpp
  | ".m" `Text.isSuffixOf` file = Just Language_Cpp
  | ".tcc" `Text.isSuffixOf` file = Just Language_Cpp

  | ".flow" `Text.isSuffixOf` file = Just Language_JavaScript
  | ".js" `Text.isSuffixOf` file = Just Language_JavaScript

  | ".hhi" `Text.isSuffixOf` file = Just Language_Hack
  | ".php" `Text.isSuffixOf` file = Just Language_Hack

  | ".hs" `Text.isSuffixOf` file = Just Language_Haskell

  | ".py" `Text.isSuffixOf` file = Just Language_Python
  | ".cinc" `Text.isSuffixOf` file = Just Language_Python
  | ".cconf" `Text.isSuffixOf` file = Just Language_Python
  | ".mcconf" `Text.isSuffixOf` file = Just Language_Python
  | ".ctest" `Text.isSuffixOf` file = Just Language_Python
  | ".thrift-cvalidator" `Text.isSuffixOf` file = Just Language_Python

  | ".thrift" `Text.isSuffixOf` file = Just Language_Thrift

  | ".rs" `Text.isSuffixOf` file = Just Language_Rust
  | ".erl" `Text.isSuffixOf` file = Just Language_Erlang
  | ".go" `Text.isSuffixOf` file = Just Language_Go
  | ".ts" `Text.isSuffixOf` file = Just Language_TypeScript

  | "TARGETS" `Text.isSuffixOf` file = Just Language_Buck
  | "BUCK" `Text.isSuffixOf` file = Just Language_Buck
  | ".bzl" `Text.isSuffixOf` file = Just Language_Buck

  | otherwise = Nothing

--
-- Operating on the latest repo state
--
-- TODO: exception handling behavior
--

-- | Fetch all latest dbs we care for
getLatestRepos
  :: Glean.Backend b => b -> Set GleanDBName -> IO Glean.LatestRepos
getLatestRepos backend repoNames = Glean.getLatestRepos backend $ \name ->
  GleanDBName name `Set.member` repoNames

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
   -> DiffTimePoints
   -> (TVar Glean.LatestRepos -> TVar ScmRevisions -> IO a)
   -> IO a
withLatestRepos backend freq f = do
  repos <- getLatestRepos backend allGleanRepos
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
      updateLatestRepos backend tvRepos tvRevs

-- | Update a TVar with the latest repos
-- TODO: should take latest configuration repo list
updateLatestRepos
  :: Glean.Backend b
  => b -> TVar Glean.LatestRepos -> TVar ScmRevisions -> IO ()
updateLatestRepos backend tvRepos tvRevs = do
  repos <- getLatestRepos backend allGleanRepos
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
findRepos :: Text -> [SymbolId]
findRepos pRepo =
  let repos = Map.toList $ Map.filterWithKey
        (const . Text.isPrefixOf pRepo . unRepoName) gleanIndices
  in concatMap (\(RepoName repo, repolangs) ->
      map (\(_, lang) -> SymbolId $ repo <> "/" <> toShortCode lang <> "/")
      repolangs) repos

-- | Symbols of the form "/repo/lang/" where pLang is a prefix of lang
findLanguages :: RepoName -> Text -> [SymbolId]
findLanguages repoName@(RepoName repo) pLang =
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
