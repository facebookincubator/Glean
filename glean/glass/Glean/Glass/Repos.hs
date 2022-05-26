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
  , filterRepoLang
  ) where

import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Control.Concurrent.Async ( withAsync )
import Control.Concurrent.STM
    ( readTVarIO, writeTVar, atomically, newTVarIO, TVar )
import Control.Exception ( uninterruptibleMask_, throwIO )
import Data.Maybe ( catMaybes )
import Data.Set ( Set )
import Data.Text ( Text )

import Glean.Util.Periodic ( doPeriodically )
import qualified Data.Map.Strict as Map
import Glean.Util.Time ( DiffTimePoints )
import qualified Glean
import qualified Glean.Repo as Glean

import Glean.Glass.Base
import Glean.Glass.SymbolId ( toShortCode )
import Glean.Glass.Types
    ( Path(Path),
      RepoName(RepoName),
      Language(..),
      SymbolId(SymbolId),
      unRepoName,
      ServerException(ServerException),
    )

import Glean.Glass.RepoMapping  -- site-specific

--  all pairs (repoName, Language) which maps to glean db,
--  except for "test" repoName
repoLangs :: [ (RepoName, Language) ]
repoLangs =
  dedup $ concatMap f gleanIndicesList where
  isTest (RepoName "test", _) = True
  isTest _ = False
  dedup = Set.toList . Set.fromList
  f (repo, dbLangs) = map (\(_db, lang) -> (repo, lang)) dbLangs
  gleanIndicesList = filter (not . isTest) $ Map.toList gleanIndices

-- return a RepoName if indexed by glean
toRepoName :: Text -> Maybe RepoName
toRepoName repo =
  let repoName = RepoName repo in
  case Map.lookup repoName gleanIndices of
       Just _ -> Just repoName
       Nothing -> Nothing

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

filterRepoLang :: Maybe RepoName -> Maybe Language -> IO [(RepoName, Language)]
filterRepoLang mRepoName mLang = do
  let pairs = filter pred repoLangs
  when (null pairs) err
  return pairs
  where
    pred (r, l) = case (mRepoName, mLang) of
      (Nothing, Nothing) -> True
      (Just rr, Just ll) -> r == rr && l == ll
      (Nothing, Just ll) -> l == ll
      (Just rr, Nothing) -> r == rr

    err = throwIO $ ServerException $ case (mRepoName, mLang) of
      (Nothing, Nothing) -> error "no repo or language"
      (Just r, Nothing) -> "Unknown repository: " <> unRepoName r
      (Nothing, Just l) -> "No repository for " <> toShortCode l
      (Just r, Just l) -> "Unknown combination: " <> unRepoName r
        <> "(" <> toShortCode l <> ")"

-- Do something simple to map SCS repo to Glean repos
-- Names from configerator/scm/myles/service as a start
-- This should be in a config or SV to make onboarding simple, or from Glean
-- properties?
fromSCSRepo :: RepoName -> Maybe Language -> [GleanDBName]
fromSCSRepo r hint
  | Just rs <- Map.lookup r gleanIndices
  = map fst $ case hint of
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

  | ".thrift" `Text.isSuffixOf` file = Just Language_Thrift

  | ".rs" `Text.isSuffixOf` file = Just Language_Rust
  | ".erl" `Text.isSuffixOf` file = Just Language_Erlang
  | ".go" `Text.isSuffixOf` file = Just Language_Go
  | ".ts" `Text.isSuffixOf` file = Just Language_TypeScript
  | ".tsx" `Text.isSuffixOf` file = Just Language_TypeScript

  | "TARGETS" `Text.isSuffixOf` file = Just Language_Buck
  | "BUCK" `Text.isSuffixOf` file = Just Language_Buck

  | otherwise = Nothing

--
-- Operating on the latest repo state
--
-- TODO: exception handling behavior
--

-- | Fetch all latest dbs we care for
getLatestRepos
  :: Glean.Backend b => b -> Set GleanDBName  -> IO Glean.LatestRepos
getLatestRepos backend repoNames = Glean.getLatestRepos backend $ \name ->
  GleanDBName name `Set.member` repoNames

-- | Introduce a latest repo cache.
-- TODO: this should pass the configured repo list through
withLatestRepos
  :: Glean.Backend b
   => b
   -> DiffTimePoints
   -> (TVar Glean.LatestRepos -> IO a)
   -> IO a
withLatestRepos backend freq f = do
  repos <- getLatestRepos backend allGleanRepos
  tv <- newTVarIO repos
  withAsync (worker tv) $ \_async -> f tv
  where
    worker tv =
      doPeriodically freq $
      uninterruptibleMask_ $
        -- prevents the update from being cancelled while in progress
        -- which can cause memory leaks if the process exits
        -- immediately. This is benign, but can lead to ASAN test
        -- failures.
      updateLatestRepos backend tv

-- | Update a TVar with the latest repos
-- TODO: should take latest configuration repo list
updateLatestRepos
  :: Glean.Backend b => b -> TVar Glean.LatestRepos -> IO ()
updateLatestRepos backend tv = do
  repos <- getLatestRepos backend allGleanRepos
  atomically $ writeTVar tv repos

-- | Lookup latest repo in the cache
lookupLatestRepos
  :: TVar Glean.LatestRepos -> [GleanDBName] -> IO [(GleanDBName, Glean.Repo)]
lookupLatestRepos tv repoNames = do
  repos <- Glean.latestRepos <$> readTVarIO tv
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
