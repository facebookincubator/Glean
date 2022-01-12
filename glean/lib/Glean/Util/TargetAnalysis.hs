{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Util.TargetAnalysis
  ( -- * Util
    bfsM, bfsPM
    -- * Searches
  , getDependencyLocators
  , getTraceIncludeIds, getTraceIncludes, getAllTraceIncludes
  , getEdges
    -- * Query helpers
  , getTraceKey, getTraceFile, getTraceDeclarations
  ) where

import Control.Monad (forM)
import Data.Default (def)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Extra (nubOrd)
import qualified Data.Set as Set
import Data.Text (Text)

import Glean
import qualified Glean.Schema.Buck.Types as Buck
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Query.Buck.Types as Query.Buck
import qualified Glean.Schema.Query.Cxx1.Types as Query.Cxx
import qualified Glean.Schema.Query.Src.Types as Query.Src
import Glean.Pretty.Cxx ()
import Glean.Util.PredMap (PredMap)
import qualified Glean.Util.PredMap as PredMap
import qualified Glean.Util.PredSet as PredSet


-- -----------------------------------------------------------------------------

-- | @bfsM getSuccs roots@ search of a directed graph
-- given
-- * a function which for any vertex returns the list of succesors
--     (i.e. the ends of the edges starting in that vertex)
-- * a list of initial vertices
-- returns a map from the reachable vertices to their successors
--
-- @getSuccs@ is called at most once on each @IdOf p@.
-- The values in 'PredMap' are the results of all @getSuccs@ calls.
bfsM :: (Monad m, Ord a)
     => (a -> m [a]) -- getSuccs
     -> [a] -- initial set of vertices
     -> m (Map a [a]) -- the graph
bfsM getSuccs = go Map.empty
  where
    go m [] = return m
    go m vs = do
      let dedup =
            filter (`Map.notMember` m)
            $ (Set.toList . Set.fromList) vs
      assocs <- forM dedup $ \v -> do
        succs <- getSuccs v
        return (v, succs)
      let
        m' = Map.union m (Map.fromList assocs)
        vs' = concatMap snd assocs
      go m' vs'

-- | @bfsPM getSuccs roots@ search of a directed graph
-- given
-- * a function @getSuccs@ which for any vertex returns the list of succesors
--     (i.e. the ends of the edges starting in that vertex)
-- * a list of initial vertices @roots@
-- returns a map from the reachable vertices to their successors
--
-- @getSuccs@ is called at most once on each @IdOf p@.
-- The values in 'PredMap' are the results of all @getSuccs@ calls.
bfsPM
  :: (Monad m, Predicate p)
   => (IdOf p -> m [IdOf p]) -- getSuccs
   -> [IdOf p] -- initial set of vertices
   -> m (PredMap p [IdOf p]) -- the graph
bfsPM getSuccs = go PredMap.empty
  where
    go mAcc vsIn = do
      let newDistinctAsc = filter (`PredMap.notMember` mAcc)
            $ (PredSet.toAscList . PredSet.fromList) vsIn
      if null newDistinctAsc
        then return mAcc
        else do
          assocsDistinctAsc <- forM newDistinctAsc $ \v -> do
            succs <- getSuccs v
            return (v, succs)
          let
            mAccNew = mAcc <> PredMap.fromDistinctAscList assocsDistinctAsc
            vsOut = concatMap snd assocsDistinctAsc
          go mAccNew vsOut

-- -----------------------------------------------------------------------------

getDependencyLocators :: Buck.Locator_id
                     -> Glean.Haxl w [Buck.Locator_id]
getDependencyLocators locatorId = do
  targetDependenciess
    <- Glean.search_ $ Glean.query
    $ Query.Buck.TargetDependencies_with_key $ def
      { Query.Buck.targetDependencies_key_target = Just $
          Query.Buck.Target_with_key $ def
          { Query.Buck.target_key_locator = Just $
              Query.Buck.Locator_with_id locatorId
          }
      }
  dependencyLocatorIdss
    <- forM targetDependenciess
    $ \(Buck.TargetDependencies _ (Just targetDependenciesKey)) -> do
      let dependencies =
            Buck.targetDependencies_key_dependencies targetDependenciesKey
      return
        [ Buck.locator_id $ Buck.dependency_target dependency
        | dependency <- dependencies
        ]
  return $ concat dependencyLocatorIdss

-- -----------------------------------------------------------------------------

getTraceKey :: Cxx.Trace_id
            -> Glean.Haxl w Cxx.Trace_key
getTraceKey traceId = do
  [Cxx.Trace _ (Just traceKey)]
    <- Glean.search_ $ Glean.query
    $ Query.Cxx.Trace_with_id traceId
  return traceKey

getTraceFile :: Cxx.Trace_id
             -> Glean.Haxl w Text
getTraceFile traceId = do
  traceKey <- getTraceKey traceId
  let fileId = Src.file_id $ Cxx.trace_key_file traceKey
  [Src.File _ (Just file)]
    <- Glean.search_ $ Glean.query
    $ Query.Src.File_with_id fileId
  return file

getTraceDeclarations :: Cxx.Trace_id
                     -> Glean.Haxl w [Cxx.FunctionDeclaration_id]
getTraceDeclarations traceId = do
  traceKey <- getTraceKey traceId
  let declarationsId = Cxx.declarations_id
                     $ Cxx.trace_key_declarations traceKey
  [Cxx.Declarations _ (Just declarations)]
    <- Glean.search_ $ Glean.query
    $ Query.Cxx.Declarations_with_id declarationsId
  return
    [ Cxx.functionDeclaration_id funDecl
    | Cxx.Declaration_function_ funDecl <- declarations ]

-- -----------------------------------------------------------------------------

getTraceIncludeIds :: IdOf Cxx.Trace -> Glean.Haxl w [IdOf Cxx.Trace]
getTraceIncludeIds traceId = do
  traceKey <- getKeyOfId traceId
  let ppTraceId :: IdOf Cxx.PPTrace
      ppTraceId = getId (Cxx.trace_key_preprocessor traceKey)
  ppTraceKey <- getKeyOfId ppTraceId
  return
    [ getId trace
    | (Just trace) <-
        [ Cxx.includeTrace_trace includeTrace
        | Cxx.PPEvent_include_ includeTrace <-
            Cxx.pPTrace_key_events ppTraceKey
        ]
    ]

getTraceIncludes :: Cxx.Trace_id
                  -> Glean.Haxl w [Cxx.Trace_id]
getTraceIncludes = fmap (map (fromFid . idOf)) . getTraceIncludeIds . IdOf . Fid

-- | Map from trace to directly included traces
getAllTraceIncludes
  :: [Cxx.Trace_id] -> Glean.Haxl w (Map Cxx.Trace_id [Cxx.Trace_id])
getAllTraceIncludes = bfsM getTraceIncludes

-- -----------------------------------------------------------------------------

-- | Result should have no duplicates
getFunctionTargetsFrom
  :: Cxx.FunctionDeclaration_id
  -> Glean.Haxl w [Cxx.FunctionDeclaration_id]
getFunctionTargetsFrom funDeclId = do
  dtks <- Glean.search_ $ Glean.keys $ Glean.query $
    Query.Cxx.DeclarationTargets_with_key $ def {
      Query.Cxx.declarationTargets_key_source = Just def
        { Query.Cxx.declaration_function_ = Just $
            Query.Cxx.FunctionDeclaration_with_id funDeclId } }
  return $ nubOrd
    [ Cxx.functionDeclaration_id f
    | dtk <- dtks
    , Cxx.Declaration_function_ f <-
        Cxx.declarationTargets_key_targets dtk
    ]


getFamily :: Cxx.FunctionDeclaration_id
             -> Glean.Haxl w [Cxx.FunctionDeclaration_id]
getFamily funDeclId = do
  -- the length of `dtfks` will be at most 1
  dtfks <- Glean.search_ $ Glean.keys $ Glean.query $
    Query.Cxx.DeclToFamily_with_key $ def {
      Query.Cxx.declToFamily_key_decl = Just $ def {
        Query.Cxx.declaration_function_ = Just $
          Query.Cxx.FunctionDeclaration_with_id funDeclId
      }
    }
  fmap concat
    $ forM dtfks $ \ dtfKey -> do
      let declFamily = Cxx.declToFamily_key_family dtfKey
      [Cxx.DeclFamily _ (Just declList)]
        <- Glean.search_ $ Glean.query $
          Query.Cxx.DeclFamily_with_id $ Cxx.declFamily_id declFamily
      return [Cxx.functionDeclaration_id fd
             | (Cxx.Declaration_function_ fd) <- declList]

getOverriddenMethods :: Cxx.FunctionDeclaration_id
                    -> Glean.Haxl w [Cxx.FunctionDeclaration_id]
getOverriddenMethods funDeclId = do
  results <- Glean.search_ $ Glean.recursive $ Glean.query $
     Query.Cxx.MethodOverrides_with_key $ def {
       Query.Cxx.methodOverrides_key_derived = Just $
         Query.Cxx.FunctionDeclaration_with_id funDeclId
       }
  return $ flip map results $ \(Cxx.MethodOverrides _ (Just mok)) ->
    Cxx.functionDeclaration_id $ Cxx.methodOverrides_key_base mok

getEdges :: Cxx.FunctionDeclaration_id
         -> Glean.Haxl w [Cxx.FunctionDeclaration_id]
getEdges funDeclId = do
  calls <- getFunctionTargetsFrom funDeclId
  family <- getFamily funDeclId
  overridden <- getOverriddenMethods funDeclId
  let edges = calls ++ family ++ overridden
  return edges
