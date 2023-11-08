{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Derive.CxxSame
  ( deriveSame
  ) where

import Control.Exception
import Control.Monad
import Data.Coerce
import Data.Int
import qualified Data.IntMap.Strict as IntMap
import Data.Graph as Graph
import Data.Proxy
import Data.Tree as Tree

import Util.Log (logInfo)

import Glean
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.Declarations (getDeclId)
import Glean.Util.PredSet (PredSet)
import qualified Glean.Util.PredSet as PredSet

import Derive.Types

-- -----------------------------------------------------------------------------

{- |
Derive Cxx.DeclFamily predicates from Cxx.Same predicates.

The indexer produces Cxx.Same predicates which indicates that two
declarations refer to the same underlying entity. Typically this
expresses the connection between a declaration and a definition,
but there may in general be many declarations for the same
definition, and even multiple definitions.

The Cxx.Same edges produced by the indexer are not transitively
closed. We want to find the components of the graph represented by the
cxx.Same edges, and produce:

  predicate DeclFamily : [Declaration]

  predicate DeclToFamily :
    {
      decl : Declaration,
      family : DeclFamily,
    }

The basic algorithm is:
* For each @Cxx.Same d1 d2@,
  * add @d1->d2@ to the graph
  * find the components of this graph (/O(n)/)
  * each component is a @Cxx.DeclFamily@
* For each @Cxx.DeclFamily@
  * Emit a @Cxx.DeclToFamily@ predicate for each decl in the family
-}
deriveSame :: Backend e => e -> Config -> Writer -> IO ()
deriveSame e cfg writer = do
  logInfo "deriveSame"

  let
    q :: Query Cxx.Same
    q = maybe id limit (cfgMaxQueryFacts cfg) $
       limitBytes (cfgMaxQuerySize cfg) allFacts

  -- The nodes and edges are IntMap because 'getDeclId' is not a single
  -- predicate type.  We cannot use PredMap here.
  (nodes,edges) <- runQueryEachBatch e (cfgRepo cfg) q (mempty, mempty)
    $ \(nodes,edges) bs -> do
    let
      procSame (nodes, edges)
        (Cxx.Same fid (Just (Cxx.Same_key decl1 decl2))) = do
        let
          d1 = getDeclId decl1
          d2 = getDeclId decl2
          id = coerce fid :: IdOf Cxx.Same
          one_id = PredSet.singleton id
          ins (decl, set) _ = (decl, PredSet.insert id set)
          !nodes' =
            IntMap.insertWith ins (fromIntegral d1) (decl1, one_id) $
            IntMap.insertWith ins (fromIntegral d2) (decl2, one_id) nodes
          !edges' = IntMap.insertWith (const (d2:)) (fromIntegral d1) [d2] edges
        return (nodes',edges')
      procSame _ _ = throwIO $ ErrorCall "internal error: procSame"

    foldM procSame (nodes,edges) bs

  let
    adjacencyList :: [((Cxx.Declaration, PredSet Cxx.Same), Int64, [Int64])]
    adjacencyList =
      [ (decl, fromIntegral n, IntMap.findWithDefault [] n edges)
      | (n,decl) <- IntMap.toList nodes ]

    (theGraph, fromVertex, _) = Graph.graphFromEdges adjacencyList

    fromVs :: [Graph.Vertex] -> ([Cxx.Declaration], PredSet Cxx.Same)
    fromVs vs = (ds, foldr PredSet.union mempty sets)
      where
      (ds, sets) = unzip [ d | v <- vs, let (d, _, _) = fromVertex v ]

    generateFamilies :: [([Cxx.Declaration], PredSet Cxx.Same)] -> IO ()
    generateFamilies groups =
      writeFacts writer $
        forM_ groups $ \(group, sames) -> do
          let sources = coerce $ PredSet.toList sames
          fam <- makeFact @Cxx.DeclFamily group
          tos <- forM group $ \decl ->
            makeFact @Cxx.DeclToFamily $ Cxx.DeclToFamily_key decl fam
          derivedFrom sources [fam]
          derivedFrom sources tos

  generateFamilies $ map (fromVs . Tree.flatten) $ Graph.components theGraph

  completePredicates e (cfgRepo cfg) $
    CompletePredicates_derived $ CompleteDerivedPredicate $
      getName (Proxy @Cxx.DeclFamily)
  completePredicates e (cfgRepo cfg) $
    CompletePredicates_derived $ CompleteDerivedPredicate $
      getName (Proxy @Cxx.DeclToFamily)
