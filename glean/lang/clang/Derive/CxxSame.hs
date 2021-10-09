-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo, PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Derive.CxxSame
  ( deriveSame
  ) where

import Control.Exception
import Control.Monad
import Data.Int
import qualified Data.IntMap.Strict as IntMap
import Data.Graph as Graph
import Data.Tree as Tree

import Util.Log (logInfo)

import Glean
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.Declarations (getDeclId)

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
closed.

In Diffusion and the dead-code analyses we want a global view of
the complete set of declarations for a given entity. This
derivation pass establishes that, by extracting the components of
the graph formed by the Cxx.Same edges.

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
        (Cxx.Same _ (Just (Cxx.Same_key decl1 decl2))) = do
        let
          d1 = getDeclId decl1
          d2 = getDeclId decl2
          !nodes' =
            IntMap.insert (fromIntegral d1) decl1 $
            IntMap.insert (fromIntegral d2) decl2 nodes
          !edges' = IntMap.insertWith (const (d2:)) (fromIntegral d1) [d2] edges
        return (nodes',edges')
      procSame _ _ = throwIO $ ErrorCall "internal error: procSame"

    foldM procSame (nodes,edges) bs

  let
    adjacencyList :: [(Cxx.Declaration, Int64, [Int64])]
    adjacencyList =
      [ (decl, fromIntegral n, IntMap.findWithDefault [] n edges)
      | (n,decl) <- IntMap.toList nodes ]

    (theGraph, fromVertex, _) = Graph.graphFromEdges adjacencyList

    fromV :: Graph.Vertex -> Cxx.Declaration
    fromV v = case fromVertex v of (decl, _, _) -> decl

    generateFamilies :: [[Cxx.Declaration]] -> IO ()
    generateFamilies groups = writeFacts writer $ forM_ groups $ \group -> do
      fam <- makeFact @Cxx.DeclFamily group
      forM_ group $ \decl -> makeFact_ @Cxx.DeclToFamily $
        Cxx.DeclToFamily_key decl fam

  generateFamilies $ map (map fromV . Tree.flatten) $ Graph.components theGraph
