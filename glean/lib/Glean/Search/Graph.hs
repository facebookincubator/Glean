-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Search.Graph (
    findLocalGraph,
  ) where

import Control.Monad
import Data.Default
import Data.Either (partitionEithers)
import Data.Int
import Data.List (genericLength, foldl')
import Data.Maybe
import qualified Data.Set as Set

import qualified Glean
import Glean.Schema.Cxx1.Types as Cxx
import Glean.Schema.Query.Cxx1.Types as Query.Cxx
import Glean.Search.Types as Search
import Glean.Util.Declarations (matchDeclaration)
import Glean.Util.Some


data Queue a = Queue ![a] ![a]

emptyQueue :: Queue a
emptyQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue front back) = Queue front (a:back)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue [] back) = dequeue (Queue (reverse back) [])
dequeue (Queue (a:front) back) = Just (a, Queue front back)


-- Breadth first search a directed graph for a given number of steps.
--
-- The graph is given in the form of a function which given a vertex v returns
-- a list of labeled neighbours, those are pairs of the form (w, l), where
-- v is a neighbour, and l is a label of the edge between v and w.
--
-- Once the given threshold of edges is reached, the computation is terminated.
-- The returned value is the list of visted vertices in the order in which they
-- were visited. Each vertex in the list carries the list of the traversed edges
-- from that vertex, and the number of the edges which we did not traverse
-- (because of reaching the threshold of steps).

bfsM :: (Monad m, Ord v, Num i)
    => (v -> m [(v, l)])
    -> v
    -> Int
    -> m [(v, [(v, l)], i)]
bfsM getNeighbors v0 steps =
  reverse <$> go Set.empty [] (enqueue v0 emptyQueue) steps
    where
  -- we accumulate the reversed list of visited vertices and the set of already
  -- visited vertices
  go visitedSet visitedList queue steps = case dequeue queue of
    Nothing -> return visitedList
    (Just (v, queue')) ->
      if Set.member v visitedSet
      then go visitedSet visitedList queue' steps
      else do
        neighbors <- getNeighbors v
        let
          (kept, dropped) = splitAt steps neighbors
          visitedSet' = Set.insert v visitedSet
          visitedList' = (v, kept, genericLength dropped) : visitedList
          queue'' = foldl' (\q (w, _) -> enqueue w q) queue' kept
          steps' = steps - length kept
        go visitedSet' visitedList' queue'' steps'

getDeclaration :: Some Glean.Backend
               -> Glean.Repo
               -> Int64
               -> IO Cxx.FunctionDeclaration
getDeclaration backend repo declId =
  fmap head $ Glean.runQuery_ backend repo $ Glean.recursive $ Glean.query $
    Query.Cxx.FunctionDeclaration_with_id declId

getFamily :: Some Glean.Backend
          -> Glean.Repo
          -> Cxx.FunctionDeclaration
          -> IO [Cxx.FunctionDeclaration]
getFamily backend repo funDecl = do
  declToFamilies <-
    Glean.runQuery_ backend repo $ Glean.recursive $ Glean.query $
      Query.Cxx.DeclToFamily_with_key $ def {
        Query.Cxx.declToFamily_key_decl = Just def {
          Query.Cxx.declaration_function_ = Just $
            Query.Cxx.FunctionDeclaration_with_id $
            Cxx.functionDeclaration_id funDecl
        }
      }
  return $ case declToFamilies of
    [] -> [funDecl]
    [dtf] -> fromMaybe [] $ do
      dtfk <- Cxx.declToFamily_key dtf
      let declFamily = Cxx.declToFamily_key_family dtfk
      dfk <- Cxx.declFamily_key declFamily
      return [ decl | (Declaration_function_ decl) <- dfk ]
    _ -> error "There should be at most one family for a declaration"

getDeclTargetsFrom
  :: Some Glean.Backend
  -> Glean.Repo
  -> Cxx.Declaration
  -> IO [Cxx.Declaration]
getDeclTargetsFrom backend repo d = do
  results <- Glean.runQuery_ backend repo $ Glean.recursive $ Glean.query $
    Query.Cxx.DeclarationTargets_with_key $ def {
      Query.Cxx.declarationTargets_key_source = Just $ matchDeclaration d }
  return $ concat $ forM results $ \ dt ->
    let (Just dtk) = Cxx.declarationTargets_key dt
    in Cxx.declarationTargets_key_targets dtk

getFunctionCallsFrom :: Some Glean.Backend
                     -> Glean.Repo
                     -> Cxx.FunctionDeclaration
                     -> IO [Cxx.FunctionDeclaration]
getFunctionCallsFrom backend repo funDecl = do
  decls <- getDeclTargetsFrom backend repo (Cxx.Declaration_function_ funDecl)
  return [ g | Cxx.Declaration_function_ g <- decls ]

getDeclSourcesTo
  :: Some Glean.Backend
  -> Glean.Repo
  -> Cxx.Declaration
  -> IO [Cxx.Declaration]
getDeclSourcesTo backend repo d = do
  results <- Glean.runQuery_ backend repo $ Glean.recursive $ Glean.query $
    Query.Cxx.DeclarationSources_with_key $ def {
      Query.Cxx.declarationSources_key_target = Just $ matchDeclaration d }
  return $ concat $ forM results $ \ ds ->
    let (Just dsk) = Cxx.declarationSources_key ds
    in Cxx.declarationSources_key_sources dsk

getFunctionCallsTo :: Some Glean.Backend
                     -> Glean.Repo
                     -> Cxx.FunctionDeclaration
                     -> IO [Cxx.FunctionDeclaration]
getFunctionCallsTo backend repo funDecl = do
  decls <- getDeclSourcesTo backend repo (Cxx.Declaration_function_ funDecl)
  return [ g | Cxx.Declaration_function_ g <- decls ]

getOverridingMethods :: Some Glean.Backend
                     -> Glean.Repo
                     -> Cxx.FunctionDeclaration
                     -> IO [Cxx.FunctionDeclaration]
getOverridingMethods backend repo funDecl = do
  results <- Glean.runQuery_ backend repo $ Glean.recursive $ Glean.query $
    Query.Cxx.MethodOverrides_with_key $ def {
      Query.Cxx.methodOverrides_key_base = Just $
        Query.Cxx.FunctionDeclaration_with_id $
        Cxx.functionDeclaration_id funDecl
      }
  return [ Cxx.methodOverrides_key_derived mok
         | mo <- results
         , let (Just mok) = Cxx.methodOverrides_key mo ]

getOverriddenMethod :: Some Glean.Backend
                    -> Glean.Repo
                    -> Cxx.FunctionDeclaration
                    -> IO (Maybe Cxx.FunctionDeclaration)
getOverriddenMethod backend repo funDecl = do
 results <- Glean.runQuery_ backend repo $ Glean.recursive $ Glean.query $
   Query.Cxx.MethodOverrides_with_key $ def {
     Query.Cxx.methodOverrides_key_derived = Just $
       Query.Cxx.FunctionDeclaration_with_id $
       Cxx.functionDeclaration_id funDecl
     }
 return $ case results of
   [] -> Nothing
   [mo] -> let
       (Just mok) = Cxx.methodOverrides_key mo
     in Just $ Cxx.methodOverrides_key_base mok
   _ -> error "A declaration should override at most one method"

findLocalGraph
  :: Some Glean.Backend
  -> Glean.Repo
  -> Int64
  -> Int
  -> IO Search.FindLocalGraphResult
findLocalGraph backend repo declId steps = do
  let
    forwardEdges :: Cxx.FunctionDeclaration
                 -> IO [(Cxx.FunctionDeclaration, Either FunctionCall Override)]
    forwardEdges funDecl = do
      family <- getFamily backend repo funDecl
      callss <- forM family $ \same -> do
        targets <- getFunctionCallsFrom backend repo same
        return $ map (\t -> (t, Left $ FunctionCall funDecl same t)) targets
      overridess <- forM family $ \same -> do
        methods <- getOverridingMethods backend repo same
        return $ map (\m -> (m, Right $ Override funDecl same m)) methods
      return $ concat overridess ++ concat callss

    backwardEdges :: Cxx.FunctionDeclaration
                 -> IO [(Cxx.FunctionDeclaration, Either FunctionCall Override)]
    backwardEdges funDecl = do
      family <- getFamily backend repo funDecl
      callss <- forM family $ \same -> do
        sources <- getFunctionCallsTo backend repo same
        return $ map (\s -> (s, Left $ FunctionCall s same funDecl)) sources
      overrides <- forM family $ \same -> do
        method <- getOverriddenMethod backend repo same
        return $ case method of
          (Just m) -> [(m, Right $ Override m same funDecl)]
          Nothing -> []
      return $ concat overrides ++ concat callss

  funDecl <- getDeclaration backend repo declId

  visitedForward <- bfsM forwardEdges funDecl steps

  sourceVertices <- forM visitedForward $ \(s, edges, dropped) -> do
    s' <- getDeclaration backend repo (functionDeclaration_id s)
    (called, overriding) <- fmap partitionEithers $ forM edges $ \(t, e) -> do
      t' <- getDeclaration backend repo (functionDeclaration_id t)
      case e of
        (Left (FunctionCall v l w)) -> do
          when (Cxx.functionDeclaration_id v /= Cxx.functionDeclaration_id s)
            $ error "invalid source in a function call"
          let v' = s'

          l' <- getDeclaration backend repo (functionDeclaration_id l)

          when (Cxx.functionDeclaration_id w /= Cxx.functionDeclaration_id t)
            $ error "invalid target in a function call"
          let w' = t'

          return $ Left $ FunctionCall v' l' w'
        (Right (Override v l w)) -> do
          when (Cxx.functionDeclaration_id v /= Cxx.functionDeclaration_id s)
            $ error "invalid base in a method override"
          let v' = s'

          l' <- getDeclaration backend repo (functionDeclaration_id l)

          when (Cxx.functionDeclaration_id w /= Cxx.functionDeclaration_id t)
            $ error "invalid derived in a method override"
          let w' = t'
          return $ Right $ Override v' l' w'
    return $ SourceVertex s' called overriding dropped

  visitedBackward <- bfsM backwardEdges funDecl steps

  targetVertices <- forM visitedBackward $ \(t, edges, dropped) -> do
    t' <- getDeclaration backend repo (functionDeclaration_id t)
    (calling, overridden) <- fmap partitionEithers $ forM edges $ \(s, e) -> do
      s' <- getDeclaration backend repo (functionDeclaration_id s)
      case e of
        (Left (FunctionCall v l w)) -> do
          when (Cxx.functionDeclaration_id v /= Cxx.functionDeclaration_id s)
            $ error "invalid source in a function call"
          let v' = s'

          l' <- getDeclaration backend repo (functionDeclaration_id l)

          when (Cxx.functionDeclaration_id w /= Cxx.functionDeclaration_id t)
            $ error "invalid target in a function call"
          let w' = t'

          return $ Left $ FunctionCall v' l' w'
        (Right (Override v l w)) -> do
          when (Cxx.functionDeclaration_id v /= Cxx.functionDeclaration_id s)
            $ error "invalid base in a method override"
          let v' = s'

          l' <- getDeclaration backend repo (functionDeclaration_id l)

          when (Cxx.functionDeclaration_id w /= Cxx.functionDeclaration_id t)
            $ error "invalid derived in a method override"
          let w' = t'
          return $ Right $ Override v' l' w'
    return $ TargetVertex t' calling (listToMaybe overridden) dropped

  return $ Search.FindLocalGraphResult sourceVertices targetVertices
