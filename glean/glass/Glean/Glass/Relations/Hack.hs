{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Relations.Hack (
    difference
  , patchDescriptions
  ) where

import Control.Monad.State.Strict
import Data.Maybe
import Data.Text ( Text )
import Data.HashMap.Strict ( HashMap )
import Data.HashSet ( HashSet )
import Data.Map.Strict ( Map )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map

import Glean.Glass.Types
import Glean.Glass.SearchRelated

--
-- Additional filtering rules for what is in scope via inheritance
-- Implements the Hack rules for `X overrides Y syntactically`.
-- as a work around until we have recorded this `extends` relationship
-- in the Hack index.
--
-- - Removes the inherited children that are hidden by local names
-- - And a set of (implicit) `extends` methods that are now hidden
--
-- This is cumbersome to do in Glass. Idealy we index the sym -> sym
-- implict override relation, and do join in glass to remove those filtered
--
-- We need to recursively choose a winner.
--

-- | Traversal state. Record unique local names we've seen, which
-- containers we've visited. Return mappings and a final, updated index
data Env = Env {
    seenNames :: !(HashMap Text SymbolId), -- names we've seen and where
    seenContainers :: !(HashSet SymbolId), -- containers we have processed
    mappings :: !(HashMap SymbolId SymbolId), -- accumulated overrides
    indexedParents :: !ContainerIndex -- each container
  }

type ContainerIndex = HashMap SymbolId InheritedContainer

type TopoMap = HashMap SymbolId (HashSet SymbolId)

-- children of a container flattend to a name -> symid table
toNameMap :: [LocatedEntity] -> HashMap Text SymbolId
toNameMap = HashMap.fromList . map (\sym -> (localNameOf sym, symIdOf sym))

type S a = State Env a

-- initial state
newEnv :: SymbolId -> [RelatedLocatedEntities] -> ContainerIndex -> Env
newEnv baseContainerSym baseSyms allParents =
  Env {
    seenNames = toNameMap (map childRL baseSyms),
    seenContainers = HashSet.singleton baseContainerSym,
    mappings = mempty,
    indexedParents = allParents
  }

getSeenNames :: S (HashMap Text SymbolId)
getSeenNames = gets seenNames

getSeenContainers :: S (HashSet SymbolId)
getSeenContainers = gets seenContainers

getContainer :: SymbolId -> S (Maybe InheritedContainer)
getContainer sym = gets (HashMap.lookup sym . indexedParents)

updateSeenNames :: HashMap Text SymbolId  -> S ()
updateSeenNames seen = modify' $ \e@Env{..} ->
  e { seenNames = HashMap.union seenNames seen }

updateMappings :: HashMap SymbolId SymbolId -> S ()
updateMappings more = modify' $ \e@Env{..} ->
  e { mappings = HashMap.union mappings more }

updateContainer :: SymbolId -> InheritedContainer -> S ()
updateContainer sym contents = modify' $ \e@Env{..} ->
  e { indexedParents = HashMap.insert sym contents indexedParents }

addVisitedContainer :: SymbolId -> S ()
addVisitedContainer sym = modify' $ \e@Env{..} ->
  e { seenContainers = HashSet.insert sym seenContainers }

difference
  :: TopoMap
  -> SymbolId
  -> [RelatedLocatedEntities]
  -> [InheritedContainer]
  -> ([InheritedContainer], HashMap SymbolId SymbolId)
difference topoEdges baseSym baseChildrenSyms allParents =
  let state0 = newEnv baseSym  baseChildrenSyms initIndex
      final = execState (partitionOverrides topoEdges [baseSym]) state0
  in
    (HashMap.elems (indexedParents final), mappings final)
  where
    -- index parents by sym id to quickly look up their children
    initIndex :: HashMap SymbolId InheritedContainer
    initIndex = HashMap.fromList
      [ (symIdOf parent,p) | p@(parent, _) <- allParents ]

-- | Breadth first reverse-topological filtering in one pass
--
-- Remove overridden inherited methods, and note the relationship
-- as a synthetic searchRelated `extends` result
--
partitionOverrides :: TopoMap -> [SymbolId] -> S ()
partitionOverrides _ [] = pure ()
partitionOverrides topoMap (sym:rest) = do
  next <- case HashMap.lookup sym topoMap of -- topoographical direct parents
    Nothing -> pure []  -- sym has no parents, done with it
    Just parentsSet -> do
      seen <- getSeenContainers
      let toVisit = HashSet.toList (parentsSet `HashSet.difference` seen)
      catMaybes <$> mapM filterOneParent toVisit
  addVisitedContainer sym -- note this sym is done
  partitionOverrides topoMap (rest ++ next) -- and continue

--
-- with the current environment, filter the contents of just one container
-- (i.e. drop override mappings for anything here that's already been seen)
-- once done, these names are now 'seen' too and locked in.
--
filterOneParent :: SymbolId -> S (Maybe SymbolId)
filterOneParent parentSym = do
  mParent <- getContainer parentSym
  case mParent of
    Nothing -> return Nothing -- no more to visit
    Just (parent, children) -> do
      seen <- getSeenNames
      let (children', overrides) = go seen [] mempty children
      updateSeenNames (toNameMap children')
      updateContainer parentSym (parent, children')
      updateMappings overrides
      return (Just parentSym)
  where
    -- paritiion inherited symbols into those that are and are not overridden
    go _ children overrides [] = (children, overrides)
    go seen children overrides (ent : ents) =
      case HashMap.lookup (localNameOf ent) seen of
        Just this -> -- exact name locally! shadows the inherited symbol
          let !mapping = HashMap.insert this (symIdOf ent) overrides
          in go seen children mapping ents
        Nothing -> go seen (ent : children) overrides ents
          -- no override, retain `ent` as an inherited child

-- the raw marshalled tuple from the search result queries is a bit annoying
localNameOf :: LocatedEntity -> Text
localNameOf ((_ent, _file, _span, name), _sym) = name

symIdOf :: LocatedEntity -> SymbolId
symIdOf (_, sym) = sym

--
-- We then need to create synthetic `extends` facts for method overrides
-- Each key in `overrides` extends its value. We patch the child with
-- the symbol id and qname of the parent method it extends
--
patchDescriptions
  :: Map Text SymbolDescription
  -> HashMap SymbolId SymbolId
  -> Map Text SymbolDescription
patchDescriptions allsyms overrides = HashMap.foldlWithKey go allsyms overrides
  where
    go syms (SymbolId child) parent@(SymbolId p) = fromMaybe syms $ do
      desc <- Map.lookup child syms
      parentDescription <- Map.lookup p syms
      let qname = symbolDescription_name parentDescription
          childDesc = desc { symbolDescription_extends_relation =
            (symbolDescription_extends_relation desc) {
                relationDescription_firstParent = Just parent,
                relationDescription_firstParentName = Just qname
            }
          }
      return $ Map.insert child childDesc syms
