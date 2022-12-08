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
import Control.Monad.Extra (whenJust)
import Data.Maybe
import Data.Text ( Text )
import Data.List ( sortOn )
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
    visitedContainers :: !(HashSet SymbolId), -- containers we have processed
    mappings :: !(HashMap SymbolId SymbolId), -- accumulated overrides
    indexedParents :: !ContainerIndex -- each container
  }

type ContainerIndex = HashMap SymbolId InheritedContainer

type TopoMap = HashMap SymbolId (HashSet SymbolId)
type TopoKinds = HashMap SymbolId SymbolKind

-- children of a container flattend to a name -> symid table
toNameMap :: [LocatedEntity] -> HashMap Text SymbolId
toNameMap = HashMap.fromList . map (\sym -> (localNameOf sym, symIdOf sym))

type S a = State Env a

-- initial state
newEnv :: HashMap Text SymbolId -> ContainerIndex -> Env
newEnv baseNames allParents =
  Env {
    seenNames = baseNames,
    visitedContainers = mempty,
    mappings = mempty,
    indexedParents = allParents
  }

getSeenNames :: S (HashMap Text SymbolId)
getSeenNames = gets seenNames

getContainerContents :: SymbolId -> S (Maybe InheritedContainer)
getContainerContents sym = gets (HashMap.lookup sym . indexedParents)

updateSeenNames :: HashMap Text SymbolId  -> S ()
updateSeenNames seen = modify' $ \e@Env{..} ->
  e { seenNames = HashMap.union seenNames seen }

updateMappings :: HashMap SymbolId SymbolId -> S ()
updateMappings more = modify' $ \e@Env{..} ->
  e { mappings = HashMap.union mappings more }

updateContainerContents :: SymbolId -> InheritedContainer -> S ()
updateContainerContents sym contents = modify' $ \e@Env{..} ->
  e { indexedParents = HashMap.insert sym contents indexedParents }

getVisitedContainers :: S (HashSet SymbolId)
getVisitedContainers = gets visitedContainers

updateVisitedContainers :: SymbolId -> S ()
updateVisitedContainers sym = modify' $ \e@Env{..} ->
  e { visitedContainers = HashSet.insert sym visitedContainers }

difference
  :: TopoMap
  -> TopoKinds
  -> SymbolId
  -> [RelatedLocatedEntities]
  -> [InheritedContainer]
  -> ([InheritedContainer], HashMap SymbolId SymbolId)
difference topoEdges topoKinds baseSym baseSymNames allParents =
  let zero = newEnv (toNameMap (map childRL baseSymNames)) initIndex
      final = execState (partitionOverrides topoEdges topoKinds [baseSym]) zero
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
partitionOverrides :: TopoMap -> TopoKinds -> [SymbolId] -> S ()
partitionOverrides _ _ [] = pure ()
partitionOverrides topoMap kinds syms = go syms
  where
    go [] = pure () -- done
    go (sym:rest) = do
      -- add these symbols to confirmed names
      visited <- (sym `HashSet.member`) <$> getVisitedContainers
      when (not visited) $ do -- guarded for base case
        mContents <- getContainerContents sym
        whenJust (snd <$> mContents) $ updateSeenNames . toNameMap
      -- then visit parents and filter
      next <- case HashMap.lookup sym topoMap of -- first level of parents
        Nothing -> pure []  -- sym has no parents, done with it
        Just parentsSet -> do
          let toVisit = sortOn (kindOrder kinds) (HashSet.toList parentsSet)
          catMaybes <$> mapM filterOneParent toVisit -- then filter each parent
      updateVisitedContainers sym -- note this sym is done
      go (next ++ rest) -- and continue dfs

--
-- with the current environment, filter the contents of just one container
-- (i.e. drop override mappings for anything here that's already been seen)
-- once done, these names are now 'seen' too and locked in.
--
filterOneParent :: SymbolId -> S (Maybe SymbolId)
filterOneParent parentSym = do
  mContents <- getContainerContents parentSym
  case mContents of
    Nothing -> return Nothing -- no more to visit
    Just (parent, children) -> do
      seen <- getSeenNames
      let (children', overrides) = go seen [] mempty children
      updateContainerContents parentSym (parent, children')
      updateMappings overrides
      return (Just parentSym) -- to visit next
  where
    -- partition inherited symbols into those that are and are not overridden
    go _ children overrides [] = (children, overrides)
    go seen children overrides (ent : ents) =
      case HashMap.lookup (localNameOf ent) seen of
        -- both seen already and not seen in a prior visit to this symbol
        Just thisSymId | thisSymId /= symIdOf ent ->
          let !mapping = HashMap.insert thisSymId (symIdOf ent) overrides
          in go seen children mapping ents
        _ -> go seen (ent : children) overrides ents
          -- no override, retain `ent` as a visible symbol

-- | Traits are "copy/paste" semantics, and are directly included into the child
-- before resolving inherited class symbols.
kindOrder :: TopoKinds -> SymbolId -> Int
kindOrder kinds sym = case HashMap.lookup sym kinds of
  Just SymbolKind_Trait -> 0
  Just SymbolKind_Class_ -> 1
  Just SymbolKind_Interface -> 2
  Just{} -> 3
  Nothing -> 100

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
