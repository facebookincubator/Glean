{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Relations.Hack (
    difference
  , patchDescriptions

  -- * for testing
  , TopoMap
  , TopoKinds
  , ContainerIndex
  , NamedSymbol(..)
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

--
-- Abstract out the representation to just these required operations, so
-- things are testable without a full Glean entity
--
class NamedSymbol a where
  symIdOf :: a -> SymbolId
  localNameOf :: a -> Text

-- the raw marshalled tuple from the search result queries is a bit annoying
-- but we get them from the Glean data queries automatically so shrug
instance NamedSymbol LocatedEntity where
  symIdOf (_, sym) = sym
  localNameOf ((_ent, _file, _span, name), _sym) = name

-- | Traversal state. Record unique local names we've seen, which
-- containers we've visited. Return mappings and a final, updated index
data Env a = Env {
    seenNames :: !(HashMap Text SymbolId), -- names we've seen and where
    visitedContainers :: !(HashSet SymbolId), -- containers we have processed
    mappings :: !(HashMap SymbolId a), -- accumulated overrides, child to parent
    indexedParents :: !(ContainerIndex a) -- each container
  }

-- Convenience type for a parent with a list of contained children
type ContainerLike a = (a,[a])

-- And indexed containers of named/symbol-like things
type ContainerIndex a = HashMap SymbolId (ContainerLike a)

type TopoMap = HashMap SymbolId (HashSet SymbolId)
type TopoKinds = HashMap SymbolId SymbolKind

-- children of a container flattend to a name -> symid table
toNameMap :: NamedSymbol e => [e] -> HashMap Text SymbolId
toNameMap = HashMap.fromList . map (\sym -> (localNameOf sym, symIdOf sym))

type S a r = State (Env a) r

-- initial state
newEnv :: NamedSymbol a => HashMap Text SymbolId -> ContainerIndex a -> Env a
newEnv baseNames allParents =
  Env {
    seenNames = baseNames,
    visitedContainers = mempty,
    mappings = mempty,
    indexedParents = allParents
  }

getSeenNames :: S a (HashMap Text SymbolId)
getSeenNames = gets seenNames

getContainerContents :: SymbolId -> S a (Maybe (ContainerLike a))
getContainerContents sym = gets (HashMap.lookup sym . indexedParents)

updateSeenNames :: HashMap Text SymbolId -> S a ()
updateSeenNames seen = modify' $ \e@Env{..} ->
  e { seenNames = HashMap.union seenNames seen }

updateMappings :: NamedSymbol a => HashMap SymbolId a -> S a ()
updateMappings more = modify' $ \e@Env{..} ->
  e { mappings = HashMap.union mappings more } -- early mappings win over later

updateContainerContents :: SymbolId -> ContainerLike a -> S a ()
updateContainerContents sym contents = modify' $ \e@Env{..} ->
  e { indexedParents = HashMap.insert sym contents indexedParents }

getVisitedContainers :: S a (HashSet SymbolId)
getVisitedContainers = gets visitedContainers

updateVisitedContainers :: SymbolId -> S a ()
updateVisitedContainers sym = modify' $ \e@Env{..} ->
  e { visitedContainers = HashSet.insert sym visitedContainers }

difference
  :: NamedSymbol e
  => TopoMap
  -> TopoKinds
  -> SymbolId
  -> [e]
  -> [(e, [e])]
  -> ([(e, [e])], HashMap SymbolId e)
difference topoEdges topoKinds baseSym baseSymNames allParents =
  let zero = newEnv (toNameMap baseSymNames) initIndex
      final = execState (partitionOverrides topoEdges topoKinds [baseSym]) zero
  in
    (HashMap.elems (indexedParents final), mappings final)
  where
    -- index parents by sym id to quickly look up their children
    initIndex = HashMap.fromList
      [ (symIdOf parent, p) | p@(parent, _) <- allParents ]

-- | Breadth first reverse-topological filtering in one pass
--
-- Remove overridden inherited methods, and note the relationship
-- as a synthetic searchRelated `extends` result
--
partitionOverrides
  :: NamedSymbol a => TopoMap -> TopoKinds -> [SymbolId] -> S a ()
partitionOverrides topoMap kinds syms = go syms
  where
    go [] = pure () -- done
    go (sym:rest) = do
      -- add these symbols to confirmed names
      visited <- (sym `HashSet.member`) <$> getVisitedContainers
      when (not visited) $ do -- guarded for base case
        mContents <- getContainerContents sym
        whenJust (snd <$> mContents) $ \contents -> do
          updateSeenNames (toNameMap contents)
          mapM_ filterOneParent rest -- and fold this update over siblings
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
filterOneParent :: NamedSymbol a => SymbolId -> S a (Maybe SymbolId)
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
          let !mapping = HashMap.insert thisSymId ent overrides
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

--
-- We then need to create synthetic `extends` facts for method overrides
-- Each key in `overrides` extends its value. We patch the child with
-- the symbol id and qname of the parent method it extends
--
patchDescriptions
  :: Map Text SymbolDescription
  -> HashMap SymbolId (SymbolId, QualifiedName)
  -> Map Text SymbolDescription
patchDescriptions allsyms overrides = HashMap.foldlWithKey go allsyms overrides
  where
    go syms (SymbolId child) (parent, qname) = fromMaybe syms $! do
      desc <- Map.lookup child syms
      let !childDesc = desc { symbolDescription_extends_relation =
            (symbolDescription_extends_relation desc) {
                relationDescription_firstParent = Just parent,
                relationDescription_firstParentName = Just qname
            }
          }
      return $! Map.insert child childDesc syms
