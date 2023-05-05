{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Data.LSIF.Env (

    Env(..),
    FileDefs(..),
    emptyEnv,

    -- some silly type tags
    tagFile,
    tagResultSet,
    tagDefinitions,

    -- tags as values
    Id(..),
    Id_,
    VertexType(..),
    ResultSetTy,
    FileTy,
    DefinitionTy,
    IdVector,

    -- modify env
    appendRoot,
    insertType,
    insertTypes,
    addToResultSet,
    addToDefinitionFile,
    addHoverToResultSet,
    addMonikerToResultSet,
    insertSymbolKind,
    addFileContainsIds,
    shareDefinitionFile,

    -- lookups in env
    getTypeOf,
    getResultSetOf,
    getDefinitionFile,
    getHoverTextId,
    getMonikerId,
    getSymbolKind,

    -- running a parser and getting results
    Parse,

  ) where

import Control.Monad.State.Strict
import Data.Int ( Int64 )
import Data.IntMap ( IntMap )
import Data.Text ( Text )
import qualified Data.IntMap.Strict as IMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.LSIF.Gen ( SymbolKind, Id(..) )

-- Environment used to track things when we analyze the LSIF graph.
-- We can mostly process LSIF linearly, but for xrefs and related predicates
-- we track a few chains of values.
--
-- We also need to track types of identifiers (decls, refs, defns, files,
-- projects, and synthetic lsif node types, to distinguish things)
--

--
-- LSIF represents xrefs and target uses with cyclical graphs. Groups of related
-- vertices are shared via "result set" nodes, to which they point.
-- definitionResult or other nodes will refer to these result sets.
--
-- To embed into Glean, we process in two steps:
--  - first, find all anchor facts (files, ranges) and capture all edges between
--    files, refs, defs, result sets
--  - then, using that complete graph environment, extract ref -> (file,def)
--    pairs to generate File-keyed definition, hover and xref facts
--
-- We can't rely on ordering between different vertex and edge types in LSIF,
-- only that facts will be defined before they are used.
--

-- | Parser env, to track command statements in LSIF that change scope
data Env =
  Env {
    root :: [Text], -- ^ a list of path prefixes to strip from file paths

    -- type environment. We often learn the inferred type of an id in its use
    typeEnv :: !(IntMap VertexType),

    -- tracking graph edges we care about

    -- "next" edges associate ranges (defs, decls or xres) with resultsets
    resultSet :: !(IntMap (Id_ ResultSetTy)),

    -- textDocument/definition, edge from resultset back to ([def range], file)
    definitionFile :: !(IntMap FileDefs),

    -- textDocument/hover, edge from resultset to hovertext/lang
    hoverText :: !(IntMap (Id_ HoverTextTy)),

    -- symbol, file and project "moniker" records
    moniker :: !(IntMap (Id_ MonikerTy)),

    -- associate definition rangeids with their kind
    symbolKinds :: !(IntMap SymbolKind),

    -- track file "contains" edge associations from file to any ranges
    fileContains :: !(IntMap [IdVector])

  }


-- | Run the fact generate with state Env
type Parse a = forall m . Monad m => StateT Env m a

-- It is useful to record the type of ids along the way
-- We can infer these types from the use of the id, as edges lightly typed
data VertexType
  = ProjectType -- lsif.Project
  | FileType -- src.File / lsif.Document
  | DefinitionType
  | ReferenceType -- Item property:references or tag:reference
  | DeclarationType
  deriving (Eq, Show)

-- Payload for an xref is a file id and the id of the definition range
data FileDefs
   = FileDefs
      {-# UNPACK #-}!(Id_ FileTy)
      !IdVector

-- | vectors of ids can be stored a bit more compactly
type IdVector = U.Vector Int64

fill :: V.Vector Id -> IdVector
fill v = U.generate (V.length v) (\i -> case v `V.unsafeIndex` i of Id n -> n)

-- tag the Id with a type when we put it in the env
type Id_ a = Id

data DefinitionTy
-- data DeclarationTy
-- data ReferenceTy
data ResultSetTy
data FileTy
data HoverTextTy
data MonikerTy
data RangeTy

tagResultSet :: Id -> Id_ ResultSetTy
tagResultSet = id

tagFile :: Id -> Id_ FileTy
tagFile = id

tagDefinitions :: V.Vector Id -> V.Vector (Id_ DefinitionTy)
tagDefinitions = id

-- Build up some lookup tables for the assoc lists between refs/defs/decls/files
-- to aid in generaing flatter angle facts
emptyEnv :: Env
emptyEnv = Env [] IMap.empty IMap.empty IMap.empty
  IMap.empty IMap.empty IMap.empty IMap.empty

appendRoot :: Text -> Parse ()
appendRoot path = modify' (\e -> e { root = path : root e })

insertType :: Id -> VertexType -> Parse ()
insertType (Id n) ty = modify' $ \e -> e { typeEnv =
    IMap.insert (fromIntegral n) ty (typeEnv e)
  }

-- Add a bunch of types in one go
insertTypes :: V.Vector Id -> VertexType -> Parse ()
insertTypes ids ty = modify' $ \e -> e { typeEnv = IMap.union imap (typeEnv e) }
  where
    imap = IMap.fromAscList $ V.toList $
              V.map (\(Id i) -> (fromIntegral i, ty)) ids

getTypeOf :: Id -> Parse (Maybe VertexType)
getTypeOf = lookupIMapEnv typeEnv

addToResultSet :: Id -> Id_ ResultSetTy -> Parse ()
addToResultSet (Id n) resultSetId  = modify' $ \e ->
  e { resultSet = IMap.insert (fromIntegral n) resultSetId (resultSet e) }

getResultSetOf :: Id -> Parse (Maybe (Id_ ResultSetTy))
getResultSetOf = lookupIMapEnv resultSet

addToDefinitionFile
   :: Id_ ResultSetTy -> Id_ FileTy -> V.Vector (Id_ DefinitionTy) -> Parse ()
addToDefinitionFile (Id resultSetId) fileId defIds  = modify' $ \e ->
  e { definitionFile = IMap.insert
        (fromIntegral resultSetId)
          (FileDefs fileId (fill defIds)) (definitionFile e)
  }

-- like addToDefinition but shares a reference (i.e. another edge) to exixting
-- file defs. We can use this to walk chains of resultsets/definition results
shareDefinitionFile :: Id_ ResultSetTy -> FileDefs -> Parse ()
shareDefinitionFile (Id resultSetId) fileDefs  = modify' $ \e ->
  e { definitionFile = IMap.insert
        (fromIntegral resultSetId) fileDefs (definitionFile e)
  }

getDefinitionFile :: Id_ ResultSetTy -> Parse (Maybe FileDefs)
getDefinitionFile = lookupIMapEnv definitionFile

addHoverToResultSet :: Id_ HoverTextTy -> Id_ ResultSetTy -> Parse ()
addHoverToResultSet hoverId (Id resultSetId) = modify' $ \e ->
  e { hoverText = IMap.insert
        (fromIntegral resultSetId) hoverId (hoverText e)
  }

getHoverTextId :: Id_ ResultSetTy -> Parse (Maybe (Id_ HoverTextTy))
getHoverTextId = lookupIMapEnv hoverText

addMonikerToResultSet :: Id_ MonikerTy -> Id_ ResultSetTy -> Parse ()
addMonikerToResultSet monikerId (Id resultSetId) = modify' $ \e ->
  e { moniker = IMap.insert
        (fromIntegral resultSetId) monikerId (moniker e)
  }

getMonikerId :: Id_ MonikerTy -> Parse (Maybe (Id_ HoverTextTy))
getMonikerId = lookupIMapEnv moniker

insertSymbolKind :: Id_ RangeTy -> SymbolKind -> Parse ()
insertSymbolKind (Id rangeId) kind = modify' $ \e ->
  e { symbolKinds = IMap.insert
        (fromIntegral rangeId) kind (symbolKinds e)
  }

getSymbolKind :: Id -> Parse (Maybe SymbolKind)
getSymbolKind = lookupIMapEnv symbolKinds

addFileContainsIds :: Id_ FileTy -> V.Vector Id -> Parse ()
addFileContainsIds (Id fileId) contents = modify' $ \e ->
  e { fileContains = IMap.insertWith (<>)
        (fromIntegral fileId) [fill contents] (fileContains e)
  }

lookupIMapEnv :: (Env -> IntMap a) -> Id -> Parse (Maybe a)
lookupIMapEnv f (Id n) = do
  imap <- f <$> get
  pure (IMap.lookup (fromIntegral n) imap)

