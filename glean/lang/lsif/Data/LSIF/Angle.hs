{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Convert Data.LSIF into glean/schema/lsif.angle-compatible data via JSON.

Note: this module generates Angle but has no dependency on the lsif schema, to
make developer iteration quicker.

-}

{-# LANGUAGE OverloadedStrings #-}

module Data.LSIF.Angle (toAngle) where

import Control.Monad.Extra ( concatMapM )
import Control.Monad.State.Strict
import Data.Aeson ( object, Value(String, Array), KeyValue(..) )
import Data.Aeson.Types ( Pair )
import Data.Function ( on )
import Data.List ( groupBy, sortOn )
import Data.Maybe ( catMaybes, fromMaybe, listToMaybe, mapMaybe )
import Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as IMap
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Vector as V

import Data.LSIF.Types
import Data.LSIF.JSON ({- instances -})

--
-- LSIF represents xrefs and target uses with cyclical graphs. Groups of related
-- vertices are shared via "result set" nodes, to which they point.
-- definitionResult or other nodes will refer to these result sets.
--
-- To embed into Glean, we process in two steps:
--  - first, find all anchor facts (files, ranges) and capture all edges between
--    files, refs, defs, result sets
--  - then, using that complete graph environment, extract
--    ref -> (file,def) pairs
--    to generate File-keyed definition, hover and xref facts
--
-- We can't rely on ordering between different vertex and edge types
-- in LSIF, only that facts will be defined before they are used.
--

-- | Parser env, to track command statements in LSIF that change scope
data Env =
  Env {
    root :: [Text], -- ^ a list of path prefixes to strip from file paths

    -- type environment. We often learn the inferred type of an id in its use
    typeEnv :: IntMap VertexType,

    -- tracking graph edges we care about

    -- "next" edges associate ranges (defs, decls or xres) with resultsets
    resultSet :: IntMap (Id_ ResultSetTy),

    -- textDocument/definition, edge from resultset back to ([def range], file)
    definitionFile :: IntMap FileDefs,

    -- textDocument/hover, edge from resultset to hovertext/lang
    hoverText :: IntMap (Id_ HoverTextTy),

    -- track file "contains" edge associations from file to any ranges
    fileContains :: IntMap [V.Vector Id]

  }

-- It is useful to record the type of ids along the way
-- We can infer these types from the use of the id, as edges lightly typed
data VertexType
  = ResultDefinitionType
  | ResultDeclarationType
  | ResultReferenceType
  | ResultSetType
  | ProjectType
  | FileType
  | DefinitionType
  | ReferenceType
  | DeclarationType
  deriving (Eq, Show)

-- Payload for an xref is a file id and the id of the definition range
data FileDefs
   = FileDefs
      {-# UNPACK #-}!(Id_ FileTy)
      !(V.Vector (Id_ DefinitionTy))

-- tag the Id with a type when we put it in the env
type Id_ a = Id

data DefinitionTy
-- data DeclarationTy
-- data ReferenceTy
data ResultSetTy
data FileTy
data HoverTextTy

tagResultSet :: Id -> Id_ ResultSetTy
tagResultSet = id

tagFile :: Id -> Id_ FileTy
tagFile = id

tagDefinitions :: V.Vector Id -> V.Vector (Id_ DefinitionTy)
tagDefinitions = id

-- Build up some lookup tables for the assoc lists between refs/defs/decls/files
-- to aid in generaing flatter angle facts
empty :: Env
empty = Env [] IMap.empty IMap.empty IMap.empty IMap.empty IMap.empty

-- Drop projectRoot prefix from URI to yield repo-relative paths for Glean
-- Some indexers generate uris outside of the repo. In those cases we
-- just try to remove local environment specific-stuff
filterRoot :: Text -> State Env Text
filterRoot path = do
  prefixes <- root <$> get -- try a few paths
  let clean = mapMaybe (\p -> Text.stripPrefix (p <> "/") path) prefixes
  pure (fromMaybe path (listToMaybe clean))

appendRoot :: Text -> State Env ()
appendRoot path = modify' (\e -> e { root = path : root e })

setRoot :: [Text] -> State Env ()
setRoot paths = modify' (\e -> e { root = paths })

insertType :: Id -> VertexType -> State Env ()
insertType (Id n) ty = modify' $ \e -> e { typeEnv =
    IMap.insert (fromIntegral n) ty (typeEnv e)
  }

-- Add a bunch of types in one go
insertTypes :: V.Vector Id -> VertexType -> State Env ()
insertTypes ids ty = modify' $ \e -> e { typeEnv = IMap.union imap (typeEnv e) }
  where
    imap =
      IMap.fromAscList .
      V.toList .
      V.map (\(Id i) -> (fromIntegral i, ty)) $
      ids

getTypeOf :: Id -> State Env (Maybe VertexType)
getTypeOf = lookupIMapEnv typeEnv

addToResultSet :: Id -> Id_ ResultSetTy -> State Env ()
addToResultSet (Id n) resultSetId  = modify' $ \e ->
  e { resultSet = IMap.insert (fromIntegral n) resultSetId (resultSet e) }

getResultSetOf :: Id -> State Env (Maybe (Id_ ResultSetTy))
getResultSetOf = lookupIMapEnv resultSet

addToDefinitionFile
  :: Id_ ResultSetTy
  -> Id_ FileTy
  -> V.Vector (Id_ DefinitionTy)
  -> State Env ()
addToDefinitionFile (Id resultSetId) fileId defIds  = modify' $ \e ->
  e { definitionFile = IMap.insert
        (fromIntegral resultSetId) (FileDefs fileId defIds) (definitionFile e)
  }

-- like addToDefinition but shares a reference (i.e. another edge) to exixting
-- file defs. We can use this to walk chains of resultsets/definition results
shareDefinitionFile :: Id_ ResultSetTy -> FileDefs -> State Env ()
shareDefinitionFile (Id resultSetId) fileDefs  = modify' $ \e ->
  e { definitionFile = IMap.insert
        (fromIntegral resultSetId) fileDefs (definitionFile e)
  }

getDefinitionFile :: Id_ ResultSetTy -> State Env (Maybe FileDefs)
getDefinitionFile = lookupIMapEnv definitionFile

addHoverToResultSet :: Id_ HoverTextTy -> Id_ ResultSetTy -> State Env ()
addHoverToResultSet hoverId (Id resultSetId) = modify' $ \e ->
  e { hoverText = IMap.insert
        (fromIntegral resultSetId) hoverId (hoverText e)
  }

getHoverTextId :: Id_ ResultSetTy -> State Env (Maybe (Id_ HoverTextTy))
getHoverTextId = lookupIMapEnv hoverText

addFileContainsIds :: Id_ FileTy -> V.Vector Id -> State Env ()
addFileContainsIds (Id fileId) contents = modify' $ \e ->
  e { fileContains = IMap.insertWith (<>)
        (fromIntegral fileId) [contents] (fileContains e)
  }

lookupIMapEnv :: (Env -> IntMap a) -> Id -> State Env (Maybe a)
lookupIMapEnv f (Id n) = do
  imap <- f <$> get
  pure (IMap.lookup (fromIntegral n) imap)

-- A predicate "block", containing multiple facts
data Predicate
  = Predicate { pName :: !Text, pFacts :: [Value] }

-- emit Glean-friendly JSON in "predicate/fact" form
emitPredicateBlocks :: [Predicate] -> [Value]
emitPredicateBlocks facts = map emitPredicate sets
  where
    -- set of one fact block per predicate
    sets = map (\ps -> Predicate
             (pName (head ps))
             (concatMap pFacts ps)
           )
         . groupBy ((==) `on` pName)
         . sortOn dependencyOrder
         $ facts

-- | Emit predicate batches in topologically sorted order, so that later facts
-- can refer to facts in predicates previously issued.
-- This is a bit janky.
dependencyOrder :: Predicate -> Text
dependencyOrder p = case pName p of
  "lsif.Document.1" -> "0"
  "lsif.Range.1" -> "1"
  -- these refer to Range.1
  "lsif.Definition.1" -> "3"
  "lsif.Declaration.1" -> "4"
  -- these refer to Declaration.1, Range.1
  "lsif.Reference.1" -> "5"
  "lsif.DefinitionHover.1" -> "6"
  "lsif.DefinitionUse.1" -> "7"
  -- everything else
  p -> "2"  <> p

emitPredicate :: Predicate -> Value
emitPredicate (Predicate name facts) = object
  [ "predicate" .= text name
  , "facts" .= Array (V.fromList facts)
  ]

--
-- | Convert LSIF json to Glean json
--
-- We map over the statements, in order, generating 0 or more facts for Glean
-- LSIF guarantees things are defined before references to them are used, and
-- uses unique ids for facts. We can use that to simplify processing
--
-- vertexes correspond to facts holding new data
-- edges relate existnig vertex facts to each other (with new facts)
--
toAngle :: [Text] -> LSIF -> Value
toAngle prefixPaths (LSIF lsif) =
  -- first pass, get all types and edges, emit range/file facts
 let (facts,env) = runState (do
          setRoot prefixPaths
          V.mapM factToAngle lsif
        ) empty
  -- second pass, build xrefs and hover maps
     moreFacts = evalState emitFileFactSets env

  -- group by predicate, flatten and generate all data
 in Array $ V.fromList $ emitPredicateBlocks $
      concat (V.toList facts) ++ moreFacts

-- | Process each key/fact pair
factToAngle :: KeyFact -> State Env [Predicate]
factToAngle (KeyFact _ MetaData{..}) = do
  appendRoot projectRoot
  predicate "lsif.Metadata.1" ([
    "lsifVersion" .= version,
    "positionEncoding" .= positionEncoding
    ] ++ (case toolInfo of -- optional
            Nothing -> []
            Just ToolInfo{..} ->
              ["toolInfo" .= object [
                "toolName" .= toolName,
                "toolArgs" .= toolArgs,
                "version" .= version
              ]]
      ))

factToAngle (KeyFact n Project{..}) = do
  insertType n ProjectType
  predicateId "lsif.Project.1" n [ "kind" .= fromEnum kind ]

factToAngle (KeyFact _ PackageInformation{..}) =
  predicate "lsif.PackageInformation.1" [
    "name" .= name,
    "manager" .= manager,
    "version" .= version
  ]

-- LSIF Documents are uri/language pairs. We generate a src.File nested fact
factToAngle (KeyFact n Document{..}) = do
  insertType n FileType
  path <- filterRoot uri
  predicate "lsif.Document.1"
    [ "file" .= object [
            factId n, -- generates a new src.File fact with this id
            "key" .= path
         ],
      "language" .= fromEnum language
    ]

-- Push document or project identifier onto open stack. This is list of
-- documents or projects for which facts may still be emitted.
-- we don't currently use these event markers for anything
factToAngle (KeyFact _ Event{}) = pure []

-- Associate a range fact with a result set.
-- We use this to track chains of ref -> resultset -> def for later generation
-- note: inV targets are always resultSets, outVs may be ranges or resultSets
factToAngle (KeyFact _ (Edge EdgeNext outV inV)) = [] <$ do
  addToResultSet outV (tagResultSet inV)

-- record which resultset this hover text is a member of
factToAngle (KeyFact _ (Edge EdgeTextDocumentHover outV inV)) = [] <$
  addHoverToResultSet inV outV

-- edge:item for property:references associates an inV range with a
-- textDocument/references result. record that the range is a reference type
factToAngle (KeyFact _ (Item _outV inVs _fileId (Just References))) = [] <$
  insertTypes inVs ReferenceType

-- items : these add ranges to definition results
-- we check the result set of this definitionResult, then record
-- that the result set points at this definition ranges / document pair
factToAngle (KeyFact _ (Item outV inVs{-ranges-} fileId Nothing)) = [] <$ do
  insertTypes inVs DefinitionType -- todo: do we see DeclarationTy here?
  -- check if we already know the result set of this definitionResult
  -- this is generated by a textDocument/definition edge
  mResultSet <- getResultSetOf outV
  case mResultSet of
    Nothing -> -- don't know result set, so log the edge from
               -- definition to def/file
      addToDefinitionFile (tagResultSet outV) {- definitionResult -}
        (tagFile fileId) (tagDefinitions inVs)
    Just resultSetId ->
      addToDefinitionFile resultSetId
        (tagFile fileId) (tagDefinitions inVs)

-- collect textDocument/definition and textDocument/hover edges to resultsets
-- If we have seen the definitionResult inV already, then just add an entry
-- from the result set. Otherwise, record the edge from definition to result
factToAngle (KeyFact _ (Edge EdgeTextDocumentDefinition outV inV)) = [] <$ do
  mFileDefs <- getDefinitionFile (tagResultSet inV)
  case mFileDefs of
    Just fileDefs -> shareDefinitionFile (tagResultSet outV) fileDefs
    Nothing -> pure ()
  -- always log that this definitionResult is member of resultset
  addToResultSet inV (tagResultSet outV)

-- Range facts. These are range spans, 0-indexed, and may have optional
-- tag/labels indicating what kind of span they are. They may have no text
-- or no tag attached, generally.
factToAngle (KeyFact n (SymbolRange range mtag)) = do
  case tagToTy mtag of -- record the type if we have the tag handy
    Nothing -> pure ()
    Just ty -> insertType n ty

  -- emit a range fact for this id
  predicateId "lsif.Range.1" n $
    [ "range" .= toRange range
    ] ++ mFullRange ++ mText
    where
      mFullRange = fromMaybe [] (tagToRange =<< mtag)
      mText = fromMaybe [] (tagToText =<< mtag)

-- Hover text
factToAngle (KeyFact n (HoverResult contents)) = do
  facts <- V.forM contents $ \case
    HoverSignature language str ->
      predicateId "lsif.HoverContent" n
        [ "text" .= object [ "key" .= str ] -- bare lsif.HoverText
        , "language" .= fromEnum language
        ]
    HoverText str ->
      predicateId "lsif.HoverContent" n
        [ "text" .= object [ "key" .= str ]
        , "language" .= fromEnum UnknownLanguage
        ]
  return $ concat (V.toList facts)

-- These declare some types of output nodes (things pointed to by resultsets)
factToAngle (KeyFact n DefinitionResult) =
  [] <$ insertType n ResultDefinitionType
factToAngle (KeyFact n DeclarationResult) =
  [] <$ insertType n ResultDeclarationType
factToAngle (KeyFact n ReferenceResult) =
  [] <$ insertType n ResultReferenceType
factToAngle (KeyFact n ResultSet) =
  [] <$ insertType n ResultSetType

-- record the range ids that are contained in a file id
-- or from file id to project id.
factToAngle (KeyFact _ (Contains fileId inVs)) = [] <$
  addFileContainsIds (tagFile fileId) inVs

factToAngle _ = pure []

--
-- After consuming the LSIF graph, we should have the full set of
-- {def,decl,ref} -> resutlset -> definitionResult data
--
emitFileFactSets :: State Env [Predicate]
emitFileFactSets = do
  fileSet <- IMap.toList . fileContains <$> get
  ps <- mapM (\(k,v) ->
          emitFileFacts (tagFile (Id $ fromIntegral k)) v)
          fileSet
  pure (concat ps)

data IdSet =
    IdSet {
      defIds :: V.Vector Id,
      declIds :: V.Vector Id,
      refIds :: V.Vector Id,
      fileIds :: V.Vector Id
    }

-- Lookup the type of each id as we recorded it in the env
partitionByType :: V.Vector Id -> State Env IdSet
partitionByType ids = do
  -- lookup the types for each range id
  tys <- V.mapM (\i -> (,i) <$> getTypeOf i) ids

  -- partition by type, we don't care about ordering
  let (defIds, rest1) = V.unstablePartition
        ((== Just DefinitionType) . fst) tys
      (declIds, rest2) = V.unstablePartition
        ((== Just DeclarationType) . fst) rest1
      (refIds, rest3) = V.unstablePartition
        ((== Just ReferenceType) . fst) rest2
      (fileIds, _remains) = V.unstablePartition
        ((== Just FileType) . fst) rest3

  return $ IdSet
    (V.map snd defIds)
    (V.map snd declIds)
    (V.map snd refIds)
    (V.map snd fileIds)

emitFileFacts :: Id_ FileTy -> [V.Vector Id] -> State Env [Predicate]
emitFileFacts fileId = concatMapM (emitFileFacts_ fileId)

-- We delay this until the post-processing phase to ensure all
-- - item, textDocument/definition, etc.. nodes are added
emitFileFacts_ :: Id_ FileTy -> V.Vector Id -> State Env [Predicate]
emitFileFacts_ fileId rawIds = do
  IdSet{..} <- partitionByType rawIds

  let defFacts = emitDefinitions fileId defIds
      declFacts = emitDeclarations fileId declIds
      projectFacts = emitProjects fileId fileIds {- from projectId to files -}
  xrefFacts <- emitReferences fileId refIds
  useFacts <- emitTargetUses fileId refIds
  hoverFacts <- emitHovers fileId defIds
  return $ catMaybes
    [defFacts, declFacts, xrefFacts, useFacts, hoverFacts, projectFacts]

emitProjects :: Id -> V.Vector Id -> Maybe Predicate
emitProjects projId ids
  | V.null ids = Nothing
  | otherwise = Just $
      Predicate "lsif.ProjectDocument.1"
        (V.toList $ V.map (\rangeId -> (object . pure . key)
          [ "file" .= rangeId
          , "project" .= projId
          ]
        ) ids)

emitHovers :: Id -> V.Vector Id -> State Env (Maybe Predicate)
emitHovers fileId ids = do
  hoverBodies <- V.mapMaybe id <$> V.mapM (generateHoverFacts fileId) ids
  if V.null hoverBodies
    then pure Nothing
    else return $ Just $ Predicate "lsif.DefinitionHover.1" $
      V.toList hoverBodies

emitReferences :: Id -> V.Vector Id -> State Env (Maybe Predicate)
emitReferences fileId ids = do
  xrefBodies <- concat. V.toList <$> V.mapM (generateFileReferences fileId) ids
  if null xrefBodies
    then pure Nothing
    else return $ Just $
      Predicate "lsif.Reference.1" $
        map (object . pure . key) xrefBodies

emitTargetUses :: Id -> V.Vector Id -> State Env (Maybe Predicate)
emitTargetUses fileId ids = do
  useBodies <- concat. V.toList <$> V.mapM (generateTargetUses fileId) ids
  if null useBodies
    then pure Nothing
    else return $ Just $
      Predicate "lsif.DefinitionUse.1" $
        map (object . pure . key) useBodies

-- For each refId, look up the result set, find the result sets file and defs
-- and generate a single flat file -> ref -> targetDef fat
generateFileReferences :: Id -> Id -> State Env [[Pair]]
generateFileReferences fileId refRangeId =
  withResultSet refRangeId getDefinitionFile $ \case
    FileDefs targetFileId targetRanges -> pure
      [ [ "file" .= fileId
        , "range" .= refRangeId
        , "target" .= (object . pure . key) -- inner key to lsif.Definition
            [ "file" .= targetFileId
            , "range" .= targetRange
            ]
        ]
      | targetRange <- V.toList targetRanges
      ]

-- inverse of file references, emit them here since they're handy
-- maybe later we want to use textDocument/reference edges
generateTargetUses :: Id -> Id -> State Env [[Pair]]
generateTargetUses fileId refRangeId =
  withResultSet refRangeId getDefinitionFile $ \case
    FileDefs targetFileId targetRanges -> pure
      [ [ "target" .= (object . pure . key) -- inner key to lsif.Definition
            [ "file" .= targetFileId
            , "range" .= targetRange
            ]
        , "file" .= fileId
        , "range" .= refRangeId
        ]
      | targetRange <- V.toList targetRanges
      ]

emitDefinitions :: Id_ FileTy -> V.Vector (Id_ DefinitionTy) -> Maybe Predicate
emitDefinitions = emitDeclDefs "lsif.Definition.1"

emitDeclarations :: Id_ FileTy -> V.Vector Id -> Maybe Predicate
emitDeclarations = emitDeclDefs "lsif.Declaration.1"

emitDeclDefs
  :: Text -> Id_ FileTy -> V.Vector (Id_ DefinitionTy) -> Maybe Predicate
emitDeclDefs name fileId ids
  | V.null ids = Nothing
  | otherwise = Just $
      Predicate name
        (V.toList $ V.map (\rangeId -> (object . pure . key)
          [ "file" .= fileId
          , "range" .= rangeId
          ]
        ) ids)

generateHoverFacts :: Id -> Id -> State Env (Maybe Value)
generateHoverFacts fileId defRangeId =
  withResultSet defRangeId getHoverTextId $ \hoverFactId ->
    pure $ pure $ object $ pure $ key
      [ "defn" .= (object . pure . key)
          [ "file" .= fileId
          , "range" .= defRangeId
          ]
      , "hover" .= hoverFactId
      ]

-- get the result set of an id, use that result set id to look up another
-- environment, then apply a function to the result.
-- used to jump from A to B via a [resultset] node
withResultSet
  :: MonadPlus m
 => Id -> (Id_ ResultSetTy -> State Env (Maybe t)) -> (t -> State Env (m a))
 -> State Env (m a)
withResultSet id f g = do
  mResultSet <- getResultSetOf id
  case mResultSet of
    Nothing -> pure mzero
    Just resultSetId -> do
      mv <- f resultSetId
      case mv of
        Nothing -> pure mzero
        Just a -> g a

--
-- JSON-generating utilities
--

predicate :: Applicative f => Text -> [Pair] -> f [Predicate]
predicate name facts = pure [Predicate name [object [key facts]]]

predicateId :: Applicative f => Text -> Id -> [Pair] -> f [Predicate]
predicateId name id_ facts =
  pure [Predicate name [object [factId id_, key facts ]]]

toRange :: Range -> Value
toRange Range{..} =
  object [ -- lsif.Range data type
    "lineBegin" .= line start,
    "columnBegin" .= character start,
    "lineEnd" .= line end,
    "columnEnd" .= character end
  ]

key :: KeyValue kv => [Pair] -> kv
key xs = "key" .= object xs

factId :: KeyValue kv => Id -> kv
factId (Id id_) = "id" .= id_

string :: Text -> Value
string s = object [ "key" .= s ]

text :: Text -> Value
text = String

tagToTy :: Maybe Tag -> Maybe VertexType
tagToTy (Just Definition{}) = Just DefinitionType
tagToTy (Just Declaration{}) = Just DeclarationType
tagToTy (Just Reference{}) = Just ReferenceType
tagToTy _ = Nothing

tagToText :: KeyValue a => Tag -> Maybe [a]
tagToText Definition{..} = Just ["text" .= string tagText]
tagToText Declaration{..} = Just ["text" .= string tagText]
tagToText Reference{..} = Just ["text" .= string tagText]
tagToText _ = Nothing

tagToRange :: KeyValue a => Tag -> Maybe [a]
tagToRange Definition{..} = Just ["fullRange" .= toRange fullRange]
tagToRange Declaration{..} = Just ["fullRange" .= toRange fullRange]
tagToRange _ = Nothing
