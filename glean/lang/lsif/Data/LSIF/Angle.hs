{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Convert Data.LSIF into glean/schema/lsif.angle-compatible data via JSON.

Note: no dependency on the schema, to make iteration a bit quicker. Longer term
we might want this typed for maintenance reasons.

-}

{-# LANGUAGE OverloadedStrings #-}

module Data.LSIF.Angle (toAngle) where

import Control.Monad.State.Strict
import Data.Aeson ( object, Value(String, Array), KeyValue(..) )
import Data.Aeson.Types ( Pair )
import Data.IntMap ( IntMap )
import Data.Maybe ( fromMaybe, listToMaybe, mapMaybe )
import Data.Text ( Text )
import qualified Data.IntMap.Strict as IMap
import qualified Data.Text as Text
import qualified Data.Vector as V

import Data.LSIF.Types
import Data.LSIF.JSON ({- instances -})

-- Converts from AESON-parsed standard LSIF into Glean-compatible types

-- | Parser env, to track command statements in LSIF that change scope
data Env =
  Env {
    root :: [Text], -- ^ a list of path prefixes to strip

    -- track some of the types associated with ids
    idType :: IntMap VertexType,

    -- tracking edge relationships we care about
    -- "next" edges associate range vertices with result sets
    resultSet :: IntMap (Id_ ResultSetTy),

    -- textDocument/definition, associate resultset with ([def range], file)
    definitionFile :: IntMap FileDefs,

    -- textDocument/hover, associate resultset with a hovertext/lang
    hoverText :: IntMap (Id_ HoverTextTy)

  }

-- Type tags for various vertices we need
data VertexType
  = ResultDefinitionType
  | ResultDeclarationType
  | ResultReferenceType
  | ResultSetType

  | DefinitionType
  | ReferenceType
  | DeclarationType
  deriving (Eq)

-- Strict pair of file id/ definition ids
data FileDefs
   = FileDefs
      {-# UNPACK #-}!(Id_ FileTy)
      !(V.Vector (Id_ DefinitionTy))

-- tag the Id with a type when we put it in the env
type Id_ a = Id

data DefinitionTy
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
empty = Env [] IMap.empty IMap.empty IMap.empty IMap.empty

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
insertType (Id n) ty = modify' $ \e -> e { idType =
    IMap.insert (fromIntegral n) ty (idType e)
  }

-- Add a bunch of types in one go
insertTypes :: V.Vector Id -> VertexType -> State Env ()
insertTypes ids ty = modify' $ \e -> e { idType = IMap.union imap (idType e) }
  where
    imap = IMap.fromAscList . V.toList . V.map (\(Id i) -> (fromIntegral i, ty)) $ ids

getTypeOf :: Id -> State Env (Maybe VertexType)
getTypeOf = lookupIMapEnv idType

addToResultSet :: Id -> Id_ ResultSetTy -> State Env ()
addToResultSet (Id n) resultSetId  = modify' $ \e ->
  e { resultSet = IMap.insert (fromIntegral n) resultSetId (resultSet e) }

getResultSetOf :: Id -> State Env (Maybe (Id_ ResultSetTy))
getResultSetOf = lookupIMapEnv resultSet

addToDefinitionFile :: Id_ ResultSetTy -> Id_ FileTy -> V.Vector (Id_ DefinitionTy) -> State Env ()
addToDefinitionFile (Id resultSetId) fileId defIds  = modify' $ \e ->
  e { definitionFile = IMap.insert
        (fromIntegral resultSetId) (FileDefs fileId defIds) (definitionFile e)
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

lookupIMapEnv :: (Env -> IntMap a) -> Id -> State Env (Maybe a)
lookupIMapEnv f (Id n) = do
  imap <- f <$> get
  pure (IMap.lookup (fromIntegral n) imap)

_lookupIn :: t -> (t -> IntMap a) -> Id -> Maybe a
_lookupIn env f (Id n) = IMap.lookup (fromIntegral n) (f env)

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
-- Currently we emit one distinct predicate/fact pair per LSIF row, which is
-- verbose. We should group by lsif.angle predicates first, before generating
-- the final value, and likely improves load time.
--
toAngle :: [Text] -> LSIF -> Value
toAngle prefixPaths (LSIF facts) = Array $ V.concatMap V.fromList $
  evalState (setRoot prefixPaths >> V.mapM factToAngle facts) empty

-- | Process each key/fact pair
factToAngle :: KeyFact -> State Env [Value]
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

factToAngle (KeyFact _ Project{..}) =
  predicate "lsif.Project.1" [ "kind" .= fromEnum kind ]

factToAngle (KeyFact _ PackageInformation{..}) =
  predicate "lsif.PackageInformation.1" [
    "name" .= name,
    "manager" .= manager,
    "version" .= version
  ]

-- LSIF Documents are uri/language pairs. We generate a src.File nested fact
factToAngle (KeyFact n Document{..}) = do
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

-- collect textDocument/definition and textDocument/hover edges to resultsets
factToAngle (KeyFact _ (Edge EdgeTextDocumentDefinition outV inV)) = [] <$ do
  addToResultSet inV (tagResultSet outV)

-- record which resulset this hover text is a member of
factToAngle (KeyFact _ (Edge EdgeTextDocumentHover outV inV)) = [] <$
  addHoverToResultSet inV outV

-- Adds more type information about these ranges
factToAngle (KeyFact _ (Item _outV inVs _fileId (Just References))) = [] <$
  insertTypes inVs ReferenceType

-- items : these add ranges to definition results
-- we check the result set of this definitionResult, then record
-- that the result set points at this definition ranges / document pair
factToAngle (KeyFact _ (Item outV inVs fileId Nothing)) = [] <$ do
  insertTypes inVs DefinitionType -- todo: do we see DeclarationTy here?
  mResultSet <- getResultSetOf outV -- unsound: depends on ordering of resultSet
  case mResultSet of
    Nothing -> return ()
    Just resultSetId -> addToDefinitionFile resultSetId
          (tagFile fileId) (tagDefinitions inVs)

-- Range facts. These are range spans, 0-indexed, and may have optional
-- tag/labels indicating what kind of span they are. They may have no text
-- or no tag attached, generally.
--
-- Later event:next facts will associate ranges with defs/decl/ref/ other facts
--
factToAngle (KeyFact n (SymbolRange range mtag)) = do
  -- record the type if we have the tag handy
  forM_ {- Maybe -} (tagToTy =<< mtag) (insertType n)

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
factToAngle (KeyFact n DefinitionResult) = [] <$ insertType n ResultDefinitionType
factToAngle (KeyFact n DeclarationResult) = [] <$ insertType n ResultDeclarationType
factToAngle (KeyFact n ReferenceResult) = [] <$ insertType n ResultReferenceType
factToAngle (KeyFact n ResultSet) = [] <$ insertType n ResultSetType

-- emit FileDefinition, FileDeclaration and FileReference facts
factToAngle (KeyFact _ (Contains fileId inVs)) = do

  -- elaborate all ids with their types
  tys <- V.mapM (\i -> (,i) <$> getTypeOf i) inVs

  -- partition ids by type
  let defRangeIds = V.map snd $ V.filter ((== Just DefinitionType) . fst) tys
      declRangeIds = V.map snd $ V.filter ((== Just DeclarationType) . fst) tys
      refRangeIds = V.map snd $ V.filter ((== Just ReferenceType) . fst) tys

  let defFacts = emitDefinitions fileId defRangeIds
      declFacts = emitDeclarations fileId declRangeIds

  xrefFacts <- emitReferences fileId refRangeIds
  hoverFacts <- emitHovers fileId defRangeIds

  pure (defFacts <> declFacts <> xrefFacts <> hoverFacts)

factToAngle _ = pure []

emitHovers :: Id -> V.Vector Id -> State Env [Value]
emitHovers fileId ids = do
  hoverBodies <- V.catMaybes <$> V.mapM (generateHoverFacts fileId) ids
  if V.null hoverBodies
    then pure []
    else pure [ object [
        "predicate" .= text "lsif.DefinitionHover",
        "facts" .= Array hoverBodies
      ] ]

emitReferences :: Id -> V.Vector Id -> State Env [Value]
emitReferences fileId ids = do
  xrefBodies <- concat. V.toList <$> V.mapM (generateFileReferences fileId) ids
  if null xrefBodies
    then pure []
    else pure [ object [
            "predicate" .= text "lsif.Reference.1",
            "facts" .= Array (V.fromList $
              map (object . pure . key) xrefBodies
            )
          ] ]

-- For each refId, look up the result set, find the result sets file and defs
-- and generate a single flat file -> ref -> targetDef fat
generateFileReferences :: Id -> Id -> State Env [[Pair]]
generateFileReferences fileId refRangeId =
  withResultSet refRangeId getDefinitionFile $ \case
    FileDefs targetFileId targetRanges -> pure
      [ [ "file" .= fileId
        , "range" .= refRangeId
        , "target" .= (object . pure . key)
            [ "file" .= targetFileId
            , "range" .= targetRange
            ]
        ]
      | targetRange <- V.toList targetRanges
      ]

emitDefinitions :: Id_ FileTy -> V.Vector (Id_ DefinitionTy) -> [Value]
emitDefinitions = emitDeclDefs "lsif.Definition.1"

emitDeclarations :: Id_ FileTy -> V.Vector Id -> [Value]
emitDeclarations = emitDeclDefs "lsif.Declaration.1"

emitDeclDefs :: Text -> Id_ FileTy -> V.Vector (Id_ DefinitionTy) -> [Value]
emitDeclDefs name fileId ids
  | V.null ids = []
  | otherwise = pure $ object [
    "predicate" .= text name,
    "facts" .= Array (V.map (\rangeId -> (object . pure . key)
                  [ "file" .= fileId
                  , "range" .= rangeId
                  ]
                ) ids)
    ]

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

predicate :: Applicative f => Text -> [Pair] -> f [Value]
predicate name facts = pure $ pure $
  object [
    "predicate" .= text name,
    "facts" .= [
      object [ key facts ]
    ]
  ]

predicateId :: Applicative f => Text -> Id -> [Pair] -> f [Value]
predicateId name id_ facts = pure $ pure $
  object [
    "predicate" .= text name,
    "facts" .= [
      object [ factId id_, key facts ]
    ]
  ]

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

tagToTy :: Tag -> Maybe VertexType
tagToTy Definition{} = Just DefinitionType
tagToTy Declaration{} = Just DeclarationType
tagToTy Reference{} = Just ReferenceType
tagToTy _ = Just DefinitionType -- yolo

tagToText :: KeyValue a => Tag -> Maybe [a]
tagToText Definition{..} = Just ["text" .= string tagText]
tagToText Declaration{..} = Just ["text" .= string tagText]
tagToText Reference{..} = Just ["text" .= string tagText]
tagToText _ = Nothing

tagToRange :: KeyValue a => Tag -> Maybe [a]
tagToRange Definition{..} = Just ["fullRange" .= toRange fullRange]
tagToRange Declaration{..} = Just ["fullRange" .= toRange fullRange]
tagToRange _ = Nothing
