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

module Data.LSIF.Angle (
    factToAngle, Predicate, PredicateMap,
    generateJSON,
    insertPredicateMap,
    emitFileFactSets,
    -- parse state
    Env(..), emptyEnv
  ) where

import Control.Monad.Extra ( concatMapM )
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Types ( Pair )
import Data.List
import Data.List.Split
import Data.Maybe ( catMaybes, fromMaybe, listToMaybe, mapMaybe )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IMap
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.LSIF.Types
import Data.LSIF.Moniker
import Data.LSIF.Env
import Data.LSIF.JSON ({- instances -})

-- | Given a hashmap keyed by predicate names, emit an array of json pred/facts
-- with one entry per predicate. In case we have very large predicats, we chunk
-- them into smaller top level groups, which makes memory mgmt a bit easier
generateJSON :: HashMap Text [Value] -> [Value]
generateJSON hm = concat $ mapMaybe (\k -> gen k <$> HashMap.lookup k hm) keys
  where
    gen k = emitPredicate . Predicate k
    keys = sortOn dependencyOrder (HashMap.keys hm)

-- | Try to be slightly robust about which version of the lsif facts we generate
lsifSchemaVersion :: Int
lsifSchemaVersion = 2

emitPredicate :: Predicate -> [Value]
emitPredicate (Predicate name facts) =
  [ object
    [ "predicate" .= text (name <> "." <> Text.pack (show lsifSchemaVersion))
    , "facts" .= Array (V.fromList chunk)
    ]
  | chunk <- chunksOf 10000 facts
  ]

dependencyOrder :: Text -> Int
dependencyOrder p = case p of
  "lsif.Document" -> 0
  "lsif.Project" -> 0
  "lsif.Range" -> 1
  "lsif.HoverContent" -> 2
  "lsif.Moniker" -> 3
  -- these refer to Range
  "lsif.Definition" -> 10
  "lsif.Declaration" -> 11
  -- these refer to Declaration, Range
  "lsif.Reference" -> 12
  "lsif.DefinitionHover" ->  13
  "lsif.DefinitionUse" -> 14
  "lsif.ProjectDocument" -> 15
  "lsif.DefinitionMoniker" ->  16
  "lsif.DefinitionKind" ->  17
  -- everything else, in the middle
  _ -> 5

-- Drop projectRoot prefix from URI to yield repo-relative paths for Glean
-- Some indexers generate uris outside of the repo. In those cases we
-- just try to remove local environment specific-stuff
filterRoot :: Text -> Parse Text
filterRoot path = do
  prefixes <- root <$> get -- try a few paths
  let clean = mapMaybe (\p -> Text.stripPrefix (p <> "/") path) prefixes
  pure (fromMaybe path (listToMaybe clean))

--
-- | Convert LSIF json to Glean json
--
-- Process each key/fact pair
--
-- We map over the statements, in order, generating 0 or more facts for Glean
-- LSIF guarantees things are defined before references to them are used, and
-- uses unique ids for facts. We can use that to simplify processing
--
-- vertexes correspond to facts holding new data
-- edges relate existnig vertex facts to each other (with new facts)
--
factToAngle :: KeyFact -> Parse [Predicate]

factToAngle (KeyFact _ MetaData{..}) = do
  appendRoot projectRoot
  predicate "lsif.Metadata" ([
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
  predicateId "lsif.Project" n [ "kind" .= fromEnum kind ]

factToAngle (KeyFact _ PackageInformation{..}) =
  predicate "lsif.PackageInformation" [
    "name" .= name,
    "manager" .= manager,
    "version" .= version
  ]

-- LSIF Documents are uri/language pairs
-- Rather than key on src.File, we use lsif.Document to keep the language of the
-- symbol accessible, for mixed-language lsif dbs.
--
factToAngle (KeyFact n Document{..}) = do
  insertType n FileType
  path <- filterRoot uri
  predicateId "lsif.Document" n
    [ "file" .= string path, -- n.b. anonymous src.File fact
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

-- record which resultset this moniker points at
factToAngle (KeyFact _ (Edge EdgeMoniker outV inV)) = [] <$
  addMonikerToResultSet inV outV

-- edge:item for property:references associates an inV range with a
-- textDocument/references result. record that the range is a reference type
-- n.b. the spec doesn't guarantee property:references will be set.
factToAngle (KeyFact _ (Item _outV inVs _fileId (Just References))) = [] <$
  insertTypes inVs ReferenceType

-- items : these add ranges to definition results
-- we check the result set of this definitionResult, then record
-- that the result set points at this definition ranges / document pair
--
-- other ranges may point at the same result set, but they will (implicitly)
-- be reference ranges.
--
factToAngle (KeyFact _ (Item outV inVs{-ranges-} fileId Nothing)) = [] <$ do

  -- if the item range inV points to a definitionResult, it must be a definition
  -- if the item range inV points to a referenceResult, it must be a reference
  mTy <- getTypeOf outV
  case mTy of
    Just DefinitionType -> insertTypes inVs DefinitionType
    Just ReferenceType -> insertTypes inVs ReferenceType
    Just DeclarationType -> insertTypes inVs DeclarationType
    _ -> pure ()

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
-- tag/labels indicating what kind of span they are
factToAngle (KeyFact n (SymbolRange range mtag)) = do
  -- record the type of the range if we have the tag handy
  -- it's convenient , but not required in the specification
  case  mtag of
    Just Definition{} -> insertType n DefinitionType
    Just Declaration{} -> insertType n DeclarationType
    Just Reference{} -> insertType n ReferenceType
    _ -> pure ()

  -- record the symbol kind if it exists (definitions only)
  case tagToKind =<< mtag of
    Just kind -> insertSymbolKind n kind
    Nothing -> pure ()

  -- emit a range fact for this id
  predicateId "lsif.Range" n $
    [ "range" .= toRange range
    , "text" .= maybe (string "" {- better to use nothing? -}) toName mtag
    ] ++ mFullRange
    where
      mFullRange = fromMaybe [] (tagToRange =<< mtag)

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

-- Moniker payloads
factToAngle (KeyFact n (Moniker kind scheme ident)) =
  case processMoniker kind scheme ident of
    Nothing ->
      predicateId "lsif.Moniker" n
        [ "kind" .= fromEnum kind
        , "scheme" .= string scheme
        , "ident" .= string ident
        ]
    Just (ident', kindLiteral) -> do
      a <- predicateId "lsif.Moniker" n
        [ "kind" .= fromEnum kind
        , "scheme" .= string scheme
        , "ident" .= string ident'
        ]
      b <- predicate "lsif.MonikerSymbolKind"
        [ "moniker" .= n
        , "kind" .= fromEnum kindLiteral
        ]
      pure (a <> b)

-- These are output nodes. We generally need to track the type of the id,
-- as untyped item edges will piont at these, and it may be the only way to
-- find the type of the underlying range
factToAngle (KeyFact n DefinitionResult) = [] <$
  insertType n DefinitionType
factToAngle (KeyFact n DeclarationResult) = [] <$
  insertType n DeclarationType
factToAngle (KeyFact n ReferenceResult) = [] <$
  insertType n ReferenceType

factToAngle (KeyFact _ ResultSet) = pure []

-- record the range ids that are contained in a file id
-- or from file id to project id.
factToAngle (KeyFact _ (Contains fileId inVs)) = [] <$
  addFileContainsIds (tagFile fileId) inVs

factToAngle _ = pure []

--
-- After consuming the LSIF graph, we should have the full set of
-- {def,decl,ref} -> resutlset -> definitionResult data
--
emitFileFactSets :: Parse [Predicate]
emitFileFactSets = do
  fileSet <- IMap.toList . fileContains <$> get
  ps <- mapM (\(k,v) ->
          emitFileFacts (tagFile (Id $ fromIntegral k)) v)
          fileSet
  pure (concat ps)

data IdSet =
    IdSet {
      defIds :: !IdVector,
      declIds :: !IdVector,
      refIds :: !IdVector,
      fileIds :: !IdVector
    }

-- Lookup the type of each id as we recorded it in the env
partitionByType :: IdVector -> Parse IdSet
partitionByType ids = do
  -- get types of all identifiers
  tys <- V.generateM (U.length ids)
           (\i -> getTypeOf (Id (ids `U.unsafeIndex` i)))

  -- partition by type, we don't care about ordering
  let defIds = U.ifilter
        (\i _ -> tys `V.unsafeIndex` i == Just DefinitionType) ids
      declIds = U.ifilter
        (\i _ -> tys `V.unsafeIndex` i == Just DeclarationType) ids
      refIds = U.ifilter
        (\i _ -> tys `V.unsafeIndex` i == Just ReferenceType) ids
      fileIds = U.ifilter
        (\i _ -> tys `V.unsafeIndex` i == Just FileType) ids

  return $ IdSet defIds declIds refIds fileIds

emitFileFacts :: Id_ FileTy -> [IdVector] -> Parse [Predicate]
emitFileFacts fileId = concatMapM (emitFileFacts_ fileId)

-- We delay this until the post-processing phase to ensure all
-- - item, textDocument/definition, etc.. nodes are added
emitFileFacts_ :: Id_ FileTy -> IdVector -> Parse [Predicate]
emitFileFacts_ fileId rawIds = do
  IdSet{..} <- partitionByType rawIds

  let defFacts = emitDefinitions fileId defIds
      declFacts = emitDeclarations fileId declIds
      projectFacts = emitProjects fileId fileIds {- from projectId to files -}
  xrefFacts <- emitReferences fileId refIds
  useFacts <- emitTargetUses fileId refIds
  hoverFacts <- emitHovers fileId defIds
  monikerFacts <- emitMonikers fileId defIds
  symbolKindFacts <- emitSymbolKinds fileId defIds
  return $ catMaybes
    [defFacts, declFacts, xrefFacts, useFacts,
     hoverFacts, projectFacts, monikerFacts, symbolKindFacts]

emitProjects :: Id -> IdVector -> Maybe Predicate
emitProjects projId ids
  | U.null ids = Nothing
  | otherwise = Just $
      Predicate "lsif.ProjectDocument"
        (map (\rangeId -> (object . pure . key)
          [ "file" .= rangeId
          , "project" .= projId
          ]
        ) (U.toList ids))

emitSymbolKinds :: Id -> IdVector -> Parse (Maybe Predicate)
emitSymbolKinds fileId ids = do
  factBodies <- catMaybes <$>
    mapM (generateSymbolKindFacts fileId . Id) (U.toList ids)
  if null factBodies
    then pure Nothing
    else return $ Just $ Predicate "lsif.DefinitionKind" factBodies

emitMonikers :: Id -> IdVector -> Parse (Maybe Predicate)
emitMonikers fileId ids = do
  factBodies <- catMaybes <$>
    mapM (generateMonikerFacts fileId . Id) (U.toList ids)
  if null factBodies
    then pure Nothing
    else return $ Just $ Predicate "lsif.DefinitionMoniker" factBodies

emitHovers :: Id -> IdVector -> Parse (Maybe Predicate)
emitHovers fileId ids = do
  hoverBodies <- catMaybes <$>
    mapM (generateHoverFacts fileId . Id) (U.toList ids)
  if null hoverBodies
    then pure Nothing
    else return $ Just $ Predicate "lsif.DefinitionHover" hoverBodies

emitReferences :: Id -> IdVector -> Parse (Maybe Predicate)
emitReferences fileId ids = do
  xrefBodies <- concat <$> mapM
    (generateFileReferences fileId . Id) (U.toList ids)
  if null xrefBodies
    then pure Nothing
    else return $ Just $
      Predicate "lsif.Reference" $
        map (object . pure . key) xrefBodies

emitTargetUses :: Id -> IdVector -> Parse (Maybe Predicate)
emitTargetUses fileId ids = do
  useBodies <- concat <$>
    mapM (generateTargetUses fileId . Id) (U.toList ids)
  if null useBodies
    then pure Nothing
    else return $ Just $
      Predicate "lsif.DefinitionUse" $
        map (object . pure . key) useBodies

-- For each refId, look up the result set, find the result sets file and defs
-- and generate a single flat file -> ref -> targetDef fat
generateFileReferences :: Id -> Id -> Parse [[Pair]]
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
      | targetRange <- U.toList targetRanges
      ]

-- inverse of file references, emit them here since they're handy
-- maybe later we want to use textDocument/reference edges
generateTargetUses :: Id -> Id -> Parse [[Pair]]
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
      | targetRange <- U.toList targetRanges
      ]

emitDefinitions :: Id_ FileTy -> IdVector -> Maybe Predicate
emitDefinitions = emitDeclDefs "lsif.Definition"

emitDeclarations :: Id_ FileTy -> IdVector -> Maybe Predicate
emitDeclarations = emitDeclDefs "lsif.Declaration"

emitDeclDefs :: Text -> Id_ FileTy -> IdVector -> Maybe Predicate
emitDeclDefs name fileId ids
  | U.null ids = Nothing
  | otherwise = Just $
      Predicate name
        (map (\rangeId -> (object . pure . key)
          [ "file" .= fileId
          , "range" .= rangeId
          ]
        ) (U.toList ids))

generateHoverFacts :: Id -> Id -> Parse (Maybe Value)
generateHoverFacts fileId defRangeId =
  withResultSet defRangeId getHoverTextId $ \hoverFactId ->
    pure $ pure $ object $ pure $ key
      [ "defn" .= (object . pure . key)
          [ "file" .= fileId
          , "range" .= defRangeId
          ]
      , "hover" .= hoverFactId
      ]

-- Monikers are the symbol ids of LSIF. But they are optional.
-- We want to generate 'nothing' for definitions that are missing
-- monikers, so they will still be useful in entity lookups as keys
generateMonikerFacts :: Id -> Id -> Parse (Maybe Value)
generateMonikerFacts fileId defRangeId = do
  mId <- withResultSet defRangeId getMonikerId (pure . Just)
  pure $ pure $ object $ pure $ key $
    [ "defn" .= (object . pure . key)
        [ "file" .= fileId
        , "range" .= defRangeId
        ]
    ] ++
    (case mId of
        Nothing -> []
        Just monikerId -> [ "moniker" .= monikerId ]
    )

generateSymbolKindFacts :: Id -> Id -> Parse (Maybe Value)
generateSymbolKindFacts fileId defRangeId = do
  mKind <- getSymbolKind defRangeId
  case mKind of
    Nothing -> pure mzero
    Just kindLiteral ->
      pure $ pure $ object $ pure $ key
        [ "defn" .= (object . pure . key)
            [ "file" .= fileId
            , "range" .= defRangeId
            ]
        , "kind" .= fromEnum kindLiteral
        ]

-- get the result set of an id, use that result set id to look up another
-- environment, then apply a function to the result.
-- used to jump from A to B via a [resultset] node
withResultSet :: (MonadPlus m)
  => Id
  -> (Id_ ResultSetTy -> Parse (Maybe t))
  -> (t -> Parse (m a))
  -> Parse (m a)
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

-- LSIF ranges are 0-indexed, exclusive of end col.
-- We want to store as Glean ranges, 1-indexed, inclusive of end col.
toRange :: Range -> Value
toRange Range{..} =
  let colBegin = character start + 1
       -- n.b. end col should be _inclusive_ of end, and >= col start
      colEnd = max colBegin ((character end + 1) - 1)
  in
    object [
      "lineBegin" .= (line start + 1),
      "columnBegin" .= colBegin,
      "lineEnd" .= (line end + 1),
      "columnEnd" .= colEnd
    ]

key :: KeyValue kv => [Pair] -> kv
key xs = "key" .= object xs

factId :: KeyValue kv => Id -> kv
factId (Id id_) = "id" .= id_

string :: Text -> Value
string s = object [ "key" .= s ]

text :: Text -> Value
text = String

-- | Identifier text string
toName :: Tag -> Value
toName = string . tagText

tagToRange :: KeyValue a => Tag -> Maybe [a]
tagToRange Definition{..} = Just ["fullRange" .= toRange fullRange]
tagToRange Declaration{..} = Just ["fullRange" .= toRange fullRange]
tagToRange _ = Nothing

tagToKind :: Tag -> Maybe SymbolKind
tagToKind Definition{..} = Just tagKind
tagToKind Declaration{..} = Just tagKind
tagToKind _ = Nothing
