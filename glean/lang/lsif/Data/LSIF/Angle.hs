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
import qualified Data.IntMap.Strict as IMap
import Data.IntSet ( IntSet )
import qualified Data.IntSet as Set
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Vector as V

import Data.LSIF.Types
import Data.LSIF.JSON ({- instances -})

-- Converts from AESON-parsed standard LSIF into Glean-compatible types

-- | Parser env, to track command statements in LSIF that change scope
-- So much bookeeping to recover the types
data Env =
  Env {
    root :: !Text, -- ^ used to clean up file paths to be repo-relative
    openProjects :: IntSet, -- open project identifiers
    openDocuments :: IntSet, -- stack of open document identifiers

    -- track some of the types associated with ids
    idTypeMap :: IntMap VertexType,

    -- resultset id -> defnition id
    resultSetToDefMap :: IntMap Id,

    -- resultset id -> fileid of definition range
    resultSetToFileMap :: IntMap Id,

    -- each range (ref or def or decl) is a `next` edge member of a result set
    rangeToResultSetMap :: IntMap Id,

    -- when we see an textDocument/definition edge, record its resultset
    definitionResultToResultSetMap :: IntMap Id

  }

-- Track the implied type of some of the graph nodes identifiers
data VertexType
  = DefinitionType
  | DeclarationType
  | ReferenceType
  | ResultSetType
  deriving (Eq, Ord, Bounded, Enum)

-- Build up some lookup tables for the assoc lists between refs/defs/decls/files
-- to aid in generaing flatter angle facts
empty :: Env
empty = Env "" Set.empty Set.empty
  IMap.empty IMap.empty IMap.empty IMap.empty IMap.empty

-- Drop projectRoot prefix from URI to yield repo-relative paths for Glean
filterRoot :: Text -> State Env Text
filterRoot path = do
  prefix <- root <$> get
  pure $ fromMaybe path (Text.stripPrefix (prefix <> "/") path)

setRoot :: Text -> State Env ()
setRoot path = modify' (\e -> e { root = path })

insertProject, insertDocument :: Id -> State Env ()
insertProject (Id n) = modify' $ \e ->
  e { openProjects = Set.insert (fromIntegral n) (openProjects e) }
insertDocument (Id n) = modify' $ \e ->
  e { openDocuments = Set.insert (fromIntegral n) (openDocuments e) }

insertType :: Id -> VertexType -> State Env ()
insertType (Id n) ty = modify' $ \e -> e { idTypeMap =
    IMap.insert (fromIntegral n) ty (idTypeMap e)
  }

resultSetToDef :: Id -> Id -> State Env ()
resultSetToDef (Id n) defId = modify' $ \e -> e { resultSetToDefMap =
    IMap.insert (fromIntegral n) defId (resultSetToDefMap e)
  }

insertInResultSet :: Id -> Id -> State Env ()
insertInResultSet (Id n) resultSetId  = modify' $ \e ->
  e { rangeToResultSetMap =
    IMap.insert (fromIntegral n) resultSetId (rangeToResultSetMap e)
  }

insertDefinitionResult :: Id -> Id -> State Env ()
insertDefinitionResult (Id n) resultMapId = modify' $ \e ->
  e { definitionResultToResultSetMap =
    IMap.insert (fromIntegral n) resultMapId (definitionResultToResultSetMap e)
  }

insertResultSetFile :: Id -> Id -> State Env ()
insertResultSetFile (Id n) fileId = modify' $ \e ->
  e { resultSetToFileMap =
    IMap.insert (fromIntegral n) fileId (resultSetToFileMap e)
  }

getResultSetOfDefinitionResult :: Id -> State Env (Maybe Id)
getResultSetOfDefinitionResult = lookupIMapEnv definitionResultToResultSetMap

getTypeOf :: Id -> State Env (Maybe VertexType)
getTypeOf = lookupIMapEnv idTypeMap

lookupIMapEnv :: (Env -> IntMap a) -> Id -> State Env (Maybe a)
lookupIMapEnv f (Id n) = do
  imap <- f <$> get
  pure (IMap.lookup (fromIntegral n) imap)

lookupIn :: t -> (t -> IntMap a) -> Id -> Maybe a
lookupIn env f (Id n) = IMap.lookup (fromIntegral n) (f env)

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
toAngle :: LSIF -> Value
toAngle (LSIF facts) = Array $ V.concatMap V.fromList $
  evalState (V.mapM factToAngle facts) empty

-- | Process each key/fact pair
factToAngle :: KeyFact -> State Env [Value]
factToAngle (KeyFact _ MetaData{..}) = do
  setRoot projectRoot
  predicate "lsif.Metadata.1" ([
    "lsifVersion" .= version,
    "projectRoot" .= string projectRoot, -- src.File
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

-- LSIF Documents are uri/language pairs.
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
factToAngle (KeyFact _ (Event Begin scope n)) = [] <$ case scope of
  ProjectScope -> insertProject n
  DocumentScope -> insertDocument n -- should be src.File id of previous $event

-- Declares a resultset, which is used to group definitions, uses, symbol ids
-- (monikers), projects, hovertext and more
factToAngle (KeyFact n ResultSet) = [] <$ insertType n ResultSetType

-- Associate a range fact with a result set.
-- We use this to track chains of ref -> resultset -> def for later generation
factToAngle (KeyFact _ (Edge EdgeNext outV inV)) = [] <$ do
  outFactType <- getTypeOf outV
  inFactType <- getTypeOf inV
  case (outFactType, inFactType ) of
    (Just DefinitionType, Just ResultSetType) -> do
        outV `insertInResultSet` inV
        resultSetToDef inV outV -- record inverse map as well
    (Just ReferenceType, Just ResultSetType) ->
        outV `insertInResultSet` inV
    _ -> return ()

-- Range facts. These are range spans, 0-indexed, and may have optional
-- tag/labels indicating what kind of span they are. They may have no text
-- or no tag attached, generally.
--
-- Later event:next facts will associate ranges with defs/decl/ref/ other facts
--
factToAngle (KeyFact n (SymbolRange range mtag)) = do
  -- record the type if we have the tag handy
  case mtag of
    Just Definition{} -> insertType n DefinitionType
    Just Declaration{} -> insertType n DeclarationType
    Just Reference{} -> insertType n ReferenceType
    _ -> return ()

  -- emit a range fact for this id
  predicateId "lsif.Range.1" n $
    [ "range" .= toRange range
    ] ++ mFullRange ++ mText
    where
      mFullRange = case mtag of
        Just Definition{..} -> ["fullRange" .= toRange fullRange]
        Just Declaration{..} -> ["fullRange" .= toRange fullRange]
        _ -> []

      mText = case mtag of
        Just Definition{..} -> ["text" .= string tagText]
        Just Declaration{..} -> ["text" .= string tagText]
        Just Reference{..} -> ["text" .= string tagText]
        _ -> []

-- These declare some types
factToAngle (KeyFact n DefinitionResult) = [] <$
  insertType n DefinitionType
factToAngle (KeyFact n DeclarationResult) = [] <$
  insertType n DeclarationType
factToAngle (KeyFact n ReferenceResult) = [] <$
  insertType n ReferenceType

-- if we see a text/Document/Definition edge, track the resultmap it points to
-- we need this later to look up files -> to their definition resultmaps
factToAngle (KeyFact _ (Edge EdgeTextDocumentDefinition outV inV)) = do
  insertDefinitionResult inV outV
  pure []

-- Item edges link ranges to semantic elements like definitions and references
-- They are tied to documents.
factToAngle (KeyFact _ (Item outV inVs fileId prop)) = do
  ty <- getTypeOf outV
  case ty of
    Just DefinitionType -> do
      -- we can now map fileIds to their definition result sets
      mResultSet <- getResultSetOfDefinitionResult outV
      case mResultSet of
        -- record the definition resultSet points to its fileId
        Just resultSetId -> insertResultSetFile resultSetId fileId
        _ -> return ()

      -- and emit the file definition fact
      predicate "lsif.FileDefinitions.1"
              [ "file" .= fileId
              , "range" .= inVs -- vector
              ]

    Just DeclarationType ->
      predicate "lsif.FileDeclarations.1"
              [ "file" .= fileId
              , "range" .= inVs -- vector
              ]

    -- this is an item set of references in this file
    -- we should be able to emit the target def if it has already been
    -- seen, for each ref id
    Just ReferenceType | prop == Just References -> do
      env <- get
      let xrefFacts :: [[Pair]]
          xrefFacts =
            [ [ "file" .= fileId
              , "range" .= inV
              , "target" .= object
                    [ "file" .= targetFileId
                    , "range" .= targetDefId
                    ]
              ]
            | inV <- V.toList inVs
            , Just resultSetId <-
                pure (lookupIn env rangeToResultSetMap inV)
            , Just targetFileId <-
                pure (lookupIn env resultSetToFileMap resultSetId)
            , Just targetDefId <-
                pure (lookupIn env resultSetToDefMap resultSetId)
            ]
      if null xrefFacts
        then pure []
        else pure [
            object [
              "predicate" .= text "lsif.FileReferences.1",
              "facts" .= [ object (map key xrefFacts) ]
            ]
        ]

    _ -> pure [] -- warning? a definition for something we haven't seen declared

factToAngle _ = pure []

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
