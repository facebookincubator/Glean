{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Helpers for generating lsif angle json. Shared between the scip and lsif 
converters

-}

module Data.LSIF.Gen (

    Predicate(..),
    PredicateMap,
    predicate,
    predicateId,
    key,
    string,
    generateJSON,
    insertPredicateMap,
    factId,

    -- * more shared types
    MonikerKind(..),
    Id(..),
    Position(..),
    Range(..),
    toRange

  ) where

import Data.Aeson
import Data.Maybe ( mapMaybe )
import Data.Int ( Int64 )
import Data.Aeson.Types ( Pair )
import GHC.Generics ( Generic )
import Data.HashMap.Strict ( HashMap )
import Data.List ( foldl', sortOn )
import Data.Text ( Text )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import Data.List.Split ( chunksOf )

-- A predicate "block", containing multiple facts
data Predicate = Predicate !Text [Value]
  deriving Show

-- | Tracking output by predicate
type PredicateMap = HashMap Text [Value]

-- | An Id to identify a vertex or an edge.
newtype Id = Id Int64
  deriving (Generic, Eq)

-- keep instances handy
instance FromJSON Id
instance ToJSON Id where
  toEncoding = genericToEncoding defaultOptions

predicate :: Applicative f => Text -> [Pair] -> f [Predicate]
predicate name facts = pure [Predicate name [object [key facts]]]

predicateId :: Applicative f => Text -> Id -> [Pair] -> f [Predicate]
predicateId name id_ facts =
  pure [Predicate name [object [factId id_, key facts ]]]

key :: KeyValue kv => [Pair] -> kv
key xs = "key" .= object xs

string :: Text -> Value
string s = object [ "key" .= s ]

factId :: KeyValue kv => Id -> kv
factId (Id id_) = "id" .= id_

-- | Accumulate predicates
insertPredicateMap :: PredicateMap -> [Predicate] -> PredicateMap
insertPredicateMap = foldl' ins
  where
    ins hm (Predicate name vs) = HashMap.insertWith (++) name vs hm

-- | Given a hashmap keyed by lsif predicate names, emit an array of json
-- pred/facts with one entry per predicate. In case we have very large
-- predicats, we chunk them into smaller top level groups, which makes memory
-- mgmt a bit easier
generateJSON :: PredicateMap -> [Value]
generateJSON hm = concat $ mapMaybe (\k -> gen k <$> HashMap.lookup k hm) keys
  where
    gen k = emitPredicate . Predicate k
    keys = sortOn dependencyOrder (HashMap.keys hm)

emitPredicate :: Predicate -> [Value]
emitPredicate (Predicate name facts) =
  [ object
    [ "predicate" .= text name
    , "facts" .= Array (V.fromList chunk)
    ]
  | chunk <- chunksOf 10000 facts
  ]
  where
    text :: Text -> Value
    text = String

dependencyOrder :: Text -> Int
dependencyOrder p = case p of
  "lsif.Document.1" -> 0
  "lsif.Project" -> 0
  "lsif.Range.1" -> 1
  "lsif.HoverContent" -> 2
  "lsif.Moniker" -> 3
  -- these refer to Range.1
  "lsif.Definition.1" -> 10
  "lsif.Declaration.1" -> 11
  -- these refer to Declaration.1, Range.1
  "lsif.Reference.1" -> 12
  "lsif.DefinitionHover.1" ->  13
  "lsif.DefinitionUse.1" -> 14
  "lsif.ProjectDocument.1" -> 15
  "lsif.DefinitionMoniker.1" ->  16
  "lsif.DefinitionKind.1" ->  17
  -- everything else, in the middle
  _ -> 5


data MonikerKind = Export | Local | Import | Implementation
  deriving (Enum)

instance FromJSON MonikerKind where
  parseJSON (String str) = case str of
    "export" -> pure Export
    "local" -> pure Local
    "import" -> pure Import
    "implementation" -> pure Implementation
    s -> fail ("FromJSON.MonikerKind: unknown kind: " <> show s)
  parseJSON s = fail ("FromJSON.MonikerKind: unknown kind: " <> show s)

-- | A 0-indexed line/character-offset point in a document
data Position = Position {
      line :: {-# UNPACK #-} !Int64,
      character :: {-# UNPACK #-} !Int64
    }
  deriving (Generic)

instance FromJSON  Position where

data Range = Range {
      start :: {-# UNPACK #-} !Position,
      end :: {-# UNPACK #-} !Position
    }
  deriving (Generic)

instance FromJSON Range where

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
