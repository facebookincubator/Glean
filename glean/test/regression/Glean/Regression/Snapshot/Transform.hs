-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

module Glean.Regression.Snapshot.Transform
  ( Transform(..)
  , Transforms
  , defaultTransforms
  ) where

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.JSON as JSON

data Transform = forall a. Aeson.FromJSON a => Transform
  (a -> [JSON.JSValue] -> [JSON.JSValue])

type Transforms = HashMap Text Transform

defaultTransforms :: Transforms
defaultTransforms = HashMap.fromList
  [ ("sort", Transform sortFactsBy)
  , ("gensort", Transform sortFacts)
  , ("extract", Transform extract)
  , ("normord", Transform normOrd)
  ]

extract :: [Text] -> [JSON.JSValue] -> [JSON.JSValue]
extract path = map (`select` path)

sortFacts :: () -> [JSON.JSValue] -> [JSON.JSValue]
sortFacts _ = sort

sortFactsBy :: [[Text]] -> [JSON.JSValue] -> [JSON.JSValue]
sortFactsBy [] = id
sortFactsBy paths = sortOn (\x -> map (select x) paths)

-- | input JSValue is an object, take the key name from the head of
-- the list, and recursively select to follow the path.
select :: JSON.JSValue -> [Text] -> JSON.JSValue
select = foldl' $ \v sel ->
  case v of
    JSON.JSObject o
      | JSON.Ok field <- JSON.valFromObj (Text.unpack sel) o -> field
      | otherwise -> error $ Text.unpack $ "missing field '" <> sel <> "'"
    _ -> error $ Text.unpack
      $ "can't select '" <> sel <> "' from a non-object"

normOrd :: () -> [JSON.JSValue] -> [JSON.JSValue]
normOrd _ = sort . map norm
  where
    norm j = case j of
      JSON.JSNull -> j
      JSON.JSBool{} -> j
      JSON.JSRational{} -> j
      JSON.JSString{} -> j
      JSON.JSArray v -> JSON.JSArray $ sort $ map norm v
      JSON.JSObject o -> JSON.JSObject $ JSON.toJSObject $
        sort $ map (fmap norm) $ JSON.fromJSObject o
