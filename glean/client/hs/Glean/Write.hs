{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Utilities for writing data to Glean
module Glean.Write
  ( fileToBatches
  , parseRef
  , parseJsonFactBatches
  ) where

import Control.Monad.Extra
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector

import Glean.Types hiding (Value)
import Glean.Schema.Util

newtype ParseJsonFactBatches = ParseJsonFactBatches [JsonFactBatch]
instance FromJSON ParseJsonFactBatches where
  parseJSON = fmap ParseJsonFactBatches <$> parseJsonFactBatches

fileToBatches :: FilePath -> IO [JsonFactBatch]
fileToBatches file = do
  res <- eitherDecodeFileStrict' file
  case res of
    Right (ParseJsonFactBatches res) -> return res
    Left err -> error err

parsePredicate :: Value -> Aeson.Parser PredicateRef
parsePredicate = withText "predicate" $ \txt -> do
  let SourceRef pred mbVersion = parseRef txt
  return (PredicateRef pred (fromMaybe 1 mbVersion))

parsePredicateRef :: Value -> Aeson.Parser PredicateRef
parsePredicateRef = withObject "predicate ref" $ \obj ->
  PredicateRef
    <$> obj .: "name"
    <*> obj .: "version"

parseFact :: Value -> Aeson.Parser ByteString
parseFact = withObject "fact" $ \obj -> return (LB.toStrict (encode obj))

parseJsonFactBatch :: Value -> Aeson.Parser JsonFactBatch
parseJsonFactBatch = withObject "JsonFactBatch" $ \v ->
  JsonFactBatch
    <$> Aeson.explicitParseField parsePred v "predicate"
    <*> Aeson.explicitParseField parseFacts v "facts"
    <*> fmap (fmap Text.encodeUtf8) (v .:? "unit")
  where
    parsePred v = parsePredicate v `mplus` parsePredicateRef v
    parseFacts = withArray "facts" (mapM parseFact . Vector.toList)

parseJsonFactBatches :: Value -> Aeson.Parser [JsonFactBatch]
parseJsonFactBatches = withArray "JsonFactBatch" $ \vec ->
  mapM parseJsonFactBatch (Vector.toList vec)
