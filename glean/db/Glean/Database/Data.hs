{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Data
  ( storeSchema
  , retrieveSchema
  , storeUnits
  , retrieveUnits
  , storeSlices
  , retrieveSlices
  ) where

import Data.Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)

import Thrift.Protocol.Compact

import Glean.Database.Exception
import Glean.Database.Storage (Storage, Database)
import qualified Glean.Database.Storage as Storage
import Glean.RTS.Foreign.Ownership
import Glean.Types (Repo)
import Glean.Internal.Types (StoredSchema)

sCHEMA_KEY :: ByteString
sCHEMA_KEY = "schema"

-- | Stores the units that are excluded (or included) from the base DB
uNITS_KEY :: ByteString
uNITS_KEY = "units"

-- | Stores the slices for the base DBs in a stack
sLICES_KEY :: ByteString
sLICES_KEY = "slices"

storeSchema :: Storage s => Database s -> StoredSchema -> IO ()
storeSchema db = Storage.store db sCHEMA_KEY . serializeCompact

retrieveSchema :: Storage s => Repo -> Database s -> IO (Maybe StoredSchema)
retrieveSchema repo db = do
  value <- Storage.retrieve db sCHEMA_KEY
  case deserializeCompact <$> value of
    Just (Right info) -> return $ Just info
    Just (Left msg) -> dbError repo $ "invalid schema: " ++ msg
    Nothing -> return Nothing

storeUnits :: Storage s => Database s -> [ByteString] -> IO ()
storeUnits db = Storage.store db uNITS_KEY . toStrict . encode

retrieveUnits :: Storage s => Repo -> Database s -> IO (Maybe [ByteString])
retrieveUnits repo db = do
  value <- Storage.retrieve db uNITS_KEY
  case decodeOrFail . fromStrict <$> value of
    Just (Right (_, _, units)) -> return $ Just units
    Just (Left (_, _, msg)) -> dbError repo $ "invalid units: " ++ msg
    Nothing -> return Nothing

-- Slices are each serialized using the RTS Slice::serialize(), and then
-- the list of serialized slices :: [ByteString] is serialized with
-- the Haskell Binary encoder.
storeSlices :: Storage s => Database s -> [Slice] -> IO ()
storeSlices db slices = do
  bytestrings <- mapM serializeSlice slices
  Storage.store db sLICES_KEY $ toStrict $ encode bytestrings

retrieveSlices :: Storage s => Repo -> Database s -> IO (Maybe [Slice])
retrieveSlices repo db = do
  value <- Storage.retrieve db sLICES_KEY
  case decodeOrFail . fromStrict <$> value of
    Just (Right (_, _, bytestrings)) ->
      Just <$> mapM deserializeSlice bytestrings
    Just (Left (_, _, msg)) -> dbError repo $ "invalid slices: " ++ msg
    Nothing -> return Nothing
