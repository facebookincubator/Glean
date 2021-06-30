-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Data
  ( storeSchema
  , retrieveSchema
  ) where

import Data.ByteString (ByteString)

import Thrift.Protocol.Compact

import Glean.Database.Exception
import Glean.Database.Storage (Storage, Database)
import qualified Glean.Database.Storage as Storage
import Glean.Types (Repo)
import qualified Glean.Types as Thrift

sCHEMA_KEY :: ByteString
sCHEMA_KEY = "schema"

storeSchema :: Storage s => Database s -> Thrift.SchemaInfo -> IO ()
storeSchema db = Storage.store db sCHEMA_KEY . serializeCompact

retrieveSchema
    :: Storage s => Repo -> Database s -> IO (Maybe Thrift.SchemaInfo)
retrieveSchema repo db = do
  value <- Storage.retrieve db sCHEMA_KEY
  case deserializeCompact <$> value of
    Just (Right info) -> return $ Just info
    Just (Left msg) -> dbError repo $ "invalid schema: " ++ msg
    Nothing -> return Nothing
