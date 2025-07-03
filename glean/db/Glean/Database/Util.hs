{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Util
  (getDbSchemaFromId
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)

import Glean.Database.Config (DebugFlags, SchemaIndex, schemaForSchemaId)
import Glean.Database.Schema (DbSchema, DbContent,
  readWriteContent, newDbSchema)
import Glean.Database.Schema.Types (SchemaSelector(..))
import Glean.Types (SchemaId(..))
import qualified Data.Text as Text


-- | Get a DbSchema instance from a SchemaId
dbSchemaFromId :: MonadIO m
               => SchemaIndex          -- ^ Schema index to search in
               -> SchemaId             -- ^ Schema ID to look up
               -> DbContent            -- ^ Database content information
               -> DebugFlags           -- ^ Debug flags
               -> m DbSchema
dbSchemaFromId index schemaId dbContent debug = liftIO $ do
  case schemaForSchemaId index schemaId of
    Nothing -> throwIO $ userError $ "Schema ID not found: " ++
      Text.unpack (unSchemaId schemaId)
    Just _ -> do
      -- Create a new DbSchema from the processed schema
      newDbSchema Nothing index (SpecificSchemaId schemaId) dbContent debug

-- | Get a DbSchema instance from a SchemaId using default settings
getDbSchemaFromId :: MonadIO m
                  => Maybe SchemaIndex  -- ^ Schema index to search in
                  -> SchemaId           -- ^ Schema ID to look up
                  -> m DbSchema
getDbSchemaFromId Nothing _ =
  liftIO $ throwIO $ userError "No schema index provided"
getDbSchemaFromId (Just index) schemaId =
  dbSchemaFromId index schemaId readWriteContent def
