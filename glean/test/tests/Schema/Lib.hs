{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Schema.Lib
  ( withSchemaAndFacts
  , withSchema
  , withSchemaFile
  , angleQuery
  , mkBatch
  , mkAngleQuery
  ) where

import Data.ByteString (ByteString)
import Control.Exception
import Control.Monad
import Data.Default
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import System.FilePath
import System.IO.Temp

import Util.Control.Exception

import Glean.Angle.Types (latestAngleVersion)
import Glean.Backend
import Glean.Database.Open
import Glean.Database.Config
import Glean.Database.Test
import Glean.Database.Types
import Glean.Database.Schema.Types
import Glean.Database.Schema
import Glean.Types as Thrift
import Glean.Write.JSON

angleQuery :: Env -> Repo -> ByteString -> IO UserQueryResults
angleQuery env repo q = userQuery env repo $ mkAngleQuery q

mkBatch :: PredicateRef -> [ByteString] -> JsonFactBatch
mkBatch ref facts =
  JsonFactBatch
    { jsonFactBatch_predicate = ref
    , jsonFactBatch_facts = facts
    , jsonFactBatch_unit = Nothing
    }

mkAngleQuery :: ByteString -> UserQuery
mkAngleQuery q = def
  { userQuery_query = q
  , userQuery_options = Just def
    { userQueryOptions_syntax = QuerySyntax_ANGLE
    }
  }

withSchemaAndFacts
  :: [Setting]
  -> String                    -- ^ schema
  -> [JsonFactBatch]           -- ^ db contents
  -> Text                    -- ^ initial query
  -> ( DbSchema
    -> Either BadQuery UserQueryResults                  -- query response
    -> (Text -> IO (Either BadQuery UserQueryResults)) -- run more queries
    -> IO a )
  -> IO a
withSchemaAndFacts customSettings schema facts query act =
  withSchemaFile latestAngleVersion schema $ \root file -> do
  let settings =
        [ setRoot root
        , setSchemaPath file
        ] ++ customSettings

  -- create db and write facts
  repo <- withEmptyTestDB settings $ \env repo -> do
      void $ syncWriteJsonBatch env repo facts Nothing
      completeTestDB env repo
      return repo

  -- get PredicateDetails
  dbSchema <- do
    schema <- either error return
      $ processOneSchema Map.empty $ encodeUtf8 $ pack schema
    newDbSchema schema LatestSchemaAll readWriteContent

  let run q = do
        -- open db for querying
        -- We need to open the db again because schema evolutions are
        -- only triggered when the db is read-only
        response <- withTestEnv settings $ \env ->
          try $ runQuery env repo (encodeUtf8 q)
        print (response :: Either BadQuery UserQueryResults)
        return response

  res <- run query
  act dbSchema res run
  where
    runQuery env repo q = userQuery env repo $ def
      { userQuery_query = q
      , userQuery_options = Just def
        { userQueryOptions_syntax = QuerySyntax_ANGLE
        , userQueryOptions_recursive = True
        , userQueryOptions_collect_facts_searched = True
        , userQueryOptions_debug = def
          { queryDebugOptions_bytecode = False
          , queryDebugOptions_ir = False
          }
        }
      , userQuery_encodings = [ UserQueryEncoding_bin def ]
      }

withSchemaFile :: Int -> String -> (FilePath -> FilePath -> IO a) -> IO a
withSchemaFile version str action = do
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let newSchemaFile = root </> "schema"
    appendFile newSchemaFile $ "version: " <> show version
    appendFile newSchemaFile str
    action root newSchemaFile

withSchema :: Int -> String -> (Either SomeException () -> IO a) -> IO a
withSchema version str action =
  withSchemaFile version str $ \root file -> do
    let settings =
          [ setRoot root
          , setSchemaPath file
          ]
    r <- tryAll $
      withEmptyTestDB settings $ \env repo ->
      withOpenDatabase env repo $ \_ ->
        return ()

    print (r :: Either SomeException ())
    action r
