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
  , decodeResults
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.Map (Map)
import Control.Exception
import Control.Monad
import Data.Default
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import System.FilePath
import System.IO.Temp

import Util.Control.Exception

import Glean.Angle.Types (latestAngleVersion)
import Glean.Backend.Types
import Glean.Database.Open
import Glean.Database.Config
import Glean.Database.Test
import Glean.Database.Types
import Glean.Database.Schema.Types
import Glean.Database.Schema
import Glean.Types as Thrift
import Glean.Write.JSON

import qualified Glean.RTS as RTS
import qualified Glean.RTS.Term as RTS
import qualified Glean.RTS.Types as RTS

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

decodeResults
  :: RTS.Type
  -> (UserQueryResultsBin -> Map Id Fact) -- facts/nested
  -> Either BadQuery UserQueryResults
  -> IO (Either String [RTS.Value])
decodeResults ty getFacts eitherRes = runExceptT $ do
  results <- case eitherRes of
    Left err -> fail $ "BadQuery: " <> show err
    Right r -> return r
  bin <- binResults results
  let keys = fmap fact_key $ Map.elems $ getFacts bin
  mapM (decodeAs ty) keys
  where
    binResults :: UserQueryResults -> ExceptT String IO UserQueryResultsBin
    binResults UserQueryResults{..} =
      case userQueryResults_results of
        UserQueryEncodedResults_bin b -> return b
        _ -> fail "wrong encoding"

    decodeAs :: RTS.Type -> ByteString -> ExceptT String IO RTS.Value
    decodeAs ty bs = do
      res <- liftIO $ try $ do
        print bs
        evaluate $ RTS.toValue (withUnknown $ RTS.repType ty) bs
      case res of
        Left e -> fail $ "unable to decode : " <> showException e
        Right val -> return val
      where
        showException (RTS.DecodingException e) = e
        -- we want to decode binary values that contain the unknown alternative
        withUnknown rep = case rep of
          RTS.ByteRep -> rep
          RTS.NatRep -> rep
          RTS.ArrayRep elty -> RTS.ArrayRep $ withUnknown elty
          RTS.TupleRep tys -> RTS.TupleRep $ fmap withUnknown tys
          RTS.SumRep tys ->
            let unknown = RTS.TupleRep [] in
            RTS.SumRep $ fmap withUnknown tys ++ [unknown]
          RTS.StringRep -> rep
          RTS.PredicateRep _ -> rep

-- | Used to test schema transformations
-- Runs the callback on a read-only version of a db with the
-- given schema and facts.
withSchemaAndFacts
  :: [Setting]
  -> String                  -- ^ schema
  -> [JsonFactBatch]         -- ^ db contents
  -> ( Env -> Repo -> DbSchema -> IO a)
  -> IO a
withSchemaAndFacts customSettings schema facts act =
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

  -- open db for querying
  -- We need to open the db again because schema evolutions are
  -- only triggered when the db is read-only
  withTestEnv settings $ \env ->
    act env repo dbSchema

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
