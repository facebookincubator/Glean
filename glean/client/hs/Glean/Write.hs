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
  , fillDatabase
  , finalize
  , completePredicates
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Extra
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Default
import Data.Either
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import TextShow

import Foreign.CPP.Dynamic (parseJSONWithOptions, JSONOptions(..))
import Util.Control.Exception

import Glean.Backend.Remote hiding (completePredicates)
import qualified Glean.Backend.Remote as Backend
import Glean.Types hiding (Value)
import Glean.Schema.Util

-- | Read a file of JSON fact batches
fileToBatches :: FilePath -> IO [JsonFactBatch]
fileToBatches file = do
  bs <- B.readFile file
  r <- Foreign.CPP.Dynamic.parseJSONWithOptions opts bs
  val <- case r of
    Right val -> return val
    Left err -> throwIO $ ErrorCall $ file ++ ": " ++ Text.unpack err
  case Aeson.parse parseJsonFactBatches val of
    Aeson.Error str -> throwIO $ ErrorCall $ file ++ ": " ++ str
    Aeson.Success x -> return x
  where
    -- folly's default recursion limit is 100, which is not enough for us.
    opts = Foreign.CPP.Dynamic.JSONOptions
      { json_recursionLimit = Just 500 }

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
    <*> v .:? "unit"
  where
    parsePred v = parsePredicate v `mplus` parsePredicateRef v
    parseFacts = withArray "facts" (mapM parseFact . Vector.toList)

parseJsonFactBatches :: Value -> Aeson.Parser [JsonFactBatch]
parseJsonFactBatches = withArray "JsonFactBatch" $ \vec ->
  mapM parseJsonFactBatch (Vector.toList vec)

-- | Create a database and run the supplied IO action to write data
-- into it. When the IO action returns, the DB will be marked complete
-- and cannot be modified further. If the IO action throws an exception,
-- the DB will be marked broken.
fillDatabase
  :: Backend a
  => a
    -- ^ The backend
  -> Maybe Version
    -- ^ The schema version to be used
  -> Repo
    -- ^ The repo to create
  -> Text
    -- ^ Handle for writing. Can be anything you like.
  -> IO ()
    -- ^ What to do if the DB already exists. @return ()@ to continue,
    -- or @throwIO@ to forbid.
  -> IO b
    -- ^ Caller-supplied action to write data into the DB.
  -> IO b
fillDatabase env mversion repo handle ifexists action =
  tryBracket create finish (const action)
  where
  create = do
    r <- kickOffDatabase env def
      { kickOff_repo = repo
      , kickOff_fill = Just $ KickOffFill_writeHandle handle
      , kickOff_properties = HashMap.fromList
          [ ("glean.schema_version", Text.pack $ show version)
          | Just version <- [mversion]
          ]
      }
    when (kickOffResponse_alreadyExists r) ifexists

  finish _ e = do
    workFinished env WorkFinished
      { workFinished_work = def
          { work_repo = repo
          , work_handle = handle
          }
      , workFinished_outcome = case e of
          Left ex -> Outcome_failure (Failure (showt ex))
          Right _ -> Outcome_success def
      }
    when (isRight e) $ finalize env repo

-- | Wait for a database to finish finalizing and enter the "complete"
-- state after all writing has finished. Before the database is
-- complete, it may be queried but a stacked database cannot be
-- created on top of it.
finalize :: Backend a => a -> Repo -> IO ()
finalize env repo =
  void $ untilDone $ finalizeDatabase env repo

-- | Notify the server when non-derived predicates are complete. This
-- must be called before derivedStored.
completePredicates :: Backend a => a -> Repo -> IO ()
completePredicates env repo =
  void $ untilDone $ Backend.completePredicates env repo

untilDone :: IO a -> IO a
untilDone io = loop
  where
  loop = do
    r <- try io
    case r of
      Right a -> return a
      Left (Retry n) -> do
        threadDelay (truncate (n * 1000000))
        loop
