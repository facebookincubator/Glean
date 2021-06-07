-- | Utilities for writing data to Glean
module Glean.Write
  ( parseRef
  , parseJsonFactBatches
  , dumpJsonToFile
  , fillDatabase
  ) where

import Control.Monad.Extra
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Default
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as Vector
import System.IO
import Text.Printf
import TextShow

import Util.Control.Exception

import Glean.Backend
import Glean.Types hiding (Value)
import Glean.Dump (dump)
import Glean.Schema.Util
import Glean.Angle.Types

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

-- | Write facts to a file in JSON format suitable for parsing using
-- 'parseJsonFactBatches'.
--
dumpJsonToFile
  :: Backend b
  => b
  -> Repo
  -> FilePath
  -> IO ()
dumpJsonToFile backend repo file =
  withFile file WriteMode $ \hdl -> do
    notFirst <- newIORef False
    hPutStrLn hdl "["
    dump backend repo (withBatch hdl notFirst)
    hPutStrLn hdl "]"
  where
    withBatch hdl notFirst JsonFactBatch{..} = do
      whenM (readIORef notFirst) $ hPutStr hdl ","
      writeIORef notFirst True
      let PredicateRef{..} = jsonFactBatch_predicate
      hPrintf hdl "{ \"predicate\": \"%s.%d\", \"facts\": [\n"
        predicateRef_name predicateRef_version
      BC.hPutStrLn hdl (BC.intercalate ",\n" jsonFactBatch_facts)
      hPutStrLn hdl "]}"

-- | Create a database and run the supplied IO action to write data
-- into it. When the IO action returns, the DB will be marked complete
-- and cannot be modified further. If the IO action throws an exception,
-- the DB will be marked broken.
fillDatabase
  :: Backend a
  => a
    -- ^ The backend
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
fillDatabase env repo handle ifexists action = tryBracket
  (do
    r <- kickOffDatabase env def
      { kickOff_repo = repo
      , kickOff_fill = Just $ KickOffFill_writeHandle handle
      }
    when (kickOffResponse_alreadyExists r) ifexists)
  (\_ e -> workFinished env WorkFinished
    { workFinished_work = def
        { work_repo = repo
        , work_handle = handle
        }
    , workFinished_outcome = case e of
        Left ex -> Outcome_failure (Failure (showt ex))
        Right _ -> Outcome_success def
    })
  $ const action
