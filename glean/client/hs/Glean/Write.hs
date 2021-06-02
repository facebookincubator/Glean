-- | Utilities for writing data to Glean
module Glean.Write
  ( parseRef
  , parseJsonFactBatches
  , dumpJsonToFile
  ) where

import Control.Monad.Extra
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import Data.Maybe
import qualified Data.Vector as Vector
import System.IO
import Text.Printf

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
