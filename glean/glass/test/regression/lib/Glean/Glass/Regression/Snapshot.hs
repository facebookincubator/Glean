{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

--
-- Simple snapshot testing framework for Glass methods
--

module Glean.Glass.Regression.Snapshot where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Default
import Data.List ( sort )
import Data.Text ( Text )
import System.Directory ( copyFile, listDirectory )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process ( readProcessWithExitCode )
import Test.HUnit ( Test(..), assertFailure )
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml

import qualified Thrift.Protocol
import qualified Thrift.Protocol.JSON as Thrift

import Glean ( Repo, Backend )
import Glean.Util.Some ( Some )
import Glean.Regression.Test ( mainTestIndex )
import qualified Glean.Indexer as Glean

import qualified Glean.Glass.Handler as Glass
import Glean.Glass.Types as Glass
import Glean.Glass.Env as Glass ( Env )
import Glean.Glass.Regression.Util as Glass ( withTestEnv )

newtype Cfg =
  Cfg {
    cfgReplace :: Bool
  }

-- queries are just thrift method names, and the toJSON string of the
-- args you want to pass
data Query
  = Query {
      action :: Text, -- ^ name of glass.thrift method
      args :: Aeson.Value -- json object of args to method
  } deriving (Show)

instance FromJSON Query where
  parseJSON = withObject "query" $ \v -> Query
    <$> v .: "action"
    <*> v .: "args"

type Output = FilePath

findQueries :: FilePath -> IO (Map.Map String FilePath)
findQueries root = do
  files <- listDirectory root
  return $ Map.fromList
    [ (takeBaseName file, root </> file)
    | file <- files
    , ".query" `isExtensionOf` file
    ]

parseQuery :: FilePath -> IO Query
parseQuery qfile = do
  r <- Yaml.decodeFileEither qfile
  case r of
    Left err -> error
      $ qfile ++ ": invalid query - " ++ Yaml.prettyPrintParseException err
    Right q ->
      return q

type Getter = IO (Some Backend, Repo)

mainGlassSnapshot
  :: String
  -> FilePath
  -> Glean.Indexer opts
  -> (Getter -> [Test])
  -> IO ()
mainGlassSnapshot testName testRoot indexer extras = do
  -- just check for --replace, everything else is passed through
  -- really want to compose these with the underlying testsuite's options
  cfgReplace <- ("--replace" `elem`) <$> getArgs
  qs <- findQueries testRoot
  withOutput cfgOutput $ \temp ->
    mainTestIndex testName indexer $ \get ->
      TestList $ testAll cfgReplace temp qs get : extras get
  where
    cfgOutput = Nothing

    withOutput (Just out) f = f out
    withOutput Nothing f = withSystemTempDirectory testName f

testAll :: Bool -> FilePath -> Map.Map String FilePath -> Getter -> Test
testAll cfgReplace outDir queries getter = TestList
  [ mkTest cfgReplace getter name qfile outDir
  | (name, qfile) <- Map.toList queries
  ]

mkTest :: Bool -> Getter -> String -> FilePath -> FilePath -> Test
mkTest cfgReplace get name qfile tempDir = TestLabel name $ TestCase $ do
  query <- parseQuery qfile
  let actual = tempDir </> replaceExtension (takeFileName qfile) "out"
      expected = replaceExtension qfile "out"
  (backend, _repo) <- get
  Glass.withTestEnv backend $ \env -> do
    evalQuery env qfile query actual
    if cfgReplace
      then copyFile actual expected
      else diff actual expected

evalQuery :: Glass.Env -> FilePath -> Query -> FilePath -> IO ()
evalQuery glassEnv qFile Query{..} oFile = case action of
  "documentSymbolListX" -> withObjectArgs qFile oFile args
    (Glass.documentSymbolListX glassEnv)
  "documentSymbolIndex" -> withObjectArgs qFile oFile args
    (Glass.documentSymbolIndex glassEnv)
  "jumpTo" -> withObjectArgs qFile oFile args
    (Glass.jumpTo glassEnv)
  "findReferences" -> withSymbolId oFile args
    (Glass.findReferences glassEnv)
  "findReferenceRanges" -> withSymbolId oFile args
    (Glass.findReferenceRanges glassEnv)
  "resolveSymbol" -> withSymbolId oFile args
    (Glass.resolveSymbol glassEnv)
  "resolveSymbolRange" -> withSymbolId oFile args
    (Glass.resolveSymbolRange glassEnv)
  "describeSymbol" -> withSymbolId oFile args
    (Glass.describeSymbol glassEnv)
  "searchByName" ->  withObjectArgs qFile oFile args
    (Glass.searchByName glassEnv)
  "searchByNamePrefix" ->  withObjectArgs qFile oFile args
    (Glass.searchByNamePrefix glassEnv)
  "searchBySymbolId" -> withSymbolId oFile args
    (Glass.searchBySymbolId glassEnv)

  _ -> error $ "Invalid action: " <> show action

decodeObjectAsThriftJson
  :: (Thrift.Protocol.ThriftStruct a, ToJSON a)
  => Aeson.Value -> Either String a
decodeObjectAsThriftJson
  = Thrift.deserializeJSON . S.concat . B.toChunks. encode

withSymbolId
  :: (ToJSON a, SortedResponse a)
  => FilePath -> Value -> (SymbolId -> RequestOptions -> IO a) -> IO ()
withSymbolId oFile args f = do
  req <- case fromJSON args of
          Success sym -> pure (SymbolId sym)
          Error str -> assertFailure $ "Invalid SymbolId: " <> str
  res <- f req def
  writeResult oFile res

withObjectArgs
 :: (Thrift.Protocol.ThriftStruct req, ToJSON req, ToJSON a, SortedResponse a)
 => FilePath
 -> FilePath
 -> Value
 -> (req -> RequestOptions -> IO a) -> IO ()
withObjectArgs qFile oFile args f = do
  req <- parseAsObject qFile args
  res <- f req def
  writeResult oFile res

parseAsObject
  :: (Thrift.Protocol.ThriftStruct a, ToJSON a) => String -> Value -> IO a
parseAsObject file args = case decodeObjectAsThriftJson args of
  Left str -> assertFailure $ "Invalid args in " <> file <> " : " <> str
  Right req -> pure req

writeResult :: (ToJSON a, SortedResponse a) => FilePath -> a -> IO ()
writeResult oFile res = B.writeFile oFile content
  where
    generatedTag = '@':"generated"
    content = J.encodePretty' cfg $ (generatedTag,sorted res)

    cfg = J.defConfig {
      J.confCompare = compare
    }

class SortedResponse a where
  sorted :: a -> a

instance SortedResponse DocumentSymbolListXResult where
  sorted (DocumentSymbolListXResult refs defs rev) =
    DocumentSymbolListXResult (sorted refs) (sorted defs) rev

instance SortedResponse DocumentSymbolIndex where
  sorted (DocumentSymbolIndex syms rev size) =
    DocumentSymbolIndex (Map.map sort syms) rev size

instance SortedResponse SearchByNameResult where
  sorted (SearchByNameResult syms deets) =
    SearchByNameResult (sorted syms) (sorted deets)

instance SortedResponse SearchBySymbolIdResult where
  sorted (SearchBySymbolIdResult syms) =
    SearchBySymbolIdResult (sorted syms)

instance Ord a => SortedResponse [a] where
  sorted = sort

instance SortedResponse Range where sorted = id
instance SortedResponse Location where sorted = id
instance SortedResponse LocationRange where sorted = id
instance SortedResponse SymbolDescription where sorted = id

diff :: FilePath -> FilePath -> IO ()
diff outGenerated outSpec = do
  (e, sout, serr) <- readProcessWithExitCode
    "diff"
    [outGenerated, outSpec]
    ""
  case e of
    ExitSuccess -> return ()
    ExitFailure n -> assertFailure $
      takeFileName outGenerated ++
        if n == 1
          then ": unexpected result\n" ++ sout
          else ": fatal error\n" ++ serr
