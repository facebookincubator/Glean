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

module Glean.Glass.Regression.Snapshot (
    mainGlassSnapshot,
    mainGlassSnapshotGeneric,
    mainGlassSnapshotXLang,
    Cfg(..),
    Output, Getter
  ) where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson ( parse )
import Data.Default
import Data.List ( sort )
import Data.Text ( Text )
import System.Directory ( copyFile, listDirectory )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process ( readProcessWithExitCode )
import Options.Applicative (Parser)
import Test.HUnit ( Test(..), assertFailure )
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Util.List ( uniq )
import Data.Tuple.Extra
import Data.Either.Extra

import qualified Thrift.Protocol
import qualified Thrift.Protocol.JSON as Thrift

import Glean ( Repo )
import Glean.Indexer
import Glean.LocalOrRemote ( LocalOrRemote )
import Glean.Util.Some ( Some(..) )
import Glean.Regression.Test
import qualified Glean.Indexer as Glean

import qualified Glean.Glass.Handler as Glass
import Glean.Glass.Types as Glass
import Glean.Glass.Env as Glass ( Env )
import Glean.Glass.Regression.Util as Glass ( withTestEnv )
import qualified Glean.Regression.Snapshot.Driver as Glean
import qualified Glean.Glass.SymbolId as Glass
import qualified Glean.Glass.SymbolId.Cxx.Parse as Cxx

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

type Getter = IO (Some LocalOrRemote, Repo)

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

mainGlassSnapshot
  :: String
  -> FilePath
  -> Glean.Indexer opts
  -> (Getter -> [Test])
  -> IO ()
mainGlassSnapshot testName testRoot indexer extras =
  mainGlassSnapshot_ testName testRoot
    (Glean.driverFromIndexer indexer) (pure ()) extras

mainGlassSnapshotGeneric
  :: String
  -> FilePath
  -> Glean.Driver opts
  -> (Getter -> [Test])
  -> IO ()
mainGlassSnapshotGeneric testName testRoot driver extras =
  mainGlassSnapshot_ testName testRoot driver (pure ()) extras

mainGlassSnapshot_
  :: String
  -> FilePath
  -> Glean.Driver opts
  -> Parser extraOpts
  -> (Getter -> [Test])
  -> IO ()
mainGlassSnapshot_ testName testRoot driver extraOpts extras = do
  -- just check for --replace, everything else is passed through
  -- really want to compose these with the underlying testsuite's options
  cfgReplace <- any (`elem` ["--replace", "--replace-all"]) <$> getArgs
  qs <- findQueries testRoot
  withOutput cfgOutput $ \temp ->
    mainTestIndexGeneric driver extraOpts testName $ \_ _ _ _ get ->
      TestList $ testAll cfgReplace temp qs get : extras get
  where
    cfgOutput = Nothing

    withOutput (Just out) f = f out
    withOutput Nothing f = withSystemTempDirectory testName f

mainGlassSnapshotXLang
  :: String
  -> FilePath
  -> (Glean.Driver opt, Text)
  -> (Indexer opts, Text)
  -> IO ()
mainGlassSnapshotXLang testName testRoot driver indexer = do
  cfgReplace <- any (`elem` ["--replace", "--replace-all"]) <$> getArgs
  qs <- findQueries testRoot
  withOutput cfgOutput $ \temp ->
    mainTestIndexXlang driver indexer testName $
      \get -> TestList [testAll cfgReplace temp qs get]
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

xlang :: RequestOptions -> RequestOptions
xlang opts = opts { requestOptions_feature_flags = Just
  (def { featureFlags_include_xlang_refs = Just True })
}

evalQuery :: Glass.Env -> FilePath -> Query -> FilePath -> IO ()
evalQuery glassEnv qFile Query{..} oFile = case action of
  "documentSymbolListX" -> withObjectArgs qFile oFile args
    (\req opts -> Glass.documentSymbolListX glassEnv req (xlang opts))
  "documentSymbolIndex" -> withObjectArgs qFile oFile args
    (\req opts -> Glass.documentSymbolIndex glassEnv req (xlang opts))
  "findReferences" -> withSymbolId oFile args
    (Glass.findReferences glassEnv)
  "findReferenceRanges" -> withSymbolId oFile args
    (Glass.findReferenceRanges glassEnv)
  "resolveSymbolRange" -> withSymbolId oFile args
    (Glass.resolveSymbolRange glassEnv)
  "describeSymbol" -> withSymbolId oFile args
    (Glass.describeSymbol glassEnv)
  "searchSymbol" ->  withObjectArgs qFile oFile args
    (Glass.searchSymbol glassEnv)
  "searchBySymbolId" -> withSymbolId oFile args
    (Glass.searchBySymbolId glassEnv)
  "searchRelated" -> withObjectAndSymbolId qFile oFile args
    (Glass.searchRelated glassEnv)
  "searchRelatedNeighborhood" -> withSymbolId oFile args
    (\sym opts -> Glass.searchRelatedNeighborhood glassEnv sym opts
       def { relatedNeighborhoodRequest_hide_uninteresting = True } )
  "fileIncludeLocations" -> withObjectArgs qFile oFile args
    (Glass.fileIncludeLocations glassEnv)

  -- this lists all symbol ids in a file and then validates them
  -- wrapper for validating C++
  "validateCxxSymbolIds" -> withObjectArgs qFile oFile args
    (validateCxxSymbols glassEnv)

  _ -> error $ "Invalid action: " <> show action

-- | Take all the symbol ids and validate them as C++ symbol id syntax
-- Return the errors and any successes as records
validateCxxSymbols
  :: Env -> DocumentSymbolsRequest -> RequestOptions
  -> IO [Either [Text] Cxx.SymbolEnv]
validateCxxSymbols glassEnv req def = do
  res <- Glass.documentSymbolListX glassEnv req def
  let defns = map Glass.definitionSymbolX_sym
        (Glass.documentSymbolListXResult_definitions res)
      refs = map Glass.referenceRangeSymbolX_sym
        (Glass.documentSymbolListXResult_references res)
      syms = uniq (defns ++ refs)
      toks = map thd3 (filter ((== Language_Cpp) . snd3) -- only cxx ids thx
                (rights (map Glass.symbolTokens syms)))
  return (map Cxx.validateSymbolId toks)

decodeObjectAsThriftJson
  :: (Thrift.Protocol.ThriftStruct a, ToJSON a)
  => Aeson.Value -> Either String a
decodeObjectAsThriftJson
  = Thrift.deserializeJSON . S.concat . B.toChunks. encode

withSymbolId
  :: (ToJSON a, DeterministicResponse a)
  => FilePath -> Value -> (SymbolId -> RequestOptions -> IO a) -> IO ()
withSymbolId oFile args f = do
  req <- case fromJSON args of
          Success sym -> pure (SymbolId sym)
          Error str -> assertFailure $ "Invalid SymbolId: " <> str
  res <- f req def
  writeResult oFile res

withObjectArgs
 :: (Thrift.Protocol.ThriftStruct req,
     ToJSON req, ToJSON a, DeterministicResponse a)
 => FilePath
 -> FilePath
 -> Value
 -> (req -> RequestOptions -> IO a) -> IO ()
withObjectArgs qFile oFile args f = do
  req <- parseAsObject qFile args
  res <- f req def
  writeResult oFile res

withObjectAndSymbolId
 :: (Thrift.Protocol.ThriftStruct req,
    ToJSON req, ToJSON a, DeterministicResponse a)
 => FilePath
 -> FilePath
 -> Value
 -> (SymbolId -> RequestOptions -> req -> IO a) -> IO ()
withObjectAndSymbolId qFile oFile args f = do
  let argParser = withObject "symbol-and-request" $ \o -> do
        sym <- o .: "symbol"
        req <- o .: "request"
        pure (SymbolId sym, req)

  (sym, rawReq) <- case Aeson.parse argParser args of
    Success (a, b) -> pure (a,b)
    Error str -> assertFailure $
        "Invalid json in " <> qFile <> " : " <> str

  req <- case decodeObjectAsThriftJson rawReq of
          Left str -> assertFailure $
            "Invalid args in " <> qFile <> " : " <> str
          Right req -> pure req
  res <- f sym def req
  writeResult oFile res

parseAsObject
  :: (Thrift.Protocol.ThriftStruct a, ToJSON a) => String -> Value -> IO a
parseAsObject file args = case decodeObjectAsThriftJson args of
  Left str -> assertFailure $ "Invalid args in " <> file <> " : " <> str
  Right req -> pure req

writeResult :: (ToJSON a, DeterministicResponse a) => FilePath -> a -> IO ()
writeResult oFile res = B.writeFile oFile content
  where
    generatedTag = '@':"generated"
    content = J.encodePretty' cfg (generatedTag,det res)

    cfg = J.defConfig {
      J.confCompare = compare
    }

class DeterministicResponse a where
  det :: a -> a

instance DeterministicResponse (Either [Text] Cxx.SymbolEnv) where
  det = id

instance DeterministicResponse DocumentSymbolListXResult where
  det (DocumentSymbolListXResult refs defs _rev truncated digest fileMap) =
    DocumentSymbolListXResult (det refs) (det defs) (Revision "testhash")
      truncated
      digest
      fileMap
      -- n.b. don't want to include any test group revision tags

instance DeterministicResponse DocumentSymbolIndex where
  det (DocumentSymbolIndex syms _rev size truncated digest fileMap) =
    DocumentSymbolIndex (Map.map sort syms) (Revision "testhash") size truncated
      digest
      fileMap

instance DeterministicResponse SymbolSearchResult where
  det (SymbolSearchResult syms deets) =
    SymbolSearchResult (det syms) (det deets)

instance DeterministicResponse SearchBySymbolIdResult where
  det (SearchBySymbolIdResult syms) =
    SearchBySymbolIdResult (det syms)

instance (DeterministicResponse a, Ord a) => DeterministicResponse [a] where
  det = sort . map det

instance DeterministicResponse Range where det = id
instance DeterministicResponse Location where det = id
instance DeterministicResponse LocationRange where det = id
instance DeterministicResponse SearchRelatedResult where
  det (SearchRelatedResult xs ys) = -- to edit the desc hash
    SearchRelatedResult (det xs) (det ys)
instance DeterministicResponse RelatedNeighborhoodResult where
  det (RelatedNeighborhoodResult as bs cs ds es fs gs hs is js) =
    RelatedNeighborhoodResult (det as) (det bs) (det cs) (det ds)
      (det es) (det fs) (det gs) (det hs) (det is) (det js)
instance DeterministicResponse RelatedSymbols where
  det = id
instance DeterministicResponse InheritedSymbols where
  det (InheritedSymbols a xs) = InheritedSymbols a (det xs)
instance DeterministicResponse SymbolId where
  det = id
instance DeterministicResponse DefinitionSymbolX where
  det = id
instance DeterministicResponse ReferenceRangeSymbolX where
  det = id
instance DeterministicResponse SymbolBasicDescription where
  det = id
instance DeterministicResponse (Map.Map Text SymbolBasicDescription) where
  det = Map.map det
instance DeterministicResponse SymbolDescription where
  det sd = sd
    { symbolDescription_repo_hash = Revision "testhash"
    , symbolDescription_contains_relation = Glass.RelationDescription
        { relationDescription_firstParent = Just $ SymbolId "nondeterministic"
        , relationDescription_firstChild = Just $ SymbolId "nondeterministic"
        , relationDescription_hasMoreChildren =
            relationDescription_hasMoreChildren $
              symbolDescription_contains_relation sd
        , relationDescription_hasMoreParents =
            relationDescription_hasMoreParents $
              symbolDescription_contains_relation sd
        , relationDescription_firstParentName = Nothing
        , relationDescription_firstChildName = Nothing
        }
    , symbolDescription_extends_relation = Glass.RelationDescription
        { relationDescription_firstParent = Just $ SymbolId "nondeterministic"
        , relationDescription_firstChild = Just $ SymbolId "nondeterministic"
        , relationDescription_hasMoreChildren =
           relationDescription_hasMoreChildren $
            symbolDescription_extends_relation sd
        , relationDescription_hasMoreParents =
           relationDescription_hasMoreParents $
            symbolDescription_extends_relation sd
        , relationDescription_firstParentName = Nothing
        , relationDescription_firstChildName = Nothing
        }
    }
instance DeterministicResponse (Map.Map Text SymbolDescription) where
  det = Map.map det
instance DeterministicResponse SymbolResult where
  det = id
instance DeterministicResponse FileIncludeLocationResults where
  det (FileIncludeLocationResults _rev (XRefFileList refs)) =
    FileIncludeLocationResults
     (Revision "testhash")
     (XRefFileList (sort (map det refs)))
instance DeterministicResponse FileIncludeXRef where
  det (FileIncludeXRef path incs) =
    FileIncludeXRef path (sort incs)

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
