{- | Generate thrift/Haskell/C++ headers from a Glean schema

This script is run automatically via Buck custom_rule()s, see
glean/schema/TARGETS.

buck run @mode/opt //glean/schema/gen:gen-schema -- --help

Usage: gen-schema ([--cpp ARG] | [--thrift ARG] | [--hs ARG]) (-i|--input FILE)
                  [-d|--install_dir DIR]

Available options:
  -i,--input FILE          materialized JSON input file
  -h,--help                Show this help text
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Schema.Gen.Main
  ( main
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc hiding ((<>))
import qualified Data.Text.IO as Text ( writeFile )
import Options.Applicative
import System.Directory
import System.FilePath

import Util.Timing

import Glean.Angle.Types
import Glean.Angle.Parser
import Glean.Database.Config (catSchemaFiles)
import Glean.Database.Schema
import Glean.Schema.Gen.Thrift
import Glean.Schema.Gen.Cpp ( genSchemaCpp )
import Glean.Schema.Gen.HackJson ( genSchemaHackJson )
import Glean.Schema.Gen.Haskell ( genSchemaHS )
import Glean.Schema.Gen.Utils ( Mode(..) )
import Glean.Schema.Resolve

data WhichVersion
  = AllVersions
  | CurrentVersion
  | OneVersion Version

data Options = Options
  { thrift :: Maybe FilePath
  , cpp :: Maybe FilePath
  , hackjson :: Maybe FilePath
  , hs :: Maybe FilePath
  , source :: Maybe FilePath
  , input :: Either FilePath FilePath
  , install_dir :: FilePath
  , version :: WhichVersion
  }

options :: Parser Options
options = do
  thrift <- optional $ strOption $
    long "thrift" <> metavar "FILE"
  cpp <- optional $ strOption $
    long "cpp" <> metavar "FILE"
  hackjson <- optional $ strOption $
    long "hackjson" <> metavar "FILE"
  hs <- optional $ strOption $
    long "hs" <> metavar "FILE"
  source <- optional $ strOption $
    long "source" <> metavar "FILE"
  let
    dir = strOption (long "dir" <> metavar "DIR")
    oneFile = strOption (long "input" <> metavar "FILE")
  input <- (Left <$> oneFile) <|> (Right <$> dir)
  install_dir <- strOption $
    long "install_dir" <> short 'd' <> metavar "DIR" <> value ""
  let
    allVersions = flag' AllVersions (long "all-versions")
    justVersion = fmap OneVersion $ option auto $
      long "version" <> metavar "VERSION" <>
      help "version of the schema to generate code for"
  version <- allVersions <|> justVersion <|> pure CurrentVersion
  return Options{..}

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) fullDesc)

  (src, sourceSchemas, schemas) <- reportTime "parse/resolve/typecheck" $ do
    str <- case input opts of
      Left one -> BC.readFile one
      Right dir -> do
        files <- map (dir </>) <$> getDirectoryContents dir
        catSchemaFiles files
    (sourceSchemas, schemas) <- case parseAndResolveSchema str of
      Left err -> throwIO $ ErrorCall $ err
      Right schema -> return schema
    -- just for typechecking
    void $ newDbSchema sourceSchemas schemas readWriteContent
    return (str, sourceSchemas, schemas)

  reportTime "checking schema roundtrip" $ do
    let pp = show (pretty sourceSchemas)
    case parseSchema (BC.pack pp) of
      Left err -> throwIO $ ErrorCall $ "schema roundtrip error: " <> err
         ++ "\n" ++ pp
      Right schemasRoundTrip ->
        when (sourceSchemas /= schemasRoundTrip) $
          throwIO $ ErrorCall "schema did not roundtrip successfully"

  forM_ (source opts) $ \f -> BC.writeFile f src

  reportTime "gen" $ gen opts schemas

gen :: Options -> Schemas -> IO ()
gen Options{..} Schemas{..} = do
  case version of
    CurrentVersion -> genFor schemasCurrentVersion Nothing
    OneVersion v -> genFor v Nothing
    AllVersions ->
      forM_ (HashMap.keys schemasSchemas) $ \v ->
        genFor v (Just ('v' : show v))
  where
  genFor :: Version -> Maybe FilePath -> IO ()
  genFor ver dir = case HashMap.lookup ver schemasSchemas of
    Nothing ->
      fail $ "schema version " ++
        show schemasCurrentVersion ++ " undefined"
    Just Schema{..} -> do
      let
        allTypes = HashMap.unions (map resolvedSchemaTypes schemasResolved)
        allPreds = HashMap.unions (map resolvedSchemaPredicates schemasResolved)

        ts =
          [ typedef
          | (name, versions) <- HashMap.toList schemaTypes
          , version <- Set.toList versions
          , Just typedef <-
              [HashMap.lookup (TypeRef name version) allTypes]
          ]
        ps =
          [ pred
          | (name,versions) <- HashMap.toList schemaPredicates
          , version <- Set.toList versions
          , Just pred <-
              [HashMap.lookup (PredicateRef name version) allPreds]
          ]
      let
        doGen _ Nothing = return ()
        doGen gen (Just output) = do
          let odir = install_dir </> fromMaybe "" dir </> output
          forM_ (gen ver ps ts) $ \(file,text) -> do
            let path = odir </> file
            createDirectoryIfMissing True (takeDirectory path)
            Text.writeFile path (text <> "\n")
      doGen genSchemaCpp cpp
      doGen genSchemaHackJson hackjson
      doGen genSchemaHS hs
      doGen (genSchemaThrift Data dir) thrift
      doGen (genSchemaThrift Query dir) thrift
