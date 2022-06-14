{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
import Data.Bifoldable (bifoldMap)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.Graph
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc hiding ((<>))
import qualified Data.Tree as Tree
import Options.Applicative
import System.Directory
import System.FilePath

import Util.IO
import Util.Timing
import Util.OptParse (commandParser)

import Glean.Query.Typecheck (tcQueryDeps)
import Glean.Query.Codegen (QueryWithInfo(..))
import Glean.Angle.Hash
import Glean.Angle.Types
import Glean.Angle.Parser
import Glean.Database.Config (catSchemaFiles)
import Glean.Database.Schema
import Glean.RTS.Types (PidRef(..), ExpandedType(..))
import Glean.Schema.Util (showRef)
import Glean.Schema.Gen.Thrift
import Glean.Schema.Gen.Cpp ( genSchemaCpp )
import Glean.Schema.Gen.HackJson ( genSchemaHackJson )
import Glean.Schema.Gen.Haskell ( genSchemaHS )
import Glean.Schema.Gen.Utils ( Mode(..) )
import Glean.Schema.Resolve
import Glean.Schema.Types

data WhichVersion
  = AllVersions
  | HighestVersion
  | OneVersion Version

data Options = Options
  { input :: Either FilePath FilePath
  , version :: WhichVersion
  , omitArchive :: Bool
  , actOptions :: Either GraphOptions GenOptions
  }

data GraphOptions = GraphOptions
  { target :: Maybe Text
  , maxDepth :: Maybe Int
  , reverseDeps :: Bool
  , topologicalSort :: Bool
  , graphType :: GraphType
  }

data GraphType = SchemaGraph | PredicateGraph

data GenOptions =  GenOptions
  { thrift :: Maybe FilePath
  , cpp :: Maybe FilePath
  , hackjson :: Maybe FilePath
  , hs :: Maybe FilePath
  , source :: Maybe FilePath
  , install_dir :: FilePath
  }

options :: Parser Options
options = do
  omitArchive <- switch $
    long "omit-archive" <>
    help ("ignore .angle files under archive/ directories. " <>
      "Non-archived schemas should not depend on archived schemas; this " <>
      "flag is for testing that that property holds.")
  let
    dir = strOption (long "dir" <> metavar "DIR")
    oneFile = strOption (long "input" <> metavar "FILE")
  input <- (Left <$> oneFile) <|> (Right <$> dir)
  let
    allVersions = flag' AllVersions (long "all-versions")
    justVersion = fmap OneVersion $ option auto $
      long "version" <> metavar "VERSION" <>
      help "version of the schema to generate code for"
  version <- allVersions <|> justVersion <|> pure HighestVersion
  actOptions <-
    fmap Left graphOptions <|>
    fmap Right genOptions
  return Options{..}
  where
    graphOptions = do
      commandParser "graph"
        (progDesc "View the dependency graph of a local schema") $ do
        reverseDeps <- switch
          (  long "reverse-deps"
          <> help "view reverse dependencies instead"
          )
        topologicalSort <- switch
          (  long "topsort"
          <> help "print a topologically sorted list of dependencies"
          )
        target <- optional $ strOption
          (  long "root"
          <> metavar "NODE"
          <> help
            ("reference schema/predicate. "
            <> "If unspecified the latest version of `all`is used.")
          )
        maxDepth <- optional $ option auto
          (  long "depth"
          <> metavar "INT"
          <> help "restrict how many levels deep the tree should go"
          )
        graphType <- flag SchemaGraph PredicateGraph
          (  long "predicates"
          <> help "show a graph of predicates rather than of schemas"
          )

        return GraphOptions{..}

    genOptions = do
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
      install_dir <- strOption $
        long "install_dir" <> short 'd' <> metavar "DIR" <> value ""
      return GenOptions{..}

main :: IO ()
main = do
  Options{..} <- execParser (info (options <**> helper) fullDesc)
  (src, sourceSchemas, schemas, dbschema) <-
    reportTime "parse/resolve/typecheck" $ do
    str <- case input of
      Left one -> BC.readFile one
      Right dir -> do
        files <- listDirectoryRecursive dir
        catSchemaFiles $
          if omitArchive
            then filter (not . ("/archive/" `isInfixOf`)) files
            else files
    (sourceSchemas, schemas) <- case parseAndResolveSchema str of
      Left err -> throwIO $ ErrorCall err
      Right schema -> return schema
    -- for typechecking
    dbschema <- newDbSchema sourceSchemas schemas readWriteContent
    return (str, sourceSchemas, schemas, dbschema)

  reportTime "checking schema roundtrip" $ do
    let pp = show (pretty sourceSchemas)
    case parseSchema (BC.pack pp) of
      Left err -> throwIO $ ErrorCall $ "schema roundtrip error: " <> err
         ++ "\n" ++ pp
      Right schemasRoundTrip ->
        when (rmLocSchemas sourceSchemas /= rmLocSchemas schemasRoundTrip) $
          throwIO $ ErrorCall "schema did not roundtrip successfully"

  let
    -- We have to ensure the types and predicates exported by each
    -- "all" schema is transitively closed, otherwise either the code
    -- generator will fail, or the generated code will fail to compile.
    allSchemas =
      [ schema {
          resolvedSchemaReExportedPredicates = preds,
          resolvedSchemaReExportedTypes = types
        }
      | schema <- schemasResolved schemas
      , resolvedSchemaName schema == "all"
      , let
          deps :: [ResolvedSchemaRef]
          deps =
            [ s
            | Just v <- [toVertex (showSchemaRef (schemaRef schema))]
            , (_, n, _) <- reachableFrom v
            , Just s <- [HashMap.lookup n schemaMap]
            ]
          types = HashMap.unions (map resolvedSchemaTypes deps)
          preds = HashMap.unions (map resolvedSchemaPredicates deps)
      ]

    (depGraph, fromVertex, toVertex) = graphFromEdges edges

    reachableFrom v = map fromVertex $
      concatMap Tree.flatten (dfs depGraph [v])

    schemaDependencies SourceSchema{..} =
      schemaInherits ++ [ name | SourceImport name <- schemaDecls ]

    edges =
      [ (schema, schemaName schema, schemaDependencies schema)
      | schema <- srcSchemas sourceSchemas ]

    schemaMap = HashMap.fromList
      [ (showSchemaRef (schemaRef s), s) | s <- schemasResolved schemas ]

    findVersion v = listToMaybe
      [ s | s@ResolvedSchema{..} <- allSchemas
      , resolvedSchemaVersion == v
      ]

    verToHash = legacyAllVersions dbschema

    hashOf ver = case IntMap.lookup (fromIntegral ver) verToHash of
      Nothing -> error $ "no schema version: " <> show ver
      Just hash -> hash

  versions <- case version of
    HighestVersion ->
      case schemasHighestVersion schemas of
        Just ver | Just schema <- findVersion ver ->
          return [(ver, hashOf ver, schema, Nothing)]
        _otherwise -> fail "missing 'all' schema"
    OneVersion v ->
      case findVersion v of
        Just schema -> return [(v, hashOf v, schema, Nothing)]
        Nothing -> fail $ "can't find all." <> show v
    AllVersions -> do
      let withPath schema = (v, hashOf v, schema, Just $ 'v' : show v)
            where v = resolvedSchemaVersion schema
      return $ withPath <$> allSchemas

  case actOptions of
    Left opts -> graph opts dbschema sourceSchemas [ v | (v,_,_,_) <- versions ]
    Right opts -> do
      forM_ (source opts) $ \f -> BC.writeFile f src
      reportTime "gen" $ gen opts versions

graph :: GraphOptions -> DbSchema -> SourceSchemas -> [Version] -> IO ()
graph opts dbschema sourceSchemas versions =
  Text.putStrLn $ drawGraph opts graph roots
  where
    (roots, graph) = case graphType opts of
      PredicateGraph ->
        let graph = predicateGraph dbschema
            roots = maybe (Map.keys graph) pure (target opts)
        in (roots, graph)
      SchemaGraph ->
        let everyAllSchema  = map (\v -> "all." <> Text.pack (show v)) versions
            roots = maybe everyAllSchema pure (target opts)
        in (roots, schemaGraph sourceSchemas)

schemaGraph :: SourceSchemas -> Map Text [Text]
schemaGraph sourceSchemas = Map.fromList
  [ (schemaName s, dependencies s) | s <- srcSchemas sourceSchemas ]
  where
    dependencies SourceSchema{..} =
      schemaInherits ++ [ name | SourceImport name <- schemaDecls ]

predicateGraph :: DbSchema -> Map Text [Text]
predicateGraph dbschema = Map.fromList
  [ (ref, deps)
  | details <- IntMap.elems (predicatesByPid dbschema)
  , let ref = showRef (predicateRef details)
        deps = showRef <$> Set.toList (dependencies details)
  ]
  where
    dependencies details = keyValueDeps <> derivationDeps
      where
        keyValueDeps =
          typeDeps (predicateKeyType details) <>
          typeDeps (predicateValueType details)
        derivationDeps = case predicateDeriving details of
          Derive _ (QueryWithInfo query _ _) -> tcQueryDeps query
          _ -> mempty
    typeDeps = bifoldMap overPidRef overExpanded
      where
        overExpanded (ExpandedType _ ty) = typeDeps ty
        overPidRef (PidRef _ ref) = Set.singleton ref

drawGraph :: GraphOptions -> Map Text [Text] -> [Text] -> Text
drawGraph GraphOptions{..} deps' roots =
  Text.unlines $ if topologicalSort
    then topsort forest
    else drawTree <$> forest
  where
    forest = map (mkTree deps mempty 1) roots
    deps
      | reverseDeps =
          Map.fromListWith (<>) $ concatMap (\(s,ss) -> (,[s]) <$> ss) $
            Map.toList deps'
      | otherwise = deps'
    mkTree graph seen depth root = Tree.Node root subtree
      where
        subtree = map (mkTree graph (Set.insert root seen) (depth + 1)) children
        continue = notRecursing && canGoDeeper
          where notRecursing  = not (root `Set.member` seen)
                canGoDeeper = maybe True (depth <=) maxDepth
        children
          | continue = Map.findWithDefault [] root graph
          | otherwise = []

    drawTree :: Tree Text -> Text
    drawTree (Node root children) = Text.unlines $ root : draw children
      where
      draw [] = []
      draw (x:xs) = drawNode (null xs) x <> draw xs
      drawNode isLast (Node name children) =
        (pre <> name) : map (spacer <>) (draw children)
        where
          (spacer, pre) = if isLast
            then ("    ", "└── ")
            else ("│   ", "├── ")

    topsort :: Ord a => [Tree a] -> [a]
    topsort forest = reverse postorder
      where
        postorder = go mempty forest (const [])
          where
            go seen [] cont = cont seen
            go seen (Node node children : rest) cont
              | node `Set.member` seen = go seen rest cont
              | otherwise = go (Set.insert node seen) children $
                 \seen' -> node : go seen' rest cont

gen
  :: GenOptions
  -> [(Version, Hash, ResolvedSchemaRef, Maybe FilePath)]
  -> IO ()
gen GenOptions{..} versions =
  mapM_ genFor versions
  where
  genFor :: (Version, Hash, ResolvedSchemaRef, Maybe FilePath) -> IO ()
  genFor (_, hash, ResolvedSchema{..}, dir) = do
      let
        ts = HashMap.elems resolvedSchemaReExportedTypes
        ps = HashMap.elems resolvedSchemaReExportedPredicates

        doGen _ Nothing = return ()
        doGen gen (Just output) = do
          let odir = install_dir </> fromMaybe "" dir </> output
          forM_ (gen resolvedSchemaVersion ps ts) $ \(file,text) -> do
            let path = odir </> file
            createDirectoryIfMissing True (takeDirectory path)
            Text.writeFile path (text <> "\n")
      doGen genSchemaCpp cpp
      doGen genSchemaHackJson hackjson
      doGen genSchemaHS hs
      doGen (genSchemaThrift Data dir hash) thrift
      doGen (genSchemaThrift Query dir hash) thrift
