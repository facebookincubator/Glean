{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{- | Generate thrift/Haskell/C++/Python headers from a Glean schema

This script is run automatically via Buck custom_rule()s, see
glean/schema/TARGETS.

buck run @mode/opt //glean/schema/gen:gen-schema -- --help

Usage: gen-schema ([--cpp ARG] | [--thrift ARG] | [--hs ARG] | [--py ARG])
                  (-i|--input FILE) [-d|--install_dir DIR]

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
import Control.Monad.Except
import Data.Bifoldable (bifoldMap)
import qualified Data.ByteString.Char8 as BC
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.Graph
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text, splitOn)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Tree as Tree
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import qualified Text.Fuzzy as Fuzzy
import Text.Printf

import Thrift.Util
import Util.IO
import Util.Timing
import Util.OptParse (commandParser)

import Glean.Query.Typecheck (tcQueryDeps)
import Glean.Query.Codegen.Types (QueryWithInfo(..))
import Glean.Angle.Types
import Glean.Angle.Parser
import Glean.Database.Config hiding (options)
import Glean.Database.Schema
import Glean.Database.Schema.ComputeIds
import Glean.Database.Schema.Types
import Glean.Display
import qualified Glean.Internal.Types as Internal
import Glean.RTS.Types (PidRef(..), ExpandedType(..))
import Glean.Schema.Resolve (resolveSchemaRefs)
import Glean.Schema.Util (showRef)
import Glean.Schema.Gen.Thrift
import Glean.Schema.Gen.Cpp ( genSchemaCpp )
import Glean.Schema.Gen.HackJson ( genSchemaHackJson )
import Glean.Schema.Gen.Haskell ( genSchemaHS )
import Glean.Schema.Gen.Python ( genSchemaPy )
import Glean.Schema.Gen.OCaml ( genSchemaOCaml )
import Glean.Schema.Gen.Rust ( genSchemaRust )
import Glean.Schema.Gen.Utils ( NameSpaces, Oncall )

import Glean.Schema.Types
import Glean.Types (SchemaId(..))

data Options = Options
  { input :: Either FilePath FilePath
  , omitArchive :: Bool
  , actOptions :: Either GraphOptions GenOptions
  }

data GraphOptions = GraphOptions
  { target :: Maybe Text
  , maxDepth :: Maybe Int
  , reverseDeps :: Bool
  , topologicalSort :: Bool
  , graphType :: GraphType
  , ignoreDerivations :: Bool
  }

data GraphType = SchemaGraph | PredicateGraph

data GenOptions =  GenOptions
  { thrift :: Maybe FilePath
  , cpp :: Maybe FilePath
  , hackjson :: Maybe FilePath
  , hs :: Maybe FilePath
  , py :: Maybe FilePath
  , ocaml :: Maybe FilePath
  , rust :: Maybe FilePath
  , source :: Maybe FilePath
  , updateIndex :: Maybe FilePath
  , install_dir :: FilePath
  , restrictSchemas :: Maybe [NameSpaces]
  , oncall :: Maybe Oncall
  }

toNameSpaces :: [Text] -> [NameSpaces]
toNameSpaces names = splitOn "." <$> names

optSchemas :: Parser (Maybe [NameSpaces])
optSchemas = optional ( toNameSpaces . splitOn "," <$> strOption
        ( long "schemas"
        <> metavar "SCHEMAS"
        <> help "Restrict schemas to generate" ))

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
          <> help "reference schema/predicate"
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
        ignoreDerivations <- switch
          ( long "ignore-derivations"
          <> help "in a predicate graph ignore dependencies \
              \from derivation queries."
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
      py <- optional $ strOption $
        long "py" <> metavar "FILE"
      ocaml <- optional $ strOption $
        long "ocaml" <> metavar "FILE"
      rust <- optional $ strOption $
        long "rust" <> metavar "FILE"
      source <- optional $ strOption $
        long "source" <> metavar "FILE"
      updateIndex <- optional $ strOption $
        long "update-index" <> metavar "FILE" <>
        help ("Add the schema to the index at FILE, making it the new " <>
          "current schema")
      install_dir <- strOption $
        long "install_dir" <> short 'd' <> metavar "DIR" <> value ""
      oncall <- optional $ strOption
        ( long "oncall"
          <> value ""
          <> metavar "NAME"
          <> help "Adds oncall annotation with oncall NAME to generated files "
        )

      restrictSchemas <- optSchemas
      return GenOptions{..}

main :: IO ()
main = do
  Options{..} <- execParser (info (options <**> helper) fullDesc)
  (src, schema, dbschema) <-
    reportTime "parse/resolve/typecheck" $ do
    str <- case input of
      Left one -> BC.readFile one
      Right dir -> do
        files <- listDirectoryRecursive dir
        catSchemaFiles $
          if omitArchive
            then filter (not . ("/archive/" `isInfixOf`)) files
            else files
    schema <- case processSchema Nothing str of
      Left err -> throwIO $ ErrorCall err
      Right schema -> return schema

    -- Just check that parseSchemaCached works, because the server
    -- will be using it.
    case processSchemaCached Nothing HashMap.empty str of
      Left err -> throwIO $ ErrorCall $ err
      Right{} -> return ()

    -- for typechecking
    dbschema <- newDbSchema Nothing (SchemaIndex schema [])
      LatestSchema readWriteContent def
    return (str, schema, dbschema)

  let ProcessedSchema sourceSchemas resolved _ = schema

  reportTime "checking schema roundtrip" $ do
    let pp = show (displayDefault sourceSchemas)
    case parseSchema (BC.pack pp) of
      Left err -> throwIO $ ErrorCall $ "schema roundtrip error: " <> err
         ++ "\n" ++ pp
      Right schemasRoundTrip ->
        when (rmLocSchemas sourceSchemas /= rmLocSchemas schemasRoundTrip) $
          throwIO $ ErrorCall "schema did not roundtrip successfully"

  refsResolved <- either (die 1 . Text.unpack) return $
    runExcept $ resolveSchemaRefs sourceSchemas

  let
    -- We have to ensure the types and predicates exported by each
    -- "all" schema is transitively closed, otherwise either the code
    -- generator will fail, or the generated code will fail to compile.
    allSchemas =
      [ schema {
          resolvedSchemaReExportedPredicates = preds,
          resolvedSchemaReExportedTypes = types
        }
      | schema <- schemasResolved resolved
      , resolvedSchemaName schema == "all"
      , let
          deps :: [ResolvedSchemaRef]
          deps =
            [ s
            | Just v <- [toVertex (schemaSrcRef (schemaRef schema))]
            , (_, n, _) <- reachableFrom v
            , Just s <- [HashMap.lookup n schemaMap]
            ]
          types = HashMap.unions (map resolvedSchemaTypes deps)
          preds = HashMap.unions (map resolvedSchemaPredicates deps)
      ]

    schemaSrcRef (SchemaRef n v) = SourceRef n (Just v)

    (depGraph, fromVertex, toVertex) = graphFromEdges edges

    reachableFrom v = map fromVertex $
      concatMap Tree.flatten (dfs depGraph [v])

    edges =
      [ (schema, schemaName schema, schemaDependencies schema)
      | schema <- srcSchemas refsResolved ]

    schemaMap = HashMap.fromList
      [ (schemaSrcRef (schemaRef s), s) | s <- schemasResolved resolved ]

    findVersion v = listToMaybe
      [ s | s@ResolvedSchema{..} <- allSchemas
      , resolvedSchemaVersion == v
      ]

  versions <-
    case schemasHighestVersion resolved of
      Just ver
        | Just schema <- findVersion ver, ver == schemaAllVersion dbschema ->
          return [(ver, schemaId dbschema, schema, Nothing)]
      _otherwise -> fail "missing 'all' schema"

  case actOptions of
    Left opts -> graph opts dbschema refsResolved [ v | (v,_,_,_) <- versions ]
    Right opts -> do
      forM_ (source opts) $ \f -> BC.writeFile f src
      forM_ (updateIndex opts) (doUpdateIndex src schema)
      reportTime "gen" $ gen opts versions

graph :: GraphOptions -> DbSchema -> SourceSchemas -> [Version] -> IO ()
graph opts dbschema sourceSchemas versions =
  Text.putStrLn $ drawGraph opts graph roots
  where
    predicatesGraph = predicateGraph (ignoreDerivations opts) dbschema
    (roots, graph) = case graphType opts of
      PredicateGraph ->
        case target opts of
          Nothing -> (Map.keys predicatesGraph, predicatesGraph)
          Just t -> case Map.lookup t predicatesGraph of
            Nothing
              | cand:_ <- Fuzzy.simpleFilter t (Map.keys predicatesGraph)
              -> errorWithoutStackTrace $ Text.unpack $
                "did you mean " <> cand <> " ?"
            Nothing
              -> errorWithoutStackTrace $ Text.unpack $
                "predicate not found in graph: " <> t
            Just _
              -> (pure t, predicatesGraph)
      SchemaGraph ->
        let everyAllSchema  = map (\v -> "all." <> Text.pack (show v)) versions
            roots = maybe everyAllSchema pure (target opts)
        in (roots, schemaGraph sourceSchemas)

schemaGraph :: SourceSchemas -> Map Text [Text]
schemaGraph sourceSchemas = Map.fromList
  [ (showRef (schemaName s), map showRef (schemaDependencies s))
  | s <- srcSchemas sourceSchemas ]

schemaDependencies :: SourceSchema -> [SourceRef]
schemaDependencies SourceSchema{..} =
  schemaInherits ++ [ name | SourceImport name _ <- schemaDecls ]

predicateGraph :: Bool -> DbSchema -> Map Text [Text]
predicateGraph ignoreDerivations dbschema = Map.fromList
  [ (ref, deps)
  | details <- IntMap.elems (predicatesByPid dbschema)
  , let ref = showRef (predicateRef details)
        deps = showRef <$> Set.toList (dependencies details)
  ]
  where
    dependencies details = asRefs $ keyValueDeps <> derivationDeps
      where
        keyValueDeps =
          typeDeps (predicateKeyType details) <>
          typeDeps (predicateValueType details)
        derivationDeps
          | not ignoreDerivations
          , Derive _ (QueryWithInfo query _ _ _) <- predicateDeriving details
          = tcQueryDeps query
          | otherwise
          = mempty
    typeDeps = bifoldMap overPidRef overExpanded
      where
        overExpanded (ExpandedType _ ty) = typeDeps ty
        overPidRef (PidRef _ ref) = Set.singleton ref

    asRefs = Set.fromList . fmap predicateIdRef . Set.toList

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
  -> [(Version, SchemaId, ResolvedSchemaRef, Maybe FilePath)]
  -> IO ()
gen GenOptions{..} versions =
  mapM_ genFor versions
  where
  genFor :: (Version, SchemaId, ResolvedSchemaRef, Maybe FilePath) -> IO ()
  genFor (_, hash, ResolvedSchema{..}, dir) = do
      let
        ts = HashMap.elems resolvedSchemaReExportedTypes
        ps = HashMap.elems resolvedSchemaReExportedPredicates

        doGen _ Nothing = return ()
        doGen gen (Just output) = do
          let odir = install_dir </> fromMaybe "" dir </> output
          forM_ (gen resolvedSchemaVersion ps ts oncall) $ \(file,text) -> do
            let path = odir </> file
            createDirectoryIfMissing True (takeDirectory path)
            Text.writeFile path (text <> "\n")
      doGen (genSchemaCpp hash) cpp
      doGen genSchemaHackJson hackjson
      doGen genSchemaHS hs
      doGen genSchemaPy py
      doGen (genSchemaOCaml restrictSchemas) ocaml
      doGen (genSchemaRust hash) rust
      doGen (genSchemaThrift dir hash) thrift

-- -----------------------------------------------------------------------------
-- Working with the SchemaIndex

-- | Add a new schema to an existing SchemaIndex.
doUpdateIndex :: BC.ByteString -> ProcessedSchema -> FilePath -> IO ()
doUpdateIndex src new@(ProcessedSchema _ _ hashed) indexFile = do
  hPutStrLn stderr $ "Updating index in " <> indexFile

  -- check that the new schema instance is compatible with the
  -- existing ones
  exists <- doesFileExist indexFile
  when exists $ do
    current <- parseSchemaIndex indexFile
    validateNewSchemaInstance $ current {
      schemaIndexCurrent = new,
      schemaIndexOlder =
        schemaIndexCurrent current : schemaIndexOlder current }

  let
    -- store the new schema in the file "instance/<schemaid>" relative
    -- to the index file, where <schemaid> is the SchemaId of the
    -- latest "all" schema
    file = "instance/" <> unSchemaId (hashedSchemaId hashed)

    versions = Map.fromList [(
        unSchemaId (hashedSchemaId hashed),
        fromIntegral (hashedSchemaAllVersion hashed)
      )]

    baseIndex = def {
        Internal.schemaIndex_current = def {
          Internal.schemaInstance_versions = versions,
          Internal.schemaInstance_file = file }
      }

  -- If this exact schema is present already (perhaps as an older
  -- instance) then promote it to be the current schema. If the new
  -- schema is identical to the current one, this will leave
  -- everything as it was.
  newIndex <- do
    exists <- doesFileExist indexFile
    if exists
      then do
        oldIndex <- loadJSON indexFile
        let
          existingInstances =
            filter (((/=) versions) . Internal.schemaInstance_versions) $
              Internal.schemaIndex_current oldIndex :
              Internal.schemaIndex_older oldIndex
        hPutStrLn stderr $ printf "Old index contains %d instance(s)"
          (1 + length (Internal.schemaIndex_older oldIndex))
        return baseIndex { Internal.schemaIndex_older = existingInstances }
      else
        return baseIndex

  hPutStrLn stderr $ printf "New index contains %d instance(s)"
    (1 + length (Internal.schemaIndex_older newIndex))

  saveJSON indexFile newIndex
  let fileToWrite = takeDirectory indexFile </> Text.unpack file
  createDirectoryIfMissing True (takeDirectory fileToWrite)
  BC.writeFile fileToWrite src
