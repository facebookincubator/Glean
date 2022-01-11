{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, TypeApplications #-}

module Search (main) where

import Control.Monad
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Foldable
import Data.Char
import Data.Int
import Data.Maybe
import Data.Text ( Text, pack )
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc.Util
import Data.Text.Prettyprint.Doc
import Options.Applicative
import System.Process
import Text.Printf

import Util.EventBase
import Util.Log
import Util.OptParse

import qualified Glean
import Glean ( getFactKey, Nat(..) )
import Glean.Impl.ConfigProvider
import Glean.Pretty.Code ()
import Glean.Pretty.Cxx ()
import Glean.Pretty.Hs ()
import Glean.Pretty.Search ()
import Glean.Schema.Code.Types as Code
import Glean.Schema.CodeCxx.Types as Cxx
import Glean.Schema.CodeJava.Types as Java
import Glean.Schema.CodePp.Types as Pp
import Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Java.Types as Java
import Glean.Schema.Src.Types
import Glean.Search.Graph
import Glean.Search.Search
import Glean.Search.Types
import Glean.Util.ConfigProvider
import Glean.Util.Declarations
import Glean.Util.Range (locRange, HasSrcRange(..))
import Glean.Util.Some


data Command
  = FindDeclarations
    { entity :: String
    , refs :: Bool
    , countRefs :: Bool
    , json :: Bool
    , mangled :: Bool
    , limitRefs :: Maybe Int
    , targetFile :: Maybe Text
    , targetLine :: Maybe Nat
    , showDeclId :: Bool
    , caseSensitive :: Bool
    }
  | FindLocalGraph
    { declId :: Int64
    , steps :: Int32
    , dotFile :: Maybe String
    }

data Config = Config
  { cfgService :: Glean.ThriftSource Glean.ClientConfig
  , cfgCommand :: Command
  }

options :: ParserInfo Config
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser Config
    parser = do
      cfgService <- Glean.options
      cfgCommand <- asum
        [ findDeclarations
        , findLocalGraph
          -- there will be more commands later
        ]
      return Config{..}

    findDeclarations =
      commandParser "find-decls" (progDesc "Find declarations by name") $ do
        entity <- strArgument
          (  metavar "NAME"
          <> help "Entity to search for")
        refs <- switch
          (  long "refs"
          <> help "Show references to the declaration(s)" )
        countRefs <- switch
          (  long "count-refs"
          <> help "Show count of references to the declaration(s)" )
        json <- switch
          (  long "json"
          <> help "Show output in JSON format" )
        mangled <- switch
          (  long "mangled"
          <> help "Unmangle the C++ name" )
        limitRefs <- optional $ option (fromIntegral <$> auto @Int)
          (  long "limit-refs"
          <> help "limit query results for refs" )
        targetFile <- optional $ pack <$> strOption
          (  long "file"
          <> help "Target file path" )
        targetLine <- optional $ option (Nat . fromIntegral <$> auto @Int)
          (  long "line"
          <> help "Target line number" )
        showDeclId <- switch
          (  long "show-decl-id"
          <> help "Print glean-id of the declarations" )
        caseSensitive <- flag True False
          (  long "ignore-case"
          <> short 'i'
          <> help "Ignore case when finding matching declarations" )
        return FindDeclarations{..}

    findLocalGraph =
      commandParser "find-local-graph"
            (progDesc "Find local graph for a declaration with a given id") $ do
          declId <- option (auto @Int64)
            (  long "decl-id"
            <> help "The id of a function declaration" )
          steps <- option (auto @Int32)
            (  long "steps"
            <> help "Number of steps in graph searching" )
          dotFile <- optional $ strOption
            (  long "dot-file"
            <> help "write Dot graph to the file")

          return FindLocalGraph{..}

main :: IO ()
main = do
  withConfigOptions options $ \(cfg, cfgOpts) ->
    withEventBaseDataplane $ \evb ->
      withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) -> do
        Glean.withRemoteBackend evb cfgAPI (cfgService cfg) $ \be -> do
          doQuery (Some be) cfg

doQuery :: Some Glean.Backend -> Config -> IO ()
doQuery backend Config{..}
  | FindDeclarations{..} <- cfgCommand = do
    let
      printResults :: String -> [EntityRefs] -> IO ()
      printResults = if json then jsonResults else
        prettyResults targetFile targetLine showDeclId refs countRefs

      logMissing e name = logWarning $ show e <>
          " (results from " <> Text.unpack name <> " omitted)"

    unmangled <- if mangled then unmangle entity else pure entity
    repos <- getSearchRepos backend logMissing
    let
      q = SearchQuery
        { query = Text.pack unmangled
        , case_sensitive = caseSensitive
        , languages = Nothing
        }
    results <- findEntities limitRefs backend repos q (refs || countRefs)
      -- The above searches both the Cpp, etc and Haskell repos
    let filteredResults = filter (matchesTarget targetFile targetLine) results
    printResults entity filteredResults
  | FindLocalGraph{..} <- cfgCommand = do
    repo <- Glean.getLatestRepo backend "fbsource"
    FindLocalGraphResult{..} <-
      findLocalGraph backend repo declId (fromIntegral steps)

    printf "Forward search:\n"
    forM_ (zip [0::Int ..] findLocalGraphResult_source_vertices) $
      \(i, sourceVertex) -> do
        printf "%d:\n" i
        let decl = sourceVertex_declaration sourceVertex
        putDocW 80 $ pretty decl <> line

        let overriding_methods = sourceVertex_overriding_methods sourceVertex
        when (not $ null overriding_methods) $ do
          printf "is overridden by:\n\n"
          forM_ overriding_methods $ \(Override _ _ method) ->
            putDocW 80 $ indent 2 (pretty method) <> line <> line

        let called_functions = sourceVertex_called_functions sourceVertex
        when (not $ null called_functions) $ do
          printf "calls:\n\n"
          forM_ called_functions $ \(FunctionCall _ _ func) ->
            putDocW 80 $ indent 2 (pretty func) <> line <> line

        let dropped_edges = sourceVertex_dropped_edges sourceVertex
        when (dropped_edges > 0) $ do
          when (not (null overriding_methods && null called_functions)) $
            printf "and "
          printf "is overridden by or calls %d more\n" dropped_edges
        printf "\n"

    putStrLn "Backward search:"
    forM_ (zip [0::Int ..] findLocalGraphResult_target_vertices) $
      \(i, targetVertex) -> do
        printf "%d:\n" i
        let decl = targetVertex_declaration targetVertex
        putDocW 80 $ pretty decl <> line

        let overridden_method = targetVertex_overridden_method targetVertex
        when (isJust overridden_method) $ do
          printf "is overriding:\n\n"
          forM_ overridden_method $ \(Override method _ _) ->
            putDocW 80 $ indent 2 (pretty method) <> line <> line

        let calling_functions = targetVertex_calling_functions targetVertex
        when (not $ null calling_functions) $ do
          printf "is called by:\n\n"
          forM_ calling_functions $ \(FunctionCall func _ _) ->
            putDocW 80 $ indent 2 (pretty func) <> line <> line

        let dropped_edges = targetVertex_dropped_edges targetVertex
        when (dropped_edges > 0) $ do
          when (not (isNothing overridden_method && null calling_functions)) $
            printf "and "
          printf "is overriding or called by %d more\n" dropped_edges
        printf "\n"


-- This should be replaced by symbol view-based rendering in the future
prettyResults :: Maybe Text
              -> Maybe Nat
              -> Bool
              -> Bool
              -> Bool
              -> String
              -> [EntityRefs]
              -> IO ()
prettyResults
  targetFile
  targetLine
  showDeclId
  refs
  countRefs
  entity
  results = do
  let
    isFunDefn (Entity_cxx (Cxx.Entity_defn Cxx.Definition_function_{})) = True
    isFunDefn _ = False

    isFunDecl (Entity_cxx (Cxx.Entity_decl Cxx.Declaration_function_{})) = True
    isFunDecl _ = False

    isRecDefn (Entity_cxx (Cxx.Entity_defn Cxx.Definition_record_{})) = True
    isRecDefn _ = False

    isRecDecl (Entity_cxx (Cxx.Entity_decl Cxx.Declaration_record_{})) = True
    isRecDecl _ = False

    isEnumDefn (Entity_cxx (Cxx.Entity_defn Cxx.Definition_enum_{})) = True
    isEnumDefn _ = False

    isEnumDecl (Entity_cxx (Cxx.Entity_decl Cxx.Declaration_enum_{})) = True
    isEnumDecl _ = False

    isEnumerator (Entity_cxx Cxx.Entity_enumerator{}) = True
    isEnumerator _ = False

    isMacro Entity_pp{} = True
    isMacro _ = False

    isJavaClassDecl (Entity_java Java.Entity_class_{}) = True
    isJavaClassDecl _ = False

    isHackDeclaration Entity_hack{} = True
    isHackDeclaration _ = False

    classes :: [(String, Code.Entity -> Bool)]
    classes =
      [ ("C/C++ function definition", isFunDefn)
      , ("C/C++ function declaration", isFunDecl)
      , ("C/C++ record definition", isRecDefn)
      , ("C/C++ record declaration", isRecDecl)
      , ("C/C++ enum definition", isEnumDefn)
      , ("C/C++ enum declaration", isEnumDecl)
      , ("C/C++ enumerator", isEnumerator)
      , ("C preprocessor macro", isMacro)
      , ("Java class declaration", isJavaClassDecl)
      , ("Hack declaration", isHackDeclaration)
      ]

  forM_ classes $ \(desc, pred) -> do
    let these = [ r | r@(EntityRefs _ ent _) <- results, pred ent ]
    when (not (null these)) $ do
      printf "%s has %d %ss" entity (length these) desc
      forM_ targetFile $ \file -> do
        printf " matching target file %s" file
      when (isJust targetFile && isJust targetLine) $ printf " and"
      forM_ targetLine $ \line -> do
        printf " matching target line %d" (unNat line)
      printf ":\n"
      forM_ these $ \declRefs -> do
        putDocW 80 $ indent 2 (pretty $ decl declRefs)
          <> line
        when showDeclId $
          case decl declRefs of
            Entity_cxx (Cxx.Entity_decl (Cxx.Declaration_function_
                (FunctionDeclaration declId _))) ->
              printf "  declaration id: %d\n" declId
            Entity_cxx (Cxx.Entity_defn (Cxx.Definition_function_
                (FunctionDefinition _ (Just fdk)))) -> do
              let decl = functionDefinition_key_declaration fdk
              let declId = functionDeclaration_id decl
              printf "  declaration id: %d\n" declId
            _ -> return ()
        printf "\n"
        when refs $ prettyXRefs (xrefs declRefs)
        when countRefs $
          print (indent 2 $ hsep
            [ "Total References: ", pretty (length (xrefs declRefs)) ])

  when (null results) $
    printf "No declarations for %s found\n" entity

prettyXRefs :: [FileXRef] -> IO ()
prettyXRefs xrefs =
  when (not (null xrefs)) $
    print (indent 2 $ vsep [ "References:", indent 2 (vsep docs) ])
    where docs = map pretty xrefs

jsonResults :: forall a . (ToJSON a) => String -> [a] -> IO ()
jsonResults _ results = LB.putStrLn $ encodePretty $ map toJSON results

unmangle :: String -> IO String
unmangle entity = do
  result <- readProcess "c++filt" [] entity
  return $ takeWhile (\c -> isAlphaNum c || c == ':' || c == '_') result

matchesRange :: Maybe Text -> Maybe Nat -> Maybe Range -> Bool
matchesRange targetFile targetLine mrange =
    matchesFile && matchesLine where
  matchesFile = case (targetFile, mrange) of
    (Nothing, _) -> True
    (Just file, Just Range{..})
        -> file_key range_file == Just file
    _ -> False
  matchesLine = case (targetLine, mrange) of
    (Nothing, _) -> True
    (Just line, Just Range{range_lineBegin = rline})
        -> rline == line
    _ -> False

matchesTarget :: Maybe Text -> Maybe Nat -> EntityRefs -> Bool
matchesTarget targetFile targetLine EntityRefs{..} =
    matchesRange targetFile targetLine $ declToRange decl

-- This should be replaced by derived predicates in the future
declToRange :: Code.Entity -> Maybe Range
declToRange decl =
  case decl of
    Code.Entity_cxx cxx -> cxxEntityToRange cxx
    Code.Entity_pp (Pp.Entity_define defn) -> srcRange <$> getFactKey defn
    Code.Entity_java (Java.Entity_class_ decl) ->
      locRange . Java.classDeclaration_key_loc <$> getFactKey decl
    _ -> Nothing
  where
  cxxEntityToRange cxx = case cxx of
    Cxx.Entity_decl decl -> declarationSrcRange decl
    Cxx.Entity_defn (Cxx.Definition_function_ defn) ->
      srcRange <$>
        (getFactKey =<<
          (functionDefinition_key_declaration <$> getFactKey defn))
    Cxx.Entity_defn (Cxx.Definition_record_ defn) ->
      srcRange <$> (getFactKey =<<
        (recordDefinition_key_declaration <$> getFactKey defn))
    Cxx.Entity_defn (Cxx.Definition_enum_ defn) ->
      srcRange <$> (getFactKey =<<
        (enumDefinition_key_declaration <$> getFactKey defn))
    Cxx.Entity_defn (Cxx.Definition_objcMethod defn) ->
      srcRange <$> (getFactKey =<< getFactKey defn)
    Cxx.Entity_defn (Cxx.Definition_objcContainer defn) ->
      srcRange <$> (getFactKey =<<
        (Cxx.objcContainerDefinition_key_declaration <$> getFactKey defn))
    _ -> Nothing
