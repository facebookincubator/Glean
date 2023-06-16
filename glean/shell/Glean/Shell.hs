{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Shell
  ( ShellCommand
  ) where

import Control.Concurrent
import Control.Exception hiding (evaluate)
import Control.Monad.Extra
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.Default
import Data.Functor
import Data.Foldable (asum)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import Text.Printf
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc as Pretty hiding ((<>), pageWidth)
import Data.Text.Prettyprint.Doc.Util as Pretty hiding (words)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Options.Applicative as O
import Options.Applicative hiding (help)
import qualified System.Console.Haskeline as Haskeline
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeBaseName)
import System.IO
import System.IO.Temp
import System.Process (callCommand)
import System.Mem.Weak
import System.Posix.Signals
import System.Exit
import qualified Text.JSON as JSON
import Text.Parsec (runParser)

import Util.JSON.Pretty ()
import Util.List
import Util.OptParse
import Util.String
import Util.Text
import Util.TimeSec

import qualified Glean
import qualified Glean.BuildInfo as BuildInfo
import Glean.Angle.Types as SchemaTypes
import Glean.Display
import Glean.Remote (clientInfo, thriftBackendClientConfig)
import Glean.Database.Ownership
import Glean.Database.Open
import Glean.Database.Schema.ComputeIds (
  emptyHashedSchema, HashedSchema(..), RefTargetId )
import Glean.Database.Config (parseSchemaDir, SchemaIndex(..),
  ProcessedSchema(..))
import qualified Glean.Database.Config as DB (Config(..))
import Glean.Database.Storage (describe)
import Glean.Database.Types (Env(..))
import Glean.Indexer
import Glean.Indexer.List
import Glean.LocalOrRemote as Glean hiding (options, withBackend)
import Glean.RTS.Types (Pid(..), Fid(..))
import Glean.RTS.Foreign.Query (interruptRunningQueries)
import Glean.Schema.Types
import Glean.Schema.Util
import Glean.Shell.Index
import Glean.Shell.Terminal
import Glean.Shell.Types
import Glean.Shell.Error (Ann, BadQuery(..), prettyBadQuery)
import qualified Glean.Types as Thrift
#if GLEAN_FACEBOOK
import Glean.Util.CxxXRef
#endif
import Glean.Util.Service
import Glean.Util.ShellPrint
import Glean.Util.Some
import qualified Glean.Util.ThriftSource as ThriftSource

import GleanCLI.Types

data Config = Config
  { cfgDatabase :: Maybe String
  , cfgQuery :: [String]
  , cfgMode :: ShellMode
  , cfgLimit :: Int64
  , cfgWidth :: Maybe Int
  , cfgPager :: Bool
  }

options :: Parser Config
options = commandParser "shell" (progDesc "Start the Glean shell") parser
  where
    parser = do
      cfgDatabase <- optional $ strOption
        (  long "db"
        <> metavar "NAME"
        <> O.help "database to use (default: use latest complete DB)" )
      cfgMode <- option readMode
        (  long "mode"
        <> metavar "json|angle"
        <> value ShellAngle
        <> O.help "Which mode to start in (default: angle)"
        )
      cfgLimit <- option auto
        (  long "limit"
        <> metavar "N"
        <> value 100
        <> O.help "Set max number of results to fetch"
        )
      cfgQuery <- many $ strArgument
        (  metavar "QUERY"
        <> O.help
          ( "Perform one or more queries or commands"
          <> " (default: enter the REPL)")
        )
      cfgWidth <- optional $ option auto
        (  long "width"
        <> metavar "N"
        <> O.help "Output line width (0: unbounded, default: adaptive)"
        )
      cfgPager <- switch
        (  long "pager"
        <> O.help "Use a pager for displaying long output"
        )
      return Config{..}
     where
      readMode = maybeReader $ \s ->
        case s of
          "json" -> Just ShellJSON
          "angle" -> Just ShellAngle
          _ -> Nothing

output :: Doc Ann -> Eval ()
output doc = do
  ShellState{..} <- getState
  out <- liftIO $ readMVar outputHandle
  liftIO
    $ Pretty.renderIO out
    $ layoutPretty
        LayoutOptions{ layoutPageWidth = fromMaybe Unbounded pageWidth }
    $ (if isTTY then id else Pretty.unAnnotate)
    $ doc <> hardline

outputShellPrint :: ShellPrint a => a -> Eval ()
outputShellPrint x = do
  ShellState{..} <- getState
  out <- liftIO $ readMVar outputHandle
  now <- liftIO $ utcTimeToPOSIXSeconds <$> getCurrentTime
  let
    t0 = Time (round now)
    format = if isTTY then TTY else PlainText
    opts = PrintOpts
      { poFormat = format
      , poNow = t0
      , poWidth = pageWidth
      }
  liftIO $ do
    shellPrint out opts x
    hPutStrLn out ""

newtype Repl a = Repl
  { unRepl :: Haskeline.InputT Eval a
  }
  deriving (Functor, Applicative, Monad, Haskeline.MonadException, MonadIO)

repoString :: Thrift.Repo -> String
repoString repo = concat
  [Text.unpack (Thrift.repo_name repo)
  ,"/"
  ,Text.unpack (Thrift.repo_hash repo)
  ]

lookupPid :: Pid -> Eval (Maybe PredicateRef)
lookupPid (Pid pid) = do
  m <- schemaInfo <$> getState
  case m of
    Nothing -> return Nothing
    Just Thrift.SchemaInfo{..} ->
      return $ Map.lookup pid schemaInfo_predicateIds

withRepo :: (Thrift.Repo -> Eval a) -> Eval a
withRepo f = do
  r <- getRepo
  case r of
    Just repo -> f repo
    Nothing -> liftIO $ throwIO $ ErrorCall "no database selected"

useSchema :: String -> Eval ()
useSchema "" = do
  ShellState{..} <- getState
  maybeCurrentId <- getSchemaId
  forM_ maybeCurrentId $ \currentId ->
    output $ "Using Schema ID: " <> pretty (Thrift.unSchemaId currentId)
  forM_ schemaInfo $ \Thrift.SchemaInfo{..} -> do
    output $ vcat $ map (nest 4 . vcat)
      [ [ "Current schema ID(s):",
          showSchemaIds schemaInfo_schemaIds ]
      , [ "Schema ID(s) stored in the DB:",
          showSchemaIds schemaInfo_dbSchemaIds ]
      , [ "Other available schema ID(s):",
          vcat (map showSchemaIds schemaInfo_otherSchemaIds) ]
      ]
  where
  showSchemaIds m = vcat
    [ pretty schemaId <+> parens ("all." <> pretty ver)
    | (schemaId, ver) <- Map.toList m
    ]
useSchema str = do
  state <- getState
  sel <- case str of
    "current" -> return (Thrift.SelectSchema_current def)
    "stored" -> return (Thrift.SelectSchema_stored def)
    id | Just Thrift.SchemaInfo{..} <- schemaInfo state,
         Text.pack id `Map.member` schemaInfo_schemaIds ||
         Text.pack id `Map.member` schemaInfo_dbSchemaIds ||
         any (Text.pack id `Map.member`) schemaInfo_otherSchemaIds ->
           return (Thrift.SelectSchema_schema_id
             (Thrift.SchemaId (Text.pack id)))
       | otherwise -> liftIO $ throwIO $ ErrorCall $ "unknown schema: " <> id
  Eval $ State.modify $ \s -> s { useSchemaId = sel }
  -- Fetch the new schema
  mapM_ setRepo =<< getRepo

getSchemaCmd :: String -> Eval ()
getSchemaCmd str = do
  maybeProc <- schemas <$> getState

  ProcessedSchema{..} <- case maybeProc of
    Nothing -> liftIO $ throwIO $ ErrorCall
      "no schema loaded. Use :db to load a DB."
    Just proc  -> return proc

  nameEnv <- getNameEnv
  let
    HashedSchema{..} = procSchemaHashed

    opts = defaultDisplayOpts { predicateStyle = PredicateWithoutHash }

    found refs = output $ vcat $ punctuate line $ map pp refs
      where
      pp ref = case ref of
        RefPred p -> case HashMap.lookup p hashedPreds of
          Nothing -> mempty
          Just def -> display opts def
        RefType p -> case HashMap.lookup p hashedTypes of
          Nothing -> mempty
          Just def -> display opts def

  env <- case nameEnv of
    Nothing -> liftIO $ throwIO $ ErrorCall "can't find schema"
    Just env -> return env

  let name = Text.pack (strip str)
  case resolveRef env (parseRef name) of
    ResolvesTo one -> found [one]
    Ambiguous many -> found many
    OutOfScope ->  -- doesn't match exactly; match it as a prefix
      found $ Set.toList $ Set.unions $ HashMap.elems $
        HashMap.filterWithKey prefixMatch env
      where
      prefixMatch k _ = name `Text.isPrefixOf` showRef k

displayStatistics :: String -> Eval ()
displayStatistics arg =
  withRepo $ \repo ->
  withBackend $ \backend -> do
  let containsRemove element list = return (filter (/=element) list,
                                            isJust (find (==element) list))
  let args = words arg
  (args, sortBySize) <- containsRemove "-s" args
  (args, topmost) <- containsRemove "--topmost" args
  predicate <- case args of
    [] -> return ""
    [predicate] -> return predicate
    _ -> liftIO $ throwIO $
      ErrorCall "syntax: :statistics [--topmost] [-s] [<predicate>]"
  let
  xs <- liftIO $ Glean.predicateStats backend repo (
      if topmost then Glean.ExcludeBase else Glean.IncludeBase)
  preds <- forM (Map.toList xs) $ \(id,stats) -> do
    ref <- maybe (Left id) Right <$> lookupPid (Pid id)
    return (ref,stats)
  let
    filterPred :: Either Thrift.Id PredicateRef -> Bool
    filterPred ref =
      null predicate ||
          case ref of
            Right pref -> predicate `isPrefixOf` show (pretty pref)
            Left _ -> False
  let showTotal = null predicate
  let format = StatsFormatOpts { showTotal, sortBySize }
  outputShellPrint $ (filterPred, preds) `withFormatOpts` format

getDatabases
  :: Bool -- ^ Include DBs that can be restored from backups
  -> String -- ^ Only display dbs with a repo name containing this string
  -> Eval [Thrift.Database]
getDatabases all filterStr = do
  state <- getState
  r <- withBackend $ \be ->
    liftIO $ Glean.listDatabases be
      def { Thrift.listDatabases_includeBackups = all
          , Thrift.listDatabases_client_info = Just (client_info state)
          }
  let
    -- argument can be NAME or NAME/HASH
    repoFilter str db =
      str `isInfixOf` repoString repo ||
      Text.pack str `Text.isInfixOf` Thrift.repo_name repo
      where repo = Thrift.database_repo db
    xs = Thrift.listDatabasesResult_databases r
    match = strip filterStr
    dbs = if null match then xs else filter (repoFilter match) xs
  return dbs

displayDatabases:: Bool -> Bool -> String -> Eval ()
displayDatabases all verbose filterStr = do
  dbs <- getDatabases all filterStr
  forM_ (sortOn Thrift.database_created_since_epoch dbs) $ \db ->
    outputShellPrint $ db
      `withFormatOpts` if verbose then DbDescribe else DbSummarise

dbCmd :: String -> Eval ()
dbCmd "" = do
  r <- getRepo
  case r of
    Nothing -> output "no database selected"
    Just repo -> displayDatabases False True (repoString repo)
dbCmd str
  | Just repo <- Glean.parseRepo str = setRepo repo
  | otherwise = do
      let repoName = Text.pack str
      result <- C.try $ withBackend $ \be ->
        liftIO $ Glean.getLatestRepo be repoName
      case result of
        Left e
          | Just Glean.NoDatabase{} <- fromException e -> do
            output $ pretty $ "no " <> repoName <> " database available"
          | Just SomeAsyncException{} <- fromException e -> liftIO $ throwIO e
          | otherwise -> do
            output $ pretty $ "couldn't find database: " <>
              Text.pack (show e)
        Right repo -> do
          output $ pretty $ "using database " ++ repoString repo
          setRepo repo

kickOff :: String -> Eval ()
kickOff s
  | repo:rest <- words s
  , Just repo <- Glean.parseRepo repo = do
      fill <- case rest of
        [] -> return Nothing
        ["write"] -> return $ Just $ Thrift.KickOffFill_writeHandle ""
        [r]
          | Just handle <- stripPrefix "write=" r -> return $ Just $
              Thrift.KickOffFill_writeHandle $ Text.pack handle
          | Just name <- stripPrefix "recipes=" r -> return $ Just $
              Thrift.KickOffFill_recipes $ Text.pack name
          | otherwise -> liftIO $ throwIO $ ErrorCall "Invalid fill spec"
        _ -> liftIO $ throwIO $ ErrorCall "Too many arguments"
      withBackend $ \be -> do
        r <- liftIO $
          Glean.kickOffDatabase be $ def
            { Thrift.kickOff_repo = repo
            , Thrift.kickOff_fill = fill
            }
        when (Thrift.kickOffResponse_alreadyExists r) $
          output "database already exists"
  | otherwise = liftIO $ throwIO $ ErrorCall "Invalid database specification"

restoreDatabase :: String -> Eval ()
restoreDatabase loc = withBackend $ \be ->
  liftIO $ Glean.restoreDatabase be $ Text.pack loc

deleteDatabase :: String -> Eval ()
deleteDatabase db
  | Just repo <- Glean.parseRepo db = withBackend $ \be ->
    void $ liftIO $ Glean.deleteDatabase be repo
  | otherwise
  = liftIO $ throwIO $ ErrorCall "syntax:  :!delete <name>/<hash>"


initialize :: Config -> Eval ()
initialize cfg = mapM_ dbCmd (cfgDatabase cfg)

hello :: Eval ()
hello = output msg
  where
    msg
      | "dev" `Text.isPrefixOf` BuildInfo.buildMode =
        "Glean Shell (dev build)"
      | otherwise =
        "Glean Shell, built on " <> pretty BuildInfo.buildTimeISO8601 <>
        ", from rev " <> pretty BuildInfo.buildRevision

help :: Eval ()
help = do
  hello
  mode <- getMode
  output $ "" <> line <> helptext mode

helptext :: ShellMode -> Doc ann
helptext mode = vcat
  [ "Commands:"
  , indent 2 $ vcat
      [ fillBreak (command_width + 2) (":" <> pretty command)
          <+> align (reflow text)
        | (command,text) <- commands ]
  , ""
  , queries ]
  where
    command_width = maximum $ map (Text.length . fst) commands

    commands :: [(Text.Text,Text.Text)]
    commands =
      [ ("database [<db>]",
            "Use database <db>")
      , ("index <lang> <dir>",
            "Index source files in <dir> and create a database.")
      , ("list [<db>]",
            "List available databases which match <db>")
      , ("list-all [<db>]",
            "List available databases and restorable backups which match "
            <> "<db>")
      , ("debug off|[-]ir|[-]bytecode|all",
            "Enable/disable query debugging options")
      , ("describe [<db>]",
            "Like :list, but show more details")
      , ("describe-all [<db>]",
            "Like :list-all, but show more details")
      , ("mode [json|angle]",
            "Select mode for query syntax and results")
      , ("schema [predicate|type]",
            "Show schema for the given predicate or type")
      , ("edit",
            "Edit a query in an external editor. "
            <> "Set the EDITOR environment variable to choose an editor")
      , ("limit <n>",
            "Set limit on the number of query results")
      , ("load (<file> | <db>/<hash> <file> ...)",
            "Create a DB from file(s) of JSON facts")
      , ("timeout off|<n>",
            "Set the query time budget")
      , ("expand off|on|<predicate>...",
            "Recursively expand nested facts in the response")
      , ("pager off|on",
            "Enable/disable result paging")
      , ("count <query>",
            "Show only a count of query results, not the results themselves")
      , ("more",
            "Fetch more results from the previous query")
      , ("profile [off|summary|full]",
            "Show query profiling information")
      , ("reload",
            "Reload the schema (when using --schema)")
      , ("statistics [--topmost] [-s] [<predicate>]",
            "Show statistics for the database."
            <> " Use --topmost to only show statisticsfor the top database"
            <> " and -s to sort by decreasing size")
      , ("use-schema [current|stored|<schema-id>]",
            "Select which schema to use. Without an argument lists the"
            <> " available schemas")
      , ("quit",
            "Exit the shell")
      ]

    queries = case mode of
      ShellJSON -> helpSchema
      ShellAngle -> helpAngle

helpSchema :: Doc ann
helpSchema = vcat
  [ "Queries (schema mode):"
  , "  {1234}                    Look up a fact by its Id"
  , ""
  , "Pattern syntax (schema mode):"
  , "  1234                     :: byte or nat"
  , "  \"abc\"                    :: string"
  , "  True|False               :: bool"
  , "  [ val, ... ]             :: array(T)"
  , "  { \"field\" : val, ... }   :: record(fields), omitted fields are wild"
  , "  { \"field\" : val }        :: sum(fields)"
  , "  { \"id\" : 1234 }          :: fact with Id 1234"
  , "  { \"key\" : val }          :: fact with key that matches val"
  , "  { \"get\" : { } }          :: fetch fact"
  , ""
  , "Examples:"
  , "  {1234}                                       fetch a fact by its Id"
  , "  pp1.Define                                   all the pp1.Define facts"
  , "  pp1.Define { \"macro\": { \"key\": \"NULL\" } }    every #define of NULL"
  ]

helpAngle :: Doc ann
helpAngle = vcat
  [ "Queries:"
  , "  {1234}                    Look up a fact by its Id"
  , "  <predicate> <pat>         Query a predicate for facts matching <pat>"
  , ""
  , "Pattern syntax:"
  , "  1234                     :: byte or nat"
  , "  \"abc\"                    :: string"
  , "  \"abc\"..                  :: string prefix match"
  , "  true|false               :: bool"
  , "  [ val1, val2]            :: array(T)"
  , "  [ val1, val2, ..]        :: array(T) prefix"
  , "  { field = val, ... }     :: record(fields), omitted fields are wild"
  , "  { field = val }          :: sum(fields)"
  , ""
  , "Please consult the documentation for the full query syntax."
  , ""
  , "Examples:"
  , "  {1234}                                   fetch a fact by its Id"
  , "  pp1.Define _                             all the pp1.Define facts"
  , "  pp1.Define { macro = \"NULL\" }            every #define of NULL"
  ]

withTTY :: Eval a -> Eval (Maybe a)
withTTY action = do
  state <- getState
  if not (isTTY state) then Just <$> action else do
    let !preset_width = pageWidth state
        !outh = outputHandle state
    page_width <- case preset_width of
      Just w -> return w
      Nothing -> do
        width <- fromMaybe 80 <$> liftIO getTerminalWidth
        return $ AvailablePerLine width 1
    let without_pager f = Just <$> f outh
    r <- liftIO $ (if pager state then withPager else without_pager) $
      \handle_var -> State.runStateT (unEval action) state
        { pageWidth = Just page_width
        , outputHandle = handle_var
        }
    forM_ r $ \(_, new_state) -> Eval $ State.put $ new_state
      { pageWidth = preset_width
      , outputHandle = outh
      }
    return $ fst <$> r

evaluate :: String -> Eval Bool
evaluate s = do
  m <- getMode
  let
    go :: Parse pat => (Statement pat -> Eval Bool) -> Eval Bool
    go run = do
      case runParser parse () "<input>" s of
        Left err -> do
          output $ "*** Syntax error:" <+> pretty (show err)
          return False
        Right stmt -> run stmt
  case m of
    ShellJSON -> go doJSONStmt
    ShellAngle -> go doAngleStmt


doJSONStmt :: Statement JSONQuery -> Eval Bool
doJSONStmt (Command name arg) = doCmd name arg
doJSONStmt (FactRef fid) = userFact fid >> return False
doJSONStmt (Pattern query) = do
  q <- fromJSONQuery query
  runUserQuery q
  return False

fromJSONQuery :: JSONQuery -> Eval SchemaQuery
fromJSONQuery (JSONQuery ide deprecatedRec stored pat) = do
  exp <- expandResults <$> getState
  when deprecatedRec deprecatedExpansionWarning
  return SchemaQuery
    { sqPredicate = pred
    , sqRecursive = exp
    , sqStored = stored
    , sqQuery = pat
    , sqCont = Nothing
    , sqTransform = trans
    , sqSyntax = Thrift.QuerySyntax_JSON
    , sqOmitResults = False }
  where
    -- magic transformation when we query for "xrefs". This is to make
    -- debugging of xref issues easier by presenting xref data in an
    -- easier-to-comprehend format.
    (pred, trans)
#if GLEAN_FACEBOOK
      | ide == "xrefs" = ("cxx1.FileXRefs", Just transformXRefs)
#endif
      | otherwise = (ide, Nothing)

doAngleStmt :: Statement AngleQuery -> Eval Bool
doAngleStmt (Command name arg) = doCmd name arg
doAngleStmt (FactRef fid) = userFact fid >> return False
doAngleStmt (Pattern query) = do
  q <- fromAngleQuery query
  runUserQuery q
  return False

fromAngleQuery :: AngleQuery -> Eval SchemaQuery
fromAngleQuery (AngleQuery deprecatedRec stored pat) = do
  exp <- expandResults <$> getState
  when deprecatedRec deprecatedExpansionWarning
  return SchemaQuery
    { sqPredicate = ""
    , sqRecursive = exp
    , sqStored = stored
    , sqQuery = pat
    , sqCont = Nothing
    , sqTransform = Nothing
    , sqSyntax = Thrift.QuerySyntax_ANGLE
    , sqOmitResults = False }

deprecatedExpansionWarning :: Eval ()
deprecatedExpansionWarning = output $ vcat
  [ "WARNING: Deprecated syntax. '!' at the start of a line to "
    <> "recursively expand facts is deprecated."
  , "Fact expansion is now enabled by default."
  , "Use ':expand off' to disable it."
  ]

data Cmd = Cmd
  { cmdName :: String
  , cmdCompletion :: Haskeline.CompletionFunc Eval
  , cmdImpl :: String -> IORef Bool -> Eval ()
  }

commands :: [Cmd]
commands =
  [ Cmd "quit" Haskeline.noCompletion $ const $
      \stop -> liftIO $ writeIORef stop True
  , Cmd "statistics" (completeWords availablePredicates) $
      \str _ -> displayStatistics str
  , Cmd "edit" Haskeline.noCompletion $ \_ _ -> editCmd
  , Cmd "limit" Haskeline.noCompletion $ \str _ -> limitCmd str
  , Cmd "describe" completeDatabaseName $ const . displayDatabases False True
  , Cmd "describe-all" completeDatabaseName $ const . displayDatabases True True
  , Cmd "index" indexCompletion $ \str _ -> indexCmd str
  , Cmd "list" completeDatabaseName $ const . displayDatabases False False
  , Cmd "list-all" completeDatabaseName $ const . displayDatabases True False
  , Cmd "dump" Haskeline.noCompletion $ \str _ -> dumpCmd str
  , Cmd "load" Haskeline.completeFilename $ \str _ -> loadCmd str
  , Cmd "mode" (completeWords (pure ["json","angle"])) $
      \str _ -> setModeCmd str
  , Cmd "more" Haskeline.noCompletion $ const $ const moreCmd
  , Cmd "database" completeDatabases $ const . dbCmd
  , Cmd "db" completeDatabaseName $ const . dbCmd
  , Cmd "debug" (completeWords (pure
      ["off", "ir", "-ir", "bytecode", "-bytecode", "all"])) $
        \str _ -> debugCmd str
  , Cmd "reload" Haskeline.noCompletion $ const $ const reloadCmd
  , Cmd "schema" (completeWords availablePredicatesAndTypes) $
      \str _ -> getSchemaCmd str
  , Cmd "use-schema" completeUseSchema $ \str _ -> useSchema str
  , Cmd "profile" (completeWords (pure ["off","summary","full"])) $
      \str _ -> statsCmd str
  , Cmd "timeout" Haskeline.noCompletion $ \str _ -> timeoutCmd str
  , Cmd "expand" (completeWords (pure ["on", "off"])) $ \str _ -> expandCmd str
  , Cmd "pager" (completeWords (pure ["on", "off"])) $ \str _ -> pagerCmd str
  , Cmd "count" Haskeline.noCompletion $ \str _ -> countCmd str
  , Cmd "!restore" Haskeline.noCompletion $ const . restoreDatabase
  , Cmd "!kickoff" Haskeline.noCompletion $ const . kickOff
  , Cmd "!delete" completeDatabases $ const . deleteDatabase
  , Cmd "!owner" Haskeline.noCompletion $ \str _ -> ownerCmd str
  , Cmd "help" Haskeline.noCompletion $ \_ _ -> help
  , Cmd "?" Haskeline.noCompletion $ \_ _ -> help
  ]

doCmd :: String -> String -> Eval Bool
doCmd name arg0 = do
  stop <- liftIO $ newIORef False
  let arg = strip arg0
  case filter ((== name) . cmdName) commands of
    [Cmd{..}] -> cmdImpl arg stop
    _otherwise -> case filter (isPrefixOf name . cmdName) commands of
      [Cmd{..}]
        | "!" `isPrefixOf` cmdName -> output
            "*** This is an unsafe command, it can't be abbreviated"
        | otherwise -> cmdImpl arg stop
      [] -> output "*** Unknown command. Type :help for help."
      _ -> output "*** Ambiguous command. Type :help for help."
  liftIO $ readIORef stop

ownerCmd :: String -> Eval ()
ownerCmd str
  | Right fid <- textToInt (Text.pack str) = do
    withBackend $ \backend ->
      case backendKind backend of
        BackendEnv env -> withRepo $ \repo -> do
          maybeExpr <- liftIO $ factOwnership env repo (Fid (fromIntegral fid))
          case maybeExpr of
            Nothing -> output "*** no ownership information"
            Just expr -> output (prettyOwner expr)
        _other -> liftIO $ throwIO $ ErrorCall
          "!owner only works with --db-root"
  | otherwise = liftIO $ throwIO $ ErrorCall "syntax:  :!owner <fact>"
  where
  prettyOwner (Unit x) = pretty (Text.decodeUtf8 x)
  prettyOwner (OrOwners [one]) = prettyOwner one
  prettyOwner (OrOwners many) = sep $ intersperse "||" (map prettyOwner1 many)
  prettyOwner (AndOwners [one]) = prettyOwner one
  prettyOwner (AndOwners many) = sep $ intersperse "&&" (map prettyOwner1 many)

  prettyOwner1 (Unit x) = prettyOwner (Unit x)
  prettyOwner1 (OrOwners [one]) = prettyOwner1 one
  prettyOwner1 (AndOwners [one]) = prettyOwner1 one
  prettyOwner1 owner = parens (prettyOwner owner)

moreCmd :: Eval ()
moreCmd = do
  last_query <- lastSchemaQuery <$> getState
  case last_query of
    Just q -> runUserQuery q
    Nothing -> liftIO $ throwIO $ ErrorCall "no last query"

reloadCmd :: Eval ()
reloadCmd = do
  state <- getState
  case updateSchema state of
    Nothing -> output ":reload requires the shell to be started with --schema"
    Just io -> io
  case repo state of
    Nothing -> return ()
    Just repo -> do
      Thrift.GetDatabaseResult{..} <- withBackend $ \be ->
        liftIO $ Glean.getDatabase be repo
      case Thrift.database_status getDatabaseResult_database of
        Thrift.DatabaseStatus_Complete -> return ()
        _otherwise -> output $ vcat
          [ "WARNING: the current database is writable, so its schema will not"
          , "be updated. To use the new schema, complete the current database"
          , "and restart the shell, or create a new database and load it." ]

loadCmd :: String -> Eval ()
loadCmd str
  | (db : files@(_ : _)) <- args
  , Just repo <- Glean.parseRepo db
  = do load repo files; setRepo repo

  -- Just a file: derive the repo from the filename and pick an unused hash
  | [file] <- args = do
    let name = Text.pack (takeBaseName file)
    hash <- pickHash name
    let repo = Thrift.Repo name hash
    load repo [file]
    setRepo repo

  | otherwise
  = liftIO $ throwIO $ ErrorCall
      "syntax:  :load (<file> | <db>/<hash> <file> ...)"
  where
    args = Data.List.words str

dumpCmd :: String -> Eval ()
dumpCmd str =
  withBackend $ \backend ->
  withRepo $ \repo ->
  liftIO $ Glean.dumpJsonToFile backend repo str

setModeCmd :: String -> Eval ()
setModeCmd "json" = setMode ShellJSON
setModeCmd "angle" = setMode ShellAngle
setModeCmd _ = liftIO $ throwIO $ ErrorCall "syntax: :mode [json|angle]"

editCmd :: Eval ()
editCmd = do
  file <- query_file <$> getState
  meditor <- liftIO getEditor
  case meditor of
    Nothing -> liftIO $ do
      putStrLn "EDITOR not set."
      putStrLn "Set the EDITOR environment variable choose an editor."
    Just editor -> do
      query <- liftIO $ do
        callCommand $ unwords [editor, file]
        readFile file
      void $ evaluate query

getEditor :: IO (Maybe String)
getEditor = asum <$> sequence
  [ lookupEnv "EDITOR"
  , lookupEnv "VISUAL"
  ]

limitCmd :: String -> Eval ()
limitCmd "" = do
  l <- limit <$> getState
  output $ "current limit: " <> pretty l
limitCmd str
  | Right n <- textToInt (Text.pack str) =
    Eval $ State.modify $ \s -> s { limit = fromIntegral n }
  | otherwise = liftIO $ throwIO $ ErrorCall "syntax: :limit <number>"

timeoutCmd :: String -> Eval ()
timeoutCmd "" = do
  t <- timeout <$> getState
  output $ "current query time limit:" <+>
    maybe "not set" (\ms -> pretty ms <> "ms") t
timeoutCmd "off" =
  Eval $ State.modify $ \s -> s { timeout = Nothing }
timeoutCmd str
  | Right n <- textToInt (Text.pack str) =
    Eval $ State.modify $ \s -> s { timeout = Just (fromIntegral n) }
  | otherwise = liftIO $ throwIO $ ErrorCall "syntax: :timeout off|<number>"

countCmd :: String -> Eval ()
countCmd str = do
  ShellState {..} <- getState
  case mode of
    ShellJSON -> run fromJSONQuery
    ShellAngle -> run fromAngleQuery
  where
    run :: Parse query => (query -> Eval SchemaQuery) -> Eval ()
    run from = do
      case runParser parse () "<input>" str of
        Left err -> do
          output $ "*** Syntax error:" <+> pretty (show err)
          return ()
        Right query -> do
          q <- from query
          runUserQuery q { sqOmitResults = True }

statsCmd :: String -> Eval ()
statsCmd "" = do
  s <- stats <$> getState
  output $ "current profile setting: " <>
    case s of
      NoStats -> "off"
      SummaryStats -> "summary"
      FullStats -> "full"
statsCmd str = do
  new <-  case str of
    "off" -> return NoStats
    "summary" -> return SummaryStats
    "full" -> return FullStats
    _ -> liftIO $ throwIO $ ErrorCall "syntax: :stats off|summary|full"
  Eval $ State.modify $ \s -> s { stats = new }


debugCmd :: String -> Eval ()
debugCmd str = case words (strip str) of
  [] -> do
    d <- debug <$> getState
    output $ "query debugging is currently: " <>
      let opts =
            [ "ir" | Thrift.queryDebugOptions_ir d ] ++
            [ "bytecode" | Thrift.queryDebugOptions_bytecode d ]
      in
      if null opts then "off" else hcat (punctuate "," opts)
  ["all"] -> do
    Eval $ State.modify $ \s -> s
      { debug = Thrift.QueryDebugOptions
        { queryDebugOptions_ir = True
        , queryDebugOptions_bytecode = True
        }
      }
  [word] | Just onoff <- irFlag word -> Eval $ State.modify $ \s -> s
    { debug = (debug s) { Thrift.queryDebugOptions_ir = onoff } }
  [word] | Just onoff <- bytecodeFlag word -> Eval $ State.modify $ \s -> s
    { debug = (debug s) { Thrift.queryDebugOptions_bytecode = onoff } }
  ["off"] -> Eval $ State.modify $ \s -> s { debug = def }
  _ -> liftIO $ throwIO $ ErrorCall "syntax: :debug off|[-]ir|[-]bytecode|all"
  where
  irFlag "ir" = Just True
  irFlag "-ir" = Just False
  irFlag _ = Nothing

  bytecodeFlag "bytecode" = Just True
  bytecodeFlag "-bytecode" = Just False
  bytecodeFlag _ = Nothing


expandCmd :: String -> Eval ()
expandCmd str = case str of
  "" -> do
    expand <- expandResults <$> getState
    output $ case expand of
      ExpandRecursive -> "result expansion is on"
      ExpandPredicates ps -> "expanding " <> hsep (map (pretty . showRef) ps)
  "off" -> Eval $ State.modify $ \s -> s { expandResults = ExpandPredicates [] }
  "on" -> Eval $ State.modify $ \s -> s { expandResults = ExpandRecursive }
  other -> Eval $ State.modify $ \s -> s { expandResults = ExpandPredicates $
    map (parseRef . Text.pack) (words other) }

pagerCmd :: String -> Eval ()
pagerCmd str = case str of
  "" -> do
    pagerOn <- pager <$> getState
    output $ "result paging is " <> if pagerOn then "on" else "off"
  "off" -> Eval $ State.modify $ \s -> s { pager = False }
  "on" -> Eval $ State.modify $ \s -> s { pager = True }
  _ -> liftIO $ throwIO $ ErrorCall "syntax: :pager [off|on]"

getExpandResults :: Eval (Bool, [Thrift.SourcePredicate])
getExpandResults =  expandResultsOpts . expandResults <$> getState

expandResultsOpts :: ExpandResults -> (Bool, [Thrift.SourcePredicate])
expandResultsOpts exp = case exp of
  ExpandRecursive -> (True, [])
  ExpandPredicates refs -> (False, ps)
    where
    ps = [ Thrift.SourcePredicate name ver | SourceRef name ver <- refs ]

userFact :: Glean.Fid -> Eval ()
userFact fid = do
  (rec, expandPreds) <- getExpandResults
  Thrift.UserQueryResults{..} <- withRepo $ \repo -> withBackend $ \be ->
    liftIO $ Glean.userQueryFacts be repo $
      def { Thrift.userQueryFacts_facts =
              [def { Thrift.factQuery_id = fromFid fid }]
          , Thrift.userQueryFacts_options = Just def
            { Thrift.userQueryOptions_no_base64_binary = True
            , Thrift.userQueryOptions_expand_results = True
            , Thrift.userQueryOptions_recursive = rec
            , Thrift.userQueryOptions_expand_predicates = expandPreds }
          }
  Thrift.Fact{..} <- withRepo $ \repo -> withBackend $ \be -> do
    r <- liftIO $ Glean.queryFact be repo (fromFid fid)
    case r of
      Nothing -> liftIO $ throwIO $ ErrorCall "cannot fetch fact"
      Just f -> return f
  pref <- lookupPid (Pid fact_type) >>= \case
    Nothing -> liftIO $ throwIO $ ErrorCall "unknown predicate type"
    Just p -> return p
  case userQueryResults_stats of
    Nothing -> return ()
    Just Thrift.UserQueryStats{..} -> output $ pretty
      ( printf "%d facts, %.2fms, %ld bytes"
          userQueryStats_num_facts
          (realToFrac userQueryStats_elapsed_ns / 1000000 :: Double)
          userQueryStats_allocated_bytes
        :: String )
  case JSON.decode (UTF8.toString (head userQueryResults_facts)) of
    JSON.Error err -> output $ pretty err
    JSON.Ok (value :: JSON.JSValue) ->
      output $ pretty pref <> line <> pretty value

asBadQuery :: Thrift.QuerySyntax -> String -> Thrift.BadQuery -> BadQuery
asBadQuery syntax query (Thrift.BadQuery err) =
  if syntax == Thrift.QuerySyntax_ANGLE
     then BadQueryAngle (Text.pack query) err
     else BadQueryJSON err

runUserQuery :: SchemaQuery -> Eval ()
runUserQuery SchemaQuery
    { sqPredicate = str
    , sqRecursive = exp
    , sqStored = stored
    , sqQuery = rest
    , sqCont = cont
    , sqTransform = transform
    , sqSyntax = syntax
    , sqOmitResults = omitResults } = do
  let SourceRef pred maybeVer = parseRef (Text.pack str)
      (recursive, expandPreds) = expandResultsOpts exp
  ShellState{..} <- getState
  schema_id <- getSchemaId
  Thrift.UserQueryResults{..} <- withRepo $ \repo -> withBackend $ \be -> do
    liftIO $ handle (throwIO . asBadQuery syntax rest) $ Glean.userQuery be repo
      def { Thrift.userQuery_predicate = pred
          , Thrift.userQuery_predicate_version = maybeVer
          , Thrift.userQuery_query =
            if syntax == Thrift.QuerySyntax_ANGLE then UTF8.fromString rest else
            if all isSpace rest
              then "{\"get\": {}}"
              else "{\"key\":" <> UTF8.fromString rest <> "}"
          , Thrift.userQuery_options = Just def
             { Thrift.userQueryOptions_no_base64_binary = True
             , Thrift.userQueryOptions_expand_results = True
             , Thrift.userQueryOptions_recursive = recursive
             , Thrift.userQueryOptions_max_results = Just limit
             , Thrift.userQueryOptions_max_time_ms = timeout
             , Thrift.userQueryOptions_continuation = cont
             , Thrift.userQueryOptions_syntax = syntax
             , Thrift.userQueryOptions_store_derived_facts = stored
             , Thrift.userQueryOptions_collect_facts_searched =
                 stats == FullStats
             , Thrift.userQueryOptions_debug = debug
             , Thrift.userQueryOptions_omit_results = omitResults
             , Thrift.userQueryOptions_expand_predicates = expandPreds
             }
          -- When running locally with --enable-logging, logs are emitted
          -- before the ThriftBackend has a chance to incude client_info in the
          -- request.  This makes sure client_info will appear in the logs
          , Thrift.userQuery_client_info = Just client_info
          , Thrift.userQuery_schema_id = schema_id
          }
  output $ vcat $
    [ "*** " <> pretty diag | diag <- userQueryResults_diagnostics ]
    ++
    [ "" | not (null userQueryResults_diagnostics) ]
    ++
    [ case JSON.decode (UTF8.toString fact) of
          JSON.Error err -> pretty err
          JSON.Ok (value :: JSON.JSValue) -> case transform of
            Nothing -> pretty value
            Just t -> case t value of
              JSON.Error err -> pretty err
              JSON.Ok transformed -> pretty transformed
      | fact <- userQueryResults_facts
    ]
    ++
    [ "" ]
    ++
    [ case userQueryResults_stats of
        Nothing -> pretty (length userQueryResults_facts) <+> "results"
        Just Thrift.UserQueryStats{..} ->
          pretty
            ( printf "%d results, %d facts, %.2fms, %ld bytes"
              userQueryStats_result_count
              userQueryStats_num_facts
              (realToFrac userQueryStats_elapsed_ns / 1000000 :: Double)
              userQueryStats_allocated_bytes
            :: String ) <>
          maybe mempty (\n -> "," <+> pretty n <+> "compiled bytes")
            userQueryStats_bytecode_size
    | stats == SummaryStats || stats == FullStats
    ]
    ++
    [ vcat $ "Facts searched:" :
        [ pretty (printf "%40s : %d" (show (pretty ref)) count :: String)
        | (pid, count) <- sortOn (Down . snd) $ Map.toList m
        , Just info <- [schemaInfo]
        , Just ref <- [Map.lookup pid (Thrift.schemaInfo_predicateIds info)] ]
    | stats == FullStats
    , Just stats <- [userQueryResults_stats]
    , Just m <- [Thrift.userQueryStats_facts_searched stats]
    ]
    ++
    [ vcat $ if Thrift.userQueryStats_result_count stats < fromIntegral limit
        then
            [ case timeout of
                Nothing -> "timeout (server-side time limit)"
                Just ms ->
                  "timeout (currently " <> pretty ms <> "ms), " <>
                  "use :timeout <ms> to change it"
           , "Use :more to continue the query." ]
        else
          [ "results truncated (current limit " <> pretty limit <> ", " <>
            "use :limit <n> to change it)"
          , "Use :more to see more results"
          ]
    | isJust userQueryResults_continuation
    , Just stats <- [userQueryResults_stats]
    ]
  Eval $ State.modify $ \s ->
    s { lastSchemaQuery = userQueryResults_continuation <&>
      \cont -> SchemaQuery
        { sqPredicate = str
        , sqRecursive = exp
        , sqStored = stored
        , sqQuery = rest
        , sqCont = Just cont
        , sqTransform = transform
        , sqSyntax = syntax
        , sqOmitResults = omitResults
        }}

-- | A line from the user (or entry on the command line) may end in a backslash
-- and be a 'Cont' continued line, otherwise it is a 'Whole' line, see 'endBS'
data OneLine = Whole String | Cont String

-- | The REPL input may be complete line , or a message to 'Stop'
data GotLines = Stop | Go String

-- | The 'whole' is a list of command that do not end in a backslash (such
-- lines have been combined with spaces).  The 'cont' should be an empty
-- list, but if there are trailing lines with backslashes then they are
-- collected into 'cont' for clear error reporting.
data ManyLines = ManyLines { whole :: [String], cont :: [String]}

-- | Check if line should continue onto the next line
-- Detect (odd number of) ending backslashes (REPL and command line).
-- If found then remove last backslash and return 'Cont' otherwise
-- if the line ends in ';' or '|' return 'Cont' otherwise
-- return input unchanged as 'Whole'
shouldCont :: String -> OneLine
shouldCont s =
  if cont then Cont s' else Whole s
  where
    bs = odd . length . takeWhile ('\\' ==) $ revS
    revS = reverse s
    cont = bs || semi || vert
    semi = ";" `isPrefixOf` revS
    vert = "|" `isPrefixOf` revS
    s' = if bs then init s else s

-- | Parse sequence of lines (from command line) for ending backslashes.
-- This converts zero or more partial Cont lines that end in a Whole line into
-- one element in the 'whole' list in 'ManyLines'.  If there are trailing
-- 'Cont' lines that do not end in a 'Whole' line then return these (as a
-- syntax error) as the 'cont' list of 'ManyLines'
manyWhole :: [String] -> ManyLines
manyWhole = foldl' go (ManyLines [] []) where
  go ml x = case shouldCont x of
    Whole s -> let fullLine = unwords (cont ml ++ [s]) -- pending cont plus s
                   newWhole = whole ml ++ [fullLine] -- append complete line
               in ml{whole = newWhole, cont = []} -- no pending cont lines now
    Cont c -> ml{cont = cont ml ++ [c]} -- collect a pending cont line

-- | Any line ending in an odd number of backslashes means we will
-- continue accepting input on the next line.  Returned string will
-- have multiple input lines separated by spaces, not newlines.
getInputLines :: Repl GotLines
getInputLines = Repl $ getLines prompt1 []
  where
    promptWith suffix = do
      r <- lift getState
      return $ maybe "" (Text.unpack . Thrift.repo_name) (repo r) ++ suffix

    prompt1 = promptWith "> "
    prompt2 = promptWith "| "

    getLines prompt prior = do
      maybeLine <-
        Haskeline.handle (\(e::IOError) -> do
          liftIO $ hPrint stderr e
          return Nothing) $
          Haskeline.getInputLine =<< prompt
      case maybeLine of
        Nothing -> return Stop  -- stop on IOError, ^D or EOF
        Just sIn -> case shouldCont sIn of
          Whole s -> return (Go (intercalate "\n" (reverse (s:prior))))
          Cont c -> getLines prompt2 (c:prior)

repl :: Repl ()
repl = replMask $ \restore ->
  let
    loop = do
      eitherStop <- replTry $ restore $ do
        gotLines <- getInputLines
        case gotLines of
          Stop -> return True  -- stop on ^D or EOF
          Go s
            | all isSpace s -> return False -- ignore blank input
            | otherwise -> fmap (fromMaybe False) $
                Repl $ lift $ withTTY $ evaluate s
      stop <- case eitherStop of
        Right stop -> return stop
        Left e
          | Just UserInterrupt{} <- fromException e -> do
            liftIO $ putStrLn "Interrupted"
            return False
          | Just SomeAsyncException{} <- fromException e -> liftIO $ throwIO e
          | Just (err :: BadQuery) <- fromException e -> do
            Repl $ lift $ output $ prettyBadQuery err
            return False
          | otherwise -> do
            liftIO $ putStrLn $ "*** Exception: " <> show e
            return False
      when (not stop) loop
  in
  loop

replTry :: Exception e => Repl a -> Repl (Either e a)
replTry r = Haskeline.controlIO $ \(Haskeline.RunIO run) -> do
  result <- try (run r)
  case result of
    Left e -> return (return (Left e))
    Right r -> return (Right <$> r)

replMask :: ((Repl a -> Repl a) -> Repl b) -> Repl b
replMask f = Haskeline.controlIO $ \(Haskeline.RunIO run) -> do
  mask $ \restore ->
    run (f (\repl -> Haskeline.controlIO $ \(Haskeline.RunIO run') ->
                restore (run' repl)))

-- | Temporarily install standard signal handlers for catching ^C, which just
-- throw an exception in the current thread.
withSignalHandlers :: Repl a -> Repl a
withSignalHandlers act = do
  main_thread <- liftIO myThreadId
  wtid <- liftIO (mkWeakThreadId main_thread)

  let
    interrupt = do
      r <- deRefWeak wtid
      case r of
        Nothing -> return ()
        Just t  -> do
          interruptRunningQueries
          throwTo t UserInterrupt

    installHandlers = liftIO $ do
      let installHandler' a b = installHandler a b Nothing
      hdlQUIT <- installHandler' sigQUIT  (Catch interrupt)
      hdlINT  <- installHandler' sigINT   (Catch interrupt)
      return (hdlQUIT,hdlINT)

    uninstallHandlers (hdlQUIT,hdlINT) = liftIO $ do
      _ <- installHandler sigQUIT  hdlQUIT Nothing
      _ <- installHandler sigINT   hdlINT  Nothing
      return ()

  Haskeline.bracket installHandlers uninstallHandlers (const act)

completeDatabases :: Haskeline.CompletionFunc Eval
completeDatabases =
  completeWords $ do
    dbs <- getDatabases False ""
    return (map (repoString . Thrift.database_repo) dbs)

completeDatabaseName :: Haskeline.CompletionFunc Eval
completeDatabaseName =
  completeWords $ do
    dbs <- getDatabases False ""
    return $
      uniq (map (Text.unpack . Thrift.repo_name . Thrift.database_repo) dbs) ++
      map (repoString . Thrift.database_repo) dbs

completeWords :: Eval [String] -> Haskeline.CompletionFunc Eval
completeWords words =
  Haskeline.completeWord Nothing " \t" $ \str ->
    fromVocabulary str <$> words

completeUseSchema :: Haskeline.CompletionFunc Eval
completeUseSchema =
  completeWords ((["current", "stored"] <>) <$> availableSchemaIds)

fromVocabulary :: String -> [String] -> [Haskeline.Completion]
fromVocabulary str words =
  map Haskeline.simpleCompletion $ filter (str `isPrefixOf`) words

availableSchemaIds :: Eval [String]
availableSchemaIds = do
  m <- schemaInfo <$> getState
  case m of
    Nothing -> return []
    Just Thrift.SchemaInfo{..} ->
      return $ map Text.unpack $ uniq $
        Map.keys schemaInfo_schemaIds <>
        Map.keys schemaInfo_dbSchemaIds <>
        concatMap Map.keys schemaInfo_otherSchemaIds

getSchemaId :: Eval (Maybe Thrift.SchemaId)
getSchemaId = do
  ShellState{..} <- getState
  return $ do
    ProcessedSchema{..} <- schemas
    case useSchemaId of
      Thrift.SelectSchema_schema_id id -> Just id
      _ -> snd <$> IntMap.lookupMax (hashedSchemaAllVersions procSchemaHashed)

getNameEnv :: Eval (Maybe (NameEnv RefTargetId))
getNameEnv = do
  ShellState{..} <- getState
  case schemas of
    Nothing -> return Nothing
    Just ProcessedSchema{..} -> do
      schema_id <- getSchemaId
      return $ case schema_id of
        Nothing -> Nothing
        Just id -> Map.lookup id (hashedSchemaEnvs procSchemaHashed)

availablePredicates :: Eval [String]
availablePredicates = maybe [] preds <$> getNameEnv
  where
    isPred RefPred{} = True
    isPred _ = False

    preds env =
        [ Text.unpack (showRef ref)
        | (ref, set) <- HashMap.toList env
        , any isPred (Set.toList set) ]

indexCompletion :: Haskeline.CompletionFunc Eval
indexCompletion line@(left,_) =
  case splitWhen isSpace (reverse left) of
    [_cmd, _lang] -> ($line) $ Haskeline.completeWord Nothing " \t" $ \str ->
      let words = [ indexerShortName ix | SomeIndexer ix <- indexers ] in
      return (fromVocabulary str words)
    (_cmd : _lang : rest)
      | not (null rest) -> Haskeline.completeFilename line
    _otherwise -> Haskeline.noCompletion line

availablePredicatesAndTypes :: Eval [String]
availablePredicatesAndTypes = do
  env <- getNameEnv
  let names env = [ Text.unpack (showRef ref) | ref <- HashMap.keys env ]
  return $ maybe [] names env

commandSpecificCompletion :: String -> Haskeline.CompletionFunc Eval
commandSpecificCompletion cmd =
  case [ cmdCompletion | Cmd{..} <- commands, cmd `isPrefixOf` cmdName ] of
    (comp:_) -> comp
    _ -> Haskeline.noCompletion

completeFirstWord :: Haskeline.CompletionFunc Eval
completeFirstWord = completeWords $ (++) <$> availablePredicates <*> pure cmds
  where cmds = map ((":"++) . cmdName) commands

-- Based on GHCi's completion
completion :: Haskeline.CompletionFunc Eval
completion line@(left,_) =
  case firstWord of
    -- if the first word is a command, choose completion based on it:
    ':':cmd | not (null rest) -> commandSpecificCompletion cmd line
    -- otherwise, we only know how to complete the first word (for now):
    _ | not (null rest) -> Haskeline.noCompletion line
    _otherwise -> completeFirstWord line
  where
  (firstWord,rest) = break isSpace $ dropWhile isSpace $ reverse left

reportService :: LocalOrRemote backend => backend -> Eval ()
reportService backend = case backendKind backend of
  BackendEnv Env{..} -> do
    output $  "Using local DBs from " <> pretty (describe envStorage)
  BackendThrift thrift -> do
    case Glean.clientConfig_serv (thriftBackendClientConfig thrift) of
      Tier tier -> output $ "Using service " <> pretty tier
      HostPort host port ->
        output $ "Using service at " <> pretty host <> ":" <> pretty port
      _ -> error "shouldn't happen"

evalMain :: Config -> Eval ()
evalMain cfg = do
  case cfgQuery cfg of
    [] -> do
      hello
      reportService . backend =<< getState
      output "type :help for help."
      initialize cfg
      home <- liftIO $ lookupEnv "HOME"

      let settings = Haskeline.setComplete completion Haskeline.defaultSettings
              { Haskeline.historyFile = fmap (</> ".glean.shell.history") home }
      Haskeline.runInputT settings $ unRepl $ withSignalHandlers repl
    qs@(_:_) -> do
      let ManyLines{whole, cont} = manyWhole qs
      if not (null cont) then liftIO $ do
        putStrLn "Error: Final command(s) end in continuation backslash: "
        when (not (null whole)) $ putStrLn "whole lines:" >> mapM_ print whole
        putStrLn "incomplete cont line(s):" >> mapM_ print cont
      else do
        initialize cfg
        forM_ whole $ \ q -> do
          liftIO $ putStr "> " >> putStrLn q;
          void (evaluate q) `C.catch` \e -> do
            output $ prettyBadQuery e
            liftIO exitFailure

setupLocalSchema :: Glean.Service -> IO (Glean.Service, Maybe (Eval ()))
setupLocalSchema service = do
  case service of
    Remote{} -> return (service, Nothing)
    Local dbConfig logging -> case DB.cfgSchemaDir dbConfig of
      Nothing -> return (service, Nothing)
      Just dir -> do
        schema <- parseSchemaDir dir
          `catch` \(e :: ErrorCall) -> do
            print e
            let proc = ProcessedSchema
                  (SourceSchemas (AngleVersion 0) [] [])
                  (ResolvedSchemas Nothing [])
                  emptyHashedSchema
            return (SchemaIndex proc [])
        (schemaTS, update) <- ThriftSource.mutable schema
        let
          updateSchema :: Eval ()
          updateSchema = do
            be <- backend <$> getState
            case backendKind be of
              BackendThrift{} -> return ()
              BackendEnv env -> do
                new <- liftIO $ parseSchemaDir dir
                liftIO $ update (const new)
                let current = schemaIndexCurrent new

                -- Update all the schemas for open DBs. This would
                -- normally be done in the background by the schema
                -- updater thread, but we're doing it manually and
                -- disabling the auto-update so that we can
                -- synchronously check for errors and update our local
                -- view of the schema in the monad.
                liftIO $ schemaUpdated env Nothing

                state <- getState
                whenJust (repo state) $ \r -> do
                  info <- liftIO $
                    Glean.getSchemaInfo env r
                      def { Thrift.getSchemaInfo_select = useSchemaId state }

                  Eval $ State.modify $ \s ->
                    s { schemaInfo = Just info, schemas = Just current }

                let
                  numSchemas = length (srcSchemas (procSchemaSource current))
                  numPredicates = HashMap.size (hashedPreds
                    (procSchemaHashed current))
                output $ "reloading schema [" <>
                  pretty numSchemas <> " schemas, " <>
                  pretty numPredicates <> " predicates]"

          -- When using --schema, we also set --db-schema-override. This
          -- allows the local schema to override whatever was in the DB,
          -- and also allows the local schema to take effect when the
          -- DB is writable.
          dbConfig' = dbConfig {
            DB.cfgSchemaSource = schemaTS,
            DB.cfgUpdateSchema = False
          }

        return
          ( Local dbConfig' logging
          , Just updateSchema
          )

type ShellCommand = Config

instance Plugin ShellCommand where
  parseCommand = options

  argTransform _ args
    | "-v" `elem` args = args
    | otherwise = "--minloglevel=2" : args

  withService evb cfgAPI service cfg = do
    (service', updateSchema) <- setupLocalSchema service
    Glean.withBackendWithDefaultOptions evb cfgAPI
      service' Nothing $ \backend -> do
    withSystemTempFile "scratch-query.angle" $ \q handle -> do
      hClose handle
      client_info <- clientInfo
      tty <- hIsTerminalDevice stdout
      outh <- newMVar stdout
      State.evalStateT (unEval $ evalMain cfg) ShellState
        { backend = Some backend
        , repo = Nothing
        , mode = cfgMode cfg
        , schemas = Nothing
        , schemaInfo = def
        , useSchemaId = Thrift.SelectSchema_current def
        , limit = cfgLimit cfg
        , timeout = Just 10000      -- Sensible default for fresh shell.
        , stats = SummaryStats
        , lastSchemaQuery = Nothing
        , updateSchema = updateSchema
        , isTTY = tty
        , pageWidth =
            (\n -> if n == 0 then Unbounded else AvailablePerLine n 1)
            <$> cfgWidth cfg
        , expandResults = ExpandRecursive
        , outputHandle = outh
        , pager = cfgPager cfg
        , debug = def
        , client_info = client_info
        , query_file = q
        }
