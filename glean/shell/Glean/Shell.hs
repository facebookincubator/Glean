{-# LANGUAGE ApplicativeDo, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Shell (main)
where

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
import Data.IORef
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Text.Printf
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc as Pretty hiding ((<>), pageWidth)
import Data.Text.Prettyprint.Doc.Util as Pretty hiding (words)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Options.Applicative as O
import Options.Applicative hiding (help)
import Util.Timing
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
import TextShow

import Util.EventBase
import Util.JSON.Pretty ()
import Util.List
import Util.String
import Util.Text
import Util.TimeSec

import qualified Glean hiding (options)
import qualified Glean.BuildInfo as BuildInfo
import Glean.Backend.Remote (clientInfo, StackedDbOpts(..))
import Glean.Database.Schema.Types (DbSchema(..))
import Glean.Database.Schema (newDbSchema, readWriteContent)
import Glean.Database.Config (parseSchemaDir)
import qualified Glean.Database.Config as DB (Config(..))
import Glean.Init
import Glean.LocalOrRemote (Service(..), withBackendWithDefaultOptions)
import qualified Glean.LocalOrRemote as Glean
import Glean.RTS.Types (Pid(..), Fid(..))
import Glean.RTS.Foreign.Query (interruptRunningQueries)
import Glean.Schema.Resolve
import Glean.Angle.Types as SchemaTypes
import Glean.Schema.Util
import Glean.Shell.Index
import Glean.Shell.Terminal
import Glean.Shell.Types
import Glean.Shell.Error (Ann, BadQuery(..), prettyBadQuery)
import qualified Glean.Types as Thrift
import Glean.Util.ConfigProvider
#if FACEBOOK
import Glean.Util.CxxXRef
#endif
import Glean.Util.ShellPrint
import Glean.Util.Some
import qualified Glean.Util.ThriftSource as ThriftSource

data Config = Config
  { cfgService :: Service
  , cfgDatabase :: Maybe String
  , cfgQuery :: [String]
  , cfgMode :: ShellMode
  , cfgLimit :: Int64
  , cfgWidth :: Maybe Int
  , cfgPager :: Bool
  , cfgVerbose :: Maybe Int
  }

options :: ConfigProvider cfg => ParserInfo (Config, ConfigOptions cfg)
options = info (O.helper <*> liftA2 (,) parser configOptions) fullDesc
  where
    parser :: Parser Config
    parser = do
      cfgService <- Glean.options
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
      cfgVerbose <- optional $ option auto
        (  long "verbose"
        <> short 'v'
        <> metavar "N"
        <> O.help "Enbable debug logging at level N"
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
  Thrift.SchemaInfo{..} <- schemaInfo <$> getState
  return $ Map.lookup pid schemaInfo_predicateIds

withRepo :: (Thrift.Repo -> Eval a) -> Eval a
withRepo f = do
  r <- getRepo
  case r of
    Just repo -> f repo
    Nothing -> liftIO $ throwIO $ ErrorCall "no database selected"

getSchemaCmd :: String -> Eval ()
getSchemaCmd str = do
  Schemas{..} <- schemas <$> getState
  let
    txt = Text.pack (strip str)

    (predicate_matches, typedef_matches)
      | Text.null txt = (const True, const False)
      | otherwise =
          let SourceRef match_name match_maybeVer = parseRef txt
              ref_matches name ver =
                name == match_name && maybe True (== ver) match_maybeVer
          in
          ( \(PredicateRef name ver) -> ref_matches name ver
          , \(TypeRef name ver) -> ref_matches name ver )

    preds =
      [ pred
      | (ref, pred) <-
          concatMap (HashMap.toList . resolvedSchemaPredicates) schemasResolved
      , predicate_matches ref ]

    types =
      [ typedef
      | (ref, typedef) <-
          concatMap (HashMap.toList . resolvedSchemaTypes) schemasResolved
      , typedef_matches ref ]

  output $ if null types && null preds
    then "*** nothing found"
    else mconcat
      $ map (<> line <> line)
      $ map pretty types ++ map pretty preds

displayStatistics :: String -> Eval ()
displayStatistics arg =
  withRepo $ \repo ->
  withBackend $ \backend -> do
  xs <- liftIO $ Glean.predicateStats backend repo ExcludeBase
  preds <- forM (Map.toList xs) $ \(id,stats) -> do
    ref <- maybe (Left id) Right <$> lookupPid (Pid id)
    return (ref,stats)
  let
    totalSizeBytes = foldl' (+) 0 $ map (Thrift.predicateStats_size . snd) preds
  output $ vsep
    [ nest 2 $ vsep
        [ case ref of
            Left id -> pretty id
            Right pref -> pretty pref
        , "count:" <+> pretty (Thrift.predicateStats_count stats)
        , "size: " <+> pretty (getSizeInfo
             (Thrift.predicateStats_size stats) totalSizeBytes)
        ]
      | (ref,stats) <- sortOn fst preds
      , null arg ||
          case ref of
            Right pref -> arg `isPrefixOf` show (pretty pref)
            Left _ -> False
    ]

  when (null arg) $
    output $ vcat [ "", "Total size: " <> pretty (showAllocs totalSizeBytes) ]

getSizeInfo :: Int64 -> Int64 -> String
getSizeInfo bytes total =
  printf "%d (%s) %.4f%%" bytes humanReadableSize percentage_x
    where
      percentage_x :: Double
      percentage_x = 100 * fromIntegral bytes / fromIntegral total
      humanReadableSize = showAllocs bytes

getDatabases
  :: Bool -- ^ Include DBs that can be restored from backups
  -> String -- ^ Only display dbs with a repo name containing this string
  -> Eval [Thrift.Database]
getDatabases all filterStr = do
  r <- withBackend $ \be ->
    liftIO $ Glean.listDatabases be
      def { Thrift.listDatabases_includeBackups = all }
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
  now <- liftIO $ utcTimeToPOSIXSeconds <$> getCurrentTime
  state <- getState
  outh <- liftIO $ readMVar $ outputHandle state
  liftIO $ forM_ (sortOn Thrift.database_created_since_epoch dbs) $ \db -> do
    let t0 = Time (round now)
    let format = if isTTY state then TTY else PlainText
    hPutStrLn outh $ shellPrint verbose format t0 db
    hPutStrLn outh ""


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
      [ ("database [<name>]",
            "Use database <name>")
      , ("index <lang> <dir>",
            "Index source files in <dir> and create a database. <lang> supports "
            <> "only flow currently.")
      , ("list [<reponame>]",
            "List available databases which match <reponame>")
      , ("list-all [<reponame>]",
            "List available databases, and restorable backups, which match "
            <> "<reponame>")
      , ("debug off|[-]ir|[-]bytecode|all",
            "Enable/disable query debugging options")
      , ("describe [<reponame>]",
            "Like :list, but show more details")
      , ("describe-all [<reponame>]",
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
      , ("timeout off|<n>",
            "Set the query time budget")
      , ("count <query>",
            "Show only a count of query results, not the results themselves")
      , ("more",
            "Fetch more results from the previous query")
      , ("profile [off|summary|full]",
            "Show query profiling information")
      , ("reload",
            "Reload the schema (when using --schema)")
      , ("statistics [<predicate>]",
            "Show statistics for the current database")
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
  , "  !{1234}                   Look up a fact and dereference subterms"
  , "  <predicate> [!] <pat>     Query a predicate for facts matching <pat>"
  , "                            ('!' means recursively expand nested facts)"
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
  , "  !{1234}                                      fetch a fact and expand"
  , "  pp1.Define                                   all the pp1.Define facts"
  , "  pp1.Define { \"macro\": { \"key\": \"NULL\" } }    every #define of NULL"
  , "  pp1.Define ! { \"macro\": { \"key\": \"NULL\" } }  with contents expanded"
  ]

helpAngle :: Doc ann
helpAngle = vcat
  [ "Queries (angle mode):"
  , "  {1234}                    Look up a fact by its Id"
  , "  !{1234}                   Look up a fact and dereference subterms"
  , "  [!] <predicate> <pat>     Query a predicate for facts matching <pat>"
  , "                            ('!' means recursively expand nested facts)"
  , ""
  , "Pattern syntax (angle mode):"
  , "  1234                     :: byte or nat"
  , "  \"abc\"                    :: string"
  , "  \"abc\"..                  :: string prefix match"
  , "  true|false               :: bool"
  , "  [ val, ... ]             :: array(T)"
  , "  { field = val, ... }     :: record(fields), omitted fields are wild"
  , "  { field = val }          :: sum(fields)"
  , ""
  , "Please consult the documentation for the full query syntax."
  , ""
  , "Examples:"
  , "  {1234}                                   fetch a fact by its Id"
  , "  !{1234}                                  fetch a fact and expand"
  , "  pp1.Define _                             all the pp1.Define facts"
  , "  pp1.Define { macro = \"NULL\" }            every #define of NULL"
  , "  !pp1.Define { macro = \"NULL\" }           with contents expanded"
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
        width <- fromMaybe 80 <$> liftIO getWidth
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
doJSONStmt (FactRef bang fid) = userFact bang fid >> return False
doJSONStmt (Pattern query) = runUserQuery (fromJSONQuery query) >> return False

fromJSONQuery :: JSONQuery -> SchemaQuery
fromJSONQuery (JSONQuery ide rec stored pat) = SchemaQuery
    { sqPredicate = pred
    , sqRecursive = rec
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
#if FACEBOOK
      | ide == "xrefs" = ("cxx1.FileXRefs", Just transformXRefs)
#endif
      | otherwise = (ide, Nothing)

doAngleStmt :: Statement AngleQuery -> Eval Bool
doAngleStmt (Command name arg) = doCmd name arg
doAngleStmt (FactRef bang fid) = userFact bang fid >> return False
doAngleStmt (Pattern query)
  = runUserQuery (fromAngleQuery query) >> return False

fromAngleQuery :: AngleQuery -> SchemaQuery
fromAngleQuery (AngleQuery rec stored pat) = SchemaQuery
  { sqPredicate = ""
  , sqRecursive = rec
  , sqStored = stored
  , sqQuery = pat
  , sqCont = Nothing
  , sqTransform = Nothing
  , sqSyntax = Thrift.QuerySyntax_ANGLE
  , sqOmitResults = False }

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
  , Cmd "profile" (completeWords (pure ["off","summary","full"])) $
      \str _ -> statsCmd str
  , Cmd "timeout" Haskeline.noCompletion $ \str _ -> timeoutCmd str
  , Cmd "count" Haskeline.noCompletion $ \str _ -> countCmd str
  , Cmd "!restore" Haskeline.noCompletion $ const . restoreDatabase
  , Cmd "!kickoff" Haskeline.noCompletion $ const . kickOff
  , Cmd "!delete" completeDatabases $ const . deleteDatabase
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
        Just Thrift.DatabaseStatus_Complete -> return ()
        _otherwise -> output $ vcat
          [ "WARNING: the current database is writable, so its schema will not"
          , "be updated. To use the new schema, complete the current database"
          , "and restart the shell, or create a new database and load it." ]

loadCmd :: String -> Eval ()
loadCmd str
  | [db, file] <- args
  , Just repo <- Glean.parseRepo db
  = do load repo [file]; setRepo repo

  -- Just a file: derive the repo from the filename and pick an unused hash
  | [file] <- args = do
    let name = Text.pack (takeBaseName file)
    hash <- pickHash name
    let repo = Thrift.Repo name hash
    load repo [file]
    setRepo repo

  | otherwise
  = liftIO $ throwIO $ ErrorCall "syntax:  :load [<db>/<hash>] <file>"
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
    run :: Parse query => (query -> SchemaQuery) -> Eval ()
    run from =
      case runParser parse () "<input>" str of
        Left err -> do
          output $ "*** Syntax error:" <+> pretty (show err)
          return ()
        Right query -> runUserQuery (from query) { sqOmitResults = True }

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

userFact :: Bool -> Glean.Fid -> Eval ()
userFact bang fid = do
  Thrift.UserQueryResults{..} <- withRepo $ \repo -> withBackend $ \be ->
    liftIO $ Glean.userQueryFacts be repo $
      def { Thrift.userQueryFacts_facts =
              [def { Thrift.factQuery_id = fromFid fid }]
          , Thrift.userQueryFacts_options = Just def
            { Thrift.userQueryOptions_no_base64_binary = True
            , Thrift.userQueryOptions_expand_results = True
            , Thrift.userQueryOptions_recursive = bang }
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
    , sqRecursive = recursive
    , sqStored = stored
    , sqQuery = rest
    , sqCont = cont
    , sqTransform = transform
    , sqSyntax = syntax
    , sqOmitResults = omitResults } = do
  let SourceRef pred maybeVer = parseRef (Text.pack str)
  ShellState{..} <- getState
  Thrift.UserQueryResults{..} <- withRepo $ \repo -> withBackend $ \be -> do
    liftIO $ handle (throwIO . asBadQuery syntax rest) $ Glean.userQuery be repo
      def { Thrift.userQuery_predicate = pred
          , Thrift.userQuery_predicate_version = maybeVer
          , Thrift.userQuery_query =
            if syntax == Thrift.QuerySyntax_ANGLE then UTF8.fromString rest else
            if null (filter (not.isSpace) rest)
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
             }
          -- When running locally with --enable-logging, logs are emitted
          -- before the ThriftBackend has a chance to incude client_info in the
          -- request.  This makes sure client_info will appear in the logs
          , Thrift.userQuery_client_info = Just client_info
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
        let
          pidMap = Thrift.schemaInfo_predicateIds schemaInfo
        in
        [ pretty (printf "%40s : %d" (show (pretty ref)) count :: String)
        | (pid, count) <- sortOn (Down . snd) $ Map.toList m
        , Just ref <- [Map.lookup pid pidMap] ]
    | stats == FullStats
    , Just stats <- [userQueryResults_stats]
    , Just m <- [Thrift.userQueryStats_facts_searched stats]
    ]
    ++
    if isJust userQueryResults_continuation
      then
        if length userQueryResults_facts < fromIntegral limit
          then
            [ case timeout of
                Nothing -> "timeout (server-side time limit)"
                Just ms ->
                  "timeout (currently " <> pretty ms <> "ms), " <>
                  "use :timeout <ms> to change it"
            , "Use :more to continue the query."
            ]
          else
            [ "results truncated (current limit " <> pretty limit <> ", " <>
              "use :limit <n> to change it)"
            , "Use :more to see more results"
            ]
      else []
  Eval $ State.modify $ \s ->
    s { lastSchemaQuery = userQueryResults_continuation <&>
      \cont -> SchemaQuery
        { sqPredicate = str
        , sqRecursive = recursive
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

-- | Detect (odd number of) ending backslashes (REPL and command line).
-- If found then remove last backslash and return 'Cont' otherwise
-- return input unchanged as 'Whole'
endBS :: String -> OneLine
endBS s = let bs = odd . length . takeWhile ('\\' ==) . reverse $ s
          in if bs then Cont (init s)
             else Whole s

-- | Parse sequence of lines (from command line) for ending backslashes.
-- This converts zero or more partial Cont lines that end in a Whole line into
-- one element in the 'whole' list in 'ManyLines'.  If there are trailing
-- 'Cont' lines that do not end in a 'Whole' line then return these (as a
-- syntax error) as the 'cont' list of 'ManyLines'
manyWhole :: [String] -> ManyLines
manyWhole = foldl' go (ManyLines [] []) where
  go ml x = case endBS x of
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
      maybeLine <- Haskeline.getInputLine =<< prompt
      case maybeLine of
        Nothing -> return Stop  -- stop on ^D or EOF
        Just sIn -> case endBS sIn of
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

  Haskeline.bracket installHandlers uninstallHandlers (\_ -> act)

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

fromVocabulary :: String -> [String] -> [Haskeline.Completion]
fromVocabulary str words =
  map Haskeline.simpleCompletion $ filter (str `isPrefixOf`) words

availablePredicates :: Eval [String]
availablePredicates = do
  Schemas{..} <- schemas <$> getState
  let
    refs = concatMap (HashMap.keys . resolvedSchemaPredicates) schemasResolved
    withVer =
      [ predicateRef_name <> "." <> showt predicateRef_version
      | PredicateRef{..} <- refs ]
    noVer = map predicateRef_name refs
  return $ map Text.unpack $ sort noVer ++ sort withVer

availableTypes :: Eval [String]
availableTypes = do
  Schemas{..} <- schemas <$> getState
  let
    refs = concatMap (HashMap.keys . resolvedSchemaTypes) schemasResolved
    withVer =
      [ typeRef_name <> "." <> showt typeRef_version
      | TypeRef{..} <- refs ]
    noVer = map typeRef_name refs
  return $ map Text.unpack $ sort noVer ++ sort withVer

indexCompletion :: Haskeline.CompletionFunc Eval
indexCompletion line@(left,_) =
  case splitWhen isSpace (reverse left) of
    [_cmd, _lang] -> ($line) $ Haskeline.completeWord Nothing " \t" $ \str ->
      return (fromVocabulary str ["flow"])
    (_cmd : _lang : rest)
      | not (null rest) -> Haskeline.completeFilename line
    _otherwise -> Haskeline.noCompletion line

availablePredicatesAndTypes :: Eval [String]
availablePredicatesAndTypes = (++) <$> availablePredicates <*> availableTypes

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

evalMain :: Config -> Eval ()
evalMain cfg = do
  case cfgQuery cfg of
    [] -> do
      hello
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

setupLocalSchema :: Config -> IO (Config, Maybe (Eval ()))
setupLocalSchema cfg = do
  case cfgService cfg of
    Remote{} -> return (cfg, Nothing)
    Local dbConfig logging -> case DB.cfgSchemaDir dbConfig of
      Nothing -> return (cfg, Nothing)
      Just dir -> do
        schema <- parseSchemaDir dir
          `catch` \(e :: ErrorCall) -> do
            print e
            return $ (SourceSchemas 0 [], Schemas HashMap.empty 0 [])
        (schemaTS, update) <- ThriftSource.mutable schema
        let
          updateSchema :: Eval ()
          updateSchema = do
            new@(source,resolved) <- liftIO $ parseSchemaDir dir
            -- convert to a DbSchema, because this forces
            -- typechecking of the derived predicates. Otherwise we
            -- won't notice type errors until after the schema is
            -- updated below.
            db <- liftIO $ newDbSchema source resolved readWriteContent
            liftIO $ update (const new)
            let
              numSchemas = length (srcSchemas (schemaSource db))
              numPredicates = HashMap.size (predicatesByRef db)
            output $ "reloading schema [" <>
              pretty numSchemas <> " schemas, " <>
              pretty numPredicates <> " predicates]"

          -- When using --schema, we also set --db-schema-override. This
          -- allows the local schema to override whatever was in the DB,
          -- and also allows the local schema to take effect when the
          -- DB is writable.
          dbConfig' = dbConfig
            { DB.cfgSchemaSource = schemaTS
            , DB.cfgSchemaOverride = True }

        return
          ( cfg { cfgService = Local dbConfig' logging }
          , Just updateSchema
          )

main :: IO ()
main = do
  (cfg, cfgOpts) <- execParser options
  glog_v <- lookupEnv "GLOG_v"
  let
    gflags
      | Just n <- cfgVerbose cfg = ["--v=" <> show n]
      | isNothing glog_v = ["--minloglevel=2"]
        -- default: warnings and above only
      | otherwise = []
  withGflags gflags $ do
  withEventBaseDataplane $ \evb ->
    withConfigProvider cfgOpts $ \cfgAPI -> do
      (cfg, updateSchema) <- setupLocalSchema cfg
      withBackendWithDefaultOptions evb cfgAPI (cfgService cfg) $ \be -> do
      withSystemTempFile "scratch-query.angle" $ \q handle -> do
        hClose handle
        client_info <- clientInfo
        tty <- hIsTerminalDevice stdout
        outh <- newMVar stdout
        State.evalStateT (unEval $ evalMain cfg) ShellState
          { backend = Some be
          , repo = Nothing
          , mode = cfgMode cfg
          , schemas = Schemas HashMap.empty 0 []
          , schemaInfo = def
          , limit = cfgLimit cfg
          , timeout = Just 10000      -- Sensible default for fresh shell.
          , stats = SummaryStats
          , lastSchemaQuery = Nothing
          , updateSchema = updateSchema
          , isTTY = tty
          , pageWidth =
              (\n -> if n == 0 then Unbounded else AvailablePerLine n 1)
              <$> cfgWidth cfg
          , outputHandle = outh
          , pager = cfgPager cfg
          , debug = def
          , client_info = client_info
          , query_file = q
          }
