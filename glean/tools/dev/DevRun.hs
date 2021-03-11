{-# LANGUAGE ApplicativeDo #-}
module DevRun(main) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Char
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import Options.Applicative ((<|>))
import qualified Options.Applicative as O
import qualified ScUtil
import System.Directory
import System.Environment (getEnv, getEnvironment)
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Timeout

-- | It is sometime more convenient to specify the diff number, and sometimes
-- more convenient to specify the full fbsource hash.
data Build = BuildDiff String | BuildHash String

data DevRunOpts = DevRunOpts
  { build :: Build
  , region :: String
  , repo :: String
  , pickHash :: Maybe String
  , configHash :: Maybe String
  , pickRecipes :: Maybe String
  , requireTasks :: Int
  , dbName :: Maybe String
  , stacked :: Maybe String
  , fbcodeDir :: Maybe FilePath
  , mustUpdate :: Bool -- ^ True to force a tw update
  }

devRunOpts :: O.ParserInfo DevRunOpts
devRunOpts = O.info (O.helper <*> parser) O.fullDesc
  where
    diffOpt = O.strOption $ O.long "diff" <> O.metavar "DIFF_NUMBER"
        <> O.help ("Diff number to build a clang.indexer from, "
        <> "gets latest commmit hash")

    buildHashOpt = O.strOption $ O.long "build"
      <> O.metavar "COMMIT_HASH"
      <> O.help ("Hash for commit to build a clang.indexer from "
      <> "(instead of diff number)")

    parser = do
      build <- (BuildHash <$> buildHashOpt) <|> (BuildDiff <$> diffOpt)
      region <- O.strOption $ O.long "region" <> O.metavar "REGION"
        <> O.help ("Logical glean.clang.indexer.dev cluster "
        <> "like odn, odn-2, or frc")
      repo <- O.strOption $ O.long "repo" <> O.metavar "REPOSITORY"
        <> O.value "fbsource"
        <> O.help "Repository scm name (default fbsource) to index"
      pickHash <- O.optional $ O.strOption $ O.long "hash"
        <> O.metavar "REPO_HASH"
        <> O.help "Hash of the repository to index"
      configHash <- O.optional $ O.strOption $ O.long "config"
        <> O.metavar "REV"
        <> O.help ("rev (or diff) of ~/configerator to canary on " <>
          "the server and indexer tiers (passed to \"hg up\")")
      pickRecipes <- O.optional $ O.strOption $ O.long "recipes"
        <> O.metavar "RECIPES"
        <> O.help "The --recipes parameter for coordinator for new database"
      requireTasks <- O.option O.auto $ O.long "tasks"
        <> O.metavar "TASK_COUNT"
        <> O.help "Minimum # of ready tupperware tasks to wait for (default 4)"
        <> O.value 4
      dbName <- O.optional $ O.strOption $ O.long "db"
        <> O.metavar "NAME/HASH"
        <> O.help "database name to create (default fbsource/revision)"
      stacked <- O.optional $ O.strOption $ O.long "stacked"
        <> O.metavar "NAME/HASH"
        <> O.help "base for creating stacked database"
      fbcodeDir <- O.optional $ O.strOption $ O.long "fbcode"
        <> O.metavar "DIR"
        <> O.help  "Directory for fbsource/fbcode (for tupperware files)"
      mustUpdate <- O.switch $ O.long "tw-update"
        <> O.help "force a tw update (to always pick up changes in tw file)"
      return DevRunOpts{..}

-- | Find an fbpkg for a particular hg hash.
findFbpkg :: String -> String -> IO (Maybe String)
findFbpkg name hash = parse <$> readProcess
  "fbpkg"
  ["find-builds", "--all", "--package", name, hash]
  ""
  where
    parse out = listToMaybe
      [ver
        | _:pkg:_ <- map words $ lines out
        , Just ver <- [stripPrefix (name ++ ":") pkg]]

configeratorCanary :: Maybe String -> [String] -> IO x -> IO x
configeratorCanary _ [] act = act
configeratorCanary Nothing _ act = act
configeratorCanary (Just hash) tiers actIn = do
  cwd <- getCurrentDirectory
  home <- getHomeDirectory
  let configerator = home </> "configerator"
  withCurrentDirectory configerator $ do
    hgUp
    onTiers tiers $ withCurrentDirectory cwd $ do
      say "Giving the tiers time to update from configerator"
      threadDelay (1000000 * 60)
      actIn
  where
    hgUp = do
      say ("Checking out configerator : " <> hash)
      (exit, out, err) <- readProcessWithExitCode "hg" ["up", hash] ""
      case exit of
        ExitSuccess -> say ("Checked out configerator : " <> hash)
        _ -> do
          say (show exit)
          say out
          say err
          die ("Could not checkout configerator : " <> hash)

    onTiers [] act = act
    onTiers (tier:tiers) act = bracket canary cancel (\ _ -> onTiers tiers act)
      where
        canary = do
          say ("Starting canary on " <> tier)
          (exit, out, err) <- readProcessWithExitCode
            "conf"
            ["canary", "start", "--ttl", secondsInOneDay, "--smc-tier", tier]
            ""
          case exit of
            ExitSuccess | Just canaryId <- parse out -> do
              say ("Started canary on " <> tier <> " : " <> canaryId)
              return (tier, canaryId)
            _ -> do
              say (show exit)
              say out
              say err
              die ("Can not start canary on tier " <> tier
                <> ". Manually cancel any existing canaries if there are any")

        cancel (tier, canaryId) = do
          say ("Cancelling canary on " <> tier <> " : " <> canaryId)
          (exit, out, err) <- readProcessWithExitCode
            "conf"
            ["canary", "cancel", canaryId]
            ""
          case exit of
            ExitSuccess ->
              say ("Cancelled canary on " <> tier <> " : " <> canaryId)
            _ -> do
              say (show exit)
              say out
              say err
              say ("Failed to cancel canary on " <> tier <> " : " <> canaryId)

    secondsInOneDay = show (60 * 60 * 24 :: Int)

    parse = listToMaybe . mapMaybe getId . lines

    getId line = case words line of
      ("|" : canaryId : "|" : _) | all valid canaryId -> Just canaryId
      ("Canary" : "ID:" : canaryId : _) | all valid canaryId -> Just canaryId
      _ -> Nothing

    valid :: Char -> Bool
    valid = (`elem` ("0123456789_" :: String))


-- | Build an fbpkg if it doesn't exist yet.
buildFbpkg :: String -> String -> IO String
buildFbpkg name hash = do
  say $ "Checking if " ++ name ++ " exists"
  r <- findFbpkg name hash
  case r of
    Just ver -> do
      say $ "Found " ++ name ++ ':' : ver
      return ver
    Nothing -> do
      say $ "Building " ++ name
      user <- getEnv "USER"
      -- read json job definiiton and substitute variables (prefixed by $$)
      tpl <- readFile "glean/tools/dev/build-fbpkg.json"
      let vars = HashMap.fromList
            [ ("USER", user)
            , ("ALIAS",
                "glean-dev-" ++ map (\c -> if c == '.' then '_' else c) name)
            , ("FBPKG", name)
            , ("HASH", hash) ]
      -- create Sandcastle job
      maybeCreateJob <- ScUtil.createJobWithSubstitutions tpl vars
      (url, scid) <- either
        (\err -> die $ "Job creation failed: " <> err)
        (\info -> return
          (ScUtil.jobInfo_url info, ScUtil.jobInfo_id info))
        maybeCreateJob
      say $ "Sandcastle URL for " ++ show name ++ " is " ++ url
      -- wait for Sandcastle job to finish
      maybeWait <- ScUtil.awaitJob scid Nothing
      either
        (\err -> die ("Building " ++ name ++ " failed: " ++ err))
        return
        maybeWait
      say $ "Verifying " ++ name
      r <- findFbpkg name hash
      case r of
        Just ver -> do
          say $ "Verified " ++ name ++ ':':ver
          return ver
        Nothing -> die $ name ++ " is missing after building"

-- | Download an fbpkg to /tmp/glean-dev - we use it to get the right version
-- of glean.clang.coordinator
downloadFbpkg :: String -> String -> IO FilePath
downloadFbpkg pkg ver = do
  say $ "Checking if " ++ fbpkg ++ " exists locally"
  ex <- checkExists
  if ex
    then say $ "Reusing " ++ path
    else do
      say $ "Downloading " ++ fbpkg ++ " to " ++ path
      createDirectoryIfMissing True path
      callProcess "fbpkg" ["fetch","-d",path,fbpkg]
      ex <- checkExists
      if ex
        then say $ "Downloaded " ++ fbpkg ++ " to " ++ path
        else die $ fbpkg ++ " not found after downloading to " ++ path
  return path
  where
    fbpkg = pkg ++ ':' : ver
    path = "/tmp/glean-dev" </> pkg </> ver

    checkExists = doesFileExist $ path </> fbpkg <.> "CHECKSUMS"

data TWJob = TWJob
  { twFbpkg :: String
  , twSpec :: FilePath
  , twName :: String
  }

indexerJob :: TWJob
indexerJob = TWJob
  { twFbpkg = "glean.clang.indexer"
  , twName = "glean.clang.indexer"
  , twSpec = "clang.indexer.tw"
  }

serverJob :: TWJob
serverJob = TWJob
  { twFbpkg = "glean.server"
  , twName = "glean.write"
  , twSpec = "server.tw"
  }

jobHandle :: TWJob -> String -> String
jobHandle TWJob{..} region = concat
  [ "tsp_"
  , take 3 region
  , "/code_indexing/"
  , twName
  , ".dev."
  , region ]

-- | Check if we need to update a tw job.
twCheck :: TWJob -> String -> String -> IO Bool
twCheck job region version = do
  (code, out, err) <- readProcessWithExitCode
    "tw"
    ["print", jobHandle job region]
    ""
  case code of
    ExitSuccess -> return $ any matches $ lines out
    ExitFailure _ -> do
      hPutStr stderr err
      return False
  where
    matches =
      isPrefixOf ("hash='" ++ twFbpkg job ++ ':' : version)
      . dropWhile isSpace

twUpdate :: Bool -> TWJob -> String -> String -> IO Bool
twUpdate mustUpdate job region version = do
  say $ "Checking if " ++ handle ++ " needs an update"
  ok <- if mustUpdate then return False else twCheck job region version
  if ok
    then do
      say $ "Not updating " ++ handle
      return False
    else do
      say $ "Updating " ++ handle
      vars <- getEnvironment
      code <- withCreateProcess
        (proc
          "tw"
          [ "update"
          , "--fast"
          , "--force"
          , "tupperware/config/glean/" ++ twSpec job
          , handle ])
          { env = Just $ ("FBPKG", version) : vars } $ \_ _ _ h ->
          waitForProcess h
      case code of
        ExitSuccess -> return True
        ExitFailure _ -> die $ "couldn't tw update " ++ handle
  where
    handle = jobHandle job region

say :: String -> IO ()
say s = do
  putStrLn s
  hFlush stdout

checkFbcode :: Maybe FilePath -> IO () -> IO ()
checkFbcode (Just dir) act = withCurrentDirectory dir act
checkFbcode Nothing act = do
  x <- getCurrentDirectory
  if not (isSuffixOf "/fbsource/fbcode" x) then do
    putStrLn "You much execute devrun from /fbsource/fbcode"
    putStrLn $ "Current directory is: " <> x
    exitWith (ExitFailure 1)
  else
    act

getBuildHash :: Build -> IO String
getBuildHash build = case build of
  BuildHash x -> do
    say $ "Building with Hash " ++ x
    return x
  BuildDiff diff -> do
    say $ "Getting " ++ diff
    -- Ignoring the result of jf get because it fails for diffs which already
    -- landed and if it fails for other reasons, hg log will fail later.
    _ <- readProcessWithExitCode "jf" ["get", diff] ""
    say $ "Obtaining hash for " ++ diff
    hg_out <- readProcess "hg" ["log", "-r", diff, "--template", "{node}"] ""
    let x = takeWhile (not . isSpace) hg_out
    if null x
      then die "couldn't obtain hash from hg"
      else do
        say $ "Hash for " ++ diff ++ " is " ++ x
        return x

main :: IO ()
main = do
  DevRunOpts{..} <- O.execParser devRunOpts
  checkFbcode fbcodeDir $ do
  buildHash <- getBuildHash build
  [server_ver, indexer_ver, coordinator_ver] <-
    Async.forConcurrently
      ["glean.server", "glean.clang.indexer", "glean.clang.coordinator"]
      $ \pkg -> buildFbpkg pkg buildHash
  (coordinator_path, _) <- Async.concurrently
    (downloadFbpkg "glean.clang.coordinator" coordinator_ver)
    $ Async.concurrently
        (twUpdate mustUpdate serverJob region server_ver)
        (twUpdate mustUpdate indexerJob region indexer_ver)
  say "Waiting for server version and hostname"
  let tierWrite = "glean.write.dev." ++ region
      tierIndex = "glean.clang.indexer.dev." ++ region
      -- Wait for only the new package to be present
      waitForVersion tier version = do
        say $ "wait for version: " <> tier <> " " <> version
        let loop = do
              (exit, out, _err) <- readProcessWithExitCode
                "smcc"
                ["fb303-exports", tier , "build_package_version"]
                ""
              let versions = nub
                    [ v
                    | ["build_package_version", v] <- map words $ lines out ]
                  found = case versions of
                    [v] -> isPrefixOf version v
                    _ -> False
              case exit of
                ExitSuccess
                  | found -> do
                      say ("Found " <> tier <> " "<> unwords versions)
                      return ()
                _ -> do
                  threadDelay 10000000
                  loop
        loop
      -- Wait for SMC to update, otherwise the coordinator will fail
      -- when trying to connect to the server.
      waitForHosts tier n = do
        say $ "wait for hosts: " <> tier <> " " <> show n
        let loop = do
              (exit, out, _err) <- readProcessWithExitCode
                "smcc"
                ["list-hosts", "--enabled", tier]
                ""
              let hosts = filter (".facebook.com" `isSuffixOf`) (lines out)
                  found = n <= length hosts
              case exit of
                ExitSuccess
                  | found -> say ("Found hosts " <> unwords hosts)
                _ -> do
                  threadDelay 5000000
                  loop
        loop

  r <- timeout (20 * 60 * 1000000) $ do
    waitForVersion tierWrite server_ver
    waitForHosts tierWrite 1
    when (isJust configHash) $ do
      waitForVersion tierIndex indexer_ver
      waitForHosts tierIndex requireTasks
  case r of
    Just () -> say "Server(s) up"
    Nothing -> die "Server(s) timed out"

  configeratorCanary configHash [tierWrite, tierIndex] $ do
    say "Starting indexing"
    let revisionArgs = case pickHash of
          Nothing -> []
          Just indexHash -> ["--repo", repo, "--revision", indexHash]
        recipeArgs = case pickRecipes of
          Nothing -> []
          Just recipes -> ["--recipes", recipes]
        dbArgs = case dbName of
          Nothing ->  []
          Just db -> ["--db", db]
        stackedArgs = case stacked of
          Nothing ->  []
          Just name -> ["--stacked", name]
    callProcess (coordinator_path </> "coordinator") $
      [ "--service", tierWrite ] ++ revisionArgs ++ recipeArgs
        ++ dbArgs ++ stackedArgs

  say "DevrRun complete"
