{-# LANGUAGE ApplicativeDo #-}
module CleanManifold
where

import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)
import Options.Applicative
import System.FilePath
import qualified System.FilePath.Glob as Glob

import Control.Concurrent.Stream (stream)
import Manifold.Client
import Util.EventBase (withEventBaseDataplane)

import Glean.Init

data Config = Config
  { cfgBucket :: String
  , cfgPath :: String
  , cfgRepo :: String
  , cfgBefore :: String
  , cfgDryRun :: Bool
  , cfgVerbose :: Bool
  , cfgThreads :: Maybe Int
  }

options :: Parser Config
options =  do
  cfgBucket <- strOption
    $ long "bucket" <> metavar "NAME" <> value "glean_databases"
  cfgPath <- strOption
    $ long "path" <> metavar "PATH" <> value "nodes/dev-backup"
  cfgRepo <- strOption
    $ long "repo" <> metavar "PATTERN"
  cfgBefore <- strOption $ long "before" <> metavar "YYYY-MM-DD"
  cfgDryRun <- switch $ long "dry-run"
  cfgVerbose <- switch $ short 'v' <> long "verbose"
  cfgThreads <- optional $ option auto $ long "threads" <> metavar "N"
  return Config{..}

main :: IO ()
main =
  withOptions (info options fullDesc) $ \Config{..} ->
  withEventBaseDataplane $ \evb -> do
  manager <- defaultHTTPManager
  let common =
        [eventBaseDataplane evb :: Option Common
        ,httpManager manager]

  conc <- do
    conc <- case cfgThreads of
      Just 0 -> getNumCapabilities
      Just n -> return n
      Nothing -> getNumProcessors
    caps <- getNumCapabilities
    when (conc /= caps) $ setNumCapabilities conc
    return conc

  v <- newIORef []

  enumerate
    [include common]
    cfgBucket
    cfgPath
    $ \xs -> atomicModifyIORef v $ \ys -> (Map.toList xs:ys, True)

  let pat = Glob.compile cfgRepo
      match (name,meta) = do
        guard $ Glob.match pat $ dropExtension name
        created <- Map.lookup "created" $ objectProperties meta
        guard $ created < cfgBefore
        return (name, created, objectSize meta)

  entries <- sortOn (\(_,created,_) -> created) . mapMaybe match . concat
    <$> readIORef v
  let !total_count = length entries
      !total_size = sum [size | (_,_,size) <- entries]
  if cfgDryRun
    then do
      when cfgVerbose $ putStrLn $ unlines
        [name ++ " [" ++ created ++ "]" | (name,created,_) <- entries]
    else do
      output_lock <- newMVar ()
      stream conc (forM_ entries) $ \(name,created,_) -> do
        let obj = cfgPath ++ "/" ++ name
        deleteObj common cfgBucket obj
        when cfgVerbose
          $ withMVar output_lock
          $ \_ -> putStrLn $ name ++ " [" ++ created ++ "]"
  putStrLn $ unwords
    ["Total:", show total_count, "databases,", show total_size, "bytes"]
