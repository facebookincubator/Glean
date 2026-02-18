{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | An implementation of ConfigProvider that uses files on the local
-- filesystem, watching for changes using INotify.

{-# LANGUAGE ApplicativeDo #-}
module Glean.Impl.ConfigProvider (
    LocalConfigOptions(..),
    LocalSubscription,
    ConfigAPI(..),
  ) where

import Control.Concurrent
import Control.Exception
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO.Error

import qualified System.FSNotify as FSNotify

import Util.Control.Exception
import Util.Concurrent

import Glean.Util.ConfigProvider

data ConfigAPI = ConfigAPI
  { canonConfigDir :: FilePath
  , opts :: LocalConfigOptions
  -- ^ Canonicalized configuration directory (since the file watcher will
  -- likely canonicalize paths).
  , watchManager :: FSNotify.WatchManager
  , subscriptions ::
      IO (MVar (HashMap ConfigPath [ByteString -> IO ()]))
  }

newtype LocalConfigOptions = LocalConfigOptions
  { configDir :: Maybe FilePath
      -- ^ directory where the configs are found. If Nothing, we'll
      -- use $HOME/.config/glean.
  }

type instance ConfigOptions ConfigAPI = LocalConfigOptions

data LocalSubscription a = LocalSubscription

newtype ConfigProviderException = ConfigProviderException Text
  deriving (Show)

instance Exception ConfigProviderException

-- | Whether to accept a FS event for a given path
acceptEvent :: FSNotify.Event -> Bool
acceptEvent (FSNotify.Added _path _time FSNotify.IsFile) = True
acceptEvent (FSNotify.Modified _path _time FSNotify.IsFile) = True
-- Included for documentation of intent
acceptEvent (FSNotify.Removed _path _time _isDir) = False
acceptEvent (FSNotify.ModifiedAttributes _path _time _isDir) = False
acceptEvent _ = False

onEvent :: MVar (HashMap ConfigPath [ByteString -> IO ()]) -> FilePath -> IO ()
onEvent subs path = do
  callbacks <- fromMaybe [] . HashMap.lookup (Text.pack path) <$> readMVar subs
  contents <- ByteString.readFile path
  mapM_ ($ contents) callbacks
    `catchAll` \_ -> return ()

instance ConfigProvider ConfigAPI where
  configOptions = do
    configDir <- optional $ strOption
      (  long "config-dir"
      <> metavar "DIR"
      <> help ("directory where the config files can be found " <>
        "(default: $HOME/.config/glean)")
      )
    return LocalConfigOptions{..}

  defaultConfigOptions = LocalConfigOptions { configDir = Nothing }

  withConfigProvider opts f =
    FSNotify.withManager $ \watchManager -> do
      subs <- newMVar HashMap.empty
      -- fsnotify seems to give us canonicalized absolute paths back; we would
      -- like to look things up by the paths it gives us, so we need to have
      -- our own paths be canonicalized and absolute as well.
      canonConfigDir <- canonicalizePath =<< getDir opts
      -- Defer watcher startup until someone actually subscribes to an event
      -- (notably, proving that the config directory actually exists so that we
      -- can watch it, as watching a nonexistent directory on Linux is an
      -- error).
      subs' <- cacheSuccess
        (subs <$ FSNotify.watchTree watchManager canonConfigDir acceptEvent (\ev -> onEvent subs (FSNotify.eventPath ev)))
      let cfg = ConfigAPI canonConfigDir opts watchManager subs'
      f cfg

  type Subscription ConfigAPI = LocalSubscription

  subscribe cfg@ConfigAPI{..} path updated deserializer = do
    a <- get cfg path deserializer
    updated a
    let absPath = Text.pack $ canonConfigDir </> Text.unpack path
    subscriptions >>= \subs -> modifyMVar_ subs $ \hm ->
      let changed contents =
            deserialize path deserializer contents >>= updated
      in pure $ HashMap.insertWith (<>) absPath [changed] hm
    return LocalSubscription

  cancel _ _ = return () -- unimplemented for now

  get ConfigAPI{..} path deserializer = do
    contents <- ByteString.readFile (canonConfigDir </> Text.unpack path)
      `catch` \e ->
        if isDoesNotExistError e
          then throwIO $ ConfigProviderException $
            "no config for " <> path <> " at " <>
              Text.pack (canonConfigDir </> Text.unpack path)
          else
            throwIO e
    deserialize path deserializer contents

  isConfigFailure _ e
    | Just ConfigProviderException{} <- fromException e = True
    | otherwise = False

deserialize :: ConfigPath -> Deserializer a -> ByteString -> IO a
deserialize path deserializer contents =
  case deserializer contents of
    Left err -> throwIO $ ConfigProviderException $
      "deserialization failed for: " <> path <>
      ": " <> Text.pack err
    Right a -> return a

-- | default to $HOME/.config/glean if --config-dir is not set
getDir :: LocalConfigOptions -> IO FilePath
getDir opts =
  case configDir opts of
    Nothing -> getXdgDirectory XdgConfig "glean"
    Just dir -> return dir
