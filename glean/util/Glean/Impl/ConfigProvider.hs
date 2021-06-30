-- Copyright (c) Facebook, Inc. and its affiliates.

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
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO.Error
import System.INotify

import Util.Control.Exception

import Glean.Util.ConfigProvider

data ConfigAPI = ConfigAPI
  { opts :: LocalConfigOptions
  , inotify :: INotify
  , subscriptions ::
      MVar (HashMap ConfigPath (WatchDescriptor, [ByteString -> IO ()]))
  }

data LocalConfigOptions = LocalConfigOptions
  { configDir :: Maybe FilePath
      -- ^ directory where the configs are found. If Nothing, we'll
      -- use $HOME/.config/glean.
  }

type instance ConfigOptions ConfigAPI = LocalConfigOptions

data LocalSubscription a = LocalSubscription

newtype ConfigProviderException = ConfigProviderException Text
  deriving (Show)

instance Exception ConfigProviderException

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
    withINotify $ \inotify -> do
      subs <- newMVar HashMap.empty
      f (ConfigAPI opts inotify subs)

  type Subscription ConfigAPI = LocalSubscription

  subscribe cfg@ConfigAPI{..} path updated deserializer = do
    a <- get cfg path deserializer
    updated a
    dir <- getDir opts
    modifyMVar_ subscriptions $ \hm -> do
      let
        changed contents =
          deserialize path deserializer contents >>= updated
      case HashMap.lookup path hm of
        Just (watch, others) ->
          return $ HashMap.insert path (watch, changed:others) hm
        Nothing -> do
          let file = BC.pack $ dir </> Text.unpack path
          watch <- addWatch inotify [Modify,MoveIn,Create] file $ \_events -> do
            callbacks <- withMVar subscriptions $ \hm -> do
              case HashMap.lookup path hm of
                Nothing -> return []
                Just (_, callbacks) -> return callbacks
            contents <- ByteString.readFile (dir </> Text.unpack path)
            mapM_ ($ contents) callbacks
              `catchAll` \_ -> return ()
          return $ HashMap.insert path (watch, [changed]) hm
    return LocalSubscription

  cancel _ _ = return () -- unimplemented for now

  get ConfigAPI{..} path deserializer = do
    dir <- getDir opts
    contents <- ByteString.readFile (dir </> Text.unpack path)
      `catch` \e ->
        if isDoesNotExistError e
          then throwIO $ ConfigProviderException $
            "no config for " <> path <> " at " <>
              Text.pack (dir </> Text.unpack path)
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
