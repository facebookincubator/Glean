{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.ThriftSource (
  ThriftSource,
  ThriftSourceException(..),

  -- * Creation
  config,
  configDefault,
  file,
  value,
  once,
  mutable,
  parse,

  -- ** With a custom deserializer
  Deserializer,
  configWithDeserializer,
  configWithDeserializerDefault,
  fileWithDeserializer,
  parseWithDeserializer,
  genericConfig,

  -- * Reading
  withValue,
  load,
  loadDefault
) where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)

import Thrift.Protocol (ThriftSerializable)
import Thrift.Protocol.JSON (deserializeJSON)

import Glean.Util.ConfigProvider as Config
import Glean.Util.Observed as Observed

-- | A source of values that may change over time.
data ThriftSource a = ThriftSource
  { thriftSourceWith
      :: forall cfg b . (ConfigProvider cfg)
      => cfg
      -> (Observed a -> IO b)
      -> IO b
  , thriftSourceLoad :: forall cfg . (ConfigProvider cfg) => cfg -> IO a
  , thriftSourceShow :: String
  }

-- | Load a configuration value from a source. The value isn't cached - use
-- 'withValue' and 'Observed.get' for that.
load
  :: (ConfigProvider cfg, Typeable a)
  => cfg
  -> ThriftSource a
  -> IO a
load cfg t = thriftSourceLoad t cfg

-- | Subscribe to a 'ThriftSource' - 'get' will produce the current value.
--
-- If actions are added to the 'Observed' via 'doOnUpdate', no further
-- instances of the actions will be initiated after 'withValue'
-- returns.
--
withValue
  :: (ConfigProvider cfg, Typeable a)
  => cfg
  -> ThriftSource a
  -> (Observed a -> IO b)
  -> IO b
withValue cfg t = thriftSourceWith t cfg

instance Show (ThriftSource a) where
  show = thriftSourceShow

instance Functor ThriftSource where
  fmap f ThriftSource{..} = ThriftSource
    { thriftSourceWith = \cfg g -> thriftSourceWith cfg (\ob -> g (fmap f ob))
    , thriftSourceLoad = \cfg -> fmap f $ thriftSourceLoad cfg
    , .. }

instance Default a => Default (ThriftSource a) where
  def = value def

newtype ThriftSourceException = ThriftSourceException Text
  deriving(Show)

instance Exception ThriftSourceException

-- | A 'ThriftSource' whose value is obtained by looking up the given
-- key using the 'ConfigProvider'.
config
  :: Typeable a
  => ThriftSerializable a
  => Text
  -> ThriftSource a
config path = configWithDeserializer path deserializeJSON

-- | As 'config', except that 'Data.Default.def' is used if the config
-- is missing.
configDefault
  :: (Typeable a, Default a)
  => ThriftSerializable a
  => Text
  -> ThriftSource a
configDefault path = configWithDeserializerDefault path deserializeJSON

-- | A 'ThriftSource' whose value is obtained by looking up the given
-- key using the 'ConfigProvider', and converting it using the given
-- 'Deserializer'
configWithDeserializer
  :: Typeable a
  => Text
  -> Deserializer a
  -> ThriftSource a
configWithDeserializer path d = genericConfig path d (const return) Nothing

-- | As 'configWithDeserializer' except that 'Data.Default.def' is
-- used if the config is missing.
configWithDeserializerDefault
  :: (Typeable a, Default a)
  => Text
  -> Deserializer a
  -> ThriftSource a
configWithDeserializerDefault path d =
  genericConfig path d (const return) (Just def)

genericConfig
  :: (Typeable c)
  => Text
  -> Deserializer c
  -> (forall cfg. ConfigProvider cfg => cfg -> c -> IO a)
  -> Maybe a
  -> ThriftSource a
genericConfig key deserializer mkValue maybDefault = ThriftSource
  { thriftSourceWith = \cfg ->
      configWithValue cfg key deserializer mkValue maybDefault
  , thriftSourceLoad = \cfg ->
      configLoad cfg key deserializer mkValue maybDefault
  , thriftSourceShow = "ThriftSource Fixed"
  }

file :: ThriftSerializable a => FilePath -> ThriftSource a
file path = fileWithDeserializer path deserializeJSON

fileWithDeserializer :: FilePath -> Deserializer a -> ThriftSource a
fileWithDeserializer path deserializer = once $ do
  b <- BS.readFile path
  case deserializer b of
    Left err -> throwIO $ ThriftSourceException $ Text.pack $
      "invalid Thrift value in file '" ++ path ++ "': " ++ err
    Right a -> return a

value :: a -> ThriftSource a
value = once . return

once :: IO a -> ThriftSource a
once io = ThriftSource
  { thriftSourceWith = \_ f -> do x <- io; f (fixedValue x)
  , thriftSourceLoad = \_ -> io
  , thriftSourceShow = "ThriftSource Fixed"
  }

changes :: Observed a -> ThriftSource a
changes ob = ThriftSource
  { thriftSourceWith = \_ f -> f ob
  , thriftSourceLoad = \_ -> Observed.get ob
  , thriftSourceShow = "ThriftSource Changes"
  }

mutable :: a -> IO (ThriftSource a, (a -> a) -> IO ())
mutable x = do
  (ob, onUpdate) <- changingValue x
  return (changes ob, onUpdate)

-- | Parser for  \"config:PATH\" and \"file:PATH\" providers
parse
  :: (Typeable a, ThriftSerializable a)
  => String
  -> Either String (ThriftSource a)
parse s = parseWithDeserializer s deserializeJSON

-- | Parser for  \"config:PATH\" and \"file:PATH\" providers with a
-- custom deserializer.
parseWithDeserializer
  :: Typeable a
  => String
  -> Deserializer a
  -> Either String (ThriftSource a)
parseWithDeserializer s des = case break (==':') s of
  ("config",':':path) ->
    Right $ configWithDeserializer (Text.pack path) des
  ("file",':':path) -> Right $ fileWithDeserializer path des
  (p,':':_) -> Left $ "unsupported Thrift provider '" ++ p ++ "'"
  _ -> Left $ "invalid Thrift source '" ++ s ++ "'"

-- | Subscribe to a 'ThriftSource' - 'get' will produce the current value.
--
-- If actions are added to the 'Observed' via 'doOnUpdate', no further
-- instances of the actions will be initiated after 'withValue'
-- returns.
--
configWithValue
  :: (ConfigProvider cfg, Typeable c)
  => cfg
  -> Text
  -> Deserializer c
  -> (forall cfg. ConfigProvider cfg => cfg -> c -> IO a)
  -> Maybe a
  -> (Observed a -> IO b)
  -> IO b

configWithValue cfgapi path deserialize mkValue maybeDefault action =
  do
    (ob, onUpdate) <- changingValue
      (error "ThriftSource.withValue: internal error")
    let
      callback new = do a <- mkValue cfgapi new; onUpdate (const a)
      acquire =
        (Just <$> subscribe cfgapi path callback deserialize)
          `catch` \e -> if
            | isConfigFailure cfgapi e, Just val <- maybeDefault -> do
              onUpdate (const val)
              return Nothing
            | otherwise -> throwIO e
    bracket acquire (mapM (cancel cfgapi)) $ const $ action ob

configLoad
  :: (ConfigProvider cfg, Typeable c)
  => cfg
  -> Text
  -> Deserializer c
  -> (forall cfg. ConfigProvider cfg => cfg -> c -> IO a)
  -> Maybe a
  -> IO a
configLoad cfgapi path deserialize mkValue maybeDefault =
  (do c <- Config.get cfgapi path deserialize; mkValue cfgapi c)
    `catch` \e -> if
      | isConfigFailure cfgapi e, Just val <- maybeDefault ->
        return val
      | otherwise -> throwIO e

-- | Like 'load', but if the value cannot be retrieved then fall back
-- to using the default value given by the 'Default' instance.
loadDefault
  :: (ConfigProvider cfg, Default a, Typeable a)
  => cfg
  -> ThriftSource a
  -> IO a
loadDefault cfgAPI ts =
  load cfgAPI ts `catch` \e ->
    if isConfigFailure cfgAPI e then return def else throwIO e
