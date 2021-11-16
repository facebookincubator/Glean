{-
  Copyright (c) Facebook, Inc. and its affiliates.
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

data ThriftSource a where
  -- | A value that we obtain from the ConfigProvider. Each value of 'Text' may
  -- only have a single 'Deserializer' value (part of global mutable state).
  --
  -- The @'Deserializer' b@ is globally registered. The @(b -> a)@ is here
  -- to support @instance Functor ThriftSource@ without affecting the
  -- global @'Deserializer' b@.
  Config
    :: Typeable b
    => Text                             -- config provider path key
    -> Deserializer b                   -- usually 'deserializeJSON'
    -> Maybe b                          -- optional default value
    -> (b -> a)                         -- for 'Functor' instance
    -> ThriftSource a

  -- | A value that is determined once and then never changes
  Fixed
    :: IO a
    -> ThriftSource a

  -- | A value that may change over time
  Changes :: Observed a -> ThriftSource a

instance Show (ThriftSource a) where
  show (Config t _ _ _) = unwords [ "ThriftSource Config {"
    , show t, "}" ]
  show Fixed{} = "ThriftSource Fixed"
  show Changes{} = "ThriftSource Changes"

instance Functor ThriftSource where
  fmap f (Config t d m g) = Config t d m (f . g)
  fmap f (Fixed g) = Fixed (fmap f g)
  fmap f (Changes ob) = Changes (fmap f ob)

instance Default a => Default (ThriftSource a) where
  def = Fixed $ return def

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
configWithDeserializer path d = Config path d Nothing id

-- | As 'configWithDeserializer' except that 'Data.Default.def' is
-- used if the config is missing.
configWithDeserializerDefault
  :: (Typeable a, Default a)
  => Text
  -> Deserializer a
  -> ThriftSource a
configWithDeserializerDefault path d = Config path d (Just def) id

file :: ThriftSerializable a => FilePath -> ThriftSource a
file path = fileWithDeserializer path deserializeJSON

fileWithDeserializer :: FilePath -> Deserializer a -> ThriftSource a
fileWithDeserializer path deserializer = Fixed $ do
  b <- BS.readFile path
  case deserializer b of
    Left err -> throwIO $ ThriftSourceException $ Text.pack $
      "invalid Thrift value in file '" ++ path ++ "': " ++ err
    Right a -> return a

value :: a -> ThriftSource a
value = Fixed . return

once :: IO a -> ThriftSource a
once = Fixed

mutable :: a -> IO (ThriftSource a, (a -> a) -> IO ())
mutable x = do
  (ob, onUpdate) <- changingValue x
  return (Changes ob, onUpdate)

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
withValue
  :: (ConfigProvider cfg, Typeable a)
  => cfg
  -> ThriftSource a
  -> (Observed a -> IO b)
  -> IO b

withValue cfgapi (Config path deserialize maybeDefault f) action =
  do
    (ob, onUpdate) <- changingValue
      (error "ThriftSource.withValue: internal error")
    let
      callback new = onUpdate (const new)
      acquire =
        (Just <$> subscribe cfgapi path callback deserialize)
          `catch` \e -> if
            | isConfigFailure cfgapi e, Just val <- maybeDefault -> do
              onUpdate (const val)
              return Nothing
            | otherwise -> throwIO e
    let
    bracket acquire (mapM (cancel cfgapi)) $ const $ action $ fmap f ob


withValue _ (Changes ob) action = action ob

withValue cfgapi source@Fixed{} action = do
  x <- load cfgapi source
  action $ fixedValue x

-- | Load a configuration value from a source. The value isn't cached - use
-- 'withValue' and 'Observed.get' for that.
load
  :: (ConfigProvider cfg, Typeable a)
  => cfg
  -> ThriftSource a
  -> IO a
load cfgapi (Config path deserialize maybeDefault f) =
  fmap f $
    Config.get cfgapi path deserialize
      `catch` \e -> if
        | isConfigFailure cfgapi e, Just val <- maybeDefault ->
          return val
        | otherwise -> throwIO e
load _ (Fixed io) = io
load _ (Changes ob) = Observed.get ob

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
