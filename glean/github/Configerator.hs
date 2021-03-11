module Configerator (module Configerator) where

import Data.ByteString (ByteString)
import Control.Exception
import Data.Text
import Data.Typeable

import Thrift.Protocol

data ConfigAPI = ConfigAPI
data ConfigeratorOptions = ConfigeratorOptions

defaultConfigeratorOptions :: ConfigeratorOptions
defaultConfigeratorOptions = ConfigeratorOptions

newtype ConfigeratorException = ConfigeratorException Text
  deriving Show

instance Exception ConfigeratorException

newtype SubscribeFailed = SubscribeFailed Text
  deriving Show

instance Exception SubscribeFailed

withConfigeratorAPI
  :: ConfigeratorOptions -> (ConfigAPI -> IO a) -> IO a
withConfigeratorAPI _ f  = f ConfigAPI

getConfigThrift
  :: forall a . (ThriftSerializable a, Typeable a)
  => ConfigAPI -> Text -> IO a
getConfigThrift _ key = throwIO $
  ConfigeratorException $ "not supported: " <> key

getConfigWithCustomDeserializer
  :: forall a . (Typeable a)
  => ConfigAPI
  -> Text
  -> (ByteString -> Either String a)
  -> IO a
getConfigWithCustomDeserializer _ key _ =
  throwIO $ ConfigeratorException $ "not supported: " <> key

data Subscription a

subscribeToThriftValue
  :: forall a . (ThriftSerializable a, Typeable a)
  => ConfigAPI
  -> Text
  -> (Maybe a -> a -> IO ())
  -> IO (Subscription a)
subscribeToThriftValue _ _ _ = throwIO $ ConfigeratorException "not supported"

subscribeToThriftValueWithCustomDeserializer
  :: forall a . (Typeable a)
  => ConfigAPI
  -> Text
  -> (Maybe a -> a -> IO ())
  -> (ByteString -> Either String a)
  -> IO (Subscription a)
subscribeToThriftValueWithCustomDeserializer _ _ _ _ =
  throwIO $ ConfigeratorException "not supported"

cancelSubscription :: Subscription a -> IO ()
cancelSubscription _ = throwIO $ ConfigeratorException "not supported"
