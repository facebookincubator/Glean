-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE StandaloneDeriving #-}
module Glean.Impl.ThriftService
  ( ThriftService
  ) where

import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe
import qualified Data.Text.Encoding as Text
import Network.Socket

import Thrift.Channel.HeaderChannel
import Thrift.Protocol.Id

import Glean.Util.Service
import Glean.Util.ThriftService

-- | A basic 'ThriftService' supporting connections to a specific host/port
newtype ThriftService p = ThriftService
  { headerConfig :: HeaderConfig p
  }

deriving instance Show (ThriftService p)

instance IsThriftService ThriftService where
  mkThriftService (HostPort h p) ThriftServiceOptions{..} = ThriftService
    { headerConfig = headerConfig
    }
    where
    timeout = round (fromMaybe 30 processingTimeout * 1000)
    headerConfig = HeaderConfig
      { headerHost = Text.encodeUtf8 h
      , headerPort = fromIntegral p
      , headerProtocolId = compactProtocolId
      , headerConnTimeout = timeout
      , headerSendTimeout = timeout
      , headerRecvTimeout = timeout
      }
  mkThriftService _ _ = error "basic-thriftservice does not support Tier"

  thriftServiceWithShard t _ = t  -- shards are irrelevant if we have host/port

  runThrift evb ThriftService{..} action = do
    addrs <- getAddrInfo
      (Just defaultHints)
      (Just (UTF8.toString (headerHost headerConfig)))
      Nothing
    headerConfig' <- case addrs of
       [] -> return headerConfig
       (addr : _) -> do
         (mHost, _) <- getNameInfo [NI_NUMERICHOST] True False
           (addrAddress addr)
         case mHost of
           Nothing -> return headerConfig
           Just host -> return
             headerConfig { headerHost = UTF8.fromString host }
    withHeaderChannel evb headerConfig' action

  getSelection _evb ThriftService{..} _ =
    return
      [ (Text.decodeUtf8 (headerHost headerConfig), headerPort headerConfig)
      ]
