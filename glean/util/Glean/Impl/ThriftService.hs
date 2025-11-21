{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP, StandaloneDeriving #-}
module Glean.Impl.ThriftService
  ( ThriftService
  ) where

import Data.Maybe
import qualified Data.Text.Encoding as Text

import Glean.Util.Service
import Glean.Util.ThriftService

#ifdef FBTHRIFT

import qualified Data.ByteString.UTF8 as UTF8
import Network.Socket

import Thrift.Protocol.Id
import Thrift.Channel.HeaderChannel

-- | A basic 'ThriftService' supporting connections to a specific host/port
newtype ThriftService p = ThriftService
  { headerConfig :: HeaderConfig p
  }

deriving instance Show (ThriftService p)

instance IsThriftService ThriftService where
  mkThriftService (HostPort h p) ThriftServiceOptions{..} = Just $ ThriftService
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
  mkThriftService _ _ = Nothing

  thriftServiceWithDbShard t _ = t  -- shards are irrelevant if we have host/port

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

#else /* !FBTHRIFT */

import Data.List (uncons)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI (URI(..), URIAuth(..), parseURI)
import Text.Read (readMaybe)

import Thrift.Channel.HTTP
import Thrift.Protocol.Id

-- | A basic 'ThriftService' supporting connections to a specific host/port
newtype ThriftService p = ThriftService
  { httpConfig :: HTTPConfig p
  }

deriving instance Show (ThriftService p)

baseHttpConfig :: Text -> Int -> ThriftServiceOptions -> HTTPConfig p
baseHttpConfig host port ThriftServiceOptions{..} = HTTPConfig
  { httpHost = Text.encodeUtf8 host
  , httpPort = port
  , httpProtocolId = compactProtocolId
  , httpUseHttps = False
  , httpResponseTimeout =
      Just $ round (fromMaybe 30 processingTimeout * 1000000)
  }

instance IsThriftService ThriftService where
  mkThriftService (HostPort h p) serviceOpts = Just $ ThriftService
    { httpConfig = baseHttpConfig h (fromIntegral p) serviceOpts
    }
  mkThriftService (Uri u_) serviceOpts
    | Just u <- parseURI (Text.unpack u_)
    , Just auth <- uriAuthority u
    , uriScheme u `elem` ["http:", "https:"]
    , Just port <- parsePort (uriScheme u) (uriPort auth)
    = Just $ ThriftService
      { httpConfig = (baseHttpConfig (Text.pack $ uriRegName auth) port serviceOpts)
        { httpUseHttps = uriScheme u == "https:"
        }
      }
    where
    parsePort :: String -> String -> Maybe Int
    parsePort scheme port_ = do
      -- The parser gives us a leading :, for fun!
      (_, port) <- uncons $
        if port_ == "" then
          if scheme == "https:" then ":443" else ":80"
        else port_
      readMaybe port

  mkThriftService _ _ = Nothing

  thriftServiceWithDbShard t _ = t
    -- shards are irrelevant if we have host/port

  runThrift _evb ThriftService{..} action =
    withHTTPChannel httpConfig action

  getSelection _evb ThriftService{..} _ =
    return
      [ (Text.decodeUtf8 (httpHost httpConfig), httpPort httpConfig)
      ]

#endif
