{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Facebook.Service.HttpFb303 where

import qualified Data.ByteString.Lazy as LBS
import Data.IORef (readIORef)
import Network.HTTP.Types
import Network.Wai

import Facebook.Fb303
import Fb303Core.Types

-- | Middleware exposing Fb303 as a more standard health check endpoint at @/health@.
fb303Middleware :: Fb303State -> Middleware
fb303Middleware fb303 app req respond = do
  case pathInfo req of
    ["health"] -> do
      status <- readIORef (fb303_status fb303)
      let replyStatus = case status of
            Fb303_status_ALIVE -> ok200
            Fb303_status_WARNING -> mkStatus 290 "warning"
            Fb303_status_STARTING -> mkStatus 291 "starting"
            Fb303_status_STOPPING -> mkStatus 292 "stopping"
            Fb303_status_STOPPED -> mkStatus 293 "stopped"
            Fb303_status_DEAD -> mkStatus 294 "dead"
            Fb303_status__UNKNOWN _ -> mkStatus 299 "unknown status"
      respond $ responseLBS replyStatus [(hContentType, "text/plain")] (LBS.fromStrict $ statusMessage replyStatus)
    _ -> app req respond
