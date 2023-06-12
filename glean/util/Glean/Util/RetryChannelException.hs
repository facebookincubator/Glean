{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{- |
  Provides a configurable wrapper for retrying Thrift requests that may fail
  with ChannelException.
-}
module Glean.Util.RetryChannelException
  ( RetryPolicy(..)
  , defaultRetryPolicy
  , retryChannelExceptions
  ) where

import Control.Exception
import Text.Printf
import System.Random
import System.Time.Extra
import Util.Log

import Thrift.Channel

data RetryPolicy = RetryPolicy
  { maxRetries :: Int
  , minRetryDelay :: Seconds
  , maxRetryDelay :: Seconds
  , retryJitter :: Double -- ^ fraction of delay
  , onError :: ChannelException -> Int -> Int -> Maybe Double -> IO ()
  }

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy
  { maxRetries = 5
  , minRetryDelay = 1
  , maxRetryDelay = 30
  , retryJitter = 0.2  -- 20%
  , onError = logRetry
  }

logRetry :: ChannelException -> Int -> Int -> Maybe Double -> IO ()
logRetry ex n maxRetries maybeDelay = do
  let msg = printf "failed (%s) (try %d/%d)" (show ex) n (maxRetries+1)
  logWarning $ msg <> case maybeDelay of
    Nothing -> ""
    Just delay -> printf ", retry after %.2fs" delay

retryChannelExceptions :: RetryPolicy -> IO a -> IO a
retryChannelExceptions r@RetryPolicy{..} act = go 1
  where
  go n = do -- attempt number n
    e <- try act
    case e of
      Right x -> return x
      Left ex@ChannelException{}
        | n >= maxRetries -> do
          onError ex n maxRetries Nothing
          throwIO ex
        | otherwise -> do
          delay <- retryDuration r n
          onError ex n maxRetries (Just delay)
          sleep delay
          go (n+1)

retryDuration :: RetryPolicy -> Int -> IO Double
retryDuration RetryPolicy{..} n = do
  randomJitter <- randomRIO (-retryJitter, retryJitter)
  let dur = exp randomJitter * minRetryDelay * 2^(n-1)
  return $! max minRetryDelay $ min maxRetryDelay dur
