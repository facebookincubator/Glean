{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.RetryRecvTimeout
  ( retryRecvTimeout
  , retryServerQueueTimeout
  ) where

import Data.Text (Text)

retryRecvTimeout
  :: (Text -> IO ())
  -> Word
  -> IO x
  -> IO x
retryRecvTimeout _onRetry _retries act = act

retryServerQueueTimeout
  :: (Text -> IO ())
  -> Word
  -> IO x
  -> IO x
retryServerQueueTimeout _onRetry _retries act = act
