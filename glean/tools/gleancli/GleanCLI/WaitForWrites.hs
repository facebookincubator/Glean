{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.WaitForWrites (WaitForWritesCommand) where

import Options.Applicative

import Util.OptParse

import GleanCLI.Common
import GleanCLI.Types
import Util.Log
import Glean.Types as Thrift hiding (Database)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)

import Glean

data WaitForWritesCommand
  = WaitForWrites
      { waitForWritesRepo :: Repo
      }

instance Plugin WaitForWritesCommand where
  parseCommand =
    commandParser "wait-for-writes"
      (progDesc "Wait for all pending async writes to complete") $ do
      waitForWritesRepo <- dbOpts
      return WaitForWrites{..}

  -- | Wait for all pending asynchronous writes to complete for the
  -- given repo. Polls the server until writes are done.
  runCommand _ _ backend WaitForWrites{..} = loop
    where
      loop = do
        result <- try $ waitForWrites backend waitForWritesRepo
        case result of
          Left err -> logError $
            "wait-for-writes: " <> show (err :: SomeException)
          Right WaitForWritesResponse{..} -> do
            let count = fromIntegral
                  waitForWritesResponse_pending_writes_count
            if count > 0
              then do
                logInfo $ "Waiting for " <> show count <> " pending writes"
                threadDelay (min maxDelay (max minDelay (count * timePerWrite)))
                loop
              else logInfo "All writes complete"
      minDelay = 60000000   -- 1 minute
      maxDelay = 300000000  -- 5 minutes
      timePerWrite = 2000000 -- 2 seconds
