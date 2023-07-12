{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Facebook.Service
  ( runFacebookService
  , withBackgroundFacebookService
  , runFacebookServiceDeferredAlive
  , withBackgroundFacebookServiceDeferredAlive
  , waitForTerminateSignals
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (void)
import Data.IORef
import System.Posix.Signals

import Fb303Core.Types
import Facebook.Fb303
import Thrift.Processor
#ifdef FBTHRIFT
import Thrift.Server.CppServer
#else
import Thrift.Server.HTTP
#endif

-- | Runs a facebook service on a CPPServer
-- Handles fb303 statuses as well as shutting down on process signals
runFacebookService
  :: Processor s
  => Fb303State -- ^ Mandatory fb303 state
  -> (forall r. s r -> IO r) -- ^ your handler
  -> ServerOptions -- ^ server options
  -> IO ()
runFacebookService fb303State handler opts = do
  withBackgroundFacebookService fb303State handler opts $
    const waitForTerminateSignals

-- | Like 'runFacebookService' but does not set the fb303 status to
-- @ALIVE@ when the server starts up. It is the caller's responsibility
-- to set the status to alive via the 'Fb303State' when the server is
-- ready to receive requests.
runFacebookServiceDeferredAlive
  :: Processor s
  => Fb303State -- ^ Mandatory fb303 state
  -> (forall r. s r -> IO r) -- ^ your handler
  -> ServerOptions -- ^ server options
  -> IO ()
runFacebookServiceDeferredAlive fb303State handler opts = do
  withBackgroundFacebookServiceDeferredAlive fb303State handler opts $
    const waitForTerminateSignals

-- | Block indefinitely until the process receives SIGTERM or SIGINT,
-- and then return.
waitForTerminateSignals :: IO ()
waitForTerminateSignals = do
  -- To wait in Haskell-land while the server is taking requests,
  -- use an mvar that gets filled when the right signals are read
  mvar <- newEmptyMVar
  let sigHandler = void $ tryPutMVar mvar ()
  withSignalHandler sigTERM sigHandler $ \_ ->
    withSignalHandler sigINT sigHandler $ \_ ->
      -- Haskell will wait here until being instructed to stop
      takeMVar mvar
  where
    withSignalHandler sig h = bracket
      (installHandler sig (Catch h) Nothing)
      (\old -> installHandler sig old Nothing)

-- | Runs a facebook service on a CPPServer
-- Handles fb303 statuses and gracefully shuts down the server once the
-- passed in wait action completes.
withBackgroundFacebookService
  :: Processor s
  => Fb303State -- ^ Mandatory fb303 state
  -> (forall r. s r -> IO r) -- ^ your handler
  -> ServerOptions -- ^ server options
  -> (Server -> IO a) -- ^ wait action
  -> IO a
withBackgroundFacebookService fb303 handler opts waitAction =
  withBackgroundFacebookService_ fb303 handler opts $ \server ->
    bracketFb303 fb303 Fb303_status_ALIVE Fb303_status_STOPPING $
      waitAction server

-- | Like 'withBackgroundFacebookService', but does not set the fb303
-- status to @ALIVE@ when the server starts up. It is the caller's
-- responsibility to set the status to alive via the 'Fb303State'
-- when the server is ready to receive requests.
withBackgroundFacebookServiceDeferredAlive
  :: Processor s
  => Fb303State -- ^ Mandatory fb303 state
  -> (forall r. s r -> IO r) -- ^ your handler
  -> ServerOptions -- ^ server options
  -> (Server -> IO a) -- ^ wait action
  -> IO a
withBackgroundFacebookServiceDeferredAlive fb303 handler opts waitAction =
  withBackgroundFacebookService_ fb303 handler opts $ \server ->
    waitAction server
      `finally` writeIORef (fb303_status fb303) Fb303_status_STOPPING

bracketFb303 :: Fb303State -> Fb303_status -> Fb303_status -> IO a -> IO a
bracketFb303 Fb303State{..} a b =
  bracket_ (writeIORef fb303_status a) (writeIORef fb303_status b)

withBackgroundFacebookService_
  :: Processor s
  => Fb303State
  -> (forall r. s r -> IO r)
  -> ServerOptions
  -> (Server -> IO a)
  -> IO a
withBackgroundFacebookService_ fb303 handler opts waitAction = do
  bracketFb303 fb303 Fb303_status_STARTING Fb303_status_STOPPED $ do
    withBackgroundServer handler opts waitAction
