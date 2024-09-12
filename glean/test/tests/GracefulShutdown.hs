{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GracefulShutdown (
  main,
) where

import Control.Concurrent.Async (async)
import Control.DeepSeq (rnf)
import Control.Exception (Exception, catchJust, evaluate, onException)
import Control.Monad.Extra (unless)
import Data.Default (def)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.Text as T
import GHC.Exts (fromString)
import System.Environment (getEnv)
import System.IO (hGetContents, hPutStrLn, stderr)
import System.Process (
  CreateProcess (std_err),
  ProcessHandle,
  StdStream (CreatePipe),
  getProcessExitCode,
  proc,
  terminateProcess,
 )
import System.Time.Extra (Seconds, sleep, timeout)
import Test.HUnit (
  Test (TestCase, TestLabel, TestList),
  assertBool,
 )
import Test.HUnit.Lang

import Configerator (ConfigAPI)
import TestRunner (testRunner)
import Thrift.Channel (ChannelException (ChannelException))
import Util.EventBase (withEventBaseDataplane)
import Util.Log.String (logInfo)

import Glean (
  Backend (workFinished),
  ClientConfig (clientConfig_serv),
  KickOff (kickOff_fill, kickOff_repo),
  KickOffFill (KickOffFill_writeHandle),
  KickOffResponse (KickOffResponse),
  Outcome (Outcome_success),
  Repo (Repo),
  Work (..),
  WorkFinished (..),
  completePredicates,
  kickOffDatabase,
  listDatabases,
 )
import Glean.Init (withUnitTest)
import Glean.Remote (
  ThriftBackend,
  defaultClientConfigSource,
  withRemoteBackend,
 )
import Glean.Server.Spawn (withServer)
import qualified Glean.Types as Glean
import Glean.Util.ConfigProvider (
  ConfigProvider (defaultConfigOptions, withConfigProvider),
 )

data Config = Config
  { serverBinary :: FilePath
  , schemaDir :: FilePath
  }

getConfig :: IO Config
getConfig = do
  serverBinary <- getEnv "GLEAN_SERVER"
  schemaDir <- getEnv "SCHEMA"
  return Config{..}

main :: IO ()
main = do
  cfg <- getConfig
  withUnitTest $
    testRunner $
      TestList
        [ TestLabel "server waits for incomplete DBs" $
            TestCase $
              waitForIncompleteDBs cfg
        , TestLabel "test assumptions" $
            TestList
              [ TestLabel "server shuts down timely with default config" $
                  TestCase $ serverShutdown 0 cfg
              , TestLabel "server shuts down when no incomplete DBs" $
                  TestCase $ serverShutdown 1000 cfg
              ]
        ]

serverShutdown :: Int -> Config -> Assertion
serverShutdown timeout = setupOutOfProcessServer timeout $ \ph err backend -> do
  -- consumer the server error output to prevent it from getting blocked
  _ <- async $ evaluate (rnf $ length err)
  -- check that the server is responding
  _ <- listDatabases backend def
  terminateProcess ph
  retry assertions 5 $ do
    ec <- getProcessExitCode ph
    assertBool "Server did not shut down in a reasonable time" (isJust ec)

waitForIncompleteDBs :: Config -> Assertion
waitForIncompleteDBs = setupOutOfProcessServer 10000 $ \ph err backend -> do
  -- kick off a database
  let repo = Repo "repo" "hash"
  KickOffResponse _ <-
    kickOffDatabase
      backend
      def
        { kickOff_repo = repo
        , kickOff_fill = Just $ KickOffFill_writeHandle "writeHandle"
        }
  -- send a SIGTERM to the server and verify that it refuses to die
  terminateProcess ph
  !found <- timeout 5 $ evaluate $ "Waiting for incomplete dbs" `isInfixOf` err
  assertBool "Waits for incomplete DBs" (found == Just True)

  -- consumer the remaining error output to prevent blocking
  _ <- async $ evaluate (rnf $ length err)

  -- finish the DB and check the server exits
  Glean.completePredicates backend repo (Glean.CompletePredicates_axiom def)
  workFinished
    backend
    WorkFinished
      { workFinished_work =
          def
            { work_repo = repo
            , work_task = ""
            , work_parcelIndex = 0
            , work_handle = "writeHandle"
            }
      , workFinished_outcome = Outcome_success def
      }
  retry assertions 10 $ do
    ec <- getProcessExitCode ph
    assertBool "Dies when all incomplete DBs are finalized" (isJust ec)

setupOutOfProcessServer ::
  Int ->
  (ProcessHandle -> String -> ThriftBackend -> IO a) ->
  Config ->
  IO a
setupOutOfProcessServer timeout test Config{..} = do
  let cp =
        ( proc
            serverBinary
            [ "--db-tmp"
            , "--graceful-shutdown-wait-seconds=" <> show timeout
            , "--schema=" <> schemaDir
            ]
        )
          { std_err = CreatePipe
          }
  -- start an out-of-process server
  withServer cp $ \port _in _out (Just err) ph -> do
    let service =
          defaultClientConfigSource <&> \x ->
            x {clientConfig_serv = fromString $ "localhost:" <> show port}

    -- and set up a remote backend to talk to it
    retryChannelException $
      withEventBaseDataplane $ \evb ->
        withConfigProvider defaultConfigOptions $ \(cfgAPI :: ConfigAPI) ->
          withRemoteBackend evb cfgAPI service Nothing $ \backend -> do
            errContents <- hGetContents err
            test ph errContents backend
              `onException` hPutStrLn stderr errContents

retryChannelException :: IO a -> IO a
retryChannelException = retry connectionRefused maxAttempts
  where
    maxAttempts = 5
    connectionRefused (ChannelException msg) =
      "Connection refused" `T.isInfixOf` msg

retry :: (Exception e) => (e -> Bool) -> Seconds -> IO b -> IO b
retry exceptionType maxAttempts action =
  go (max 1 maxAttempts)
  where
    go 1 = do
      logInfo "last attempt"
      action
    go attemptsLeft = do
      let currentAttempt = maxAttempts - attemptsLeft
      unless (currentAttempt == 0) $
        logInfo $ "retry " <> show (floor currentAttempt :: Int)
      sleep (maxAttempts + 1 - attemptsLeft)
      catchJust (\e -> if exceptionType e then Just () else Nothing) action $
        \_ -> go (attemptsLeft -1)

-- | 'retry assertions'
assertions :: HUnitFailure -> Bool
assertions HUnitFailure {} = True
