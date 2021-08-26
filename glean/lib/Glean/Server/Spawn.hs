-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Server.Spawn (withServer)
where

import Control.Concurrent (threadDelay)
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Process

-- | Spawn a server with an added --write-port flag, wait until it has written
-- the port number into fail and then pass it to the function.
withServer
  :: CreateProcess
  -> (Int
      -> Maybe Handle
      -> Maybe Handle
      -> Maybe Handle
      -> ProcessHandle
      -> IO a)
  -> IO a
withServer proc f =
  withSystemTempFile "glean-port" $ \path h -> do
  hClose h
  let new_proc = proc
        { cmdspec = case cmdspec proc of
            ShellCommand s ->
              ShellCommand $ s ++ " --write-port='" ++ path ++ "'"
            RawCommand bin args ->
              RawCommand bin $ args ++ ["--write-port=" ++ path]
        }
  withCreateProcess new_proc $ \mb_in mb_out mb_err ph -> do
    let wait = do
          threadDelay 500000
          loop

        loop = do
          r <- try $ BS.readFile path
          case r of
            Left err
              | isDoesNotExistError err || isAlreadyInUseError err -> wait
              | otherwise -> throwIO err
            Right s
              | BS.null s -> wait
              | otherwise -> return $! read (BS.unpack s)
    port <- loop
    f port mb_in mb_out mb_err ph
