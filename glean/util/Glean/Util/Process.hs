{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Process (withCreateProcessGroup)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO (Handle)
import System.Posix.IO
import System.Posix.Signals (signalProcessGroup, sigKILL, sigTERM)
import System.Process

import Util.Control.Exception

-- | Just like 'withCreateProcess' except that it always creates a new process
-- group and terminates all processes in that group on exit - first politely
-- (SIGTERM) and after a while rudely (SIGKILL). As this happens asynchronously,
-- we can expect all processes in the new group to be dead soon after
-- withCreateProcessGroup returns.
--
withCreateProcessGroup
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcessGroup c action =
  mask $ \restore ->
  withCreateProcess c{create_group=True} $ \m_in m_out m_err ph -> bracket
    (do
      r <- getPid ph
      case r of
        Just pid ->
          do
            -- Keeping an open handle to any file in /proc/[pid]/ns/ prevents
            -- the pid from being reused. Assuming nothing calls waitProcess
            -- before this point (which it shouldn't), this should ensure that
            -- the pid still refers to the same (potentially dead) process in
            -- the close action.
            fd <- openFd
              ("/proc/" ++ show pid ++ "/ns/pid")
              ReadOnly
              Nothing
              defaultFileFlags
            return $ Just (pid,fd)
          `catchAll` const (return Nothing)
        Nothing -> return Nothing)
    (\r -> case r of
        Just (pid,fd) -> void $ forkIO $
          restore (do
          -- This is a fire-and-forget action. First, withCreateProcess will
          -- SIGTERM the actual process. We give things some time to settle and
          -- then SIGTERM the entire group and then, after some more time, we
          -- SIGKILL the entire group.
          --
          -- This is somewhat buck specific: we don't SIGTERM the entire group
          -- immediately because buck likes to spawn things that dump a stack
          -- trace if they receive SIGTERM but they seem to go away once buck
          -- itself is dead. So the hope is that they will be gone by the time
          -- we signal the group.
          --
          -- Unfortunately, there is no easy way to figure out if all processes
          -- in a group are dead so we just signal the group unconditionally but
          -- really hope that there are no processes left when we do so.
            threadDelay 2000000
            void $ tryAll $ signalProcessGroup sigTERM pid
            threadDelay 2000000
            void $ tryAll $ signalProcessGroup sigKILL pid)
          `finally` void (tryAll $ closeFd fd)
        Nothing -> return ())
    $ const $ restore (action m_in m_out m_err ph)
