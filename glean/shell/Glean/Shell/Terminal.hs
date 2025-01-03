{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Glean.Shell.Terminal
  ( withPager
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import System.IO
#if __GLASGOW_HASKELL__ >= 810
import System.IO.Error
#endif
import System.Process

-- | Spawn a pager (currently hard-coded as `less` with some flags) and pass its
-- stdin handle to the action. The pager is spawned in parallel to the action
-- so the handle is wrapped in an 'MVar' which can be waited on. If the pager
-- fails for any reason, the 'MVar' will be set to the current stdout. The
-- action will be aborted if the pager exits before it is finished (allowing the
-- user to stop the action by quitting the pager).
withPager :: (MVar Handle -> IO a) -> IO (Maybe a)
withPager action = mask $ \restore -> do
  handle_var <- newEmptyMVar
  withAsyncWithUnmask (act handle_var) $ \async_action ->
    withAsyncWithUnmask (pager handle_var) $ \async_pager ->
      restore $ do
        r <- waitEitherCatch async_action async_pager
        case r of
          Left (Left exc)
#if __GLASGOW_HASKELL__ >= 810
            | Just e <- fromException exc, isResourceVanishedError e ->
              -- action got a "Broken pipe" error because the user
              -- quit the pager; ignore it
              return Nothing
#endif
            | otherwise -> throwIO exc
          Left (Right x) -> do
            void $ wait async_pager
            return $ Just x
          Right (Left _) -> Just <$> wait async_action
          Right (Right _) -> return Nothing
  where
    act handle_var restore =
      restore (action handle_var)
      `finally` do
        h <- readMVar handle_var
        when (h /= stdout) $ hClose h

    pager handle_var restore =
      restore (withCreateProcess
        (proc "less" ["-eFRX"]){ std_in = CreatePipe, std_err = CreatePipe }
        (\(Just inh) _ (Just errh) ph ->
          do
            putMVar handle_var inh
            withAsync (consume errh) $ const $ void $ waitForProcess ph
          `finally` hClose errh))
      `onException` tryPutMVar handle_var stdout

    consume h = do
      s <- hGetContents h
      void $ evaluate s
