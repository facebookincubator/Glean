{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Bisect (
  Bisect(..), BisectM, execBisectM, bisect
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.Class (lift)

data Bisect a b err = Bisect
  { bisectProcess :: [a] -> IO (Either err b)
  , bisectFailed :: err -> a -> IO ()
    -- ^ called if the bisect successfully identified a failing item,
    -- regardless of whether we continued or aborted afterwards.
  , bisectSplitting :: Int -> Int -> [a] -> IO ()
  }

newtype BisectM err a = BisectM (ExceptT err (State.StateT Int IO) a)
  deriving(Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

-- | Run a bisect operation
execBisectM
  :: Int
     -- ^ Number of failures that we will bisect down the failing item
     -- and continue.
  -> BisectM err a
     -- ^ e.g. a call to 'bisect'
  -> IO (Either err a)
execBisectM allowed (BisectM action) =
  State.evalStateT (runExceptT action) allowed

bisect
  :: Bisect a b err
  -> [a]
  -> BisectM (err,[a]) [b]
     -- ^ On abort, returns the last error and the list of items
bisect !bis xs = BisectM $ attempt 0 (length xs) xs
  where
    attempt k n xs = do
      r <- liftIO $ bisectProcess bis xs
      case r of
        Right x -> return [x]
        Left err -> do
          e <- lift State.get
          case xs of
            [x] -> liftIO $ bisectFailed bis err x
            _ -> return ()
          if e > 0
            then split k n xs
            else throwE (err,xs)

    split _ _ [] = return []
    split _ _ [_] = do
      lift $ State.modify' (subtract 1)
      return []
    split k n xs = do
      liftIO $ bisectSplitting bis k n xs
      (++) <$> attempt k m ys <*> attempt (k+m) (n-m) zs
      where
        m = n `div` 2
        (ys,zs) = splitAt m xs
