{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Exception
  ( DBError(..)
  , dbException
  , dbError
  ) where

import Control.Exception
import Control.Monad.Catch (MonadThrow(..))

import Glean.Types hiding (Exception)
import Glean.Database.Repo

newtype DBError = DBError String

instance Show DBError where
  show (DBError s) = s

instance Exception DBError

dbException :: Repo -> String -> DBError
dbException repo = DBError . inRepo repo

dbError :: MonadThrow m => Repo -> String -> m a
dbError repo msg = throwM $ dbException repo msg
