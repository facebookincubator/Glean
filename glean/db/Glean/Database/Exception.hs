{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Exception
  ( DBError(..)
  , DBAlreadyExists(..)
  , dbException
  , dbError
  ) where

import Control.Exception
import Control.Monad.Catch (MonadThrow(..))
import Data.Typeable (Typeable, cast)

import Glean.Types hiding (Exception)
import Glean.Database.Repo

data DBError where
  DBError :: String -> DBError
  SomeDBError :: Exception e => e -> DBError

instance Show DBError where
  show (DBError s) = s
  show (SomeDBError e) = show e

instance Exception DBError

dbException :: Repo -> String -> DBError
dbException repo = DBError . inRepo repo

dbError :: MonadThrow m => Repo -> String -> m a
dbError repo msg = throwM $ dbException repo msg

toDBError :: Exception e => e -> SomeException
toDBError = toException . SomeDBError

fromDBError :: Typeable b => SomeException -> Maybe b
fromDBError e = do
    SomeDBError e <- fromException e
    cast e

data DBAlreadyExists = DBAlreadyExists
  deriving Show

instance Exception DBAlreadyExists where
  fromException = fromDBError
  toException = toDBError
