{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Glean.Haxl.Repos
  ( ReposHaxl
  , RepoHaxl
  , queryAllRepos
  , withRepo
  , queryEachRepo
  , runHaxlAllRepos
  ) where

import Haxl.Core hiding (Env, runHaxl)
import Haxl.DataSource.Glean (HasRepo(..))
import qualified Haxl.DataSource.Glean
import Data.List.NonEmpty
import Glean.Types
import Glean.Backend.Remote (Backend)
import qualified Glean.Haxl as Glean


-- Searching across repos --------

type ReposHaxl u w a = QueryRepos u => GenHaxl u w a
type RepoHaxl u w a = Haxl.DataSource.Glean.HasRepo u => GenHaxl u w a

-- This type has instances for both HasRepo and HasRepos.
-- The instance `HasRepo` allows us to use it as the userState
-- but its not exported so the user doesn't accidently run a query over
-- only the first repo. Instead the user should just depend on the
-- QueryRepos constraint
newtype Repos u = Repos u

class HasRepos us where
  headRepoFromUserEnv :: us -> Repo
  tailReposFromUserEnv :: us -> Maybe us
  singleRepo :: Repo -> us

instance HasRepos (NonEmpty Repo) where
  headRepoFromUserEnv (x:|_) = x
  tailReposFromUserEnv (_ :| []) = Nothing
  tailReposFromUserEnv (_ :| x : xs) = Just $ x :| xs
  singleRepo repo = repo :| []

instance HasRepos u => HasRepos (Repos u) where
  headRepoFromUserEnv (Repos xs) = headRepoFromUserEnv xs
  tailReposFromUserEnv (Repos xs) = Repos <$> tailReposFromUserEnv xs
  singleRepo repo = Repos $ singleRepo repo

instance HasRepos u => HasRepo (Repos u) where
  getRepoFromUserEnv (Repos xs) = headRepoFromUserEnv xs

class QueryRepos u where
  queryEachRepo :: (HasRepo u => GenHaxl u w q) -> GenHaxl u w [q]
  withRepo :: Repo -> RepoHaxl u w q -> ReposHaxl u w q

instance (HasRepos u) => QueryRepos (Repos u) where
  queryEachRepo act = do
    head <- act
    userEnv <- env userEnv
    tail <- maybe
      (return [])
      (\r -> Haxl.DataSource.Glean.withRepo r $ queryEachRepo act)
      (tailReposFromUserEnv userEnv)
    return $ head:tail
  withRepo repo act = Haxl.DataSource.Glean.withRepo (singleRepo repo) act

queryAllRepos
  :: RepoHaxl u w [q]
  -> ReposHaxl u w [q]
queryAllRepos act = concat <$> queryEachRepo act

runHaxlAllRepos
  :: (HasRepos u)
  => Backend be
  => be
  -> u
  -> (forall u'. ReposHaxl u' w a)
  -> IO a
runHaxlAllRepos backend u h = Glean.runHaxl backend (Repos u) h
