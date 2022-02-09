{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Haxl.DataSource.Glean.Repos
  ( queryAllRepos
  ) where

import Haxl.Core hiding (Env)
import Haxl.DataSource.Glean
import Data.List.NonEmpty
import Glean.Types
import Glean.Backend.Remote (Backend)


-- Searching across repos --------

-- This type has instances for both HasRepo and HasRepos.
-- The instance `HasRepo` allows us to use it as the userState
-- but its not exported so the user doesn't accidently run a query over
-- only the first repo. Instead the user should just depend on the
-- QueryRepos constraint
newtype Repos u = Repos u

class HasRepos us where
  headRepoFromUserEnv :: us -> Repo
  tailRepoFromUserEnv :: us -> Maybe us

instance HasRepos (NonEmpty Repo) where
  headRepoFromUserEnv (x:|_) = x
  tailRepoFromUserEnv (_ :| []) = Nothing
  tailRepoFromUserEnv (_ :| x:xs) = Just $ x:|xs

instance HasRepos u => HasRepos (Repos u) where
  headRepoFromUserEnv (Repos xs) = headRepoFromUserEnv xs
  tailRepoFromUserEnv (Repos xs) = Repos <$> tailRepoFromUserEnv xs

instance HasRepos u => HasRepo (Repos u) where
  getRepoFromUserEnv (Repos xs) = headRepoFromUserEnv xs

class QueryRepos u where
  queryEachRepos :: (HasRepo u => GenHaxl u w [q]) -> GenHaxl u w [[q]]

queryAllRepos
  :: QueryRepos u
  => (HasRepo u => GenHaxl u w [q])
  -> GenHaxl u w [q]
queryAllRepos act = concat <$> queryEachRepos act

instance (HasRepos u, HasRepo u) => QueryRepos (Repos u) where
  queryEachRepos act = do
    head <- act
    userEnv <- env userEnv
    tail <- maybe
      (return [])
      (\r -> withRepo r $ queryEachRepos act)
      (tailRepoFromUserEnv userEnv)
    return $ head:tail

runHaxl
  :: (HasRepos u, QueryRepos u')
  => Backend be
  => be
  -> u
  -> GenHaxl u' w a
  -> IO a
runHaxl backend u h = Glean.runHaxl (Repos u) h
