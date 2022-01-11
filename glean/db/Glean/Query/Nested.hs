{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Nested
  ( refutableNested
  , getExpandPids
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Glean.Query.Nested.Types
import Glean.RTS as RTS
import Glean.Database.Schema
import Glean.RTS.Term as RTS


-- | Returns 'True' if the 'Term' may fail to match.
refutable :: (r -> Bool) -> Term (Match r) -> Bool
refutable _ (Ref Wildcard) = False
refutable _ (Ref Variable) = False
refutable f (Ref (MatchTerm r)) = f r
refutable f (Tuple xs) = any (refutable f) xs  -- unlifted tuples!
refutable _ _ = True

-- | Returns 'True' if the nested term may fail to match
refutableNested :: Term (Match (Nested Fid)) -> Bool
refutableNested = refutable f where
  f (NestedRef _) = True
  f (NestedPred _ (Just _) _) = True
  f (NestedPred _ Nothing (Just t)) = refutableNested t
  f (NestedSum SumMatchAny _) = False
    -- alts of SumMatchAny are required to be irrefutable
  f (NestedSum SumMatchThese alts) = any check alts
    where check Nothing = True -- a missing alt is refutable
          check (Just t) = refutableNested t
  f (NestedArray t) = refutableNested t
  f _ = False

getExpandPids :: Term (Match (Nested Fid)) -> Set Pid
getExpandPids = foldr f Set.empty
  where
  f (MatchTerm (NestedPred d _ term)) pids =
    let pids' = Set.insert (predicatePid d) pids in
    foldr (flip (foldr f)) pids' term
  f (MatchTerm (NestedSum _ alts)) pids =
    foldr (flip (foldr (flip (foldr f)))) pids alts
  f (MatchTerm (NestedArray t)) pids = foldr f pids t
  f _ pids = pids
