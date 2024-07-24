{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveFunctor #-}

module Glean.Query.Incremental
  ( makeIncremental
  )
  where

import Data.Maybe (mapMaybe, catMaybes)

import Glean.Query.Codegen.Types
import Glean.RTS.Types (Pid, PidRef(..))
import Glean.RTS.Term

-- | Make a predicate derivation query efficiently derive new facts on an
-- incremental database.
-- See Note [Incremental derivation - Strategy]
makeIncremental
  :: (SeekSection -> Pid -> Bool)
  -- ^ whether a predicate has facts in the section or not.
  -> CodegenQuery
  -> CodegenQuery
makeIncremental hasFacts (QueryWithInfo (CgQuery h body) v t) =
 QueryWithInfo (CgQuery h (expandGenerators hasFacts body)) v t

-- | Represents the output from expanding statements such that they will
-- perform a combination of querying the base and incremental database.
data Incremental a = Incremental
  { seekingStacked :: Maybe a
    -- ^ invariant: contains at least one generator with SeekOnStacked
  , seekingOnlyBase :: Maybe a
    -- ^ invariant: contains 0 or more generators and all with SeekOnBase
  }
  deriving (Functor)

falseStmt  :: CgStatement
falseStmt = CgDisjunction []

-- | See Note [Incremental derivation - Implementation]
expandGenerators
  :: (SeekSection -> Pid -> Bool)
  -> [CgStatement]
  -> [CgStatement]
expandGenerators hasFacts stmts =
  case seekingStacked $ expandSequence stmts of
    Nothing -> [falseStmt]
    Just xs -> xs
  where
  expandSequence :: [CgStatement] -> Incremental [CgStatement]
  expandSequence [] = Incremental Nothing (Just [])
  expandSequence (x:xs) = Incremental new old
    where
      Incremental mnew mold = expandStatement x
      Incremental mnews molds = expandSequence xs
      new = nonnull $ disjunctionToSequence $ catMaybes
        [ do new <- mnew
             return (new:xs)
        , do old <- mold
             news <- mnews
             return (old:news)
        ]
      old = do
        old <- mold
        olds <- molds
        return (old:olds)

  expandStatement :: CgStatement -> Incremental CgStatement
  expandStatement stmt = case stmt of
    CgStatement pat (FactGenerator pref key val SeekOnAllFacts)
      -- when pat is known, this is a lookup not a seek so we
      -- don't need to expand it.
      | not (known pat) ->
      let PidRef pid _ = pref
          generator where_ =
            if hasFacts where_ pid
            then Just $ CgStatement pat (FactGenerator pref key val where_)
            else Nothing
      in
      Incremental (generator SeekOnStacked) (generator SeekOnBase)
    CgStatement{} ->
      Incremental Nothing (Just stmt)
    CgAllStatement{} -> error "Set"
    CgDisjunction stmtss ->
      CgDisjunction <$> expandDisjunction stmtss
    CgNegation{} ->
      error "unexpected negation in stored derived predicate"
    CgConditional{} ->
      error "unexpected if statement in stored derived predicate"

  known (Ref MatchFid{}) = True
  known (Ref MatchVar{}) = True
  known Nat{} = True
  known (Ref (MatchAnd p q)) = known p || known q
  known _ = False

  expandDisjunction :: [[CgStatement]] -> Incremental [[CgStatement]]
  expandDisjunction branches = Incremental (nonnull news) (nonnull olds)
    where
      ts = map expandSequence branches
      news = mapMaybe seekingStacked ts
      olds = mapMaybe seekingOnlyBase ts

  disjunctionToSequence ::  [[CgStatement]] -> [CgStatement]
  disjunctionToSequence = \case
    [] -> []
    [x] -> x
    xs -> [CgDisjunction xs]

  nonnull :: [a] -> Maybe [a]
  nonnull [] = Nothing
  nonnull xs = Just xs

{- Note [Incremental derivation - Strategy]

When deriving stored predicates on stacked databases we want to make sure that
we only derive facts that use the newly available data and that we are not
re-deriving everything that was derived before.

We can do this by making sure that the derivation query will always look at
fact combinations involving new facts in the stacked database.

We could require that at least one field of new derived facts come from the
stacked database, but that would be too restrictive. Consider this case

  predicate Path2 { a: Node, b: Node }
    { A, B } where
      Edge { A, K };
      Edge { K, B }

New edges should entail new `Path2` facts despite `Edge` not being part of
`Path2`'s key or value.

Instead we require that at least one FactGenerator in every query path
be restricted only to the stacked db.

-}

{- Note [Incremental derivation - Implementation]

When running derivations in an incremental database we want to enforce that:

 1. Every derived predicate be generated using some data coming from the new
    increment fact set.

 2. We must not repeat the derivation process already performend on the base db.

This can be achieved by querying all possible combinations of seeking on base
and incremental db for every fact generator. Rule 2 can be enforced by not
running the case where all generators query the base db. This is key as we
expect that branch to take much longer than all others.

We may try to transform each fact generator into a disjunction. Given the query:

   P A; Q B; R C;

naively transform it into

   (Pbase A | Pincr A);
   (Pbase B | Pincr B);
   (Rbase C | Rincr C);

This is problematic becase we cannot remove the case where all generators query
the base db (rule 2).

We may try another approach. Generate all possible combinations and drop the
unwanted case.

   (Pbase A; Qbase B; Rbase C)  -- this one is dropped
   (Pbase A; Qbase B; Rincr C)
   (Pbase A; Qincr B; Rbase C)
   (Pbase A; Qincr B; Rincr C)
   (Pincr A; Qbase B; Rbase C)
   (Pincr A; Qincr B; Rbase C)
   (Pincr A; Qbase B; Rincr C)
   (Pincr A; Qincr B; Rincr C)

this has two problems: the bytecode size complexity is O(2^n) and it is
inefficient as we are not doing any sharing between different paths.

Finally, we opt for the approach of making a tree out of it by branching on
every fact generator.

   ( Pincr A;
     ( Qbase B;
         ( Rbase C
         | Rincr C
         )
     ) |
     ( Qincr B;
         ( Rbase C
         | Rincr C
         )
     )
   ) |
   ( Pbase A;
     ( Qbase B;
         ( Rbase C -- we remove this node to have no branch querying only base
         | Rincr C
         )
     ) |
     ( Qincr B;
         ( Rbase C
         | Rincr C
         )
     )
   )

The bytecode size is still O(2^n) but the query is more efficient as we now
have more sharing.
We optimise it by realising that (Pincr A | Pbase A) == P A.

   ( Pincr A; Q B; R C ) |
   ( Pbase A;
      ( Qincr B; R C) |
      ( Qbase B; Rincr C)
   )

This leads fact generator expansion to two cases
  * Seek on increment db and proceed with the original query
  * Seek on the base db and process the rest of the query further.

The resulting algorithm is O(n) in time and space, but the output tree will
generate bytecode that is O(n^2) where n is the number of fact generators in
the longest query branch.

## Handling disjunctions

To have all possible combinations of seeking base and stacked dbs for
disjunctions we will need to separate the case where all branches seek
only on base from the other cases.

  (P A | Q B); R C;

becomes

     ( ( Pincr A | Qincr B ); R C )
   | ( ( Pbase A | Qbase B ); Rincr C )

-}
