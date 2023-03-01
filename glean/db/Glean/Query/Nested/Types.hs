{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Types for nested (JSON) queries
module Glean.Query.Nested.Types
  ( Nested(..)
  , SumMatchMode(..)
  ) where

import Data.Maybe
import Data.Text.Prettyprint.Doc hiding ((<>))

import Glean.Database.Schema
import Glean.Display
import Glean.RTS.Term as RTS


-- | @Term (Match (Nested r))@ represents queries over nested facts.
data Nested r
  = NestedRef r
    -- ^ This was @{ "id" = N }@ in the original query, match it in the parent
  | NestedPred
      PredicateDetails
      (Maybe [r])
        -- Just facts that this nested query resolved to, or Nothing if
        -- the nested query was irrefutable.
      (Maybe (Term (Match (Nested r))))
    -- ^ This was @{ }@ or @{ "key" = pat }@ in the original query.
  | NestedSum SumMatchMode [Maybe (Term (Match (Nested r)))]
    -- ^ Matching against sum types
  | NestedArray (Term (Match (Nested r)))
    -- ^ Match each element of an array

data SumMatchMode
  = SumMatchThese
     -- ^ must match one of the specified alternatives. Currently only
     -- supported with a single alternative.
  | SumMatchAny
     -- ^ matches any alternative. The alts specify additional
     -- structure to fetch and/or nested queries.

instance Display r => Display (Nested r) where
  display opts (NestedRef r) = display opts r
  display opts (NestedPred _ _ maybeTerm) =
    "*" <> maybe emptyDoc (parens . display opts) maybeTerm
  display opts (NestedSum match alts) =
    surround $ hcat $ punctuate "|" $
      map (display opts . fromMaybe (Ref Wildcard)) alts
    where
     surround = case match of
       SumMatchThese -> angles
       SumMatchAny -> parens
  display opts (NestedArray t) = "[" <> display opts t <> ",...]"
