{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-
// @lint-ignore-every HSMODULEHEADER
-}

import Data.Text.Prettyprint.Doc

instance Pretty PredicateRef where
  pretty (PredicateRef n v) = pretty n <> "." <> pretty v

instance Pretty TypeRef where
  pretty (TypeRef n v) = pretty n <> "." <> pretty v

instance Prelude.Enum Nat where
    succ = Nat . Prelude.succ . unNat
    pred = Nat . Prelude.pred . unNat
    toEnum = Nat . Prelude.toEnum
    fromEnum = Prelude.fromEnum . unNat

instance Prelude.Enum Byte where
    succ = Byte . Prelude.succ . unByte
    pred = Byte . Prelude.pred . unByte
    toEnum = Byte . Prelude.toEnum
    fromEnum = Prelude.fromEnum . unByte

instance Prelude.Semigroup PredicateStats where
  x <> y = PredicateStats
    { predicateStats_count
      = predicateStats_count x Prelude.+ predicateStats_count y
    , predicateStats_size
      = predicateStats_size x Prelude.+ predicateStats_size y
    }
