{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-
// @lint-ignore-every HSMODULEHEADER
-}

import Compat.Prettyprinter

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

instance Prelude.Semigroup UserQueryStats where
  (<>)
    UserQueryStats
      { userQueryStats_num_facts = num_facts1
      , userQueryStats_elapsed_ns = elapsed_ns1
      , userQueryStats_allocated_bytes = allocated_bytes1
      , userQueryStats_facts_searched = facts_searched1
      , userQueryStats_compile_time_ns = compile_time_ns1
      , userQueryStats_bytecode_size = bytecode_size1
      , userQueryStats_execute_time_ns = execute_time_ns1
      , userQueryStats_result_count = result_count1
      , userQueryStats_codegen_time_ns = codegen_time_ns1
      , userQueryStats_full_scans = full_scans1
      , userQueryStats_result_bytes = result_bytes1
      }
    UserQueryStats
      { userQueryStats_num_facts = num_facts2
      , userQueryStats_elapsed_ns = elapsed_ns2
      , userQueryStats_allocated_bytes = allocated_bytes2
      , userQueryStats_facts_searched = facts_searched2
      , userQueryStats_compile_time_ns = compile_time_ns2
      , userQueryStats_bytecode_size = bytecode_size2
      , userQueryStats_execute_time_ns = execute_time_ns2
      , userQueryStats_result_count = result_count2
      , userQueryStats_codegen_time_ns = codegen_time_ns2
      , userQueryStats_full_scans = full_scans2
      , userQueryStats_result_bytes = result_bytes2
      }
    = UserQueryStats
      { userQueryStats_num_facts = num_facts1 Prelude.+ num_facts2
      , userQueryStats_elapsed_ns = elapsed_ns1 Prelude.+ elapsed_ns2
      , userQueryStats_allocated_bytes =
          allocated_bytes1 Prelude.+ allocated_bytes2
      , userQueryStats_facts_searched =
          fMaybe (Map.unionWith (Prelude.+))
            facts_searched1 facts_searched2
      , userQueryStats_compile_time_ns =
          fMaybe (Prelude.+) compile_time_ns1 compile_time_ns2
      , userQueryStats_bytecode_size =
          bytecode_size1 `Prelude.max` bytecode_size2
      , userQueryStats_execute_time_ns =
          fMaybe (Prelude.+) execute_time_ns1 execute_time_ns2
      , userQueryStats_result_count =
          result_count1 Prelude.+ result_count2
      , userQueryStats_codegen_time_ns =
          fMaybe (Prelude.+) codegen_time_ns1 codegen_time_ns2
      , userQueryStats_full_scans =
          List.nub (full_scans1 <> full_scans2)
      , userQueryStats_result_bytes =
          fMaybe (Prelude.+) result_bytes1 result_bytes2
      }
      where
      fMaybe _ Prelude.Nothing a = a
      fMaybe _ a Prelude.Nothing = a
      fMaybe f (Prelude.Just a) (Prelude.Just b) = Prelude.Just (f a b)
