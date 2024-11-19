{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module TestData
  ( kitchenSink1
  , testFacts
  , testFacts1
  , testFacts2
  ) where

import Data.Default
import qualified Data.Set as Set

import Glean.Typed
import Glean.Types
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import qualified Glean.Schema.Sys.Types as Sys

rec :: Glean.Test.Rec
rec = def
    { Glean.Test.rec_beta = Glean.Test.Sum_wed True }

kitchenSink1 :: Glean.Test.KitchenSink
kitchenSink1 = def
  { Glean.Test.kitchenSink_byt = Byte 33
  , Glean.Test.kitchenSink_nat = Nat 42
  , Glean.Test.kitchenSink_array_of_byte = "xyz\0\0xff"
  , Glean.Test.kitchenSink_array_of_nat = map toNat [1,2]
  , Glean.Test.kitchenSink_array_of_string = ["abba", "baba"]
  , Glean.Test.kitchenSink_record_ = Glean.Test.KitchenSink_record_
    { Glean.Test.kitchenSink_record__a = Byte 34
    , Glean.Test.kitchenSink_record__b = Nat 35 }
  , Glean.Test.kitchenSink_named_record_ = rec
  , Glean.Test.kitchenSink_named_sum_ = Glean.Test.Sum_tue (Nat 37)
  , Glean.Test.kitchenSink_named_enum_ = Glean.Test.Enum__blue
  , Glean.Test.kitchenSink_maybe_ = Just def
  , Glean.Test.kitchenSink_bool_ = True
  , Glean.Test.kitchenSink_string_ = "Hello\0world!\0"
  , Glean.Test.kitchenSink_set_of_nat =
      Set.fromList [toNat 1, toNat 2, toNat 65535]
  , Glean.Test.kitchenSink_set_of_string = Set.fromList ["apa", "bepa"]
  }

mkTestFacts :: NewFact m => (m () -> m ()) -> (m () -> m ()) -> m ()
mkTestFacts first second = do
  sysBlobFact <- makeFact @Sys.Blob "hello"
  sysBlobFact2 <- makeFact @Sys.Blob "bye"

  cxxNameFact <- makeFact @Cxx.Name "foo"
  makeFact_ @Cxx.FunctionName $ Cxx.FunctionName_key_name cxxNameFact

  let mk_names xs = do
        names <- mapM (makeFact @Cxx.Name) xs
        mapM_
          (makeFact_ @Cxx.FunctionName . Cxx.FunctionName_key_name)
          names
  first $ mk_names
    [ "abba"
    , "anonymous"
    , "azimuth"
    , "blubber"
    , "book"
    ]
  second $ mk_names
    [ "abcd"
    , "allin"
    , "anywhere"
    , "barbie"
    , "blob"
    ]

  let
    kitchenSink2Term0 = kitchenSink1
      { Glean.Test.kitchenSink_pred = sysBlobFact
      , Glean.Test.kitchenSink_sum_ =
          Glean.Test.KitchenSink_sum__d sysBlobFact
      }

    -- also make a small variant of kitchenSink2Term0
    kitchenSink2Term0b = kitchenSink2Term0
      { Glean.Test.kitchenSink_nat = toNat 43
      , Glean.Test.kitchenSink_string_ = "acca"
      , Glean.Test.kitchenSink_array_of_nat = map toNat [3,4,5]
      , Glean.Test.kitchenSink_named_sum_ = Glean.Test.Sum_wed True
      , Glean.Test.kitchenSink_maybe_ = Nothing
      }

  kitchenSink2Fact0 <- makeFact @Glean.Test.Predicate kitchenSink2Term0
  kitchenSink2Fact0b <- makeFact @Glean.Test.Predicate kitchenSink2Term0b

  let
    -- refers to kitchenSink2Fact0 through the sum_ field, the
    -- array_of_pred field and the set_of_pred field.
    kitchenSink2Term1 = def
      { Glean.Test.kitchenSink_pred = sysBlobFact2
      , Glean.Test.kitchenSink_sum_ =
          Glean.Test.KitchenSink_sum__c kitchenSink2Fact0
      , Glean.Test.kitchenSink_array_of_pred = [ kitchenSink2Fact0 ]
      , Glean.Test.kitchenSink_string_ = "clobber"
      , Glean.Test.kitchenSink_array_of_nat = map toNat [6]
      , Glean.Test.kitchenSink_named_sum_ = Glean.Test.Sum_wed True
      , Glean.Test.kitchenSink_named_record_ = rec
      , Glean.Test.kitchenSink_maybe_ = Nothing
      , Glean.Test.kitchenSink_set_of_pred = Set.fromList [kitchenSink2Fact0]
      }

    kitchenSink2Term1b = kitchenSink2Term1
      { Glean.Test.kitchenSink_sum_ =
          Glean.Test.KitchenSink_sum__c kitchenSink2Fact0b
      , Glean.Test.kitchenSink_array_of_pred = [ kitchenSink2Fact0b ]
      , Glean.Test.kitchenSink_string_ = "flubber"
      , Glean.Test.kitchenSink_array_of_nat = []
      }

    mk_refs term = do
      p <- makeFact @Glean.Test.Predicate term
      r <- makeFact @Glean.Test.Ref p
      makeFact_ @Glean.Test.RefRef r

  first $ mk_refs kitchenSink2Term1
  second $ mk_refs kitchenSink2Term1b

  first $ makeFactV_ @Glean.Test.KeyValue
    (def
      { Glean.Test.keyValue_key_kstring = "hello"
      , Glean.Test.keyValue_key_knat = toNat 42 })
    (def
      { Glean.Test.keyValue_value_vnat = toNat 24
      , Glean.Test.keyValue_value_vstring = "world" })

  first $ makeFactV_ @Glean.Test.KeyValue
    (def
      { Glean.Test.keyValue_key_kstring = "foo"
      , Glean.Test.keyValue_key_knat = toNat 3 })
    (def
      { Glean.Test.keyValue_value_vnat = toNat 4
      , Glean.Test.keyValue_value_vstring = "bar" })

  second $ makeFactV_ @Glean.Test.KeyValue
    (def
      { Glean.Test.keyValue_key_kstring = "foo"
      , Glean.Test.keyValue_key_knat = toNat 5 })
    (def
      { Glean.Test.keyValue_value_vnat = toNat 5
      , Glean.Test.keyValue_value_vstring = "foo" })

  second $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "a" "b"
  second $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "b" "a"
  second $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "c" "d"
  second $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "e" "e"
  second $
    makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "xx" "xx"
  second $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "xy" "xz"

  a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
  b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
  c <- makeFact @Glean.Test.Node (Glean.Test.Node_key "c")
  d <- makeFact @Glean.Test.Node (Glean.Test.Node_key "d")
  e <- makeFact @Glean.Test.Node (Glean.Test.Node_key "e")
  f <- makeFact @Glean.Test.Node (Glean.Test.Node_key "f")
  g <- makeFact @Glean.Test.Node (Glean.Test.Node_key "g")

  first $ makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a b)
  second $ makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a c)
  second $ makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key b d)
  second $ makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key c d)

  d_ <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key d Nothing Nothing)
  c_ <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key c (Just d_) Nothing)
  b_ <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key b Nothing (Just d_))
  a_ <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key a (Just b_) (Just c_))
  g_ <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key g Nothing Nothing)
  f_ <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key f Nothing (Just g_))
  e_ <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key e (Just f_) Nothing)

  first $ makeFactV_ @Glean.Test.TreeToTree a_ e_

  qux1 <- makeFact @Glean.Test.Qux "qux1"
  bar1 <- makeFactV @Glean.Test.Bar "bar1" qux1
  foo1 <- makeFactV @Glean.Test.Foo "foo1" bar1
  qux2 <- makeFact @Glean.Test.Qux "qux2"
  bar2 <- makeFactV @Glean.Test.Bar "bar2" qux2
  foo2 <- makeFactV @Glean.Test.Foo "foo2" bar2
  first $ makeFactV_ @Glean.Test.FooToFoo foo1 foo2

testFacts :: NewFact m => m ()
testFacts = mkTestFacts id id

testFacts1 :: NewFact m => m ()
testFacts1 = mkTestFacts id (const $ return ())

testFacts2 :: NewFact m => m ()
testFacts2 = mkTestFacts (const $ return ()) id
