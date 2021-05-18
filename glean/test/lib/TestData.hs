{-# LANGUAGE TypeApplications #-}
module TestData
  ( kitchenSink1
  , testFacts
  , testFacts1
  , testFacts2
  ) where

import Data.Default

import Glean.Typed
import Glean.Types
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import qualified Glean.Schema.Sys.Types as Sys

kitchenSink1 :: Glean.Test.KitchenSink_1
kitchenSink1 = def
  { Glean.Test.kitchenSink_1_byt = Byte 33
  , Glean.Test.kitchenSink_1_nat = Nat 42
  , Glean.Test.kitchenSink_1_array_of_byte = "xyz\0\0xff"
  , Glean.Test.kitchenSink_1_array_of_nat = [Nat 99,Nat 98]
  , Glean.Test.kitchenSink_1_record_ = Glean.Test.KitchenSink_1_record_
    { Glean.Test.kitchenSink_1_record__a = Byte 34
    , Glean.Test.kitchenSink_1_record__b = Nat 35 }
  , Glean.Test.kitchenSink_1_sum_ = Glean.Test.KitchenSink_1_sum__d (Nat 36)
  , Glean.Test.kitchenSink_1_named_record_ = Glean.Test.Rec
    { Glean.Test.rec_alpha = Glean.Test.Enum_green
    , Glean.Test.rec_beta = Glean.Test.Sum_wed True
    }
  , Glean.Test.kitchenSink_1_named_sum_ = Glean.Test.Sum_tue (Nat 37)
  , Glean.Test.kitchenSink_1_named_enum_ = Glean.Test.Enum_blue
  , Glean.Test.kitchenSink_1_pred = Sys.Blob { blob_id = 3, blob_key = Nothing }
  , Glean.Test.kitchenSink_1_maybe_ = Just def
  , Glean.Test.kitchenSink_1_bool_ = True
  , Glean.Test.kitchenSink_1_string_ = "Hello\0world!\0"
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

  let kitchenSinkTerm0 = def
        { Glean.Test.kitchenSink_1_pred = sysBlobFact }

  first $ makeFact_ @Glean.Test.Predicate_1 kitchenSinkTerm0

  let kitchenSinkTerm1 = kitchenSink1
        { Glean.Test.kitchenSink_1_pred = sysBlobFact2 }

  second $ makeFact_ @Glean.Test.Predicate_1 kitchenSinkTerm1

  let
    kitchenSink2Term0 = def
      { Glean.Test.kitchenSink_pred = sysBlobFact
      , Glean.Test.kitchenSink_sum_ =
          Glean.Test.KitchenSink_sum__d sysBlobFact
      , Glean.Test.kitchenSink_string_ = "abba"
      , Glean.Test.kitchenSink_array_of_nat = map toNat [1,2] }

    -- also make a small variant of kitchenSink2Term0
    kitchenSink2Term0b = kitchenSink2Term0
      { Glean.Test.kitchenSink_nat = toNat 42
      , Glean.Test.kitchenSink_string_ = "acca"
      , Glean.Test.kitchenSink_array_of_nat = map toNat [3,4,5] }

  kitchenSink2Fact0 <- makeFact @Glean.Test.Predicate kitchenSink2Term0
  kitchenSink2Fact0b <- makeFact @Glean.Test.Predicate kitchenSink2Term0b

  let
    -- refers to kitchenSink2Fact0 through the sum_ field and the
    -- array_of_pred field
    kitchenSink2Term1 = def
      { Glean.Test.kitchenSink_pred = sysBlobFact2
      , Glean.Test.kitchenSink_sum_ =
          Glean.Test.KitchenSink_sum__c kitchenSink2Fact0
      , Glean.Test.kitchenSink_array_of_pred = [ kitchenSink2Fact0 ]
      , Glean.Test.kitchenSink_string_ = "clobber"
      , Glean.Test.kitchenSink_array_of_nat = map toNat [6] }

    kitchenSink2Term1b = kitchenSink2Term1
      { Glean.Test.kitchenSink_sum_ =
          Glean.Test.KitchenSink_sum__c kitchenSink2Fact0b
      , Glean.Test.kitchenSink_array_of_pred = [ kitchenSink2Fact0b ]
      , Glean.Test.kitchenSink_string_ = "flubber"
      , Glean.Test.kitchenSink_array_of_nat = [] }

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

  first $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "a" "b"
  second $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "b" "a"
  second $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "c" "d"
  first $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "e" "e"
  second $
    makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "xx" "xx"
  first $ makeFact_ @Glean.Test.StringPair $ Glean.Test.StringPair_key "xy" "xz"

  a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
  b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
  c <- makeFact @Glean.Test.Node (Glean.Test.Node_key "c")
  d <- makeFact @Glean.Test.Node (Glean.Test.Node_key "d")

  first $ makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a b)
  second $ makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a c)
  second $ makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key b d)
  second $ makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key c d)

  d <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key "d" Nothing Nothing)
  c <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key "c" (Just d) Nothing)
  b <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key "b" Nothing (Just d))
  a <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key "a" (Just b) (Just c))
  g <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key "g" Nothing Nothing)
  f <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key "f" Nothing (Just g))
  e <- makeFact @Glean.Test.Tree (Glean.Test.Tree_key "e" (Just f) Nothing)

  first $ makeFactV_ @Glean.Test.TreeToTree a e

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
