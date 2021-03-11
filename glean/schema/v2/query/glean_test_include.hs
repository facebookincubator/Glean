-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.CodeCxx.Types
import qualified Glean.Schema.Query.CodeCxx.Types

import qualified Glean.Schema.Pp1.Types
import qualified Glean.Schema.Query.Pp1.Types

import qualified Glean.Schema.Sys.Types
import qualified Glean.Schema.Query.Sys.Types

import qualified Glean.Schema.GleanTest.Types


type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RevStringPair_key = Glean.Schema.GleanTest.Types.RevStringPair_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RevStringPair_key = Glean.Schema.Query.GleanTest.Types.RevStringPair_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RevStringPair_key where
  toQuery (Glean.Schema.GleanTest.Types.RevStringPair_key x1 x2) = Glean.Schema.Query.GleanTest.Types.RevStringPair_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.RevStringPair where
  toQueryId = Glean.Schema.Query.GleanTest.Types.RevStringPair_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.RevStringPair_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RevStringPair = Glean.Schema.GleanTest.Types.RevStringPair
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RevStringPair = Glean.Schema.Query.GleanTest.Types.RevStringPair

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RevStringPair

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.LeftOr_key = Glean.Schema.GleanTest.Types.LeftOr_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.LeftOr_key = Glean.Schema.Query.GleanTest.Types.LeftOr_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.LeftOr_key where
  toQuery (Glean.Schema.GleanTest.Types.LeftOr_key x1 x2) = Glean.Schema.Query.GleanTest.Types.LeftOr_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.LeftOr where
  toQueryId = Glean.Schema.Query.GleanTest.Types.LeftOr_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.LeftOr_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.LeftOr = Glean.Schema.GleanTest.Types.LeftOr
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.LeftOr = Glean.Schema.Query.GleanTest.Types.LeftOr

instance Glean.ToQuery Glean.Schema.GleanTest.Types.LeftOr

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Tree_key = Glean.Schema.GleanTest.Types.Tree_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Tree_key = Glean.Schema.Query.GleanTest.Types.Tree_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Tree_key where
  toQuery (Glean.Schema.GleanTest.Types.Tree_key x1 x2 x3) = Glean.Schema.Query.GleanTest.Types.Tree_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.GleanTest.Types.tree_left_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.GleanTest.Types.tree_left_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.GleanTest.Types.tree_right_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.GleanTest.Types.tree_right_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Tree where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Tree_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Tree_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Tree = Glean.Schema.GleanTest.Types.Tree
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Tree = Glean.Schema.Query.GleanTest.Types.Tree

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Tree

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.StoredRevStringPairWithA_key = Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key = Glean.Schema.Query.GleanTest.Types.StoredRevStringPairWithA_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key where
  toQuery (Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key x1 x2) = Glean.Schema.Query.GleanTest.Types.StoredRevStringPairWithA_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.StoredRevStringPairWithA where
  toQueryId = Glean.Schema.Query.GleanTest.Types.StoredRevStringPairWithA_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.StoredRevStringPairWithA_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.StoredRevStringPairWithA = Glean.Schema.GleanTest.Types.StoredRevStringPairWithA
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.StoredRevStringPairWithA = Glean.Schema.Query.GleanTest.Types.StoredRevStringPairWithA

instance Glean.ToQuery Glean.Schema.GleanTest.Types.StoredRevStringPairWithA

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Ref where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Ref_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Ref_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Ref = Glean.Schema.GleanTest.Types.Ref
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Ref = Glean.Schema.Query.GleanTest.Types.Ref

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Ref

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Bar where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Bar_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Bar_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Bar = Glean.Schema.GleanTest.Types.Bar
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Bar = Glean.Schema.Query.GleanTest.Types.Bar

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Bar

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.DerivedKeyValue_key = Glean.Schema.GleanTest.Types.DerivedKeyValue_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.DerivedKeyValue_key = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.DerivedKeyValue_key where
  toQuery (Glean.Schema.GleanTest.Types.DerivedKeyValue_key x1 x2 x3 x4) = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.DerivedKeyValue where
  toQueryId = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.DerivedKeyValue = Glean.Schema.GleanTest.Types.DerivedKeyValue
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.DerivedKeyValue = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue

instance Glean.ToQuery Glean.Schema.GleanTest.Types.DerivedKeyValue

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.ViaStringPair_key = Glean.Schema.GleanTest.Types.ViaStringPair_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.ViaStringPair_key = Glean.Schema.Query.GleanTest.Types.ViaStringPair_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.ViaStringPair_key where
  toQuery (Glean.Schema.GleanTest.Types.ViaStringPair_key x1 x2) = Glean.Schema.Query.GleanTest.Types.ViaStringPair_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.ViaStringPair where
  toQueryId = Glean.Schema.Query.GleanTest.Types.ViaStringPair_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.ViaStringPair_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.ViaStringPair = Glean.Schema.GleanTest.Types.ViaStringPair
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.ViaStringPair = Glean.Schema.Query.GleanTest.Types.ViaStringPair

instance Glean.ToQuery Glean.Schema.GleanTest.Types.ViaStringPair

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.LeftOr2_key = Glean.Schema.GleanTest.Types.LeftOr2_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.LeftOr2_key = Glean.Schema.Query.GleanTest.Types.LeftOr2_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.LeftOr2_key where
  toQuery (Glean.Schema.GleanTest.Types.LeftOr2_key x1 x2) = Glean.Schema.Query.GleanTest.Types.LeftOr2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.LeftOr2 where
  toQueryId = Glean.Schema.Query.GleanTest.Types.LeftOr2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.LeftOr2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.LeftOr2 = Glean.Schema.GleanTest.Types.LeftOr2
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.LeftOr2 = Glean.Schema.Query.GleanTest.Types.LeftOr2

instance Glean.ToQuery Glean.Schema.GleanTest.Types.LeftOr2

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.StringPair_key = Glean.Schema.GleanTest.Types.StringPair_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.StringPair_key = Glean.Schema.Query.GleanTest.Types.StringPair_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.StringPair_key where
  toQuery (Glean.Schema.GleanTest.Types.StringPair_key x1 x2) = Glean.Schema.Query.GleanTest.Types.StringPair_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.StringPair where
  toQueryId = Glean.Schema.Query.GleanTest.Types.StringPair_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.StringPair_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.StringPair = Glean.Schema.GleanTest.Types.StringPair
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.StringPair = Glean.Schema.Query.GleanTest.Types.StringPair

instance Glean.ToQuery Glean.Schema.GleanTest.Types.StringPair

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Name where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Name_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Name_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Name = Glean.Schema.GleanTest.Types.Name
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Name = Glean.Schema.Query.GleanTest.Types.Name

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Name

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.StringPairBox_key = Glean.Schema.GleanTest.Types.StringPairBox_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.StringPairBox_key = Glean.Schema.Query.GleanTest.Types.StringPairBox_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.StringPairBox_key where
  toQuery (Glean.Schema.GleanTest.Types.StringPairBox_key x1) = Glean.Schema.Query.GleanTest.Types.StringPairBox_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.StringPairBox where
  toQueryId = Glean.Schema.Query.GleanTest.Types.StringPairBox_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.StringPairBox_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.StringPairBox = Glean.Schema.GleanTest.Types.StringPairBox
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.StringPairBox = Glean.Schema.Query.GleanTest.Types.StringPairBox

instance Glean.ToQuery Glean.Schema.GleanTest.Types.StringPairBox

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.ReflStringPair where
  toQueryId = Glean.Schema.Query.GleanTest.Types.ReflStringPair_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.ReflStringPair_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.ReflStringPair = Glean.Schema.GleanTest.Types.ReflStringPair
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.ReflStringPair = Glean.Schema.Query.GleanTest.Types.ReflStringPair

instance Glean.ToQuery Glean.Schema.GleanTest.Types.ReflStringPair

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.StoredRevStringPair_key = Glean.Schema.GleanTest.Types.StoredRevStringPair_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.StoredRevStringPair_key = Glean.Schema.Query.GleanTest.Types.StoredRevStringPair_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.StoredRevStringPair_key where
  toQuery (Glean.Schema.GleanTest.Types.StoredRevStringPair_key x1 x2) = Glean.Schema.Query.GleanTest.Types.StoredRevStringPair_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.StoredRevStringPair where
  toQueryId = Glean.Schema.Query.GleanTest.Types.StoredRevStringPair_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.StoredRevStringPair_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.StoredRevStringPair = Glean.Schema.GleanTest.Types.StoredRevStringPair
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.StoredRevStringPair = Glean.Schema.Query.GleanTest.Types.StoredRevStringPair

instance Glean.ToQuery Glean.Schema.GleanTest.Types.StoredRevStringPair

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RevStringPairs_key = Glean.Schema.GleanTest.Types.RevStringPairs_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RevStringPairs_key = Glean.Schema.Query.GleanTest.Types.RevStringPairs_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RevStringPairs_key where
  toQuery (Glean.Schema.GleanTest.Types.RevStringPairs_key x1 x2) = Glean.Schema.Query.GleanTest.Types.RevStringPairs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.RevStringPairs where
  toQueryId = Glean.Schema.Query.GleanTest.Types.RevStringPairs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.RevStringPairs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RevStringPairs = Glean.Schema.GleanTest.Types.RevStringPairs
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RevStringPairs = Glean.Schema.Query.GleanTest.Types.RevStringPairs

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RevStringPairs

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.RefRef where
  toQueryId = Glean.Schema.Query.GleanTest.Types.RefRef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.RefRef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RefRef = Glean.Schema.GleanTest.Types.RefRef
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RefRef = Glean.Schema.Query.GleanTest.Types.RefRef

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RefRef

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2_key = Glean.Schema.GleanTest.Types.DerivedKeyValue2_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.DerivedKeyValue2_key = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.DerivedKeyValue2_key where
  toQuery (Glean.Schema.GleanTest.Types.DerivedKeyValue2_key x1 x2) = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2_value = Glean.Schema.GleanTest.Types.DerivedKeyValue2_value
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.DerivedKeyValue2_value = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2_value

instance Glean.ToQuery Glean.Schema.GleanTest.Types.DerivedKeyValue2_value where
  toQuery (Glean.Schema.GleanTest.Types.DerivedKeyValue2_value x1 x2) = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2_value (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.DerivedKeyValue2 where
  toQueryId = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2 = Glean.Schema.GleanTest.Types.DerivedKeyValue2
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.DerivedKeyValue2 = Glean.Schema.Query.GleanTest.Types.DerivedKeyValue2

instance Glean.ToQuery Glean.Schema.GleanTest.Types.DerivedKeyValue2

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.NothingTest_key = Glean.Schema.GleanTest.Types.NothingTest_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.NothingTest_key = Glean.Schema.Query.GleanTest.Types.NothingTest_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.NothingTest_key where
  toQuery (Glean.Schema.GleanTest.Types.NothingTest_key x1 x2) = Glean.Schema.Query.GleanTest.Types.NothingTest_key (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.GleanTest.Types.nothingTest_a_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.GleanTest.Types.nothingTest_a_just = Prelude.Just (Glean.toQuery x)})) x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.NothingTest where
  toQueryId = Glean.Schema.Query.GleanTest.Types.NothingTest_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.NothingTest_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.NothingTest = Glean.Schema.GleanTest.Types.NothingTest
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.NothingTest = Glean.Schema.Query.GleanTest.Types.NothingTest

instance Glean.ToQuery Glean.Schema.GleanTest.Types.NothingTest

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.FooToFoo where
  toQueryId = Glean.Schema.Query.GleanTest.Types.FooToFoo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.FooToFoo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.FooToFoo = Glean.Schema.GleanTest.Types.FooToFoo
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.FooToFoo = Glean.Schema.Query.GleanTest.Types.FooToFoo

instance Glean.ToQuery Glean.Schema.GleanTest.Types.FooToFoo

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Edge_key = Glean.Schema.GleanTest.Types.Edge_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Edge_key = Glean.Schema.Query.GleanTest.Types.Edge_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Edge_key where
  toQuery (Glean.Schema.GleanTest.Types.Edge_key x1 x2) = Glean.Schema.Query.GleanTest.Types.Edge_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Edge where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Edge_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Edge_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Edge = Glean.Schema.GleanTest.Types.Edge
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Edge = Glean.Schema.Query.GleanTest.Types.Edge

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Edge

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.IsGlean where
  toQueryId = Glean.Schema.Query.GleanTest.Types.IsGlean_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.IsGlean_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.IsGlean = Glean.Schema.GleanTest.Types.IsGlean
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.IsGlean = Glean.Schema.Query.GleanTest.Types.IsGlean

instance Glean.ToQuery Glean.Schema.GleanTest.Types.IsGlean

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Foo where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Foo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Foo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Foo = Glean.Schema.GleanTest.Types.Foo
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Foo = Glean.Schema.Query.GleanTest.Types.Foo

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Foo

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.MatchOneAlt_key = Glean.Schema.GleanTest.Types.MatchOneAlt_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.MatchOneAlt_key = Glean.Schema.Query.GleanTest.Types.MatchOneAlt_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.MatchOneAlt_key where
  toQuery (Glean.Schema.GleanTest.Types.MatchOneAlt_key x1 x2) = Glean.Schema.Query.GleanTest.Types.MatchOneAlt_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.MatchOneAlt where
  toQueryId = Glean.Schema.Query.GleanTest.Types.MatchOneAlt_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.MatchOneAlt_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.MatchOneAlt = Glean.Schema.GleanTest.Types.MatchOneAlt
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.MatchOneAlt = Glean.Schema.Query.GleanTest.Types.MatchOneAlt

instance Glean.ToQuery Glean.Schema.GleanTest.Types.MatchOneAlt

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Predicate where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Predicate_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Predicate_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Predicate = Glean.Schema.GleanTest.Types.Predicate
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Predicate = Glean.Schema.Query.GleanTest.Types.Predicate

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Predicate

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Predicate_1 where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Predicate_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Predicate_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Predicate_1 = Glean.Schema.GleanTest.Types.Predicate_1
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Predicate_1 = Glean.Schema.Query.GleanTest.Types.Predicate_1

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Predicate_1

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Unbound_key = Glean.Schema.GleanTest.Types.Unbound_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Unbound_key = Glean.Schema.Query.GleanTest.Types.Unbound_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Unbound_key where
  toQuery (Glean.Schema.GleanTest.Types.Unbound_key x1 x2) = Glean.Schema.Query.GleanTest.Types.Unbound_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Unbound where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Unbound_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Unbound_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Unbound = Glean.Schema.GleanTest.Types.Unbound
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Unbound = Glean.Schema.Query.GleanTest.Types.Unbound

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Unbound

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.IsThree where
  toQueryId = Glean.Schema.Query.GleanTest.Types.IsThree_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.IsThree_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.IsThree = Glean.Schema.GleanTest.Types.IsThree
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.IsThree = Glean.Schema.Query.GleanTest.Types.IsThree

instance Glean.ToQuery Glean.Schema.GleanTest.Types.IsThree

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Qux where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Qux_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Qux_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Qux = Glean.Schema.GleanTest.Types.Qux
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Qux = Glean.Schema.Query.GleanTest.Types.Qux

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Qux

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KeyValue_key = Glean.Schema.GleanTest.Types.KeyValue_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KeyValue_key = Glean.Schema.Query.GleanTest.Types.KeyValue_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KeyValue_key where
  toQuery (Glean.Schema.GleanTest.Types.KeyValue_key x1 x2) = Glean.Schema.Query.GleanTest.Types.KeyValue_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KeyValue_value = Glean.Schema.GleanTest.Types.KeyValue_value
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KeyValue_value = Glean.Schema.Query.GleanTest.Types.KeyValue_value

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KeyValue_value where
  toQuery (Glean.Schema.GleanTest.Types.KeyValue_value x1 x2) = Glean.Schema.Query.GleanTest.Types.KeyValue_value (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.KeyValue where
  toQueryId = Glean.Schema.Query.GleanTest.Types.KeyValue_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.KeyValue_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KeyValue = Glean.Schema.GleanTest.Types.KeyValue
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KeyValue = Glean.Schema.Query.GleanTest.Types.KeyValue

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KeyValue

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.SameString_key = Glean.Schema.GleanTest.Types.SameString_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.SameString_key = Glean.Schema.Query.GleanTest.Types.SameString_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.SameString_key where
  toQuery (Glean.Schema.GleanTest.Types.SameString_key x1 x2) = Glean.Schema.Query.GleanTest.Types.SameString_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.SameString where
  toQueryId = Glean.Schema.Query.GleanTest.Types.SameString_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.SameString_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.SameString = Glean.Schema.GleanTest.Types.SameString
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.SameString = Glean.Schema.Query.GleanTest.Types.SameString

instance Glean.ToQuery Glean.Schema.GleanTest.Types.SameString

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Expr_key = Glean.Schema.GleanTest.Types.Expr_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Expr_key = Glean.Schema.Query.GleanTest.Types.Expr_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Expr_key where
  toQuery (Glean.Schema.GleanTest.Types.Expr_key_var_ x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.expr_key_var_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.Expr_key_lit x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.expr_key_lit = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.Expr_key_prim x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.expr_key_prim = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.Expr_key_ap x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.expr_key_ap = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.Expr_key_lam x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.expr_key_lam = Prelude.Just (Glean.toQuery x) }

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Expr where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Expr_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Expr_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Expr = Glean.Schema.GleanTest.Types.Expr
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Expr = Glean.Schema.Query.GleanTest.Types.Expr

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Expr

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RevRevStringPair_key = Glean.Schema.GleanTest.Types.RevRevStringPair_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RevRevStringPair_key = Glean.Schema.Query.GleanTest.Types.RevRevStringPair_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RevRevStringPair_key where
  toQuery (Glean.Schema.GleanTest.Types.RevRevStringPair_key x1 x2) = Glean.Schema.Query.GleanTest.Types.RevRevStringPair_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.RevRevStringPair where
  toQueryId = Glean.Schema.Query.GleanTest.Types.RevRevStringPair_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.RevRevStringPair_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RevRevStringPair = Glean.Schema.GleanTest.Types.RevRevStringPair
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RevRevStringPair = Glean.Schema.Query.GleanTest.Types.RevRevStringPair

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RevRevStringPair

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RevStringPairRec_key = Glean.Schema.GleanTest.Types.RevStringPairRec_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RevStringPairRec_key = Glean.Schema.Query.GleanTest.Types.RevStringPairRec_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RevStringPairRec_key where
  toQuery (Glean.Schema.GleanTest.Types.RevStringPairRec_key x1 x2) = Glean.Schema.Query.GleanTest.Types.RevStringPairRec_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.RevStringPairRec where
  toQueryId = Glean.Schema.Query.GleanTest.Types.RevStringPairRec_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.RevStringPairRec_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.RevStringPairRec = Glean.Schema.GleanTest.Types.RevStringPairRec
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.RevStringPairRec = Glean.Schema.Query.GleanTest.Types.RevStringPairRec

instance Glean.ToQuery Glean.Schema.GleanTest.Types.RevStringPairRec

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Node_key = Glean.Schema.GleanTest.Types.Node_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Node_key = Glean.Schema.Query.GleanTest.Types.Node_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Node_key where
  toQuery (Glean.Schema.GleanTest.Types.Node_key x1) = Glean.Schema.Query.GleanTest.Types.Node_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Node where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Node_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Node_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Node = Glean.Schema.GleanTest.Types.Node
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Node = Glean.Schema.Query.GleanTest.Types.Node

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Node

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Unbound2_key = Glean.Schema.GleanTest.Types.Unbound2_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Unbound2_key = Glean.Schema.Query.GleanTest.Types.Unbound2_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Unbound2_key where
  toQuery (Glean.Schema.GleanTest.Types.Unbound2_key x1 x2) = Glean.Schema.Query.GleanTest.Types.Unbound2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.Unbound2 where
  toQueryId = Glean.Schema.Query.GleanTest.Types.Unbound2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.Unbound2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Unbound2 = Glean.Schema.GleanTest.Types.Unbound2
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Unbound2 = Glean.Schema.Query.GleanTest.Types.Unbound2

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Unbound2

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.TreeToTree where
  toQueryId = Glean.Schema.Query.GleanTest.Types.TreeToTree_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.TreeToTree_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.TreeToTree = Glean.Schema.GleanTest.Types.TreeToTree
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.TreeToTree = Glean.Schema.Query.GleanTest.Types.TreeToTree

instance Glean.ToQuery Glean.Schema.GleanTest.Types.TreeToTree

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.DualStringPair_key = Glean.Schema.GleanTest.Types.DualStringPair_key
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.DualStringPair_key = Glean.Schema.Query.GleanTest.Types.DualStringPair_key

instance Glean.ToQuery Glean.Schema.GleanTest.Types.DualStringPair_key where
  toQuery (Glean.Schema.GleanTest.Types.DualStringPair_key x1 x2) = Glean.Schema.Query.GleanTest.Types.DualStringPair_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.GleanTest.Types.DualStringPair where
  toQueryId = Glean.Schema.Query.GleanTest.Types.DualStringPair_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.GleanTest.Types.DualStringPair_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.DualStringPair = Glean.Schema.GleanTest.Types.DualStringPair
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.DualStringPair = Glean.Schema.Query.GleanTest.Types.DualStringPair

instance Glean.ToQuery Glean.Schema.GleanTest.Types.DualStringPair

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KitchenSink = Glean.Schema.GleanTest.Types.KitchenSink
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KitchenSink = Glean.Schema.Query.GleanTest.Types.KitchenSink

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KitchenSink where
  toQuery (Glean.Schema.GleanTest.Types.KitchenSink x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) = Glean.Schema.Query.GleanTest.Types.KitchenSink (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.GleanTest.Types.kitchenSink_maybe__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.GleanTest.Types.kitchenSink_maybe__just = Prelude.Just (Glean.toQuery x)})) x6)) (Prelude.Just (Glean.toQuery x7)) (Prelude.Just (Glean.toQuery x8)) (Prelude.Just (Glean.toQuery x9)) (Prelude.Just (Glean.toQuery x10)) (Prelude.Just (Glean.toQuery x11)) (Prelude.Just (Glean.toQuery x12)) (Prelude.Just (Glean.toQuery x13)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array_of_nat_array_exact . Prelude.map Glean.toQuery) x14)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array_of_bool_array_exact . Prelude.map Glean.toQuery) x15)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array_of_string_array_exact . Prelude.map Glean.toQuery) x16)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array_of_pred_array_exact . Prelude.map Glean.toQuery) x17)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array_of_named_record_array_exact . Prelude.map Glean.toQuery) x18)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array_of_named_sum_array_exact . Prelude.map Glean.toQuery) x19)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array_of_named_enum_array_exact . Prelude.map Glean.toQuery) x20)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array2_of_byte_array_exact . Prelude.map Glean.toQuery) x21)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array2_of_nat_array_exact . Prelude.map (Glean.Schema.Query.GleanTest.Types.ArrayNat_exact . Prelude.map Glean.toQuery)) x22)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array2_of_bool_array_exact . Prelude.map (Glean.Schema.Query.GleanTest.Types.ArrayBool_exact . Prelude.map Glean.toQuery)) x23)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_array2_of_string_array_exact . Prelude.map (Glean.Schema.Query.GleanTest.Types.ArrayString_exact . Prelude.map Glean.toQuery)) x24))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KitchenSink_1 = Glean.Schema.GleanTest.Types.KitchenSink_1
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KitchenSink_1 = Glean.Schema.Query.GleanTest.Types.KitchenSink_1

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KitchenSink_1 where
  toQuery (Glean.Schema.GleanTest.Types.KitchenSink_1 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = Glean.Schema.Query.GleanTest.Types.KitchenSink_1 (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Glean.Schema.Query.GleanTest.Types.KitchenSink_1_array_of_nat_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just (Glean.toQuery x5)) (Prelude.Just (Glean.toQuery x6)) (Prelude.Just (Glean.toQuery x7)) (Prelude.Just (Glean.toQuery x8)) (Prelude.Just (Glean.toQuery x9)) (Prelude.Just (Glean.toQuery x10)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.GleanTest.Types.kitchenSink_1_maybe__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.GleanTest.Types.kitchenSink_1_maybe__just = Prelude.Just (Glean.toQuery x)})) x11)) (Prelude.Just (Glean.toQuery x12)) (Prelude.Just (Glean.toQuery x13))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Rec = Glean.Schema.GleanTest.Types.Rec
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Rec = Glean.Schema.Query.GleanTest.Types.Rec

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Rec where
  toQuery (Glean.Schema.GleanTest.Types.Rec x1 x2) = Glean.Schema.Query.GleanTest.Types.Rec (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Enum = Glean.Schema.GleanTest.Types.Enum
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Enum = Glean.Schema.Query.GleanTest.Types.Enum

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Enum where
  toQuery Glean.Schema.GleanTest.Types.Enum_red = Glean.Schema.Query.GleanTest.Types.Enum_red
  toQuery Glean.Schema.GleanTest.Types.Enum_green = Glean.Schema.Query.GleanTest.Types.Enum_green
  toQuery Glean.Schema.GleanTest.Types.Enum_blue = Glean.Schema.Query.GleanTest.Types.Enum_blue

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Entity = Glean.Schema.GleanTest.Types.Entity
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Entity = Glean.Schema.Query.GleanTest.Types.Entity

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Entity where
  toQuery (Glean.Schema.GleanTest.Types.Entity_cxx x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.entity_cxx = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.Entity_pp x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.entity_pp = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.CodeCxx.Types.Entity Glean.Schema.Query.GleanTest.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.entity_cxx = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Pp1.Types.Define Glean.Schema.Query.GleanTest.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.entity_pp = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.WrappedStringPair = Glean.Schema.GleanTest.Types.WrappedStringPair
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.WrappedStringPair = Glean.Schema.Query.GleanTest.Types.WrappedStringPair

instance Glean.ToQuery Glean.Schema.GleanTest.Types.WrappedStringPair where
  toQuery (Glean.Schema.GleanTest.Types.WrappedStringPair x1) = Glean.Schema.Query.GleanTest.Types.WrappedStringPair (Prelude.Just (Glean.toQuery x1))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Sum = Glean.Schema.GleanTest.Types.Sum
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Sum = Glean.Schema.Query.GleanTest.Types.Sum

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Sum where
  toQuery (Glean.Schema.GleanTest.Types.Sum_mon x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.sum_mon = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.Sum_tue x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.sum_tue = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.Sum_wed x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.sum_wed = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Byte Glean.Schema.Query.GleanTest.Types.Sum where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.sum_mon = Prelude.Just q }

instance Glean.SumQuery Glean.Nat Glean.Schema.Query.GleanTest.Types.Sum where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.sum_tue = Prelude.Just q }

instance Glean.SumQuery Prelude.Bool Glean.Schema.Query.GleanTest.Types.Sum where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.sum_wed = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Expr_ap_ = Glean.Schema.GleanTest.Types.Expr_ap_
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Expr_ap_ = Glean.Schema.Query.GleanTest.Types.Expr_ap_

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Expr_ap_ where
  toQuery (Glean.Schema.GleanTest.Types.Expr_ap_ x1 x2) = Glean.Schema.Query.GleanTest.Types.Expr_ap_ (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.Expr_lam_ = Glean.Schema.GleanTest.Types.Expr_lam_
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.Expr_lam_ = Glean.Schema.Query.GleanTest.Types.Expr_lam_

instance Glean.ToQuery Glean.Schema.GleanTest.Types.Expr_lam_ where
  toQuery (Glean.Schema.GleanTest.Types.Expr_lam_ x1 x2) = Glean.Schema.Query.GleanTest.Types.Expr_lam_ (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KitchenSink_record_ = Glean.Schema.GleanTest.Types.KitchenSink_record_
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KitchenSink_record_ = Glean.Schema.Query.GleanTest.Types.KitchenSink_record_

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KitchenSink_record_ where
  toQuery (Glean.Schema.GleanTest.Types.KitchenSink_record_ x1 x2) = Glean.Schema.Query.GleanTest.Types.KitchenSink_record_ (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KitchenSink_sum_ = Glean.Schema.GleanTest.Types.KitchenSink_sum_
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KitchenSink_sum_ = Glean.Schema.Query.GleanTest.Types.KitchenSink_sum_

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KitchenSink_sum_ where
  toQuery (Glean.Schema.GleanTest.Types.KitchenSink_sum__c x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.kitchenSink_sum__c = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.KitchenSink_sum__d x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.kitchenSink_sum__d = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.GleanTest.Types.Predicate Glean.Schema.Query.GleanTest.Types.KitchenSink_sum_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.kitchenSink_sum__c = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Sys.Types.Blob Glean.Schema.Query.GleanTest.Types.KitchenSink_sum_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.kitchenSink_sum__d = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KitchenSink_enum_ = Glean.Schema.GleanTest.Types.KitchenSink_enum_
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KitchenSink_enum_ = Glean.Schema.Query.GleanTest.Types.KitchenSink_enum_

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KitchenSink_enum_ where
  toQuery Glean.Schema.GleanTest.Types.KitchenSink_enum__e = Glean.Schema.Query.GleanTest.Types.KitchenSink_enum__e
  toQuery Glean.Schema.GleanTest.Types.KitchenSink_enum__f = Glean.Schema.Query.GleanTest.Types.KitchenSink_enum__f
  toQuery Glean.Schema.GleanTest.Types.KitchenSink_enum__g = Glean.Schema.Query.GleanTest.Types.KitchenSink_enum__g

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KitchenSink_1_record_ = Glean.Schema.GleanTest.Types.KitchenSink_1_record_
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KitchenSink_1_record_ = Glean.Schema.Query.GleanTest.Types.KitchenSink_1_record_

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KitchenSink_1_record_ where
  toQuery (Glean.Schema.GleanTest.Types.KitchenSink_1_record_ x1 x2) = Glean.Schema.Query.GleanTest.Types.KitchenSink_1_record_ (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.GleanTest.Types.KitchenSink_1_sum_ = Glean.Schema.GleanTest.Types.KitchenSink_1_sum_
type instance Glean.QueryOf Glean.Schema.GleanTest.Types.KitchenSink_1_sum_ = Glean.Schema.Query.GleanTest.Types.KitchenSink_1_sum_

instance Glean.ToQuery Glean.Schema.GleanTest.Types.KitchenSink_1_sum_ where
  toQuery (Glean.Schema.GleanTest.Types.KitchenSink_1_sum__c x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.kitchenSink_1_sum__c = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.GleanTest.Types.KitchenSink_1_sum__d x) = Data.Default.def { Glean.Schema.Query.GleanTest.Types.kitchenSink_1_sum__d = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Byte Glean.Schema.Query.GleanTest.Types.KitchenSink_1_sum_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.kitchenSink_1_sum__c = Prelude.Just q }

instance Glean.SumQuery Glean.Nat Glean.Schema.Query.GleanTest.Types.KitchenSink_1_sum_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.GleanTest.Types.kitchenSink_1_sum__d = Prelude.Just q }
