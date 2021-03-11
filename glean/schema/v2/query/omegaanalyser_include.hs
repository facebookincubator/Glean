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

import qualified Glean.Schema.Omegaanalyser.Types


instance Glean.PredicateQuery Glean.Schema.Omegaanalyser.Types.OmegaPolicy where
  toQueryId = Glean.Schema.Query.Omegaanalyser.Types.OmegaPolicy_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Omegaanalyser.Types.OmegaPolicy_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Omegaanalyser.Types.OmegaPolicy = Glean.Schema.Omegaanalyser.Types.OmegaPolicy
type instance Glean.QueryOf Glean.Schema.Omegaanalyser.Types.OmegaPolicy = Glean.Schema.Query.Omegaanalyser.Types.OmegaPolicy

instance Glean.ToQuery Glean.Schema.Omegaanalyser.Types.OmegaPolicy

instance Glean.PredicateQuery Glean.Schema.Omegaanalyser.Types.Class_ where
  toQueryId = Glean.Schema.Query.Omegaanalyser.Types.Class__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Omegaanalyser.Types.Class__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Omegaanalyser.Types.Class_ = Glean.Schema.Omegaanalyser.Types.Class_
type instance Glean.QueryOf Glean.Schema.Omegaanalyser.Types.Class_ = Glean.Schema.Query.Omegaanalyser.Types.Class_

instance Glean.ToQuery Glean.Schema.Omegaanalyser.Types.Class_

type instance Glean.QueryResult Glean.Schema.Query.Omegaanalyser.Types.DependencyList_key = Glean.Schema.Omegaanalyser.Types.DependencyList_key
type instance Glean.QueryOf Glean.Schema.Omegaanalyser.Types.DependencyList_key = Glean.Schema.Query.Omegaanalyser.Types.DependencyList_key

instance Glean.ToQuery Glean.Schema.Omegaanalyser.Types.DependencyList_key where
  toQuery (Glean.Schema.Omegaanalyser.Types.DependencyList_key x1 x2) = Glean.Schema.Query.Omegaanalyser.Types.DependencyList_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Omegaanalyser.Types.DependencyList_endpoints_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Omegaanalyser.Types.DependencyList where
  toQueryId = Glean.Schema.Query.Omegaanalyser.Types.DependencyList_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Omegaanalyser.Types.DependencyList_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Omegaanalyser.Types.DependencyList = Glean.Schema.Omegaanalyser.Types.DependencyList
type instance Glean.QueryOf Glean.Schema.Omegaanalyser.Types.DependencyList = Glean.Schema.Query.Omegaanalyser.Types.DependencyList

instance Glean.ToQuery Glean.Schema.Omegaanalyser.Types.DependencyList

instance Glean.PredicateQuery Glean.Schema.Omegaanalyser.Types.Function_ where
  toQueryId = Glean.Schema.Query.Omegaanalyser.Types.Function__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Omegaanalyser.Types.Function__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Omegaanalyser.Types.Function_ = Glean.Schema.Omegaanalyser.Types.Function_
type instance Glean.QueryOf Glean.Schema.Omegaanalyser.Types.Function_ = Glean.Schema.Query.Omegaanalyser.Types.Function_

instance Glean.ToQuery Glean.Schema.Omegaanalyser.Types.Function_

instance Glean.PredicateQuery Glean.Schema.Omegaanalyser.Types.OmegaEndpoint where
  toQueryId = Glean.Schema.Query.Omegaanalyser.Types.OmegaEndpoint_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Omegaanalyser.Types.OmegaEndpoint_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Omegaanalyser.Types.OmegaEndpoint = Glean.Schema.Omegaanalyser.Types.OmegaEndpoint
type instance Glean.QueryOf Glean.Schema.Omegaanalyser.Types.OmegaEndpoint = Glean.Schema.Query.Omegaanalyser.Types.OmegaEndpoint

instance Glean.ToQuery Glean.Schema.Omegaanalyser.Types.OmegaEndpoint

instance Glean.PredicateQuery Glean.Schema.Omegaanalyser.Types.Method where
  toQueryId = Glean.Schema.Query.Omegaanalyser.Types.Method_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Omegaanalyser.Types.Method_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Omegaanalyser.Types.Method = Glean.Schema.Omegaanalyser.Types.Method
type instance Glean.QueryOf Glean.Schema.Omegaanalyser.Types.Method = Glean.Schema.Query.Omegaanalyser.Types.Method

instance Glean.ToQuery Glean.Schema.Omegaanalyser.Types.Method

type instance Glean.QueryResult Glean.Schema.Query.Omegaanalyser.Types.Node = Glean.Schema.Omegaanalyser.Types.Node
type instance Glean.QueryOf Glean.Schema.Omegaanalyser.Types.Node = Glean.Schema.Query.Omegaanalyser.Types.Node

instance Glean.ToQuery Glean.Schema.Omegaanalyser.Types.Node where
  toQuery (Glean.Schema.Omegaanalyser.Types.Node_class_ x) = Data.Default.def { Glean.Schema.Query.Omegaanalyser.Types.node_class_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Omegaanalyser.Types.Node_method x) = Data.Default.def { Glean.Schema.Query.Omegaanalyser.Types.node_method = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Omegaanalyser.Types.Node_function_ x) = Data.Default.def { Glean.Schema.Query.Omegaanalyser.Types.node_function_ = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Omegaanalyser.Types.Class_ Glean.Schema.Query.Omegaanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Omegaanalyser.Types.node_class_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Omegaanalyser.Types.Method Glean.Schema.Query.Omegaanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Omegaanalyser.Types.node_method = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Omegaanalyser.Types.Function_ Glean.Schema.Query.Omegaanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Omegaanalyser.Types.node_function_ = Prelude.Just q }
