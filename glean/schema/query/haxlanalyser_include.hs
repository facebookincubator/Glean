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

import qualified Glean.Schema.Hs.Types
import qualified Glean.Schema.Query.Hs.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Haxlanalyser.Types


instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ScubaResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ScubaResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ScubaResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ScubaResponse = Glean.Schema.Haxlanalyser.Types.ScubaResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ScubaResponse = Glean.Schema.Query.Haxlanalyser.Types.ScubaResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ScubaResponse

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ClassifierRead where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ClassifierRead_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ClassifierRead_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ClassifierRead = Glean.Schema.Haxlanalyser.Types.ClassifierRead
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ClassifierRead = Glean.Schema.Query.Haxlanalyser.Types.ClassifierRead

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ClassifierRead

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.SitevarFetch where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.SitevarFetch_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.SitevarFetch_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.SitevarFetch = Glean.Schema.Haxlanalyser.Types.SitevarFetch
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.SitevarFetch = Glean.Schema.Query.Haxlanalyser.Types.SitevarFetch

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.SitevarFetch

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Call_key = Glean.Schema.Haxlanalyser.Types.Call_key
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Call_key = Glean.Schema.Query.Haxlanalyser.Types.Call_key

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Call_key where
  toQuery (Glean.Schema.Haxlanalyser.Types.Call_key x1 x2 x3) = Glean.Schema.Query.Haxlanalyser.Types.Call_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.Call where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.Call_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.Call_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Call = Glean.Schema.Haxlanalyser.Types.Call
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Call = Glean.Schema.Query.Haxlanalyser.Types.Call

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Call

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.FeatureSetFeature where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.FeatureSetFeature_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.FeatureSetFeature_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.FeatureSetFeature = Glean.Schema.Haxlanalyser.Types.FeatureSetFeature
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.FeatureSetFeature = Glean.Schema.Query.Haxlanalyser.Types.FeatureSetFeature

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.FeatureSetFeature

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ACDCProperty_key = Glean.Schema.Haxlanalyser.Types.ACDCProperty_key
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ACDCProperty_key = Glean.Schema.Query.Haxlanalyser.Types.ACDCProperty_key

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ACDCProperty_key where
  toQuery (Glean.Schema.Haxlanalyser.Types.ACDCProperty_key x1 x2) = Glean.Schema.Query.Haxlanalyser.Types.ACDCProperty_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ACDCProperty where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ACDCProperty_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ACDCProperty_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ACDCProperty = Glean.Schema.Haxlanalyser.Types.ACDCProperty
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ACDCProperty = Glean.Schema.Query.Haxlanalyser.Types.ACDCProperty

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ACDCProperty

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ScribeResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ScribeResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ScribeResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ScribeResponse = Glean.Schema.Haxlanalyser.Types.ScribeResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ScribeResponse = Glean.Schema.Query.Haxlanalyser.Types.ScribeResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ScribeResponse

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.TallyResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.TallyResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.TallyResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.TallyResponse = Glean.Schema.Haxlanalyser.Types.TallyResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.TallyResponse = Glean.Schema.Query.Haxlanalyser.Types.TallyResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.TallyResponse

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.HiveResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.HiveResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.HiveResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.HiveResponse = Glean.Schema.Haxlanalyser.Types.HiveResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.HiveResponse = Glean.Schema.Query.Haxlanalyser.Types.HiveResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.HiveResponse

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ConfigeratorFetch_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ConfigeratorFetch_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ConfigeratorFetch = Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch = Glean.Schema.Query.Haxlanalyser.Types.ConfigeratorFetch

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.TallyConfig_key = Glean.Schema.Haxlanalyser.Types.TallyConfig_key
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.TallyConfig_key = Glean.Schema.Query.Haxlanalyser.Types.TallyConfig_key

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.TallyConfig_key where
  toQuery (Glean.Schema.Haxlanalyser.Types.TallyConfig_key x1 x2) = Glean.Schema.Query.Haxlanalyser.Types.TallyConfig_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.TallyConfig where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.TallyConfig_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.TallyConfig_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.TallyConfig = Glean.Schema.Haxlanalyser.Types.TallyConfig
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.TallyConfig = Glean.Schema.Query.Haxlanalyser.Types.TallyConfig

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.TallyConfig

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ThriftResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ThriftResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ThriftResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ThriftResponse = Glean.Schema.Haxlanalyser.Types.ThriftResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ThriftResponse = Glean.Schema.Query.Haxlanalyser.Types.ThriftResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ThriftResponse

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Endpoint_key = Glean.Schema.Haxlanalyser.Types.Endpoint_key
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Endpoint_key = Glean.Schema.Query.Haxlanalyser.Types.Endpoint_key

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Endpoint_key where
  toQuery (Glean.Schema.Haxlanalyser.Types.Endpoint_key x1 x2) = Glean.Schema.Query.Haxlanalyser.Types.Endpoint_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.Endpoint where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.Endpoint_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.Endpoint_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Endpoint = Glean.Schema.Haxlanalyser.Types.Endpoint
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Endpoint = Glean.Schema.Query.Haxlanalyser.Types.Endpoint

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Endpoint

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Edge_key = Glean.Schema.Haxlanalyser.Types.Edge_key
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Edge_key = Glean.Schema.Query.Haxlanalyser.Types.Edge_key

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Edge_key where
  toQuery (Glean.Schema.Haxlanalyser.Types.Edge_key x1 x2) = Glean.Schema.Query.Haxlanalyser.Types.Edge_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.Edge where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.Edge_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.Edge_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Edge = Glean.Schema.Haxlanalyser.Types.Edge
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Edge = Glean.Schema.Query.Haxlanalyser.Types.Edge

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Edge

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ContextName where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ContextName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ContextName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ContextName = Glean.Schema.Haxlanalyser.Types.ContextName
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ContextName = Glean.Schema.Query.Haxlanalyser.Types.ContextName

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ContextName

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.TestFile where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.TestFile_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.TestFile_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.TestFile = Glean.Schema.Haxlanalyser.Types.TestFile
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.TestFile = Glean.Schema.Query.Haxlanalyser.Types.TestFile

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.TestFile

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.TallyFetch where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.TallyFetch_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.TallyFetch_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.TallyFetch = Glean.Schema.Haxlanalyser.Types.TallyFetch
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.TallyFetch = Glean.Schema.Query.Haxlanalyser.Types.TallyFetch

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.TallyFetch

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Policy_key = Glean.Schema.Haxlanalyser.Types.Policy_key
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Policy_key = Glean.Schema.Query.Haxlanalyser.Types.Policy_key

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Policy_key where
  toQuery (Glean.Schema.Haxlanalyser.Types.Policy_key x1 x2) = Glean.Schema.Query.Haxlanalyser.Types.Policy_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.Policy where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.Policy_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.Policy_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Policy = Glean.Schema.Haxlanalyser.Types.Policy
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Policy = Glean.Schema.Query.Haxlanalyser.Types.Policy

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Policy

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.InputFetch_key = Glean.Schema.Haxlanalyser.Types.InputFetch_key
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.InputFetch_key = Glean.Schema.Query.Haxlanalyser.Types.InputFetch_key

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.InputFetch_key where
  toQuery (Glean.Schema.Haxlanalyser.Types.InputFetch_key x1 x2) = Glean.Schema.Query.Haxlanalyser.Types.InputFetch_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.InputFetch where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.InputFetch_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.InputFetch_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.InputFetch = Glean.Schema.Haxlanalyser.Types.InputFetch
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.InputFetch = Glean.Schema.Query.Haxlanalyser.Types.InputFetch

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.InputFetch

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.RestrictionResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.RestrictionResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.RestrictionResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.RestrictionResponse = Glean.Schema.Haxlanalyser.Types.RestrictionResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.RestrictionResponse = Glean.Schema.Query.Haxlanalyser.Types.RestrictionResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.RestrictionResponse

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Context_key = Glean.Schema.Haxlanalyser.Types.Context_key
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Context_key = Glean.Schema.Query.Haxlanalyser.Types.Context_key

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Context_key where
  toQuery (Glean.Schema.Haxlanalyser.Types.Context_key x1 x2) = Glean.Schema.Query.Haxlanalyser.Types.Context_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.Context where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.Context_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.Context_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Context = Glean.Schema.Haxlanalyser.Types.Context
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Context = Glean.Schema.Query.Haxlanalyser.Types.Context

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Context

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.PolicyName where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.PolicyName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.PolicyName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.PolicyName = Glean.Schema.Haxlanalyser.Types.PolicyName
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.PolicyName = Glean.Schema.Query.Haxlanalyser.Types.PolicyName

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.PolicyName

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.EndpointName where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.EndpointName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.EndpointName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.EndpointName = Glean.Schema.Haxlanalyser.Types.EndpointName
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.EndpointName = Glean.Schema.Query.Haxlanalyser.Types.EndpointName

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.EndpointName

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.JankyJSONResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.JankyJSONResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.JankyJSONResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.JankyJSONResponse = Glean.Schema.Haxlanalyser.Types.JankyJSONResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.JankyJSONResponse = Glean.Schema.Query.Haxlanalyser.Types.JankyJSONResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.JankyJSONResponse

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ThriftFetch where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ThriftFetch_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ThriftFetch_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ThriftFetch = Glean.Schema.Haxlanalyser.Types.ThriftFetch
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ThriftFetch = Glean.Schema.Query.Haxlanalyser.Types.ThriftFetch

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ThriftFetch

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.InputKey where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.InputKey_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.InputKey_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.InputKey = Glean.Schema.Haxlanalyser.Types.InputKey
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.InputKey = Glean.Schema.Query.Haxlanalyser.Types.InputKey

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.InputKey

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ACDCPropertyAccess_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ACDCPropertyAccess_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ACDCPropertyAccess = Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess = Glean.Schema.Query.Haxlanalyser.Types.ACDCPropertyAccess

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.PiranhaResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.PiranhaResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.PiranhaResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.PiranhaResponse = Glean.Schema.Haxlanalyser.Types.PiranhaResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.PiranhaResponse = Glean.Schema.Query.Haxlanalyser.Types.PiranhaResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.PiranhaResponse

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.LaserDataset where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.LaserDataset_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.LaserDataset_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.LaserDataset = Glean.Schema.Haxlanalyser.Types.LaserDataset
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.LaserDataset = Glean.Schema.Query.Haxlanalyser.Types.LaserDataset

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.LaserDataset

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.ClassifierFetch where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.ClassifierFetch_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.ClassifierFetch_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.ClassifierFetch = Glean.Schema.Haxlanalyser.Types.ClassifierFetch
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.ClassifierFetch = Glean.Schema.Query.Haxlanalyser.Types.ClassifierFetch

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.ClassifierFetch

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.TallyName where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.TallyName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.TallyName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.TallyName = Glean.Schema.Haxlanalyser.Types.TallyName
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.TallyName = Glean.Schema.Query.Haxlanalyser.Types.TallyName

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.TallyName

instance Glean.PredicateQuery Glean.Schema.Haxlanalyser.Types.LogFeatureResponse where
  toQueryId = Glean.Schema.Query.Haxlanalyser.Types.LogFeatureResponse_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Haxlanalyser.Types.LogFeatureResponse_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.LogFeatureResponse = Glean.Schema.Haxlanalyser.Types.LogFeatureResponse
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.LogFeatureResponse = Glean.Schema.Query.Haxlanalyser.Types.LogFeatureResponse

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.LogFeatureResponse

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Tally = Glean.Schema.Haxlanalyser.Types.Tally
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Tally = Glean.Schema.Query.Haxlanalyser.Types.Tally

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Tally where
  toQuery (Glean.Schema.Haxlanalyser.Types.Tally x1 x2) = Glean.Schema.Query.Haxlanalyser.Types.Tally (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType = Glean.Schema.Haxlanalyser.Types.TallyCounterType
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.TallyCounterType = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType where
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_counter = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_counter
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_uniqueCounter = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_uniqueCounter
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_topK = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_topK
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_quantiles = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_quantiles
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_uniqueQuantiles = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_uniqueQuantiles
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_firstN = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_firstN
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_lastN = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_lastN
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_moments = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_moments
  toQuery Glean.Schema.Haxlanalyser.Types.TallyCounterType_infiniteCounter = Glean.Schema.Query.Haxlanalyser.Types.TallyCounterType_infiniteCounter

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Fetch = Glean.Schema.Haxlanalyser.Types.Fetch
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Fetch = Glean.Schema.Query.Haxlanalyser.Types.Fetch

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Fetch where
  toQuery (Glean.Schema.Haxlanalyser.Types.Fetch_thrift x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.fetch_thrift = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Fetch_configerator x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.fetch_configerator = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Fetch_tally x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.fetch_tally = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Fetch_classifier x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.fetch_classifier = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Fetch_sitevar x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.fetch_sitevar = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ThriftFetch Glean.Schema.Query.Haxlanalyser.Types.Fetch where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.fetch_thrift = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ConfigeratorFetch Glean.Schema.Query.Haxlanalyser.Types.Fetch where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.fetch_configerator = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.TallyFetch Glean.Schema.Query.Haxlanalyser.Types.Fetch where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.fetch_tally = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ClassifierFetch Glean.Schema.Query.Haxlanalyser.Types.Fetch where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.fetch_classifier = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.SitevarFetch Glean.Schema.Query.Haxlanalyser.Types.Fetch where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.fetch_sitevar = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Response = Glean.Schema.Haxlanalyser.Types.Response
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Response = Glean.Schema.Query.Haxlanalyser.Types.Response

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Response where
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_janky_json x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_janky_json = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_scuba x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_scuba = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_thrift x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_thrift = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_piranha x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_piranha = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_tally x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_tally = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_log_feature x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_log_feature = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_restriction x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_restriction = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_hive x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_hive = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Response_scribe x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.response_scribe = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.JankyJSONResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_janky_json = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ScubaResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_scuba = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ThriftResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_thrift = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.PiranhaResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_piranha = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.TallyResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_tally = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.LogFeatureResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_log_feature = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.RestrictionResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_restriction = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.HiveResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_hive = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ScribeResponse Glean.Schema.Query.Haxlanalyser.Types.Response where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.response_scribe = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Haxlanalyser.Types.Node = Glean.Schema.Haxlanalyser.Types.Node
type instance Glean.QueryOf Glean.Schema.Haxlanalyser.Types.Node = Glean.Schema.Query.Haxlanalyser.Types.Node

instance Glean.ToQuery Glean.Schema.Haxlanalyser.Types.Node where
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_hs_module x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_hs_module = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_definition x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_definition = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_typeclass x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_typeclass = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_class_instance x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_class_instance = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_context x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_context = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_policy x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_policy = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_tally x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_tally = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_endpoint x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_endpoint = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_acdc x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_acdc = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_input_fetch x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_input_fetch = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_response x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_response = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_fetch x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_fetch = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_classifier_read x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_classifier_read = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_laser_dataset x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_laser_dataset = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_test_file x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_test_file = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_call x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_call = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_acdc_access x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_acdc_access = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Haxlanalyser.Types.Node_feature x) = Data.Default.def { Glean.Schema.Query.Haxlanalyser.Types.node_feature = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.SourceModule Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_hs_module = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.DefinitionName Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_definition = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.ClassName Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_typeclass = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.ClassInstance Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_class_instance = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.Context Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_context = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.Policy Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_policy = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.TallyConfig Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_tally = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.Endpoint Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_endpoint = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ACDCProperty Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_acdc = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.InputFetch Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_input_fetch = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.Response Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_response = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.Fetch Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_fetch = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ClassifierRead Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_classifier_read = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.LaserDataset Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_laser_dataset = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.TestFile Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_test_file = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.Call Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_call = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.ACDCPropertyAccess Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_acdc_access = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Haxlanalyser.Types.FeatureSetFeature Glean.Schema.Query.Haxlanalyser.Types.Node where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Haxlanalyser.Types.node_feature = Prelude.Just q }
