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

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Sys.Types
import qualified Glean.Schema.Query.Sys.Types

import qualified Glean.Schema.Buck.Types


type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Locator_key = Glean.Schema.Buck.Types.Locator_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.Locator_key = Glean.Schema.Query.Buck.Types.Locator_key

instance Glean.ToQuery Glean.Schema.Buck.Types.Locator_key where
  toQuery (Glean.Schema.Buck.Types.Locator_key x1 x2 x3) = Glean.Schema.Query.Buck.Types.Locator_key (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Buck.Types.locator_subdir_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Buck.Types.locator_subdir_just = Prelude.Just (Glean.toQuery x)})) x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Locator where
  toQueryId = Glean.Schema.Query.Buck.Types.Locator_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Locator_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Locator = Glean.Schema.Buck.Types.Locator
type instance Glean.QueryOf Glean.Schema.Buck.Types.Locator = Glean.Schema.Query.Buck.Types.Locator

instance Glean.ToQuery Glean.Schema.Buck.Types.Locator

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetDependencies_key = Glean.Schema.Buck.Types.TargetDependencies_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetDependencies_key = Glean.Schema.Query.Buck.Types.TargetDependencies_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetDependencies_key where
  toQuery (Glean.Schema.Buck.Types.TargetDependencies_key x1 x2) = Glean.Schema.Query.Buck.Types.TargetDependencies_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Buck.Types.TargetDependencies_dependencies_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetDependencies where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetDependencies_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetDependencies_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetDependencies = Glean.Schema.Buck.Types.TargetDependencies
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetDependencies = Glean.Schema.Query.Buck.Types.TargetDependencies

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetDependencies

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Type where
  toQueryId = Glean.Schema.Query.Buck.Types.Type__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Type__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Type_ = Glean.Schema.Buck.Types.Type
type instance Glean.QueryOf Glean.Schema.Buck.Types.Type = Glean.Schema.Query.Buck.Types.Type_

instance Glean.ToQuery Glean.Schema.Buck.Types.Type

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Platform where
  toQueryId = Glean.Schema.Query.Buck.Types.Platform_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Platform_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Platform = Glean.Schema.Buck.Types.Platform
type instance Glean.QueryOf Glean.Schema.Buck.Types.Platform = Glean.Schema.Query.Buck.Types.Platform

instance Glean.ToQuery Glean.Schema.Buck.Types.Platform

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetSources_key = Glean.Schema.Buck.Types.TargetSources_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetSources_key = Glean.Schema.Query.Buck.Types.TargetSources_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetSources_key where
  toQuery (Glean.Schema.Buck.Types.TargetSources_key x1 x2 x3 x4) = Glean.Schema.Query.Buck.Types.TargetSources_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Buck.Types.TargetSources_headers_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Buck.Types.TargetSources_exportedHeaders_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Buck.Types.TargetSources_srcs_array_exact . Prelude.map Glean.toQuery) x4))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetSources where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetSources_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetSources_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetSources = Glean.Schema.Buck.Types.TargetSources
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetSources = Glean.Schema.Query.Buck.Types.TargetSources

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetSources

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetSources_1_key = Glean.Schema.Buck.Types.TargetSources_1_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetSources_1_key = Glean.Schema.Query.Buck.Types.TargetSources_1_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetSources_1_key where
  toQuery (Glean.Schema.Buck.Types.TargetSources_1_key x1 x2 x3 x4) = Glean.Schema.Query.Buck.Types.TargetSources_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Buck.Types.TargetSources_1_headers_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Buck.Types.TargetSources_1_exportedHeaders_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Buck.Types.TargetSources_1_srcs_array_exact . Prelude.map Glean.toQuery) x4))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetSources_1 where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetSources_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetSources_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetSources_1 = Glean.Schema.Buck.Types.TargetSources_1
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetSources_1 = Glean.Schema.Query.Buck.Types.TargetSources_1

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetSources_1

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.OutTarget_key = Glean.Schema.Buck.Types.OutTarget_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.OutTarget_key = Glean.Schema.Query.Buck.Types.OutTarget_key

instance Glean.ToQuery Glean.Schema.Buck.Types.OutTarget_key where
  toQuery (Glean.Schema.Buck.Types.OutTarget_key x1 x2) = Glean.Schema.Query.Buck.Types.OutTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.OutTarget where
  toQueryId = Glean.Schema.Query.Buck.Types.OutTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.OutTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.OutTarget = Glean.Schema.Buck.Types.OutTarget
type instance Glean.QueryOf Glean.Schema.Buck.Types.OutTarget = Glean.Schema.Query.Buck.Types.OutTarget

instance Glean.ToQuery Glean.Schema.Buck.Types.OutTarget

instance Glean.PredicateQuery Glean.Schema.Buck.Types.OutputLabel where
  toQueryId = Glean.Schema.Query.Buck.Types.OutputLabel_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.OutputLabel_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.OutputLabel = Glean.Schema.Buck.Types.OutputLabel
type instance Glean.QueryOf Glean.Schema.Buck.Types.OutputLabel = Glean.Schema.Query.Buck.Types.OutputLabel

instance Glean.ToQuery Glean.Schema.Buck.Types.OutputLabel

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Owner_key = Glean.Schema.Buck.Types.Owner_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.Owner_key = Glean.Schema.Query.Buck.Types.Owner_key

instance Glean.ToQuery Glean.Schema.Buck.Types.Owner_key where
  toQuery (Glean.Schema.Buck.Types.Owner_key x1 x2) = Glean.Schema.Query.Buck.Types.Owner_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Owner where
  toQueryId = Glean.Schema.Query.Buck.Types.Owner_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Owner_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Owner = Glean.Schema.Buck.Types.Owner
type instance Glean.QueryOf Glean.Schema.Buck.Types.Owner = Glean.Schema.Query.Buck.Types.Owner

instance Glean.ToQuery Glean.Schema.Buck.Types.Owner

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Owner_1_key = Glean.Schema.Buck.Types.Owner_1_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.Owner_1_key = Glean.Schema.Query.Buck.Types.Owner_1_key

instance Glean.ToQuery Glean.Schema.Buck.Types.Owner_1_key where
  toQuery (Glean.Schema.Buck.Types.Owner_1_key x1 x2) = Glean.Schema.Query.Buck.Types.Owner_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Owner_1 where
  toQueryId = Glean.Schema.Query.Buck.Types.Owner_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Owner_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Owner_1 = Glean.Schema.Buck.Types.Owner_1
type instance Glean.QueryOf Glean.Schema.Buck.Types.Owner_1 = Glean.Schema.Query.Buck.Types.Owner_1

instance Glean.ToQuery Glean.Schema.Buck.Types.Owner_1

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Target_key = Glean.Schema.Buck.Types.Target_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.Target_key = Glean.Schema.Query.Buck.Types.Target_key

instance Glean.ToQuery Glean.Schema.Buck.Types.Target_key where
  toQuery (Glean.Schema.Buck.Types.Target_key x1 x2 x3 x4) = Glean.Schema.Query.Buck.Types.Target_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Buck.Types.target_defaultPlatform_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Buck.Types.target_defaultPlatform_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Target where
  toQueryId = Glean.Schema.Query.Buck.Types.Target_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Target_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Target = Glean.Schema.Buck.Types.Target
type instance Glean.QueryOf Glean.Schema.Buck.Types.Target = Glean.Schema.Query.Buck.Types.Target

instance Glean.ToQuery Glean.Schema.Buck.Types.Target

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Target_1_key = Glean.Schema.Buck.Types.Target_1_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.Target_1_key = Glean.Schema.Query.Buck.Types.Target_1_key

instance Glean.ToQuery Glean.Schema.Buck.Types.Target_1_key where
  toQuery (Glean.Schema.Buck.Types.Target_1_key x1 x2 x3) = Glean.Schema.Query.Buck.Types.Target_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Buck.Types.target_1_platform_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Buck.Types.target_1_platform_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Target_1 where
  toQueryId = Glean.Schema.Query.Buck.Types.Target_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Target_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Target_1 = Glean.Schema.Buck.Types.Target_1
type instance Glean.QueryOf Glean.Schema.Buck.Types.Target_1 = Glean.Schema.Query.Buck.Types.Target_1

instance Glean.ToQuery Glean.Schema.Buck.Types.Target_1

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.RuleKey_key = Glean.Schema.Buck.Types.RuleKey_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.RuleKey_key = Glean.Schema.Query.Buck.Types.RuleKey_key

instance Glean.ToQuery Glean.Schema.Buck.Types.RuleKey_key where
  toQuery (Glean.Schema.Buck.Types.RuleKey_key x1 x2) = Glean.Schema.Query.Buck.Types.RuleKey_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.RuleKey where
  toQueryId = Glean.Schema.Query.Buck.Types.RuleKey_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.RuleKey_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.RuleKey = Glean.Schema.Buck.Types.RuleKey
type instance Glean.QueryOf Glean.Schema.Buck.Types.RuleKey = Glean.Schema.Query.Buck.Types.RuleKey

instance Glean.ToQuery Glean.Schema.Buck.Types.RuleKey

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetHash_key = Glean.Schema.Buck.Types.TargetHash_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetHash_key = Glean.Schema.Query.Buck.Types.TargetHash_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetHash_key where
  toQuery (Glean.Schema.Buck.Types.TargetHash_key x1 x2) = Glean.Schema.Query.Buck.Types.TargetHash_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetHash where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetHash_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetHash_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetHash = Glean.Schema.Buck.Types.TargetHash
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetHash = Glean.Schema.Query.Buck.Types.TargetHash

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetHash

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Label where
  toQueryId = Glean.Schema.Query.Buck.Types.Label_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Label_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Label = Glean.Schema.Buck.Types.Label
type instance Glean.QueryOf Glean.Schema.Buck.Types.Label = Glean.Schema.Query.Buck.Types.Label

instance Glean.ToQuery Glean.Schema.Buck.Types.Label

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Labels where
  toQueryId = Glean.Schema.Query.Buck.Types.Labels_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Labels_with_key . (Glean.Schema.Query.Buck.Types.Labels_array_exact . Prelude.map Glean.toQuery)

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Labels = Glean.Schema.Buck.Types.Labels
type instance Glean.QueryOf Glean.Schema.Buck.Types.Labels = Glean.Schema.Query.Buck.Types.Labels

instance Glean.ToQuery Glean.Schema.Buck.Types.Labels

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetLinkWhole where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetLinkWhole_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetLinkWhole_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetLinkWhole = Glean.Schema.Buck.Types.TargetLinkWhole
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetLinkWhole = Glean.Schema.Query.Buck.Types.TargetLinkWhole

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetLinkWhole

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetIndexer_key = Glean.Schema.Buck.Types.TargetIndexer_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetIndexer_key = Glean.Schema.Query.Buck.Types.TargetIndexer_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetIndexer_key where
  toQuery (Glean.Schema.Buck.Types.TargetIndexer_key x1 x2) = Glean.Schema.Query.Buck.Types.TargetIndexer_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetIndexer where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetIndexer_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetIndexer_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetIndexer = Glean.Schema.Buck.Types.TargetIndexer
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetIndexer = Glean.Schema.Query.Buck.Types.TargetIndexer

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetIndexer

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetIndexerName where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetIndexerName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetIndexerName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetIndexerName = Glean.Schema.Buck.Types.TargetIndexerName
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetIndexerName = Glean.Schema.Query.Buck.Types.TargetIndexerName

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetIndexerName

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetOut_key = Glean.Schema.Buck.Types.TargetOut_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetOut_key = Glean.Schema.Query.Buck.Types.TargetOut_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetOut_key where
  toQuery (Glean.Schema.Buck.Types.TargetOut_key x1 x2) = Glean.Schema.Query.Buck.Types.TargetOut_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetOut where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetOut_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetOut_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetOut = Glean.Schema.Buck.Types.TargetOut
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetOut = Glean.Schema.Query.Buck.Types.TargetOut

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetOut

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.LocatorReverseDeps_key = Glean.Schema.Buck.Types.LocatorReverseDeps_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.LocatorReverseDeps_key = Glean.Schema.Query.Buck.Types.LocatorReverseDeps_key

instance Glean.ToQuery Glean.Schema.Buck.Types.LocatorReverseDeps_key where
  toQuery (Glean.Schema.Buck.Types.LocatorReverseDeps_key x1 x2) = Glean.Schema.Query.Buck.Types.LocatorReverseDeps_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Buck.Types.LocatorReverseDeps_rdeps_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.LocatorReverseDeps where
  toQueryId = Glean.Schema.Query.Buck.Types.LocatorReverseDeps_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.LocatorReverseDeps_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.LocatorReverseDeps = Glean.Schema.Buck.Types.LocatorReverseDeps
type instance Glean.QueryOf Glean.Schema.Buck.Types.LocatorReverseDeps = Glean.Schema.Query.Buck.Types.LocatorReverseDeps

instance Glean.ToQuery Glean.Schema.Buck.Types.LocatorReverseDeps

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetOuts_key = Glean.Schema.Buck.Types.TargetOuts_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetOuts_key = Glean.Schema.Query.Buck.Types.TargetOuts_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetOuts_key where
  toQuery (Glean.Schema.Buck.Types.TargetOuts_key x1 x2 x3) = Glean.Schema.Query.Buck.Types.TargetOuts_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Buck.Types.targetOuts_outputLabel_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Buck.Types.targetOuts_outputLabel_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TargetOuts where
  toQueryId = Glean.Schema.Query.Buck.Types.TargetOuts_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TargetOuts_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TargetOuts = Glean.Schema.Buck.Types.TargetOuts
type instance Glean.QueryOf Glean.Schema.Buck.Types.TargetOuts = Glean.Schema.Query.Buck.Types.TargetOuts

instance Glean.ToQuery Glean.Schema.Buck.Types.TargetOuts

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.LocatorWithLabel_key = Glean.Schema.Buck.Types.LocatorWithLabel_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.LocatorWithLabel_key = Glean.Schema.Query.Buck.Types.LocatorWithLabel_key

instance Glean.ToQuery Glean.Schema.Buck.Types.LocatorWithLabel_key where
  toQuery (Glean.Schema.Buck.Types.LocatorWithLabel_key x1 x2) = Glean.Schema.Query.Buck.Types.LocatorWithLabel_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.LocatorWithLabel where
  toQueryId = Glean.Schema.Query.Buck.Types.LocatorWithLabel_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.LocatorWithLabel_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.LocatorWithLabel = Glean.Schema.Buck.Types.LocatorWithLabel
type instance Glean.QueryOf Glean.Schema.Buck.Types.LocatorWithLabel = Glean.Schema.Query.Buck.Types.LocatorWithLabel

instance Glean.ToQuery Glean.Schema.Buck.Types.LocatorWithLabel

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.OutsTarget_key = Glean.Schema.Buck.Types.OutsTarget_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.OutsTarget_key = Glean.Schema.Query.Buck.Types.OutsTarget_key

instance Glean.ToQuery Glean.Schema.Buck.Types.OutsTarget_key where
  toQuery (Glean.Schema.Buck.Types.OutsTarget_key x1 x2 x3) = Glean.Schema.Query.Buck.Types.OutsTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Buck.Types.outsTarget_outputLabel_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Buck.Types.outsTarget_outputLabel_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.OutsTarget where
  toQueryId = Glean.Schema.Query.Buck.Types.OutsTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.OutsTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.OutsTarget = Glean.Schema.Buck.Types.OutsTarget
type instance Glean.QueryOf Glean.Schema.Buck.Types.OutsTarget = Glean.Schema.Query.Buck.Types.OutsTarget

instance Glean.ToQuery Glean.Schema.Buck.Types.OutsTarget

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.File_key = Glean.Schema.Buck.Types.File_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.File_key = Glean.Schema.Query.Buck.Types.File_key

instance Glean.ToQuery Glean.Schema.Buck.Types.File_key where
  toQuery (Glean.Schema.Buck.Types.File_key_source x) = Data.Default.def { Glean.Schema.Query.Buck.Types.file_key_source = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Buck.Types.File_key_generated x) = Data.Default.def { Glean.Schema.Query.Buck.Types.file_key_generated = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Buck.Types.File_key_generatedLabel x) = Data.Default.def { Glean.Schema.Query.Buck.Types.file_key_generatedLabel = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Src.Types.File Glean.Schema.Query.Buck.Types.File_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Buck.Types.file_key_source = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Buck.Types.Locator Glean.Schema.Query.Buck.Types.File_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Buck.Types.file_key_generated = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Buck.Types.LocatorWithLabel Glean.Schema.Query.Buck.Types.File_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Buck.Types.file_key_generatedLabel = Prelude.Just q }

instance Glean.PredicateQuery Glean.Schema.Buck.Types.File where
  toQueryId = Glean.Schema.Query.Buck.Types.File_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.File_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.File = Glean.Schema.Buck.Types.File
type instance Glean.QueryOf Glean.Schema.Buck.Types.File = Glean.Schema.Query.Buck.Types.File

instance Glean.ToQuery Glean.Schema.Buck.Types.File

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.File_1_key = Glean.Schema.Buck.Types.File_1_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.File_1_key = Glean.Schema.Query.Buck.Types.File_1_key

instance Glean.ToQuery Glean.Schema.Buck.Types.File_1_key where
  toQuery (Glean.Schema.Buck.Types.File_1_key_source x) = Data.Default.def { Glean.Schema.Query.Buck.Types.file_1_key_source = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Buck.Types.File_1_key_generated x) = Data.Default.def { Glean.Schema.Query.Buck.Types.file_1_key_generated = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Src.Types.File Glean.Schema.Query.Buck.Types.File_1_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Buck.Types.file_1_key_source = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Buck.Types.Locator Glean.Schema.Query.Buck.Types.File_1_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Buck.Types.file_1_key_generated = Prelude.Just q }

instance Glean.PredicateQuery Glean.Schema.Buck.Types.File_1 where
  toQueryId = Glean.Schema.Query.Buck.Types.File_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.File_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.File_1 = Glean.Schema.Buck.Types.File_1
type instance Glean.QueryOf Glean.Schema.Buck.Types.File_1 = Glean.Schema.Query.Buck.Types.File_1

instance Glean.ToQuery Glean.Schema.Buck.Types.File_1

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.FileResolved_key = Glean.Schema.Buck.Types.FileResolved_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.FileResolved_key = Glean.Schema.Query.Buck.Types.FileResolved_key

instance Glean.ToQuery Glean.Schema.Buck.Types.FileResolved_key where
  toQuery (Glean.Schema.Buck.Types.FileResolved_key x1 x2) = Glean.Schema.Query.Buck.Types.FileResolved_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.FileResolved where
  toQueryId = Glean.Schema.Query.Buck.Types.FileResolved_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.FileResolved_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.FileResolved = Glean.Schema.Buck.Types.FileResolved
type instance Glean.QueryOf Glean.Schema.Buck.Types.FileResolved = Glean.Schema.Query.Buck.Types.FileResolved

instance Glean.ToQuery Glean.Schema.Buck.Types.FileResolved

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Consumer_key = Glean.Schema.Buck.Types.Consumer_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.Consumer_key = Glean.Schema.Query.Buck.Types.Consumer_key

instance Glean.ToQuery Glean.Schema.Buck.Types.Consumer_key where
  toQuery (Glean.Schema.Buck.Types.Consumer_key x1 x2) = Glean.Schema.Query.Buck.Types.Consumer_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.Consumer where
  toQueryId = Glean.Schema.Query.Buck.Types.Consumer_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.Consumer_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Consumer = Glean.Schema.Buck.Types.Consumer
type instance Glean.QueryOf Glean.Schema.Buck.Types.Consumer = Glean.Schema.Query.Buck.Types.Consumer

instance Glean.ToQuery Glean.Schema.Buck.Types.Consumer

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TranslationUnit_key = Glean.Schema.Buck.Types.TranslationUnit_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TranslationUnit_key = Glean.Schema.Query.Buck.Types.TranslationUnit_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TranslationUnit_key where
  toQuery (Glean.Schema.Buck.Types.TranslationUnit_key x1 x2 x3) = Glean.Schema.Query.Buck.Types.TranslationUnit_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Buck.Types.translationUnit_platform_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Buck.Types.translationUnit_platform_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TranslationUnit where
  toQueryId = Glean.Schema.Query.Buck.Types.TranslationUnit_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TranslationUnit_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TranslationUnit = Glean.Schema.Buck.Types.TranslationUnit
type instance Glean.QueryOf Glean.Schema.Buck.Types.TranslationUnit = Glean.Schema.Query.Buck.Types.TranslationUnit

instance Glean.ToQuery Glean.Schema.Buck.Types.TranslationUnit

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TranslationUnit_1_key = Glean.Schema.Buck.Types.TranslationUnit_1_key
type instance Glean.QueryOf Glean.Schema.Buck.Types.TranslationUnit_1_key = Glean.Schema.Query.Buck.Types.TranslationUnit_1_key

instance Glean.ToQuery Glean.Schema.Buck.Types.TranslationUnit_1_key where
  toQuery (Glean.Schema.Buck.Types.TranslationUnit_1_key x1 x2) = Glean.Schema.Query.Buck.Types.TranslationUnit_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buck.Types.TranslationUnit_1 where
  toQueryId = Glean.Schema.Query.Buck.Types.TranslationUnit_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buck.Types.TranslationUnit_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.TranslationUnit_1 = Glean.Schema.Buck.Types.TranslationUnit_1
type instance Glean.QueryOf Glean.Schema.Buck.Types.TranslationUnit_1 = Glean.Schema.Query.Buck.Types.TranslationUnit_1

instance Glean.ToQuery Glean.Schema.Buck.Types.TranslationUnit_1

type instance Glean.QueryResult Glean.Schema.Query.Buck.Types.Dependency = Glean.Schema.Buck.Types.Dependency
type instance Glean.QueryOf Glean.Schema.Buck.Types.Dependency = Glean.Schema.Query.Buck.Types.Dependency

instance Glean.ToQuery Glean.Schema.Buck.Types.Dependency where
  toQuery (Glean.Schema.Buck.Types.Dependency x1 x2 x3) = Glean.Schema.Query.Buck.Types.Dependency (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))
