-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Buck.Types
import qualified Glean.Schema.Query.Buck.Types

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Cxx1.Types
import qualified Glean.Schema.Query.Cxx1.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Buckuses.Types


type instance Glean.QueryResult Glean.Schema.Query.Buckuses.Types.UsesOfTarget_key = Glean.Schema.Buckuses.Types.UsesOfTarget_key
type instance Glean.QueryOf Glean.Schema.Buckuses.Types.UsesOfTarget_key = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_key

instance Glean.ToQuery Glean.Schema.Buckuses.Types.UsesOfTarget_key where
  toQuery (Glean.Schema.Buckuses.Types.UsesOfTarget_key x1 x2 x3) = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Buckuses.Types.UsesOfTarget where
  toQueryId = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buckuses.Types.UsesOfTarget = Glean.Schema.Buckuses.Types.UsesOfTarget
type instance Glean.QueryOf Glean.Schema.Buckuses.Types.UsesOfTarget = Glean.Schema.Query.Buckuses.Types.UsesOfTarget

instance Glean.ToQuery Glean.Schema.Buckuses.Types.UsesOfTarget

type instance Glean.QueryResult Glean.Schema.Query.Buckuses.Types.UsesOfTarget_1_key = Glean.Schema.Buckuses.Types.UsesOfTarget_1_key
type instance Glean.QueryOf Glean.Schema.Buckuses.Types.UsesOfTarget_1_key = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_1_key

instance Glean.ToQuery Glean.Schema.Buckuses.Types.UsesOfTarget_1_key where
  toQuery (Glean.Schema.Buckuses.Types.UsesOfTarget_1_key x1 x2 x3) = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Buckuses.Types.UsesOfTarget_1 where
  toQueryId = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buckuses.Types.UsesOfTarget_1 = Glean.Schema.Buckuses.Types.UsesOfTarget_1
type instance Glean.QueryOf Glean.Schema.Buckuses.Types.UsesOfTarget_1 = Glean.Schema.Query.Buckuses.Types.UsesOfTarget_1

instance Glean.ToQuery Glean.Schema.Buckuses.Types.UsesOfTarget_1

type instance Glean.QueryResult Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader_key = Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key
type instance Glean.QueryOf Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader_key

instance Glean.ToQuery Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key where
  toQuery (Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key x1 x2) = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buckuses.Types.UsesOfTargetHeader where
  toQueryId = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader = Glean.Schema.Buckuses.Types.UsesOfTargetHeader
type instance Glean.QueryOf Glean.Schema.Buckuses.Types.UsesOfTargetHeader = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader

instance Glean.ToQuery Glean.Schema.Buckuses.Types.UsesOfTargetHeader

type instance Glean.QueryResult Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader1_key = Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key
type instance Glean.QueryOf Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader1_key

instance Glean.ToQuery Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key where
  toQuery (Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key x1 x2) = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Buckuses.Types.UsesOfTargetHeader1 where
  toQueryId = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader1 = Glean.Schema.Buckuses.Types.UsesOfTargetHeader1
type instance Glean.QueryOf Glean.Schema.Buckuses.Types.UsesOfTargetHeader1 = Glean.Schema.Query.Buckuses.Types.UsesOfTargetHeader1

instance Glean.ToQuery Glean.Schema.Buckuses.Types.UsesOfTargetHeader1
