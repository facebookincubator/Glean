-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Testinfra.Types
import qualified Glean.Schema.Query.Testinfra.Types

import qualified Glean.Schema.Lionhead.Types


type instance Glean.QueryResult Glean.Schema.Query.Lionhead.Types.CoveredHarness_key = Glean.Schema.Lionhead.Types.CoveredHarness_key
type instance Glean.QueryOf Glean.Schema.Lionhead.Types.CoveredHarness_key = Glean.Schema.Query.Lionhead.Types.CoveredHarness_key

instance Glean.ToQuery Glean.Schema.Lionhead.Types.CoveredHarness_key where
  toQuery (Glean.Schema.Lionhead.Types.CoveredHarness_key x1 x2) = Glean.Schema.Query.Lionhead.Types.CoveredHarness_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Lionhead.Types.CoveredHarness where
  toQueryId = Glean.Schema.Query.Lionhead.Types.CoveredHarness_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Lionhead.Types.CoveredHarness_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Lionhead.Types.CoveredHarness = Glean.Schema.Lionhead.Types.CoveredHarness
type instance Glean.QueryOf Glean.Schema.Lionhead.Types.CoveredHarness = Glean.Schema.Query.Lionhead.Types.CoveredHarness

instance Glean.ToQuery Glean.Schema.Lionhead.Types.CoveredHarness

instance Glean.PredicateQuery Glean.Schema.Lionhead.Types.FbId where
  toQueryId = Glean.Schema.Query.Lionhead.Types.FbId_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Lionhead.Types.FbId_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Lionhead.Types.FbId = Glean.Schema.Lionhead.Types.FbId
type instance Glean.QueryOf Glean.Schema.Lionhead.Types.FbId = Glean.Schema.Query.Lionhead.Types.FbId

instance Glean.ToQuery Glean.Schema.Lionhead.Types.FbId
