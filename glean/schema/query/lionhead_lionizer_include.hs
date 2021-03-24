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

import qualified Glean.Schema.Cxx1.Types
import qualified Glean.Schema.Query.Cxx1.Types

import qualified Glean.Schema.Docmarkup.Types
import qualified Glean.Schema.Query.Docmarkup.Types

import qualified Glean.Schema.LionheadLionizer.Types


type instance Glean.QueryResult Glean.Schema.Query.LionheadLionizer.Types.FindFunctionWithDef_key = Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key
type instance Glean.QueryOf Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key = Glean.Schema.Query.LionheadLionizer.Types.FindFunctionWithDef_key

instance Glean.ToQuery Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key where
  toQuery (Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key x1 x2 x3 x4) = Glean.Schema.Query.LionheadLionizer.Types.FindFunctionWithDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef where
  toQueryId = Glean.Schema.Query.LionheadLionizer.Types.FindFunctionWithDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.LionheadLionizer.Types.FindFunctionWithDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.LionheadLionizer.Types.FindFunctionWithDef = Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef
type instance Glean.QueryOf Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef = Glean.Schema.Query.LionheadLionizer.Types.FindFunctionWithDef

instance Glean.ToQuery Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef

type instance Glean.QueryResult Glean.Schema.Query.LionheadLionizer.Types.FindFunction_key = Glean.Schema.LionheadLionizer.Types.FindFunction_key
type instance Glean.QueryOf Glean.Schema.LionheadLionizer.Types.FindFunction_key = Glean.Schema.Query.LionheadLionizer.Types.FindFunction_key

instance Glean.ToQuery Glean.Schema.LionheadLionizer.Types.FindFunction_key where
  toQuery (Glean.Schema.LionheadLionizer.Types.FindFunction_key x1 x2 x3) = Glean.Schema.Query.LionheadLionizer.Types.FindFunction_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.LionheadLionizer.Types.FindFunction where
  toQueryId = Glean.Schema.Query.LionheadLionizer.Types.FindFunction_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.LionheadLionizer.Types.FindFunction_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.LionheadLionizer.Types.FindFunction = Glean.Schema.LionheadLionizer.Types.FindFunction
type instance Glean.QueryOf Glean.Schema.LionheadLionizer.Types.FindFunction = Glean.Schema.Query.LionheadLionizer.Types.FindFunction

instance Glean.ToQuery Glean.Schema.LionheadLionizer.Types.FindFunction
