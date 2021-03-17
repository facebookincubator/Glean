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

import qualified Glean.Schema.Pp1.Types
import qualified Glean.Schema.Query.Pp1.Types

import qualified Glean.Schema.SearchPp.Types


type instance Glean.QueryResult Glean.Schema.Query.SearchPp.Types.SearchByName_key = Glean.Schema.SearchPp.Types.SearchByName_key
type instance Glean.QueryOf Glean.Schema.SearchPp.Types.SearchByName_key = Glean.Schema.Query.SearchPp.Types.SearchByName_key

instance Glean.ToQuery Glean.Schema.SearchPp.Types.SearchByName_key where
  toQuery (Glean.Schema.SearchPp.Types.SearchByName_key x1 x2) = Glean.Schema.Query.SearchPp.Types.SearchByName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.SearchPp.Types.SearchByName where
  toQueryId = Glean.Schema.Query.SearchPp.Types.SearchByName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchPp.Types.SearchByName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchPp.Types.SearchByName = Glean.Schema.SearchPp.Types.SearchByName
type instance Glean.QueryOf Glean.Schema.SearchPp.Types.SearchByName = Glean.Schema.Query.SearchPp.Types.SearchByName

instance Glean.ToQuery Glean.Schema.SearchPp.Types.SearchByName

type instance Glean.QueryResult Glean.Schema.Query.SearchPp.Types.SearchByName_1_key = Glean.Schema.SearchPp.Types.SearchByName_1_key
type instance Glean.QueryOf Glean.Schema.SearchPp.Types.SearchByName_1_key = Glean.Schema.Query.SearchPp.Types.SearchByName_1_key

instance Glean.ToQuery Glean.Schema.SearchPp.Types.SearchByName_1_key where
  toQuery (Glean.Schema.SearchPp.Types.SearchByName_1_key x1 x2) = Glean.Schema.Query.SearchPp.Types.SearchByName_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.SearchPp.Types.SearchByName_1 where
  toQueryId = Glean.Schema.Query.SearchPp.Types.SearchByName_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchPp.Types.SearchByName_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchPp.Types.SearchByName_1 = Glean.Schema.SearchPp.Types.SearchByName_1
type instance Glean.QueryOf Glean.Schema.SearchPp.Types.SearchByName_1 = Glean.Schema.Query.SearchPp.Types.SearchByName_1

instance Glean.ToQuery Glean.Schema.SearchPp.Types.SearchByName_1
