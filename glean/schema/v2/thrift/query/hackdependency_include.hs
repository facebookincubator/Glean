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

import qualified Glean.Schema.Hackdependency.Types


type instance Glean.QueryResult Glean.Schema.Query.Hackdependency.Types.Inheritance_key = Glean.Schema.Hackdependency.Types.Inheritance_key
type instance Glean.QueryOf Glean.Schema.Hackdependency.Types.Inheritance_key = Glean.Schema.Query.Hackdependency.Types.Inheritance_key

instance Glean.ToQuery Glean.Schema.Hackdependency.Types.Inheritance_key where
  toQuery (Glean.Schema.Hackdependency.Types.Inheritance_key x1 x2) = Glean.Schema.Query.Hackdependency.Types.Inheritance_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hackdependency.Types.Inheritance where
  toQueryId = Glean.Schema.Query.Hackdependency.Types.Inheritance_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hackdependency.Types.Inheritance_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hackdependency.Types.Inheritance = Glean.Schema.Hackdependency.Types.Inheritance
type instance Glean.QueryOf Glean.Schema.Hackdependency.Types.Inheritance = Glean.Schema.Query.Hackdependency.Types.Inheritance

instance Glean.ToQuery Glean.Schema.Hackdependency.Types.Inheritance

instance Glean.PredicateQuery Glean.Schema.Hackdependency.Types.Name where
  toQueryId = Glean.Schema.Query.Hackdependency.Types.Name_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hackdependency.Types.Name_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hackdependency.Types.Name = Glean.Schema.Hackdependency.Types.Name
type instance Glean.QueryOf Glean.Schema.Hackdependency.Types.Name = Glean.Schema.Query.Hackdependency.Types.Name

instance Glean.ToQuery Glean.Schema.Hackdependency.Types.Name
