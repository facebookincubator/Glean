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

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Pp1.Types


type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Include_key = Glean.Schema.Pp1.Types.Include_key
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Include_key = Glean.Schema.Query.Pp1.Types.Include_key

instance Glean.ToQuery Glean.Schema.Pp1.Types.Include_key where
  toQuery (Glean.Schema.Pp1.Types.Include_key x1 x2 x3) = Glean.Schema.Query.Pp1.Types.Include_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Pp1.Types.Include where
  toQueryId = Glean.Schema.Query.Pp1.Types.Include_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Pp1.Types.Include_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Include = Glean.Schema.Pp1.Types.Include
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Include = Glean.Schema.Query.Pp1.Types.Include

instance Glean.ToQuery Glean.Schema.Pp1.Types.Include

instance Glean.PredicateQuery Glean.Schema.Pp1.Types.Macro where
  toQueryId = Glean.Schema.Query.Pp1.Types.Macro_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Pp1.Types.Macro_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Macro = Glean.Schema.Pp1.Types.Macro
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Macro = Glean.Schema.Query.Pp1.Types.Macro

instance Glean.ToQuery Glean.Schema.Pp1.Types.Macro

type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Define_key = Glean.Schema.Pp1.Types.Define_key
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Define_key = Glean.Schema.Query.Pp1.Types.Define_key

instance Glean.ToQuery Glean.Schema.Pp1.Types.Define_key where
  toQuery (Glean.Schema.Pp1.Types.Define_key x1 x2) = Glean.Schema.Query.Pp1.Types.Define_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Pp1.Types.Define where
  toQueryId = Glean.Schema.Query.Pp1.Types.Define_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Pp1.Types.Define_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Define = Glean.Schema.Pp1.Types.Define
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Define = Glean.Schema.Query.Pp1.Types.Define

instance Glean.ToQuery Glean.Schema.Pp1.Types.Define

type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Use_key = Glean.Schema.Pp1.Types.Use_key
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Use_key = Glean.Schema.Query.Pp1.Types.Use_key

instance Glean.ToQuery Glean.Schema.Pp1.Types.Use_key where
  toQuery (Glean.Schema.Pp1.Types.Use_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Pp1.Types.Use_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Pp1.Types.use_definition_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Pp1.Types.use_definition_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5))

instance Glean.PredicateQuery Glean.Schema.Pp1.Types.Use where
  toQueryId = Glean.Schema.Query.Pp1.Types.Use_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Pp1.Types.Use_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Use = Glean.Schema.Pp1.Types.Use
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Use = Glean.Schema.Query.Pp1.Types.Use

instance Glean.ToQuery Glean.Schema.Pp1.Types.Use

type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Undef_key = Glean.Schema.Pp1.Types.Undef_key
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Undef_key = Glean.Schema.Query.Pp1.Types.Undef_key

instance Glean.ToQuery Glean.Schema.Pp1.Types.Undef_key where
  toQuery (Glean.Schema.Pp1.Types.Undef_key x1 x2) = Glean.Schema.Query.Pp1.Types.Undef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Pp1.Types.Undef where
  toQueryId = Glean.Schema.Query.Pp1.Types.Undef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Pp1.Types.Undef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Pp1.Types.Undef = Glean.Schema.Pp1.Types.Undef
type instance Glean.QueryOf Glean.Schema.Pp1.Types.Undef = Glean.Schema.Query.Pp1.Types.Undef

instance Glean.ToQuery Glean.Schema.Pp1.Types.Undef
