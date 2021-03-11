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

import qualified Glean.Schema.Hack.Types
import qualified Glean.Schema.Query.Hack.Types

import qualified Glean.Schema.SearchHack.Types


type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInContext_key = Glean.Schema.SearchHack.Types.SearchInContext_key
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInContext_key = Glean.Schema.Query.SearchHack.Types.SearchInContext_key

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInContext_key where
  toQuery (Glean.Schema.SearchHack.Types.SearchInContext_key x1 x2 x3 x4) = Glean.Schema.Query.SearchHack.Types.SearchInContext_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInContext_contextNamespace_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInContext_contextNamespace_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.SearchHack.Types.SearchInContext where
  toQueryId = Glean.Schema.Query.SearchHack.Types.SearchInContext_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchHack.Types.SearchInContext_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInContext = Glean.Schema.SearchHack.Types.SearchInContext
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInContext = Glean.Schema.Query.SearchHack.Types.SearchInContext

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInContext

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInNamespace_key = Glean.Schema.SearchHack.Types.SearchInNamespace_key
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInNamespace_key = Glean.Schema.Query.SearchHack.Types.SearchInNamespace_key

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInNamespace_key where
  toQuery (Glean.Schema.SearchHack.Types.SearchInNamespace_key x1 x2 x3) = Glean.Schema.Query.SearchHack.Types.SearchInNamespace_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInNamespace_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInNamespace_namespace__just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.SearchHack.Types.SearchInNamespace where
  toQueryId = Glean.Schema.Query.SearchHack.Types.SearchInNamespace_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchHack.Types.SearchInNamespace_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInNamespace = Glean.Schema.SearchHack.Types.SearchInNamespace
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInNamespace = Glean.Schema.Query.SearchHack.Types.SearchInNamespace

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInNamespace

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchByName_key = Glean.Schema.SearchHack.Types.SearchByName_key
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchByName_key = Glean.Schema.Query.SearchHack.Types.SearchByName_key

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchByName_key where
  toQuery (Glean.Schema.SearchHack.Types.SearchByName_key x1 x2) = Glean.Schema.Query.SearchHack.Types.SearchByName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.SearchHack.Types.SearchByName where
  toQueryId = Glean.Schema.Query.SearchHack.Types.SearchByName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchHack.Types.SearchByName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchByName = Glean.Schema.SearchHack.Types.SearchByName
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchByName = Glean.Schema.Query.SearchHack.Types.SearchByName

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchByName

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInContainer_key = Glean.Schema.SearchHack.Types.SearchInContainer_key
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInContainer_key = Glean.Schema.Query.SearchHack.Types.SearchInContainer_key

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInContainer_key where
  toQuery (Glean.Schema.SearchHack.Types.SearchInContainer_key x1 x2 x3 x4) = Glean.Schema.Query.SearchHack.Types.SearchInContainer_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInContainer_containerNamespace_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInContainer_containerNamespace_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.SearchHack.Types.SearchInContainer where
  toQueryId = Glean.Schema.Query.SearchHack.Types.SearchInContainer_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchHack.Types.SearchInContainer_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInContainer = Glean.Schema.SearchHack.Types.SearchInContainer
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInContainer = Glean.Schema.Query.SearchHack.Types.SearchInContainer

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInContainer

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInEnum_key = Glean.Schema.SearchHack.Types.SearchInEnum_key
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInEnum_key = Glean.Schema.Query.SearchHack.Types.SearchInEnum_key

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInEnum_key where
  toQuery (Glean.Schema.SearchHack.Types.SearchInEnum_key x1 x2 x3 x4) = Glean.Schema.Query.SearchHack.Types.SearchInEnum_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInEnum_enumNamespace_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInEnum_enumNamespace_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.SearchHack.Types.SearchInEnum where
  toQueryId = Glean.Schema.Query.SearchHack.Types.SearchInEnum_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchHack.Types.SearchInEnum_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInEnum = Glean.Schema.SearchHack.Types.SearchInEnum
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInEnum = Glean.Schema.Query.SearchHack.Types.SearchInEnum

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInEnum

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInContainerOrEnum_key = Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key = Glean.Schema.Query.SearchHack.Types.SearchInContainerOrEnum_key

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key where
  toQuery (Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key x1 x2 x3 x4) = Glean.Schema.Query.SearchHack.Types.SearchInContainerOrEnum_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInContainerOrEnum_contextNamespace_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.SearchHack.Types.searchInContainerOrEnum_contextNamespace_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.SearchHack.Types.SearchInContainerOrEnum where
  toQueryId = Glean.Schema.Query.SearchHack.Types.SearchInContainerOrEnum_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchHack.Types.SearchInContainerOrEnum_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchHack.Types.SearchInContainerOrEnum = Glean.Schema.SearchHack.Types.SearchInContainerOrEnum
type instance Glean.QueryOf Glean.Schema.SearchHack.Types.SearchInContainerOrEnum = Glean.Schema.Query.SearchHack.Types.SearchInContainerOrEnum

instance Glean.ToQuery Glean.Schema.SearchHack.Types.SearchInContainerOrEnum
