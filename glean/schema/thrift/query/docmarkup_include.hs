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

import qualified Glean.Schema.Code.Types
import qualified Glean.Schema.Query.Code.Types

import qualified Glean.Schema.Hack.Types
import qualified Glean.Schema.Query.Hack.Types

import qualified Glean.Schema.Java.Types
import qualified Glean.Schema.Query.Java.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Docmarkup.Types


type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.EntityAnnotations_key = Glean.Schema.Docmarkup.Types.EntityAnnotations_key
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.EntityAnnotations_key = Glean.Schema.Query.Docmarkup.Types.EntityAnnotations_key

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.EntityAnnotations_key where
  toQuery (Glean.Schema.Docmarkup.Types.EntityAnnotations_key x1 x2) = Glean.Schema.Query.Docmarkup.Types.EntityAnnotations_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Docmarkup.Types.EntityAnnotations where
  toQueryId = Glean.Schema.Query.Docmarkup.Types.EntityAnnotations_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Docmarkup.Types.EntityAnnotations_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.EntityAnnotations = Glean.Schema.Docmarkup.Types.EntityAnnotations
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.EntityAnnotations = Glean.Schema.Query.Docmarkup.Types.EntityAnnotations

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.EntityAnnotations

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.EntityComments_key = Glean.Schema.Docmarkup.Types.EntityComments_key
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.EntityComments_key = Glean.Schema.Query.Docmarkup.Types.EntityComments_key

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.EntityComments_key where
  toQuery (Glean.Schema.Docmarkup.Types.EntityComments_key x1 x2 x3) = Glean.Schema.Query.Docmarkup.Types.EntityComments_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Docmarkup.Types.EntityComments where
  toQueryId = Glean.Schema.Query.Docmarkup.Types.EntityComments_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Docmarkup.Types.EntityComments_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.EntityComments = Glean.Schema.Docmarkup.Types.EntityComments
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.EntityComments = Glean.Schema.Query.Docmarkup.Types.EntityComments

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.EntityComments

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.DocAttr_key = Glean.Schema.Docmarkup.Types.DocAttr_key
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.DocAttr_key = Glean.Schema.Query.Docmarkup.Types.DocAttr_key

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.DocAttr_key where
  toQuery (Glean.Schema.Docmarkup.Types.DocAttr_key x1 x2) = Glean.Schema.Query.Docmarkup.Types.DocAttr_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Docmarkup.Types.DocAttr where
  toQueryId = Glean.Schema.Query.Docmarkup.Types.DocAttr_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Docmarkup.Types.DocAttr_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.DocAttr = Glean.Schema.Docmarkup.Types.DocAttr
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.DocAttr = Glean.Schema.Query.Docmarkup.Types.DocAttr

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.DocAttr

instance Glean.PredicateQuery Glean.Schema.Docmarkup.Types.DocAttrKey where
  toQueryId = Glean.Schema.Query.Docmarkup.Types.DocAttrKey_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Docmarkup.Types.DocAttrKey_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.DocAttrKey = Glean.Schema.Docmarkup.Types.DocAttrKey
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.DocAttrKey = Glean.Schema.Query.Docmarkup.Types.DocAttrKey

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.DocAttrKey

instance Glean.PredicateQuery Glean.Schema.Docmarkup.Types.EntityDocAttr where
  toQueryId = Glean.Schema.Query.Docmarkup.Types.EntityDocAttr_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Docmarkup.Types.EntityDocAttr_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.EntityDocAttr = Glean.Schema.Docmarkup.Types.EntityDocAttr
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.EntityDocAttr = Glean.Schema.Query.Docmarkup.Types.EntityDocAttr

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.EntityDocAttr

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.EntityByDocAttrKey_key = Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key = Glean.Schema.Query.Docmarkup.Types.EntityByDocAttrKey_key

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key where
  toQuery (Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key x1 x2) = Glean.Schema.Query.Docmarkup.Types.EntityByDocAttrKey_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Docmarkup.Types.EntityByDocAttrKey where
  toQueryId = Glean.Schema.Query.Docmarkup.Types.EntityByDocAttrKey_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Docmarkup.Types.EntityByDocAttrKey_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.EntityByDocAttrKey = Glean.Schema.Docmarkup.Types.EntityByDocAttrKey
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.EntityByDocAttrKey = Glean.Schema.Query.Docmarkup.Types.EntityByDocAttrKey

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.EntityByDocAttrKey

type instance Glean.QueryResult Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations = Glean.Schema.Docmarkup.Types.GeneralAnnotations
type instance Glean.QueryOf Glean.Schema.Docmarkup.Types.GeneralAnnotations = Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations

instance Glean.ToQuery Glean.Schema.Docmarkup.Types.GeneralAnnotations where
  toQuery (Glean.Schema.Docmarkup.Types.GeneralAnnotations_doc x) = Data.Default.def { Glean.Schema.Query.Docmarkup.Types.generalAnnotations_doc = Prelude.Just ((Glean.Schema.Query.Docmarkup.Types.DocAttrs_exact . Prelude.map Glean.toQuery) x) }
  toQuery (Glean.Schema.Docmarkup.Types.GeneralAnnotations_hack x) = Data.Default.def { Glean.Schema.Query.Docmarkup.Types.generalAnnotations_hack = Prelude.Just ((Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations_hack__array_exact . Prelude.map Glean.toQuery) x) }
  toQuery (Glean.Schema.Docmarkup.Types.GeneralAnnotations_java x) = Data.Default.def { Glean.Schema.Query.Docmarkup.Types.generalAnnotations_java = Prelude.Just ((Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations_java__array_exact . Prelude.map Glean.toQuery) x) }

instance Glean.SumQuery Glean.Schema.Query.Docmarkup.Types.DocAttrs Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Docmarkup.Types.generalAnnotations_doc = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations_hack__array Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Docmarkup.Types.generalAnnotations_hack = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations_java__array Glean.Schema.Query.Docmarkup.Types.GeneralAnnotations where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Docmarkup.Types.generalAnnotations_java = Prelude.Just q }
