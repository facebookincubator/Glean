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

import qualified Glean.Schema.Sys.Types


instance Glean.PredicateQuery Glean.Schema.Sys.Types.Blob where
  toQueryId = Glean.Schema.Query.Sys.Types.Blob_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Sys.Types.Blob_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Sys.Types.Blob = Glean.Schema.Sys.Types.Blob
type instance Glean.QueryOf Glean.Schema.Sys.Types.Blob = Glean.Schema.Query.Sys.Types.Blob

instance Glean.ToQuery Glean.Schema.Sys.Types.Blob
