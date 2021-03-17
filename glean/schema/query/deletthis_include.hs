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

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Deletthis.Types


type instance Glean.QueryResult Glean.Schema.Query.Deletthis.Types.FileReverseDeps_key = Glean.Schema.Deletthis.Types.FileReverseDeps_key
type instance Glean.QueryOf Glean.Schema.Deletthis.Types.FileReverseDeps_key = Glean.Schema.Query.Deletthis.Types.FileReverseDeps_key

instance Glean.ToQuery Glean.Schema.Deletthis.Types.FileReverseDeps_key where
  toQuery (Glean.Schema.Deletthis.Types.FileReverseDeps_key x1 x2 x3) = Glean.Schema.Query.Deletthis.Types.FileReverseDeps_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Deletthis.Types.FileReverseDeps where
  toQueryId = Glean.Schema.Query.Deletthis.Types.FileReverseDeps_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Deletthis.Types.FileReverseDeps_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Deletthis.Types.FileReverseDeps = Glean.Schema.Deletthis.Types.FileReverseDeps
type instance Glean.QueryOf Glean.Schema.Deletthis.Types.FileReverseDeps = Glean.Schema.Query.Deletthis.Types.FileReverseDeps

instance Glean.ToQuery Glean.Schema.Deletthis.Types.FileReverseDeps
