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

import qualified Glean.Schema.Flow.Types
import qualified Glean.Schema.Query.Flow.Types

import qualified Glean.Schema.CodeFlow.Types


type instance Glean.QueryResult Glean.Schema.Query.CodeFlow.Types.Entity = Glean.Schema.CodeFlow.Types.Entity
type instance Glean.QueryOf Glean.Schema.CodeFlow.Types.Entity = Glean.Schema.Query.CodeFlow.Types.Entity

instance Glean.ToQuery Glean.Schema.CodeFlow.Types.Entity where
  toQuery (Glean.Schema.CodeFlow.Types.Entity_decl x) = Data.Default.def { Glean.Schema.Query.CodeFlow.Types.entity_decl = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeFlow.Types.Entity_module_ x) = Data.Default.def { Glean.Schema.Query.CodeFlow.Types.entity_module_ = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.SomeDeclaration Glean.Schema.Query.CodeFlow.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeFlow.Types.entity_decl = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Module Glean.Schema.Query.CodeFlow.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeFlow.Types.entity_module_ = Prelude.Just q }
