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

import qualified Glean.Schema.Hs.Types
import qualified Glean.Schema.Query.Hs.Types

import qualified Glean.Schema.CodeHs.Types


type instance Glean.QueryResult Glean.Schema.Query.CodeHs.Types.Entity = Glean.Schema.CodeHs.Types.Entity
type instance Glean.QueryOf Glean.Schema.CodeHs.Types.Entity = Glean.Schema.Query.CodeHs.Types.Entity

instance Glean.ToQuery Glean.Schema.CodeHs.Types.Entity where
  toQuery (Glean.Schema.CodeHs.Types.Entity_function_ x) = Data.Default.def { Glean.Schema.Query.CodeHs.Types.entity_function_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeHs.Types.Entity_class_ x) = Data.Default.def { Glean.Schema.Query.CodeHs.Types.entity_class_ = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.FunctionDefinition Glean.Schema.Query.CodeHs.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeHs.Types.entity_function_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.Class Glean.Schema.Query.CodeHs.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeHs.Types.entity_class_ = Prelude.Just q }
