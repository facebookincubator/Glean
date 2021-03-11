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

import qualified Glean.Schema.Python.Types
import qualified Glean.Schema.Query.Python.Types

import qualified Glean.Schema.CodePython.Types


type instance Glean.QueryResult Glean.Schema.Query.CodePython.Types.Entity = Glean.Schema.CodePython.Types.Entity
type instance Glean.QueryOf Glean.Schema.CodePython.Types.Entity = Glean.Schema.Query.CodePython.Types.Entity

instance Glean.ToQuery Glean.Schema.CodePython.Types.Entity where
  toQuery (Glean.Schema.CodePython.Types.Entity_decl x) = Data.Default.def { Glean.Schema.Query.CodePython.Types.entity_decl = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.Declaration Glean.Schema.Query.CodePython.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodePython.Types.entity_decl = Prelude.Just q }
