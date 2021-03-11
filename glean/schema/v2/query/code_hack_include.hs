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

import qualified Glean.Schema.CodeHack.Types


type instance Glean.QueryResult Glean.Schema.Query.CodeHack.Types.Entity = Glean.Schema.CodeHack.Types.Entity
type instance Glean.QueryOf Glean.Schema.CodeHack.Types.Entity = Glean.Schema.Query.CodeHack.Types.Entity

instance Glean.ToQuery Glean.Schema.CodeHack.Types.Entity where
  toQuery (Glean.Schema.CodeHack.Types.Entity_decl x) = Data.Default.def { Glean.Schema.Query.CodeHack.Types.entity_decl = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.Declaration Glean.Schema.Query.CodeHack.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeHack.Types.entity_decl = Prelude.Just q }
