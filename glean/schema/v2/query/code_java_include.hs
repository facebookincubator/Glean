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

import qualified Glean.Schema.Java.Types
import qualified Glean.Schema.Query.Java.Types

import qualified Glean.Schema.CodeJava.Types


type instance Glean.QueryResult Glean.Schema.Query.CodeJava.Types.Entity = Glean.Schema.CodeJava.Types.Entity
type instance Glean.QueryOf Glean.Schema.CodeJava.Types.Entity = Glean.Schema.Query.CodeJava.Types.Entity

instance Glean.ToQuery Glean.Schema.CodeJava.Types.Entity where
  toQuery (Glean.Schema.CodeJava.Types.Entity_class_ x) = Data.Default.def { Glean.Schema.Query.CodeJava.Types.entity_class_ = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Java.Types.ClassDeclaration Glean.Schema.Query.CodeJava.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeJava.Types.entity_class_ = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.CodeJava.Types.Entity_2 = Glean.Schema.CodeJava.Types.Entity_2
type instance Glean.QueryOf Glean.Schema.CodeJava.Types.Entity_2 = Glean.Schema.Query.CodeJava.Types.Entity_2

instance Glean.ToQuery Glean.Schema.CodeJava.Types.Entity_2 where
  toQuery (Glean.Schema.CodeJava.Types.Entity_2_class_ x) = Data.Default.def { Glean.Schema.Query.CodeJava.Types.entity_2_class_ = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Java.Types.ClassDeclaration_3 Glean.Schema.Query.CodeJava.Types.Entity_2 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeJava.Types.entity_2_class_ = Prelude.Just q }
