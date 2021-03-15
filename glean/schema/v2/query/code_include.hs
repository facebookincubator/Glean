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

import qualified Glean.Schema.CodeCxx.Types
import qualified Glean.Schema.Query.CodeCxx.Types

import qualified Glean.Schema.CodeFlow.Types
import qualified Glean.Schema.Query.CodeFlow.Types

import qualified Glean.Schema.CodeHack.Types
import qualified Glean.Schema.Query.CodeHack.Types

import qualified Glean.Schema.CodeHs.Types
import qualified Glean.Schema.Query.CodeHs.Types

import qualified Glean.Schema.CodeJava.Types
import qualified Glean.Schema.Query.CodeJava.Types

import qualified Glean.Schema.CodePython.Types
import qualified Glean.Schema.Query.CodePython.Types

import qualified Glean.Schema.Pp1.Types
import qualified Glean.Schema.Query.Pp1.Types

import qualified Glean.Schema.Code.Types


type instance Glean.QueryResult Glean.Schema.Query.Code.Types.Entity = Glean.Schema.Code.Types.Entity
type instance Glean.QueryOf Glean.Schema.Code.Types.Entity = Glean.Schema.Query.Code.Types.Entity

instance Glean.ToQuery Glean.Schema.Code.Types.Entity where
  toQuery (Glean.Schema.Code.Types.Entity_cxx x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_cxx = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_pp x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_pp = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_java x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_java = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_hs x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_hs = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_python x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_python = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_hack x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_hack = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_flow x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_flow = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.CodeCxx.Types.Entity Glean.Schema.Query.Code.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_cxx = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Pp1.Types.Define Glean.Schema.Query.Code.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_pp = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodeJava.Types.Entity Glean.Schema.Query.Code.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_java = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodeHs.Types.Entity Glean.Schema.Query.Code.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_hs = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodePython.Types.Entity Glean.Schema.Query.Code.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_python = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodeHack.Types.Entity Glean.Schema.Query.Code.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_hack = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodeFlow.Types.Entity Glean.Schema.Query.Code.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_flow = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Code.Types.Entity_8 = Glean.Schema.Code.Types.Entity_8
type instance Glean.QueryOf Glean.Schema.Code.Types.Entity_8 = Glean.Schema.Query.Code.Types.Entity_8

instance Glean.ToQuery Glean.Schema.Code.Types.Entity_8 where
  toQuery (Glean.Schema.Code.Types.Entity_8_cxx x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_8_cxx = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_8_pp x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_8_pp = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_8_java x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_8_java = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_8_hs x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_8_hs = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_8_python x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_8_python = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Code.Types.Entity_8_hack x) = Data.Default.def { Glean.Schema.Query.Code.Types.entity_8_hack = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.CodeCxx.Types.Entity Glean.Schema.Query.Code.Types.Entity_8 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_8_cxx = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Pp1.Types.Define Glean.Schema.Query.Code.Types.Entity_8 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_8_pp = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodeJava.Types.Entity_2 Glean.Schema.Query.Code.Types.Entity_8 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_8_java = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodeHs.Types.Entity Glean.Schema.Query.Code.Types.Entity_8 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_8_hs = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodePython.Types.Entity Glean.Schema.Query.Code.Types.Entity_8 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_8_python = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodeHack.Types.Entity Glean.Schema.Query.Code.Types.Entity_8 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Code.Types.entity_8_hack = Prelude.Just q }
