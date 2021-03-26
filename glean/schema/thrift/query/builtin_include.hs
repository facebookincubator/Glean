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


type instance Glean.QueryResult Glean.Schema.Query.Builtin.Types.Unit = Glean.Schema.Builtin.Types.Unit
type instance Glean.QueryOf Glean.Schema.Builtin.Types.Unit = Glean.Schema.Query.Builtin.Types.Unit

instance Glean.ToQuery Glean.Schema.Builtin.Types.Unit where
  toQuery Glean.Schema.Builtin.Types.Unit = Glean.Schema.Query.Builtin.Types.Unit
