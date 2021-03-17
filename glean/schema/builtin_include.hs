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


type instance Angle.SumFields (Prelude.Maybe t) =
  'Angle.TField "nothing" Unit (
  'Angle.TField "just" t
  'Angle.TNoFields)


instance Glean.Type Glean.Schema.Builtin.Types.Unit where
  buildRtsValue _b Glean.Schema.Builtin.Types.Unit = Prelude.return ()
  decodeRtsValue = Prelude.pure Glean.Schema.Builtin.Types.Unit

type instance Angle.RecordFields Glean.Schema.Builtin.Types.Unit = 'Angle.TNoFields
