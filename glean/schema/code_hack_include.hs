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
import qualified Glean.Schema.Hack.Types


instance Glean.Type Glean.Schema.CodeHack.Types.Entity where
  buildRtsValue b (Glean.Schema.CodeHack.Types.Entity_decl x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.CodeHack.Types.Entity_decl
    ]

type instance Angle.SumFields Glean.Schema.CodeHack.Types.Entity = 'Angle.TField "decl" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields)

instance Glean.SumBranches Glean.Schema.Hack.Types.Declaration Glean.Schema.CodeHack.Types.Entity where
  injectBranch = Glean.Schema.CodeHack.Types.Entity_decl
  projectBranch (Glean.Schema.CodeHack.Types.Entity_decl x) = Prelude.Just x
