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
import qualified Glean.Schema.Python.Types


instance Glean.Type Glean.Schema.CodePython.Types.Entity where
  buildRtsValue b (Glean.Schema.CodePython.Types.Entity_decl x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.CodePython.Types.Entity_decl
    ]

type instance Angle.SumFields Glean.Schema.CodePython.Types.Entity = 'Angle.TField "decl" (Glean.Schema.Python.Types.Declaration) ('Angle.TNoFields)

instance Glean.SumBranches Glean.Schema.Python.Types.Declaration Glean.Schema.CodePython.Types.Entity where
  injectBranch = Glean.Schema.CodePython.Types.Entity_decl
  projectBranch (Glean.Schema.CodePython.Types.Entity_decl x) = Prelude.Just x
