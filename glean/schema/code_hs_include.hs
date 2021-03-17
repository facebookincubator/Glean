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
import qualified Glean.Schema.Hs.Types


instance Glean.Type Glean.Schema.CodeHs.Types.Entity where
  buildRtsValue b (Glean.Schema.CodeHs.Types.Entity_function_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeHs.Types.Entity_class_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.CodeHs.Types.Entity_function_
    , Glean.mapD Glean.Schema.CodeHs.Types.Entity_class_
    ]

type instance Angle.SumFields Glean.Schema.CodeHs.Types.Entity = 'Angle.TField "function_" (Glean.KeyType Glean.Schema.Hs.Types.FunctionDefinition) ('Angle.TField "class_" (Glean.KeyType Glean.Schema.Hs.Types.Class) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.Hs.Types.FunctionDefinition Glean.Schema.CodeHs.Types.Entity where
  injectBranch = Glean.Schema.CodeHs.Types.Entity_function_
  projectBranch (Glean.Schema.CodeHs.Types.Entity_function_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hs.Types.Class Glean.Schema.CodeHs.Types.Entity where
  injectBranch = Glean.Schema.CodeHs.Types.Entity_class_
  projectBranch (Glean.Schema.CodeHs.Types.Entity_class_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
