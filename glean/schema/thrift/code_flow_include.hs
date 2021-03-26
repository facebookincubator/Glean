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
import qualified Glean.Schema.Flow.Types


instance Glean.Type Glean.Schema.CodeFlow.Types.Entity where
  buildRtsValue b (Glean.Schema.CodeFlow.Types.Entity_decl x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeFlow.Types.Entity_module_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.CodeFlow.Types.Entity_decl
    , Glean.mapD Glean.Schema.CodeFlow.Types.Entity_module_
    ]

type instance Angle.SumFields Glean.Schema.CodeFlow.Types.Entity = 'Angle.TField "decl" (Glean.Schema.Flow.Types.SomeDeclaration) ('Angle.TField "module_" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.Flow.Types.SomeDeclaration Glean.Schema.CodeFlow.Types.Entity where
  injectBranch = Glean.Schema.CodeFlow.Types.Entity_decl
  projectBranch (Glean.Schema.CodeFlow.Types.Entity_decl x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.Module Glean.Schema.CodeFlow.Types.Entity where
  injectBranch = Glean.Schema.CodeFlow.Types.Entity_module_
  projectBranch (Glean.Schema.CodeFlow.Types.Entity_module_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
