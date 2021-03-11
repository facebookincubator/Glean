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
import qualified Glean.Schema.Flow.Types


instance Glean.Type Glean.Schema.CodeFlow.Types.Entity where
  buildRtsValue b (Glean.Schema.CodeFlow.Types.Entity_decl x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.CodeFlow.Types.Entity_decl
    ]

type instance Angle.SumFields Glean.Schema.CodeFlow.Types.Entity = 'Angle.TField "decl" (Glean.Schema.Flow.Types.SomeDeclaration) ('Angle.TNoFields)

instance Glean.SumBranches Glean.Schema.Flow.Types.SomeDeclaration Glean.Schema.CodeFlow.Types.Entity where
  injectBranch = Glean.Schema.CodeFlow.Types.Entity_decl
  projectBranch (Glean.Schema.CodeFlow.Types.Entity_decl x) = Prelude.Just x
