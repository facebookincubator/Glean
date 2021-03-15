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
import qualified Glean.Schema.Java.Types


instance Glean.Type Glean.Schema.CodeJava.Types.Entity where
  buildRtsValue b (Glean.Schema.CodeJava.Types.Entity_class_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.CodeJava.Types.Entity_class_
    ]

type instance Angle.SumFields Glean.Schema.CodeJava.Types.Entity = 'Angle.TField "class_" (Glean.KeyType Glean.Schema.Java.Types.ClassDeclaration) ('Angle.TNoFields)

instance Glean.SumBranches Glean.Schema.Java.Types.ClassDeclaration Glean.Schema.CodeJava.Types.Entity where
  injectBranch = Glean.Schema.CodeJava.Types.Entity_class_
  projectBranch (Glean.Schema.CodeJava.Types.Entity_class_ x) = Prelude.Just x
