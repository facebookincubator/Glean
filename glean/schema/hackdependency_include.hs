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


instance Glean.Type Glean.Schema.Hackdependency.Types.Inheritance_key where
  buildRtsValue b (Glean.Schema.Hackdependency.Types.Inheritance_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hackdependency.Types.Inheritance_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hackdependency.Types.Inheritance_key = 'Angle.TField "parent" (Glean.KeyType Glean.Schema.Hackdependency.Types.Name) ('Angle.TField "child" (Glean.KeyType Glean.Schema.Hackdependency.Types.Name) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hackdependency.Types.Inheritance where
  type KeyType Glean.Schema.Hackdependency.Types.Inheritance =
    Glean.Schema.Hackdependency.Types.Inheritance_key
  getName _proxy  = Glean.PredicateRef "hackdependency.inheritance"1
  getIndex _proxy  = 292
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hackdependency.Types.inheritance_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hackdependency.Types.Inheritance x k
  getFactKey = Glean.Schema.Hackdependency.Types.inheritance_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hackdependency.Types.Inheritance where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hackdependency.Types.Name where
  type KeyType Glean.Schema.Hackdependency.Types.Name = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hackdependency.name"1
  getIndex _proxy  = 48
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hackdependency.Types.name_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hackdependency.Types.Name x k
  getFactKey = Glean.Schema.Hackdependency.Types.name_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hackdependency.Types.Name where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
