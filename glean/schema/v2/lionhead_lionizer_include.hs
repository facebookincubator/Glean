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
import qualified Glean.Schema.Cxx1.Types
import qualified Glean.Schema.Docmarkup.Types


instance Glean.Type Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key where
  buildRtsValue b (Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key = 'Angle.TField "key" (Glean.KeyType Glean.Schema.Docmarkup.Types.DocAttrKey) ('Angle.TField "value" (Glean.Schema.Docmarkup.Types.DocAttrValue) ('Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TField "definition" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDefinition) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef where
  type KeyType Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef =
    Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef_key
  getName _proxy  =
    Glean.PredicateRef "lionhead.lionizer.FindFunctionWithDef"1
  getIndex _proxy  = 445
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.LionheadLionizer.Types.findFunctionWithDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef x k
  getFactKey = Glean.Schema.LionheadLionizer.Types.findFunctionWithDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.LionheadLionizer.Types.FindFunctionWithDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.LionheadLionizer.Types.FindFunction_key where
  buildRtsValue b (Glean.Schema.LionheadLionizer.Types.FindFunction_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.LionheadLionizer.Types.FindFunction_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.LionheadLionizer.Types.FindFunction_key = 'Angle.TField "key" (Glean.KeyType Glean.Schema.Docmarkup.Types.DocAttrKey) ('Angle.TField "value" (Glean.Schema.Docmarkup.Types.DocAttrValue) ('Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.LionheadLionizer.Types.FindFunction where
  type KeyType Glean.Schema.LionheadLionizer.Types.FindFunction =
    Glean.Schema.LionheadLionizer.Types.FindFunction_key
  getName _proxy  = Glean.PredicateRef "lionhead.lionizer.FindFunction"1
  getIndex _proxy  = 190
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.LionheadLionizer.Types.findFunction_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.LionheadLionizer.Types.FindFunction x k
  getFactKey = Glean.Schema.LionheadLionizer.Types.findFunction_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.LionheadLionizer.Types.FindFunction where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
