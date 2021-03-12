-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Buck.Types
import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Cxx1.Types
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Buckuses.Types.UsesOfTarget_key where
  buildRtsValue b (Glean.Schema.Buckuses.Types.UsesOfTarget_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Buckuses.Types.UsesOfTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buckuses.Types.UsesOfTarget_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "use_xref" (Glean.Schema.Cxx1.Types.XRefTarget) ('Angle.TField "use_file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Buckuses.Types.UsesOfTarget where
  type KeyType Glean.Schema.Buckuses.Types.UsesOfTarget =
    Glean.Schema.Buckuses.Types.UsesOfTarget_key
  getName _proxy  = Glean.PredicateRef "buckuses.UsesOfTarget"2
  getIndex _proxy  = 485
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buckuses.Types.usesOfTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buckuses.Types.UsesOfTarget x k
  getFactKey = Glean.Schema.Buckuses.Types.usesOfTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buckuses.Types.UsesOfTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buckuses.Types.UsesOfTarget_1_key where
  buildRtsValue b (Glean.Schema.Buckuses.Types.UsesOfTarget_1_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Buckuses.Types.UsesOfTarget_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buckuses.Types.UsesOfTarget_1_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "use_xref" (Glean.Schema.Cxx1.Types.XRefTarget) ('Angle.TField "use_file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Buckuses.Types.UsesOfTarget_1 where
  type KeyType Glean.Schema.Buckuses.Types.UsesOfTarget_1 =
    Glean.Schema.Buckuses.Types.UsesOfTarget_1_key
  getName _proxy  = Glean.PredicateRef "buckuses.UsesOfTarget"1
  getIndex _proxy  = 484
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buckuses.Types.usesOfTarget_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buckuses.Types.UsesOfTarget_1 x k
  getFactKey = Glean.Schema.Buckuses.Types.usesOfTarget_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buckuses.Types.UsesOfTarget_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key where
  buildRtsValue b (Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "exportedHeader" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buckuses.Types.UsesOfTargetHeader where
  type KeyType Glean.Schema.Buckuses.Types.UsesOfTargetHeader =
    Glean.Schema.Buckuses.Types.UsesOfTargetHeader_key
  getName _proxy  = Glean.PredicateRef "buckuses.UsesOfTargetHeader"2
  getIndex _proxy  = 442
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buckuses.Types.usesOfTargetHeader_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buckuses.Types.UsesOfTargetHeader x k
  getFactKey = Glean.Schema.Buckuses.Types.usesOfTargetHeader_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buckuses.Types.UsesOfTargetHeader where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key where
  buildRtsValue b (Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "exportedHeader" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buckuses.Types.UsesOfTargetHeader1 where
  type KeyType Glean.Schema.Buckuses.Types.UsesOfTargetHeader1 =
    Glean.Schema.Buckuses.Types.UsesOfTargetHeader1_key
  getName _proxy  = Glean.PredicateRef "buckuses.UsesOfTargetHeader1"1
  getIndex _proxy  = 65
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buckuses.Types.usesOfTargetHeader1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buckuses.Types.UsesOfTargetHeader1 x k
  getFactKey = Glean.Schema.Buckuses.Types.usesOfTargetHeader1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buckuses.Types.UsesOfTargetHeader1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
