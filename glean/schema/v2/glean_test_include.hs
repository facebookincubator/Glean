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
import qualified Glean.Schema.CodeCxx.Types
import qualified Glean.Schema.Pp1.Types
import qualified Glean.Schema.Sys.Types


instance Glean.Type Glean.Schema.GleanTest.Types.RevStringPair_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.RevStringPair_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.RevStringPair_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.RevStringPair_key = 'Angle.TField "fst" (Data.Text.Text) ('Angle.TField "snd" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.RevStringPair where
  type KeyType Glean.Schema.GleanTest.Types.RevStringPair =
    Glean.Schema.GleanTest.Types.RevStringPair_key
  getName _proxy  = Glean.PredicateRef "glean.test.RevStringPair"1
  getIndex _proxy  = 497
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.revStringPair_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.RevStringPair x k
  getFactKey = Glean.Schema.GleanTest.Types.revStringPair_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.RevStringPair where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.LeftOr_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.LeftOr_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.LeftOr_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.LeftOr_key = 'Angle.TField "x" (Data.Text.Text) ('Angle.TField "y" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.LeftOr where
  type KeyType Glean.Schema.GleanTest.Types.LeftOr =
    Glean.Schema.GleanTest.Types.LeftOr_key
  getName _proxy  = Glean.PredicateRef "glean.test.LeftOr"1
  getIndex _proxy  = 483
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.leftOr_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.LeftOr x k
  getFactKey = Glean.Schema.GleanTest.Types.leftOr_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.LeftOr where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.Tree_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Tree_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.GleanTest.Types.Tree_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.Tree_key = 'Angle.TField "node" (Data.Text.Text) ('Angle.TField "left" (Prelude.Maybe (Glean.KeyType Glean.Schema.GleanTest.Types.Tree)) ('Angle.TField "right" (Prelude.Maybe (Glean.KeyType Glean.Schema.GleanTest.Types.Tree)) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.GleanTest.Types.Tree where
  type KeyType Glean.Schema.GleanTest.Types.Tree =
    Glean.Schema.GleanTest.Types.Tree_key
  getName _proxy  = Glean.PredicateRef "glean.test.Tree"4
  getIndex _proxy  = 478
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.tree_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Tree x k
  getFactKey = Glean.Schema.GleanTest.Types.tree_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Tree where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key = 'Angle.TField "fst" (Data.Text.Text) ('Angle.TField "snd" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.StoredRevStringPairWithA where
  type KeyType Glean.Schema.GleanTest.Types.StoredRevStringPairWithA =
    Glean.Schema.GleanTest.Types.StoredRevStringPairWithA_key
  getName _proxy  = Glean.PredicateRef "glean.test.StoredRevStringPairWithA"1
  getIndex _proxy  = 477
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.storedRevStringPairWithA_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.StoredRevStringPairWithA x k
  getFactKey = Glean.Schema.GleanTest.Types.storedRevStringPairWithA_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.StoredRevStringPairWithA where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.Ref where
  type KeyType Glean.Schema.GleanTest.Types.Ref =
    Glean.Schema.GleanTest.Types.Predicate
  getName _proxy  = Glean.PredicateRef "glean.test.Ref"4
  getIndex _proxy  = 468
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.ref_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Ref x k
  getFactKey = Glean.Schema.GleanTest.Types.ref_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Ref where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.Bar where
  type KeyType Glean.Schema.GleanTest.Types.Bar = Data.Text.Text
  type ValueType Glean.Schema.GleanTest.Types.Bar =
    Glean.Schema.GleanTest.Types.Bar_value
  getName _proxy  = Glean.PredicateRef "glean.test.Bar"4
  getIndex _proxy  = 465
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.bar_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.GleanTest.Types.Bar x k v
  getFactKey = Glean.Schema.GleanTest.Types.bar_key
  getFactValue = Glean.Schema.GleanTest.Types.bar_value

instance Glean.Type Glean.Schema.GleanTest.Types.Bar where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.DerivedKeyValue_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.DerivedKeyValue_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.GleanTest.Types.DerivedKeyValue_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.DerivedKeyValue_key = 'Angle.TField "kstring" (Data.Text.Text) ('Angle.TField "knat" (Glean.Nat) ('Angle.TField "vnat" (Glean.Nat) ('Angle.TField "vstring" (Data.Text.Text) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.GleanTest.Types.DerivedKeyValue where
  type KeyType Glean.Schema.GleanTest.Types.DerivedKeyValue =
    Glean.Schema.GleanTest.Types.DerivedKeyValue_key
  getName _proxy  = Glean.PredicateRef "glean.test.DerivedKeyValue"1
  getIndex _proxy  = 457
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.derivedKeyValue_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.DerivedKeyValue x k
  getFactKey = Glean.Schema.GleanTest.Types.derivedKeyValue_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.DerivedKeyValue where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.ViaStringPair_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.ViaStringPair_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.ViaStringPair_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.ViaStringPair_key = 'Angle.TField "fst" (Data.Text.Text) ('Angle.TField "snd" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.ViaStringPair where
  type KeyType Glean.Schema.GleanTest.Types.ViaStringPair =
    Glean.Schema.GleanTest.Types.ViaStringPair_key
  getName _proxy  = Glean.PredicateRef "glean.test.ViaStringPair"1
  getIndex _proxy  = 443
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.viaStringPair_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.ViaStringPair x k
  getFactKey = Glean.Schema.GleanTest.Types.viaStringPair_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.ViaStringPair where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.LeftOr2_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.LeftOr2_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.LeftOr2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.LeftOr2_key = 'Angle.TField "x" (Data.Text.Text) ('Angle.TField "y" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.LeftOr2 where
  type KeyType Glean.Schema.GleanTest.Types.LeftOr2 =
    Glean.Schema.GleanTest.Types.LeftOr2_key
  getName _proxy  = Glean.PredicateRef "glean.test.LeftOr2"1
  getIndex _proxy  = 433
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.leftOr2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.LeftOr2 x k
  getFactKey = Glean.Schema.GleanTest.Types.leftOr2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.LeftOr2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.StringPair_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.StringPair_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.StringPair_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.StringPair_key = 'Angle.TField "fst" (Data.Text.Text) ('Angle.TField "snd" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.StringPair where
  type KeyType Glean.Schema.GleanTest.Types.StringPair =
    Glean.Schema.GleanTest.Types.StringPair_key
  getName _proxy  = Glean.PredicateRef "glean.test.StringPair"1
  getIndex _proxy  = 418
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.stringPair_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.StringPair x k
  getFactKey = Glean.Schema.GleanTest.Types.stringPair_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.StringPair where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.Name where
  type KeyType Glean.Schema.GleanTest.Types.Name = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "glean.test.Name"1
  getIndex _proxy  = 414
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.name_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Name x k
  getFactKey = Glean.Schema.GleanTest.Types.name_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Name where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.StringPairBox_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.StringPairBox_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.GleanTest.Types.StringPairBox_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.StringPairBox_key = 'Angle.TField "box" (Glean.KeyType Glean.Schema.GleanTest.Types.StringPair) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.GleanTest.Types.StringPairBox where
  type KeyType Glean.Schema.GleanTest.Types.StringPairBox =
    Glean.Schema.GleanTest.Types.StringPairBox_key
  getName _proxy  = Glean.PredicateRef "glean.test.StringPairBox"1
  getIndex _proxy  = 387
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.stringPairBox_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.StringPairBox x k
  getFactKey = Glean.Schema.GleanTest.Types.stringPairBox_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.StringPairBox where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.ReflStringPair where
  type KeyType Glean.Schema.GleanTest.Types.ReflStringPair = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "glean.test.ReflStringPair"1
  getIndex _proxy  = 367
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.reflStringPair_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.ReflStringPair x k
  getFactKey = Glean.Schema.GleanTest.Types.reflStringPair_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.ReflStringPair where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.StoredRevStringPair_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.StoredRevStringPair_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.StoredRevStringPair_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.StoredRevStringPair_key = 'Angle.TField "fst" (Data.Text.Text) ('Angle.TField "snd" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.StoredRevStringPair where
  type KeyType Glean.Schema.GleanTest.Types.StoredRevStringPair =
    Glean.Schema.GleanTest.Types.StoredRevStringPair_key
  getName _proxy  = Glean.PredicateRef "glean.test.StoredRevStringPair"1
  getIndex _proxy  = 340
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.storedRevStringPair_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.StoredRevStringPair x k
  getFactKey = Glean.Schema.GleanTest.Types.storedRevStringPair_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.StoredRevStringPair where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.RevStringPairs_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.RevStringPairs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.RevStringPairs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.RevStringPairs_key = 'Angle.TField "x" (Data.Text.Text) ('Angle.TField "r" (Glean.KeyType Glean.Schema.GleanTest.Types.RevStringPair) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.RevStringPairs where
  type KeyType Glean.Schema.GleanTest.Types.RevStringPairs =
    Glean.Schema.GleanTest.Types.RevStringPairs_key
  getName _proxy  = Glean.PredicateRef "glean.test.RevStringPairs"1
  getIndex _proxy  = 339
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.revStringPairs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.RevStringPairs x k
  getFactKey = Glean.Schema.GleanTest.Types.revStringPairs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.RevStringPairs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.RefRef where
  type KeyType Glean.Schema.GleanTest.Types.RefRef =
    Glean.Schema.GleanTest.Types.Ref
  getName _proxy  = Glean.PredicateRef "glean.test.RefRef"4
  getIndex _proxy  = 315
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.refRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.RefRef x k
  getFactKey = Glean.Schema.GleanTest.Types.refRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.RefRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.DerivedKeyValue2_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.DerivedKeyValue2_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.DerivedKeyValue2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.DerivedKeyValue2_key = 'Angle.TField "kstring" (Data.Text.Text) ('Angle.TField "knat" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.GleanTest.Types.DerivedKeyValue2_value where
  buildRtsValue b (Glean.Schema.GleanTest.Types.DerivedKeyValue2_value x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.DerivedKeyValue2_value
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.DerivedKeyValue2_value = 'Angle.TField "vnat" (Glean.Nat) ('Angle.TField "vstring" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.DerivedKeyValue2 where
  type KeyType Glean.Schema.GleanTest.Types.DerivedKeyValue2 =
    Glean.Schema.GleanTest.Types.DerivedKeyValue2_key
  type ValueType Glean.Schema.GleanTest.Types.DerivedKeyValue2 =
    Glean.Schema.GleanTest.Types.DerivedKeyValue2_value
  getName _proxy  = Glean.PredicateRef "glean.test.DerivedKeyValue2"1
  getIndex _proxy  = 291
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.derivedKeyValue2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.GleanTest.Types.DerivedKeyValue2 x k v
  getFactKey = Glean.Schema.GleanTest.Types.derivedKeyValue2_key
  getFactValue = Glean.Schema.GleanTest.Types.derivedKeyValue2_value

instance Glean.Type Glean.Schema.GleanTest.Types.DerivedKeyValue2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.NothingTest_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.NothingTest_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.NothingTest_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.NothingTest_key = 'Angle.TField "a" (Prelude.Maybe Data.Text.Text) ('Angle.TField "b" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.NothingTest where
  type KeyType Glean.Schema.GleanTest.Types.NothingTest =
    Glean.Schema.GleanTest.Types.NothingTest_key
  getName _proxy  = Glean.PredicateRef "glean.test.nothingTest"4
  getIndex _proxy  = 287
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.nothingTest_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.NothingTest x k
  getFactKey = Glean.Schema.GleanTest.Types.nothingTest_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.NothingTest where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.FooToFoo where
  type KeyType Glean.Schema.GleanTest.Types.FooToFoo =
    Glean.Schema.GleanTest.Types.Foo
  type ValueType Glean.Schema.GleanTest.Types.FooToFoo =
    Glean.Schema.GleanTest.Types.FooToFoo_value
  getName _proxy  = Glean.PredicateRef "glean.test.FooToFoo"4
  getIndex _proxy  = 283
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.fooToFoo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.GleanTest.Types.FooToFoo x k v
  getFactKey = Glean.Schema.GleanTest.Types.fooToFoo_key
  getFactValue = Glean.Schema.GleanTest.Types.fooToFoo_value

instance Glean.Type Glean.Schema.GleanTest.Types.FooToFoo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.Edge_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Edge_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.Edge_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.Edge_key = 'Angle.TField "parent" (Glean.KeyType Glean.Schema.GleanTest.Types.Node) ('Angle.TField "child" (Glean.KeyType Glean.Schema.GleanTest.Types.Node) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.Edge where
  type KeyType Glean.Schema.GleanTest.Types.Edge =
    Glean.Schema.GleanTest.Types.Edge_key
  getName _proxy  = Glean.PredicateRef "glean.test.Edge"4
  getIndex _proxy  = 281
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.edge_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Edge x k
  getFactKey = Glean.Schema.GleanTest.Types.edge_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Edge where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.IsGlean where
  type KeyType Glean.Schema.GleanTest.Types.IsGlean = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "glean.test.IsGlean"1
  getIndex _proxy  = 280
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.isGlean_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.IsGlean x k
  getFactKey = Glean.Schema.GleanTest.Types.isGlean_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.IsGlean where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.Foo where
  type KeyType Glean.Schema.GleanTest.Types.Foo = Data.Text.Text
  type ValueType Glean.Schema.GleanTest.Types.Foo =
    Glean.Schema.GleanTest.Types.Foo_value
  getName _proxy  = Glean.PredicateRef "glean.test.Foo"4
  getIndex _proxy  = 257
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.foo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.GleanTest.Types.Foo x k v
  getFactKey = Glean.Schema.GleanTest.Types.foo_key
  getFactValue = Glean.Schema.GleanTest.Types.foo_value

instance Glean.Type Glean.Schema.GleanTest.Types.Foo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.MatchOneAlt_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.MatchOneAlt_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.MatchOneAlt_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.MatchOneAlt_key = 'Angle.TField "x" (Glean.Schema.GleanTest.Types.Sum) ('Angle.TField "y" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.MatchOneAlt where
  type KeyType Glean.Schema.GleanTest.Types.MatchOneAlt =
    Glean.Schema.GleanTest.Types.MatchOneAlt_key
  getName _proxy  = Glean.PredicateRef "glean.test.MatchOneAlt"1
  getIndex _proxy  = 251
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.matchOneAlt_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.MatchOneAlt x k
  getFactKey = Glean.Schema.GleanTest.Types.matchOneAlt_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.MatchOneAlt where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.Predicate where
  type KeyType Glean.Schema.GleanTest.Types.Predicate =
    Glean.Schema.GleanTest.Types.KitchenSink
  getName _proxy  = Glean.PredicateRef "glean.test.Predicate"4
  getIndex _proxy  = 234
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.predicate_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Predicate x k
  getFactKey = Glean.Schema.GleanTest.Types.predicate_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Predicate where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.Predicate_1 where
  type KeyType Glean.Schema.GleanTest.Types.Predicate_1 =
    Glean.Schema.GleanTest.Types.KitchenSink_1
  getName _proxy  = Glean.PredicateRef "glean.test.Predicate"1
  getIndex _proxy  = 233
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.predicate_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Predicate_1 x k
  getFactKey = Glean.Schema.GleanTest.Types.predicate_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Predicate_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.Unbound_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Unbound_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.Unbound_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.Unbound_key = 'Angle.TField "x" (Data.Text.Text) ('Angle.TField "y" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.Unbound where
  type KeyType Glean.Schema.GleanTest.Types.Unbound =
    Glean.Schema.GleanTest.Types.Unbound_key
  getName _proxy  = Glean.PredicateRef "glean.test.Unbound"1
  getIndex _proxy  = 217
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.unbound_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Unbound x k
  getFactKey = Glean.Schema.GleanTest.Types.unbound_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Unbound where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.IsThree where
  type KeyType Glean.Schema.GleanTest.Types.IsThree = Glean.Nat
  getName _proxy  = Glean.PredicateRef "glean.test.IsThree"1
  getIndex _proxy  = 205
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.isThree_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.IsThree x k
  getFactKey = Glean.Schema.GleanTest.Types.isThree_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.IsThree where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.Qux where
  type KeyType Glean.Schema.GleanTest.Types.Qux = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "glean.test.Qux"4
  getIndex _proxy  = 201
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.qux_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Qux x k
  getFactKey = Glean.Schema.GleanTest.Types.qux_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Qux where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.KeyValue_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.KeyValue_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.KeyValue_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.KeyValue_key = 'Angle.TField "kstring" (Data.Text.Text) ('Angle.TField "knat" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.GleanTest.Types.KeyValue_value where
  buildRtsValue b (Glean.Schema.GleanTest.Types.KeyValue_value x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.KeyValue_value
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.KeyValue_value = 'Angle.TField "vnat" (Glean.Nat) ('Angle.TField "vstring" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.KeyValue where
  type KeyType Glean.Schema.GleanTest.Types.KeyValue =
    Glean.Schema.GleanTest.Types.KeyValue_key
  type ValueType Glean.Schema.GleanTest.Types.KeyValue =
    Glean.Schema.GleanTest.Types.KeyValue_value
  getName _proxy  = Glean.PredicateRef "glean.test.KeyValue"1
  getIndex _proxy  = 197
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.keyValue_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.GleanTest.Types.KeyValue x k v
  getFactKey = Glean.Schema.GleanTest.Types.keyValue_key
  getFactValue = Glean.Schema.GleanTest.Types.keyValue_value

instance Glean.Type Glean.Schema.GleanTest.Types.KeyValue where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.SameString_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.SameString_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.SameString_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.SameString_key = 'Angle.TField "x" (Data.Text.Text) ('Angle.TField "y" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.SameString where
  type KeyType Glean.Schema.GleanTest.Types.SameString =
    Glean.Schema.GleanTest.Types.SameString_key
  getName _proxy  = Glean.PredicateRef "glean.test.SameString"1
  getIndex _proxy  = 177
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.sameString_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.SameString x k
  getFactKey = Glean.Schema.GleanTest.Types.sameString_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.SameString where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.Expr_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Expr_key_var_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.Expr_key_lit x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.Expr_key_prim x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.Expr_key_ap x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.Expr_key_lam x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.GleanTest.Types.Expr_key_var_
    , Glean.mapD Glean.Schema.GleanTest.Types.Expr_key_lit
    , Glean.mapD Glean.Schema.GleanTest.Types.Expr_key_prim
    , Glean.mapD Glean.Schema.GleanTest.Types.Expr_key_ap
    , Glean.mapD Glean.Schema.GleanTest.Types.Expr_key_lam
    ]

type instance Angle.SumFields Glean.Schema.GleanTest.Types.Expr_key = 'Angle.TField "var_" (Glean.KeyType Glean.Schema.GleanTest.Types.Name) ('Angle.TField "lit" (Glean.Nat) ('Angle.TField "prim" (Glean.KeyType Glean.Schema.GleanTest.Types.Name) ('Angle.TField "ap" (Glean.Schema.GleanTest.Types.Expr_ap_) ('Angle.TField "lam" (Glean.Schema.GleanTest.Types.Expr_lam_) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.GleanTest.Types.Expr where
  type KeyType Glean.Schema.GleanTest.Types.Expr =
    Glean.Schema.GleanTest.Types.Expr_key
  getName _proxy  = Glean.PredicateRef "glean.test.Expr"1
  getIndex _proxy  = 174
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.expr_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Expr x k
  getFactKey = Glean.Schema.GleanTest.Types.expr_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Expr where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.RevRevStringPair_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.RevRevStringPair_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.RevRevStringPair_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.RevRevStringPair_key = 'Angle.TField "fst" (Data.Text.Text) ('Angle.TField "snd" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.RevRevStringPair where
  type KeyType Glean.Schema.GleanTest.Types.RevRevStringPair =
    Glean.Schema.GleanTest.Types.RevRevStringPair_key
  getName _proxy  = Glean.PredicateRef "glean.test.RevRevStringPair"1
  getIndex _proxy  = 171
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.revRevStringPair_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.RevRevStringPair x k
  getFactKey = Glean.Schema.GleanTest.Types.revRevStringPair_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.RevRevStringPair where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.RevStringPairRec_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.RevStringPairRec_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.RevStringPairRec_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.RevStringPairRec_key = 'Angle.TField "fst" (Data.Text.Text) ('Angle.TField "snd" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.RevStringPairRec where
  type KeyType Glean.Schema.GleanTest.Types.RevStringPairRec =
    Glean.Schema.GleanTest.Types.RevStringPairRec_key
  getName _proxy  = Glean.PredicateRef "glean.test.RevStringPairRec"1
  getIndex _proxy  = 124
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.revStringPairRec_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.RevStringPairRec x k
  getFactKey = Glean.Schema.GleanTest.Types.revStringPairRec_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.RevStringPairRec where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.Node_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Node_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.GleanTest.Types.Node_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.Node_key = 'Angle.TField "label" (Data.Text.Text) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.GleanTest.Types.Node where
  type KeyType Glean.Schema.GleanTest.Types.Node =
    Glean.Schema.GleanTest.Types.Node_key
  getName _proxy  = Glean.PredicateRef "glean.test.Node"4
  getIndex _proxy  = 64
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.node_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Node x k
  getFactKey = Glean.Schema.GleanTest.Types.node_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Node where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.Unbound2_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Unbound2_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.Unbound2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.Unbound2_key = 'Angle.TField "x" (Data.Text.Text) ('Angle.TField "y" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.Unbound2 where
  type KeyType Glean.Schema.GleanTest.Types.Unbound2 =
    Glean.Schema.GleanTest.Types.Unbound2_key
  getName _proxy  = Glean.PredicateRef "glean.test.Unbound2"1
  getIndex _proxy  = 40
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.unbound2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.Unbound2 x k
  getFactKey = Glean.Schema.GleanTest.Types.unbound2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.Unbound2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.GleanTest.Types.TreeToTree where
  type KeyType Glean.Schema.GleanTest.Types.TreeToTree =
    Glean.Schema.GleanTest.Types.Tree
  type ValueType Glean.Schema.GleanTest.Types.TreeToTree =
    Glean.Schema.GleanTest.Types.TreeToTree_value
  getName _proxy  = Glean.PredicateRef "glean.test.TreeToTree"4
  getIndex _proxy  = 14
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.treeToTree_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.GleanTest.Types.TreeToTree x k v
  getFactKey = Glean.Schema.GleanTest.Types.treeToTree_key
  getFactValue = Glean.Schema.GleanTest.Types.treeToTree_value

instance Glean.Type Glean.Schema.GleanTest.Types.TreeToTree where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.DualStringPair_key where
  buildRtsValue b (Glean.Schema.GleanTest.Types.DualStringPair_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.DualStringPair_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.DualStringPair_key = 'Angle.TField "fst" (Glean.KeyType Glean.Schema.GleanTest.Types.StringPair) ('Angle.TField "snd" (Glean.KeyType Glean.Schema.GleanTest.Types.StringPair) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.GleanTest.Types.DualStringPair where
  type KeyType Glean.Schema.GleanTest.Types.DualStringPair =
    Glean.Schema.GleanTest.Types.DualStringPair_key
  getName _proxy  = Glean.PredicateRef "glean.test.DualStringPair"1
  getIndex _proxy  = 4
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.GleanTest.Types.dualStringPair_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.GleanTest.Types.DualStringPair x k
  getFactKey = Glean.Schema.GleanTest.Types.dualStringPair_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.GleanTest.Types.DualStringPair where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.GleanTest.Types.KitchenSink where
  buildRtsValue b (Glean.Schema.GleanTest.Types.KitchenSink x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
    Glean.buildRtsValue b x9
    Glean.buildRtsValue b x10
    Glean.buildRtsValue b x11
    Glean.buildRtsValue b x12
    Glean.buildRtsValue b x13
    Glean.buildRtsValue b x14
    Glean.buildRtsValue b x15
    Glean.buildRtsValue b x16
    Glean.buildRtsValue b x17
    Glean.buildRtsValue b x18
    Glean.buildRtsValue b x19
    Glean.buildRtsValue b x20
    Glean.buildRtsValue b x21
    Glean.buildRtsValue b x22
    Glean.buildRtsValue b x23
    Glean.buildRtsValue b x24
  decodeRtsValue = Glean.Schema.GleanTest.Types.KitchenSink
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.KitchenSink = 'Angle.TField "byt" (Glean.Byte) ('Angle.TField "nat" (Glean.Nat) ('Angle.TField "bool_" (Prelude.Bool) ('Angle.TField "string_" (Data.Text.Text) ('Angle.TField "pred" (Glean.KeyType Glean.Schema.Sys.Types.Blob) ('Angle.TField "maybe_" (Prelude.Maybe Glean.Schema.Builtin.Types.Unit) ('Angle.TField "record_" (Glean.Schema.GleanTest.Types.KitchenSink_record_) ('Angle.TField "sum_" (Glean.Schema.GleanTest.Types.KitchenSink_sum_) ('Angle.TField "enum_" (Glean.Schema.GleanTest.Types.KitchenSink_enum_) ('Angle.TField "named_record_" (Glean.Schema.GleanTest.Types.Rec) ('Angle.TField "named_sum_" (Glean.Schema.GleanTest.Types.Sum) ('Angle.TField "named_enum_" (Glean.Schema.GleanTest.Types.Enum) ('Angle.TField "array_of_byte" (Data.ByteString.ByteString) ('Angle.TField "array_of_nat" ([Glean.Nat]) ('Angle.TField "array_of_bool" ([Prelude.Bool]) ('Angle.TField "array_of_string" ([Data.Text.Text]) ('Angle.TField "array_of_pred" ([Glean.KeyType Glean.Schema.GleanTest.Types.Predicate]) ('Angle.TField "array_of_named_record" ([Glean.Schema.GleanTest.Types.Rec]) ('Angle.TField "array_of_named_sum" ([Glean.Schema.GleanTest.Types.Sum]) ('Angle.TField "array_of_named_enum" ([Glean.Schema.GleanTest.Types.Enum]) ('Angle.TField "array2_of_byte" ([Glean.Schema.GleanTest.Types.ArrayByte]) ('Angle.TField "array2_of_nat" ([Glean.Schema.GleanTest.Types.ArrayNat]) ('Angle.TField "array2_of_bool" ([Glean.Schema.GleanTest.Types.ArrayBool]) ('Angle.TField "array2_of_string" ([Glean.Schema.GleanTest.Types.ArrayString]) ('Angle.TNoFields))))))))))))))))))))))))

instance Glean.Type Glean.Schema.GleanTest.Types.KitchenSink_1 where
  buildRtsValue b (Glean.Schema.GleanTest.Types.KitchenSink_1 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
    Glean.buildRtsValue b x9
    Glean.buildRtsValue b x10
    Glean.buildRtsValue b x11
    Glean.buildRtsValue b x12
    Glean.buildRtsValue b x13
  decodeRtsValue = Glean.Schema.GleanTest.Types.KitchenSink_1
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.KitchenSink_1 = 'Angle.TField "byt" (Glean.Byte) ('Angle.TField "nat" (Glean.Nat) ('Angle.TField "array_of_byte" (Data.ByteString.ByteString) ('Angle.TField "array_of_nat" ([Glean.Nat]) ('Angle.TField "record_" (Glean.Schema.GleanTest.Types.KitchenSink_1_record_) ('Angle.TField "sum_" (Glean.Schema.GleanTest.Types.KitchenSink_1_sum_) ('Angle.TField "named_record_" (Glean.Schema.GleanTest.Types.Rec) ('Angle.TField "named_sum_" (Glean.Schema.GleanTest.Types.Sum) ('Angle.TField "named_enum_" (Glean.Schema.GleanTest.Types.Enum) ('Angle.TField "pred" (Glean.KeyType Glean.Schema.Sys.Types.Blob) ('Angle.TField "maybe_" (Prelude.Maybe Glean.Schema.Builtin.Types.Unit) ('Angle.TField "bool_" (Prelude.Bool) ('Angle.TField "string_" (Data.Text.Text) ('Angle.TNoFields)))))))))))))

instance Glean.Type Glean.Schema.GleanTest.Types.Rec where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Rec x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.Rec
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.Rec = 'Angle.TField "alpha" (Glean.Schema.GleanTest.Types.Enum) ('Angle.TField "beta" (Glean.Schema.GleanTest.Types.Sum) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.GleanTest.Types.Enum where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.GleanTest.Types.Enum = 'Angle.TField "red" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "green" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "blue" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.GleanTest.Types.Entity where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Entity_cxx x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.Entity_pp x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.GleanTest.Types.Entity_cxx
    , Glean.mapD Glean.Schema.GleanTest.Types.Entity_pp
    ]

type instance Angle.SumFields Glean.Schema.GleanTest.Types.Entity = 'Angle.TField "cxx" (Glean.Schema.CodeCxx.Types.Entity) ('Angle.TField "pp" (Glean.KeyType Glean.Schema.Pp1.Types.Define) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.CodeCxx.Types.Entity Glean.Schema.GleanTest.Types.Entity where
  injectBranch = Glean.Schema.GleanTest.Types.Entity_cxx
  projectBranch (Glean.Schema.GleanTest.Types.Entity_cxx x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Pp1.Types.Define Glean.Schema.GleanTest.Types.Entity where
  injectBranch = Glean.Schema.GleanTest.Types.Entity_pp
  projectBranch (Glean.Schema.GleanTest.Types.Entity_pp x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.GleanTest.Types.WrappedStringPair where
  buildRtsValue b (Glean.Schema.GleanTest.Types.WrappedStringPair x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.GleanTest.Types.WrappedStringPair
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.WrappedStringPair = 'Angle.TField "wrapped" (Glean.KeyType Glean.Schema.GleanTest.Types.StringPair) ('Angle.TNoFields)

instance Glean.Type Glean.Schema.GleanTest.Types.Sum where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Sum_mon x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.Sum_tue x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.Sum_wed x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.GleanTest.Types.Sum_mon
    , Glean.mapD Glean.Schema.GleanTest.Types.Sum_tue
    , Glean.mapD Glean.Schema.GleanTest.Types.Sum_wed
    ]

type instance Angle.SumFields Glean.Schema.GleanTest.Types.Sum = 'Angle.TField "mon" (Glean.Byte) ('Angle.TField "tue" (Glean.Nat) ('Angle.TField "wed" (Prelude.Bool) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Byte Glean.Schema.GleanTest.Types.Sum where
  injectBranch = Glean.Schema.GleanTest.Types.Sum_mon
  projectBranch (Glean.Schema.GleanTest.Types.Sum_mon x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Nat Glean.Schema.GleanTest.Types.Sum where
  injectBranch = Glean.Schema.GleanTest.Types.Sum_tue
  projectBranch (Glean.Schema.GleanTest.Types.Sum_tue x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Prelude.Bool Glean.Schema.GleanTest.Types.Sum where
  injectBranch = Glean.Schema.GleanTest.Types.Sum_wed
  projectBranch (Glean.Schema.GleanTest.Types.Sum_wed x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.GleanTest.Types.Expr_ap_ where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Expr_ap_ x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.Expr_ap_
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.Expr_ap_ = 'Angle.TField "fun" (Glean.KeyType Glean.Schema.GleanTest.Types.Expr) ('Angle.TField "arg" (Glean.KeyType Glean.Schema.GleanTest.Types.Expr) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.GleanTest.Types.Expr_lam_ where
  buildRtsValue b (Glean.Schema.GleanTest.Types.Expr_lam_ x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.Expr_lam_
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.Expr_lam_ = 'Angle.TField "var_" (Glean.KeyType Glean.Schema.GleanTest.Types.Name) ('Angle.TField "body" (Glean.KeyType Glean.Schema.GleanTest.Types.Expr) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.GleanTest.Types.KitchenSink_record_ where
  buildRtsValue b (Glean.Schema.GleanTest.Types.KitchenSink_record_ x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.KitchenSink_record_
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.KitchenSink_record_ = 'Angle.TField "a" (Glean.Byte) ('Angle.TField "b" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.GleanTest.Types.KitchenSink_sum_ where
  buildRtsValue b (Glean.Schema.GleanTest.Types.KitchenSink_sum__c x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.KitchenSink_sum__d x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.GleanTest.Types.KitchenSink_sum__c
    , Glean.mapD Glean.Schema.GleanTest.Types.KitchenSink_sum__d
    ]

type instance Angle.SumFields Glean.Schema.GleanTest.Types.KitchenSink_sum_ = 'Angle.TField "c" (Glean.KeyType Glean.Schema.GleanTest.Types.Predicate) ('Angle.TField "d" (Glean.KeyType Glean.Schema.Sys.Types.Blob) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.GleanTest.Types.Predicate Glean.Schema.GleanTest.Types.KitchenSink_sum_ where
  injectBranch = Glean.Schema.GleanTest.Types.KitchenSink_sum__c
  projectBranch (Glean.Schema.GleanTest.Types.KitchenSink_sum__c x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Sys.Types.Blob Glean.Schema.GleanTest.Types.KitchenSink_sum_ where
  injectBranch = Glean.Schema.GleanTest.Types.KitchenSink_sum__d
  projectBranch (Glean.Schema.GleanTest.Types.KitchenSink_sum__d x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.GleanTest.Types.KitchenSink_enum_ where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.GleanTest.Types.KitchenSink_enum_ = 'Angle.TField "e" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "f" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "g" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.GleanTest.Types.KitchenSink_1_record_ where
  buildRtsValue b (Glean.Schema.GleanTest.Types.KitchenSink_1_record_ x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.GleanTest.Types.KitchenSink_1_record_
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.GleanTest.Types.KitchenSink_1_record_ = 'Angle.TField "a" (Glean.Byte) ('Angle.TField "b" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.GleanTest.Types.KitchenSink_1_sum_ where
  buildRtsValue b (Glean.Schema.GleanTest.Types.KitchenSink_1_sum__c x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.GleanTest.Types.KitchenSink_1_sum__d x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.GleanTest.Types.KitchenSink_1_sum__c
    , Glean.mapD Glean.Schema.GleanTest.Types.KitchenSink_1_sum__d
    ]

type instance Angle.SumFields Glean.Schema.GleanTest.Types.KitchenSink_1_sum_ = 'Angle.TField "c" (Glean.Byte) ('Angle.TField "d" (Glean.Nat) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Byte Glean.Schema.GleanTest.Types.KitchenSink_1_sum_ where
  injectBranch = Glean.Schema.GleanTest.Types.KitchenSink_1_sum__c
  projectBranch (Glean.Schema.GleanTest.Types.KitchenSink_1_sum__c x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Nat Glean.Schema.GleanTest.Types.KitchenSink_1_sum_ where
  injectBranch = Glean.Schema.GleanTest.Types.KitchenSink_1_sum__d
  projectBranch (Glean.Schema.GleanTest.Types.KitchenSink_1_sum__d x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
