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


instance Glean.Type Glean.Schema.Src.Types.ByteSpanContains_key where
  buildRtsValue b (Glean.Schema.Src.Types.ByteSpanContains_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Src.Types.ByteSpanContains_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.ByteSpanContains_key = 'Angle.TField "byteSpan" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TField "contains" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Src.Types.ByteSpanContains where
  type KeyType Glean.Schema.Src.Types.ByteSpanContains =
    Glean.Schema.Src.Types.ByteSpanContains_key
  getName _proxy  = Glean.PredicateRef "src.ByteSpanContains"1
  getIndex _proxy  = 474
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Src.Types.byteSpanContains_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Src.Types.ByteSpanContains x k
  getFactKey = Glean.Schema.Src.Types.byteSpanContains_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Src.Types.ByteSpanContains where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Src.Types.FileLanguage_key where
  buildRtsValue b (Glean.Schema.Src.Types.FileLanguage_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Src.Types.FileLanguage_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.FileLanguage_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "language" (Glean.Schema.Src.Types.Language) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Src.Types.FileLanguage where
  type KeyType Glean.Schema.Src.Types.FileLanguage =
    Glean.Schema.Src.Types.FileLanguage_key
  getName _proxy  = Glean.PredicateRef "src.FileLanguage"1
  getIndex _proxy  = 402
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Src.Types.fileLanguage_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Src.Types.FileLanguage x k
  getFactKey = Glean.Schema.Src.Types.fileLanguage_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Src.Types.FileLanguage where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Src.Types.File where
  type KeyType Glean.Schema.Src.Types.File = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "src.File"1
  getIndex _proxy  = 92
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Src.Types.file_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Src.Types.File x k
  getFactKey = Glean.Schema.Src.Types.file_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Src.Types.File where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Src.Types.FileLines_key where
  buildRtsValue b (Glean.Schema.Src.Types.FileLines_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Src.Types.FileLines_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.FileLines_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "lengths" ([Glean.Nat]) ('Angle.TField "endsInNewline" (Prelude.Bool) ('Angle.TField "hasUnicodeOrTabs" (Prelude.Bool) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Src.Types.FileLines where
  type KeyType Glean.Schema.Src.Types.FileLines =
    Glean.Schema.Src.Types.FileLines_key
  getName _proxy  = Glean.PredicateRef "src.FileLines"1
  getIndex _proxy  = 74
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Src.Types.fileLines_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Src.Types.FileLines x k
  getFactKey = Glean.Schema.Src.Types.fileLines_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Src.Types.FileLines where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Src.Types.RelByteSpan where
  buildRtsValue b (Glean.Schema.Src.Types.RelByteSpan x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Src.Types.RelByteSpan
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.RelByteSpan = 'Angle.TField "offset" (Glean.Nat) ('Angle.TField "length" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Src.Types.ByteRange where
  buildRtsValue b (Glean.Schema.Src.Types.ByteRange x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Src.Types.ByteRange
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.ByteRange = 'Angle.TField "begin" (Glean.Nat) ('Angle.TField "end" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Src.Types.Range where
  buildRtsValue b (Glean.Schema.Src.Types.Range x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Src.Types.Range
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.Range = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "lineBegin" (Glean.Nat) ('Angle.TField "columnBegin" (Glean.Nat) ('Angle.TField "lineEnd" (Glean.Nat) ('Angle.TField "columnEnd" (Glean.Nat) ('Angle.TNoFields)))))

instance Glean.Type Glean.Schema.Src.Types.Loc where
  buildRtsValue b (Glean.Schema.Src.Types.Loc x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Src.Types.Loc
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.Loc = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "line" (Glean.Nat) ('Angle.TField "column" (Glean.Nat) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Src.Types.Language where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Src.Types.Language = 'Angle.TField "Buck" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "C" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Cpp" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Hack" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Haskell" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "ObjC" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "ObjCpp" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Python" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Thrift" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Java" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "GraphQL" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))))))))))

instance Glean.Type Glean.Schema.Src.Types.ByteSpan where
  buildRtsValue b (Glean.Schema.Src.Types.ByteSpan x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Src.Types.ByteSpan
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.ByteSpan = 'Angle.TField "start" (Glean.Nat) ('Angle.TField "length" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Src.Types.FileLocation where
  buildRtsValue b (Glean.Schema.Src.Types.FileLocation x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Src.Types.FileLocation
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Src.Types.FileLocation = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))
