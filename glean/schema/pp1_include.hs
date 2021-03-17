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
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Pp1.Types.Include_key where
  buildRtsValue b (Glean.Schema.Pp1.Types.Include_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Pp1.Types.Include_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Pp1.Types.Include_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "path" (Glean.Schema.Src.Types.ByteRange) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Pp1.Types.Include where
  type KeyType Glean.Schema.Pp1.Types.Include =
    Glean.Schema.Pp1.Types.Include_key
  getName _proxy  = Glean.PredicateRef "pp1.Include"1
  getIndex _proxy  = 426
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Pp1.Types.include_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Pp1.Types.Include x k
  getFactKey = Glean.Schema.Pp1.Types.include_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Pp1.Types.Include where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Pp1.Types.Macro where
  type KeyType Glean.Schema.Pp1.Types.Macro = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "pp1.Macro"1
  getIndex _proxy  = 362
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Pp1.Types.macro_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Pp1.Types.Macro x k
  getFactKey = Glean.Schema.Pp1.Types.macro_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Pp1.Types.Macro where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Pp1.Types.Define_key where
  buildRtsValue b (Glean.Schema.Pp1.Types.Define_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Pp1.Types.Define_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Pp1.Types.Define_key = 'Angle.TField "macro" (Glean.KeyType Glean.Schema.Pp1.Types.Macro) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Pp1.Types.Define where
  type KeyType Glean.Schema.Pp1.Types.Define =
    Glean.Schema.Pp1.Types.Define_key
  getName _proxy  = Glean.PredicateRef "pp1.Define"1
  getIndex _proxy  = 340
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Pp1.Types.define_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Pp1.Types.Define x k
  getFactKey = Glean.Schema.Pp1.Types.define_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Pp1.Types.Define where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Pp1.Types.Use_key where
  buildRtsValue b (Glean.Schema.Pp1.Types.Use_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Pp1.Types.Use_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Pp1.Types.Use_key = 'Angle.TField "macro" (Glean.KeyType Glean.Schema.Pp1.Types.Macro) ('Angle.TField "name" (Glean.Schema.Src.Types.ByteRange) ('Angle.TField "definition" (Prelude.Maybe Glean.Schema.Src.Types.Loc) ('Angle.TField "expand" (Prelude.Bool) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Pp1.Types.Use where
  type KeyType Glean.Schema.Pp1.Types.Use = Glean.Schema.Pp1.Types.Use_key
  getName _proxy  = Glean.PredicateRef "pp1.Use"1
  getIndex _proxy  = 295
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Pp1.Types.use_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Pp1.Types.Use x k
  getFactKey = Glean.Schema.Pp1.Types.use_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Pp1.Types.Use where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Pp1.Types.Undef_key where
  buildRtsValue b (Glean.Schema.Pp1.Types.Undef_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Pp1.Types.Undef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Pp1.Types.Undef_key = 'Angle.TField "macro" (Glean.KeyType Glean.Schema.Pp1.Types.Macro) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Pp1.Types.Undef where
  type KeyType Glean.Schema.Pp1.Types.Undef = Glean.Schema.Pp1.Types.Undef_key
  getName _proxy  = Glean.PredicateRef "pp1.Undef"1
  getIndex _proxy  = 205
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Pp1.Types.undef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Pp1.Types.Undef x k
  getFactKey = Glean.Schema.Pp1.Types.undef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Pp1.Types.Undef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
