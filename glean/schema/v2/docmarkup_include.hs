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
import qualified Glean.Schema.Code.Types
import qualified Glean.Schema.Hack.Types
import qualified Glean.Schema.Java.Types
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Docmarkup.Types.EntityAnnotations_key where
  buildRtsValue b (Glean.Schema.Docmarkup.Types.EntityAnnotations_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Docmarkup.Types.EntityAnnotations_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Docmarkup.Types.EntityAnnotations_key = 'Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TField "annotations" (Glean.Schema.Docmarkup.Types.GeneralAnnotations) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Docmarkup.Types.EntityAnnotations where
  type KeyType Glean.Schema.Docmarkup.Types.EntityAnnotations =
    Glean.Schema.Docmarkup.Types.EntityAnnotations_key
  getName _proxy  = Glean.PredicateRef "docmarkup.EntityAnnotations"1
  getIndex _proxy  = 498
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Docmarkup.Types.entityAnnotations_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Docmarkup.Types.EntityAnnotations x k
  getFactKey = Glean.Schema.Docmarkup.Types.entityAnnotations_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Docmarkup.Types.EntityAnnotations where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Docmarkup.Types.EntityComments_key where
  buildRtsValue b (Glean.Schema.Docmarkup.Types.EntityComments_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Docmarkup.Types.EntityComments_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Docmarkup.Types.EntityComments_key = 'Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Docmarkup.Types.EntityComments where
  type KeyType Glean.Schema.Docmarkup.Types.EntityComments =
    Glean.Schema.Docmarkup.Types.EntityComments_key
  getName _proxy  = Glean.PredicateRef "docmarkup.EntityComments"1
  getIndex _proxy  = 371
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Docmarkup.Types.entityComments_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Docmarkup.Types.EntityComments x k
  getFactKey = Glean.Schema.Docmarkup.Types.entityComments_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Docmarkup.Types.EntityComments where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Docmarkup.Types.DocAttr_key where
  buildRtsValue b (Glean.Schema.Docmarkup.Types.DocAttr_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Docmarkup.Types.DocAttr_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Docmarkup.Types.DocAttr_key = 'Angle.TField "key" (Glean.KeyType Glean.Schema.Docmarkup.Types.DocAttrKey) ('Angle.TField "value" (Glean.Schema.Docmarkup.Types.DocAttrValue) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Docmarkup.Types.DocAttr where
  type KeyType Glean.Schema.Docmarkup.Types.DocAttr =
    Glean.Schema.Docmarkup.Types.DocAttr_key
  getName _proxy  = Glean.PredicateRef "docmarkup.DocAttr"1
  getIndex _proxy  = 361
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Docmarkup.Types.docAttr_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Docmarkup.Types.DocAttr x k
  getFactKey = Glean.Schema.Docmarkup.Types.docAttr_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Docmarkup.Types.DocAttr where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Docmarkup.Types.DocAttrKey where
  type KeyType Glean.Schema.Docmarkup.Types.DocAttrKey = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "docmarkup.DocAttrKey"1
  getIndex _proxy  = 347
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Docmarkup.Types.docAttrKey_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Docmarkup.Types.DocAttrKey x k
  getFactKey = Glean.Schema.Docmarkup.Types.docAttrKey_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Docmarkup.Types.DocAttrKey where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Docmarkup.Types.EntityDocAttr where
  type KeyType Glean.Schema.Docmarkup.Types.EntityDocAttr =
    Glean.Schema.Code.Types.Entity
  type ValueType Glean.Schema.Docmarkup.Types.EntityDocAttr =
    Glean.Schema.Docmarkup.Types.EntityDocAttr_value
  getName _proxy  = Glean.PredicateRef "docmarkup.EntityDocAttr"1
  getIndex _proxy  = 238
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Docmarkup.Types.entityDocAttr_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Docmarkup.Types.EntityDocAttr x k v
  getFactKey = Glean.Schema.Docmarkup.Types.entityDocAttr_key
  getFactValue = Glean.Schema.Docmarkup.Types.entityDocAttr_value

instance Glean.Type Glean.Schema.Docmarkup.Types.EntityDocAttr where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key where
  buildRtsValue b (Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key = 'Angle.TField "key" (Glean.KeyType Glean.Schema.Docmarkup.Types.DocAttrKey) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Docmarkup.Types.EntityByDocAttrKey where
  type KeyType Glean.Schema.Docmarkup.Types.EntityByDocAttrKey =
    Glean.Schema.Docmarkup.Types.EntityByDocAttrKey_key
  getName _proxy  = Glean.PredicateRef "docmarkup.EntityByDocAttrKey"1
  getIndex _proxy  = 90
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Docmarkup.Types.entityByDocAttrKey_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Docmarkup.Types.EntityByDocAttrKey x k
  getFactKey = Glean.Schema.Docmarkup.Types.entityByDocAttrKey_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Docmarkup.Types.EntityByDocAttrKey where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Docmarkup.Types.GeneralAnnotations where
  buildRtsValue b (Glean.Schema.Docmarkup.Types.GeneralAnnotations_doc x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Docmarkup.Types.GeneralAnnotations_hack x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Docmarkup.Types.GeneralAnnotations_java x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Docmarkup.Types.GeneralAnnotations_doc
    , Glean.mapD Glean.Schema.Docmarkup.Types.GeneralAnnotations_hack
    , Glean.mapD Glean.Schema.Docmarkup.Types.GeneralAnnotations_java
    ]

type instance Angle.SumFields Glean.Schema.Docmarkup.Types.GeneralAnnotations = 'Angle.TField "doc" (Glean.Schema.Docmarkup.Types.DocAttrs) ('Angle.TField "hack" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TField "java" ([Glean.KeyType Glean.Schema.Java.Types.Annotation]) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Docmarkup.Types.DocAttrs Glean.Schema.Docmarkup.Types.GeneralAnnotations where
  injectBranch = Glean.Schema.Docmarkup.Types.GeneralAnnotations_doc
  projectBranch (Glean.Schema.Docmarkup.Types.GeneralAnnotations_doc x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches [Glean.Schema.Hack.Types.UserAttribute] Glean.Schema.Docmarkup.Types.GeneralAnnotations where
  injectBranch = Glean.Schema.Docmarkup.Types.GeneralAnnotations_hack
  projectBranch (Glean.Schema.Docmarkup.Types.GeneralAnnotations_hack x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches [Glean.Schema.Java.Types.Annotation] Glean.Schema.Docmarkup.Types.GeneralAnnotations where
  injectBranch = Glean.Schema.Docmarkup.Types.GeneralAnnotations_java
  projectBranch (Glean.Schema.Docmarkup.Types.GeneralAnnotations_java x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
