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
import qualified Glean.Schema.Hack.Types


instance Glean.Type Glean.Schema.SearchHack.Types.SearchInContext_key where
  buildRtsValue b (Glean.Schema.SearchHack.Types.SearchInContext_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.SearchHack.Types.SearchInContext_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchHack.Types.SearchInContext_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "contextName" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "contextNamespace" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName)) ('Angle.TField "decl" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.SearchHack.Types.SearchInContext where
  type KeyType Glean.Schema.SearchHack.Types.SearchInContext =
    Glean.Schema.SearchHack.Types.SearchInContext_key
  getName _proxy  = Glean.PredicateRef "search.hack.SearchInContext"5
  getIndex _proxy  = 492
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchHack.Types.searchInContext_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchHack.Types.SearchInContext x k
  getFactKey = Glean.Schema.SearchHack.Types.searchInContext_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInContext where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInNamespace_key where
  buildRtsValue b (Glean.Schema.SearchHack.Types.SearchInNamespace_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.SearchHack.Types.SearchInNamespace_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchHack.Types.SearchInNamespace_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName)) ('Angle.TField "decl" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.SearchHack.Types.SearchInNamespace where
  type KeyType Glean.Schema.SearchHack.Types.SearchInNamespace =
    Glean.Schema.SearchHack.Types.SearchInNamespace_key
  getName _proxy  = Glean.PredicateRef "search.hack.SearchInNamespace"5
  getIndex _proxy  = 393
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchHack.Types.searchInNamespace_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchHack.Types.SearchInNamespace x k
  getFactKey = Glean.Schema.SearchHack.Types.searchInNamespace_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInNamespace where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchHack.Types.SearchByName_key where
  buildRtsValue b (Glean.Schema.SearchHack.Types.SearchByName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.SearchHack.Types.SearchByName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchHack.Types.SearchByName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "decl" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.SearchHack.Types.SearchByName where
  type KeyType Glean.Schema.SearchHack.Types.SearchByName =
    Glean.Schema.SearchHack.Types.SearchByName_key
  getName _proxy  = Glean.PredicateRef "search.hack.SearchByName"5
  getIndex _proxy  = 380
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchHack.Types.searchByName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchHack.Types.SearchByName x k
  getFactKey = Glean.Schema.SearchHack.Types.searchByName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchHack.Types.SearchByName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInContainer_key where
  buildRtsValue b (Glean.Schema.SearchHack.Types.SearchInContainer_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.SearchHack.Types.SearchInContainer_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchHack.Types.SearchInContainer_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "containerName" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "containerNamespace" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName)) ('Angle.TField "decl" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.SearchHack.Types.SearchInContainer where
  type KeyType Glean.Schema.SearchHack.Types.SearchInContainer =
    Glean.Schema.SearchHack.Types.SearchInContainer_key
  getName _proxy  = Glean.PredicateRef "search.hack.SearchInContainer"5
  getIndex _proxy  = 254
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchHack.Types.searchInContainer_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchHack.Types.SearchInContainer x k
  getFactKey = Glean.Schema.SearchHack.Types.searchInContainer_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInContainer where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInEnum_key where
  buildRtsValue b (Glean.Schema.SearchHack.Types.SearchInEnum_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.SearchHack.Types.SearchInEnum_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchHack.Types.SearchInEnum_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "enumName" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "enumNamespace" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName)) ('Angle.TField "decl" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.SearchHack.Types.SearchInEnum where
  type KeyType Glean.Schema.SearchHack.Types.SearchInEnum =
    Glean.Schema.SearchHack.Types.SearchInEnum_key
  getName _proxy  = Glean.PredicateRef "search.hack.SearchInEnum"5
  getIndex _proxy  = 139
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchHack.Types.searchInEnum_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchHack.Types.SearchInEnum x k
  getFactKey = Glean.Schema.SearchHack.Types.searchInEnum_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInEnum where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key where
  buildRtsValue b (Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "contextName" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "contextNamespace" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName)) ('Angle.TField "decl" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.SearchHack.Types.SearchInContainerOrEnum where
  type KeyType Glean.Schema.SearchHack.Types.SearchInContainerOrEnum =
    Glean.Schema.SearchHack.Types.SearchInContainerOrEnum_key
  getName _proxy  = Glean.PredicateRef "search.hack.SearchInContainerOrEnum"5
  getIndex _proxy  = 71
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchHack.Types.searchInContainerOrEnum_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchHack.Types.SearchInContainerOrEnum x k
  getFactKey = Glean.Schema.SearchHack.Types.searchInContainerOrEnum_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchHack.Types.SearchInContainerOrEnum where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
