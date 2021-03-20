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
import qualified Glean.Schema.Cxx1.Types


instance Glean.Type Glean.Schema.SearchCxx.Types.EntityUses_key where
  buildRtsValue b (Glean.Schema.SearchCxx.Types.EntityUses_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.SearchCxx.Types.EntityUses_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchCxx.Types.EntityUses_key = 'Angle.TField "entity" (Glean.Schema.CodeCxx.Types.Entity) ('Angle.TField "uses" (Glean.KeyType Glean.Schema.Cxx1.Types.TargetUses) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.SearchCxx.Types.EntityUses where
  type KeyType Glean.Schema.SearchCxx.Types.EntityUses =
    Glean.Schema.SearchCxx.Types.EntityUses_key
  getName _proxy  = Glean.PredicateRef "search.cxx.EntityUses"4
  getIndex _proxy  = 240
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchCxx.Types.entityUses_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchCxx.Types.EntityUses x k
  getFactKey = Glean.Schema.SearchCxx.Types.entityUses_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchCxx.Types.EntityUses where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key where
  buildRtsValue b (Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "scope" (Glean.Schema.Cxx1.Types.Scope) ('Angle.TField "entity" (Glean.Schema.CodeCxx.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.SearchCxx.Types.SearchByNameAndScope where
  type KeyType Glean.Schema.SearchCxx.Types.SearchByNameAndScope =
    Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key
  getName _proxy  = Glean.PredicateRef "search.cxx.SearchByNameAndScope"4
  getIndex _proxy  = 230
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchCxx.Types.searchByNameAndScope_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchCxx.Types.SearchByNameAndScope x k
  getFactKey = Glean.Schema.SearchCxx.Types.searchByNameAndScope_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchCxx.Types.SearchByNameAndScope where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key where
  buildRtsValue b (Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName where
  type KeyType Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName =
    Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key
  getName _proxy  = Glean.PredicateRef "search.cxx.GlobalDeclarationWithName"1
  getIndex _proxy  = 198
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchCxx.Types.globalDeclarationWithName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName x k
  getFactKey = Glean.Schema.SearchCxx.Types.globalDeclarationWithName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchCxx.Types.DeclIsDefn_key where
  buildRtsValue b (Glean.Schema.SearchCxx.Types.DeclIsDefn_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.SearchCxx.Types.DeclIsDefn_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchCxx.Types.DeclIsDefn_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "defn" (Glean.Schema.CodeCxx.Types.Definition) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.SearchCxx.Types.DeclIsDefn where
  type KeyType Glean.Schema.SearchCxx.Types.DeclIsDefn =
    Glean.Schema.SearchCxx.Types.DeclIsDefn_key
  getName _proxy  = Glean.PredicateRef "search.cxx.DeclIsDefn"4
  getIndex _proxy  = 176
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchCxx.Types.declIsDefn_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchCxx.Types.DeclIsDefn x k
  getFactKey = Glean.Schema.SearchCxx.Types.declIsDefn_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchCxx.Types.DeclIsDefn where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchCxx.Types.SearchBySelector_key where
  buildRtsValue b (Glean.Schema.SearchCxx.Types.SearchBySelector_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.SearchCxx.Types.SearchBySelector_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchCxx.Types.SearchBySelector_key = 'Angle.TField "selector" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcSelector) ('Angle.TField "entity" (Glean.Schema.CodeCxx.Types.Entity) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.SearchCxx.Types.SearchBySelector where
  type KeyType Glean.Schema.SearchCxx.Types.SearchBySelector =
    Glean.Schema.SearchCxx.Types.SearchBySelector_key
  getName _proxy  = Glean.PredicateRef "search.cxx.SearchBySelector"4
  getIndex _proxy  = 128
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchCxx.Types.searchBySelector_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchCxx.Types.SearchBySelector x k
  getFactKey = Glean.Schema.SearchCxx.Types.searchBySelector_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchCxx.Types.SearchBySelector where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
