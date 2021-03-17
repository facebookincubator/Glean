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


instance Glean.Type Glean.Schema.Hack.Types.ClassDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.ClassDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Hack.Types.ClassDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.ClassDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.QName) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Hack.Types.ClassDeclaration where
  type KeyType Glean.Schema.Hack.Types.ClassDeclaration =
    Glean.Schema.Hack.Types.ClassDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.ClassDeclaration"4
  getIndex _proxy  = 488
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.classDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.ClassDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.classDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.ClassDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.QName_key where
  buildRtsValue b (Glean.Schema.Hack.Types.QName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.QName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.QName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName)) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.QName where
  type KeyType Glean.Schema.Hack.Types.QName =
    Glean.Schema.Hack.Types.QName_key
  getName _proxy  = Glean.PredicateRef "hack.QName"4
  getIndex _proxy  = 480
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.qName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.QName x k
  getFactKey = Glean.Schema.Hack.Types.qName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.QName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.TargetUses_key where
  buildRtsValue b (Glean.Schema.Hack.Types.TargetUses_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Hack.Types.TargetUses_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.TargetUses_key = 'Angle.TField "target" (Glean.Schema.Hack.Types.XRefTarget) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "uses" (Glean.Schema.Src.Types.ByteSpans) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Hack.Types.TargetUses where
  type KeyType Glean.Schema.Hack.Types.TargetUses =
    Glean.Schema.Hack.Types.TargetUses_key
  getName _proxy  = Glean.PredicateRef "hack.TargetUses"4
  getIndex _proxy  = 474
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.targetUses_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.TargetUses x k
  getFactKey = Glean.Schema.Hack.Types.targetUses_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.TargetUses where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.Filename_key where
  buildRtsValue b (Glean.Schema.Hack.Types.Filename_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.Filename_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Filename_key = 'Angle.TField "filename" (Data.Text.Text) ('Angle.TField "filehash_id" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.Filename where
  type KeyType Glean.Schema.Hack.Types.Filename =
    Glean.Schema.Hack.Types.Filename_key
  getName _proxy  = Glean.PredicateRef "hack.filename"1
  getIndex _proxy  = 465
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.filename_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Filename x k
  getFactKey = Glean.Schema.Hack.Types.filename_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Filename where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.TargetUsesAbs_key where
  buildRtsValue b (Glean.Schema.Hack.Types.TargetUsesAbs_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Hack.Types.TargetUsesAbs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.TargetUsesAbs_key = 'Angle.TField "target" (Glean.Schema.Hack.Types.XRefTarget) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "uses" (Glean.Schema.Src.Types.ByteSpans) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Hack.Types.TargetUsesAbs where
  type KeyType Glean.Schema.Hack.Types.TargetUsesAbs =
    Glean.Schema.Hack.Types.TargetUsesAbs_key
  getName _proxy  = Glean.PredicateRef "hack.TargetUsesAbs"4
  getIndex _proxy  = 459
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.targetUsesAbs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.TargetUsesAbs x k
  getFactKey = Glean.Schema.Hack.Types.targetUsesAbs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.TargetUsesAbs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.TraitDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.TraitDefinition_key x1 x2 x3 x4 x5 x6 x7 x8 x9) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
    Glean.buildRtsValue b x9
  decodeRtsValue = Glean.Schema.Hack.Types.TraitDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.TraitDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.TraitDeclaration) ('Angle.TField "members" ([Glean.Schema.Hack.Types.Declaration]) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceDeclaration)) ('Angle.TField "implements_" ([Glean.KeyType Glean.Schema.Hack.Types.InterfaceDeclaration]) ('Angle.TField "uses" ([Glean.KeyType Glean.Schema.Hack.Types.TraitDeclaration]) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TField "typeParams" ([Glean.Schema.Hack.Types.TypeParameter]) ('Angle.TField "requireExtends" ([Glean.KeyType Glean.Schema.Hack.Types.ClassDeclaration]) ('Angle.TField "requireImplements" ([Glean.KeyType Glean.Schema.Hack.Types.InterfaceDeclaration]) ('Angle.TNoFields)))))))))

instance Glean.Predicate Glean.Schema.Hack.Types.TraitDefinition where
  type KeyType Glean.Schema.Hack.Types.TraitDefinition =
    Glean.Schema.Hack.Types.TraitDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.TraitDefinition"4
  getIndex _proxy  = 419
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.traitDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.TraitDefinition x k
  getFactKey = Glean.Schema.Hack.Types.traitDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.TraitDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.DeclarationName_key where
  buildRtsValue b (Glean.Schema.Hack.Types.DeclarationName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.DeclarationName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.DeclarationName_key = 'Angle.TField "declaration" (Glean.Schema.Hack.Types.Declaration) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.DeclarationName where
  type KeyType Glean.Schema.Hack.Types.DeclarationName =
    Glean.Schema.Hack.Types.DeclarationName_key
  getName _proxy  = Glean.PredicateRef "hack.DeclarationName"4
  getIndex _proxy  = 404
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.declarationName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.DeclarationName x k
  getFactKey = Glean.Schema.Hack.Types.declarationName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.DeclarationName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.FunctionDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.FunctionDefinition_key x1 x2 x3 x4 x5 x6) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
  decodeRtsValue = Glean.Schema.Hack.Types.FunctionDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.FunctionDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.FunctionDeclaration) ('Angle.TField "signature" (Glean.KeyType Glean.Schema.Hack.Types.Signature) ('Angle.TField "isAsync" (Prelude.Bool) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceDeclaration)) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TField "typeParams" ([Glean.Schema.Hack.Types.TypeParameter]) ('Angle.TNoFields))))))

instance Glean.Predicate Glean.Schema.Hack.Types.FunctionDefinition where
  type KeyType Glean.Schema.Hack.Types.FunctionDefinition =
    Glean.Schema.Hack.Types.FunctionDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.FunctionDefinition"4
  getIndex _proxy  = 397
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.functionDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.FunctionDefinition x k
  getFactKey = Glean.Schema.Hack.Types.functionDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.FunctionDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.PropertyDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.PropertyDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.PropertyDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.PropertyDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "container" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.PropertyDeclaration where
  type KeyType Glean.Schema.Hack.Types.PropertyDeclaration =
    Glean.Schema.Hack.Types.PropertyDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.PropertyDeclaration"4
  getIndex _proxy  = 392
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.propertyDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.PropertyDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.propertyDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.PropertyDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.PropertyDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.PropertyDefinition_key x1 x2 x3 x4 x5 x6 x7) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
  decodeRtsValue = Glean.Schema.Hack.Types.PropertyDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.PropertyDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.PropertyDeclaration) ('Angle.TField "type" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Type)) ('Angle.TField "visibility" (Glean.Schema.Hack.Types.Visibility) ('Angle.TField "isFinal" (Prelude.Bool) ('Angle.TField "isAbstract" (Prelude.Bool) ('Angle.TField "isStatic" (Prelude.Bool) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TNoFields)))))))

instance Glean.Predicate Glean.Schema.Hack.Types.PropertyDefinition where
  type KeyType Glean.Schema.Hack.Types.PropertyDefinition =
    Glean.Schema.Hack.Types.PropertyDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.PropertyDefinition"4
  getIndex _proxy  = 361
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.propertyDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.PropertyDefinition x k
  getFactKey = Glean.Schema.Hack.Types.propertyDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.PropertyDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.ContainerChild_key where
  buildRtsValue b (Glean.Schema.Hack.Types.ContainerChild_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.ContainerChild_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.ContainerChild_key = 'Angle.TField "container" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TField "child" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.ContainerChild where
  type KeyType Glean.Schema.Hack.Types.ContainerChild =
    Glean.Schema.Hack.Types.ContainerChild_key
  getName _proxy  = Glean.PredicateRef "hack.ContainerChild"4
  getIndex _proxy  = 352
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.containerChild_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.ContainerChild x k
  getFactKey = Glean.Schema.Hack.Types.containerChild_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.ContainerChild where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.SymbolNamespace_key where
  buildRtsValue b (Glean.Schema.Hack.Types.SymbolNamespace_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.SymbolNamespace_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.SymbolNamespace_key = 'Angle.TField "namespace_id" (Glean.Nat) ('Angle.TField "namespace_name" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.SymbolNamespace where
  type KeyType Glean.Schema.Hack.Types.SymbolNamespace =
    Glean.Schema.Hack.Types.SymbolNamespace_key
  getName _proxy  = Glean.PredicateRef "hack.symbolNamespace"1
  getIndex _proxy  = 345
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.symbolNamespace_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.SymbolNamespace x k
  getFactKey = Glean.Schema.Hack.Types.symbolNamespace_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.SymbolNamespace where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.UserAttribute_key where
  buildRtsValue b (Glean.Schema.Hack.Types.UserAttribute_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.UserAttribute_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.UserAttribute_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "parameters" ([Data.Text.Text]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.UserAttribute where
  type KeyType Glean.Schema.Hack.Types.UserAttribute =
    Glean.Schema.Hack.Types.UserAttribute_key
  getName _proxy  = Glean.PredicateRef "hack.UserAttribute"4
  getIndex _proxy  = 341
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.userAttribute_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.UserAttribute x k
  getFactKey = Glean.Schema.Hack.Types.userAttribute_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.UserAttribute where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.DeclarationSource_key where
  buildRtsValue b (Glean.Schema.Hack.Types.DeclarationSource_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.DeclarationSource_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.DeclarationSource_key = 'Angle.TField "target" (Glean.Schema.Hack.Types.Declaration) ('Angle.TField "source" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.DeclarationSource where
  type KeyType Glean.Schema.Hack.Types.DeclarationSource =
    Glean.Schema.Hack.Types.DeclarationSource_key
  getName _proxy  = Glean.PredicateRef "hack.DeclarationSource"4
  getIndex _proxy  = 336
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.declarationSource_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.DeclarationSource x k
  getFactKey = Glean.Schema.Hack.Types.declarationSource_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.DeclarationSource where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.NamespaceQName_key where
  buildRtsValue b (Glean.Schema.Hack.Types.NamespaceQName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.NamespaceQName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.NamespaceQName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "parent" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName)) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.NamespaceQName where
  type KeyType Glean.Schema.Hack.Types.NamespaceQName =
    Glean.Schema.Hack.Types.NamespaceQName_key
  getName _proxy  = Glean.PredicateRef "hack.NamespaceQName"4
  getIndex _proxy  = 328
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.namespaceQName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.NamespaceQName x k
  getFactKey = Glean.Schema.Hack.Types.namespaceQName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.NamespaceQName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.InterfaceDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.InterfaceDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Hack.Types.InterfaceDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.InterfaceDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.QName) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Hack.Types.InterfaceDeclaration where
  type KeyType Glean.Schema.Hack.Types.InterfaceDeclaration =
    Glean.Schema.Hack.Types.InterfaceDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.InterfaceDeclaration"4
  getIndex _proxy  = 327
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.interfaceDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.InterfaceDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.interfaceDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.InterfaceDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.Kind_key where
  buildRtsValue b (Glean.Schema.Hack.Types.Kind_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.Kind_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Kind_key = 'Angle.TField "id" (Glean.Nat) ('Angle.TField "name" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.Kind where
  type KeyType Glean.Schema.Hack.Types.Kind = Glean.Schema.Hack.Types.Kind_key
  getName _proxy  = Glean.PredicateRef "hack.kind"1
  getIndex _proxy  = 325
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.kind_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Kind x k
  getFactKey = Glean.Schema.Hack.Types.kind_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Kind where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.InterfaceDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.InterfaceDefinition_key x1 x2 x3 x4 x5 x6 x7) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
  decodeRtsValue = Glean.Schema.Hack.Types.InterfaceDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.InterfaceDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.InterfaceDeclaration) ('Angle.TField "members" ([Glean.Schema.Hack.Types.Declaration]) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceDeclaration)) ('Angle.TField "extends_" ([Glean.KeyType Glean.Schema.Hack.Types.InterfaceDeclaration]) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TField "typeParams" ([Glean.Schema.Hack.Types.TypeParameter]) ('Angle.TField "requireExtends" ([Glean.KeyType Glean.Schema.Hack.Types.ClassDeclaration]) ('Angle.TNoFields)))))))

instance Glean.Predicate Glean.Schema.Hack.Types.InterfaceDefinition where
  type KeyType Glean.Schema.Hack.Types.InterfaceDefinition =
    Glean.Schema.Hack.Types.InterfaceDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.InterfaceDefinition"4
  getIndex _proxy  = 310
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.interfaceDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.InterfaceDefinition x k
  getFactKey = Glean.Schema.Hack.Types.interfaceDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.InterfaceDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hack.Types.Comment where
  type KeyType Glean.Schema.Hack.Types.Comment = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hack.Comment"4
  getIndex _proxy  = 293
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.comment_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Comment x k
  getFactKey = Glean.Schema.Hack.Types.comment_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Comment where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.Identifier_key where
  buildRtsValue b (Glean.Schema.Hack.Types.Identifier_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.Identifier_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Identifier_key = 'Angle.TField "kind" (Glean.Nat) ('Angle.TField "name" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.Identifier where
  type KeyType Glean.Schema.Hack.Types.Identifier =
    Glean.Schema.Hack.Types.Identifier_key
  getName _proxy  = Glean.PredicateRef "hack.identifier"1
  getIndex _proxy  = 292
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.identifier_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Identifier x k
  getFactKey = Glean.Schema.Hack.Types.identifier_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Identifier where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.NamespaceMember_key where
  buildRtsValue b (Glean.Schema.Hack.Types.NamespaceMember_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.NamespaceMember_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.NamespaceMember_key = 'Angle.TField "namespace_" (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName) ('Angle.TField "decl" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.NamespaceMember where
  type KeyType Glean.Schema.Hack.Types.NamespaceMember =
    Glean.Schema.Hack.Types.NamespaceMember_key
  getName _proxy  = Glean.PredicateRef "hack.NamespaceMember"4
  getIndex _proxy  = 283
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.namespaceMember_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.NamespaceMember x k
  getFactKey = Glean.Schema.Hack.Types.namespaceMember_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.NamespaceMember where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.ContainerParent_key where
  buildRtsValue b (Glean.Schema.Hack.Types.ContainerParent_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.ContainerParent_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.ContainerParent_key = 'Angle.TField "container" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TField "parent" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.ContainerParent where
  type KeyType Glean.Schema.Hack.Types.ContainerParent =
    Glean.Schema.Hack.Types.ContainerParent_key
  getName _proxy  = Glean.PredicateRef "hack.ContainerParent"4
  getIndex _proxy  = 272
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.containerParent_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.ContainerParent x k
  getFactKey = Glean.Schema.Hack.Types.containerParent_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.ContainerParent where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.GlobalConstDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.GlobalConstDefinition_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Hack.Types.GlobalConstDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.GlobalConstDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.GlobalConstDeclaration) ('Angle.TField "type" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Type)) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceDeclaration)) ('Angle.TField "value" (Data.Text.Text) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Hack.Types.GlobalConstDefinition where
  type KeyType Glean.Schema.Hack.Types.GlobalConstDefinition =
    Glean.Schema.Hack.Types.GlobalConstDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.GlobalConstDefinition"4
  getIndex _proxy  = 270
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.globalConstDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.GlobalConstDefinition x k
  getFactKey = Glean.Schema.Hack.Types.globalConstDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.GlobalConstDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.EnumDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.EnumDefinition_key x1 x2 x3 x4 x5 x6) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
  decodeRtsValue = Glean.Schema.Hack.Types.EnumDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.EnumDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.EnumDeclaration) ('Angle.TField "enumBase" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Type)) ('Angle.TField "enumConstraint" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Type)) ('Angle.TField "enumerators" ([Glean.KeyType Glean.Schema.Hack.Types.Enumerator]) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceDeclaration)) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TNoFields))))))

instance Glean.Predicate Glean.Schema.Hack.Types.EnumDefinition where
  type KeyType Glean.Schema.Hack.Types.EnumDefinition =
    Glean.Schema.Hack.Types.EnumDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.EnumDefinition"4
  getIndex _proxy  = 262
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.enumDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.EnumDefinition x k
  getFactKey = Glean.Schema.Hack.Types.enumDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.EnumDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.Symbol_key where
  buildRtsValue b (Glean.Schema.Hack.Types.Symbol_key x1 x2 x3 x4 x5 x6 x7 x8) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
  decodeRtsValue = Glean.Schema.Hack.Types.Symbol_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Symbol_key = 'Angle.TField "name_lowercase" (Data.Text.Text) ('Angle.TField "valid" (Glean.Schema.Hack.Types.Context) ('Angle.TField "kind_id" (Glean.Nat) ('Angle.TField "ns_id" (Glean.Nat) ('Angle.TField "filehash_id" (Data.Text.Text) ('Angle.TField "is_abstract" (Prelude.Bool) ('Angle.TField "is_final" (Prelude.Bool) ('Angle.TField "canonical_name" (Data.Text.Text) ('Angle.TNoFields))))))))

instance Glean.Predicate Glean.Schema.Hack.Types.Symbol where
  type KeyType Glean.Schema.Hack.Types.Symbol =
    Glean.Schema.Hack.Types.Symbol_key
  getName _proxy  = Glean.PredicateRef "hack.symbol"1
  getIndex _proxy  = 261
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.symbol_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Symbol x k
  getFactKey = Glean.Schema.Hack.Types.symbol_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Symbol where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.ClassConstDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.ClassConstDefinition_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Hack.Types.ClassConstDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.ClassConstDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.ClassConstDeclaration) ('Angle.TField "type" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Type)) ('Angle.TField "value" (Prelude.Maybe Data.Text.Text) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Hack.Types.ClassConstDefinition where
  type KeyType Glean.Schema.Hack.Types.ClassConstDefinition =
    Glean.Schema.Hack.Types.ClassConstDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.ClassConstDefinition"4
  getIndex _proxy  = 258
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.classConstDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.ClassConstDefinition x k
  getFactKey = Glean.Schema.Hack.Types.classConstDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.ClassConstDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.GlobalConstDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.GlobalConstDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Hack.Types.GlobalConstDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.GlobalConstDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.QName) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Hack.Types.GlobalConstDeclaration where
  type KeyType Glean.Schema.Hack.Types.GlobalConstDeclaration =
    Glean.Schema.Hack.Types.GlobalConstDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.GlobalConstDeclaration"4
  getIndex _proxy  = 252
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.globalConstDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.GlobalConstDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.globalConstDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.GlobalConstDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.DeclarationTarget_key where
  buildRtsValue b (Glean.Schema.Hack.Types.DeclarationTarget_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.DeclarationTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.DeclarationTarget_key = 'Angle.TField "source" (Glean.Schema.Hack.Types.Declaration) ('Angle.TField "target" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.DeclarationTarget where
  type KeyType Glean.Schema.Hack.Types.DeclarationTarget =
    Glean.Schema.Hack.Types.DeclarationTarget_key
  getName _proxy  = Glean.PredicateRef "hack.DeclarationTarget"4
  getIndex _proxy  = 227
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.declarationTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.DeclarationTarget x k
  getFactKey = Glean.Schema.Hack.Types.declarationTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.DeclarationTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.FileXRefs_key where
  buildRtsValue b (Glean.Schema.Hack.Types.FileXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.FileXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.FileXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xrefs" ([Glean.Schema.Hack.Types.XRef]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.FileXRefs where
  type KeyType Glean.Schema.Hack.Types.FileXRefs =
    Glean.Schema.Hack.Types.FileXRefs_key
  getName _proxy  = Glean.PredicateRef "hack.FileXRefs"4
  getIndex _proxy  = 213
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.fileXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.FileXRefs x k
  getFactKey = Glean.Schema.Hack.Types.fileXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.FileXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.DeclarationSpan_key where
  buildRtsValue b (Glean.Schema.Hack.Types.DeclarationSpan_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Hack.Types.DeclarationSpan_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.DeclarationSpan_key = 'Angle.TField "declaration" (Glean.Schema.Hack.Types.Declaration) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Hack.Types.DeclarationSpan where
  type KeyType Glean.Schema.Hack.Types.DeclarationSpan =
    Glean.Schema.Hack.Types.DeclarationSpan_key
  getName _proxy  = Glean.PredicateRef "hack.DeclarationSpan"4
  getIndex _proxy  = 210
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.declarationSpan_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.DeclarationSpan x k
  getFactKey = Glean.Schema.Hack.Types.declarationSpan_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.DeclarationSpan where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.Enumerator_key where
  buildRtsValue b (Glean.Schema.Hack.Types.Enumerator_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.Enumerator_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Enumerator_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "enumeration" (Glean.KeyType Glean.Schema.Hack.Types.EnumDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.Enumerator where
  type KeyType Glean.Schema.Hack.Types.Enumerator =
    Glean.Schema.Hack.Types.Enumerator_key
  getName _proxy  = Glean.PredicateRef "hack.Enumerator"4
  getIndex _proxy  = 194
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.enumerator_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Enumerator x k
  getFactKey = Glean.Schema.Hack.Types.enumerator_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Enumerator where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.Signature_key where
  buildRtsValue b (Glean.Schema.Hack.Types.Signature_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.Signature_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Signature_key = 'Angle.TField "returns" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Type)) ('Angle.TField "parameters" ([Glean.Schema.Hack.Types.Parameter]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.Signature where
  type KeyType Glean.Schema.Hack.Types.Signature =
    Glean.Schema.Hack.Types.Signature_key
  getName _proxy  = Glean.PredicateRef "hack.Signature"4
  getIndex _proxy  = 188
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.signature_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Signature x k
  getFactKey = Glean.Schema.Hack.Types.signature_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Signature where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.TypedefDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.TypedefDeclaration_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Hack.Types.TypedefDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.TypedefDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.QName) ('Angle.TField "isTransparent" (Prelude.Bool) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceDeclaration)) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TField "typeParams" ([Glean.Schema.Hack.Types.TypeParameter]) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Hack.Types.TypedefDeclaration where
  type KeyType Glean.Schema.Hack.Types.TypedefDeclaration =
    Glean.Schema.Hack.Types.TypedefDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.TypedefDeclaration"4
  getIndex _proxy  = 183
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.typedefDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.TypedefDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.typedefDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.TypedefDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hack.Types.Name where
  type KeyType Glean.Schema.Hack.Types.Name = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hack.Name"4
  getIndex _proxy  = 182
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.name_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Name x k
  getFactKey = Glean.Schema.Hack.Types.name_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Name where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.MethodDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.MethodDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.MethodDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.MethodDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "container" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.MethodDeclaration where
  type KeyType Glean.Schema.Hack.Types.MethodDeclaration =
    Glean.Schema.Hack.Types.MethodDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.MethodDeclaration"4
  getIndex _proxy  = 164
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.methodDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.MethodDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.methodDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.MethodDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.TraitDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.TraitDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Hack.Types.TraitDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.TraitDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.QName) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Hack.Types.TraitDeclaration where
  type KeyType Glean.Schema.Hack.Types.TraitDeclaration =
    Glean.Schema.Hack.Types.TraitDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.TraitDeclaration"4
  getIndex _proxy  = 147
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.traitDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.TraitDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.traitDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.TraitDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.FunctionDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.FunctionDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Hack.Types.FunctionDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.FunctionDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.QName) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Hack.Types.FunctionDeclaration where
  type KeyType Glean.Schema.Hack.Types.FunctionDeclaration =
    Glean.Schema.Hack.Types.FunctionDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.FunctionDeclaration"4
  getIndex _proxy  = 140
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.functionDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.FunctionDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.functionDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.FunctionDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.MethodDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.MethodDefinition_key x1 x2 x3 x4 x5 x6 x7 x8 x9) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
    Glean.buildRtsValue b x9
  decodeRtsValue = Glean.Schema.Hack.Types.MethodDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.MethodDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.MethodDeclaration) ('Angle.TField "signature" (Glean.KeyType Glean.Schema.Hack.Types.Signature) ('Angle.TField "visibility" (Glean.Schema.Hack.Types.Visibility) ('Angle.TField "isAbstract" (Prelude.Bool) ('Angle.TField "isAsync" (Prelude.Bool) ('Angle.TField "isFinal" (Prelude.Bool) ('Angle.TField "isStatic" (Prelude.Bool) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TField "typeParams" ([Glean.Schema.Hack.Types.TypeParameter]) ('Angle.TNoFields)))))))))

instance Glean.Predicate Glean.Schema.Hack.Types.MethodDefinition where
  type KeyType Glean.Schema.Hack.Types.MethodDefinition =
    Glean.Schema.Hack.Types.MethodDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.MethodDefinition"4
  getIndex _proxy  = 121
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.methodDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.MethodDefinition x k
  getFactKey = Glean.Schema.Hack.Types.methodDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.MethodDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.FileDeclarations_key where
  buildRtsValue b (Glean.Schema.Hack.Types.FileDeclarations_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.FileDeclarations_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.FileDeclarations_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "declarations" ([Glean.Schema.Hack.Types.Declaration]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.FileDeclarations where
  type KeyType Glean.Schema.Hack.Types.FileDeclarations =
    Glean.Schema.Hack.Types.FileDeclarations_key
  getName _proxy  = Glean.PredicateRef "hack.FileDeclarations"4
  getIndex _proxy  = 102
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.fileDeclarations_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.FileDeclarations x k
  getFactKey = Glean.Schema.Hack.Types.fileDeclarations_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.FileDeclarations where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.DeclarationLocation_key where
  buildRtsValue b (Glean.Schema.Hack.Types.DeclarationLocation_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Hack.Types.DeclarationLocation_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.DeclarationLocation_key = 'Angle.TField "declaration" (Glean.Schema.Hack.Types.Declaration) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Hack.Types.DeclarationLocation where
  type KeyType Glean.Schema.Hack.Types.DeclarationLocation =
    Glean.Schema.Hack.Types.DeclarationLocation_key
  getName _proxy  = Glean.PredicateRef "hack.DeclarationLocation"4
  getIndex _proxy  = 96
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.declarationLocation_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.DeclarationLocation x k
  getFactKey = Glean.Schema.Hack.Types.declarationLocation_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.DeclarationLocation where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.TypeConstDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.TypeConstDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.TypeConstDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.TypeConstDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "container" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.TypeConstDeclaration where
  type KeyType Glean.Schema.Hack.Types.TypeConstDeclaration =
    Glean.Schema.Hack.Types.TypeConstDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.TypeConstDeclaration"4
  getIndex _proxy  = 95
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.typeConstDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.TypeConstDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.typeConstDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.TypeConstDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.NameLowerCase_key where
  buildRtsValue b (Glean.Schema.Hack.Types.NameLowerCase_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.NameLowerCase_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.NameLowerCase_key = 'Angle.TField "nameLowercase" (Data.Text.Text) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.NameLowerCase where
  type KeyType Glean.Schema.Hack.Types.NameLowerCase =
    Glean.Schema.Hack.Types.NameLowerCase_key
  getName _proxy  = Glean.PredicateRef "hack.NameLowerCase"4
  getIndex _proxy  = 56
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.nameLowerCase_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.NameLowerCase x k
  getFactKey = Glean.Schema.Hack.Types.nameLowerCase_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.NameLowerCase where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.TypeConstDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.TypeConstDefinition_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Hack.Types.TypeConstDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.TypeConstDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.TypeConstDeclaration) ('Angle.TField "type" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Type)) ('Angle.TField "kind" (Glean.Schema.Hack.Types.TypeConstKind) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Hack.Types.TypeConstDefinition where
  type KeyType Glean.Schema.Hack.Types.TypeConstDefinition =
    Glean.Schema.Hack.Types.TypeConstDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.TypeConstDefinition"4
  getIndex _proxy  = 52
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.typeConstDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.TypeConstDefinition x k
  getFactKey = Glean.Schema.Hack.Types.typeConstDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.TypeConstDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hack.Types.Type where
  type KeyType Glean.Schema.Hack.Types.Type = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hack.Type"4
  getIndex _proxy  = 49
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.type_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.Type x k
  getFactKey = Glean.Schema.Hack.Types.type_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.Type where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.DeclarationComment_key where
  buildRtsValue b (Glean.Schema.Hack.Types.DeclarationComment_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Hack.Types.DeclarationComment_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.DeclarationComment_key = 'Angle.TField "declaration" (Glean.Schema.Hack.Types.Declaration) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Prelude.Maybe Glean.Schema.Src.Types.ByteSpan) ('Angle.TField "comment" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Comment)) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Hack.Types.DeclarationComment where
  type KeyType Glean.Schema.Hack.Types.DeclarationComment =
    Glean.Schema.Hack.Types.DeclarationComment_key
  getName _proxy  = Glean.PredicateRef "hack.DeclarationComment"4
  getIndex _proxy  = 43
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.declarationComment_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.DeclarationComment x k
  getFactKey = Glean.Schema.Hack.Types.declarationComment_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.DeclarationComment where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.ClassConstDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.ClassConstDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.ClassConstDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.ClassConstDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "container" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hack.Types.ClassConstDeclaration where
  type KeyType Glean.Schema.Hack.Types.ClassConstDeclaration =
    Glean.Schema.Hack.Types.ClassConstDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.ClassConstDeclaration"4
  getIndex _proxy  = 34
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.classConstDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.ClassConstDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.classConstDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.ClassConstDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.EnumDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.EnumDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Hack.Types.EnumDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.EnumDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.QName) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Hack.Types.EnumDeclaration where
  type KeyType Glean.Schema.Hack.Types.EnumDeclaration =
    Glean.Schema.Hack.Types.EnumDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.EnumDeclaration"4
  getIndex _proxy  = 27
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.enumDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.EnumDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.enumDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.EnumDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.NamespaceDeclaration_key where
  buildRtsValue b (Glean.Schema.Hack.Types.NamespaceDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Hack.Types.NamespaceDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.NamespaceDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.NamespaceQName) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Hack.Types.NamespaceDeclaration where
  type KeyType Glean.Schema.Hack.Types.NamespaceDeclaration =
    Glean.Schema.Hack.Types.NamespaceDeclaration_key
  getName _proxy  = Glean.PredicateRef "hack.NamespaceDeclaration"4
  getIndex _proxy  = 19
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.namespaceDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.NamespaceDeclaration x k
  getFactKey = Glean.Schema.Hack.Types.namespaceDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.NamespaceDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.ClassDefinition_key where
  buildRtsValue b (Glean.Schema.Hack.Types.ClassDefinition_key x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = do
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
  decodeRtsValue = Glean.Schema.Hack.Types.ClassDefinition_key
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

type instance Angle.RecordFields Glean.Schema.Hack.Types.ClassDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Hack.Types.ClassDeclaration) ('Angle.TField "isAbstract" (Prelude.Bool) ('Angle.TField "isFinal" (Prelude.Bool) ('Angle.TField "members" ([Glean.Schema.Hack.Types.Declaration]) ('Angle.TField "namespace_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.NamespaceDeclaration)) ('Angle.TField "extends_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.ClassDeclaration)) ('Angle.TField "implements_" ([Glean.KeyType Glean.Schema.Hack.Types.InterfaceDeclaration]) ('Angle.TField "uses" ([Glean.KeyType Glean.Schema.Hack.Types.TraitDeclaration]) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TField "typeParams" ([Glean.Schema.Hack.Types.TypeParameter]) ('Angle.TNoFields))))))))))

instance Glean.Predicate Glean.Schema.Hack.Types.ClassDefinition where
  type KeyType Glean.Schema.Hack.Types.ClassDefinition =
    Glean.Schema.Hack.Types.ClassDefinition_key
  getName _proxy  = Glean.PredicateRef "hack.ClassDefinition"4
  getIndex _proxy  = 1
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hack.Types.classDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hack.Types.ClassDefinition x k
  getFactKey = Glean.Schema.Hack.Types.classDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hack.Types.ClassDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hack.Types.TypeParameter where
  buildRtsValue b (Glean.Schema.Hack.Types.TypeParameter x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Hack.Types.TypeParameter
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.TypeParameter = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "variance" (Glean.Schema.Hack.Types.Variance) ('Angle.TField "reifyKind" (Glean.Schema.Hack.Types.ReifyKind) ('Angle.TField "constraints" ([Glean.Schema.Hack.Types.Constraint]) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TNoFields)))))

instance Glean.Type Glean.Schema.Hack.Types.ContainerDeclaration where
  buildRtsValue b (Glean.Schema.Hack.Types.ContainerDeclaration_class_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.ContainerDeclaration_interface_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.ContainerDeclaration_trait x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Hack.Types.ContainerDeclaration_class_
    , Glean.mapD Glean.Schema.Hack.Types.ContainerDeclaration_interface_
    , Glean.mapD Glean.Schema.Hack.Types.ContainerDeclaration_trait
    ]

type instance Angle.SumFields Glean.Schema.Hack.Types.ContainerDeclaration = 'Angle.TField "class_" (Glean.KeyType Glean.Schema.Hack.Types.ClassDeclaration) ('Angle.TField "interface_" (Glean.KeyType Glean.Schema.Hack.Types.InterfaceDeclaration) ('Angle.TField "trait" (Glean.KeyType Glean.Schema.Hack.Types.TraitDeclaration) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Hack.Types.ClassDeclaration Glean.Schema.Hack.Types.ContainerDeclaration where
  injectBranch = Glean.Schema.Hack.Types.ContainerDeclaration_class_
  projectBranch (Glean.Schema.Hack.Types.ContainerDeclaration_class_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.InterfaceDeclaration Glean.Schema.Hack.Types.ContainerDeclaration where
  injectBranch = Glean.Schema.Hack.Types.ContainerDeclaration_interface_
  projectBranch (Glean.Schema.Hack.Types.ContainerDeclaration_interface_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.TraitDeclaration Glean.Schema.Hack.Types.ContainerDeclaration where
  injectBranch = Glean.Schema.Hack.Types.ContainerDeclaration_trait
  projectBranch (Glean.Schema.Hack.Types.ContainerDeclaration_trait x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Hack.Types.Visibility where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Hack.Types.Visibility = 'Angle.TField "Private" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Protected" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Public" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Hack.Types.XRef where
  buildRtsValue b (Glean.Schema.Hack.Types.XRef x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.XRef
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.XRef = 'Angle.TField "target" (Glean.Schema.Hack.Types.XRefTarget) ('Angle.TField "ranges" (Glean.Schema.Src.Types.ByteSpans) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Hack.Types.Parameter where
  buildRtsValue b (Glean.Schema.Hack.Types.Parameter x1 x2 x3 x4 x5 x6) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
  decodeRtsValue = Glean.Schema.Hack.Types.Parameter
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Parameter = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hack.Types.Name) ('Angle.TField "type" (Prelude.Maybe (Glean.KeyType Glean.Schema.Hack.Types.Type)) ('Angle.TField "isInout" (Prelude.Bool) ('Angle.TField "isVariadic" (Prelude.Bool) ('Angle.TField "defaultValue" (Prelude.Maybe Data.Text.Text) ('Angle.TField "attributes" ([Glean.KeyType Glean.Schema.Hack.Types.UserAttribute]) ('Angle.TNoFields))))))

instance Glean.Type Glean.Schema.Hack.Types.ReifyKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Hack.Types.ReifyKind = 'Angle.TField "Erased" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Reified" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "SoftReified" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Hack.Types.Context where
  buildRtsValue b (Glean.Schema.Hack.Types.Context x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Hack.Types.Context
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Context = 'Angle.TField "acid" (Prelude.Bool) ('Angle.TField "actype" (Prelude.Bool) ('Angle.TField "acnew" (Prelude.Bool) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Hack.Types.Constraint where
  buildRtsValue b (Glean.Schema.Hack.Types.Constraint x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hack.Types.Constraint
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hack.Types.Constraint = 'Angle.TField "constraintKind" (Glean.Schema.Hack.Types.ConstraintKind) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Hack.Types.Type) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Hack.Types.Variance where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Hack.Types.Variance = 'Angle.TField "Contravariant" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Covariant" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Invariant" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Hack.Types.TypeConstKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Hack.Types.TypeConstKind = 'Angle.TField "Abstract" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Concrete" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "PartiallyAbstract" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Hack.Types.ConstraintKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Hack.Types.ConstraintKind = 'Angle.TField "As" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Equal" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Super" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Hack.Types.Declaration where
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_classConst x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_container x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_enum_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_enumerator x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_function_ x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_globalConst x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_namespace_ x) = do
    Glean.buildRtsSelector b 6
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_method x) = do
    Glean.buildRtsSelector b 7
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_property_ x) = do
    Glean.buildRtsSelector b 8
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_typeConst x) = do
    Glean.buildRtsSelector b 9
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hack.Types.Declaration_typedef_ x) = do
    Glean.buildRtsSelector b 10
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Hack.Types.Declaration_classConst
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_container
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_enum_
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_enumerator
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_function_
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_globalConst
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_namespace_
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_method
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_property_
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_typeConst
    , Glean.mapD Glean.Schema.Hack.Types.Declaration_typedef_
    ]

type instance Angle.SumFields Glean.Schema.Hack.Types.Declaration = 'Angle.TField "classConst" (Glean.KeyType Glean.Schema.Hack.Types.ClassConstDeclaration) ('Angle.TField "container" (Glean.Schema.Hack.Types.ContainerDeclaration) ('Angle.TField "enum_" (Glean.KeyType Glean.Schema.Hack.Types.EnumDeclaration) ('Angle.TField "enumerator" (Glean.KeyType Glean.Schema.Hack.Types.Enumerator) ('Angle.TField "function_" (Glean.KeyType Glean.Schema.Hack.Types.FunctionDeclaration) ('Angle.TField "globalConst" (Glean.KeyType Glean.Schema.Hack.Types.GlobalConstDeclaration) ('Angle.TField "namespace_" (Glean.KeyType Glean.Schema.Hack.Types.NamespaceDeclaration) ('Angle.TField "method" (Glean.KeyType Glean.Schema.Hack.Types.MethodDeclaration) ('Angle.TField "property_" (Glean.KeyType Glean.Schema.Hack.Types.PropertyDeclaration) ('Angle.TField "typeConst" (Glean.KeyType Glean.Schema.Hack.Types.TypeConstDeclaration) ('Angle.TField "typedef_" (Glean.KeyType Glean.Schema.Hack.Types.TypedefDeclaration) ('Angle.TNoFields)))))))))))

instance Glean.SumBranches Glean.Schema.Hack.Types.ClassConstDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_classConst
  projectBranch (Glean.Schema.Hack.Types.Declaration_classConst x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.ContainerDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_container
  projectBranch (Glean.Schema.Hack.Types.Declaration_container x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.EnumDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_enum_
  projectBranch (Glean.Schema.Hack.Types.Declaration_enum_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.Enumerator Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_enumerator
  projectBranch (Glean.Schema.Hack.Types.Declaration_enumerator x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.FunctionDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_function_
  projectBranch (Glean.Schema.Hack.Types.Declaration_function_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.GlobalConstDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_globalConst
  projectBranch (Glean.Schema.Hack.Types.Declaration_globalConst x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.NamespaceDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_namespace_
  projectBranch (Glean.Schema.Hack.Types.Declaration_namespace_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.MethodDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_method
  projectBranch (Glean.Schema.Hack.Types.Declaration_method x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.PropertyDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_property_
  projectBranch (Glean.Schema.Hack.Types.Declaration_property_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.TypeConstDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_typeConst
  projectBranch (Glean.Schema.Hack.Types.Declaration_typeConst x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hack.Types.TypedefDeclaration Glean.Schema.Hack.Types.Declaration where
  injectBranch = Glean.Schema.Hack.Types.Declaration_typedef_
  projectBranch (Glean.Schema.Hack.Types.Declaration_typedef_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Hack.Types.XRefTarget where
  buildRtsValue b (Glean.Schema.Hack.Types.XRefTarget_declaration x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Hack.Types.XRefTarget_declaration
    ]

type instance Angle.SumFields Glean.Schema.Hack.Types.XRefTarget = 'Angle.TField "declaration" (Glean.Schema.Hack.Types.Declaration) ('Angle.TNoFields)

instance Glean.SumBranches Glean.Schema.Hack.Types.Declaration Glean.Schema.Hack.Types.XRefTarget where
  injectBranch = Glean.Schema.Hack.Types.XRefTarget_declaration
  projectBranch (Glean.Schema.Hack.Types.XRefTarget_declaration x) = Prelude.Just x
