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
import qualified Glean.Schema.Pp1.Types
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Cxx1.Types.NamespaceQName_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.NamespaceQName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.NamespaceQName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.NamespaceQName_key = 'Angle.TField "name" (Prelude.Maybe (Glean.KeyType Glean.Schema.Cxx1.Types.Name)) ('Angle.TField "parent" (Prelude.Maybe (Glean.KeyType Glean.Schema.Cxx1.Types.NamespaceQName)) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.NamespaceQName where
  type KeyType Glean.Schema.Cxx1.Types.NamespaceQName =
    Glean.Schema.Cxx1.Types.NamespaceQName_key
  getName _proxy  = Glean.PredicateRef "cxx1.NamespaceQName"1
  getIndex _proxy  = 494
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.namespaceQName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.NamespaceQName x k
  getFactKey = Glean.Schema.Cxx1.Types.namespaceQName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.NamespaceQName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationSrcRange_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclarationSrcRange_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclarationSrcRange_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclarationSrcRange_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclarationSrcRange where
  type KeyType Glean.Schema.Cxx1.Types.DeclarationSrcRange =
    Glean.Schema.Cxx1.Types.DeclarationSrcRange_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclarationSrcRange"4
  getIndex _proxy  = 493
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declarationSrcRange_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclarationSrcRange x k
  getFactKey = Glean.Schema.Cxx1.Types.declarationSrcRange_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationSrcRange where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.RecordDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.RecordDeclaration_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.RecordDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.RecordDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.QName) ('Angle.TField "kind" (Glean.Schema.Cxx1.Types.RecordKind) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.RecordDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.RecordDeclaration =
    Glean.Schema.Cxx1.Types.RecordDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.RecordDeclaration"1
  getIndex _proxy  = 490
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.recordDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.RecordDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.recordDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.RecordDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.XRefIndirectTarget_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefIndirectTarget_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.XRefIndirectTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.XRefIndirectTarget_key = 'Angle.TField "via" (Glean.Schema.Cxx1.Types.XRefVia) ('Angle.TField "target" (Glean.Schema.Cxx1.Types.XRefTarget) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.XRefIndirectTarget where
  type KeyType Glean.Schema.Cxx1.Types.XRefIndirectTarget =
    Glean.Schema.Cxx1.Types.XRefIndirectTarget_key
  getName _proxy  = Glean.PredicateRef "cxx1.XRefIndirectTarget"2
  getIndex _proxy  = 465
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.xRefIndirectTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.XRefIndirectTarget x k
  getFactKey = Glean.Schema.Cxx1.Types.xRefIndirectTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.XRefIndirectTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclByName_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclByName_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclByName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclByName_key = 'Angle.TField "name_lowercase" (Data.Text.Text) ('Angle.TField "kind" (Glean.Schema.Cxx1.Types.DeclKind) ('Angle.TField "ident" (Glean.Schema.Cxx1.Types.DeclIdent) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclByName where
  type KeyType Glean.Schema.Cxx1.Types.DeclByName =
    Glean.Schema.Cxx1.Types.DeclByName_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclByName"4
  getIndex _proxy  = 464
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declByName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclByName x k
  getFactKey = Glean.Schema.Cxx1.Types.declByName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclByName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclInObjcContainer_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclInObjcContainer_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclInObjcContainer_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclInObjcContainer_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "record" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDefinition) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclInObjcContainer where
  type KeyType Glean.Schema.Cxx1.Types.DeclInObjcContainer =
    Glean.Schema.Cxx1.Types.DeclInObjcContainer_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclInObjcContainer"4
  getIndex _proxy  = 456
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declInObjcContainer_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclInObjcContainer x k
  getFactKey = Glean.Schema.Cxx1.Types.declInObjcContainer_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclInObjcContainer where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationTargets_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclarationTargets_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclarationTargets_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclarationTargets_key = 'Angle.TField "source" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "targets" ([Glean.Schema.Cxx1.Types.Declaration]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclarationTargets where
  type KeyType Glean.Schema.Cxx1.Types.DeclarationTargets =
    Glean.Schema.Cxx1.Types.DeclarationTargets_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclarationTargets"1
  getIndex _proxy  = 446
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declarationTargets_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclarationTargets x k
  getFactKey = Glean.Schema.Cxx1.Types.declarationTargets_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationTargets where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key = 'Angle.TField "base" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcContainerInheritance where
  type KeyType Glean.Schema.Cxx1.Types.ObjcContainerInheritance =
    Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcContainerInheritance"4
  getIndex _proxy  = 426
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcContainerInheritance_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcContainerInheritance x k
  getFactKey = Glean.Schema.Cxx1.Types.objcContainerInheritance_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerInheritance where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key = 'Angle.TField "property" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration) ('Angle.TField "ivar" (Glean.KeyType Glean.Schema.Cxx1.Types.VariableDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcPropertyIVar where
  type KeyType Glean.Schema.Cxx1.Types.ObjcPropertyIVar =
    Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcPropertyIVar"4
  getIndex _proxy  = 420
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcPropertyIVar_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcPropertyIVar x k
  getFactKey = Glean.Schema.Cxx1.Types.objcPropertyIVar_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcPropertyIVar where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionDefinition_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionDefinition_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.FunctionDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.FunctionDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TField "isInline" (Prelude.Bool) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.FunctionDefinition where
  type KeyType Glean.Schema.Cxx1.Types.FunctionDefinition =
    Glean.Schema.Cxx1.Types.FunctionDefinition_key
  getName _proxy  = Glean.PredicateRef "cxx1.FunctionDefinition"1
  getIndex _proxy  = 418
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.functionDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.FunctionDefinition x k
  getFactKey = Glean.Schema.Cxx1.Types.functionDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key = 'Angle.TField "tunit" (Glean.KeyType Glean.Schema.Buck.Types.TranslationUnit) ('Angle.TField "xrefs" ([Glean.KeyType Glean.Schema.Cxx1.Types.FileXRefs]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.TranslationUnitXRefs where
  type KeyType Glean.Schema.Cxx1.Types.TranslationUnitXRefs =
    Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key
  getName _proxy  = Glean.PredicateRef "cxx1.TranslationUnitXRefs"4
  getIndex _proxy  = 417
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.translationUnitXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.TranslationUnitXRefs x k
  getFactKey = Glean.Schema.Cxx1.Types.translationUnitXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.TranslationUnitXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.Signature_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Signature_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.Signature_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.Signature_key = 'Angle.TField "returns" (Glean.KeyType Glean.Schema.Cxx1.Types.Type) ('Angle.TField "parameters" ([Glean.Schema.Cxx1.Types.Parameter]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.Signature where
  type KeyType Glean.Schema.Cxx1.Types.Signature =
    Glean.Schema.Cxx1.Types.Signature_key
  getName _proxy  = Glean.PredicateRef "cxx1.Signature"1
  getIndex _proxy  = 409
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.signature_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.Signature x k
  getFactKey = Glean.Schema.Cxx1.Types.signature_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.Signature where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Cxx1.Types.Attribute where
  type KeyType Glean.Schema.Cxx1.Types.Attribute = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "cxx1.Attribute"4
  getIndex _proxy  = 406
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.attribute_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.Attribute x k
  getFactKey = Glean.Schema.Cxx1.Types.attribute_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.Attribute where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclInRecord_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclInRecord_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclInRecord_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclInRecord_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "record" (Glean.KeyType Glean.Schema.Cxx1.Types.RecordDefinition) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclInRecord where
  type KeyType Glean.Schema.Cxx1.Types.DeclInRecord =
    Glean.Schema.Cxx1.Types.DeclInRecord_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclInRecord"4
  getIndex _proxy  = 398
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declInRecord_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclInRecord x k
  getFactKey = Glean.Schema.Cxx1.Types.declInRecord_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclInRecord where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.FileXRefs_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.FileXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.FileXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.FileXRefs_key = 'Angle.TField "xmap" (Glean.KeyType Glean.Schema.Cxx1.Types.FileXRefMap) ('Angle.TField "externals" ([Glean.Schema.Cxx1.Types.XRefTarget]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.FileXRefs where
  type KeyType Glean.Schema.Cxx1.Types.FileXRefs =
    Glean.Schema.Cxx1.Types.FileXRefs_key
  getName _proxy  = Glean.PredicateRef "cxx1.FileXRefs"2
  getIndex _proxy  = 388
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.fileXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.FileXRefs x k
  getFactKey = Glean.Schema.Cxx1.Types.fileXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.FileXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcSelector where
  type KeyType Glean.Schema.Cxx1.Types.ObjcSelector = [Data.Text.Text]
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcSelector"1
  getIndex _proxy  = 382
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcSelector_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcSelector x k
  getFactKey = Glean.Schema.Cxx1.Types.objcSelector_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcSelector where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.NamespaceDefinition_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.NamespaceDefinition_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.NamespaceDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.NamespaceDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.NamespaceDeclaration) ('Angle.TField "members" (Glean.KeyType Glean.Schema.Cxx1.Types.Declarations) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.NamespaceDefinition where
  type KeyType Glean.Schema.Cxx1.Types.NamespaceDefinition =
    Glean.Schema.Cxx1.Types.NamespaceDefinition_key
  getName _proxy  = Glean.PredicateRef "cxx1.NamespaceDefinition"2
  getIndex _proxy  = 346
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.namespaceDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.NamespaceDefinition x k
  getFactKey = Glean.Schema.Cxx1.Types.namespaceDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.NamespaceDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionAttribute_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionAttribute_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.FunctionAttribute_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.FunctionAttribute_key = 'Angle.TField "attr" (Glean.KeyType Glean.Schema.Cxx1.Types.Attribute) ('Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.FunctionAttribute where
  type KeyType Glean.Schema.Cxx1.Types.FunctionAttribute =
    Glean.Schema.Cxx1.Types.FunctionAttribute_key
  getName _proxy  = Glean.PredicateRef "cxx1.FunctionAttribute"4
  getIndex _proxy  = 342
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.functionAttribute_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.FunctionAttribute x k
  getFactKey = Glean.Schema.Cxx1.Types.functionAttribute_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionAttribute where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.QName) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Cxx1.Types.Type) ('Angle.TField "kind" (Glean.Schema.Cxx1.Types.TypeAliasKind) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Cxx1.Types.TypeAliasDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.TypeAliasDeclaration =
    Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.TypeAliasDeclaration"2
  getIndex _proxy  = 340
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.typeAliasDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.TypeAliasDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.typeAliasDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.TypeAliasDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.FileXRefMap_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.FileXRefMap_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.FileXRefMap_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.FileXRefMap_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "fixed" ([Glean.Schema.Cxx1.Types.FixedXRef]) ('Angle.TField "variable" ([Glean.Schema.Src.Types.ByteSpans]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.FileXRefMap where
  type KeyType Glean.Schema.Cxx1.Types.FileXRefMap =
    Glean.Schema.Cxx1.Types.FileXRefMap_key
  getName _proxy  = Glean.PredicateRef "cxx1.FileXRefMap"2
  getIndex _proxy  = 322
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.fileXRefMap_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.FileXRefMap x k
  getFactKey = Glean.Schema.Cxx1.Types.fileXRefMap_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.FileXRefMap where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.EnumDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.EnumDeclaration_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Cxx1.Types.EnumDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.EnumDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.QName) ('Angle.TField "isScoped" (Prelude.Bool) ('Angle.TField "type" (Prelude.Maybe (Glean.KeyType Glean.Schema.Cxx1.Types.Type)) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Cxx1.Types.EnumDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.EnumDeclaration =
    Glean.Schema.Cxx1.Types.EnumDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.EnumDeclaration"1
  getIndex _proxy  = 317
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.enumDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.EnumDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.enumDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.EnumDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key = 'Angle.TField "id" (Glean.Schema.Cxx1.Types.ObjcContainerId) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcContainerDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration =
    Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcContainerDeclaration"1
  getIndex _proxy  = 314
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcContainerDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcContainerDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.objcContainerDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key x1 x2 x3 x4 x5 x6 x7) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key = 'Angle.TField "selector" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcSelector) ('Angle.TField "container" (Glean.Schema.Cxx1.Types.ObjcContainerId) ('Angle.TField "signature" (Glean.KeyType Glean.Schema.Cxx1.Types.Signature) ('Angle.TField "isInstance" (Prelude.Bool) ('Angle.TField "isOptional" (Prelude.Bool) ('Angle.TField "isAccessor" (Prelude.Bool) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields)))))))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcMethodDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.ObjcMethodDeclaration =
    Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcMethodDeclaration"1
  getIndex _proxy  = 313
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcMethodDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcMethodDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.objcMethodDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcMethodDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.MethodOverrides_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.MethodOverrides_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.MethodOverrides_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.MethodOverrides_key = 'Angle.TField "derived" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TField "base" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.MethodOverrides where
  type KeyType Glean.Schema.Cxx1.Types.MethodOverrides =
    Glean.Schema.Cxx1.Types.MethodOverrides_key
  getName _proxy  = Glean.PredicateRef "cxx1.MethodOverrides"1
  getIndex _proxy  = 292
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.methodOverrides_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.MethodOverrides x k
  getFactKey = Glean.Schema.Cxx1.Types.methodOverrides_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.MethodOverrides where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key = 'Angle.TField "interface_" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TField "implementation" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation where
  type KeyType Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation =
    Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcInterfaceToImplementation"1
  getIndex _proxy  = 275
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcInterfaceToImplementation_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation x k
  getFactKey = Glean.Schema.Cxx1.Types.objcInterfaceToImplementation_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionName_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionName_key_name x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionName_key_operator_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionName_key_literalOperator x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionName_key_constructor x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionName_key_destructor x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionName_key_conversionOperator x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.FunctionName_key_name
    , Glean.mapD Glean.Schema.Cxx1.Types.FunctionName_key_operator_
    , Glean.mapD Glean.Schema.Cxx1.Types.FunctionName_key_literalOperator
    , Glean.mapD Glean.Schema.Cxx1.Types.FunctionName_key_constructor
    , Glean.mapD Glean.Schema.Cxx1.Types.FunctionName_key_destructor
    , Glean.mapD Glean.Schema.Cxx1.Types.FunctionName_key_conversionOperator
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.FunctionName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "operator_" (Glean.Schema.Cxx1.Types.Operator) ('Angle.TField "literalOperator" (Glean.Schema.Cxx1.Types.LiteralOperator) ('Angle.TField "constructor" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "destructor" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "conversionOperator" (Glean.KeyType Glean.Schema.Cxx1.Types.Type) ('Angle.TNoFields))))))

instance Glean.Predicate Glean.Schema.Cxx1.Types.FunctionName where
  type KeyType Glean.Schema.Cxx1.Types.FunctionName =
    Glean.Schema.Cxx1.Types.FunctionName_key
  getName _proxy  = Glean.PredicateRef "cxx1.FunctionName"1
  getIndex _proxy  = 252
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.functionName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.FunctionName x k
  getFactKey = Glean.Schema.Cxx1.Types.functionName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.Same_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Same_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.Same_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.Same_key = 'Angle.TField "declaration1" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "declaration2" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.Same where
  type KeyType Glean.Schema.Cxx1.Types.Same = Glean.Schema.Cxx1.Types.Same_key
  getName _proxy  = Glean.PredicateRef "cxx1.Same"2
  getIndex _proxy  = 238
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.same_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.Same x k
  getFactKey = Glean.Schema.Cxx1.Types.same_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.Same where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionDeclaration_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Cxx1.Types.FunctionDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.FunctionDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionQName) ('Angle.TField "signature" (Glean.KeyType Glean.Schema.Cxx1.Types.Signature) ('Angle.TField "method" (Prelude.Maybe Glean.Schema.Cxx1.Types.MethodSignature) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Cxx1.Types.FunctionDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration =
    Glean.Schema.Cxx1.Types.FunctionDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.FunctionDeclaration"1
  getIndex _proxy  = 222
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.functionDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.FunctionDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.functionDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Cxx1.Types.Name where
  type KeyType Glean.Schema.Cxx1.Types.Name = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "cxx1.Name"1
  getIndex _proxy  = 210
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.name_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.Name x k
  getFactKey = Glean.Schema.Cxx1.Types.name_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.Name where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclImpliesRecord_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclImpliesRecord_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclImpliesRecord_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclImpliesRecord_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "record" (Glean.KeyType Glean.Schema.Cxx1.Types.RecordDefinition) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclImpliesRecord where
  type KeyType Glean.Schema.Cxx1.Types.DeclImpliesRecord =
    Glean.Schema.Cxx1.Types.DeclImpliesRecord_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclImpliesRecord"4
  getIndex _proxy  = 180
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declImpliesRecord_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclImpliesRecord x k
  getFactKey = Glean.Schema.Cxx1.Types.declImpliesRecord_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclImpliesRecord where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionQName_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.FunctionQName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.FunctionQName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.FunctionQName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionName) ('Angle.TField "scope" (Glean.Schema.Cxx1.Types.Scope) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.FunctionQName where
  type KeyType Glean.Schema.Cxx1.Types.FunctionQName =
    Glean.Schema.Cxx1.Types.FunctionQName_key
  getName _proxy  = Glean.PredicateRef "cxx1.FunctionQName"1
  getIndex _proxy  = 168
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.functionQName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.FunctionQName x k
  getFactKey = Glean.Schema.Cxx1.Types.functionQName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.FunctionQName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.TranslationUnitTrace_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.TranslationUnitTrace_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.TranslationUnitTrace_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.TranslationUnitTrace_key = 'Angle.TField "tunit" (Glean.KeyType Glean.Schema.Buck.Types.TranslationUnit) ('Angle.TField "trace" (Glean.KeyType Glean.Schema.Cxx1.Types.Trace) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.TranslationUnitTrace where
  type KeyType Glean.Schema.Cxx1.Types.TranslationUnitTrace =
    Glean.Schema.Cxx1.Types.TranslationUnitTrace_key
  getName _proxy  = Glean.PredicateRef "cxx1.TranslationUnitTrace"3
  getIndex _proxy  = 161
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.translationUnitTrace_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.TranslationUnitTrace x k
  getFactKey = Glean.Schema.Cxx1.Types.translationUnitTrace_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.TranslationUnitTrace where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.Enumerator_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Enumerator_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.Enumerator_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.Enumerator_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "enumeration" (Glean.KeyType Glean.Schema.Cxx1.Types.EnumDeclaration) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.Enumerator where
  type KeyType Glean.Schema.Cxx1.Types.Enumerator =
    Glean.Schema.Cxx1.Types.Enumerator_key
  getName _proxy  = Glean.PredicateRef "cxx1.Enumerator"1
  getIndex _proxy  = 158
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.enumerator_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.Enumerator x k
  getFactKey = Glean.Schema.Cxx1.Types.enumerator_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.Enumerator where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration) ('Angle.TField "kind" (Glean.Schema.Cxx1.Types.ObjcPropertyKind) ('Angle.TField "ivar" (Prelude.Maybe (Glean.KeyType Glean.Schema.Cxx1.Types.Name)) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcPropertyImplementation where
  type KeyType Glean.Schema.Cxx1.Types.ObjcPropertyImplementation =
    Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcPropertyImplementation"1
  getIndex _proxy  = 149
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcPropertyImplementation_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcPropertyImplementation x k
  getFactKey = Glean.Schema.Cxx1.Types.objcPropertyImplementation_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcPropertyImplementation where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.Trace_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Trace_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.Trace_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.Trace_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "declarations" (Glean.KeyType Glean.Schema.Cxx1.Types.Declarations) ('Angle.TField "preprocessor" (Glean.KeyType Glean.Schema.Cxx1.Types.PPTrace) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.Trace where
  type KeyType Glean.Schema.Cxx1.Types.Trace =
    Glean.Schema.Cxx1.Types.Trace_key
  getName _proxy  = Glean.PredicateRef "cxx1.Trace"2
  getIndex _proxy  = 144
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.trace_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.Trace x k
  getFactKey = Glean.Schema.Cxx1.Types.trace_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.Trace where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.MethodOverridden_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.MethodOverridden_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.MethodOverridden_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.MethodOverridden_key = 'Angle.TField "base" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TField "derived" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.MethodOverridden where
  type KeyType Glean.Schema.Cxx1.Types.MethodOverridden =
    Glean.Schema.Cxx1.Types.MethodOverridden_key
  getName _proxy  = Glean.PredicateRef "cxx1.MethodOverridden"4
  getIndex _proxy  = 141
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.methodOverridden_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.MethodOverridden x k
  getFactKey = Glean.Schema.Cxx1.Types.methodOverridden_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.MethodOverridden where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.TargetUses_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.TargetUses_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.TargetUses_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.TargetUses_key = 'Angle.TField "target" (Glean.Schema.Cxx1.Types.XRefTarget) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "uses" (Glean.Schema.Src.Types.ByteSpans) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.TargetUses where
  type KeyType Glean.Schema.Cxx1.Types.TargetUses =
    Glean.Schema.Cxx1.Types.TargetUses_key
  getName _proxy  = Glean.PredicateRef "cxx1.TargetUses"2
  getIndex _proxy  = 133
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.targetUses_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.TargetUses x k
  getFactKey = Glean.Schema.Cxx1.Types.targetUses_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.TargetUses where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.UsingDirective_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.UsingDirective_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.UsingDirective_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.UsingDirective_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.QName) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.UsingDirective where
  type KeyType Glean.Schema.Cxx1.Types.UsingDirective =
    Glean.Schema.Cxx1.Types.UsingDirective_key
  getName _proxy  = Glean.PredicateRef "cxx1.UsingDirective"2
  getIndex _proxy  = 131
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.usingDirective_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.UsingDirective x k
  getFactKey = Glean.Schema.Cxx1.Types.usingDirective_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.UsingDirective where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclFamilyOf_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclFamilyOf_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclFamilyOf_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclFamilyOf_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "family" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclFamilyOf where
  type KeyType Glean.Schema.Cxx1.Types.DeclFamilyOf =
    Glean.Schema.Cxx1.Types.DeclFamilyOf_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclFamilyOf"4
  getIndex _proxy  = 124
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declFamilyOf_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclFamilyOf x k
  getFactKey = Glean.Schema.Cxx1.Types.declFamilyOf_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclFamilyOf where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.PPTrace_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.PPTrace_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.PPTrace_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.PPTrace_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "events" ([Glean.Schema.Cxx1.Types.PPEvent]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.PPTrace where
  type KeyType Glean.Schema.Cxx1.Types.PPTrace =
    Glean.Schema.Cxx1.Types.PPTrace_key
  getName _proxy  = Glean.PredicateRef "cxx1.PPTrace"2
  getIndex _proxy  = 123
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.pPTrace_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.PPTrace x k
  getFactKey = Glean.Schema.Cxx1.Types.pPTrace_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.PPTrace where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.QName_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.QName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.QName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.QName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "scope" (Glean.Schema.Cxx1.Types.Scope) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.QName where
  type KeyType Glean.Schema.Cxx1.Types.QName =
    Glean.Schema.Cxx1.Types.QName_key
  getName _proxy  = Glean.PredicateRef "cxx1.QName"1
  getIndex _proxy  = 113
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.qName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.QName x k
  getFactKey = Glean.Schema.Cxx1.Types.qName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.QName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.VariableDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.VariableDeclaration_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Cxx1.Types.VariableDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.VariableDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.QName) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Cxx1.Types.Type) ('Angle.TField "kind" (Glean.Schema.Cxx1.Types.VariableKind) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Cxx1.Types.VariableDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.VariableDeclaration =
    Glean.Schema.Cxx1.Types.VariableDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.VariableDeclaration"2
  getIndex _proxy  = 108
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.variableDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.VariableDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.variableDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.VariableDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationSources_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclarationSources_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclarationSources_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclarationSources_key = 'Angle.TField "target" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "sources" ([Glean.Schema.Cxx1.Types.Declaration]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclarationSources where
  type KeyType Glean.Schema.Cxx1.Types.DeclarationSources =
    Glean.Schema.Cxx1.Types.DeclarationSources_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclarationSources"1
  getIndex _proxy  = 107
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declarationSources_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclarationSources x k
  getFactKey = Glean.Schema.Cxx1.Types.declarationSources_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationSources where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerBase_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerBase_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcContainerBase_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcContainerBase_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TField "base" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcContainerBase where
  type KeyType Glean.Schema.Cxx1.Types.ObjcContainerBase =
    Glean.Schema.Cxx1.Types.ObjcContainerBase_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcContainerBase"4
  getIndex _proxy  = 106
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcContainerBase_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcContainerBase x k
  getFactKey = Glean.Schema.Cxx1.Types.objcContainerBase_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerBase where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.UsingDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.UsingDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.UsingDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.UsingDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionQName) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.UsingDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.UsingDeclaration =
    Glean.Schema.Cxx1.Types.UsingDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.UsingDeclaration"2
  getIndex _proxy  = 104
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.usingDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.UsingDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.usingDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.UsingDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcMethodDefinition where
  type KeyType Glean.Schema.Cxx1.Types.ObjcMethodDefinition =
    Glean.Schema.Cxx1.Types.ObjcMethodDeclaration
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcMethodDefinition"1
  getIndex _proxy  = 90
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcMethodDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcMethodDefinition x k
  getFactKey = Glean.Schema.Cxx1.Types.objcMethodDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcMethodDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationInTrace_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclarationInTrace_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclarationInTrace_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclarationInTrace_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "trace" (Glean.KeyType Glean.Schema.Cxx1.Types.Trace) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclarationInTrace where
  type KeyType Glean.Schema.Cxx1.Types.DeclarationInTrace =
    Glean.Schema.Cxx1.Types.DeclarationInTrace_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclarationInTrace"4
  getIndex _proxy  = 89
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declarationInTrace_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclarationInTrace x k
  getFactKey = Glean.Schema.Cxx1.Types.declarationInTrace_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationInTrace where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TField "protocols" ([Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration]) ('Angle.TField "members" (Glean.KeyType Glean.Schema.Cxx1.Types.Declarations) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcContainerDefinition where
  type KeyType Glean.Schema.Cxx1.Types.ObjcContainerDefinition =
    Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcContainerDefinition"2
  getIndex _proxy  = 87
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcContainerDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcContainerDefinition x k
  getFactKey = Glean.Schema.Cxx1.Types.objcContainerDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.EnumDefinition_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.EnumDefinition_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.EnumDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.EnumDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.EnumDeclaration) ('Angle.TField "enumerators" ([Glean.KeyType Glean.Schema.Cxx1.Types.Enumerator]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.EnumDefinition where
  type KeyType Glean.Schema.Cxx1.Types.EnumDefinition =
    Glean.Schema.Cxx1.Types.EnumDefinition_key
  getName _proxy  = Glean.PredicateRef "cxx1.EnumDefinition"1
  getIndex _proxy  = 84
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.enumDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.EnumDefinition x k
  getFactKey = Glean.Schema.Cxx1.Types.enumDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.EnumDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Cxx1.Types.Declarations where
  type KeyType Glean.Schema.Cxx1.Types.Declarations =
    [Glean.Schema.Cxx1.Types.Declaration]
  getName _proxy  = Glean.PredicateRef "cxx1.Declarations"2
  getIndex _proxy  = 77
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declarations_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.Declarations x k
  getFactKey = Glean.Schema.Cxx1.Types.declarations_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.Declarations where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclToFamily_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclToFamily_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclToFamily_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclToFamily_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "family" (Glean.KeyType Glean.Schema.Cxx1.Types.DeclFamily) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclToFamily where
  type KeyType Glean.Schema.Cxx1.Types.DeclToFamily =
    Glean.Schema.Cxx1.Types.DeclToFamily_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclToFamily"2
  getIndex _proxy  = 64
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declToFamily_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclToFamily x k
  getFactKey = Glean.Schema.Cxx1.Types.declToFamily_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclToFamily where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.NamespaceDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.NamespaceDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.NamespaceDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.NamespaceDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.NamespaceQName) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.NamespaceDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.NamespaceDeclaration =
    Glean.Schema.Cxx1.Types.NamespaceDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.NamespaceDeclaration"1
  getIndex _proxy  = 61
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.namespaceDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.NamespaceDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.namespaceDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.NamespaceDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationComment_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclarationComment_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.DeclarationComment_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.DeclarationComment_key = 'Angle.TField "declaration" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclarationComment where
  type KeyType Glean.Schema.Cxx1.Types.DeclarationComment =
    Glean.Schema.Cxx1.Types.DeclarationComment_key
  getName _proxy  = Glean.PredicateRef "cxx1.DeclarationComment"1
  getIndex _proxy  = 57
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declarationComment_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclarationComment x k
  getFactKey = Glean.Schema.Cxx1.Types.declarationComment_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclarationComment where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key x1 x2 x3 x4 x5 x6 x7 x8) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "container" (Glean.Schema.Cxx1.Types.ObjcContainerId) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Cxx1.Types.Type) ('Angle.TField "isInstance" (Prelude.Bool) ('Angle.TField "isOptional" (Prelude.Bool) ('Angle.TField "isReadOnly" (Prelude.Bool) ('Angle.TField "isAtomic" (Prelude.Bool) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))))))))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration where
  type KeyType Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration =
    Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcPropertyDeclaration"1
  getIndex _proxy  = 55
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcPropertyDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration x k
  getFactKey = Glean.Schema.Cxx1.Types.objcPropertyDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.RecordDefinition_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.RecordDefinition_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.RecordDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.RecordDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Cxx1.Types.RecordDeclaration) ('Angle.TField "bases" ([Glean.Schema.Cxx1.Types.RecordBase]) ('Angle.TField "members" (Glean.KeyType Glean.Schema.Cxx1.Types.Declarations) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Cxx1.Types.RecordDefinition where
  type KeyType Glean.Schema.Cxx1.Types.RecordDefinition =
    Glean.Schema.Cxx1.Types.RecordDefinition_key
  getName _proxy  = Glean.PredicateRef "cxx1.RecordDefinition"2
  getIndex _proxy  = 46
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.recordDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.RecordDefinition x k
  getFactKey = Glean.Schema.Cxx1.Types.recordDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.RecordDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcImplements_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcImplements_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcImplements_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcImplements_key = 'Angle.TField "implementation" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TField "interface_" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.ObjcImplements where
  type KeyType Glean.Schema.Cxx1.Types.ObjcImplements =
    Glean.Schema.Cxx1.Types.ObjcImplements_key
  getName _proxy  = Glean.PredicateRef "cxx1.ObjcImplements"1
  getIndex _proxy  = 39
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.objcImplements_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.ObjcImplements x k
  getFactKey = Glean.Schema.Cxx1.Types.objcImplements_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcImplements where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.EnumeratorInEnum_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.EnumeratorInEnum_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.EnumeratorInEnum_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.EnumeratorInEnum_key = 'Angle.TField "enumerator" (Glean.KeyType Glean.Schema.Cxx1.Types.Enumerator) ('Angle.TField "enum_" (Glean.KeyType Glean.Schema.Cxx1.Types.EnumDefinition) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.EnumeratorInEnum where
  type KeyType Glean.Schema.Cxx1.Types.EnumeratorInEnum =
    Glean.Schema.Cxx1.Types.EnumeratorInEnum_key
  getName _proxy  = Glean.PredicateRef "cxx1.EnumeratorInEnum"4
  getIndex _proxy  = 37
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.enumeratorInEnum_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.EnumeratorInEnum x k
  getFactKey = Glean.Schema.Cxx1.Types.enumeratorInEnum_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.EnumeratorInEnum where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Cxx1.Types.DeclFamily where
  type KeyType Glean.Schema.Cxx1.Types.DeclFamily =
    [Glean.Schema.Cxx1.Types.Declaration]
  getName _proxy  = Glean.PredicateRef "cxx1.DeclFamily"2
  getIndex _proxy  = 13
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.declFamily_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.DeclFamily x k
  getFactKey = Glean.Schema.Cxx1.Types.declFamily_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.DeclFamily where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Cxx1.Types.Type where
  type KeyType Glean.Schema.Cxx1.Types.Type = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "cxx1.Type"1
  getIndex _proxy  = 9
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.type_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.Type x k
  getFactKey = Glean.Schema.Cxx1.Types.type_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.Type where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.RecordDerived_key where
  buildRtsValue b (Glean.Schema.Cxx1.Types.RecordDerived_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.RecordDerived_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.RecordDerived_key = 'Angle.TField "base" (Glean.KeyType Glean.Schema.Cxx1.Types.RecordDeclaration) ('Angle.TField "derived" (Glean.KeyType Glean.Schema.Cxx1.Types.RecordDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Cxx1.Types.RecordDerived where
  type KeyType Glean.Schema.Cxx1.Types.RecordDerived =
    Glean.Schema.Cxx1.Types.RecordDerived_key
  getName _proxy  = Glean.PredicateRef "cxx1.RecordDerived"4
  getIndex _proxy  = 2
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Cxx1.Types.recordDerived_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Cxx1.Types.RecordDerived x k
  getFactKey = Glean.Schema.Cxx1.Types.recordDerived_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Cxx1.Types.RecordDerived where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Cxx1.Types.GlobalVariable where
  buildRtsValue b (Glean.Schema.Cxx1.Types.GlobalVariable x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.GlobalVariable
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.GlobalVariable = 'Angle.TField "kind" (Glean.Schema.Cxx1.Types.GlobalVariableKind) ('Angle.TField "attribute" (Glean.Schema.Cxx1.Types.GlobalVariableAttribute) ('Angle.TField "definition" (Prelude.Bool) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Cxx1.Types.GlobalVariableKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Cxx1.Types.GlobalVariableKind = 'Angle.TField "SimpleVariable" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "StaticVariable" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "StaticMember" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Cxx1.Types.Scope where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Scope_global_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Scope_namespace_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Scope_recordWithAccess x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Scope_local x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.Scope_global_
    , Glean.mapD Glean.Schema.Cxx1.Types.Scope_namespace_
    , Glean.mapD Glean.Schema.Cxx1.Types.Scope_recordWithAccess
    , Glean.mapD Glean.Schema.Cxx1.Types.Scope_local
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.Scope = 'Angle.TField "global_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "namespace_" (Glean.KeyType Glean.Schema.Cxx1.Types.NamespaceQName) ('Angle.TField "recordWithAccess" (Glean.Schema.Cxx1.Types.Scope_recordWithAccess_) ('Angle.TField "local" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionQName) ('Angle.TNoFields))))

instance Glean.SumBranches Glean.Schema.Builtin.Types.Unit Glean.Schema.Cxx1.Types.Scope where
  injectBranch = Glean.Schema.Cxx1.Types.Scope_global_
  projectBranch (Glean.Schema.Cxx1.Types.Scope_global_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.NamespaceQName Glean.Schema.Cxx1.Types.Scope where
  injectBranch = Glean.Schema.Cxx1.Types.Scope_namespace_
  projectBranch (Glean.Schema.Cxx1.Types.Scope_namespace_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.Scope_recordWithAccess_ Glean.Schema.Cxx1.Types.Scope where
  injectBranch = Glean.Schema.Cxx1.Types.Scope_recordWithAccess
  projectBranch (Glean.Schema.Cxx1.Types.Scope_recordWithAccess x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.FunctionQName Glean.Schema.Cxx1.Types.Scope where
  injectBranch = Glean.Schema.Cxx1.Types.Scope_local
  projectBranch (Glean.Schema.Cxx1.Types.Scope_local x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Cxx1.Types.FixedXRef where
  buildRtsValue b (Glean.Schema.Cxx1.Types.FixedXRef x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.FixedXRef
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.FixedXRef = 'Angle.TField "target" (Glean.Schema.Cxx1.Types.XRefTarget) ('Angle.TField "ranges" (Glean.Schema.Src.Types.ByteSpans) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Cxx1.Types.PPEvent where
  buildRtsValue b (Glean.Schema.Cxx1.Types.PPEvent_include_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.PPEvent_define x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.PPEvent_undef x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.PPEvent_use x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.PPEvent_include_
    , Glean.mapD Glean.Schema.Cxx1.Types.PPEvent_define
    , Glean.mapD Glean.Schema.Cxx1.Types.PPEvent_undef
    , Glean.mapD Glean.Schema.Cxx1.Types.PPEvent_use
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.PPEvent = 'Angle.TField "include_" (Glean.Schema.Cxx1.Types.IncludeTrace) ('Angle.TField "define" (Glean.KeyType Glean.Schema.Pp1.Types.Define) ('Angle.TField "undef" (Glean.KeyType Glean.Schema.Pp1.Types.Undef) ('Angle.TField "use" (Glean.KeyType Glean.Schema.Pp1.Types.Use) ('Angle.TNoFields))))

instance Glean.SumBranches Glean.Schema.Cxx1.Types.IncludeTrace Glean.Schema.Cxx1.Types.PPEvent where
  injectBranch = Glean.Schema.Cxx1.Types.PPEvent_include_
  projectBranch (Glean.Schema.Cxx1.Types.PPEvent_include_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Pp1.Types.Define Glean.Schema.Cxx1.Types.PPEvent where
  injectBranch = Glean.Schema.Cxx1.Types.PPEvent_define
  projectBranch (Glean.Schema.Cxx1.Types.PPEvent_define x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Pp1.Types.Undef Glean.Schema.Cxx1.Types.PPEvent where
  injectBranch = Glean.Schema.Cxx1.Types.PPEvent_undef
  projectBranch (Glean.Schema.Cxx1.Types.PPEvent_undef x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Pp1.Types.Use Glean.Schema.Cxx1.Types.PPEvent where
  injectBranch = Glean.Schema.Cxx1.Types.PPEvent_use
  projectBranch (Glean.Schema.Cxx1.Types.PPEvent_use x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Cxx1.Types.Access where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Cxx1.Types.Access = 'Angle.TField "Public" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Protected" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Private" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Cxx1.Types.TypeAliasKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Cxx1.Types.TypeAliasKind = 'Angle.TField "Typedef" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Using" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcContainerId where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerId_protocol x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerId_interface_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerId_categoryInterface x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerId_extensionInterface x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerId_implementation x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcContainerId_categoryImplementation x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.ObjcContainerId_protocol
    , Glean.mapD Glean.Schema.Cxx1.Types.ObjcContainerId_interface_
    , Glean.mapD Glean.Schema.Cxx1.Types.ObjcContainerId_categoryInterface
    , Glean.mapD Glean.Schema.Cxx1.Types.ObjcContainerId_extensionInterface
    , Glean.mapD Glean.Schema.Cxx1.Types.ObjcContainerId_implementation
    , Glean.mapD Glean.Schema.Cxx1.Types.ObjcContainerId_categoryImplementation
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.ObjcContainerId = 'Angle.TField "protocol" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "interface_" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "categoryInterface" (Glean.Schema.Cxx1.Types.ObjcCategoryId) ('Angle.TField "extensionInterface" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "implementation" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "categoryImplementation" (Glean.Schema.Cxx1.Types.ObjcCategoryId) ('Angle.TNoFields))))))

instance Glean.Type Glean.Schema.Cxx1.Types.DeclKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Cxx1.Types.DeclKind = 'Angle.TField "namespace_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "usingDeclaration" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "usingDirective" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "record_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "enum_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "enumerator" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "function_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "variable" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "objcContainer" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "objcMethod" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "objcProperty" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "typeAlias" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "macro" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))))))))))))

instance Glean.Type Glean.Schema.Cxx1.Types.MethodSignature where
  buildRtsValue b (Glean.Schema.Cxx1.Types.MethodSignature x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Cxx1.Types.MethodSignature
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.MethodSignature = 'Angle.TField "isVirtual" (Prelude.Bool) ('Angle.TField "isConst" (Prelude.Bool) ('Angle.TField "isVolatile" (Prelude.Bool) ('Angle.TField "refQualifier" (Glean.Schema.Cxx1.Types.RefQualifier) ('Angle.TNoFields))))

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcPropertyKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Cxx1.Types.ObjcPropertyKind = 'Angle.TField "Synthesize" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Dynamic" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Cxx1.Types.Declaration where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_namespace_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_usingDeclaration x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_usingDirective x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_record_ x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_enum_ x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_function_ x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_variable x) = do
    Glean.buildRtsSelector b 6
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_objcContainer x) = do
    Glean.buildRtsSelector b 7
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_objcMethod x) = do
    Glean.buildRtsSelector b 8
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_objcProperty x) = do
    Glean.buildRtsSelector b 9
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.Declaration_typeAlias x) = do
    Glean.buildRtsSelector b 10
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.Declaration_namespace_
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_usingDeclaration
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_usingDirective
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_record_
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_enum_
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_function_
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_variable
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_objcContainer
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_objcMethod
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_objcProperty
    , Glean.mapD Glean.Schema.Cxx1.Types.Declaration_typeAlias
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.Declaration = 'Angle.TField "namespace_" (Glean.KeyType Glean.Schema.Cxx1.Types.NamespaceDeclaration) ('Angle.TField "usingDeclaration" (Glean.KeyType Glean.Schema.Cxx1.Types.UsingDeclaration) ('Angle.TField "usingDirective" (Glean.KeyType Glean.Schema.Cxx1.Types.UsingDirective) ('Angle.TField "record_" (Glean.KeyType Glean.Schema.Cxx1.Types.RecordDeclaration) ('Angle.TField "enum_" (Glean.KeyType Glean.Schema.Cxx1.Types.EnumDeclaration) ('Angle.TField "function_" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDeclaration) ('Angle.TField "variable" (Glean.KeyType Glean.Schema.Cxx1.Types.VariableDeclaration) ('Angle.TField "objcContainer" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDeclaration) ('Angle.TField "objcMethod" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcMethodDeclaration) ('Angle.TField "objcProperty" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration) ('Angle.TField "typeAlias" (Glean.KeyType Glean.Schema.Cxx1.Types.TypeAliasDeclaration) ('Angle.TNoFields)))))))))))

instance Glean.SumBranches Glean.Schema.Cxx1.Types.NamespaceDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_namespace_
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_namespace_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.UsingDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_usingDeclaration
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_usingDeclaration x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.UsingDirective Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_usingDirective
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_usingDirective x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.RecordDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_record_
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_record_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.EnumDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_enum_
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_enum_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.FunctionDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_function_
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_function_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.VariableDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_variable
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_variable x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.ObjcContainerDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_objcContainer
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_objcContainer x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.ObjcMethodDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_objcMethod
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_objcMethod x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_objcProperty
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_objcProperty x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.TypeAliasDeclaration Glean.Schema.Cxx1.Types.Declaration where
  injectBranch = Glean.Schema.Cxx1.Types.Declaration_typeAlias
  projectBranch (Glean.Schema.Cxx1.Types.Declaration_typeAlias x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcIVar where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcIVar x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcIVar
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcIVar = 'Angle.TField "synthesize" (Prelude.Bool) ('Angle.TField "bitsize" (Prelude.Maybe Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Cxx1.Types.RecordKind where
  buildRtsValue b (Glean.Schema.Cxx1.Types.RecordKind_struct_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.RecordKind_class_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.RecordKind_union_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.RecordKind_struct_
    , Glean.mapD Glean.Schema.Cxx1.Types.RecordKind_class_
    , Glean.mapD Glean.Schema.Cxx1.Types.RecordKind_union_
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.RecordKind = 'Angle.TField "struct_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "class_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "union_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Cxx1.Types.IncludeTrace where
  buildRtsValue b (Glean.Schema.Cxx1.Types.IncludeTrace x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.IncludeTrace
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.IncludeTrace = 'Angle.TField "include_" (Glean.KeyType Glean.Schema.Pp1.Types.Include) ('Angle.TField "trace" (Prelude.Maybe (Glean.KeyType Glean.Schema.Cxx1.Types.Trace)) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Cxx1.Types.Field where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Field x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.Field
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.Field = 'Angle.TField "mutable_" (Prelude.Bool) ('Angle.TField "bitsize" (Prelude.Maybe Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Cxx1.Types.XRefVia where
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefVia_usingDeclaration x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefVia_usingDirective x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefVia_macro x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.XRefVia_usingDeclaration
    , Glean.mapD Glean.Schema.Cxx1.Types.XRefVia_usingDirective
    , Glean.mapD Glean.Schema.Cxx1.Types.XRefVia_macro
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.XRefVia = 'Angle.TField "usingDeclaration" (Glean.KeyType Glean.Schema.Cxx1.Types.UsingDeclaration) ('Angle.TField "usingDirective" (Glean.KeyType Glean.Schema.Cxx1.Types.UsingDirective) ('Angle.TField "macro" (Glean.KeyType Glean.Schema.Pp1.Types.Use) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Cxx1.Types.UsingDeclaration Glean.Schema.Cxx1.Types.XRefVia where
  injectBranch = Glean.Schema.Cxx1.Types.XRefVia_usingDeclaration
  projectBranch (Glean.Schema.Cxx1.Types.XRefVia_usingDeclaration x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.UsingDirective Glean.Schema.Cxx1.Types.XRefVia where
  injectBranch = Glean.Schema.Cxx1.Types.XRefVia_usingDirective
  projectBranch (Glean.Schema.Cxx1.Types.XRefVia_usingDirective x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Pp1.Types.Use Glean.Schema.Cxx1.Types.XRefVia where
  injectBranch = Glean.Schema.Cxx1.Types.XRefVia_macro
  projectBranch (Glean.Schema.Cxx1.Types.XRefVia_macro x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Cxx1.Types.DeclIdent where
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclIdent_name x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclIdent_macro x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.DeclIdent_selector x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.DeclIdent_name
    , Glean.mapD Glean.Schema.Cxx1.Types.DeclIdent_macro
    , Glean.mapD Glean.Schema.Cxx1.Types.DeclIdent_selector
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.DeclIdent = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "macro" (Glean.KeyType Glean.Schema.Pp1.Types.Macro) ('Angle.TField "selector" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcSelector) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Cxx1.Types.Name Glean.Schema.Cxx1.Types.DeclIdent where
  injectBranch = Glean.Schema.Cxx1.Types.DeclIdent_name
  projectBranch (Glean.Schema.Cxx1.Types.DeclIdent_name x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Pp1.Types.Macro Glean.Schema.Cxx1.Types.DeclIdent where
  injectBranch = Glean.Schema.Cxx1.Types.DeclIdent_macro
  projectBranch (Glean.Schema.Cxx1.Types.DeclIdent_macro x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.ObjcSelector Glean.Schema.Cxx1.Types.DeclIdent where
  injectBranch = Glean.Schema.Cxx1.Types.DeclIdent_selector
  projectBranch (Glean.Schema.Cxx1.Types.DeclIdent_selector x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Cxx1.Types.GlobalVariableAttribute where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Cxx1.Types.GlobalVariableAttribute = 'Angle.TField "Plain" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Inline" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "Constexpr" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Cxx1.Types.ObjcCategoryId where
  buildRtsValue b (Glean.Schema.Cxx1.Types.ObjcCategoryId x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.ObjcCategoryId
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.ObjcCategoryId = 'Angle.TField "className" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "categoryName" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Cxx1.Types.Parameter where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Parameter x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.Parameter
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.Parameter = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Cxx1.Types.Name) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Cxx1.Types.Type) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Cxx1.Types.RefQualifier where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Cxx1.Types.RefQualifier = 'Angle.TField "None_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "LValue" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "RValue" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Cxx1.Types.VariableKind where
  buildRtsValue b (Glean.Schema.Cxx1.Types.VariableKind_global_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.VariableKind_field x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.VariableKind_ivar x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.VariableKind_global_
    , Glean.mapD Glean.Schema.Cxx1.Types.VariableKind_field
    , Glean.mapD Glean.Schema.Cxx1.Types.VariableKind_ivar
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.VariableKind = 'Angle.TField "global_" (Glean.Schema.Cxx1.Types.GlobalVariable) ('Angle.TField "field" (Glean.Schema.Cxx1.Types.Field) ('Angle.TField "ivar" (Glean.Schema.Cxx1.Types.ObjcIVar) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Cxx1.Types.GlobalVariable Glean.Schema.Cxx1.Types.VariableKind where
  injectBranch = Glean.Schema.Cxx1.Types.VariableKind_global_
  projectBranch (Glean.Schema.Cxx1.Types.VariableKind_global_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.Field Glean.Schema.Cxx1.Types.VariableKind where
  injectBranch = Glean.Schema.Cxx1.Types.VariableKind_field
  projectBranch (Glean.Schema.Cxx1.Types.VariableKind_field x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.ObjcIVar Glean.Schema.Cxx1.Types.VariableKind where
  injectBranch = Glean.Schema.Cxx1.Types.VariableKind_ivar
  projectBranch (Glean.Schema.Cxx1.Types.VariableKind_ivar x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Cxx1.Types.XRefTarget where
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefTarget_declaration x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefTarget_enumerator x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefTarget_objcSelector x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefTarget_unknown x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Cxx1.Types.XRefTarget_indirect x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Cxx1.Types.XRefTarget_declaration
    , Glean.mapD Glean.Schema.Cxx1.Types.XRefTarget_enumerator
    , Glean.mapD Glean.Schema.Cxx1.Types.XRefTarget_objcSelector
    , Glean.mapD Glean.Schema.Cxx1.Types.XRefTarget_unknown
    , Glean.mapD Glean.Schema.Cxx1.Types.XRefTarget_indirect
    ]

type instance Angle.SumFields Glean.Schema.Cxx1.Types.XRefTarget = 'Angle.TField "declaration" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "enumerator" (Glean.KeyType Glean.Schema.Cxx1.Types.Enumerator) ('Angle.TField "objcSelector" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcSelector) ('Angle.TField "unknown" (Glean.Schema.Src.Types.Loc) ('Angle.TField "indirect" (Glean.KeyType Glean.Schema.Cxx1.Types.XRefIndirectTarget) ('Angle.TNoFields)))))

instance Glean.SumBranches Glean.Schema.Cxx1.Types.Declaration Glean.Schema.Cxx1.Types.XRefTarget where
  injectBranch = Glean.Schema.Cxx1.Types.XRefTarget_declaration
  projectBranch (Glean.Schema.Cxx1.Types.XRefTarget_declaration x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.Enumerator Glean.Schema.Cxx1.Types.XRefTarget where
  injectBranch = Glean.Schema.Cxx1.Types.XRefTarget_enumerator
  projectBranch (Glean.Schema.Cxx1.Types.XRefTarget_enumerator x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.ObjcSelector Glean.Schema.Cxx1.Types.XRefTarget where
  injectBranch = Glean.Schema.Cxx1.Types.XRefTarget_objcSelector
  projectBranch (Glean.Schema.Cxx1.Types.XRefTarget_objcSelector x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Src.Types.Loc Glean.Schema.Cxx1.Types.XRefTarget where
  injectBranch = Glean.Schema.Cxx1.Types.XRefTarget_unknown
  projectBranch (Glean.Schema.Cxx1.Types.XRefTarget_unknown x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.XRefIndirectTarget Glean.Schema.Cxx1.Types.XRefTarget where
  injectBranch = Glean.Schema.Cxx1.Types.XRefTarget_indirect
  projectBranch (Glean.Schema.Cxx1.Types.XRefTarget_indirect x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Cxx1.Types.RecordBase where
  buildRtsValue b (Glean.Schema.Cxx1.Types.RecordBase x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Cxx1.Types.RecordBase
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.RecordBase = 'Angle.TField "base" (Glean.KeyType Glean.Schema.Cxx1.Types.RecordDeclaration) ('Angle.TField "access" (Glean.Schema.Cxx1.Types.Access) ('Angle.TField "isVirtual" (Prelude.Bool) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Cxx1.Types.Scope_recordWithAccess_ where
  buildRtsValue b (Glean.Schema.Cxx1.Types.Scope_recordWithAccess_ x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Cxx1.Types.Scope_recordWithAccess_
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Cxx1.Types.Scope_recordWithAccess_ = 'Angle.TField "record" (Glean.KeyType Glean.Schema.Cxx1.Types.QName) ('Angle.TField "access" (Glean.Schema.Cxx1.Types.Access) ('Angle.TNoFields))
