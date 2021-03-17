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
import qualified Glean.Schema.Code.Types
import qualified Glean.Schema.CodeFlow.Types
import qualified Glean.Schema.Flow.Types
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key = 'Angle.TField "local" (Glean.KeyType Glean.Schema.Flow.Types.Declaration) ('Angle.TField "entity" (Glean.Schema.CodeFlow.Types.Entity) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef where
  type KeyType Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef =
    Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowModuleNamespaceXRef"10
  getIndex _proxy  = 479
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowModuleNamespaceXRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowModuleNamespaceXRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.HackEntityUses_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.HackEntityUses_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.HackEntityUses_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.HackEntityUses_key = 'Angle.TField "target" (Glean.Schema.Code.Types.Entity) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.HackEntityUses where
  type KeyType Glean.Schema.Codemarkup.Types.HackEntityUses =
    Glean.Schema.Codemarkup.Types.HackEntityUses_key
  getName _proxy  = Glean.PredicateRef "codemarkup.HackEntityUses"10
  getIndex _proxy  = 476
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.hackEntityUses_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.HackEntityUses x k
  getFactKey = Glean.Schema.Codemarkup.Types.hackEntityUses_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.HackEntityUses where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FileDeclarations_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FileDeclarations_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FileDeclarations_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FileDeclarations_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "decl" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FileDeclarations where
  type KeyType Glean.Schema.Codemarkup.Types.FileDeclarations =
    Glean.Schema.Codemarkup.Types.FileDeclarations_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FileDeclarations"10
  getIndex _proxy  = 470
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.fileDeclarations_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FileDeclarations x k
  getFactKey = Glean.Schema.Codemarkup.Types.fileDeclarations_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FileDeclarations where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs =
    Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowFileEntityXRefs"10
  getIndex _proxy  = 467
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowFileEntityXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowFileEntityXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs =
    Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.HaskellFileDirectXRefs"10
  getIndex _proxy  = 449
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.haskellFileDirectXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.haskellFileDirectXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs =
    Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.PythonFileDirectXRefs"10
  getIndex _proxy  = 447
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.pythonFileDirectXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.pythonFileDirectXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.PythonResolve_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.PythonResolve_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.PythonResolve_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.PythonResolve_key = 'Angle.TField "decl" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.PythonResolve where
  type KeyType Glean.Schema.Codemarkup.Types.PythonResolve =
    Glean.Schema.Codemarkup.Types.PythonResolve_key
  getName _proxy  = Glean.PredicateRef "codemarkup.PythonResolve"10
  getIndex _proxy  = 432
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.pythonResolve_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.PythonResolve x k
  getFactKey = Glean.Schema.Codemarkup.Types.pythonResolve_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.PythonResolve where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.EntityToDeclaration_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.EntityToDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.EntityToDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.EntityToDeclaration_key = 'Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TField "decl" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.EntityToDeclaration where
  type KeyType Glean.Schema.Codemarkup.Types.EntityToDeclaration =
    Glean.Schema.Codemarkup.Types.EntityToDeclaration_key
  getName _proxy  = Glean.PredicateRef "codemarkup.EntityToDeclaration"10
  getIndex _proxy  = 419
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.entityToDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.EntityToDeclaration x k
  getFactKey = Glean.Schema.Codemarkup.Types.entityToDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.EntityToDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FileEntities_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FileEntities_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FileEntities_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FileEntities_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "decl" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FileEntities where
  type KeyType Glean.Schema.Codemarkup.Types.FileEntities =
    Glean.Schema.Codemarkup.Types.FileEntities_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FileEntities"10
  getIndex _proxy  = 400
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.fileEntities_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FileEntities x k
  getFactKey = Glean.Schema.Codemarkup.Types.fileEntities_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FileEntities where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key = 'Angle.TField "ref" (Glean.Schema.Flow.Types.XRef) ('Angle.TField "srcLoc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "targetLoc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TField "entity" (Glean.Schema.Flow.Types.SomeDeclaration) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo where
  type KeyType Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo =
    Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowXRefDeclInfo"10
  getIndex _proxy  = 397
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowXRefDeclInfo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowXRefDeclInfo_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowResolve_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowResolve_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowResolve_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowResolve_key = 'Angle.TField "decl" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowResolve where
  type KeyType Glean.Schema.Codemarkup.Types.FlowResolve =
    Glean.Schema.Codemarkup.Types.FlowResolve_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowResolve"10
  getIndex _proxy  = 366
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowResolve_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowResolve x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowResolve_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowResolve where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key = 'Angle.TField "local" (Glean.KeyType Glean.Schema.Flow.Types.TypeDeclaration) ('Angle.TField "entity" (Glean.Schema.CodeFlow.Types.Entity) ('Angle.TField "targetFile" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "targetSpan" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowTypeImportXRef where
  type KeyType Glean.Schema.Codemarkup.Types.FlowTypeImportXRef =
    Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowTypeImportXRef"10
  getIndex _proxy  = 360
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowTypeImportXRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowTypeImportXRef x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowTypeImportXRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowTypeImportXRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.Resolve_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.Resolve_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.Resolve_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.Resolve_key = 'Angle.TField "decl" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.Resolve where
  type KeyType Glean.Schema.Codemarkup.Types.Resolve =
    Glean.Schema.Codemarkup.Types.Resolve_key
  getName _proxy  = Glean.PredicateRef "codemarkup.Resolve"10
  getIndex _proxy  = 315
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.resolve_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.Resolve x k
  getFactKey = Glean.Schema.Codemarkup.Types.resolve_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.Resolve where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.HackFileDeclarations_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.HackFileDeclarations_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.HackFileDeclarations_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.HackFileDeclarations_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "declaration" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.HackFileDeclarations where
  type KeyType Glean.Schema.Codemarkup.Types.HackFileDeclarations =
    Glean.Schema.Codemarkup.Types.HackFileDeclarations_key
  getName _proxy  = Glean.PredicateRef "codemarkup.HackFileDeclarations"10
  getIndex _proxy  = 310
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.hackFileDeclarations_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.HackFileDeclarations x k
  getFactKey = Glean.Schema.Codemarkup.Types.hackFileDeclarations_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.HackFileDeclarations where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key = 'Angle.TField "left" (Glean.KeyType Glean.Schema.Flow.Types.ModuleExport) ('Angle.TField "right" (Glean.KeyType Glean.Schema.Flow.Types.ModuleExport) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport where
  type KeyType Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport =
    Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key
  getName _proxy  =
    Glean.PredicateRef "codemarkup.FlowCompatibleModuleExport"10
  getIndex _proxy  = 257
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowCompatibleModuleExport_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowCompatibleModuleExport_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowSameModule_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowSameModule_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowSameModule_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowSameModule_key = 'Angle.TField "left" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TField "right" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowSameModule where
  type KeyType Glean.Schema.Codemarkup.Types.FlowSameModule =
    Glean.Schema.Codemarkup.Types.FlowSameModule_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowSameModule"10
  getIndex _proxy  = 246
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowSameModule_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowSameModule x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowSameModule_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowSameModule where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FileAnnotations_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FileAnnotations_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FileAnnotations_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FileAnnotations_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "annotation" (Glean.Schema.Codemarkup.Types.Annotation) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FileAnnotations where
  type KeyType Glean.Schema.Codemarkup.Types.FileAnnotations =
    Glean.Schema.Codemarkup.Types.FileAnnotations_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FileAnnotations"10
  getIndex _proxy  = 243
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.fileAnnotations_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FileAnnotations x k
  getFactKey = Glean.Schema.Codemarkup.Types.fileAnnotations_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FileAnnotations where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TField "entity" (Glean.Schema.CodeFlow.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef where
  type KeyType Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef =
    Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef_key
  getName _proxy  =
    Glean.PredicateRef "codemarkup.FlowFileReferenceEntityXRef"10
  getIndex _proxy  = 234
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowFileReferenceEntityXRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowFileReferenceEntityXRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileReferenceEntityXRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowXRefInfo_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowXRefInfo_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowXRefInfo_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowXRefInfo_key = 'Angle.TField "ref" (Glean.Schema.Flow.Types.XRef) ('Angle.TField "srcLoc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "targetLoc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowXRefInfo where
  type KeyType Glean.Schema.Codemarkup.Types.FlowXRefInfo =
    Glean.Schema.Codemarkup.Types.FlowXRefInfo_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowXRefInfo"10
  getIndex _proxy  = 225
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowXRefInfo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowXRefInfo x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowXRefInfo_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowXRefInfo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TField "entity" (Glean.Schema.CodeFlow.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef where
  type KeyType Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef =
    Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef_key
  getName _proxy  =
    Glean.PredicateRef "codemarkup.FlowFileImportDeclEntityXRef"10
  getIndex _proxy  = 224
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowFileImportDeclEntityXRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowFileImportDeclEntityXRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileImportDeclEntityXRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.HackResolve_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.HackResolve_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.HackResolve_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.HackResolve_key = 'Angle.TField "decl" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.HackResolve where
  type KeyType Glean.Schema.Codemarkup.Types.HackResolve =
    Glean.Schema.Codemarkup.Types.HackResolve_key
  getName _proxy  = Glean.PredicateRef "codemarkup.HackResolve"10
  getIndex _proxy  = 223
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.hackResolve_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.HackResolve x k
  getFactKey = Glean.Schema.Codemarkup.Types.hackResolve_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.HackResolve where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.HackFileDirectXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.HackFileDirectXRefs =
    Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.HackFileDirectXRefs"10
  getIndex _proxy  = 198
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.hackFileDirectXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.HackFileDirectXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.hackFileDirectXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.HackFileDirectXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.EntityUses_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.EntityUses_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.EntityUses_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.EntityUses_key = 'Angle.TField "target" (Glean.Schema.Code.Types.Entity) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.EntityUses where
  type KeyType Glean.Schema.Codemarkup.Types.EntityUses =
    Glean.Schema.Codemarkup.Types.EntityUses_key
  getName _proxy  = Glean.PredicateRef "codemarkup.EntityUses"10
  getIndex _proxy  = 173
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.entityUses_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.EntityUses x k
  getFactKey = Glean.Schema.Codemarkup.Types.entityUses_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.EntityUses where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs =
    Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.PythonFileEntityXRefs"10
  getIndex _proxy  = 170
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.pythonFileEntityXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.pythonFileEntityXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key = 'Angle.TField "moduleTypeExport" (Glean.KeyType Glean.Schema.Flow.Types.ModuleTypeExport) ('Angle.TField "entity" (Glean.Schema.CodeFlow.Types.Entity) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowTypeExportLocation where
  type KeyType Glean.Schema.Codemarkup.Types.FlowTypeExportLocation =
    Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowTypeExportLocation"10
  getIndex _proxy  = 160
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowTypeExportLocation_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowTypeExportLocation x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowTypeExportLocation_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowTypeExportLocation where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs =
    Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowFileDirectXRefs"10
  getIndex _proxy  = 154
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowFileDirectXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowFileDirectXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.HackFileEntityXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.HackFileEntityXRefs =
    Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.HackFileEntityXRefs"10
  getIndex _proxy  = 138
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.hackFileEntityXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.HackFileEntityXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.hackFileEntityXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.HackFileEntityXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowImportXRef_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowImportXRef_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowImportXRef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowImportXRef_key = 'Angle.TField "local" (Glean.KeyType Glean.Schema.Flow.Types.Declaration) ('Angle.TField "entity" (Glean.Schema.CodeFlow.Types.Entity) ('Angle.TField "targetFile" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "targetSpan" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowImportXRef where
  type KeyType Glean.Schema.Codemarkup.Types.FlowImportXRef =
    Glean.Schema.Codemarkup.Types.FlowImportXRef_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowImportXRef"10
  getIndex _proxy  = 118
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowImportXRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowImportXRef x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowImportXRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowImportXRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key = 'Angle.TField "local" (Glean.KeyType Glean.Schema.Flow.Types.ModuleExport) ('Angle.TField "entity" (Glean.Schema.CodeFlow.Types.Entity) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowModuleExportLocation where
  type KeyType Glean.Schema.Codemarkup.Types.FlowModuleExportLocation =
    Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowModuleExportLocation"10
  getIndex _proxy  = 112
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowModuleExportLocation_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowModuleExportLocation x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowModuleExportLocation_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowModuleExportLocation where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "declaration" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.PythonFileDeclarations where
  type KeyType Glean.Schema.Codemarkup.Types.PythonFileDeclarations =
    Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key
  getName _proxy  = Glean.PredicateRef "codemarkup.PythonFileDeclarations"10
  getIndex _proxy  = 103
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.pythonFileDeclarations_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.PythonFileDeclarations x k
  getFactKey = Glean.Schema.Codemarkup.Types.pythonFileDeclarations_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.PythonFileDeclarations where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FileDirectXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FileDirectXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FileDirectXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FileDirectXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FileDirectXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.FileDirectXRefs =
    Glean.Schema.Codemarkup.Types.FileDirectXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FileDirectXRefs"10
  getIndex _proxy  = 88
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.fileDirectXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FileDirectXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.fileDirectXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FileDirectXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key = 'Angle.TField "decl" (Glean.Schema.Flow.Types.SomeDeclaration) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowDeclarationInfo where
  type KeyType Glean.Schema.Codemarkup.Types.FlowDeclarationInfo =
    Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowDeclarationInfo"10
  getIndex _proxy  = 51
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowDeclarationInfo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowDeclarationInfo x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowDeclarationInfo_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowDeclarationInfo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FileEntityXRefs_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FileEntityXRefs_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FileEntityXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FileEntityXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Codemarkup.Types.DirectXRef) ('Angle.TField "entity" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FileEntityXRefs where
  type KeyType Glean.Schema.Codemarkup.Types.FileEntityXRefs =
    Glean.Schema.Codemarkup.Types.FileEntityXRefs_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FileEntityXRefs"10
  getIndex _proxy  = 18
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.fileEntityXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FileEntityXRefs x k
  getFactKey = Glean.Schema.Codemarkup.Types.fileEntityXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FileEntityXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "declaration" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Codemarkup.Types.FlowFileDeclarations where
  type KeyType Glean.Schema.Codemarkup.Types.FlowFileDeclarations =
    Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key
  getName _proxy  = Glean.PredicateRef "codemarkup.FlowFileDeclarations"10
  getIndex _proxy  = 0
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Codemarkup.Types.flowFileDeclarations_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Codemarkup.Types.FlowFileDeclarations x k
  getFactKey = Glean.Schema.Codemarkup.Types.flowFileDeclarations_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Codemarkup.Types.FlowFileDeclarations where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Codemarkup.Types.LinkTo where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.LinkTo_localRepo x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Codemarkup.Types.LinkTo_localRepo
    ]

type instance Angle.SumFields Glean.Schema.Codemarkup.Types.LinkTo = 'Angle.TField "localRepo" (Glean.Schema.Src.Types.FileLocation) ('Angle.TNoFields)

instance Glean.SumBranches Glean.Schema.Src.Types.FileLocation Glean.Schema.Codemarkup.Types.LinkTo where
  injectBranch = Glean.Schema.Codemarkup.Types.LinkTo_localRepo
  projectBranch (Glean.Schema.Codemarkup.Types.LinkTo_localRepo x) = Prelude.Just x

instance Glean.Type Glean.Schema.Codemarkup.Types.Annotation where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.Annotation x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.Annotation
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.Annotation = 'Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TField "shortName" (Data.Text.Text) ('Angle.TField "linkTo" (Prelude.Maybe Glean.Schema.Codemarkup.Types.LinkTo) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Codemarkup.Types.Declaration where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.Declaration x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Codemarkup.Types.Declaration
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.Declaration = 'Angle.TField "name" (Data.Text.Text) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Codemarkup.Types.DirectXRef where
  buildRtsValue b (Glean.Schema.Codemarkup.Types.DirectXRef x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Codemarkup.Types.DirectXRef
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Codemarkup.Types.DirectXRef = 'Angle.TField "target" (Glean.Schema.Codemarkup.Types.Declaration) ('Angle.TField "source" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))
