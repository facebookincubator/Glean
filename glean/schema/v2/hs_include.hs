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


instance Glean.Type Glean.Schema.Hs.Types.Class_key where
  buildRtsValue b (Glean.Schema.Hs.Types.Class_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.Class_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.Class_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hs.Types.ClassName) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hs.Types.Class where
  type KeyType Glean.Schema.Hs.Types.Class = Glean.Schema.Hs.Types.Class_key
  getName _proxy  = Glean.PredicateRef "hs.Class"1
  getIndex _proxy  = 428
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.class_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.Class x k
  getFactKey = Glean.Schema.Hs.Types.class_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.Class where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hs.Types.FunctionName where
  type KeyType Glean.Schema.Hs.Types.FunctionName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hs.FunctionName"1
  getIndex _proxy  = 391
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.functionName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.FunctionName x k
  getFactKey = Glean.Schema.Hs.Types.functionName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.FunctionName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hs.Types.Type where
  type KeyType Glean.Schema.Hs.Types.Type = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hs.Type"1
  getIndex _proxy  = 358
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.type_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.Type x k
  getFactKey = Glean.Schema.Hs.Types.type_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.Type where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hs.Types.PackageId where
  type KeyType Glean.Schema.Hs.Types.PackageId = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hs.PackageId"1
  getIndex _proxy  = 354
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.packageId_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.PackageId x k
  getFactKey = Glean.Schema.Hs.Types.packageId_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.PackageId where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.Module_key where
  buildRtsValue b (Glean.Schema.Hs.Types.Module_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Hs.Types.Module_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.Module_key = 'Angle.TField "packageId" (Glean.KeyType Glean.Schema.Hs.Types.PackageId) ('Angle.TField "moduleName" (Glean.KeyType Glean.Schema.Hs.Types.ModuleName) ('Angle.TField "source" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Hs.Types.Module where
  type KeyType Glean.Schema.Hs.Types.Module = Glean.Schema.Hs.Types.Module_key
  getName _proxy  = Glean.PredicateRef "hs.Module"1
  getIndex _proxy  = 304
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.module_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.Module x k
  getFactKey = Glean.Schema.Hs.Types.module_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.Module where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hs.Types.DefinitionName where
  type KeyType Glean.Schema.Hs.Types.DefinitionName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hs.DefinitionName"1
  getIndex _proxy  = 303
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.definitionName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.DefinitionName x k
  getFactKey = Glean.Schema.Hs.Types.definitionName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.DefinitionName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.ModuleDefinitions_key where
  buildRtsValue b (Glean.Schema.Hs.Types.ModuleDefinitions_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.ModuleDefinitions_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.ModuleDefinitions_key = 'Angle.TField "module" (Glean.KeyType Glean.Schema.Hs.Types.Module) ('Angle.TField "functionDefinitions" ([Glean.KeyType Glean.Schema.Hs.Types.FunctionDefinition]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hs.Types.ModuleDefinitions where
  type KeyType Glean.Schema.Hs.Types.ModuleDefinitions =
    Glean.Schema.Hs.Types.ModuleDefinitions_key
  getName _proxy  = Glean.PredicateRef "hs.ModuleDefinitions"1
  getIndex _proxy  = 263
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.moduleDefinitions_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.ModuleDefinitions x k
  getFactKey = Glean.Schema.Hs.Types.moduleDefinitions_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.ModuleDefinitions where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.FileXRefMap_key where
  buildRtsValue b (Glean.Schema.Hs.Types.FileXRefMap_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.FileXRefMap_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.FileXRefMap_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "refs" ([Glean.Schema.Hs.Types.XReference]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hs.Types.FileXRefMap where
  type KeyType Glean.Schema.Hs.Types.FileXRefMap =
    Glean.Schema.Hs.Types.FileXRefMap_key
  getName _proxy  = Glean.PredicateRef "hs.FileXRefMap"2
  getIndex _proxy  = 222
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.fileXRefMap_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.FileXRefMap x k
  getFactKey = Glean.Schema.Hs.Types.fileXRefMap_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.FileXRefMap where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.Definition_key where
  buildRtsValue b (Glean.Schema.Hs.Types.Definition_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.Definition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.Definition_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hs.Types.DefinitionName) ('Angle.TField "source" (Glean.Schema.Src.Types.FileLocation) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hs.Types.Definition where
  type KeyType Glean.Schema.Hs.Types.Definition =
    Glean.Schema.Hs.Types.Definition_key
  getName _proxy  = Glean.PredicateRef "hs.Definition"2
  getIndex _proxy  = 180
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.definition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.Definition x k
  getFactKey = Glean.Schema.Hs.Types.definition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.Definition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.Definition_1_key where
  buildRtsValue b (Glean.Schema.Hs.Types.Definition_1_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.Definition_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.Definition_1_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hs.Types.DefinitionName) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hs.Types.Definition_1 where
  type KeyType Glean.Schema.Hs.Types.Definition_1 =
    Glean.Schema.Hs.Types.Definition_1_key
  getName _proxy  = Glean.PredicateRef "hs.Definition"1
  getIndex _proxy  = 179
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.definition_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.Definition_1 x k
  getFactKey = Glean.Schema.Hs.Types.definition_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.Definition_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hs.Types.ModuleName where
  type KeyType Glean.Schema.Hs.Types.ModuleName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hs.ModuleName"1
  getIndex _proxy  = 160
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.moduleName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.ModuleName x k
  getFactKey = Glean.Schema.Hs.Types.moduleName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.ModuleName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.ClassInstance_key where
  buildRtsValue b (Glean.Schema.Hs.Types.ClassInstance_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Hs.Types.ClassInstance_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.ClassInstance_key = 'Angle.TField "typeclass" (Glean.KeyType Glean.Schema.Hs.Types.ClassName) ('Angle.TField "instance" (Glean.KeyType Glean.Schema.Hs.Types.Type) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Hs.Types.ClassInstance where
  type KeyType Glean.Schema.Hs.Types.ClassInstance =
    Glean.Schema.Hs.Types.ClassInstance_key
  getName _proxy  = Glean.PredicateRef "hs.ClassInstance"1
  getIndex _proxy  = 112
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.classInstance_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.ClassInstance x k
  getFactKey = Glean.Schema.Hs.Types.classInstance_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.ClassInstance where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.FunctionDefinition_key where
  buildRtsValue b (Glean.Schema.Hs.Types.FunctionDefinition_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.FunctionDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.FunctionDefinition_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Hs.Types.FunctionName) ('Angle.TField "source" (Glean.Schema.Src.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hs.Types.FunctionDefinition where
  type KeyType Glean.Schema.Hs.Types.FunctionDefinition =
    Glean.Schema.Hs.Types.FunctionDefinition_key
  getName _proxy  = Glean.PredicateRef "hs.FunctionDefinition"1
  getIndex _proxy  = 111
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.functionDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.FunctionDefinition x k
  getFactKey = Glean.Schema.Hs.Types.functionDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.FunctionDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.SourceModule_key where
  buildRtsValue b (Glean.Schema.Hs.Types.SourceModule_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.SourceModule_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.SourceModule_key = 'Angle.TField "moduleName" (Glean.KeyType Glean.Schema.Hs.Types.ModuleName) ('Angle.TField "source" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hs.Types.SourceModule where
  type KeyType Glean.Schema.Hs.Types.SourceModule =
    Glean.Schema.Hs.Types.SourceModule_key
  getName _proxy  = Glean.PredicateRef "hs.SourceModule"1
  getIndex _proxy  = 106
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.sourceModule_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.SourceModule x k
  getFactKey = Glean.Schema.Hs.Types.sourceModule_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.SourceModule where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Hs.Types.ClassName where
  type KeyType Glean.Schema.Hs.Types.ClassName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "hs.ClassName"1
  getIndex _proxy  = 45
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.className_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.ClassName x k
  getFactKey = Glean.Schema.Hs.Types.className_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.ClassName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.XRef_key where
  buildRtsValue b (Glean.Schema.Hs.Types.XRef_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.XRef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.XRef_key = 'Angle.TField "loc" (Glean.Schema.Src.Types.FileLocation) ('Angle.TField "ref" (Glean.Schema.Hs.Types.XRefTarget) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Hs.Types.XRef where
  type KeyType Glean.Schema.Hs.Types.XRef = Glean.Schema.Hs.Types.XRef_key
  getName _proxy  = Glean.PredicateRef "hs.XRef"2
  getIndex _proxy  = 32
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Hs.Types.xRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Hs.Types.XRef x k
  getFactKey = Glean.Schema.Hs.Types.xRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Hs.Types.XRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Hs.Types.XRefTarget where
  buildRtsValue b (Glean.Schema.Hs.Types.XRefTarget_definition x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hs.Types.XRefTarget_typeclass x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Hs.Types.XRefTarget_hs_module x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Hs.Types.XRefTarget_definition
    , Glean.mapD Glean.Schema.Hs.Types.XRefTarget_typeclass
    , Glean.mapD Glean.Schema.Hs.Types.XRefTarget_hs_module
    ]

type instance Angle.SumFields Glean.Schema.Hs.Types.XRefTarget = 'Angle.TField "definition" (Glean.KeyType Glean.Schema.Hs.Types.DefinitionName) ('Angle.TField "typeclass" (Glean.KeyType Glean.Schema.Hs.Types.ClassName) ('Angle.TField "hs_module" (Glean.KeyType Glean.Schema.Hs.Types.ModuleName) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Hs.Types.DefinitionName Glean.Schema.Hs.Types.XRefTarget where
  injectBranch = Glean.Schema.Hs.Types.XRefTarget_definition
  projectBranch (Glean.Schema.Hs.Types.XRefTarget_definition x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hs.Types.ClassName Glean.Schema.Hs.Types.XRefTarget where
  injectBranch = Glean.Schema.Hs.Types.XRefTarget_typeclass
  projectBranch (Glean.Schema.Hs.Types.XRefTarget_typeclass x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hs.Types.ModuleName Glean.Schema.Hs.Types.XRefTarget where
  injectBranch = Glean.Schema.Hs.Types.XRefTarget_hs_module
  projectBranch (Glean.Schema.Hs.Types.XRefTarget_hs_module x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Hs.Types.XReference where
  buildRtsValue b (Glean.Schema.Hs.Types.XReference x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Hs.Types.XReference
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Hs.Types.XReference = 'Angle.TField "target" (Glean.Schema.Hs.Types.XRefTarget) ('Angle.TField "spans" ([Glean.Schema.Src.Types.ByteSpan]) ('Angle.TNoFields))
