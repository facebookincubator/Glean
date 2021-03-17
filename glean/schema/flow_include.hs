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
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Flow.Types.ImportDeclaration_key where
  buildRtsValue b (Glean.Schema.Flow.Types.ImportDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.ImportDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.ImportDeclaration_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Flow.Types.Declaration) ('Angle.TField "import_" (Glean.Schema.Flow.Types.ImportDeclaration_import_) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.ImportDeclaration where
  type KeyType Glean.Schema.Flow.Types.ImportDeclaration =
    Glean.Schema.Flow.Types.ImportDeclaration_key
  getName _proxy  = Glean.PredicateRef "flow.ImportDeclaration"3
  getIndex _proxy  = 489
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.importDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.ImportDeclaration x k
  getFactKey = Glean.Schema.Flow.Types.importDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.ImportDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Flow.Types.Documentation where
  type KeyType Glean.Schema.Flow.Types.Documentation =
    Glean.Schema.Flow.Types.Range
  getName _proxy  = Glean.PredicateRef "flow.Documentation"3
  getIndex _proxy  = 463
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.documentation_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.Documentation x k
  getFactKey = Glean.Schema.Flow.Types.documentation_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.Documentation where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.Module_key where
  buildRtsValue b (Glean.Schema.Flow.Types.Module_key_file x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.Module_key_builtin x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.Module_key_lib x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.Module_key_noSource x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.Module_key_string_ x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.Module_key_file
    , Glean.mapD Glean.Schema.Flow.Types.Module_key_builtin
    , Glean.mapD Glean.Schema.Flow.Types.Module_key_lib
    , Glean.mapD Glean.Schema.Flow.Types.Module_key_noSource
    , Glean.mapD Glean.Schema.Flow.Types.Module_key_string_
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.Module_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "builtin" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "lib" (Data.Text.Text) ('Angle.TField "noSource" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "string_" (Data.Text.Text) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Flow.Types.Module where
  type KeyType Glean.Schema.Flow.Types.Module =
    Glean.Schema.Flow.Types.Module_key
  getName _proxy  = Glean.PredicateRef "flow.Module"3
  getIndex _proxy  = 445
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.module_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.Module x k
  getFactKey = Glean.Schema.Flow.Types.module_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.Module where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.StringToFileModule_key where
  buildRtsValue b (Glean.Schema.Flow.Types.StringToFileModule_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.StringToFileModule_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.StringToFileModule_key = 'Angle.TField "string_" (Data.Text.Text) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.StringToFileModule where
  type KeyType Glean.Schema.Flow.Types.StringToFileModule =
    Glean.Schema.Flow.Types.StringToFileModule_key
  getName _proxy  = Glean.PredicateRef "flow.StringToFileModule"3
  getIndex _proxy  = 436
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.stringToFileModule_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.StringToFileModule x k
  getFactKey = Glean.Schema.Flow.Types.stringToFileModule_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.StringToFileModule where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.FileXRef_key where
  buildRtsValue b (Glean.Schema.Flow.Types.FileXRef_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.FileXRef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.FileXRef_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "ref" (Glean.Schema.Flow.Types.XRef) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.FileXRef where
  type KeyType Glean.Schema.Flow.Types.FileXRef =
    Glean.Schema.Flow.Types.FileXRef_key
  getName _proxy  = Glean.PredicateRef "flow.FileXRef"3
  getIndex _proxy  = 434
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.fileXRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.FileXRef x k
  getFactKey = Glean.Schema.Flow.Types.fileXRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.FileXRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.Export_key where
  buildRtsValue b (Glean.Schema.Flow.Types.Export_key_commonJS x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.Export_key_commonJSMember x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.Export_key_named x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.Export_key_default_ x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.Export_key_star x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.Export_key_commonJS
    , Glean.mapD Glean.Schema.Flow.Types.Export_key_commonJSMember
    , Glean.mapD Glean.Schema.Flow.Types.Export_key_named
    , Glean.mapD Glean.Schema.Flow.Types.Export_key_default_
    , Glean.mapD Glean.Schema.Flow.Types.Export_key_star
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.Export_key = 'Angle.TField "commonJS" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "commonJSMember" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "named" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "default_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "star" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Flow.Types.Export where
  type KeyType Glean.Schema.Flow.Types.Export =
    Glean.Schema.Flow.Types.Export_key
  getName _proxy  = Glean.PredicateRef "flow.Export"3
  getIndex _proxy  = 390
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.export_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.Export x k
  getFactKey = Glean.Schema.Flow.Types.export_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.Export where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.FileDeclaration_key where
  buildRtsValue b (Glean.Schema.Flow.Types.FileDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.FileDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.FileDeclaration_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "declaration" (Glean.Schema.Flow.Types.SomeDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.FileDeclaration where
  type KeyType Glean.Schema.Flow.Types.FileDeclaration =
    Glean.Schema.Flow.Types.FileDeclaration_key
  getName _proxy  = Glean.PredicateRef "flow.FileDeclaration"3
  getIndex _proxy  = 353
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.fileDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.FileDeclaration x k
  getFactKey = Glean.Schema.Flow.Types.fileDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.FileDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.ModuleTypeExport_key where
  buildRtsValue b (Glean.Schema.Flow.Types.ModuleTypeExport_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.ModuleTypeExport_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.ModuleTypeExport_key = 'Angle.TField "module" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TField "typeExport" (Glean.KeyType Glean.Schema.Flow.Types.TypeExport) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.ModuleTypeExport where
  type KeyType Glean.Schema.Flow.Types.ModuleTypeExport =
    Glean.Schema.Flow.Types.ModuleTypeExport_key
  getName _proxy  = Glean.PredicateRef "flow.ModuleTypeExport"3
  getIndex _proxy  = 333
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.moduleTypeExport_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.ModuleTypeExport x k
  getFactKey = Glean.Schema.Flow.Types.moduleTypeExport_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.ModuleTypeExport where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.LocalDeclarationReference_key where
  buildRtsValue b (Glean.Schema.Flow.Types.LocalDeclarationReference_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.LocalDeclarationReference_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.LocalDeclarationReference_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Flow.Types.Declaration) ('Angle.TField "loc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.LocalDeclarationReference where
  type KeyType Glean.Schema.Flow.Types.LocalDeclarationReference =
    Glean.Schema.Flow.Types.LocalDeclarationReference_key
  getName _proxy  = Glean.PredicateRef "flow.LocalDeclarationReference"3
  getIndex _proxy  = 331
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.localDeclarationReference_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.LocalDeclarationReference x k
  getFactKey = Glean.Schema.Flow.Types.localDeclarationReference_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.LocalDeclarationReference where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.DeclarationInfo_key where
  buildRtsValue b (Glean.Schema.Flow.Types.DeclarationInfo_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Flow.Types.DeclarationInfo_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.DeclarationInfo_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Flow.Types.Declaration) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Flow.Types.Type) ('Angle.TField "documentation" (Prelude.Maybe (Glean.KeyType Glean.Schema.Flow.Types.Documentation)) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Flow.Types.DeclarationInfo where
  type KeyType Glean.Schema.Flow.Types.DeclarationInfo =
    Glean.Schema.Flow.Types.DeclarationInfo_key
  getName _proxy  = Glean.PredicateRef "flow.DeclarationInfo"3
  getIndex _proxy  = 327
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.declarationInfo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.DeclarationInfo x k
  getFactKey = Glean.Schema.Flow.Types.declarationInfo_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.DeclarationInfo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.SourceOfTypeExport_key where
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfTypeExport_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.SourceOfTypeExport_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.SourceOfTypeExport_key = 'Angle.TField "moduleTypeExport" (Glean.KeyType Glean.Schema.Flow.Types.ModuleTypeExport) ('Angle.TField "source" (Glean.Schema.Flow.Types.SourceOfTypeExport_source) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.SourceOfTypeExport where
  type KeyType Glean.Schema.Flow.Types.SourceOfTypeExport =
    Glean.Schema.Flow.Types.SourceOfTypeExport_key
  getName _proxy  = Glean.PredicateRef "flow.SourceOfTypeExport"3
  getIndex _proxy  = 279
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.sourceOfTypeExport_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.SourceOfTypeExport x k
  getFactKey = Glean.Schema.Flow.Types.sourceOfTypeExport_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.SourceOfTypeExport where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.Declaration_key where
  buildRtsValue b (Glean.Schema.Flow.Types.Declaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.Declaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.Declaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "loc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.Declaration where
  type KeyType Glean.Schema.Flow.Types.Declaration =
    Glean.Schema.Flow.Types.Declaration_key
  getName _proxy  = Glean.PredicateRef "flow.Declaration"3
  getIndex _proxy  = 269
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.declaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.Declaration x k
  getFactKey = Glean.Schema.Flow.Types.declaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.Declaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.TypeImportDeclaration_key where
  buildRtsValue b (Glean.Schema.Flow.Types.TypeImportDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.TypeImportDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.TypeImportDeclaration_key = 'Angle.TField "typeDeclaration" (Glean.KeyType Glean.Schema.Flow.Types.TypeDeclaration) ('Angle.TField "import_" (Glean.Schema.Flow.Types.TypeImportDeclaration_import_) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.TypeImportDeclaration where
  type KeyType Glean.Schema.Flow.Types.TypeImportDeclaration =
    Glean.Schema.Flow.Types.TypeImportDeclaration_key
  getName _proxy  = Glean.PredicateRef "flow.TypeImportDeclaration"3
  getIndex _proxy  = 251
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.typeImportDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.TypeImportDeclaration x k
  getFactKey = Glean.Schema.Flow.Types.typeImportDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.TypeImportDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.Range_key where
  buildRtsValue b (Glean.Schema.Flow.Types.Range_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.Range_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.Range_key = 'Angle.TField "module" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.Range where
  type KeyType Glean.Schema.Flow.Types.Range =
    Glean.Schema.Flow.Types.Range_key
  getName _proxy  = Glean.PredicateRef "flow.Range"3
  getIndex _proxy  = 208
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.range_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.Range x k
  getFactKey = Glean.Schema.Flow.Types.range_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.Range where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Flow.Types.Name where
  type KeyType Glean.Schema.Flow.Types.Name = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "flow.Name"3
  getIndex _proxy  = 184
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.name_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.Name x k
  getFactKey = Glean.Schema.Flow.Types.name_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.Name where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.TypeExport_key where
  buildRtsValue b (Glean.Schema.Flow.Types.TypeExport_key_named x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.TypeExport_key_star x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.TypeExport_key_named
    , Glean.mapD Glean.Schema.Flow.Types.TypeExport_key_star
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.TypeExport_key = 'Angle.TField "named" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "star" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.Flow.Types.Name Glean.Schema.Flow.Types.TypeExport_key where
  injectBranch = Glean.Schema.Flow.Types.TypeExport_key_named
  projectBranch (Glean.Schema.Flow.Types.TypeExport_key_named x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.Module Glean.Schema.Flow.Types.TypeExport_key where
  injectBranch = Glean.Schema.Flow.Types.TypeExport_key_star
  projectBranch (Glean.Schema.Flow.Types.TypeExport_key_star x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Predicate Glean.Schema.Flow.Types.TypeExport where
  type KeyType Glean.Schema.Flow.Types.TypeExport =
    Glean.Schema.Flow.Types.TypeExport_key
  getName _proxy  = Glean.PredicateRef "flow.TypeExport"3
  getIndex _proxy  = 153
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.typeExport_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.TypeExport x k
  getFactKey = Glean.Schema.Flow.Types.typeExport_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.TypeExport where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.MemberDeclarationInfo_key where
  buildRtsValue b (Glean.Schema.Flow.Types.MemberDeclarationInfo_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Flow.Types.MemberDeclarationInfo_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.MemberDeclarationInfo_key = 'Angle.TField "memberDeclaration" (Glean.KeyType Glean.Schema.Flow.Types.MemberDeclaration) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Flow.Types.Type) ('Angle.TField "documentation" (Prelude.Maybe (Glean.KeyType Glean.Schema.Flow.Types.Documentation)) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Flow.Types.MemberDeclarationInfo where
  type KeyType Glean.Schema.Flow.Types.MemberDeclarationInfo =
    Glean.Schema.Flow.Types.MemberDeclarationInfo_key
  getName _proxy  = Glean.PredicateRef "flow.MemberDeclarationInfo"3
  getIndex _proxy  = 98
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.memberDeclarationInfo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.MemberDeclarationInfo x k
  getFactKey = Glean.Schema.Flow.Types.memberDeclarationInfo_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.MemberDeclarationInfo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.TypeDeclarationInfo_key where
  buildRtsValue b (Glean.Schema.Flow.Types.TypeDeclarationInfo_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Flow.Types.TypeDeclarationInfo_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.TypeDeclarationInfo_key = 'Angle.TField "typeDeclaration" (Glean.KeyType Glean.Schema.Flow.Types.TypeDeclaration) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Flow.Types.Type) ('Angle.TField "documentation" (Prelude.Maybe (Glean.KeyType Glean.Schema.Flow.Types.Documentation)) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Flow.Types.TypeDeclarationInfo where
  type KeyType Glean.Schema.Flow.Types.TypeDeclarationInfo =
    Glean.Schema.Flow.Types.TypeDeclarationInfo_key
  getName _proxy  = Glean.PredicateRef "flow.TypeDeclarationInfo"3
  getIndex _proxy  = 93
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.typeDeclarationInfo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.TypeDeclarationInfo x k
  getFactKey = Glean.Schema.Flow.Types.typeDeclarationInfo_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.TypeDeclarationInfo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.ModuleExport_key where
  buildRtsValue b (Glean.Schema.Flow.Types.ModuleExport_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.ModuleExport_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.ModuleExport_key = 'Angle.TField "module" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TField "export_" (Glean.KeyType Glean.Schema.Flow.Types.Export) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.ModuleExport where
  type KeyType Glean.Schema.Flow.Types.ModuleExport =
    Glean.Schema.Flow.Types.ModuleExport_key
  getName _proxy  = Glean.PredicateRef "flow.ModuleExport"3
  getIndex _proxy  = 85
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.moduleExport_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.ModuleExport x k
  getFactKey = Glean.Schema.Flow.Types.moduleExport_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.ModuleExport where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.TypeDeclarationReference_key where
  buildRtsValue b (Glean.Schema.Flow.Types.TypeDeclarationReference_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.TypeDeclarationReference_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.TypeDeclarationReference_key = 'Angle.TField "typeDeclaration" (Glean.KeyType Glean.Schema.Flow.Types.TypeDeclaration) ('Angle.TField "loc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.TypeDeclarationReference where
  type KeyType Glean.Schema.Flow.Types.TypeDeclarationReference =
    Glean.Schema.Flow.Types.TypeDeclarationReference_key
  getName _proxy  = Glean.PredicateRef "flow.TypeDeclarationReference"3
  getIndex _proxy  = 78
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.typeDeclarationReference_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.TypeDeclarationReference x k
  getFactKey = Glean.Schema.Flow.Types.typeDeclarationReference_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.TypeDeclarationReference where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.MemberDeclarationReference_key where
  buildRtsValue b (Glean.Schema.Flow.Types.MemberDeclarationReference_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.MemberDeclarationReference_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.MemberDeclarationReference_key = 'Angle.TField "memberDeclaration" (Glean.KeyType Glean.Schema.Flow.Types.MemberDeclaration) ('Angle.TField "loc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.MemberDeclarationReference where
  type KeyType Glean.Schema.Flow.Types.MemberDeclarationReference =
    Glean.Schema.Flow.Types.MemberDeclarationReference_key
  getName _proxy  = Glean.PredicateRef "flow.MemberDeclarationReference"3
  getIndex _proxy  = 73
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.memberDeclarationReference_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.MemberDeclarationReference x k
  getFactKey = Glean.Schema.Flow.Types.memberDeclarationReference_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.MemberDeclarationReference where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.TypeDeclaration_key where
  buildRtsValue b (Glean.Schema.Flow.Types.TypeDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.TypeDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.TypeDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "loc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.TypeDeclaration where
  type KeyType Glean.Schema.Flow.Types.TypeDeclaration =
    Glean.Schema.Flow.Types.TypeDeclaration_key
  getName _proxy  = Glean.PredicateRef "flow.TypeDeclaration"3
  getIndex _proxy  = 38
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.typeDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.TypeDeclaration x k
  getFactKey = Glean.Schema.Flow.Types.typeDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.TypeDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.MemberDeclaration_key where
  buildRtsValue b (Glean.Schema.Flow.Types.MemberDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.MemberDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.MemberDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Flow.Types.Name) ('Angle.TField "loc" (Glean.KeyType Glean.Schema.Flow.Types.Range) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.MemberDeclaration where
  type KeyType Glean.Schema.Flow.Types.MemberDeclaration =
    Glean.Schema.Flow.Types.MemberDeclaration_key
  getName _proxy  = Glean.PredicateRef "flow.MemberDeclaration"3
  getIndex _proxy  = 31
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.memberDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.MemberDeclaration x k
  getFactKey = Glean.Schema.Flow.Types.memberDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.MemberDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.SourceOfExport_key where
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfExport_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.SourceOfExport_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.SourceOfExport_key = 'Angle.TField "moduleExport" (Glean.KeyType Glean.Schema.Flow.Types.ModuleExport) ('Angle.TField "source" (Glean.Schema.Flow.Types.SourceOfExport_source) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.SourceOfExport where
  type KeyType Glean.Schema.Flow.Types.SourceOfExport =
    Glean.Schema.Flow.Types.SourceOfExport_key
  getName _proxy  = Glean.PredicateRef "flow.SourceOfExport"3
  getIndex _proxy  = 29
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.sourceOfExport_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.SourceOfExport x k
  getFactKey = Glean.Schema.Flow.Types.sourceOfExport_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.SourceOfExport where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Flow.Types.Type where
  type KeyType Glean.Schema.Flow.Types.Type = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "flow.Type"3
  getIndex _proxy  = 6
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.type_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.Type x k
  getFactKey = Glean.Schema.Flow.Types.type_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.Type where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.FileOfStringModule_key where
  buildRtsValue b (Glean.Schema.Flow.Types.FileOfStringModule_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Flow.Types.FileOfStringModule_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Flow.Types.FileOfStringModule_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "string_" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Flow.Types.FileOfStringModule where
  type KeyType Glean.Schema.Flow.Types.FileOfStringModule =
    Glean.Schema.Flow.Types.FileOfStringModule_key
  getName _proxy  = Glean.PredicateRef "flow.FileOfStringModule"3
  getIndex _proxy  = 5
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Flow.Types.fileOfStringModule_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Flow.Types.FileOfStringModule x k
  getFactKey = Glean.Schema.Flow.Types.fileOfStringModule_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Flow.Types.FileOfStringModule where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Flow.Types.XRef where
  buildRtsValue b (Glean.Schema.Flow.Types.XRef_localRef x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.XRef_memberRef x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.XRef_typeRef x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.XRef_localRef
    , Glean.mapD Glean.Schema.Flow.Types.XRef_memberRef
    , Glean.mapD Glean.Schema.Flow.Types.XRef_typeRef
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.XRef = 'Angle.TField "localRef" (Glean.KeyType Glean.Schema.Flow.Types.LocalDeclarationReference) ('Angle.TField "memberRef" (Glean.KeyType Glean.Schema.Flow.Types.MemberDeclarationReference) ('Angle.TField "typeRef" (Glean.KeyType Glean.Schema.Flow.Types.TypeDeclarationReference) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Flow.Types.LocalDeclarationReference Glean.Schema.Flow.Types.XRef where
  injectBranch = Glean.Schema.Flow.Types.XRef_localRef
  projectBranch (Glean.Schema.Flow.Types.XRef_localRef x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.MemberDeclarationReference Glean.Schema.Flow.Types.XRef where
  injectBranch = Glean.Schema.Flow.Types.XRef_memberRef
  projectBranch (Glean.Schema.Flow.Types.XRef_memberRef x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.TypeDeclarationReference Glean.Schema.Flow.Types.XRef where
  injectBranch = Glean.Schema.Flow.Types.XRef_typeRef
  projectBranch (Glean.Schema.Flow.Types.XRef_typeRef x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Flow.Types.SomeDeclaration where
  buildRtsValue b (Glean.Schema.Flow.Types.SomeDeclaration_localDecl x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.SomeDeclaration_memberDecl x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.SomeDeclaration_typeDecl x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.SomeDeclaration_localDecl
    , Glean.mapD Glean.Schema.Flow.Types.SomeDeclaration_memberDecl
    , Glean.mapD Glean.Schema.Flow.Types.SomeDeclaration_typeDecl
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.SomeDeclaration = 'Angle.TField "localDecl" (Glean.KeyType Glean.Schema.Flow.Types.Declaration) ('Angle.TField "memberDecl" (Glean.KeyType Glean.Schema.Flow.Types.MemberDeclaration) ('Angle.TField "typeDecl" (Glean.KeyType Glean.Schema.Flow.Types.TypeDeclaration) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Flow.Types.Declaration Glean.Schema.Flow.Types.SomeDeclaration where
  injectBranch = Glean.Schema.Flow.Types.SomeDeclaration_localDecl
  projectBranch (Glean.Schema.Flow.Types.SomeDeclaration_localDecl x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.MemberDeclaration Glean.Schema.Flow.Types.SomeDeclaration where
  injectBranch = Glean.Schema.Flow.Types.SomeDeclaration_memberDecl
  projectBranch (Glean.Schema.Flow.Types.SomeDeclaration_memberDecl x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.TypeDeclaration Glean.Schema.Flow.Types.SomeDeclaration where
  injectBranch = Glean.Schema.Flow.Types.SomeDeclaration_typeDecl
  projectBranch (Glean.Schema.Flow.Types.SomeDeclaration_typeDecl x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Flow.Types.ImportDeclaration_import_ where
  buildRtsValue b (Glean.Schema.Flow.Types.ImportDeclaration_import__moduleExport x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.ImportDeclaration_import__moduleNamespace x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.ImportDeclaration_import__moduleExport
    , Glean.mapD Glean.Schema.Flow.Types.ImportDeclaration_import__moduleNamespace
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.ImportDeclaration_import_ = 'Angle.TField "moduleExport" (Glean.KeyType Glean.Schema.Flow.Types.ModuleExport) ('Angle.TField "moduleNamespace" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.Flow.Types.ModuleExport Glean.Schema.Flow.Types.ImportDeclaration_import_ where
  injectBranch = Glean.Schema.Flow.Types.ImportDeclaration_import__moduleExport
  projectBranch (Glean.Schema.Flow.Types.ImportDeclaration_import__moduleExport x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.Module Glean.Schema.Flow.Types.ImportDeclaration_import_ where
  injectBranch = Glean.Schema.Flow.Types.ImportDeclaration_import__moduleNamespace
  projectBranch (Glean.Schema.Flow.Types.ImportDeclaration_import__moduleNamespace x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Flow.Types.SourceOfTypeExport_source where
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfTypeExport_source_typeDeclaration x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleTypeExport x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleNamespace x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.SourceOfTypeExport_source_typeDeclaration
    , Glean.mapD Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleTypeExport
    , Glean.mapD Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleNamespace
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.SourceOfTypeExport_source = 'Angle.TField "typeDeclaration" (Glean.KeyType Glean.Schema.Flow.Types.TypeDeclaration) ('Angle.TField "moduleTypeExport" (Glean.KeyType Glean.Schema.Flow.Types.ModuleTypeExport) ('Angle.TField "moduleNamespace" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Flow.Types.TypeDeclaration Glean.Schema.Flow.Types.SourceOfTypeExport_source where
  injectBranch = Glean.Schema.Flow.Types.SourceOfTypeExport_source_typeDeclaration
  projectBranch (Glean.Schema.Flow.Types.SourceOfTypeExport_source_typeDeclaration x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.ModuleTypeExport Glean.Schema.Flow.Types.SourceOfTypeExport_source where
  injectBranch = Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleTypeExport
  projectBranch (Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleTypeExport x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.Module Glean.Schema.Flow.Types.SourceOfTypeExport_source where
  injectBranch = Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleNamespace
  projectBranch (Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleNamespace x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Flow.Types.TypeImportDeclaration_import_ where
  buildRtsValue b (Glean.Schema.Flow.Types.TypeImportDeclaration_import__type x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.TypeImportDeclaration_import__typeof_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.TypeImportDeclaration_import__moduleTypeof x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.TypeImportDeclaration_import__type
    , Glean.mapD Glean.Schema.Flow.Types.TypeImportDeclaration_import__typeof_
    , Glean.mapD Glean.Schema.Flow.Types.TypeImportDeclaration_import__moduleTypeof
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.TypeImportDeclaration_import_ = 'Angle.TField "type" (Glean.KeyType Glean.Schema.Flow.Types.ModuleTypeExport) ('Angle.TField "typeof_" (Glean.KeyType Glean.Schema.Flow.Types.ModuleExport) ('Angle.TField "moduleTypeof" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Flow.Types.ModuleTypeExport Glean.Schema.Flow.Types.TypeImportDeclaration_import_ where
  injectBranch = Glean.Schema.Flow.Types.TypeImportDeclaration_import__type
  projectBranch (Glean.Schema.Flow.Types.TypeImportDeclaration_import__type x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.ModuleExport Glean.Schema.Flow.Types.TypeImportDeclaration_import_ where
  injectBranch = Glean.Schema.Flow.Types.TypeImportDeclaration_import__typeof_
  projectBranch (Glean.Schema.Flow.Types.TypeImportDeclaration_import__typeof_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.Module Glean.Schema.Flow.Types.TypeImportDeclaration_import_ where
  injectBranch = Glean.Schema.Flow.Types.TypeImportDeclaration_import__moduleTypeof
  projectBranch (Glean.Schema.Flow.Types.TypeImportDeclaration_import__moduleTypeof x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Flow.Types.SourceOfExport_source where
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfExport_source_declaration x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfExport_source_memberDeclaration x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfExport_source_moduleExport x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Flow.Types.SourceOfExport_source_moduleNamespace x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Flow.Types.SourceOfExport_source_declaration
    , Glean.mapD Glean.Schema.Flow.Types.SourceOfExport_source_memberDeclaration
    , Glean.mapD Glean.Schema.Flow.Types.SourceOfExport_source_moduleExport
    , Glean.mapD Glean.Schema.Flow.Types.SourceOfExport_source_moduleNamespace
    ]

type instance Angle.SumFields Glean.Schema.Flow.Types.SourceOfExport_source = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Flow.Types.Declaration) ('Angle.TField "memberDeclaration" (Glean.KeyType Glean.Schema.Flow.Types.MemberDeclaration) ('Angle.TField "moduleExport" (Glean.KeyType Glean.Schema.Flow.Types.ModuleExport) ('Angle.TField "moduleNamespace" (Glean.KeyType Glean.Schema.Flow.Types.Module) ('Angle.TNoFields))))

instance Glean.SumBranches Glean.Schema.Flow.Types.Declaration Glean.Schema.Flow.Types.SourceOfExport_source where
  injectBranch = Glean.Schema.Flow.Types.SourceOfExport_source_declaration
  projectBranch (Glean.Schema.Flow.Types.SourceOfExport_source_declaration x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.MemberDeclaration Glean.Schema.Flow.Types.SourceOfExport_source where
  injectBranch = Glean.Schema.Flow.Types.SourceOfExport_source_memberDeclaration
  projectBranch (Glean.Schema.Flow.Types.SourceOfExport_source_memberDeclaration x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.ModuleExport Glean.Schema.Flow.Types.SourceOfExport_source where
  injectBranch = Glean.Schema.Flow.Types.SourceOfExport_source_moduleExport
  projectBranch (Glean.Schema.Flow.Types.SourceOfExport_source_moduleExport x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Flow.Types.Module Glean.Schema.Flow.Types.SourceOfExport_source where
  injectBranch = Glean.Schema.Flow.Types.SourceOfExport_source_moduleNamespace
  projectBranch (Glean.Schema.Flow.Types.SourceOfExport_source_moduleNamespace x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
