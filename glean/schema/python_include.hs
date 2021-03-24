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


instance Glean.Type Glean.Schema.Python.Types.ClassDefinition_key where
  buildRtsValue b (Glean.Schema.Python.Types.ClassDefinition_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Python.Types.ClassDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ClassDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Python.Types.ClassDeclaration) ('Angle.TField "bases" (Prelude.Maybe [Glean.KeyType Glean.Schema.Python.Types.ClassDeclaration]) ('Angle.TField "keywords" (Prelude.Maybe [Glean.Schema.Python.Types.Parameter]) ('Angle.TField "decorators" (Prelude.Maybe [Glean.Schema.Python.Types.Decorator]) ('Angle.TField "docstring" (Prelude.Maybe Glean.Schema.Python.Types.Docstring) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Python.Types.ClassDefinition where
  type KeyType Glean.Schema.Python.Types.ClassDefinition =
    Glean.Schema.Python.Types.ClassDefinition_key
  getName _proxy  = Glean.PredicateRef "python.ClassDefinition"2
  getIndex _proxy  = 506
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.classDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ClassDefinition x k
  getFactKey = Glean.Schema.Python.Types.classDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ClassDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.ImportStatementByName_key where
  buildRtsValue b (Glean.Schema.Python.Types.ImportStatementByName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.ImportStatementByName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ImportStatementByName_key = 'Angle.TField "as_name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "from_name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.ImportStatementByName where
  type KeyType Glean.Schema.Python.Types.ImportStatementByName =
    Glean.Schema.Python.Types.ImportStatementByName_key
  getName _proxy  = Glean.PredicateRef "python.ImportStatementByName"2
  getIndex _proxy  = 471
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.importStatementByName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ImportStatementByName x k
  getFactKey = Glean.Schema.Python.Types.importStatementByName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ImportStatementByName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.DeclarationToName where
  type KeyType Glean.Schema.Python.Types.DeclarationToName =
    Glean.Schema.Python.Types.Declaration
  type ValueType Glean.Schema.Python.Types.DeclarationToName =
    Glean.Schema.Python.Types.DeclarationToName_value
  getName _proxy  = Glean.PredicateRef "python.DeclarationToName"2
  getIndex _proxy  = 467
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.declarationToName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.DeclarationToName x k v
  getFactKey = Glean.Schema.Python.Types.declarationToName_key
  getFactValue = Glean.Schema.Python.Types.declarationToName_value

instance Glean.Type Glean.Schema.Python.Types.DeclarationToName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.DeclarationByName where
  type KeyType Glean.Schema.Python.Types.DeclarationByName =
    Glean.Schema.Python.Types.Name
  type ValueType Glean.Schema.Python.Types.DeclarationByName =
    Glean.Schema.Python.Types.DeclarationByName_value
  getName _proxy  = Glean.PredicateRef "python.DeclarationByName"2
  getIndex _proxy  = 463
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.declarationByName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.DeclarationByName x k v
  getFactKey = Glean.Schema.Python.Types.declarationByName_key
  getFactValue = Glean.Schema.Python.Types.declarationByName_value

instance Glean.Type Glean.Schema.Python.Types.DeclarationByName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.DocstringContent where
  type KeyType Glean.Schema.Python.Types.DocstringContent = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "python.DocstringContent"2
  getIndex _proxy  = 448
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.docstringContent_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.DocstringContent x k
  getFactKey = Glean.Schema.Python.Types.docstringContent_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.DocstringContent where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.VariableBySName where
  type KeyType Glean.Schema.Python.Types.VariableBySName =
    Glean.Schema.Python.Types.SName
  type ValueType Glean.Schema.Python.Types.VariableBySName =
    Glean.Schema.Python.Types.VariableBySName_value
  getName _proxy  = Glean.PredicateRef "python.VariableBySName"2
  getIndex _proxy  = 440
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.variableBySName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.VariableBySName x k v
  getFactKey = Glean.Schema.Python.Types.variableBySName_key
  getFactValue = Glean.Schema.Python.Types.variableBySName_value

instance Glean.Type Glean.Schema.Python.Types.VariableBySName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.FileDirectXRefs_key where
  buildRtsValue b (Glean.Schema.Python.Types.FileDirectXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.FileDirectXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.FileDirectXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xrefs" ([Glean.Schema.Python.Types.DirectXRef]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.FileDirectXRefs where
  type KeyType Glean.Schema.Python.Types.FileDirectXRefs =
    Glean.Schema.Python.Types.FileDirectXRefs_key
  getName _proxy  = Glean.PredicateRef "python.FileDirectXRefs"2
  getIndex _proxy  = 439
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.fileDirectXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.FileDirectXRefs x k
  getFactKey = Glean.Schema.Python.Types.fileDirectXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.FileDirectXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.FunctionDeclaration_key where
  buildRtsValue b (Glean.Schema.Python.Types.FunctionDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Python.Types.FunctionDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.FunctionDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Python.Types.FunctionDeclaration where
  type KeyType Glean.Schema.Python.Types.FunctionDeclaration =
    Glean.Schema.Python.Types.FunctionDeclaration_key
  getName _proxy  = Glean.PredicateRef "python.FunctionDeclaration"1
  getIndex _proxy  = 417
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.functionDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.FunctionDeclaration x k
  getFactKey = Glean.Schema.Python.Types.functionDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.FunctionDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.DirectXRefsByFile_key where
  buildRtsValue b (Glean.Schema.Python.Types.DirectXRefsByFile_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.DirectXRefsByFile_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.DirectXRefsByFile_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xref" (Glean.Schema.Python.Types.DirectXRef) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.DirectXRefsByFile where
  type KeyType Glean.Schema.Python.Types.DirectXRefsByFile =
    Glean.Schema.Python.Types.DirectXRefsByFile_key
  getName _proxy  = Glean.PredicateRef "python.DirectXRefsByFile"2
  getIndex _proxy  = 401
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.directXRefsByFile_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.DirectXRefsByFile x k
  getFactKey = Glean.Schema.Python.Types.directXRefsByFile_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.DirectXRefsByFile where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.FunctionDefinition_key where
  buildRtsValue b (Glean.Schema.Python.Types.FunctionDefinition_key x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = do
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
  decodeRtsValue = Glean.Schema.Python.Types.FunctionDefinition_key
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

type instance Angle.RecordFields Glean.Schema.Python.Types.FunctionDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Python.Types.FunctionDeclaration) ('Angle.TField "is_async" (Prelude.Bool) ('Angle.TField "returns" (Prelude.Maybe (Glean.KeyType Glean.Schema.Python.Types.Type)) ('Angle.TField "params" ([Glean.Schema.Python.Types.Parameter]) ('Angle.TField "posonly_params" (Prelude.Maybe [Glean.Schema.Python.Types.Parameter]) ('Angle.TField "kwonly_params" (Prelude.Maybe [Glean.Schema.Python.Types.Parameter]) ('Angle.TField "star_arg" (Prelude.Maybe Glean.Schema.Python.Types.Parameter) ('Angle.TField "star_kwarg" (Prelude.Maybe Glean.Schema.Python.Types.Parameter) ('Angle.TField "decorators" (Prelude.Maybe [Glean.Schema.Python.Types.Decorator]) ('Angle.TField "docstring" (Prelude.Maybe Glean.Schema.Python.Types.Docstring) ('Angle.TNoFields))))))))))

instance Glean.Predicate Glean.Schema.Python.Types.FunctionDefinition where
  type KeyType Glean.Schema.Python.Types.FunctionDefinition =
    Glean.Schema.Python.Types.FunctionDefinition_key
  getName _proxy  = Glean.PredicateRef "python.FunctionDefinition"2
  getIndex _proxy  = 374
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.functionDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.FunctionDefinition x k
  getFactKey = Glean.Schema.Python.Types.functionDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.FunctionDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.DeclarationUses_key where
  buildRtsValue b (Glean.Schema.Python.Types.DeclarationUses_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Python.Types.DeclarationUses_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.DeclarationUses_key = 'Angle.TField "declaration" (Glean.Schema.Python.Types.Declaration) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Python.Types.DeclarationUses where
  type KeyType Glean.Schema.Python.Types.DeclarationUses =
    Glean.Schema.Python.Types.DeclarationUses_key
  getName _proxy  = Glean.PredicateRef "python.DeclarationUses"2
  getIndex _proxy  = 360
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.declarationUses_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.DeclarationUses x k
  getFactKey = Glean.Schema.Python.Types.declarationUses_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.DeclarationUses where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.ImportStatement_key where
  buildRtsValue b (Glean.Schema.Python.Types.ImportStatement_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.ImportStatement_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ImportStatement_key = 'Angle.TField "from_name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "as_name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.ImportStatement where
  type KeyType Glean.Schema.Python.Types.ImportStatement =
    Glean.Schema.Python.Types.ImportStatement_key
  getName _proxy  = Glean.PredicateRef "python.ImportStatement"2
  getIndex _proxy  = 337
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.importStatement_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ImportStatement x k
  getFactKey = Glean.Schema.Python.Types.importStatement_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ImportStatement where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.ImportStatement_1_key where
  buildRtsValue b (Glean.Schema.Python.Types.ImportStatement_1_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Python.Types.ImportStatement_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ImportStatement_1_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "as_name" (Prelude.Maybe (Glean.KeyType Glean.Schema.Python.Types.Name)) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Python.Types.ImportStatement_1 where
  type KeyType Glean.Schema.Python.Types.ImportStatement_1 =
    Glean.Schema.Python.Types.ImportStatement_1_key
  getName _proxy  = Glean.PredicateRef "python.ImportStatement"1
  getIndex _proxy  = 336
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.importStatement_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ImportStatement_1 x k
  getFactKey = Glean.Schema.Python.Types.importStatement_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ImportStatement_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.DeclarationLocation_key where
  buildRtsValue b (Glean.Schema.Python.Types.DeclarationLocation_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Python.Types.DeclarationLocation_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.DeclarationLocation_key = 'Angle.TField "declaration" (Glean.Schema.Python.Types.Declaration) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Python.Types.DeclarationLocation where
  type KeyType Glean.Schema.Python.Types.DeclarationLocation =
    Glean.Schema.Python.Types.DeclarationLocation_key
  getName _proxy  = Glean.PredicateRef "python.DeclarationLocation"2
  getIndex _proxy  = 327
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.declarationLocation_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.DeclarationLocation x k
  getFactKey = Glean.Schema.Python.Types.declarationLocation_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.DeclarationLocation where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.DeclarationLocation_1_key where
  buildRtsValue b (Glean.Schema.Python.Types.DeclarationLocation_1_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Python.Types.DeclarationLocation_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.DeclarationLocation_1_key = 'Angle.TField "declaration" (Glean.Schema.Python.Types.Declaration_1) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Python.Types.DeclarationLocation_1 where
  type KeyType Glean.Schema.Python.Types.DeclarationLocation_1 =
    Glean.Schema.Python.Types.DeclarationLocation_1_key
  getName _proxy  = Glean.PredicateRef "python.DeclarationLocation"1
  getIndex _proxy  = 326
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.declarationLocation_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.DeclarationLocation_1 x k
  getFactKey = Glean.Schema.Python.Types.declarationLocation_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.DeclarationLocation_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.ModuleBySName where
  type KeyType Glean.Schema.Python.Types.ModuleBySName =
    Glean.Schema.Python.Types.SName
  type ValueType Glean.Schema.Python.Types.ModuleBySName =
    Glean.Schema.Python.Types.ModuleBySName_value
  getName _proxy  = Glean.PredicateRef "python.ModuleBySName"2
  getIndex _proxy  = 325
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.moduleBySName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.ModuleBySName x k v
  getFactKey = Glean.Schema.Python.Types.moduleBySName_key
  getFactValue = Glean.Schema.Python.Types.moduleBySName_value

instance Glean.Type Glean.Schema.Python.Types.ModuleBySName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.ClassDeclaration_key where
  buildRtsValue b (Glean.Schema.Python.Types.ClassDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.ClassDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ClassDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "bases" (Prelude.Maybe [Glean.KeyType Glean.Schema.Python.Types.Name]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.ClassDeclaration where
  type KeyType Glean.Schema.Python.Types.ClassDeclaration =
    Glean.Schema.Python.Types.ClassDeclaration_key
  getName _proxy  = Glean.PredicateRef "python.ClassDeclaration"1
  getIndex _proxy  = 276
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.classDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ClassDeclaration x k
  getFactKey = Glean.Schema.Python.Types.classDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ClassDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.VariableDefinition_key where
  buildRtsValue b (Glean.Schema.Python.Types.VariableDefinition_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.VariableDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.VariableDefinition_key = 'Angle.TField "declaration" (Glean.KeyType Glean.Schema.Python.Types.VariableDeclaration) ('Angle.TField "type" (Prelude.Maybe (Glean.KeyType Glean.Schema.Python.Types.Type)) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.VariableDefinition where
  type KeyType Glean.Schema.Python.Types.VariableDefinition =
    Glean.Schema.Python.Types.VariableDefinition_key
  getName _proxy  = Glean.PredicateRef "python.VariableDefinition"2
  getIndex _proxy  = 269
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.variableDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.VariableDefinition x k
  getFactKey = Glean.Schema.Python.Types.variableDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.VariableDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.FileXRefs_key where
  buildRtsValue b (Glean.Schema.Python.Types.FileXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.FileXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.FileXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xrefs" ([Glean.Schema.Python.Types.XRef]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.FileXRefs where
  type KeyType Glean.Schema.Python.Types.FileXRefs =
    Glean.Schema.Python.Types.FileXRefs_key
  getName _proxy  = Glean.PredicateRef "python.FileXRefs"2
  getIndex _proxy  = 245
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.fileXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.FileXRefs x k
  getFactKey = Glean.Schema.Python.Types.fileXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.FileXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.FileXRefs_1_key where
  buildRtsValue b (Glean.Schema.Python.Types.FileXRefs_1_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.FileXRefs_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.FileXRefs_1_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xrefs" ([Glean.Schema.Python.Types.XRef_1]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.FileXRefs_1 where
  type KeyType Glean.Schema.Python.Types.FileXRefs_1 =
    Glean.Schema.Python.Types.FileXRefs_1_key
  getName _proxy  = Glean.PredicateRef "python.FileXRefs"1
  getIndex _proxy  = 244
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.fileXRefs_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.FileXRefs_1 x k
  getFactKey = Glean.Schema.Python.Types.fileXRefs_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.FileXRefs_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.SName_key where
  buildRtsValue b (Glean.Schema.Python.Types.SName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.SName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.SName_key = 'Angle.TField "local_name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "parent" (Prelude.Maybe (Glean.KeyType Glean.Schema.Python.Types.SName)) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.SName where
  type KeyType Glean.Schema.Python.Types.SName =
    Glean.Schema.Python.Types.SName_key
  getName _proxy  = Glean.PredicateRef "python.SName"2
  getIndex _proxy  = 234
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.sName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.SName x k
  getFactKey = Glean.Schema.Python.Types.sName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.SName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.TargetUses_key where
  buildRtsValue b (Glean.Schema.Python.Types.TargetUses_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Python.Types.TargetUses_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.TargetUses_key = 'Angle.TField "target" (Glean.Schema.Python.Types.XRefTarget) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "uses" (Glean.Schema.Src.Types.ByteSpans) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Python.Types.TargetUses where
  type KeyType Glean.Schema.Python.Types.TargetUses =
    Glean.Schema.Python.Types.TargetUses_key
  getName _proxy  = Glean.PredicateRef "python.TargetUses"2
  getIndex _proxy  = 205
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.targetUses_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.TargetUses x k
  getFactKey = Glean.Schema.Python.Types.targetUses_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.TargetUses where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.TargetUses_1_key where
  buildRtsValue b (Glean.Schema.Python.Types.TargetUses_1_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Python.Types.TargetUses_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.TargetUses_1_key = 'Angle.TField "target" (Glean.Schema.Python.Types.XRefTarget_1) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "uses" (Glean.Schema.Src.Types.ByteSpans) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Python.Types.TargetUses_1 where
  type KeyType Glean.Schema.Python.Types.TargetUses_1 =
    Glean.Schema.Python.Types.TargetUses_1_key
  getName _proxy  = Glean.PredicateRef "python.TargetUses"1
  getIndex _proxy  = 204
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.targetUses_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.TargetUses_1 x k
  getFactKey = Glean.Schema.Python.Types.targetUses_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.TargetUses_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.ClassBySName where
  type KeyType Glean.Schema.Python.Types.ClassBySName =
    Glean.Schema.Python.Types.SName
  type ValueType Glean.Schema.Python.Types.ClassBySName =
    Glean.Schema.Python.Types.ClassBySName_value
  getName _proxy  = Glean.PredicateRef "python.ClassBySName"2
  getIndex _proxy  = 190
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.classBySName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.ClassBySName x k v
  getFactKey = Glean.Schema.Python.Types.classBySName_key
  getFactValue = Glean.Schema.Python.Types.classBySName_value

instance Glean.Type Glean.Schema.Python.Types.ClassBySName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.XRefIndirectTarget_key where
  buildRtsValue b (Glean.Schema.Python.Types.XRefIndirectTarget_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.XRefIndirectTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.XRefIndirectTarget_key = 'Angle.TField "import_statement" (Glean.KeyType Glean.Schema.Python.Types.ImportStatement) ('Angle.TField "target" (Glean.Schema.Python.Types.XRefTarget) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.XRefIndirectTarget where
  type KeyType Glean.Schema.Python.Types.XRefIndirectTarget =
    Glean.Schema.Python.Types.XRefIndirectTarget_key
  getName _proxy  = Glean.PredicateRef "python.XRefIndirectTarget"2
  getIndex _proxy  = 169
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.xRefIndirectTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.XRefIndirectTarget x k
  getFactKey = Glean.Schema.Python.Types.xRefIndirectTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.XRefIndirectTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.XRefIndirectTarget_1_key where
  buildRtsValue b (Glean.Schema.Python.Types.XRefIndirectTarget_1_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.XRefIndirectTarget_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.XRefIndirectTarget_1_key = 'Angle.TField "import_statement" (Glean.KeyType Glean.Schema.Python.Types.ImportStatement_1) ('Angle.TField "target" (Glean.Schema.Python.Types.XRefTarget_1) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.XRefIndirectTarget_1 where
  type KeyType Glean.Schema.Python.Types.XRefIndirectTarget_1 =
    Glean.Schema.Python.Types.XRefIndirectTarget_1_key
  getName _proxy  = Glean.PredicateRef "python.XRefIndirectTarget"1
  getIndex _proxy  = 168
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.xRefIndirectTarget_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.XRefIndirectTarget_1 x k
  getFactKey = Glean.Schema.Python.Types.xRefIndirectTarget_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.XRefIndirectTarget_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.Module_key where
  buildRtsValue b (Glean.Schema.Python.Types.Module_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Python.Types.Module_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.Module_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Python.Types.Module where
  type KeyType Glean.Schema.Python.Types.Module =
    Glean.Schema.Python.Types.Module_key
  getName _proxy  = Glean.PredicateRef "python.Module"1
  getIndex _proxy  = 166
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.module_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.Module x k
  getFactKey = Glean.Schema.Python.Types.module_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.Module where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.DeclarationBySName where
  type KeyType Glean.Schema.Python.Types.DeclarationBySName =
    Glean.Schema.Python.Types.SName
  type ValueType Glean.Schema.Python.Types.DeclarationBySName =
    Glean.Schema.Python.Types.DeclarationBySName_value
  getName _proxy  = Glean.PredicateRef "python.DeclarationBySName"2
  getIndex _proxy  = 145
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.declarationBySName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.DeclarationBySName x k v
  getFactKey = Glean.Schema.Python.Types.declarationBySName_key
  getFactValue = Glean.Schema.Python.Types.declarationBySName_value

instance Glean.Type Glean.Schema.Python.Types.DeclarationBySName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.Name where
  type KeyType Glean.Schema.Python.Types.Name = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "python.Name"1
  getIndex _proxy  = 131
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.name_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.Name x k
  getFactKey = Glean.Schema.Python.Types.name_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.Name where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.ModuleDefinition_key where
  buildRtsValue b (Glean.Schema.Python.Types.ModuleDefinition_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.ModuleDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ModuleDefinition_key = 'Angle.TField "module" (Glean.KeyType Glean.Schema.Python.Types.Module) ('Angle.TField "docstring" (Prelude.Maybe Glean.Schema.Python.Types.Docstring) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.ModuleDefinition where
  type KeyType Glean.Schema.Python.Types.ModuleDefinition =
    Glean.Schema.Python.Types.ModuleDefinition_key
  getName _proxy  = Glean.PredicateRef "python.ModuleDefinition"2
  getIndex _proxy  = 123
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.moduleDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ModuleDefinition x k
  getFactKey = Glean.Schema.Python.Types.moduleDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ModuleDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.SNameToName where
  type KeyType Glean.Schema.Python.Types.SNameToName =
    Glean.Schema.Python.Types.SName
  type ValueType Glean.Schema.Python.Types.SNameToName =
    Glean.Schema.Python.Types.SNameToName_value
  getName _proxy  = Glean.PredicateRef "python.SNameToName"2
  getIndex _proxy  = 104
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.sNameToName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.SNameToName x k v
  getFactKey = Glean.Schema.Python.Types.sNameToName_key
  getFactValue = Glean.Schema.Python.Types.sNameToName_value

instance Glean.Type Glean.Schema.Python.Types.SNameToName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.NameToSName where
  type KeyType Glean.Schema.Python.Types.NameToSName =
    Glean.Schema.Python.Types.Name
  type ValueType Glean.Schema.Python.Types.NameToSName =
    Glean.Schema.Python.Types.NameToSName_value
  getName _proxy  = Glean.PredicateRef "python.NameToSName"2
  getIndex _proxy  = 99
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.nameToSName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.NameToSName x k v
  getFactKey = Glean.Schema.Python.Types.nameToSName_key
  getFactValue = Glean.Schema.Python.Types.nameToSName_value

instance Glean.Type Glean.Schema.Python.Types.NameToSName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.Type where
  type KeyType Glean.Schema.Python.Types.Type = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "python.Type"1
  getIndex _proxy  = 81
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.type_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.Type x k
  getFactKey = Glean.Schema.Python.Types.type_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.Type where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.DeclarationWithName_key where
  buildRtsValue b (Glean.Schema.Python.Types.DeclarationWithName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.DeclarationWithName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.DeclarationWithName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "declaration" (Glean.Schema.Python.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.DeclarationWithName where
  type KeyType Glean.Schema.Python.Types.DeclarationWithName =
    Glean.Schema.Python.Types.DeclarationWithName_key
  getName _proxy  = Glean.PredicateRef "python.DeclarationWithName"2
  getIndex _proxy  = 70
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.declarationWithName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.DeclarationWithName x k
  getFactKey = Glean.Schema.Python.Types.declarationWithName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.DeclarationWithName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.FunctionBySName where
  type KeyType Glean.Schema.Python.Types.FunctionBySName =
    Glean.Schema.Python.Types.SName
  type ValueType Glean.Schema.Python.Types.FunctionBySName =
    Glean.Schema.Python.Types.FunctionBySName_value
  getName _proxy  = Glean.PredicateRef "python.FunctionBySName"2
  getIndex _proxy  = 63
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.functionBySName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.FunctionBySName x k v
  getFactKey = Glean.Schema.Python.Types.functionBySName_key
  getFactValue = Glean.Schema.Python.Types.functionBySName_value

instance Glean.Type Glean.Schema.Python.Types.FunctionBySName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.ImportStatementByAsSName_key where
  buildRtsValue b (Glean.Schema.Python.Types.ImportStatementByAsSName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.ImportStatementByAsSName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ImportStatementByAsSName_key = 'Angle.TField "sname" (Glean.KeyType Glean.Schema.Python.Types.SName) ('Angle.TField "import_" (Glean.KeyType Glean.Schema.Python.Types.ImportStatement) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.ImportStatementByAsSName where
  type KeyType Glean.Schema.Python.Types.ImportStatementByAsSName =
    Glean.Schema.Python.Types.ImportStatementByAsSName_key
  getName _proxy  = Glean.PredicateRef "python.ImportStatementByAsSName"3
  getIndex _proxy  = 61
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.importStatementByAsSName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ImportStatementByAsSName x k
  getFactKey = Glean.Schema.Python.Types.importStatementByAsSName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ImportStatementByAsSName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.ImportStatementByAsSName_2 where
  type KeyType Glean.Schema.Python.Types.ImportStatementByAsSName_2 =
    Glean.Schema.Python.Types.SName
  type ValueType Glean.Schema.Python.Types.ImportStatementByAsSName_2 =
    Glean.Schema.Python.Types.ImportStatementByAsSName_2_value
  getName _proxy  = Glean.PredicateRef "python.ImportStatementByAsSName"2
  getIndex _proxy  = 60
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.importStatementByAsSName_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.ImportStatementByAsSName_2 x k v
  getFactKey = Glean.Schema.Python.Types.importStatementByAsSName_2_key
  getFactValue = Glean.Schema.Python.Types.importStatementByAsSName_2_value

instance Glean.Type Glean.Schema.Python.Types.ImportStatementByAsSName_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key where
  buildRtsValue b (Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key = 'Angle.TField "declaration" (Glean.Schema.Python.Types.Declaration) ('Angle.TField "container" (Glean.Schema.Python.Types.Declaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.ContainingTopLevelDeclaration where
  type KeyType Glean.Schema.Python.Types.ContainingTopLevelDeclaration =
    Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key
  getName _proxy  = Glean.PredicateRef "python.ContainingTopLevelDeclaration"3
  getIndex _proxy  = 47
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.containingTopLevelDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ContainingTopLevelDeclaration x k
  getFactKey = Glean.Schema.Python.Types.containingTopLevelDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ContainingTopLevelDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.VariableDeclaration_key where
  buildRtsValue b (Glean.Schema.Python.Types.VariableDeclaration_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Python.Types.VariableDeclaration_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.VariableDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Python.Types.VariableDeclaration where
  type KeyType Glean.Schema.Python.Types.VariableDeclaration =
    Glean.Schema.Python.Types.VariableDeclaration_key
  getName _proxy  = Glean.PredicateRef "python.VariableDeclaration"1
  getIndex _proxy  = 23
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.variableDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.VariableDeclaration x k
  getFactKey = Glean.Schema.Python.Types.variableDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.VariableDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.DeclarationsByFile_key where
  buildRtsValue b (Glean.Schema.Python.Types.DeclarationsByFile_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Python.Types.DeclarationsByFile_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.DeclarationsByFile_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "span" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TField "declaration" (Glean.Schema.Python.Types.Declaration) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Python.Types.DeclarationsByFile where
  type KeyType Glean.Schema.Python.Types.DeclarationsByFile =
    Glean.Schema.Python.Types.DeclarationsByFile_key
  getName _proxy  = Glean.PredicateRef "python.DeclarationsByFile"2
  getIndex _proxy  = 21
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.declarationsByFile_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.DeclarationsByFile x k
  getFactKey = Glean.Schema.Python.Types.declarationsByFile_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.DeclarationsByFile where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.XRefsViaNameByFile_key where
  buildRtsValue b (Glean.Schema.Python.Types.XRefsViaNameByFile_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.XRefsViaNameByFile_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.XRefsViaNameByFile_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xrefs" ([Glean.Schema.Python.Types.XRefViaName]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.XRefsViaNameByFile where
  type KeyType Glean.Schema.Python.Types.XRefsViaNameByFile =
    Glean.Schema.Python.Types.XRefsViaNameByFile_key
  getName _proxy  = Glean.PredicateRef "python.XRefsViaNameByFile"2
  getIndex _proxy  = 20
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.xRefsViaNameByFile_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.XRefsViaNameByFile x k
  getFactKey = Glean.Schema.Python.Types.xRefsViaNameByFile_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.XRefsViaNameByFile where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.ImportStatementByAsName_key where
  buildRtsValue b (Glean.Schema.Python.Types.ImportStatementByAsName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.ImportStatementByAsName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.ImportStatementByAsName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "import_" (Glean.KeyType Glean.Schema.Python.Types.ImportStatement) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Python.Types.ImportStatementByAsName where
  type KeyType Glean.Schema.Python.Types.ImportStatementByAsName =
    Glean.Schema.Python.Types.ImportStatementByAsName_key
  getName _proxy  = Glean.PredicateRef "python.ImportStatementByAsName"3
  getIndex _proxy  = 8
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.importStatementByAsName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Python.Types.ImportStatementByAsName x k
  getFactKey = Glean.Schema.Python.Types.importStatementByAsName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Python.Types.ImportStatementByAsName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Python.Types.ImportStatementByAsName_2 where
  type KeyType Glean.Schema.Python.Types.ImportStatementByAsName_2 =
    Glean.Schema.Python.Types.Name
  type ValueType Glean.Schema.Python.Types.ImportStatementByAsName_2 =
    Glean.Schema.Python.Types.ImportStatementByAsName_2_value
  getName _proxy  = Glean.PredicateRef "python.ImportStatementByAsName"2
  getIndex _proxy  = 7
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Python.Types.importStatementByAsName_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k v = Glean.Schema.Python.Types.ImportStatementByAsName_2 x k v
  getFactKey = Glean.Schema.Python.Types.importStatementByAsName_2_key
  getFactValue = Glean.Schema.Python.Types.importStatementByAsName_2_value

instance Glean.Type Glean.Schema.Python.Types.ImportStatementByAsName_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Python.Types.DirectXRef where
  buildRtsValue b (Glean.Schema.Python.Types.DirectXRef x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.DirectXRef
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.DirectXRef = 'Angle.TField "target" (Glean.Schema.Python.Types.Declaration) ('Angle.TField "source" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Python.Types.XRef where
  buildRtsValue b (Glean.Schema.Python.Types.XRef x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.XRef
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.XRef = 'Angle.TField "target" (Glean.Schema.Python.Types.XRefTarget) ('Angle.TField "source" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Python.Types.XRef_1 where
  buildRtsValue b (Glean.Schema.Python.Types.XRef_1 x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.XRef_1
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.XRef_1 = 'Angle.TField "target" (Glean.Schema.Python.Types.XRefTarget_1) ('Angle.TField "source" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Python.Types.Declaration where
  buildRtsValue b (Glean.Schema.Python.Types.Declaration_cls x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.Declaration_func x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.Declaration_variable x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.Declaration_imp x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.Declaration_module x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Python.Types.Declaration_cls
    , Glean.mapD Glean.Schema.Python.Types.Declaration_func
    , Glean.mapD Glean.Schema.Python.Types.Declaration_variable
    , Glean.mapD Glean.Schema.Python.Types.Declaration_imp
    , Glean.mapD Glean.Schema.Python.Types.Declaration_module
    ]

type instance Angle.SumFields Glean.Schema.Python.Types.Declaration = 'Angle.TField "cls" (Glean.KeyType Glean.Schema.Python.Types.ClassDeclaration) ('Angle.TField "func" (Glean.KeyType Glean.Schema.Python.Types.FunctionDeclaration) ('Angle.TField "variable" (Glean.KeyType Glean.Schema.Python.Types.VariableDeclaration) ('Angle.TField "imp" (Glean.KeyType Glean.Schema.Python.Types.ImportStatement) ('Angle.TField "module" (Glean.KeyType Glean.Schema.Python.Types.Module) ('Angle.TNoFields)))))

instance Glean.SumBranches Glean.Schema.Python.Types.ClassDeclaration Glean.Schema.Python.Types.Declaration where
  injectBranch = Glean.Schema.Python.Types.Declaration_cls
  projectBranch (Glean.Schema.Python.Types.Declaration_cls x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.FunctionDeclaration Glean.Schema.Python.Types.Declaration where
  injectBranch = Glean.Schema.Python.Types.Declaration_func
  projectBranch (Glean.Schema.Python.Types.Declaration_func x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.VariableDeclaration Glean.Schema.Python.Types.Declaration where
  injectBranch = Glean.Schema.Python.Types.Declaration_variable
  projectBranch (Glean.Schema.Python.Types.Declaration_variable x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.ImportStatement Glean.Schema.Python.Types.Declaration where
  injectBranch = Glean.Schema.Python.Types.Declaration_imp
  projectBranch (Glean.Schema.Python.Types.Declaration_imp x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.Module Glean.Schema.Python.Types.Declaration where
  injectBranch = Glean.Schema.Python.Types.Declaration_module
  projectBranch (Glean.Schema.Python.Types.Declaration_module x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Python.Types.Declaration_1 where
  buildRtsValue b (Glean.Schema.Python.Types.Declaration_1_cls x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.Declaration_1_func x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.Declaration_1_variable x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Python.Types.Declaration_1_cls
    , Glean.mapD Glean.Schema.Python.Types.Declaration_1_func
    , Glean.mapD Glean.Schema.Python.Types.Declaration_1_variable
    ]

type instance Angle.SumFields Glean.Schema.Python.Types.Declaration_1 = 'Angle.TField "cls" (Glean.KeyType Glean.Schema.Python.Types.ClassDeclaration) ('Angle.TField "func" (Glean.KeyType Glean.Schema.Python.Types.FunctionDeclaration) ('Angle.TField "variable" (Glean.KeyType Glean.Schema.Python.Types.VariableDeclaration) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Python.Types.ClassDeclaration Glean.Schema.Python.Types.Declaration_1 where
  injectBranch = Glean.Schema.Python.Types.Declaration_1_cls
  projectBranch (Glean.Schema.Python.Types.Declaration_1_cls x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.FunctionDeclaration Glean.Schema.Python.Types.Declaration_1 where
  injectBranch = Glean.Schema.Python.Types.Declaration_1_func
  projectBranch (Glean.Schema.Python.Types.Declaration_1_func x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.VariableDeclaration Glean.Schema.Python.Types.Declaration_1 where
  injectBranch = Glean.Schema.Python.Types.Declaration_1_variable
  projectBranch (Glean.Schema.Python.Types.Declaration_1_variable x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Python.Types.XRefViaName where
  buildRtsValue b (Glean.Schema.Python.Types.XRefViaName x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Python.Types.XRefViaName
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.XRefViaName = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "source" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Python.Types.Docstring where
  buildRtsValue b (Glean.Schema.Python.Types.Docstring x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Python.Types.Docstring
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.Docstring = 'Angle.TField "location" (Glean.Schema.Src.Types.ByteSpan) ('Angle.TNoFields)

instance Glean.Type Glean.Schema.Python.Types.Parameter where
  buildRtsValue b (Glean.Schema.Python.Types.Parameter x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Python.Types.Parameter
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Python.Types.Parameter = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Python.Types.Name) ('Angle.TField "type" (Prelude.Maybe (Glean.KeyType Glean.Schema.Python.Types.Type)) ('Angle.TField "value" (Prelude.Maybe Data.Text.Text) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Python.Types.XRefTarget where
  buildRtsValue b (Glean.Schema.Python.Types.XRefTarget_declaration x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.XRefTarget_indirect x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.XRefTarget_unknown x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Python.Types.XRefTarget_declaration
    , Glean.mapD Glean.Schema.Python.Types.XRefTarget_indirect
    , Glean.mapD Glean.Schema.Python.Types.XRefTarget_unknown
    ]

type instance Angle.SumFields Glean.Schema.Python.Types.XRefTarget = 'Angle.TField "declaration" (Glean.Schema.Python.Types.Declaration) ('Angle.TField "indirect" (Glean.KeyType Glean.Schema.Python.Types.XRefIndirectTarget) ('Angle.TField "unknown" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Python.Types.Declaration Glean.Schema.Python.Types.XRefTarget where
  injectBranch = Glean.Schema.Python.Types.XRefTarget_declaration
  projectBranch (Glean.Schema.Python.Types.XRefTarget_declaration x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.XRefIndirectTarget Glean.Schema.Python.Types.XRefTarget where
  injectBranch = Glean.Schema.Python.Types.XRefTarget_indirect
  projectBranch (Glean.Schema.Python.Types.XRefTarget_indirect x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Builtin.Types.Unit Glean.Schema.Python.Types.XRefTarget where
  injectBranch = Glean.Schema.Python.Types.XRefTarget_unknown
  projectBranch (Glean.Schema.Python.Types.XRefTarget_unknown x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Python.Types.XRefTarget_1 where
  buildRtsValue b (Glean.Schema.Python.Types.XRefTarget_1_declaration x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.XRefTarget_1_module x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.XRefTarget_1_indirect x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Python.Types.XRefTarget_1_unknown x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Python.Types.XRefTarget_1_declaration
    , Glean.mapD Glean.Schema.Python.Types.XRefTarget_1_module
    , Glean.mapD Glean.Schema.Python.Types.XRefTarget_1_indirect
    , Glean.mapD Glean.Schema.Python.Types.XRefTarget_1_unknown
    ]

type instance Angle.SumFields Glean.Schema.Python.Types.XRefTarget_1 = 'Angle.TField "declaration" (Glean.Schema.Python.Types.Declaration_1) ('Angle.TField "module" (Glean.KeyType Glean.Schema.Python.Types.Module) ('Angle.TField "indirect" (Glean.KeyType Glean.Schema.Python.Types.XRefIndirectTarget_1) ('Angle.TField "unknown" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields))))

instance Glean.SumBranches Glean.Schema.Python.Types.Declaration_1 Glean.Schema.Python.Types.XRefTarget_1 where
  injectBranch = Glean.Schema.Python.Types.XRefTarget_1_declaration
  projectBranch (Glean.Schema.Python.Types.XRefTarget_1_declaration x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.Module Glean.Schema.Python.Types.XRefTarget_1 where
  injectBranch = Glean.Schema.Python.Types.XRefTarget_1_module
  projectBranch (Glean.Schema.Python.Types.XRefTarget_1_module x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Python.Types.XRefIndirectTarget_1 Glean.Schema.Python.Types.XRefTarget_1 where
  injectBranch = Glean.Schema.Python.Types.XRefTarget_1_indirect
  projectBranch (Glean.Schema.Python.Types.XRefTarget_1_indirect x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Builtin.Types.Unit Glean.Schema.Python.Types.XRefTarget_1 where
  injectBranch = Glean.Schema.Python.Types.XRefTarget_1_unknown
  projectBranch (Glean.Schema.Python.Types.XRefTarget_1_unknown x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
