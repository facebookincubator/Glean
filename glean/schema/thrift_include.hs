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
import qualified Glean.Schema.Cxx1.Types
import qualified Glean.Schema.Scm.Types
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Thrift.Types.ServiceName_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ServiceName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.ServiceName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ServiceName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "locName" (Glean.Schema.Thrift.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.ServiceName where
  type KeyType Glean.Schema.Thrift.Types.ServiceName =
    Glean.Schema.Thrift.Types.ServiceName_key
  getName _proxy  = Glean.PredicateRef "thrift.ServiceName"2
  getIndex _proxy  = 480
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.serviceName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ServiceName x k
  getFactKey = Glean.Schema.Thrift.Types.serviceName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ServiceName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FunctionName_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FunctionName_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.FunctionName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FunctionName_key = 'Angle.TField "service_" (Glean.KeyType Glean.Schema.Thrift.Types.ServiceName) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.Identifier) ('Angle.TField "locName" (Glean.Schema.Thrift.Types.Loc) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.FunctionName where
  type KeyType Glean.Schema.Thrift.Types.FunctionName =
    Glean.Schema.Thrift.Types.FunctionName_key
  getName _proxy  = Glean.PredicateRef "thrift.FunctionName"2
  getIndex _proxy  = 458
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.functionName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FunctionName x k
  getFactKey = Glean.Schema.Thrift.Types.functionName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FunctionName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.TypeSpecification_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.TypeSpecification_key_primitive x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.TypeSpecification_key_container x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.TypeSpecification_key_named x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Thrift.Types.TypeSpecification_key_primitive
    , Glean.mapD Glean.Schema.Thrift.Types.TypeSpecification_key_container
    , Glean.mapD Glean.Schema.Thrift.Types.TypeSpecification_key_named
    ]

type instance Angle.SumFields Glean.Schema.Thrift.Types.TypeSpecification_key = 'Angle.TField "primitive" (Glean.Schema.Thrift.Types.PrimitiveType) ('Angle.TField "container" (Glean.Schema.Thrift.Types.ContainerType) ('Angle.TField "named" (Glean.Schema.Thrift.Types.NamedType) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Thrift.Types.PrimitiveType Glean.Schema.Thrift.Types.TypeSpecification_key where
  injectBranch = Glean.Schema.Thrift.Types.TypeSpecification_key_primitive
  projectBranch (Glean.Schema.Thrift.Types.TypeSpecification_key_primitive x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.ContainerType Glean.Schema.Thrift.Types.TypeSpecification_key where
  injectBranch = Glean.Schema.Thrift.Types.TypeSpecification_key_container
  projectBranch (Glean.Schema.Thrift.Types.TypeSpecification_key_container x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.NamedType Glean.Schema.Thrift.Types.TypeSpecification_key where
  injectBranch = Glean.Schema.Thrift.Types.TypeSpecification_key_named
  projectBranch (Glean.Schema.Thrift.Types.TypeSpecification_key_named x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Predicate Glean.Schema.Thrift.Types.TypeSpecification where
  type KeyType Glean.Schema.Thrift.Types.TypeSpecification =
    Glean.Schema.Thrift.Types.TypeSpecification_key
  getName _proxy  = Glean.PredicateRef "thrift.TypeSpecification"3
  getIndex _proxy  = 450
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.typeSpecification_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.TypeSpecification x k
  getFactKey = Glean.Schema.Thrift.Types.typeSpecification_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.TypeSpecification where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.MangleLang where
  type KeyType Glean.Schema.Thrift.Types.MangleLang = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "thrift.MangleLang"1
  getIndex _proxy  = 431
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.mangleLang_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.MangleLang x k
  getFactKey = Glean.Schema.Thrift.Types.mangleLang_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.MangleLang where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ExceptionName_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ExceptionName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.ExceptionName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ExceptionName_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "locName" (Glean.Schema.Thrift.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.ExceptionName where
  type KeyType Glean.Schema.Thrift.Types.ExceptionName =
    Glean.Schema.Thrift.Types.ExceptionName_key
  getName _proxy  = Glean.PredicateRef "thrift.ExceptionName"2
  getIndex _proxy  = 415
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.exceptionName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ExceptionName x k
  getFactKey = Glean.Schema.Thrift.Types.exceptionName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ExceptionName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.UnionType_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.UnionType_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.UnionType_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.UnionType_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "alts" ([Glean.Schema.Thrift.Types.UnqualField]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.UnionType where
  type KeyType Glean.Schema.Thrift.Types.UnionType =
    Glean.Schema.Thrift.Types.UnionType_key
  getName _proxy  = Glean.PredicateRef "thrift.UnionType"3
  getIndex _proxy  = 411
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.unionType_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.UnionType x k
  getFactKey = Glean.Schema.Thrift.Types.unionType_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.UnionType where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.Identifier where
  type KeyType Glean.Schema.Thrift.Types.Identifier = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "thrift.Identifier"1
  getIndex _proxy  = 410
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.identifier_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.Identifier x k
  getFactKey = Glean.Schema.Thrift.Types.identifier_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.Identifier where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.HackMethod_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.HackMethod_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.HackMethod_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.HackMethod_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.HackName) ('Angle.TField "record" (Glean.KeyType Glean.Schema.Thrift.Types.HackRecord) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.HackMethod where
  type KeyType Glean.Schema.Thrift.Types.HackMethod =
    Glean.Schema.Thrift.Types.HackMethod_key
  getName _proxy  = Glean.PredicateRef "thrift.HackMethod"1
  getIndex _proxy  = 384
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.hackMethod_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.HackMethod x k
  getFactKey = Glean.Schema.Thrift.Types.hackMethod_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.HackMethod where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.OutputTarget_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.OutputTarget_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.OutputTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.OutputTarget_key = 'Angle.TField "compile" (Glean.KeyType Glean.Schema.Thrift.Types.CompileTarget) ('Angle.TField "output" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TField "out" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.OutputTarget where
  type KeyType Glean.Schema.Thrift.Types.OutputTarget =
    Glean.Schema.Thrift.Types.OutputTarget_key
  getName _proxy  = Glean.PredicateRef "thrift.OutputTarget"3
  getIndex _proxy  = 378
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.outputTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.OutputTarget x k
  getFactKey = Glean.Schema.Thrift.Types.outputTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.OutputTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.QualName_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.QualName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.QualName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.QualName_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.Identifier) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.QualName where
  type KeyType Glean.Schema.Thrift.Types.QualName =
    Glean.Schema.Thrift.Types.QualName_key
  getName _proxy  = Glean.PredicateRef "thrift.QualName"2
  getIndex _proxy  = 369
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.qualName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.QualName x k
  getFactKey = Glean.Schema.Thrift.Types.qualName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.QualName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.File where
  type KeyType Glean.Schema.Thrift.Types.File = Glean.Schema.Src.Types.File
  getName _proxy  = Glean.PredicateRef "thrift.File"2
  getIndex _proxy  = 368
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.file_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.File x k
  getFactKey = Glean.Schema.Thrift.Types.file_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.File where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.HackName where
  type KeyType Glean.Schema.Thrift.Types.HackName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "thrift.HackName"1
  getIndex _proxy  = 364
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.hackName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.HackName x k
  getFactKey = Glean.Schema.Thrift.Types.hackName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.HackName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.Constant_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.Constant_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.Constant_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.Constant_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "locName" (Glean.Schema.Thrift.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.Constant where
  type KeyType Glean.Schema.Thrift.Types.Constant =
    Glean.Schema.Thrift.Types.Constant_key
  getName _proxy  = Glean.PredicateRef "thrift.Constant"2
  getIndex _proxy  = 363
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.constant_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.Constant x k
  getFactKey = Glean.Schema.Thrift.Types.constant_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.Constant where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonFunction_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonFunction_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonFunction_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonFunction_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.PythonName) ('Angle.TField "module" (Glean.KeyType Glean.Schema.Thrift.Types.PythonModule) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonFunction where
  type KeyType Glean.Schema.Thrift.Types.PythonFunction =
    Glean.Schema.Thrift.Types.PythonFunction_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonFunction"1
  getIndex _proxy  = 361
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonFunction_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonFunction x k
  getFactKey = Glean.Schema.Thrift.Types.pythonFunction_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonFunction where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.IncludeStatement_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.IncludeStatement_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.IncludeStatement_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.IncludeStatement_key = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "source" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "locSource" (Glean.Schema.Thrift.Types.Loc) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.IncludeStatement where
  type KeyType Glean.Schema.Thrift.Types.IncludeStatement =
    Glean.Schema.Thrift.Types.IncludeStatement_key
  getName _proxy  = Glean.PredicateRef "thrift.IncludeStatement"2
  getIndex _proxy  = 348
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.includeStatement_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.IncludeStatement x k
  getFactKey = Glean.Schema.Thrift.Types.includeStatement_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.IncludeStatement where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonClassContains_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonClassContains_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonClassContains_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonClassContains_key = 'Angle.TField "class_" (Glean.KeyType Glean.Schema.Thrift.Types.PythonClass) ('Angle.TField "methods" ([Glean.KeyType Glean.Schema.Thrift.Types.PythonMethod]) ('Angle.TField "fields" ([Glean.KeyType Glean.Schema.Thrift.Types.PythonField]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonClassContains where
  type KeyType Glean.Schema.Thrift.Types.PythonClassContains =
    Glean.Schema.Thrift.Types.PythonClassContains_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonClassContains"1
  getIndex _proxy  = 341
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonClassContains_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonClassContains x k
  getFactKey = Glean.Schema.Thrift.Types.pythonClassContains_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonClassContains where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.TargetX_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.TargetX_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.TargetX_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.TargetX_key = 'Angle.TField "target" (Glean.Schema.Thrift.Types.XRefTarget) ('Angle.TField "fileRef" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "locRef" (Glean.Schema.Thrift.Types.Loc) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.TargetX where
  type KeyType Glean.Schema.Thrift.Types.TargetX =
    Glean.Schema.Thrift.Types.TargetX_key
  getName _proxy  = Glean.PredicateRef "thrift.TargetX"2
  getIndex _proxy  = 335
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.targetX_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.TargetX x k
  getFactKey = Glean.Schema.Thrift.Types.targetX_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.TargetX where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.NamespaceName where
  type KeyType Glean.Schema.Thrift.Types.NamespaceName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "thrift.NamespaceName"1
  getIndex _proxy  = 324
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.namespaceName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.NamespaceName x k
  getFactKey = Glean.Schema.Thrift.Types.namespaceName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.NamespaceName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.HackRecord_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.HackRecord_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.HackRecord_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.HackRecord_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.HackName) ('Angle.TField "kind" (Glean.Schema.Thrift.Types.HackRecordKind) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.HackRecord where
  type KeyType Glean.Schema.Thrift.Types.HackRecord =
    Glean.Schema.Thrift.Types.HackRecord_key
  getName _proxy  = Glean.PredicateRef "thrift.HackRecord"1
  getIndex _proxy  = 311
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.hackRecord_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.HackRecord x k
  getFactKey = Glean.Schema.Thrift.Types.hackRecord_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.HackRecord where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FromCpp2_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FromCpp2_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.FromCpp2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FromCpp2_key = 'Angle.TField "cpp2" (Glean.Schema.Thrift.Types.Cpp2Item) ('Angle.TField "thrift" (Glean.Schema.Thrift.Types.Item) ('Angle.TField "role" (Glean.Schema.Thrift.Types.GenRole) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.FromCpp2 where
  type KeyType Glean.Schema.Thrift.Types.FromCpp2 =
    Glean.Schema.Thrift.Types.FromCpp2_key
  getName _proxy  = Glean.PredicateRef "thrift.FromCpp2"1
  getIndex _proxy  = 299
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.fromCpp2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FromCpp2 x k
  getFactKey = Glean.Schema.Thrift.Types.fromCpp2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FromCpp2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FromHack_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FromHack_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.FromHack_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FromHack_key = 'Angle.TField "hack" (Glean.Schema.Thrift.Types.HackKind) ('Angle.TField "thrift" (Glean.Schema.Thrift.Types.Item) ('Angle.TField "role" (Glean.Schema.Thrift.Types.GenRole) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.FromHack where
  type KeyType Glean.Schema.Thrift.Types.FromHack =
    Glean.Schema.Thrift.Types.FromHack_key
  getName _proxy  = Glean.PredicateRef "thrift.FromHack"1
  getIndex _proxy  = 295
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.fromHack_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FromHack x k
  getFactKey = Glean.Schema.Thrift.Types.fromHack_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FromHack where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.TypeDefType_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.TypeDefType_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.TypeDefType_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.TypeDefType_key = 'Angle.TField "alias" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "type_" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.TypeDefType where
  type KeyType Glean.Schema.Thrift.Types.TypeDefType =
    Glean.Schema.Thrift.Types.TypeDefType_key
  getName _proxy  = Glean.PredicateRef "thrift.TypeDefType"3
  getIndex _proxy  = 289
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.typeDefType_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.TypeDefType x k
  getFactKey = Glean.Schema.Thrift.Types.typeDefType_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.TypeDefType where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FunctionSpecification_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FunctionSpecification_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Thrift.Types.FunctionSpecification_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FunctionSpecification_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.FunctionName) ('Angle.TField "result" (Glean.Schema.Thrift.Types.ResultType) ('Angle.TField "arguments" ([Glean.Schema.Thrift.Types.UnqualField]) ('Angle.TField "throws_" ([Glean.Schema.Thrift.Types.ExceptionSpecification]) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Thrift.Types.FunctionSpecification where
  type KeyType Glean.Schema.Thrift.Types.FunctionSpecification =
    Glean.Schema.Thrift.Types.FunctionSpecification_key
  getName _proxy  = Glean.PredicateRef "thrift.FunctionSpecification"3
  getIndex _proxy  = 287
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.functionSpecification_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FunctionSpecification x k
  getFactKey = Glean.Schema.Thrift.Types.functionSpecification_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FunctionSpecification where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ToPython_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ToPython_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.ToPython_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ToPython_key = 'Angle.TField "thrift" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "lang" (Glean.KeyType Glean.Schema.Thrift.Types.Lang) ('Angle.TField "python" ([Glean.KeyType Glean.Schema.Thrift.Types.FromPython]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.ToPython where
  type KeyType Glean.Schema.Thrift.Types.ToPython =
    Glean.Schema.Thrift.Types.ToPython_key
  getName _proxy  = Glean.PredicateRef "thrift.ToPython"1
  getIndex _proxy  = 276
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.toPython_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ToPython x k
  getFactKey = Glean.Schema.Thrift.Types.toPython_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ToPython where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FromPython_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FromPython_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Thrift.Types.FromPython_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FromPython_key = 'Angle.TField "python" (Glean.Schema.Thrift.Types.PythonItem) ('Angle.TField "thrift" (Glean.Schema.Thrift.Types.Item) ('Angle.TField "role" (Glean.Schema.Thrift.Types.GenRole) ('Angle.TField "lang" (Glean.KeyType Glean.Schema.Thrift.Types.Lang) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Thrift.Types.FromPython where
  type KeyType Glean.Schema.Thrift.Types.FromPython =
    Glean.Schema.Thrift.Types.FromPython_key
  getName _proxy  = Glean.PredicateRef "thrift.FromPython"1
  getIndex _proxy  = 258
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.fromPython_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FromPython x k
  getFactKey = Glean.Schema.Thrift.Types.fromPython_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FromPython where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ToCpp2_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ToCpp2_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.ToCpp2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ToCpp2_key = 'Angle.TField "thrift" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "cpp2" ([Glean.KeyType Glean.Schema.Thrift.Types.FromCpp2]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.ToCpp2 where
  type KeyType Glean.Schema.Thrift.Types.ToCpp2 =
    Glean.Schema.Thrift.Types.ToCpp2_key
  getName _proxy  = Glean.PredicateRef "thrift.ToCpp2"1
  getIndex _proxy  = 253
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.toCpp2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ToCpp2 x k
  getFactKey = Glean.Schema.Thrift.Types.toCpp2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ToCpp2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonClass_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonClass_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonClass_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonClass_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.PythonName) ('Angle.TField "module" (Glean.KeyType Glean.Schema.Thrift.Types.PythonModule) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonClass where
  type KeyType Glean.Schema.Thrift.Types.PythonClass =
    Glean.Schema.Thrift.Types.PythonClass_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonClass"1
  getIndex _proxy  = 247
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonClass_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonClass x k
  getFactKey = Glean.Schema.Thrift.Types.pythonClass_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonClass where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ToHack_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ToHack_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.ToHack_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ToHack_key = 'Angle.TField "thrift" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "hack" ([Glean.KeyType Glean.Schema.Thrift.Types.FromHack]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.ToHack where
  type KeyType Glean.Schema.Thrift.Types.ToHack =
    Glean.Schema.Thrift.Types.ToHack_key
  getName _proxy  = Glean.PredicateRef "thrift.ToHack"1
  getIndex _proxy  = 242
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.toHack_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ToHack x k
  getFactKey = Glean.Schema.Thrift.Types.toHack_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ToHack where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.IncludeSpecial_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.IncludeSpecial_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.IncludeSpecial_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.IncludeSpecial_key = 'Angle.TField "source" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "special" (Data.Text.Text) ('Angle.TField "target" (Glean.KeyType Glean.Schema.Thrift.Types.IncludeSplice) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.IncludeSpecial where
  type KeyType Glean.Schema.Thrift.Types.IncludeSpecial =
    Glean.Schema.Thrift.Types.IncludeSpecial_key
  getName _proxy  = Glean.PredicateRef "thrift.IncludeSpecial"2
  getIndex _proxy  = 236
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.includeSpecial_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.IncludeSpecial x k
  getFactKey = Glean.Schema.Thrift.Types.includeSpecial_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.IncludeSpecial where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonModuleContains_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonModuleContains_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonModuleContains_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonModuleContains_key = 'Angle.TField "module" (Glean.KeyType Glean.Schema.Thrift.Types.PythonModule) ('Angle.TField "classes" ([Glean.KeyType Glean.Schema.Thrift.Types.PythonClass]) ('Angle.TField "functions" ([Glean.KeyType Glean.Schema.Thrift.Types.PythonFunction]) ('Angle.TField "values" ([Glean.KeyType Glean.Schema.Thrift.Types.PythonValue]) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonModuleContains where
  type KeyType Glean.Schema.Thrift.Types.PythonModuleContains =
    Glean.Schema.Thrift.Types.PythonModuleContains_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonModuleContains"1
  getIndex _proxy  = 217
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonModuleContains_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonModuleContains x k
  getFactKey = Glean.Schema.Thrift.Types.pythonModuleContains_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonModuleContains where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.NamedDecl_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.NamedDecl_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.NamedDecl_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.NamedDecl_key = 'Angle.TField "name" (Glean.Schema.Thrift.Types.NamedType) ('Angle.TField "locName" (Glean.Schema.Thrift.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.NamedDecl where
  type KeyType Glean.Schema.Thrift.Types.NamedDecl =
    Glean.Schema.Thrift.Types.NamedDecl_key
  getName _proxy  = Glean.PredicateRef "thrift.NamedDecl"2
  getIndex _proxy  = 216
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.namedDecl_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.NamedDecl x k
  getFactKey = Glean.Schema.Thrift.Types.namedDecl_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.NamedDecl where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.HackMap_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.HackMap_key x1 x2 x3 x4 x5 x6 x7) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
  decodeRtsValue = Glean.Schema.Thrift.Types.HackMap_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.HackMap_key = 'Angle.TField "source" (Prelude.Maybe (Glean.KeyType Glean.Schema.Src.Types.File)) ('Angle.TField "repoCode" (Glean.KeyType Glean.Schema.Scm.Types.RepoName) ('Angle.TField "path" (Data.Text.Text) ('Angle.TField "kind" (Glean.Schema.Thrift.Types.HackMapKind) ('Angle.TField "mangledsvcs" (Prelude.Bool) ('Angle.TField "rest" (Prelude.Bool) ('Angle.TField "server" (Prelude.Bool) ('Angle.TNoFields)))))))

instance Glean.Predicate Glean.Schema.Thrift.Types.HackMap where
  type KeyType Glean.Schema.Thrift.Types.HackMap =
    Glean.Schema.Thrift.Types.HackMap_key
  getName _proxy  = Glean.PredicateRef "thrift.HackMap"1
  getIndex _proxy  = 214
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.hackMap_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.HackMap x k
  getFactKey = Glean.Schema.Thrift.Types.hackMap_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.HackMap where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ServiceDefinition_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ServiceDefinition_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.ServiceDefinition_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ServiceDefinition_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.ServiceName) ('Angle.TField "functions" ([Glean.KeyType Glean.Schema.Thrift.Types.FunctionSpecification]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.ServiceDefinition where
  type KeyType Glean.Schema.Thrift.Types.ServiceDefinition =
    Glean.Schema.Thrift.Types.ServiceDefinition_key
  getName _proxy  = Glean.PredicateRef "thrift.ServiceDefinition"3
  getIndex _proxy  = 213
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.serviceDefinition_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ServiceDefinition x k
  getFactKey = Glean.Schema.Thrift.Types.serviceDefinition_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ServiceDefinition where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.NamespaceValue where
  type KeyType Glean.Schema.Thrift.Types.NamespaceValue = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "thrift.NamespaceValue"1
  getIndex _proxy  = 211
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.namespaceValue_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.NamespaceValue x k
  getFactKey = Glean.Schema.Thrift.Types.namespaceValue_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.NamespaceValue where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonModuleFile_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonModuleFile_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonModuleFile_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonModuleFile_key = 'Angle.TField "module" (Glean.KeyType Glean.Schema.Thrift.Types.PythonModule) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonModuleFile where
  type KeyType Glean.Schema.Thrift.Types.PythonModuleFile =
    Glean.Schema.Thrift.Types.PythonModuleFile_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonModuleFile"1
  getIndex _proxy  = 205
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonModuleFile_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonModuleFile x k
  getFactKey = Glean.Schema.Thrift.Types.pythonModuleFile_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonModuleFile where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.EnumValue_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.EnumValue_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.EnumValue_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.EnumValue_key = 'Angle.TField "enum_" (Glean.Schema.Thrift.Types.NamedType) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.Identifier) ('Angle.TField "locName" (Glean.Schema.Thrift.Types.Loc) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.EnumValue where
  type KeyType Glean.Schema.Thrift.Types.EnumValue =
    Glean.Schema.Thrift.Types.EnumValue_key
  getName _proxy  = Glean.PredicateRef "thrift.EnumValue"2
  getIndex _proxy  = 200
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.enumValue_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.EnumValue x k
  getFactKey = Glean.Schema.Thrift.Types.enumValue_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.EnumValue where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.CompileTarget_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.CompileTarget_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.CompileTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.CompileTarget_key = 'Angle.TField "includes" (Glean.KeyType Glean.Schema.Thrift.Types.FileTarget) ('Angle.TField "lang" (Glean.Schema.Thrift.Types.FbcodeLang) ('Angle.TField "compile" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.CompileTarget where
  type KeyType Glean.Schema.Thrift.Types.CompileTarget =
    Glean.Schema.Thrift.Types.CompileTarget_key
  getName _proxy  = Glean.PredicateRef "thrift.CompileTarget"3
  getIndex _proxy  = 189
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.compileTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.CompileTarget x k
  getFactKey = Glean.Schema.Thrift.Types.compileTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.CompileTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonFileModule_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonFileModule_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonFileModule_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonFileModule_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "module" (Glean.KeyType Glean.Schema.Thrift.Types.PythonModule) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonFileModule where
  type KeyType Glean.Schema.Thrift.Types.PythonFileModule =
    Glean.Schema.Thrift.Types.PythonFileModule_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonFileModule"1
  getIndex _proxy  = 185
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonFileModule_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonFileModule x k
  getFactKey = Glean.Schema.Thrift.Types.pythonFileModule_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonFileModule where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonName where
  type KeyType Glean.Schema.Thrift.Types.PythonName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "thrift.PythonName"1
  getIndex _proxy  = 176
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonName x k
  getFactKey = Glean.Schema.Thrift.Types.pythonName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.Namespace_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.Namespace_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Thrift.Types.Namespace_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.Namespace_key = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.NamespaceName) ('Angle.TField "namespace_" (Glean.KeyType Glean.Schema.Thrift.Types.NamespaceValue) ('Angle.TField "quoted" (Prelude.Bool) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Thrift.Types.Namespace where
  type KeyType Glean.Schema.Thrift.Types.Namespace =
    Glean.Schema.Thrift.Types.Namespace_key
  getName _proxy  = Glean.PredicateRef "thrift.Namespace"2
  getIndex _proxy  = 165
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.namespace_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.Namespace x k
  getFactKey = Glean.Schema.Thrift.Types.namespace_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.Namespace where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonMethod_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonMethod_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonMethod_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonMethod_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.PythonName) ('Angle.TField "class_" (Glean.KeyType Glean.Schema.Thrift.Types.PythonClass) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonMethod where
  type KeyType Glean.Schema.Thrift.Types.PythonMethod =
    Glean.Schema.Thrift.Types.PythonMethod_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonMethod"1
  getIndex _proxy  = 156
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonMethod_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonMethod x k
  getFactKey = Glean.Schema.Thrift.Types.pythonMethod_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonMethod where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonModule where
  type KeyType Glean.Schema.Thrift.Types.PythonModule =
    Glean.Schema.Thrift.Types.PythonName
  getName _proxy  = Glean.PredicateRef "thrift.PythonModule"1
  getIndex _proxy  = 136
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonModule_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonModule x k
  getFactKey = Glean.Schema.Thrift.Types.pythonModule_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonModule where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ConstantType_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ConstantType_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.ConstantType_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ConstantType_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "type_" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.ConstantType where
  type KeyType Glean.Schema.Thrift.Types.ConstantType =
    Glean.Schema.Thrift.Types.ConstantType_key
  getName _proxy  = Glean.PredicateRef "thrift.ConstantType"3
  getIndex _proxy  = 135
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.constantType_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ConstantType x k
  getFactKey = Glean.Schema.Thrift.Types.constantType_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ConstantType where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FileError_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FileError_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.FileError_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FileError_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "error" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.FileError where
  type KeyType Glean.Schema.Thrift.Types.FileError =
    Glean.Schema.Thrift.Types.FileError_key
  getName _proxy  = Glean.PredicateRef "thrift.FileError"2
  getIndex _proxy  = 134
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.fileError_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FileError x k
  getFactKey = Glean.Schema.Thrift.Types.fileError_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FileError where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonField_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonField_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonField_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonField_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.PythonName) ('Angle.TField "class_" (Glean.KeyType Glean.Schema.Thrift.Types.PythonClass) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonField where
  type KeyType Glean.Schema.Thrift.Types.PythonField =
    Glean.Schema.Thrift.Types.PythonField_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonField"1
  getIndex _proxy  = 132
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonField_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonField x k
  getFactKey = Glean.Schema.Thrift.Types.pythonField_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonField where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.EnumerationType_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.EnumerationType_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.EnumerationType_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.EnumerationType_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "value" ([Glean.KeyType Glean.Schema.Thrift.Types.EnumValueDef]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.EnumerationType where
  type KeyType Glean.Schema.Thrift.Types.EnumerationType =
    Glean.Schema.Thrift.Types.EnumerationType_key
  getName _proxy  = Glean.PredicateRef "thrift.EnumerationType"3
  getIndex _proxy  = 114
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.enumerationType_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.EnumerationType x k
  getFactKey = Glean.Schema.Thrift.Types.enumerationType_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.EnumerationType where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FileOutput_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FileOutput_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.FileOutput_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FileOutput_key = 'Angle.TField "output" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "origin" (Glean.KeyType Glean.Schema.Thrift.Types.OutputTarget) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.FileOutput where
  type KeyType Glean.Schema.Thrift.Types.FileOutput =
    Glean.Schema.Thrift.Types.FileOutput_key
  getName _proxy  = Glean.PredicateRef "thrift.FileOutput"3
  getIndex _proxy  = 83
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.fileOutput_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FileOutput x k
  getFactKey = Glean.Schema.Thrift.Types.fileOutput_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FileOutput where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ServiceParent_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ServiceParent_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.ServiceParent_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ServiceParent_key = 'Angle.TField "child" (Glean.KeyType Glean.Schema.Thrift.Types.ServiceName) ('Angle.TField "parent" (Glean.KeyType Glean.Schema.Thrift.Types.ServiceName) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.ServiceParent where
  type KeyType Glean.Schema.Thrift.Types.ServiceParent =
    Glean.Schema.Thrift.Types.ServiceParent_key
  getName _proxy  = Glean.PredicateRef "thrift.ServiceParent"2
  getIndex _proxy  = 79
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.serviceParent_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ServiceParent x k
  getFactKey = Glean.Schema.Thrift.Types.serviceParent_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ServiceParent where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FileXRefs_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FileXRefs_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.FileXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FileXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "targets" ([Glean.Schema.Thrift.Types.Target]) ('Angle.TField "xrefs" ([Glean.Schema.Thrift.Types.XRef]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.FileXRefs where
  type KeyType Glean.Schema.Thrift.Types.FileXRefs =
    Glean.Schema.Thrift.Types.FileXRefs_key
  getName _proxy  = Glean.PredicateRef "thrift.FileXRefs"2
  getIndex _proxy  = 71
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.fileXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FileXRefs x k
  getFactKey = Glean.Schema.Thrift.Types.fileXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FileXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.IncludeSplice where
  type KeyType Glean.Schema.Thrift.Types.IncludeSplice = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "thrift.IncludeSplice"2
  getIndex _proxy  = 67
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.includeSplice_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.IncludeSplice x k
  getFactKey = Glean.Schema.Thrift.Types.includeSplice_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.IncludeSplice where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Thrift.Types.Lang where
  type KeyType Glean.Schema.Thrift.Types.Lang = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "thrift.Lang"1
  getIndex _proxy  = 53
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.lang_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.Lang x k
  getFactKey = Glean.Schema.Thrift.Types.lang_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.Lang where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.Mangle_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.Mangle_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.Mangle_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.Mangle_key = 'Angle.TField "from" (Glean.KeyType Glean.Schema.Scm.Types.Commit) ('Angle.TField "to" (Glean.KeyType Glean.Schema.Scm.Types.Commit) ('Angle.TField "lang" (Glean.KeyType Glean.Schema.Thrift.Types.MangleLang) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Thrift.Types.Mangle where
  type KeyType Glean.Schema.Thrift.Types.Mangle =
    Glean.Schema.Thrift.Types.Mangle_key
  getName _proxy  = Glean.PredicateRef "thrift.Mangle"1
  getIndex _proxy  = 47
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.mangle_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.Mangle x k
  getFactKey = Glean.Schema.Thrift.Types.mangle_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.Mangle where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ServiceChild_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.ServiceChild_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.ServiceChild_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ServiceChild_key = 'Angle.TField "parent" (Glean.KeyType Glean.Schema.Thrift.Types.ServiceName) ('Angle.TField "child" (Glean.KeyType Glean.Schema.Thrift.Types.ServiceName) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.ServiceChild where
  type KeyType Glean.Schema.Thrift.Types.ServiceChild =
    Glean.Schema.Thrift.Types.ServiceChild_key
  getName _proxy  = Glean.PredicateRef "thrift.ServiceChild"2
  getIndex _proxy  = 41
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.serviceChild_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.ServiceChild x k
  getFactKey = Glean.Schema.Thrift.Types.serviceChild_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.ServiceChild where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.EnumValueDef_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.EnumValueDef_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.EnumValueDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.EnumValueDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.EnumValue) ('Angle.TField "value" (Glean.Schema.Thrift.Types.IntegerLiteral) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.EnumValueDef where
  type KeyType Glean.Schema.Thrift.Types.EnumValueDef =
    Glean.Schema.Thrift.Types.EnumValueDef_key
  getName _proxy  = Glean.PredicateRef "thrift.EnumValueDef"3
  getIndex _proxy  = 36
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.enumValueDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.EnumValueDef x k
  getFactKey = Glean.Schema.Thrift.Types.enumValueDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.EnumValueDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.HackRecordContains_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.HackRecordContains_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.HackRecordContains_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.HackRecordContains_key = 'Angle.TField "record" (Glean.KeyType Glean.Schema.Thrift.Types.HackRecord) ('Angle.TField "methods" ([Glean.KeyType Glean.Schema.Thrift.Types.HackMethod]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.HackRecordContains where
  type KeyType Glean.Schema.Thrift.Types.HackRecordContains =
    Glean.Schema.Thrift.Types.HackRecordContains_key
  getName _proxy  = Glean.PredicateRef "thrift.HackRecordContains"1
  getIndex _proxy  = 35
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.hackRecordContains_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.HackRecordContains x k
  getFactKey = Glean.Schema.Thrift.Types.hackRecordContains_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.HackRecordContains where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.PythonValue_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonValue_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.PythonValue_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.PythonValue_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.PythonName) ('Angle.TField "module" (Glean.KeyType Glean.Schema.Thrift.Types.PythonModule) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.PythonValue where
  type KeyType Glean.Schema.Thrift.Types.PythonValue =
    Glean.Schema.Thrift.Types.PythonValue_key
  getName _proxy  = Glean.PredicateRef "thrift.PythonValue"1
  getIndex _proxy  = 30
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.pythonValue_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.PythonValue x k
  getFactKey = Glean.Schema.Thrift.Types.pythonValue_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.PythonValue where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.StructType_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.StructType_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.StructType_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.StructType_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "fields" ([Glean.Schema.Thrift.Types.FieldSpecification]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.StructType where
  type KeyType Glean.Schema.Thrift.Types.StructType =
    Glean.Schema.Thrift.Types.StructType_key
  getName _proxy  = Glean.PredicateRef "thrift.StructType"3
  getIndex _proxy  = 28
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.structType_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.StructType x k
  getFactKey = Glean.Schema.Thrift.Types.structType_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.StructType where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.Includes_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.Includes_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.Includes_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.Includes_key = 'Angle.TField "source" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "target" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.Includes where
  type KeyType Glean.Schema.Thrift.Types.Includes =
    Glean.Schema.Thrift.Types.Includes_key
  getName _proxy  = Glean.PredicateRef "thrift.Includes"2
  getIndex _proxy  = 17
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.includes_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.Includes x k
  getFactKey = Glean.Schema.Thrift.Types.includes_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.Includes where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.FileTarget_key where
  buildRtsValue b (Glean.Schema.Thrift.Types.FileTarget_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.FileTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FileTarget_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Thrift.Types.FileTarget where
  type KeyType Glean.Schema.Thrift.Types.FileTarget =
    Glean.Schema.Thrift.Types.FileTarget_key
  getName _proxy  = Glean.PredicateRef "thrift.FileTarget"3
  getIndex _proxy  = 12
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Thrift.Types.fileTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Thrift.Types.FileTarget x k
  getFactKey = Glean.Schema.Thrift.Types.fileTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Thrift.Types.FileTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Thrift.Types.ExceptionSpecification where
  buildRtsValue b (Glean.Schema.Thrift.Types.ExceptionSpecification x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.ExceptionSpecification
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ExceptionSpecification = 'Angle.TField "id" (Glean.Schema.Thrift.Types.FieldId) ('Angle.TField "type_" (Glean.KeyType Glean.Schema.Thrift.Types.ExceptionName) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.Identifier) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Thrift.Types.Item where
  buildRtsValue b (Glean.Schema.Thrift.Types.Item_file x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Item_namespace_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Item_service_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Item_function_ x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Item_decl x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Item_exception_ x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Item_constant x) = do
    Glean.buildRtsSelector b 6
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Item_enumValue x) = do
    Glean.buildRtsSelector b 7
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Thrift.Types.Item_file
    , Glean.mapD Glean.Schema.Thrift.Types.Item_namespace_
    , Glean.mapD Glean.Schema.Thrift.Types.Item_service_
    , Glean.mapD Glean.Schema.Thrift.Types.Item_function_
    , Glean.mapD Glean.Schema.Thrift.Types.Item_decl
    , Glean.mapD Glean.Schema.Thrift.Types.Item_exception_
    , Glean.mapD Glean.Schema.Thrift.Types.Item_constant
    , Glean.mapD Glean.Schema.Thrift.Types.Item_enumValue
    ]

type instance Angle.SumFields Glean.Schema.Thrift.Types.Item = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "namespace_" (Glean.KeyType Glean.Schema.Thrift.Types.Namespace) ('Angle.TField "service_" (Glean.KeyType Glean.Schema.Thrift.Types.ServiceName) ('Angle.TField "function_" (Glean.KeyType Glean.Schema.Thrift.Types.FunctionName) ('Angle.TField "decl" (Glean.KeyType Glean.Schema.Thrift.Types.NamedDecl) ('Angle.TField "exception_" (Glean.KeyType Glean.Schema.Thrift.Types.ExceptionName) ('Angle.TField "constant" (Glean.KeyType Glean.Schema.Thrift.Types.Constant) ('Angle.TField "enumValue" (Glean.KeyType Glean.Schema.Thrift.Types.EnumValue) ('Angle.TNoFields))))))))

instance Glean.SumBranches Glean.Schema.Thrift.Types.File Glean.Schema.Thrift.Types.Item where
  injectBranch = Glean.Schema.Thrift.Types.Item_file
  projectBranch (Glean.Schema.Thrift.Types.Item_file x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.Namespace Glean.Schema.Thrift.Types.Item where
  injectBranch = Glean.Schema.Thrift.Types.Item_namespace_
  projectBranch (Glean.Schema.Thrift.Types.Item_namespace_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.ServiceName Glean.Schema.Thrift.Types.Item where
  injectBranch = Glean.Schema.Thrift.Types.Item_service_
  projectBranch (Glean.Schema.Thrift.Types.Item_service_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.FunctionName Glean.Schema.Thrift.Types.Item where
  injectBranch = Glean.Schema.Thrift.Types.Item_function_
  projectBranch (Glean.Schema.Thrift.Types.Item_function_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.NamedDecl Glean.Schema.Thrift.Types.Item where
  injectBranch = Glean.Schema.Thrift.Types.Item_decl
  projectBranch (Glean.Schema.Thrift.Types.Item_decl x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.ExceptionName Glean.Schema.Thrift.Types.Item where
  injectBranch = Glean.Schema.Thrift.Types.Item_exception_
  projectBranch (Glean.Schema.Thrift.Types.Item_exception_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.Constant Glean.Schema.Thrift.Types.Item where
  injectBranch = Glean.Schema.Thrift.Types.Item_constant
  projectBranch (Glean.Schema.Thrift.Types.Item_constant x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.EnumValue Glean.Schema.Thrift.Types.Item where
  injectBranch = Glean.Schema.Thrift.Types.Item_enumValue
  projectBranch (Glean.Schema.Thrift.Types.Item_enumValue x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Thrift.Types.IntegerLiteral where
  buildRtsValue b (Glean.Schema.Thrift.Types.IntegerLiteral x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.IntegerLiteral
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.IntegerLiteral = 'Angle.TField "isNonNegative" (Prelude.Bool) ('Angle.TField "absValue" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Thrift.Types.Loc where
  buildRtsValue b (Glean.Schema.Thrift.Types.Loc x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Thrift.Types.Loc
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.Loc = 'Angle.TField "startLine" (Glean.Nat) ('Angle.TField "startCol" (Glean.Nat) ('Angle.TField "endLine" (Glean.Nat) ('Angle.TField "endCol" (Glean.Nat) ('Angle.TNoFields))))

instance Glean.Type Glean.Schema.Thrift.Types.Qualifier where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Thrift.Types.Qualifier = 'Angle.TField "default_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "optional_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "required_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Thrift.Types.NamedKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Thrift.Types.NamedKind = 'Angle.TField "typedef_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "enum_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "struct_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "union_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields))))

instance Glean.Type Glean.Schema.Thrift.Types.XRef where
  buildRtsValue b (Glean.Schema.Thrift.Types.XRef x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.XRef
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.XRef = 'Angle.TField "locRef" (Glean.Schema.Thrift.Types.Loc) ('Angle.TField "target" (Glean.Schema.Thrift.Types.XRefTarget) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Thrift.Types.HackRecordKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Thrift.Types.HackRecordKind = 'Angle.TField "class_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "abstract_class" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "interface_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "trait_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "shape_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))))

instance Glean.Type Glean.Schema.Thrift.Types.Cpp2Item where
  buildRtsValue b (Glean.Schema.Thrift.Types.Cpp2Item_file x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Cpp2Item_decl x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.Cpp2Item_named x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Thrift.Types.Cpp2Item_file
    , Glean.mapD Glean.Schema.Thrift.Types.Cpp2Item_decl
    , Glean.mapD Glean.Schema.Thrift.Types.Cpp2Item_named
    ]

type instance Angle.SumFields Glean.Schema.Thrift.Types.Cpp2Item = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "named" (Glean.Schema.Thrift.Types.Cpp2ItemNamed) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Src.Types.File Glean.Schema.Thrift.Types.Cpp2Item where
  injectBranch = Glean.Schema.Thrift.Types.Cpp2Item_file
  projectBranch (Glean.Schema.Thrift.Types.Cpp2Item_file x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.Declaration Glean.Schema.Thrift.Types.Cpp2Item where
  injectBranch = Glean.Schema.Thrift.Types.Cpp2Item_decl
  projectBranch (Glean.Schema.Thrift.Types.Cpp2Item_decl x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.Cpp2ItemNamed Glean.Schema.Thrift.Types.Cpp2Item where
  injectBranch = Glean.Schema.Thrift.Types.Cpp2Item_named
  projectBranch (Glean.Schema.Thrift.Types.Cpp2Item_named x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Thrift.Types.Cpp2ItemNamed where
  buildRtsValue b (Glean.Schema.Thrift.Types.Cpp2ItemNamed x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.Cpp2ItemNamed
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.Cpp2ItemNamed = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "kind" (Data.Text.Text) ('Angle.TField "name" (Data.Text.Text) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Thrift.Types.ResultStream where
  buildRtsValue b (Glean.Schema.Thrift.Types.ResultStream x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.ResultStream
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.ResultStream = 'Angle.TField "response" (Prelude.Maybe (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification)) ('Angle.TField "stream_" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TField "throws_" ([Glean.Schema.Thrift.Types.ExceptionSpecification]) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Thrift.Types.HackMapKind where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Thrift.Types.HackMapKind = 'Angle.TField "core" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "intern" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Thrift.Types.UnqualField where
  buildRtsValue b (Glean.Schema.Thrift.Types.UnqualField x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Thrift.Types.UnqualField
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.UnqualField = 'Angle.TField "id" (Glean.Schema.Thrift.Types.FieldId) ('Angle.TField "type_" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.Identifier) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Thrift.Types.XRefTarget where
  buildRtsValue b (Glean.Schema.Thrift.Types.XRefTarget_include_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.XRefTarget_named x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.XRefTarget_exception_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.XRefTarget_service_ x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.XRefTarget_constant x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.XRefTarget_enumValue x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Thrift.Types.XRefTarget_include_
    , Glean.mapD Glean.Schema.Thrift.Types.XRefTarget_named
    , Glean.mapD Glean.Schema.Thrift.Types.XRefTarget_exception_
    , Glean.mapD Glean.Schema.Thrift.Types.XRefTarget_service_
    , Glean.mapD Glean.Schema.Thrift.Types.XRefTarget_constant
    , Glean.mapD Glean.Schema.Thrift.Types.XRefTarget_enumValue
    ]

type instance Angle.SumFields Glean.Schema.Thrift.Types.XRefTarget = 'Angle.TField "include_" (Glean.KeyType Glean.Schema.Thrift.Types.File) ('Angle.TField "named" (Glean.KeyType Glean.Schema.Thrift.Types.NamedDecl) ('Angle.TField "exception_" (Glean.KeyType Glean.Schema.Thrift.Types.ExceptionName) ('Angle.TField "service_" (Glean.KeyType Glean.Schema.Thrift.Types.ServiceName) ('Angle.TField "constant" (Glean.KeyType Glean.Schema.Thrift.Types.Constant) ('Angle.TField "enumValue" (Glean.KeyType Glean.Schema.Thrift.Types.EnumValue) ('Angle.TNoFields))))))

instance Glean.SumBranches Glean.Schema.Thrift.Types.File Glean.Schema.Thrift.Types.XRefTarget where
  injectBranch = Glean.Schema.Thrift.Types.XRefTarget_include_
  projectBranch (Glean.Schema.Thrift.Types.XRefTarget_include_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.NamedDecl Glean.Schema.Thrift.Types.XRefTarget where
  injectBranch = Glean.Schema.Thrift.Types.XRefTarget_named
  projectBranch (Glean.Schema.Thrift.Types.XRefTarget_named x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.ExceptionName Glean.Schema.Thrift.Types.XRefTarget where
  injectBranch = Glean.Schema.Thrift.Types.XRefTarget_exception_
  projectBranch (Glean.Schema.Thrift.Types.XRefTarget_exception_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.ServiceName Glean.Schema.Thrift.Types.XRefTarget where
  injectBranch = Glean.Schema.Thrift.Types.XRefTarget_service_
  projectBranch (Glean.Schema.Thrift.Types.XRefTarget_service_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.Constant Glean.Schema.Thrift.Types.XRefTarget where
  injectBranch = Glean.Schema.Thrift.Types.XRefTarget_constant
  projectBranch (Glean.Schema.Thrift.Types.XRefTarget_constant x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.EnumValue Glean.Schema.Thrift.Types.XRefTarget where
  injectBranch = Glean.Schema.Thrift.Types.XRefTarget_enumValue
  projectBranch (Glean.Schema.Thrift.Types.XRefTarget_enumValue x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Thrift.Types.GenRole where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Thrift.Types.GenRole = 'Angle.TField "helper" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "server" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "client" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "type" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "constant" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))))

instance Glean.Type Glean.Schema.Thrift.Types.NamedType where
  buildRtsValue b (Glean.Schema.Thrift.Types.NamedType x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.NamedType
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.NamedType = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.QualName) ('Angle.TField "kind" (Glean.Schema.Thrift.Types.NamedKind) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Thrift.Types.PrimitiveType where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Thrift.Types.PrimitiveType = 'Angle.TField "bool_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "byte_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "i16_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "i32_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "i64_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "float_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "double_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "binary_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "string_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))))))))

instance Glean.Type Glean.Schema.Thrift.Types.ResultType where
  buildRtsValue b (Glean.Schema.Thrift.Types.ResultType_oneway_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.ResultType_void_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.ResultType_result x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.ResultType_stream_ x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Thrift.Types.ResultType_oneway_
    , Glean.mapD Glean.Schema.Thrift.Types.ResultType_void_
    , Glean.mapD Glean.Schema.Thrift.Types.ResultType_result
    , Glean.mapD Glean.Schema.Thrift.Types.ResultType_stream_
    ]

type instance Angle.SumFields Glean.Schema.Thrift.Types.ResultType = 'Angle.TField "oneway_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "void_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "result" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TField "stream_" (Glean.Schema.Thrift.Types.ResultStream) ('Angle.TNoFields))))

instance Glean.Type Glean.Schema.Thrift.Types.PythonItem where
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonItem_file x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonItem_module x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonItem_class_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonItem_method x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonItem_field x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonItem_function_ x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.PythonItem_value x) = do
    Glean.buildRtsSelector b 6
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Thrift.Types.PythonItem_file
    , Glean.mapD Glean.Schema.Thrift.Types.PythonItem_module
    , Glean.mapD Glean.Schema.Thrift.Types.PythonItem_class_
    , Glean.mapD Glean.Schema.Thrift.Types.PythonItem_method
    , Glean.mapD Glean.Schema.Thrift.Types.PythonItem_field
    , Glean.mapD Glean.Schema.Thrift.Types.PythonItem_function_
    , Glean.mapD Glean.Schema.Thrift.Types.PythonItem_value
    ]

type instance Angle.SumFields Glean.Schema.Thrift.Types.PythonItem = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "module" (Glean.KeyType Glean.Schema.Thrift.Types.PythonModule) ('Angle.TField "class_" (Glean.KeyType Glean.Schema.Thrift.Types.PythonClass) ('Angle.TField "method" (Glean.KeyType Glean.Schema.Thrift.Types.PythonMethod) ('Angle.TField "field" (Glean.KeyType Glean.Schema.Thrift.Types.PythonField) ('Angle.TField "function_" (Glean.KeyType Glean.Schema.Thrift.Types.PythonFunction) ('Angle.TField "value" (Glean.KeyType Glean.Schema.Thrift.Types.PythonValue) ('Angle.TNoFields)))))))

instance Glean.SumBranches Glean.Schema.Src.Types.File Glean.Schema.Thrift.Types.PythonItem where
  injectBranch = Glean.Schema.Thrift.Types.PythonItem_file
  projectBranch (Glean.Schema.Thrift.Types.PythonItem_file x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.PythonModule Glean.Schema.Thrift.Types.PythonItem where
  injectBranch = Glean.Schema.Thrift.Types.PythonItem_module
  projectBranch (Glean.Schema.Thrift.Types.PythonItem_module x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.PythonClass Glean.Schema.Thrift.Types.PythonItem where
  injectBranch = Glean.Schema.Thrift.Types.PythonItem_class_
  projectBranch (Glean.Schema.Thrift.Types.PythonItem_class_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.PythonMethod Glean.Schema.Thrift.Types.PythonItem where
  injectBranch = Glean.Schema.Thrift.Types.PythonItem_method
  projectBranch (Glean.Schema.Thrift.Types.PythonItem_method x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.PythonField Glean.Schema.Thrift.Types.PythonItem where
  injectBranch = Glean.Schema.Thrift.Types.PythonItem_field
  projectBranch (Glean.Schema.Thrift.Types.PythonItem_field x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.PythonFunction Glean.Schema.Thrift.Types.PythonItem where
  injectBranch = Glean.Schema.Thrift.Types.PythonItem_function_
  projectBranch (Glean.Schema.Thrift.Types.PythonItem_function_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.PythonValue Glean.Schema.Thrift.Types.PythonItem where
  injectBranch = Glean.Schema.Thrift.Types.PythonItem_value
  projectBranch (Glean.Schema.Thrift.Types.PythonItem_value x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Thrift.Types.HackKind where
  buildRtsValue b (Glean.Schema.Thrift.Types.HackKind_file x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.HackKind_record x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.HackKind_method x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.HackKind_namespace_ x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Thrift.Types.HackKind_file
    , Glean.mapD Glean.Schema.Thrift.Types.HackKind_record
    , Glean.mapD Glean.Schema.Thrift.Types.HackKind_method
    , Glean.mapD Glean.Schema.Thrift.Types.HackKind_namespace_
    ]

type instance Angle.SumFields Glean.Schema.Thrift.Types.HackKind = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "record" (Glean.KeyType Glean.Schema.Thrift.Types.HackRecord) ('Angle.TField "method" (Glean.KeyType Glean.Schema.Thrift.Types.HackMethod) ('Angle.TField "namespace_" (Glean.KeyType Glean.Schema.Thrift.Types.HackName) ('Angle.TNoFields))))

instance Glean.SumBranches Glean.Schema.Src.Types.File Glean.Schema.Thrift.Types.HackKind where
  injectBranch = Glean.Schema.Thrift.Types.HackKind_file
  projectBranch (Glean.Schema.Thrift.Types.HackKind_file x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.HackRecord Glean.Schema.Thrift.Types.HackKind where
  injectBranch = Glean.Schema.Thrift.Types.HackKind_record
  projectBranch (Glean.Schema.Thrift.Types.HackKind_record x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.HackMethod Glean.Schema.Thrift.Types.HackKind where
  injectBranch = Glean.Schema.Thrift.Types.HackKind_method
  projectBranch (Glean.Schema.Thrift.Types.HackKind_method x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Thrift.Types.HackName Glean.Schema.Thrift.Types.HackKind where
  injectBranch = Glean.Schema.Thrift.Types.HackKind_namespace_
  projectBranch (Glean.Schema.Thrift.Types.HackKind_namespace_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Thrift.Types.Target where
  buildRtsValue b (Glean.Schema.Thrift.Types.Target x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.Target
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.Target = 'Angle.TField "locTarget" (Glean.Schema.Thrift.Types.Loc) ('Angle.TField "target" (Glean.Schema.Thrift.Types.XRefTarget) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Thrift.Types.MapType where
  buildRtsValue b (Glean.Schema.Thrift.Types.MapType x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Thrift.Types.MapType
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.MapType = 'Angle.TField "key_" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TField "value" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Thrift.Types.ContainerType where
  buildRtsValue b (Glean.Schema.Thrift.Types.ContainerType_list_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.ContainerType_set_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Thrift.Types.ContainerType_map_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Thrift.Types.ContainerType_list_
    , Glean.mapD Glean.Schema.Thrift.Types.ContainerType_set_
    , Glean.mapD Glean.Schema.Thrift.Types.ContainerType_map_
    ]

type instance Angle.SumFields Glean.Schema.Thrift.Types.ContainerType = 'Angle.TField "list_" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TField "set_" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TField "map_" (Glean.Schema.Thrift.Types.MapType) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Thrift.Types.FieldSpecification where
  buildRtsValue b (Glean.Schema.Thrift.Types.FieldSpecification x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Thrift.Types.FieldSpecification
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Thrift.Types.FieldSpecification = 'Angle.TField "id" (Glean.Schema.Thrift.Types.FieldId) ('Angle.TField "qualifier" (Glean.Schema.Thrift.Types.Qualifier) ('Angle.TField "type_" (Glean.KeyType Glean.Schema.Thrift.Types.TypeSpecification) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Thrift.Types.Identifier) ('Angle.TNoFields))))
