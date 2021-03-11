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
import qualified Glean.Schema.Query.Buck.Types

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Cxx1.Types
import qualified Glean.Schema.Query.Cxx1.Types

import qualified Glean.Schema.Scm.Types
import qualified Glean.Schema.Query.Scm.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Thrift.Types


type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ServiceName_key = Glean.Schema.Thrift.Types.ServiceName_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ServiceName_key = Glean.Schema.Query.Thrift.Types.ServiceName_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ServiceName_key where
  toQuery (Glean.Schema.Thrift.Types.ServiceName_key x1 x2) = Glean.Schema.Query.Thrift.Types.ServiceName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ServiceName where
  toQueryId = Glean.Schema.Query.Thrift.Types.ServiceName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ServiceName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ServiceName = Glean.Schema.Thrift.Types.ServiceName
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ServiceName = Glean.Schema.Query.Thrift.Types.ServiceName

instance Glean.ToQuery Glean.Schema.Thrift.Types.ServiceName

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FunctionName_key = Glean.Schema.Thrift.Types.FunctionName_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FunctionName_key = Glean.Schema.Query.Thrift.Types.FunctionName_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FunctionName_key where
  toQuery (Glean.Schema.Thrift.Types.FunctionName_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.FunctionName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FunctionName where
  toQueryId = Glean.Schema.Query.Thrift.Types.FunctionName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FunctionName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FunctionName = Glean.Schema.Thrift.Types.FunctionName
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FunctionName = Glean.Schema.Query.Thrift.Types.FunctionName

instance Glean.ToQuery Glean.Schema.Thrift.Types.FunctionName

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.TypeSpecification_key = Glean.Schema.Thrift.Types.TypeSpecification_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.TypeSpecification_key = Glean.Schema.Query.Thrift.Types.TypeSpecification_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.TypeSpecification_key where
  toQuery (Glean.Schema.Thrift.Types.TypeSpecification_key_primitive x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.typeSpecification_key_primitive = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.TypeSpecification_key_container x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.typeSpecification_key_container = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.TypeSpecification_key_named x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.typeSpecification_key_named = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.PrimitiveType Glean.Schema.Query.Thrift.Types.TypeSpecification_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.typeSpecification_key_primitive = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.ContainerType Glean.Schema.Query.Thrift.Types.TypeSpecification_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.typeSpecification_key_container = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.NamedType Glean.Schema.Query.Thrift.Types.TypeSpecification_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.typeSpecification_key_named = Prelude.Just q }

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.TypeSpecification where
  toQueryId = Glean.Schema.Query.Thrift.Types.TypeSpecification_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.TypeSpecification_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.TypeSpecification = Glean.Schema.Thrift.Types.TypeSpecification
type instance Glean.QueryOf Glean.Schema.Thrift.Types.TypeSpecification = Glean.Schema.Query.Thrift.Types.TypeSpecification

instance Glean.ToQuery Glean.Schema.Thrift.Types.TypeSpecification

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.MangleLang where
  toQueryId = Glean.Schema.Query.Thrift.Types.MangleLang_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.MangleLang_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.MangleLang = Glean.Schema.Thrift.Types.MangleLang
type instance Glean.QueryOf Glean.Schema.Thrift.Types.MangleLang = Glean.Schema.Query.Thrift.Types.MangleLang

instance Glean.ToQuery Glean.Schema.Thrift.Types.MangleLang

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ExceptionName_key = Glean.Schema.Thrift.Types.ExceptionName_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ExceptionName_key = Glean.Schema.Query.Thrift.Types.ExceptionName_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ExceptionName_key where
  toQuery (Glean.Schema.Thrift.Types.ExceptionName_key x1 x2) = Glean.Schema.Query.Thrift.Types.ExceptionName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ExceptionName where
  toQueryId = Glean.Schema.Query.Thrift.Types.ExceptionName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ExceptionName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ExceptionName = Glean.Schema.Thrift.Types.ExceptionName
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ExceptionName = Glean.Schema.Query.Thrift.Types.ExceptionName

instance Glean.ToQuery Glean.Schema.Thrift.Types.ExceptionName

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.UnionType_key = Glean.Schema.Thrift.Types.UnionType_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.UnionType_key = Glean.Schema.Query.Thrift.Types.UnionType_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.UnionType_key where
  toQuery (Glean.Schema.Thrift.Types.UnionType_key x1 x2) = Glean.Schema.Query.Thrift.Types.UnionType_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.UnionType_alts_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.UnionType where
  toQueryId = Glean.Schema.Query.Thrift.Types.UnionType_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.UnionType_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.UnionType = Glean.Schema.Thrift.Types.UnionType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.UnionType = Glean.Schema.Query.Thrift.Types.UnionType

instance Glean.ToQuery Glean.Schema.Thrift.Types.UnionType

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.Identifier where
  toQueryId = Glean.Schema.Query.Thrift.Types.Identifier_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.Identifier_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Identifier = Glean.Schema.Thrift.Types.Identifier
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Identifier = Glean.Schema.Query.Thrift.Types.Identifier

instance Glean.ToQuery Glean.Schema.Thrift.Types.Identifier

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackMethod_key = Glean.Schema.Thrift.Types.HackMethod_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackMethod_key = Glean.Schema.Query.Thrift.Types.HackMethod_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackMethod_key where
  toQuery (Glean.Schema.Thrift.Types.HackMethod_key x1 x2) = Glean.Schema.Query.Thrift.Types.HackMethod_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.HackMethod where
  toQueryId = Glean.Schema.Query.Thrift.Types.HackMethod_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.HackMethod_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackMethod = Glean.Schema.Thrift.Types.HackMethod
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackMethod = Glean.Schema.Query.Thrift.Types.HackMethod

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackMethod

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.OutputTarget_key = Glean.Schema.Thrift.Types.OutputTarget_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.OutputTarget_key = Glean.Schema.Query.Thrift.Types.OutputTarget_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.OutputTarget_key where
  toQuery (Glean.Schema.Thrift.Types.OutputTarget_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.OutputTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.OutputTarget where
  toQueryId = Glean.Schema.Query.Thrift.Types.OutputTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.OutputTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.OutputTarget = Glean.Schema.Thrift.Types.OutputTarget
type instance Glean.QueryOf Glean.Schema.Thrift.Types.OutputTarget = Glean.Schema.Query.Thrift.Types.OutputTarget

instance Glean.ToQuery Glean.Schema.Thrift.Types.OutputTarget

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.QualName_key = Glean.Schema.Thrift.Types.QualName_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.QualName_key = Glean.Schema.Query.Thrift.Types.QualName_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.QualName_key where
  toQuery (Glean.Schema.Thrift.Types.QualName_key x1 x2) = Glean.Schema.Query.Thrift.Types.QualName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.QualName where
  toQueryId = Glean.Schema.Query.Thrift.Types.QualName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.QualName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.QualName = Glean.Schema.Thrift.Types.QualName
type instance Glean.QueryOf Glean.Schema.Thrift.Types.QualName = Glean.Schema.Query.Thrift.Types.QualName

instance Glean.ToQuery Glean.Schema.Thrift.Types.QualName

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.File where
  toQueryId = Glean.Schema.Query.Thrift.Types.File_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.File_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.File = Glean.Schema.Thrift.Types.File
type instance Glean.QueryOf Glean.Schema.Thrift.Types.File = Glean.Schema.Query.Thrift.Types.File

instance Glean.ToQuery Glean.Schema.Thrift.Types.File

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.HackName where
  toQueryId = Glean.Schema.Query.Thrift.Types.HackName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.HackName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackName = Glean.Schema.Thrift.Types.HackName
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackName = Glean.Schema.Query.Thrift.Types.HackName

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackName

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Constant_key = Glean.Schema.Thrift.Types.Constant_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Constant_key = Glean.Schema.Query.Thrift.Types.Constant_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.Constant_key where
  toQuery (Glean.Schema.Thrift.Types.Constant_key x1 x2) = Glean.Schema.Query.Thrift.Types.Constant_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.Constant where
  toQueryId = Glean.Schema.Query.Thrift.Types.Constant_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.Constant_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Constant = Glean.Schema.Thrift.Types.Constant
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Constant = Glean.Schema.Query.Thrift.Types.Constant

instance Glean.ToQuery Glean.Schema.Thrift.Types.Constant

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonFunction_key = Glean.Schema.Thrift.Types.PythonFunction_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonFunction_key = Glean.Schema.Query.Thrift.Types.PythonFunction_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonFunction_key where
  toQuery (Glean.Schema.Thrift.Types.PythonFunction_key x1 x2) = Glean.Schema.Query.Thrift.Types.PythonFunction_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonFunction where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonFunction_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonFunction_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonFunction = Glean.Schema.Thrift.Types.PythonFunction
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonFunction = Glean.Schema.Query.Thrift.Types.PythonFunction

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonFunction

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.IncludeStatement_key = Glean.Schema.Thrift.Types.IncludeStatement_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.IncludeStatement_key = Glean.Schema.Query.Thrift.Types.IncludeStatement_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.IncludeStatement_key where
  toQuery (Glean.Schema.Thrift.Types.IncludeStatement_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.IncludeStatement_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.IncludeStatement where
  toQueryId = Glean.Schema.Query.Thrift.Types.IncludeStatement_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.IncludeStatement_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.IncludeStatement = Glean.Schema.Thrift.Types.IncludeStatement
type instance Glean.QueryOf Glean.Schema.Thrift.Types.IncludeStatement = Glean.Schema.Query.Thrift.Types.IncludeStatement

instance Glean.ToQuery Glean.Schema.Thrift.Types.IncludeStatement

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonClassContains_key = Glean.Schema.Thrift.Types.PythonClassContains_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonClassContains_key = Glean.Schema.Query.Thrift.Types.PythonClassContains_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonClassContains_key where
  toQuery (Glean.Schema.Thrift.Types.PythonClassContains_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.PythonClassContains_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.PythonClassContains_methods_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.PythonClassContains_fields_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonClassContains where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonClassContains_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonClassContains_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonClassContains = Glean.Schema.Thrift.Types.PythonClassContains
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonClassContains = Glean.Schema.Query.Thrift.Types.PythonClassContains

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonClassContains

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.TargetX_key = Glean.Schema.Thrift.Types.TargetX_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.TargetX_key = Glean.Schema.Query.Thrift.Types.TargetX_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.TargetX_key where
  toQuery (Glean.Schema.Thrift.Types.TargetX_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.TargetX_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.TargetX where
  toQueryId = Glean.Schema.Query.Thrift.Types.TargetX_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.TargetX_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.TargetX = Glean.Schema.Thrift.Types.TargetX
type instance Glean.QueryOf Glean.Schema.Thrift.Types.TargetX = Glean.Schema.Query.Thrift.Types.TargetX

instance Glean.ToQuery Glean.Schema.Thrift.Types.TargetX

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.NamespaceName where
  toQueryId = Glean.Schema.Query.Thrift.Types.NamespaceName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.NamespaceName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.NamespaceName = Glean.Schema.Thrift.Types.NamespaceName
type instance Glean.QueryOf Glean.Schema.Thrift.Types.NamespaceName = Glean.Schema.Query.Thrift.Types.NamespaceName

instance Glean.ToQuery Glean.Schema.Thrift.Types.NamespaceName

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackRecord_key = Glean.Schema.Thrift.Types.HackRecord_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackRecord_key = Glean.Schema.Query.Thrift.Types.HackRecord_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackRecord_key where
  toQuery (Glean.Schema.Thrift.Types.HackRecord_key x1 x2) = Glean.Schema.Query.Thrift.Types.HackRecord_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.HackRecord where
  toQueryId = Glean.Schema.Query.Thrift.Types.HackRecord_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.HackRecord_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackRecord = Glean.Schema.Thrift.Types.HackRecord
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackRecord = Glean.Schema.Query.Thrift.Types.HackRecord

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackRecord

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FromCpp2_key = Glean.Schema.Thrift.Types.FromCpp2_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FromCpp2_key = Glean.Schema.Query.Thrift.Types.FromCpp2_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FromCpp2_key where
  toQuery (Glean.Schema.Thrift.Types.FromCpp2_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.FromCpp2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FromCpp2 where
  toQueryId = Glean.Schema.Query.Thrift.Types.FromCpp2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FromCpp2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FromCpp2 = Glean.Schema.Thrift.Types.FromCpp2
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FromCpp2 = Glean.Schema.Query.Thrift.Types.FromCpp2

instance Glean.ToQuery Glean.Schema.Thrift.Types.FromCpp2

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FromHack_key = Glean.Schema.Thrift.Types.FromHack_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FromHack_key = Glean.Schema.Query.Thrift.Types.FromHack_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FromHack_key where
  toQuery (Glean.Schema.Thrift.Types.FromHack_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.FromHack_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FromHack where
  toQueryId = Glean.Schema.Query.Thrift.Types.FromHack_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FromHack_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FromHack = Glean.Schema.Thrift.Types.FromHack
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FromHack = Glean.Schema.Query.Thrift.Types.FromHack

instance Glean.ToQuery Glean.Schema.Thrift.Types.FromHack

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.TypeDefType_key = Glean.Schema.Thrift.Types.TypeDefType_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.TypeDefType_key = Glean.Schema.Query.Thrift.Types.TypeDefType_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.TypeDefType_key where
  toQuery (Glean.Schema.Thrift.Types.TypeDefType_key x1 x2) = Glean.Schema.Query.Thrift.Types.TypeDefType_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.TypeDefType where
  toQueryId = Glean.Schema.Query.Thrift.Types.TypeDefType_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.TypeDefType_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.TypeDefType = Glean.Schema.Thrift.Types.TypeDefType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.TypeDefType = Glean.Schema.Query.Thrift.Types.TypeDefType

instance Glean.ToQuery Glean.Schema.Thrift.Types.TypeDefType

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FunctionSpecification_key = Glean.Schema.Thrift.Types.FunctionSpecification_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FunctionSpecification_key = Glean.Schema.Query.Thrift.Types.FunctionSpecification_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FunctionSpecification_key where
  toQuery (Glean.Schema.Thrift.Types.FunctionSpecification_key x1 x2 x3 x4) = Glean.Schema.Query.Thrift.Types.FunctionSpecification_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.FunctionSpecification_arguments_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.FunctionSpecification_throws__array_exact . Prelude.map Glean.toQuery) x4))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FunctionSpecification where
  toQueryId = Glean.Schema.Query.Thrift.Types.FunctionSpecification_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FunctionSpecification_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FunctionSpecification = Glean.Schema.Thrift.Types.FunctionSpecification
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FunctionSpecification = Glean.Schema.Query.Thrift.Types.FunctionSpecification

instance Glean.ToQuery Glean.Schema.Thrift.Types.FunctionSpecification

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ToPython_key = Glean.Schema.Thrift.Types.ToPython_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ToPython_key = Glean.Schema.Query.Thrift.Types.ToPython_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ToPython_key where
  toQuery (Glean.Schema.Thrift.Types.ToPython_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.ToPython_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.ToPython_python_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ToPython where
  toQueryId = Glean.Schema.Query.Thrift.Types.ToPython_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ToPython_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ToPython = Glean.Schema.Thrift.Types.ToPython
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ToPython = Glean.Schema.Query.Thrift.Types.ToPython

instance Glean.ToQuery Glean.Schema.Thrift.Types.ToPython

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FromPython_key = Glean.Schema.Thrift.Types.FromPython_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FromPython_key = Glean.Schema.Query.Thrift.Types.FromPython_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FromPython_key where
  toQuery (Glean.Schema.Thrift.Types.FromPython_key x1 x2 x3 x4) = Glean.Schema.Query.Thrift.Types.FromPython_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FromPython where
  toQueryId = Glean.Schema.Query.Thrift.Types.FromPython_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FromPython_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FromPython = Glean.Schema.Thrift.Types.FromPython
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FromPython = Glean.Schema.Query.Thrift.Types.FromPython

instance Glean.ToQuery Glean.Schema.Thrift.Types.FromPython

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ToCpp2_key = Glean.Schema.Thrift.Types.ToCpp2_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ToCpp2_key = Glean.Schema.Query.Thrift.Types.ToCpp2_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ToCpp2_key where
  toQuery (Glean.Schema.Thrift.Types.ToCpp2_key x1 x2) = Glean.Schema.Query.Thrift.Types.ToCpp2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.ToCpp2_cpp2_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ToCpp2 where
  toQueryId = Glean.Schema.Query.Thrift.Types.ToCpp2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ToCpp2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ToCpp2 = Glean.Schema.Thrift.Types.ToCpp2
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ToCpp2 = Glean.Schema.Query.Thrift.Types.ToCpp2

instance Glean.ToQuery Glean.Schema.Thrift.Types.ToCpp2

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonClass_key = Glean.Schema.Thrift.Types.PythonClass_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonClass_key = Glean.Schema.Query.Thrift.Types.PythonClass_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonClass_key where
  toQuery (Glean.Schema.Thrift.Types.PythonClass_key x1 x2) = Glean.Schema.Query.Thrift.Types.PythonClass_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonClass where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonClass_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonClass_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonClass = Glean.Schema.Thrift.Types.PythonClass
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonClass = Glean.Schema.Query.Thrift.Types.PythonClass

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonClass

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ToHack_key = Glean.Schema.Thrift.Types.ToHack_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ToHack_key = Glean.Schema.Query.Thrift.Types.ToHack_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ToHack_key where
  toQuery (Glean.Schema.Thrift.Types.ToHack_key x1 x2) = Glean.Schema.Query.Thrift.Types.ToHack_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.ToHack_hack_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ToHack where
  toQueryId = Glean.Schema.Query.Thrift.Types.ToHack_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ToHack_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ToHack = Glean.Schema.Thrift.Types.ToHack
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ToHack = Glean.Schema.Query.Thrift.Types.ToHack

instance Glean.ToQuery Glean.Schema.Thrift.Types.ToHack

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.IncludeSpecial_key = Glean.Schema.Thrift.Types.IncludeSpecial_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.IncludeSpecial_key = Glean.Schema.Query.Thrift.Types.IncludeSpecial_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.IncludeSpecial_key where
  toQuery (Glean.Schema.Thrift.Types.IncludeSpecial_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.IncludeSpecial_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.IncludeSpecial where
  toQueryId = Glean.Schema.Query.Thrift.Types.IncludeSpecial_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.IncludeSpecial_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.IncludeSpecial = Glean.Schema.Thrift.Types.IncludeSpecial
type instance Glean.QueryOf Glean.Schema.Thrift.Types.IncludeSpecial = Glean.Schema.Query.Thrift.Types.IncludeSpecial

instance Glean.ToQuery Glean.Schema.Thrift.Types.IncludeSpecial

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonModuleContains_key = Glean.Schema.Thrift.Types.PythonModuleContains_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonModuleContains_key = Glean.Schema.Query.Thrift.Types.PythonModuleContains_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonModuleContains_key where
  toQuery (Glean.Schema.Thrift.Types.PythonModuleContains_key x1 x2 x3 x4) = Glean.Schema.Query.Thrift.Types.PythonModuleContains_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.PythonModuleContains_classes_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.PythonModuleContains_functions_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.PythonModuleContains_values_array_exact . Prelude.map Glean.toQuery) x4))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonModuleContains where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonModuleContains_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonModuleContains_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonModuleContains = Glean.Schema.Thrift.Types.PythonModuleContains
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonModuleContains = Glean.Schema.Query.Thrift.Types.PythonModuleContains

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonModuleContains

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.NamedDecl_key = Glean.Schema.Thrift.Types.NamedDecl_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.NamedDecl_key = Glean.Schema.Query.Thrift.Types.NamedDecl_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.NamedDecl_key where
  toQuery (Glean.Schema.Thrift.Types.NamedDecl_key x1 x2) = Glean.Schema.Query.Thrift.Types.NamedDecl_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.NamedDecl where
  toQueryId = Glean.Schema.Query.Thrift.Types.NamedDecl_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.NamedDecl_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.NamedDecl = Glean.Schema.Thrift.Types.NamedDecl
type instance Glean.QueryOf Glean.Schema.Thrift.Types.NamedDecl = Glean.Schema.Query.Thrift.Types.NamedDecl

instance Glean.ToQuery Glean.Schema.Thrift.Types.NamedDecl

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackMap_key = Glean.Schema.Thrift.Types.HackMap_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackMap_key = Glean.Schema.Query.Thrift.Types.HackMap_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackMap_key where
  toQuery (Glean.Schema.Thrift.Types.HackMap_key x1 x2 x3 x4 x5 x6 x7) = Glean.Schema.Query.Thrift.Types.HackMap_key (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Thrift.Types.hackMap_source_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Thrift.Types.hackMap_source_just = Prelude.Just (Glean.toQuery x)})) x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5)) (Prelude.Just (Glean.toQuery x6)) (Prelude.Just (Glean.toQuery x7))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.HackMap where
  toQueryId = Glean.Schema.Query.Thrift.Types.HackMap_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.HackMap_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackMap = Glean.Schema.Thrift.Types.HackMap
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackMap = Glean.Schema.Query.Thrift.Types.HackMap

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackMap

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ServiceDefinition_key = Glean.Schema.Thrift.Types.ServiceDefinition_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ServiceDefinition_key = Glean.Schema.Query.Thrift.Types.ServiceDefinition_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ServiceDefinition_key where
  toQuery (Glean.Schema.Thrift.Types.ServiceDefinition_key x1 x2) = Glean.Schema.Query.Thrift.Types.ServiceDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.ServiceDefinition_functions_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ServiceDefinition where
  toQueryId = Glean.Schema.Query.Thrift.Types.ServiceDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ServiceDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ServiceDefinition = Glean.Schema.Thrift.Types.ServiceDefinition
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ServiceDefinition = Glean.Schema.Query.Thrift.Types.ServiceDefinition

instance Glean.ToQuery Glean.Schema.Thrift.Types.ServiceDefinition

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.NamespaceValue where
  toQueryId = Glean.Schema.Query.Thrift.Types.NamespaceValue_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.NamespaceValue_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.NamespaceValue = Glean.Schema.Thrift.Types.NamespaceValue
type instance Glean.QueryOf Glean.Schema.Thrift.Types.NamespaceValue = Glean.Schema.Query.Thrift.Types.NamespaceValue

instance Glean.ToQuery Glean.Schema.Thrift.Types.NamespaceValue

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonModuleFile_key = Glean.Schema.Thrift.Types.PythonModuleFile_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonModuleFile_key = Glean.Schema.Query.Thrift.Types.PythonModuleFile_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonModuleFile_key where
  toQuery (Glean.Schema.Thrift.Types.PythonModuleFile_key x1 x2) = Glean.Schema.Query.Thrift.Types.PythonModuleFile_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonModuleFile where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonModuleFile_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonModuleFile_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonModuleFile = Glean.Schema.Thrift.Types.PythonModuleFile
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonModuleFile = Glean.Schema.Query.Thrift.Types.PythonModuleFile

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonModuleFile

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.EnumValue_key = Glean.Schema.Thrift.Types.EnumValue_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.EnumValue_key = Glean.Schema.Query.Thrift.Types.EnumValue_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.EnumValue_key where
  toQuery (Glean.Schema.Thrift.Types.EnumValue_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.EnumValue_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.EnumValue where
  toQueryId = Glean.Schema.Query.Thrift.Types.EnumValue_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.EnumValue_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.EnumValue = Glean.Schema.Thrift.Types.EnumValue
type instance Glean.QueryOf Glean.Schema.Thrift.Types.EnumValue = Glean.Schema.Query.Thrift.Types.EnumValue

instance Glean.ToQuery Glean.Schema.Thrift.Types.EnumValue

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.CompileTarget_key = Glean.Schema.Thrift.Types.CompileTarget_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.CompileTarget_key = Glean.Schema.Query.Thrift.Types.CompileTarget_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.CompileTarget_key where
  toQuery (Glean.Schema.Thrift.Types.CompileTarget_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.CompileTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.CompileTarget where
  toQueryId = Glean.Schema.Query.Thrift.Types.CompileTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.CompileTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.CompileTarget = Glean.Schema.Thrift.Types.CompileTarget
type instance Glean.QueryOf Glean.Schema.Thrift.Types.CompileTarget = Glean.Schema.Query.Thrift.Types.CompileTarget

instance Glean.ToQuery Glean.Schema.Thrift.Types.CompileTarget

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonFileModule_key = Glean.Schema.Thrift.Types.PythonFileModule_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonFileModule_key = Glean.Schema.Query.Thrift.Types.PythonFileModule_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonFileModule_key where
  toQuery (Glean.Schema.Thrift.Types.PythonFileModule_key x1 x2) = Glean.Schema.Query.Thrift.Types.PythonFileModule_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonFileModule where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonFileModule_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonFileModule_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonFileModule = Glean.Schema.Thrift.Types.PythonFileModule
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonFileModule = Glean.Schema.Query.Thrift.Types.PythonFileModule

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonFileModule

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonName where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonName = Glean.Schema.Thrift.Types.PythonName
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonName = Glean.Schema.Query.Thrift.Types.PythonName

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonName

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Namespace_key = Glean.Schema.Thrift.Types.Namespace_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Namespace_key = Glean.Schema.Query.Thrift.Types.Namespace_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.Namespace_key where
  toQuery (Glean.Schema.Thrift.Types.Namespace_key x1 x2 x3 x4) = Glean.Schema.Query.Thrift.Types.Namespace_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.Namespace where
  toQueryId = Glean.Schema.Query.Thrift.Types.Namespace_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.Namespace_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Namespace = Glean.Schema.Thrift.Types.Namespace
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Namespace = Glean.Schema.Query.Thrift.Types.Namespace

instance Glean.ToQuery Glean.Schema.Thrift.Types.Namespace

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonMethod_key = Glean.Schema.Thrift.Types.PythonMethod_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonMethod_key = Glean.Schema.Query.Thrift.Types.PythonMethod_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonMethod_key where
  toQuery (Glean.Schema.Thrift.Types.PythonMethod_key x1 x2) = Glean.Schema.Query.Thrift.Types.PythonMethod_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonMethod where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonMethod_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonMethod_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonMethod = Glean.Schema.Thrift.Types.PythonMethod
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonMethod = Glean.Schema.Query.Thrift.Types.PythonMethod

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonMethod

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonModule where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonModule_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonModule_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonModule = Glean.Schema.Thrift.Types.PythonModule
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonModule = Glean.Schema.Query.Thrift.Types.PythonModule

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonModule

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ConstantType_key = Glean.Schema.Thrift.Types.ConstantType_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ConstantType_key = Glean.Schema.Query.Thrift.Types.ConstantType_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ConstantType_key where
  toQuery (Glean.Schema.Thrift.Types.ConstantType_key x1 x2) = Glean.Schema.Query.Thrift.Types.ConstantType_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ConstantType where
  toQueryId = Glean.Schema.Query.Thrift.Types.ConstantType_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ConstantType_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ConstantType = Glean.Schema.Thrift.Types.ConstantType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ConstantType = Glean.Schema.Query.Thrift.Types.ConstantType

instance Glean.ToQuery Glean.Schema.Thrift.Types.ConstantType

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FileError_key = Glean.Schema.Thrift.Types.FileError_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FileError_key = Glean.Schema.Query.Thrift.Types.FileError_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FileError_key where
  toQuery (Glean.Schema.Thrift.Types.FileError_key x1 x2) = Glean.Schema.Query.Thrift.Types.FileError_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FileError where
  toQueryId = Glean.Schema.Query.Thrift.Types.FileError_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FileError_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FileError = Glean.Schema.Thrift.Types.FileError
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FileError = Glean.Schema.Query.Thrift.Types.FileError

instance Glean.ToQuery Glean.Schema.Thrift.Types.FileError

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonField_key = Glean.Schema.Thrift.Types.PythonField_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonField_key = Glean.Schema.Query.Thrift.Types.PythonField_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonField_key where
  toQuery (Glean.Schema.Thrift.Types.PythonField_key x1 x2) = Glean.Schema.Query.Thrift.Types.PythonField_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonField where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonField_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonField_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonField = Glean.Schema.Thrift.Types.PythonField
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonField = Glean.Schema.Query.Thrift.Types.PythonField

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonField

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.EnumerationType_key = Glean.Schema.Thrift.Types.EnumerationType_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.EnumerationType_key = Glean.Schema.Query.Thrift.Types.EnumerationType_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.EnumerationType_key where
  toQuery (Glean.Schema.Thrift.Types.EnumerationType_key x1 x2) = Glean.Schema.Query.Thrift.Types.EnumerationType_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.EnumerationType_value_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.EnumerationType where
  toQueryId = Glean.Schema.Query.Thrift.Types.EnumerationType_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.EnumerationType_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.EnumerationType = Glean.Schema.Thrift.Types.EnumerationType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.EnumerationType = Glean.Schema.Query.Thrift.Types.EnumerationType

instance Glean.ToQuery Glean.Schema.Thrift.Types.EnumerationType

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FileOutput_key = Glean.Schema.Thrift.Types.FileOutput_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FileOutput_key = Glean.Schema.Query.Thrift.Types.FileOutput_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FileOutput_key where
  toQuery (Glean.Schema.Thrift.Types.FileOutput_key x1 x2) = Glean.Schema.Query.Thrift.Types.FileOutput_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FileOutput where
  toQueryId = Glean.Schema.Query.Thrift.Types.FileOutput_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FileOutput_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FileOutput = Glean.Schema.Thrift.Types.FileOutput
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FileOutput = Glean.Schema.Query.Thrift.Types.FileOutput

instance Glean.ToQuery Glean.Schema.Thrift.Types.FileOutput

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ServiceParent_key = Glean.Schema.Thrift.Types.ServiceParent_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ServiceParent_key = Glean.Schema.Query.Thrift.Types.ServiceParent_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ServiceParent_key where
  toQuery (Glean.Schema.Thrift.Types.ServiceParent_key x1 x2) = Glean.Schema.Query.Thrift.Types.ServiceParent_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ServiceParent where
  toQueryId = Glean.Schema.Query.Thrift.Types.ServiceParent_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ServiceParent_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ServiceParent = Glean.Schema.Thrift.Types.ServiceParent
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ServiceParent = Glean.Schema.Query.Thrift.Types.ServiceParent

instance Glean.ToQuery Glean.Schema.Thrift.Types.ServiceParent

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FileXRefs_key = Glean.Schema.Thrift.Types.FileXRefs_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FileXRefs_key = Glean.Schema.Query.Thrift.Types.FileXRefs_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FileXRefs_key where
  toQuery (Glean.Schema.Thrift.Types.FileXRefs_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.FileXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.FileXRefs_targets_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.FileXRefs_xrefs_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FileXRefs where
  toQueryId = Glean.Schema.Query.Thrift.Types.FileXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FileXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FileXRefs = Glean.Schema.Thrift.Types.FileXRefs
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FileXRefs = Glean.Schema.Query.Thrift.Types.FileXRefs

instance Glean.ToQuery Glean.Schema.Thrift.Types.FileXRefs

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.IncludeSplice where
  toQueryId = Glean.Schema.Query.Thrift.Types.IncludeSplice_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.IncludeSplice_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.IncludeSplice = Glean.Schema.Thrift.Types.IncludeSplice
type instance Glean.QueryOf Glean.Schema.Thrift.Types.IncludeSplice = Glean.Schema.Query.Thrift.Types.IncludeSplice

instance Glean.ToQuery Glean.Schema.Thrift.Types.IncludeSplice

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.Lang where
  toQueryId = Glean.Schema.Query.Thrift.Types.Lang_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.Lang_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Lang = Glean.Schema.Thrift.Types.Lang
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Lang = Glean.Schema.Query.Thrift.Types.Lang

instance Glean.ToQuery Glean.Schema.Thrift.Types.Lang

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Mangle_key = Glean.Schema.Thrift.Types.Mangle_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Mangle_key = Glean.Schema.Query.Thrift.Types.Mangle_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.Mangle_key where
  toQuery (Glean.Schema.Thrift.Types.Mangle_key x1 x2 x3) = Glean.Schema.Query.Thrift.Types.Mangle_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.Mangle where
  toQueryId = Glean.Schema.Query.Thrift.Types.Mangle_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.Mangle_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Mangle = Glean.Schema.Thrift.Types.Mangle
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Mangle = Glean.Schema.Query.Thrift.Types.Mangle

instance Glean.ToQuery Glean.Schema.Thrift.Types.Mangle

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ServiceChild_key = Glean.Schema.Thrift.Types.ServiceChild_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ServiceChild_key = Glean.Schema.Query.Thrift.Types.ServiceChild_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.ServiceChild_key where
  toQuery (Glean.Schema.Thrift.Types.ServiceChild_key x1 x2) = Glean.Schema.Query.Thrift.Types.ServiceChild_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.ServiceChild where
  toQueryId = Glean.Schema.Query.Thrift.Types.ServiceChild_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.ServiceChild_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ServiceChild = Glean.Schema.Thrift.Types.ServiceChild
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ServiceChild = Glean.Schema.Query.Thrift.Types.ServiceChild

instance Glean.ToQuery Glean.Schema.Thrift.Types.ServiceChild

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.EnumValueDef_key = Glean.Schema.Thrift.Types.EnumValueDef_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.EnumValueDef_key = Glean.Schema.Query.Thrift.Types.EnumValueDef_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.EnumValueDef_key where
  toQuery (Glean.Schema.Thrift.Types.EnumValueDef_key x1 x2) = Glean.Schema.Query.Thrift.Types.EnumValueDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.EnumValueDef where
  toQueryId = Glean.Schema.Query.Thrift.Types.EnumValueDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.EnumValueDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.EnumValueDef = Glean.Schema.Thrift.Types.EnumValueDef
type instance Glean.QueryOf Glean.Schema.Thrift.Types.EnumValueDef = Glean.Schema.Query.Thrift.Types.EnumValueDef

instance Glean.ToQuery Glean.Schema.Thrift.Types.EnumValueDef

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackRecordContains_key = Glean.Schema.Thrift.Types.HackRecordContains_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackRecordContains_key = Glean.Schema.Query.Thrift.Types.HackRecordContains_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackRecordContains_key where
  toQuery (Glean.Schema.Thrift.Types.HackRecordContains_key x1 x2) = Glean.Schema.Query.Thrift.Types.HackRecordContains_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.HackRecordContains_methods_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.HackRecordContains where
  toQueryId = Glean.Schema.Query.Thrift.Types.HackRecordContains_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.HackRecordContains_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackRecordContains = Glean.Schema.Thrift.Types.HackRecordContains
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackRecordContains = Glean.Schema.Query.Thrift.Types.HackRecordContains

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackRecordContains

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonValue_key = Glean.Schema.Thrift.Types.PythonValue_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonValue_key = Glean.Schema.Query.Thrift.Types.PythonValue_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonValue_key where
  toQuery (Glean.Schema.Thrift.Types.PythonValue_key x1 x2) = Glean.Schema.Query.Thrift.Types.PythonValue_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.PythonValue where
  toQueryId = Glean.Schema.Query.Thrift.Types.PythonValue_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.PythonValue_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonValue = Glean.Schema.Thrift.Types.PythonValue
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonValue = Glean.Schema.Query.Thrift.Types.PythonValue

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonValue

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.StructType_key = Glean.Schema.Thrift.Types.StructType_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.StructType_key = Glean.Schema.Query.Thrift.Types.StructType_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.StructType_key where
  toQuery (Glean.Schema.Thrift.Types.StructType_key x1 x2) = Glean.Schema.Query.Thrift.Types.StructType_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.StructType_fields_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.StructType where
  toQueryId = Glean.Schema.Query.Thrift.Types.StructType_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.StructType_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.StructType = Glean.Schema.Thrift.Types.StructType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.StructType = Glean.Schema.Query.Thrift.Types.StructType

instance Glean.ToQuery Glean.Schema.Thrift.Types.StructType

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Includes_key = Glean.Schema.Thrift.Types.Includes_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Includes_key = Glean.Schema.Query.Thrift.Types.Includes_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.Includes_key where
  toQuery (Glean.Schema.Thrift.Types.Includes_key x1 x2) = Glean.Schema.Query.Thrift.Types.Includes_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.Includes where
  toQueryId = Glean.Schema.Query.Thrift.Types.Includes_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.Includes_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Includes = Glean.Schema.Thrift.Types.Includes
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Includes = Glean.Schema.Query.Thrift.Types.Includes

instance Glean.ToQuery Glean.Schema.Thrift.Types.Includes

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FileTarget_key = Glean.Schema.Thrift.Types.FileTarget_key
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FileTarget_key = Glean.Schema.Query.Thrift.Types.FileTarget_key

instance Glean.ToQuery Glean.Schema.Thrift.Types.FileTarget_key where
  toQuery (Glean.Schema.Thrift.Types.FileTarget_key x1 x2) = Glean.Schema.Query.Thrift.Types.FileTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Thrift.Types.FileTarget where
  toQueryId = Glean.Schema.Query.Thrift.Types.FileTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Thrift.Types.FileTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FileTarget = Glean.Schema.Thrift.Types.FileTarget
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FileTarget = Glean.Schema.Query.Thrift.Types.FileTarget

instance Glean.ToQuery Glean.Schema.Thrift.Types.FileTarget

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ExceptionSpecification = Glean.Schema.Thrift.Types.ExceptionSpecification
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ExceptionSpecification = Glean.Schema.Query.Thrift.Types.ExceptionSpecification

instance Glean.ToQuery Glean.Schema.Thrift.Types.ExceptionSpecification where
  toQuery (Glean.Schema.Thrift.Types.ExceptionSpecification x1 x2 x3) = Glean.Schema.Query.Thrift.Types.ExceptionSpecification (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Item = Glean.Schema.Thrift.Types.Item
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Item = Glean.Schema.Query.Thrift.Types.Item

instance Glean.ToQuery Glean.Schema.Thrift.Types.Item where
  toQuery (Glean.Schema.Thrift.Types.Item_file x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.item_file = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Item_namespace_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.item_namespace_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Item_service_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.item_service_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Item_function_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.item_function_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Item_decl x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.item_decl = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Item_exception_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.item_exception_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Item_constant x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.item_constant = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Item_enumValue x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.item_enumValue = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.File Glean.Schema.Query.Thrift.Types.Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.item_file = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.Namespace Glean.Schema.Query.Thrift.Types.Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.item_namespace_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.ServiceName Glean.Schema.Query.Thrift.Types.Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.item_service_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.FunctionName Glean.Schema.Query.Thrift.Types.Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.item_function_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.NamedDecl Glean.Schema.Query.Thrift.Types.Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.item_decl = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.ExceptionName Glean.Schema.Query.Thrift.Types.Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.item_exception_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.Constant Glean.Schema.Query.Thrift.Types.Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.item_constant = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.EnumValue Glean.Schema.Query.Thrift.Types.Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.item_enumValue = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.IntegerLiteral = Glean.Schema.Thrift.Types.IntegerLiteral
type instance Glean.QueryOf Glean.Schema.Thrift.Types.IntegerLiteral = Glean.Schema.Query.Thrift.Types.IntegerLiteral

instance Glean.ToQuery Glean.Schema.Thrift.Types.IntegerLiteral where
  toQuery (Glean.Schema.Thrift.Types.IntegerLiteral x1 x2) = Glean.Schema.Query.Thrift.Types.IntegerLiteral (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Loc = Glean.Schema.Thrift.Types.Loc
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Loc = Glean.Schema.Query.Thrift.Types.Loc

instance Glean.ToQuery Glean.Schema.Thrift.Types.Loc where
  toQuery (Glean.Schema.Thrift.Types.Loc x1 x2 x3 x4) = Glean.Schema.Query.Thrift.Types.Loc (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Qualifier = Glean.Schema.Thrift.Types.Qualifier
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Qualifier = Glean.Schema.Query.Thrift.Types.Qualifier

instance Glean.ToQuery Glean.Schema.Thrift.Types.Qualifier where
  toQuery Glean.Schema.Thrift.Types.Qualifier_default_ = Glean.Schema.Query.Thrift.Types.Qualifier_default_
  toQuery Glean.Schema.Thrift.Types.Qualifier_optional_ = Glean.Schema.Query.Thrift.Types.Qualifier_optional_
  toQuery Glean.Schema.Thrift.Types.Qualifier_required_ = Glean.Schema.Query.Thrift.Types.Qualifier_required_

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.NamedKind = Glean.Schema.Thrift.Types.NamedKind
type instance Glean.QueryOf Glean.Schema.Thrift.Types.NamedKind = Glean.Schema.Query.Thrift.Types.NamedKind

instance Glean.ToQuery Glean.Schema.Thrift.Types.NamedKind where
  toQuery Glean.Schema.Thrift.Types.NamedKind_typedef_ = Glean.Schema.Query.Thrift.Types.NamedKind_typedef_
  toQuery Glean.Schema.Thrift.Types.NamedKind_enum_ = Glean.Schema.Query.Thrift.Types.NamedKind_enum_
  toQuery Glean.Schema.Thrift.Types.NamedKind_struct_ = Glean.Schema.Query.Thrift.Types.NamedKind_struct_
  toQuery Glean.Schema.Thrift.Types.NamedKind_union_ = Glean.Schema.Query.Thrift.Types.NamedKind_union_

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.XRef = Glean.Schema.Thrift.Types.XRef
type instance Glean.QueryOf Glean.Schema.Thrift.Types.XRef = Glean.Schema.Query.Thrift.Types.XRef

instance Glean.ToQuery Glean.Schema.Thrift.Types.XRef where
  toQuery (Glean.Schema.Thrift.Types.XRef x1 x2) = Glean.Schema.Query.Thrift.Types.XRef (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackRecordKind = Glean.Schema.Thrift.Types.HackRecordKind
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackRecordKind = Glean.Schema.Query.Thrift.Types.HackRecordKind

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackRecordKind where
  toQuery Glean.Schema.Thrift.Types.HackRecordKind_class_ = Glean.Schema.Query.Thrift.Types.HackRecordKind_class_
  toQuery Glean.Schema.Thrift.Types.HackRecordKind_abstract_class = Glean.Schema.Query.Thrift.Types.HackRecordKind_abstract_class
  toQuery Glean.Schema.Thrift.Types.HackRecordKind_interface_ = Glean.Schema.Query.Thrift.Types.HackRecordKind_interface_
  toQuery Glean.Schema.Thrift.Types.HackRecordKind_trait_ = Glean.Schema.Query.Thrift.Types.HackRecordKind_trait_
  toQuery Glean.Schema.Thrift.Types.HackRecordKind_shape_ = Glean.Schema.Query.Thrift.Types.HackRecordKind_shape_

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Cpp2Item = Glean.Schema.Thrift.Types.Cpp2Item
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Cpp2Item = Glean.Schema.Query.Thrift.Types.Cpp2Item

instance Glean.ToQuery Glean.Schema.Thrift.Types.Cpp2Item where
  toQuery (Glean.Schema.Thrift.Types.Cpp2Item_file x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.cpp2Item_file = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Cpp2Item_decl x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.cpp2Item_decl = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.Cpp2Item_named x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.cpp2Item_named = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Src.Types.File Glean.Schema.Query.Thrift.Types.Cpp2Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.cpp2Item_file = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.Declaration Glean.Schema.Query.Thrift.Types.Cpp2Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.cpp2Item_decl = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.Cpp2ItemNamed Glean.Schema.Query.Thrift.Types.Cpp2Item where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.cpp2Item_named = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Cpp2ItemNamed = Glean.Schema.Thrift.Types.Cpp2ItemNamed
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Cpp2ItemNamed = Glean.Schema.Query.Thrift.Types.Cpp2ItemNamed

instance Glean.ToQuery Glean.Schema.Thrift.Types.Cpp2ItemNamed where
  toQuery (Glean.Schema.Thrift.Types.Cpp2ItemNamed x1 x2 x3) = Glean.Schema.Query.Thrift.Types.Cpp2ItemNamed (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ResultStream = Glean.Schema.Thrift.Types.ResultStream
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ResultStream = Glean.Schema.Query.Thrift.Types.ResultStream

instance Glean.ToQuery Glean.Schema.Thrift.Types.ResultStream where
  toQuery (Glean.Schema.Thrift.Types.ResultStream x1 x2 x3) = Glean.Schema.Query.Thrift.Types.ResultStream (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Thrift.Types.resultStream_response_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Thrift.Types.resultStream_response_just = Prelude.Just (Glean.toQuery x)})) x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Thrift.Types.ResultStream_throws__array_exact . Prelude.map Glean.toQuery) x3))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackMapKind = Glean.Schema.Thrift.Types.HackMapKind
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackMapKind = Glean.Schema.Query.Thrift.Types.HackMapKind

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackMapKind where
  toQuery Glean.Schema.Thrift.Types.HackMapKind_core = Glean.Schema.Query.Thrift.Types.HackMapKind_core
  toQuery Glean.Schema.Thrift.Types.HackMapKind_intern = Glean.Schema.Query.Thrift.Types.HackMapKind_intern

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.UnqualField = Glean.Schema.Thrift.Types.UnqualField
type instance Glean.QueryOf Glean.Schema.Thrift.Types.UnqualField = Glean.Schema.Query.Thrift.Types.UnqualField

instance Glean.ToQuery Glean.Schema.Thrift.Types.UnqualField where
  toQuery (Glean.Schema.Thrift.Types.UnqualField x1 x2 x3) = Glean.Schema.Query.Thrift.Types.UnqualField (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.XRefTarget = Glean.Schema.Thrift.Types.XRefTarget
type instance Glean.QueryOf Glean.Schema.Thrift.Types.XRefTarget = Glean.Schema.Query.Thrift.Types.XRefTarget

instance Glean.ToQuery Glean.Schema.Thrift.Types.XRefTarget where
  toQuery (Glean.Schema.Thrift.Types.XRefTarget_include_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.xRefTarget_include_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.XRefTarget_named x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.xRefTarget_named = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.XRefTarget_exception_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.xRefTarget_exception_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.XRefTarget_service_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.xRefTarget_service_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.XRefTarget_constant x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.xRefTarget_constant = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.XRefTarget_enumValue x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.xRefTarget_enumValue = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.File Glean.Schema.Query.Thrift.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.xRefTarget_include_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.NamedDecl Glean.Schema.Query.Thrift.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.xRefTarget_named = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.ExceptionName Glean.Schema.Query.Thrift.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.xRefTarget_exception_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.ServiceName Glean.Schema.Query.Thrift.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.xRefTarget_service_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.Constant Glean.Schema.Query.Thrift.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.xRefTarget_constant = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.EnumValue Glean.Schema.Query.Thrift.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.xRefTarget_enumValue = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.GenRole = Glean.Schema.Thrift.Types.GenRole
type instance Glean.QueryOf Glean.Schema.Thrift.Types.GenRole = Glean.Schema.Query.Thrift.Types.GenRole

instance Glean.ToQuery Glean.Schema.Thrift.Types.GenRole where
  toQuery Glean.Schema.Thrift.Types.GenRole_helper = Glean.Schema.Query.Thrift.Types.GenRole_helper
  toQuery Glean.Schema.Thrift.Types.GenRole_server = Glean.Schema.Query.Thrift.Types.GenRole_server
  toQuery Glean.Schema.Thrift.Types.GenRole_client = Glean.Schema.Query.Thrift.Types.GenRole_client
  toQuery Glean.Schema.Thrift.Types.GenRole_type = Glean.Schema.Query.Thrift.Types.GenRole_type
  toQuery Glean.Schema.Thrift.Types.GenRole_constant = Glean.Schema.Query.Thrift.Types.GenRole_constant

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.NamedType = Glean.Schema.Thrift.Types.NamedType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.NamedType = Glean.Schema.Query.Thrift.Types.NamedType

instance Glean.ToQuery Glean.Schema.Thrift.Types.NamedType where
  toQuery (Glean.Schema.Thrift.Types.NamedType x1 x2) = Glean.Schema.Query.Thrift.Types.NamedType (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PrimitiveType = Glean.Schema.Thrift.Types.PrimitiveType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PrimitiveType = Glean.Schema.Query.Thrift.Types.PrimitiveType

instance Glean.ToQuery Glean.Schema.Thrift.Types.PrimitiveType where
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_bool_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_bool_
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_byte_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_byte_
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_i16_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_i16_
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_i32_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_i32_
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_i64_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_i64_
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_float_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_float_
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_double_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_double_
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_binary_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_binary_
  toQuery Glean.Schema.Thrift.Types.PrimitiveType_string_ = Glean.Schema.Query.Thrift.Types.PrimitiveType_string_

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ResultType = Glean.Schema.Thrift.Types.ResultType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ResultType = Glean.Schema.Query.Thrift.Types.ResultType

instance Glean.ToQuery Glean.Schema.Thrift.Types.ResultType where
  toQuery (Glean.Schema.Thrift.Types.ResultType_oneway_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.resultType_oneway_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.ResultType_void_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.resultType_void_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.ResultType_result x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.resultType_result = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.ResultType_stream_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.resultType_stream_ = Prelude.Just (Glean.toQuery x) }

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.PythonItem = Glean.Schema.Thrift.Types.PythonItem
type instance Glean.QueryOf Glean.Schema.Thrift.Types.PythonItem = Glean.Schema.Query.Thrift.Types.PythonItem

instance Glean.ToQuery Glean.Schema.Thrift.Types.PythonItem where
  toQuery (Glean.Schema.Thrift.Types.PythonItem_file x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.pythonItem_file = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.PythonItem_module x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.pythonItem_module = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.PythonItem_class_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.pythonItem_class_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.PythonItem_method x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.pythonItem_method = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.PythonItem_field x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.pythonItem_field = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.PythonItem_function_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.pythonItem_function_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.PythonItem_value x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.pythonItem_value = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Src.Types.File Glean.Schema.Query.Thrift.Types.PythonItem where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.pythonItem_file = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.PythonModule Glean.Schema.Query.Thrift.Types.PythonItem where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.pythonItem_module = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.PythonClass Glean.Schema.Query.Thrift.Types.PythonItem where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.pythonItem_class_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.PythonMethod Glean.Schema.Query.Thrift.Types.PythonItem where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.pythonItem_method = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.PythonField Glean.Schema.Query.Thrift.Types.PythonItem where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.pythonItem_field = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.PythonFunction Glean.Schema.Query.Thrift.Types.PythonItem where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.pythonItem_function_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.PythonValue Glean.Schema.Query.Thrift.Types.PythonItem where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.pythonItem_value = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.HackKind = Glean.Schema.Thrift.Types.HackKind
type instance Glean.QueryOf Glean.Schema.Thrift.Types.HackKind = Glean.Schema.Query.Thrift.Types.HackKind

instance Glean.ToQuery Glean.Schema.Thrift.Types.HackKind where
  toQuery (Glean.Schema.Thrift.Types.HackKind_file x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.hackKind_file = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.HackKind_record x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.hackKind_record = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.HackKind_method x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.hackKind_method = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.HackKind_namespace_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.hackKind_namespace_ = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Src.Types.File Glean.Schema.Query.Thrift.Types.HackKind where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.hackKind_file = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.HackRecord Glean.Schema.Query.Thrift.Types.HackKind where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.hackKind_record = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.HackMethod Glean.Schema.Query.Thrift.Types.HackKind where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.hackKind_method = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Thrift.Types.HackName Glean.Schema.Query.Thrift.Types.HackKind where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Thrift.Types.hackKind_namespace_ = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.Target = Glean.Schema.Thrift.Types.Target
type instance Glean.QueryOf Glean.Schema.Thrift.Types.Target = Glean.Schema.Query.Thrift.Types.Target

instance Glean.ToQuery Glean.Schema.Thrift.Types.Target where
  toQuery (Glean.Schema.Thrift.Types.Target x1 x2) = Glean.Schema.Query.Thrift.Types.Target (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.MapType = Glean.Schema.Thrift.Types.MapType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.MapType = Glean.Schema.Query.Thrift.Types.MapType

instance Glean.ToQuery Glean.Schema.Thrift.Types.MapType where
  toQuery (Glean.Schema.Thrift.Types.MapType x1 x2) = Glean.Schema.Query.Thrift.Types.MapType (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.ContainerType = Glean.Schema.Thrift.Types.ContainerType
type instance Glean.QueryOf Glean.Schema.Thrift.Types.ContainerType = Glean.Schema.Query.Thrift.Types.ContainerType

instance Glean.ToQuery Glean.Schema.Thrift.Types.ContainerType where
  toQuery (Glean.Schema.Thrift.Types.ContainerType_list_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.containerType_list_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.ContainerType_set_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.containerType_set_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Thrift.Types.ContainerType_map_ x) = Data.Default.def { Glean.Schema.Query.Thrift.Types.containerType_map_ = Prelude.Just (Glean.toQuery x) }

type instance Glean.QueryResult Glean.Schema.Query.Thrift.Types.FieldSpecification = Glean.Schema.Thrift.Types.FieldSpecification
type instance Glean.QueryOf Glean.Schema.Thrift.Types.FieldSpecification = Glean.Schema.Query.Thrift.Types.FieldSpecification

instance Glean.ToQuery Glean.Schema.Thrift.Types.FieldSpecification where
  toQuery (Glean.Schema.Thrift.Types.FieldSpecification x1 x2 x3 x4) = Glean.Schema.Query.Thrift.Types.FieldSpecification (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))
