# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.thrift.types import (
    ToPython,
    FunctionSpecification,
    ExceptionType,
    TypeDefType,
    FromCpp2,
    FromHack,
    FileDeclaration,
    PythonField,
    FileError,
    PythonModule,
    ConstantType,
    FunctionName,
    TypeSpecification,
    NamespaceName,
    ExceptionVal,
    HackRecord,
    PythonMethod,
    PythonName,
    Namespace,
    ServiceName,
    IncludeStatement,
    TargetX,
    PythonClassContains,
    PythonFileModule,
    TypeDefException,
    EnumValue,
    ServiceDefinition,
    NamespaceValue,
    CompileTarget,
    PythonModuleFile,
    FileTarget,
    EnumValueDef,
    HackRecordContains,
    PythonValue,
    StructType,
    ServiceChild,
    Includes,
    QualName,
    OutputTarget,
    PythonFunction,
    StructuredAnnotation,
    File,
    HackMethod,
    Constant,
    HackName,
    NamedDecl,
    PythonModuleContains,
    HackMap,
    IncludeSpecial,
    FileXRefs,
    Mangle,
    StructVal,
    IncludeSplice,
    Lang,
    Literal,
    Identifier,
    UnionType,
    DeclarationNameSpan,
    FromPython,
    PythonClass,
    ToCpp2,
    ToHack,
    EnumerationType,
    FileOutput,
    ServiceParent,
    MangleLang,
    ExceptionName,
    EnumVal,
    UnionVal,
)


class ThriftToPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ToPython.7 {{ thrift = _, lang = _, python = _ }}", ToPython

  @staticmethod
  def angle_query(*, thrift: Tuple[()], lang: Tuple[()], python: Tuple[()]) -> "ThriftToPython":
    raise Exception("this function can only be called from @angle_query")

class ThriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FunctionSpecification.7 {{ name = _, result = _, arguments = _, throws_ = _ }}", FunctionSpecification

  @staticmethod
  def angle_query(*, name: Tuple[()], result: Tuple[()], arguments: Tuple[()], throws_: Tuple[()]) -> "ThriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ExceptionType.7 {{ name = _, fields = _, structuredAnnotations = _ }}", ExceptionType

  @staticmethod
  def angle_query(*, name: Tuple[()], fields: Tuple[()], structuredAnnotations: Tuple[()]) -> "ThriftExceptionType":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeDefType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.TypeDefType.7 {{ alias = _, type_ = _ }}", TypeDefType

  @staticmethod
  def angle_query(*, alias: Tuple[()], type_: Tuple[()]) -> "ThriftTypeDefType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FromCpp2.7 {{ cpp2 = _, thrift = _, role = _ }}", FromCpp2

  @staticmethod
  def angle_query(*, cpp2: Tuple[()], thrift: Tuple[()], role: Tuple[()]) -> "ThriftFromCpp2":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FromHack.7 {{ hack = _, thrift = _, role = _ }}", FromHack

  @staticmethod
  def angle_query(*, hack: Tuple[()], thrift: Tuple[()], role: Tuple[()]) -> "ThriftFromHack":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FileDeclaration.7 {{ file = _, decl = _ }}", FileDeclaration

  @staticmethod
  def angle_query(*, file: Tuple[()], decl: Tuple[()]) -> "ThriftFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonField.7 {{ name = _, class_ = _ }}", PythonField

  @staticmethod
  def angle_query(*, name: Tuple[()], class_: Tuple[()]) -> "ThriftPythonField":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileError(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FileError.7 {{ file = _, error = _ }}", FileError

  @staticmethod
  def angle_query(*, file: Tuple[()], error: str) -> "ThriftFileError":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonModule.7 {json.dumps(key)}", PythonModule

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "ThriftPythonModule":
    raise Exception("this function can only be called from @angle_query")

class ThriftConstantType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ConstantType.7 {{ name = _, type_ = _ }}", ConstantType

  @staticmethod
  def angle_query(*, name: Tuple[()], type_: Tuple[()]) -> "ThriftConstantType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FunctionName.7 {{ service_ = _, name = _, locName = _ }}", FunctionName

  @staticmethod
  def angle_query(*, service_: Tuple[()], name: Tuple[()], locName: Tuple[()]) -> "ThriftFunctionName":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.TypeSpecification.7 {json.dumps(key)}", TypeSpecification

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "ThriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespaceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.NamespaceName.7 {json.dumps(key)}", NamespaceName

  @staticmethod
  def angle_query(*, arg: str) -> "ThriftNamespaceName":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ExceptionVal.7 {json.dumps(key)}", ExceptionVal

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "ThriftExceptionVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.HackRecord.7 {{ name = _, kind = _ }}", HackRecord

  @staticmethod
  def angle_query(*, name: Tuple[()], kind: Tuple[()]) -> "ThriftHackRecord":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonMethod.7 {{ name = _, class_ = _ }}", PythonMethod

  @staticmethod
  def angle_query(*, name: Tuple[()], class_: Tuple[()]) -> "ThriftPythonMethod":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonName.7 {json.dumps(key)}", PythonName

  @staticmethod
  def angle_query(*, arg: str) -> "ThriftPythonName":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.Namespace.7 {{ target = _, name = _, namespace_ = _, quoted = _ }}", Namespace

  @staticmethod
  def angle_query(*, target: Tuple[()], name: Tuple[()], namespace_: Tuple[()], quoted: bool) -> "ThriftNamespace":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ServiceName.7 {{ name = _, locName = _ }}", ServiceName

  @staticmethod
  def angle_query(*, name: Tuple[()], locName: Tuple[()]) -> "ThriftServiceName":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.IncludeStatement.7 {{ target = _, source = _, locSource = _ }}", IncludeStatement

  @staticmethod
  def angle_query(*, target: Tuple[()], source: Tuple[()], locSource: Tuple[()]) -> "ThriftIncludeStatement":
    raise Exception("this function can only be called from @angle_query")

class ThriftTargetX(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.TargetX.7 {{ target = _, fileRef = _, locRef = _ }}", TargetX

  @staticmethod
  def angle_query(*, target: Tuple[()], fileRef: Tuple[()], locRef: Tuple[()]) -> "ThriftTargetX":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonClassContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonClassContains.7 {{ class_ = _, methods = _, fields = _ }}", PythonClassContains

  @staticmethod
  def angle_query(*, class_: Tuple[()], methods: Tuple[()], fields: Tuple[()]) -> "ThriftPythonClassContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonFileModule.7 {{ file = _, module = _ }}", PythonFileModule

  @staticmethod
  def angle_query(*, file: Tuple[()], module: Tuple[()]) -> "ThriftPythonFileModule":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.TypeDefException.7 {{ alias = _, type_ = _ }}", TypeDefException

  @staticmethod
  def angle_query(*, alias: Tuple[()], type_: Tuple[()]) -> "ThriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.EnumValue.7 {{ enum_ = _, name = _, locName = _ }}", EnumValue

  @staticmethod
  def angle_query(*, enum_: Tuple[()], name: Tuple[()], locName: Tuple[()]) -> "ThriftEnumValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ServiceDefinition.7 {{ name = _, functions = _ }}", ServiceDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()], functions: Tuple[()]) -> "ThriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespaceValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.NamespaceValue.7 {json.dumps(key)}", NamespaceValue

  @staticmethod
  def angle_query(*, arg: str) -> "ThriftNamespaceValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftCompileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.CompileTarget.7 {{ includes = _, lang = _, compile = _ }}", CompileTarget

  @staticmethod
  def angle_query(*, includes: Tuple[()], lang: Tuple[()], compile: Tuple[()]) -> "ThriftCompileTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModuleFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonModuleFile.7 {{ module = _, file = _ }}", PythonModuleFile

  @staticmethod
  def angle_query(*, module: Tuple[()], file: Tuple[()]) -> "ThriftPythonModuleFile":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FileTarget.7 {{ file = _, target = _ }}", FileTarget

  @staticmethod
  def angle_query(*, file: Tuple[()], target: Tuple[()]) -> "ThriftFileTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.EnumValueDef.7 {{ name = _, value = _ }}", EnumValueDef

  @staticmethod
  def angle_query(*, name: Tuple[()], value: Tuple[()]) -> "ThriftEnumValueDef":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackRecordContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.HackRecordContains.7 {{ record = _, methods = _ }}", HackRecordContains

  @staticmethod
  def angle_query(*, record: Tuple[()], methods: Tuple[()]) -> "ThriftHackRecordContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonValue.7 {{ name = _, module = _ }}", PythonValue

  @staticmethod
  def angle_query(*, name: Tuple[()], module: Tuple[()]) -> "ThriftPythonValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.StructType.7 {{ name = _, fields = _, structuredAnnotations = _ }}", StructType

  @staticmethod
  def angle_query(*, name: Tuple[()], fields: Tuple[()], structuredAnnotations: Tuple[()]) -> "ThriftStructType":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ServiceChild.7 {{ parent = _, child = _ }}", ServiceChild

  @staticmethod
  def angle_query(*, parent: Tuple[()], child: Tuple[()]) -> "ThriftServiceChild":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludes(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.Includes.7 {{ source = _, target = _ }}", Includes

  @staticmethod
  def angle_query(*, source: Tuple[()], target: Tuple[()]) -> "ThriftIncludes":
    raise Exception("this function can only be called from @angle_query")

class ThriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.QualName.7 {{ file = _, name = _ }}", QualName

  @staticmethod
  def angle_query(*, file: Tuple[()], name: Tuple[()]) -> "ThriftQualName":
    raise Exception("this function can only be called from @angle_query")

class ThriftOutputTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.OutputTarget.7 {{ compile = _, output = _, out = _ }}", OutputTarget

  @staticmethod
  def angle_query(*, compile: Tuple[()], output: Tuple[()], out: Tuple[()]) -> "ThriftOutputTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonFunction.7 {{ name = _, module = _ }}", PythonFunction

  @staticmethod
  def angle_query(*, name: Tuple[()], module: Tuple[()]) -> "ThriftPythonFunction":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructuredAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.StructuredAnnotation.7 {{ type_ = _, value = _ }}", StructuredAnnotation

  @staticmethod
  def angle_query(*, type_: Tuple[()], value: Tuple[()]) -> "ThriftStructuredAnnotation":
    raise Exception("this function can only be called from @angle_query")

class ThriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.File.7 {json.dumps(key)}", File

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "ThriftFile":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.HackMethod.7 {{ name = _, record = _ }}", HackMethod

  @staticmethod
  def angle_query(*, name: Tuple[()], record: Tuple[()]) -> "ThriftHackMethod":
    raise Exception("this function can only be called from @angle_query")

class ThriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.Constant.7 {{ name = _, locName = _ }}", Constant

  @staticmethod
  def angle_query(*, name: Tuple[()], locName: Tuple[()]) -> "ThriftConstant":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.HackName.7 {json.dumps(key)}", HackName

  @staticmethod
  def angle_query(*, arg: str) -> "ThriftHackName":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.NamedDecl.7 {{ name = _, locName = _ }}", NamedDecl

  @staticmethod
  def angle_query(*, name: Tuple[()], locName: Tuple[()]) -> "ThriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModuleContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonModuleContains.7 {{ module = _, classes = _, functions = _, values = _ }}", PythonModuleContains

  @staticmethod
  def angle_query(*, module: Tuple[()], classes: Tuple[()], functions: Tuple[()], values: Tuple[()]) -> "ThriftPythonModuleContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.HackMap.7 {{ source = _, repoCode = _, path = _, kind = _, mangledsvcs = _, rest = _, server = _ }}", HackMap

  @staticmethod
  def angle_query(*, source: Tuple[()], repoCode: Tuple[()], path: str, kind: Tuple[()], mangledsvcs: bool, rest: bool, server: bool) -> "ThriftHackMap":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeSpecial(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.IncludeSpecial.7 {{ source = _, special = _, target = _ }}", IncludeSpecial

  @staticmethod
  def angle_query(*, source: Tuple[()], special: str, target: Tuple[()]) -> "ThriftIncludeSpecial":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FileXRefs.7 {{ file = _, targets = _, xrefs = _ }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Tuple[()], targets: Tuple[()], xrefs: Tuple[()]) -> "ThriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class ThriftMangle(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.Mangle.7 {{ from = _, to = _, lang = _ }}", Mangle

  @staticmethod
  def angle_query(*, from: Tuple[()], to: Tuple[()], lang: Tuple[()]) -> "ThriftMangle":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.StructVal.7 {{ fields = _ }}", StructVal

  @staticmethod
  def angle_query(*, fields: Tuple[()]) -> "ThriftStructVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeSplice(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.IncludeSplice.7 {json.dumps(key)}", IncludeSplice

  @staticmethod
  def angle_query(*, arg: str) -> "ThriftIncludeSplice":
    raise Exception("this function can only be called from @angle_query")

class ThriftLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.Lang.7 {json.dumps(key)}", Lang

  @staticmethod
  def angle_query(*, arg: str) -> "ThriftLang":
    raise Exception("this function can only be called from @angle_query")

class ThriftLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.Literal.7 {json.dumps(key)}", Literal

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

class ThriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.Identifier.7 {json.dumps(key)}", Identifier

  @staticmethod
  def angle_query(*, arg: str) -> "ThriftIdentifier":
    raise Exception("this function can only be called from @angle_query")

class ThriftUnionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.UnionType.7 {{ name = _, alts = _ }}", UnionType

  @staticmethod
  def angle_query(*, name: Tuple[()], alts: Tuple[()]) -> "ThriftUnionType":
    raise Exception("this function can only be called from @angle_query")

class ThriftDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.DeclarationNameSpan.7 {{ decl = _, name = _, file = _, span = _ }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Tuple[()], name: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "ThriftDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FromPython.7 {{ python = _, thrift = _, role = _, lang = _ }}", FromPython

  @staticmethod
  def angle_query(*, python: Tuple[()], thrift: Tuple[()], role: Tuple[()], lang: Tuple[()]) -> "ThriftFromPython":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.PythonClass.7 {{ name = _, module = _ }}", PythonClass

  @staticmethod
  def angle_query(*, name: Tuple[()], module: Tuple[()]) -> "ThriftPythonClass":
    raise Exception("this function can only be called from @angle_query")

class ThriftToCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ToCpp2.7 {{ thrift = _, cpp2 = _ }}", ToCpp2

  @staticmethod
  def angle_query(*, thrift: Tuple[()], cpp2: Tuple[()]) -> "ThriftToCpp2":
    raise Exception("this function can only be called from @angle_query")

class ThriftToHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ToHack.7 {{ thrift = _, hack = _ }}", ToHack

  @staticmethod
  def angle_query(*, thrift: Tuple[()], hack: Tuple[()]) -> "ThriftToHack":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumerationType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.EnumerationType.7 {{ name = _, value = _ }}", EnumerationType

  @staticmethod
  def angle_query(*, name: Tuple[()], value: Tuple[()]) -> "ThriftEnumerationType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileOutput(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.FileOutput.7 {{ output = _, origin = _ }}", FileOutput

  @staticmethod
  def angle_query(*, output: Tuple[()], origin: Tuple[()]) -> "ThriftFileOutput":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ServiceParent.7 {{ child = _, parent = _ }}", ServiceParent

  @staticmethod
  def angle_query(*, child: Tuple[()], parent: Tuple[()]) -> "ThriftServiceParent":
    raise Exception("this function can only be called from @angle_query")

class ThriftMangleLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.MangleLang.7 {json.dumps(key)}", MangleLang

  @staticmethod
  def angle_query(*, arg: str) -> "ThriftMangleLang":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.ExceptionName.7 {{ name = _, locName = _ }}", ExceptionName

  @staticmethod
  def angle_query(*, name: Tuple[()], locName: Tuple[()]) -> "ThriftExceptionName":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.EnumVal.7 {{ name = _, loc = _ }}", EnumVal

  @staticmethod
  def angle_query(*, name: Tuple[()], loc: Tuple[()]) -> "ThriftEnumVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftUnionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"thrift.UnionVal.7 {{ field = _ }}", UnionVal

  @staticmethod
  def angle_query(*, field: Tuple[()]) -> "ThriftUnionVal":
    raise Exception("this function can only be called from @angle_query")


