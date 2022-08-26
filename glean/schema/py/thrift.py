# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ToPython.7 {{ }}", ToPython
    return f"thrift.ToPython.7 { concatenateFields(key) }", ToPython

  @staticmethod
  def angle_query(*, thrift: Optional[Tuple[()]] = None, lang: Optional[Tuple[()]] = None, python: Optional[Tuple[()]] = None) -> "ThriftToPython":
    raise Exception("this function can only be called from @angle_query")

class ThriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FunctionSpecification.7 {{ }}", FunctionSpecification
    return f"thrift.FunctionSpecification.7 { concatenateFields(key) }", FunctionSpecification

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, result: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None) -> "ThriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ExceptionType.7 {{ }}", ExceptionType
    return f"thrift.ExceptionType.7 { concatenateFields(key) }", ExceptionType

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, structuredAnnotations: Optional[Tuple[()]] = None) -> "ThriftExceptionType":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeDefType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.TypeDefType.7 {{ }}", TypeDefType
    return f"thrift.TypeDefType.7 { concatenateFields(key) }", TypeDefType

  @staticmethod
  def angle_query(*, alias: Optional[Tuple[()]] = None, type_: Optional[Tuple[()]] = None) -> "ThriftTypeDefType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FromCpp2.7 {{ }}", FromCpp2
    return f"thrift.FromCpp2.7 { concatenateFields(key) }", FromCpp2

  @staticmethod
  def angle_query(*, cpp2: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None) -> "ThriftFromCpp2":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FromHack.7 {{ }}", FromHack
    return f"thrift.FromHack.7 { concatenateFields(key) }", FromHack

  @staticmethod
  def angle_query(*, hack: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None) -> "ThriftFromHack":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FileDeclaration.7 {{ }}", FileDeclaration
    return f"thrift.FileDeclaration.7 { concatenateFields(key) }", FileDeclaration

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "ThriftFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonField.7 {{ }}", PythonField
    return f"thrift.PythonField.7 { concatenateFields(key) }", PythonField

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, class_: Optional[Tuple[()]] = None) -> "ThriftPythonField":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileError(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FileError.7 {{ }}", FileError
    return f"thrift.FileError.7 { concatenateFields(key) }", FileError

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, error: Optional[str] = None) -> "ThriftFileError":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonModule.7 {{ }}", PythonModule
    return f"thrift.PythonModule.7 {key}", PythonModule

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ThriftPythonModule":
    raise Exception("this function can only be called from @angle_query")

class ThriftConstantType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ConstantType.7 {{ }}", ConstantType
    return f"thrift.ConstantType.7 { concatenateFields(key) }", ConstantType

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type_: Optional[Tuple[()]] = None) -> "ThriftConstantType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FunctionName.7 {{ }}", FunctionName
    return f"thrift.FunctionName.7 { concatenateFields(key) }", FunctionName

  @staticmethod
  def angle_query(*, service_: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "ThriftFunctionName":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.TypeSpecification.7 {{ }}", TypeSpecification
    return f"thrift.TypeSpecification.7 {key}", TypeSpecification

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ThriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespaceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.NamespaceName.7 {{ }}", NamespaceName
    return f"thrift.NamespaceName.7 {key}", NamespaceName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftNamespaceName":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ExceptionVal.7 {{ }}", ExceptionVal
    return f"thrift.ExceptionVal.7 {key}", ExceptionVal

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ThriftExceptionVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.HackRecord.7 {{ }}", HackRecord
    return f"thrift.HackRecord.7 { concatenateFields(key) }", HackRecord

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "ThriftHackRecord":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonMethod.7 {{ }}", PythonMethod
    return f"thrift.PythonMethod.7 { concatenateFields(key) }", PythonMethod

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, class_: Optional[Tuple[()]] = None) -> "ThriftPythonMethod":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonName.7 {{ }}", PythonName
    return f"thrift.PythonName.7 {key}", PythonName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftPythonName":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.Namespace.7 {{ }}", Namespace
    return f"thrift.Namespace.7 { concatenateFields(key) }", Namespace

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, namespace_: Optional[Tuple[()]] = None, quoted: Optional[bool] = None) -> "ThriftNamespace":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ServiceName.7 {{ }}", ServiceName
    return f"thrift.ServiceName.7 { concatenateFields(key) }", ServiceName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "ThriftServiceName":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.IncludeStatement.7 {{ }}", IncludeStatement
    return f"thrift.IncludeStatement.7 { concatenateFields(key) }", IncludeStatement

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, locSource: Optional[Tuple[()]] = None) -> "ThriftIncludeStatement":
    raise Exception("this function can only be called from @angle_query")

class ThriftTargetX(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.TargetX.7 {{ }}", TargetX
    return f"thrift.TargetX.7 { concatenateFields(key) }", TargetX

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, fileRef: Optional[Tuple[()]] = None, locRef: Optional[Tuple[()]] = None) -> "ThriftTargetX":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonClassContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonClassContains.7 {{ }}", PythonClassContains
    return f"thrift.PythonClassContains.7 { concatenateFields(key) }", PythonClassContains

  @staticmethod
  def angle_query(*, class_: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None) -> "ThriftPythonClassContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonFileModule.7 {{ }}", PythonFileModule
    return f"thrift.PythonFileModule.7 { concatenateFields(key) }", PythonFileModule

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, module: Optional[Tuple[()]] = None) -> "ThriftPythonFileModule":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.TypeDefException.7 {{ }}", TypeDefException
    return f"thrift.TypeDefException.7 { concatenateFields(key) }", TypeDefException

  @staticmethod
  def angle_query(*, alias: Optional[Tuple[()]] = None, type_: Optional[Tuple[()]] = None) -> "ThriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.EnumValue.7 {{ }}", EnumValue
    return f"thrift.EnumValue.7 { concatenateFields(key) }", EnumValue

  @staticmethod
  def angle_query(*, enum_: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "ThriftEnumValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ServiceDefinition.7 {{ }}", ServiceDefinition
    return f"thrift.ServiceDefinition.7 { concatenateFields(key) }", ServiceDefinition

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, functions: Optional[Tuple[()]] = None) -> "ThriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespaceValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.NamespaceValue.7 {{ }}", NamespaceValue
    return f"thrift.NamespaceValue.7 {key}", NamespaceValue

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftNamespaceValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftCompileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.CompileTarget.7 {{ }}", CompileTarget
    return f"thrift.CompileTarget.7 { concatenateFields(key) }", CompileTarget

  @staticmethod
  def angle_query(*, includes: Optional[Tuple[()]] = None, lang: Optional[Tuple[()]] = None, compile: Optional[Tuple[()]] = None) -> "ThriftCompileTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModuleFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonModuleFile.7 {{ }}", PythonModuleFile
    return f"thrift.PythonModuleFile.7 { concatenateFields(key) }", PythonModuleFile

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "ThriftPythonModuleFile":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FileTarget.7 {{ }}", FileTarget
    return f"thrift.FileTarget.7 { concatenateFields(key) }", FileTarget

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "ThriftFileTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.EnumValueDef.7 {{ }}", EnumValueDef
    return f"thrift.EnumValueDef.7 { concatenateFields(key) }", EnumValueDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "ThriftEnumValueDef":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackRecordContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.HackRecordContains.7 {{ }}", HackRecordContains
    return f"thrift.HackRecordContains.7 { concatenateFields(key) }", HackRecordContains

  @staticmethod
  def angle_query(*, record: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None) -> "ThriftHackRecordContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonValue.7 {{ }}", PythonValue
    return f"thrift.PythonValue.7 { concatenateFields(key) }", PythonValue

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, module: Optional[Tuple[()]] = None) -> "ThriftPythonValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.StructType.7 {{ }}", StructType
    return f"thrift.StructType.7 { concatenateFields(key) }", StructType

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, structuredAnnotations: Optional[Tuple[()]] = None) -> "ThriftStructType":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ServiceChild.7 {{ }}", ServiceChild
    return f"thrift.ServiceChild.7 { concatenateFields(key) }", ServiceChild

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "ThriftServiceChild":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludes(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.Includes.7 {{ }}", Includes
    return f"thrift.Includes.7 { concatenateFields(key) }", Includes

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "ThriftIncludes":
    raise Exception("this function can only be called from @angle_query")

class ThriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.QualName.7 {{ }}", QualName
    return f"thrift.QualName.7 { concatenateFields(key) }", QualName

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None) -> "ThriftQualName":
    raise Exception("this function can only be called from @angle_query")

class ThriftOutputTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.OutputTarget.7 {{ }}", OutputTarget
    return f"thrift.OutputTarget.7 { concatenateFields(key) }", OutputTarget

  @staticmethod
  def angle_query(*, compile: Optional[Tuple[()]] = None, output: Optional[Tuple[()]] = None, out: Optional[Tuple[()]] = None) -> "ThriftOutputTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonFunction.7 {{ }}", PythonFunction
    return f"thrift.PythonFunction.7 { concatenateFields(key) }", PythonFunction

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, module: Optional[Tuple[()]] = None) -> "ThriftPythonFunction":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructuredAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.StructuredAnnotation.7 {{ }}", StructuredAnnotation
    return f"thrift.StructuredAnnotation.7 { concatenateFields(key) }", StructuredAnnotation

  @staticmethod
  def angle_query(*, type_: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "ThriftStructuredAnnotation":
    raise Exception("this function can only be called from @angle_query")

class ThriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.File.7 {{ }}", File
    return f"thrift.File.7 {key}", File

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ThriftFile":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.HackMethod.7 {{ }}", HackMethod
    return f"thrift.HackMethod.7 { concatenateFields(key) }", HackMethod

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, record: Optional[Tuple[()]] = None) -> "ThriftHackMethod":
    raise Exception("this function can only be called from @angle_query")

class ThriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.Constant.7 {{ }}", Constant
    return f"thrift.Constant.7 { concatenateFields(key) }", Constant

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "ThriftConstant":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.HackName.7 {{ }}", HackName
    return f"thrift.HackName.7 {key}", HackName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftHackName":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.NamedDecl.7 {{ }}", NamedDecl
    return f"thrift.NamedDecl.7 { concatenateFields(key) }", NamedDecl

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "ThriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModuleContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonModuleContains.7 {{ }}", PythonModuleContains
    return f"thrift.PythonModuleContains.7 { concatenateFields(key) }", PythonModuleContains

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, classes: Optional[Tuple[()]] = None, functions: Optional[Tuple[()]] = None, values: Optional[Tuple[()]] = None) -> "ThriftPythonModuleContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.HackMap.7 {{ }}", HackMap
    return f"thrift.HackMap.7 { concatenateFields(key) }", HackMap

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, repoCode: Optional[Tuple[()]] = None, path: Optional[str] = None, kind: Optional[Tuple[()]] = None, mangledsvcs: Optional[bool] = None, rest: Optional[bool] = None, server: Optional[bool] = None) -> "ThriftHackMap":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeSpecial(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.IncludeSpecial.7 {{ }}", IncludeSpecial
    return f"thrift.IncludeSpecial.7 { concatenateFields(key) }", IncludeSpecial

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, special: Optional[str] = None, target: Optional[Tuple[()]] = None) -> "ThriftIncludeSpecial":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FileXRefs.7 {{ }}", FileXRefs
    return f"thrift.FileXRefs.7 { concatenateFields(key) }", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, targets: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "ThriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class ThriftMangle(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.Mangle.7 {{ }}", Mangle
    return f"thrift.Mangle.7 { concatenateFields(key) }", Mangle

  @staticmethod
  def angle_query(*, _from: Optional[Tuple[()]] = None, to: Optional[Tuple[()]] = None, lang: Optional[Tuple[()]] = None) -> "ThriftMangle":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.StructVal.7 {{ }}", StructVal
    return f"thrift.StructVal.7 { concatenateFields(key) }", StructVal

  @staticmethod
  def angle_query(*, fields: Optional[Tuple[()]] = None) -> "ThriftStructVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeSplice(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.IncludeSplice.7 {{ }}", IncludeSplice
    return f"thrift.IncludeSplice.7 {key}", IncludeSplice

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftIncludeSplice":
    raise Exception("this function can only be called from @angle_query")

class ThriftLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.Lang.7 {{ }}", Lang
    return f"thrift.Lang.7 {key}", Lang

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftLang":
    raise Exception("this function can only be called from @angle_query")

class ThriftLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.Literal.7 {{ }}", Literal
    return f"thrift.Literal.7 {key}", Literal

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

class ThriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.Identifier.7 {{ }}", Identifier
    return f"thrift.Identifier.7 {key}", Identifier

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftIdentifier":
    raise Exception("this function can only be called from @angle_query")

class ThriftUnionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.UnionType.7 {{ }}", UnionType
    return f"thrift.UnionType.7 { concatenateFields(key) }", UnionType

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, alts: Optional[Tuple[()]] = None) -> "ThriftUnionType":
    raise Exception("this function can only be called from @angle_query")

class ThriftDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.DeclarationNameSpan.7 {{ }}", DeclarationNameSpan
    return f"thrift.DeclarationNameSpan.7 { concatenateFields(key) }", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "ThriftDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FromPython.7 {{ }}", FromPython
    return f"thrift.FromPython.7 { concatenateFields(key) }", FromPython

  @staticmethod
  def angle_query(*, python: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None, lang: Optional[Tuple[()]] = None) -> "ThriftFromPython":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.PythonClass.7 {{ }}", PythonClass
    return f"thrift.PythonClass.7 { concatenateFields(key) }", PythonClass

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, module: Optional[Tuple[()]] = None) -> "ThriftPythonClass":
    raise Exception("this function can only be called from @angle_query")

class ThriftToCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ToCpp2.7 {{ }}", ToCpp2
    return f"thrift.ToCpp2.7 { concatenateFields(key) }", ToCpp2

  @staticmethod
  def angle_query(*, thrift: Optional[Tuple[()]] = None, cpp2: Optional[Tuple[()]] = None) -> "ThriftToCpp2":
    raise Exception("this function can only be called from @angle_query")

class ThriftToHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ToHack.7 {{ }}", ToHack
    return f"thrift.ToHack.7 { concatenateFields(key) }", ToHack

  @staticmethod
  def angle_query(*, thrift: Optional[Tuple[()]] = None, hack: Optional[Tuple[()]] = None) -> "ThriftToHack":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumerationType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.EnumerationType.7 {{ }}", EnumerationType
    return f"thrift.EnumerationType.7 { concatenateFields(key) }", EnumerationType

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "ThriftEnumerationType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileOutput(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.FileOutput.7 {{ }}", FileOutput
    return f"thrift.FileOutput.7 { concatenateFields(key) }", FileOutput

  @staticmethod
  def angle_query(*, output: Optional[Tuple[()]] = None, origin: Optional[Tuple[()]] = None) -> "ThriftFileOutput":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ServiceParent.7 {{ }}", ServiceParent
    return f"thrift.ServiceParent.7 { concatenateFields(key) }", ServiceParent

  @staticmethod
  def angle_query(*, child: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "ThriftServiceParent":
    raise Exception("this function can only be called from @angle_query")

class ThriftMangleLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.MangleLang.7 {{ }}", MangleLang
    return f"thrift.MangleLang.7 {key}", MangleLang

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftMangleLang":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.ExceptionName.7 {{ }}", ExceptionName
    return f"thrift.ExceptionName.7 { concatenateFields(key) }", ExceptionName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "ThriftExceptionName":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.EnumVal.7 {{ }}", EnumVal
    return f"thrift.EnumVal.7 { concatenateFields(key) }", EnumVal

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "ThriftEnumVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftUnionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"thrift.UnionVal.7 {{ }}", UnionVal
    return f"thrift.UnionVal.7 { concatenateFields(key) }", UnionVal

  @staticmethod
  def angle_query(*, field: Optional[Tuple[()]] = None) -> "ThriftUnionVal":
    raise Exception("this function can only be called from @angle_query")


