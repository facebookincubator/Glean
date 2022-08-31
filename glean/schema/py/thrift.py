# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.buck import *
from glean.schema.py.scm import *
from glean.schema.py.src import *


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
  def build_angle(__env: Dict[str, R], thrift: ast.Expr, lang: ast.Expr, python: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ToPython.7 {{ thrift = {angle_for(__env, thrift)}, lang = {angle_for(__env, lang)}, python = {angle_for(__env, python)} }}", ToPython

  @staticmethod
  def angle_query(*, thrift: Optional["ThriftFile"] = None, lang: Optional["ThriftLang"] = None, python: Optional[Tuple[()]] = None) -> "ThriftToPython":
    raise Exception("this function can only be called from @angle_query")

class ThriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, result: ast.Expr, arguments: ast.Expr, throws_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FunctionSpecification.7 {{ name = {angle_for(__env, name)}, result = {angle_for(__env, result)}, arguments = {angle_for(__env, arguments)}, throws_ = {angle_for(__env, throws_)} }}", FunctionSpecification

  @staticmethod
  def angle_query(*, name: Optional["ThriftFunctionName"] = None, result: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None) -> "ThriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, structuredAnnotations: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ExceptionType.7 {{ name = {angle_for(__env, name)}, fields = {angle_for(__env, fields)}, structuredAnnotations = {angle_for(__env, structuredAnnotations)} }}", ExceptionType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, fields: Optional[Tuple[()]] = None, structuredAnnotations: Optional[Tuple[()]] = None) -> "ThriftExceptionType":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeDefType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], alias: ast.Expr, type_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.TypeDefType.7 {{ alias = {angle_for(__env, alias)}, type_ = {angle_for(__env, type_)} }}", TypeDefType

  @staticmethod
  def angle_query(*, alias: Optional["ThriftQualName"] = None, type_: Optional["ThriftTypeSpecification"] = None) -> "ThriftTypeDefType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cpp2: ast.Expr, thrift: ast.Expr, role: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FromCpp2.7 {{ cpp2 = {angle_for(__env, cpp2)}, thrift = {angle_for(__env, thrift)}, role = {angle_for(__env, role)} }}", FromCpp2

  @staticmethod
  def angle_query(*, cpp2: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None) -> "ThriftFromCpp2":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], hack: ast.Expr, thrift: ast.Expr, role: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FromHack.7 {{ hack = {angle_for(__env, hack)}, thrift = {angle_for(__env, thrift)}, role = {angle_for(__env, role)} }}", FromHack

  @staticmethod
  def angle_query(*, hack: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None) -> "ThriftFromHack":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileDeclaration.7 {{ file = {angle_for(__env, file)}, decl = {angle_for(__env, decl)} }}", FileDeclaration

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, decl: Optional[Tuple[()]] = None) -> "ThriftFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, class_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonField.7 {{ name = {angle_for(__env, name)}, class_ = {angle_for(__env, class_)} }}", PythonField

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, class_: Optional["ThriftPythonClass"] = None) -> "ThriftPythonField":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileError(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, error: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileError.7 {{ file = {angle_for(__env, file)}, error = {angle_for(__env, error)} }}", FileError

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, error: Optional[str] = None) -> "ThriftFileError":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonModule.7 {angle_for(__env, arg)}", PythonModule

  @staticmethod
  def angle_query(*, arg: Optional["ThriftPythonName"] = None) -> "ThriftPythonModule":
    raise Exception("this function can only be called from @angle_query")

class ThriftConstantType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ConstantType.7 {{ name = {angle_for(__env, name)}, type_ = {angle_for(__env, type_)} }}", ConstantType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, type_: Optional["ThriftTypeSpecification"] = None) -> "ThriftConstantType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], service_: ast.Expr, name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FunctionName.7 {{ service_ = {angle_for(__env, service_)}, name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", FunctionName

  @staticmethod
  def angle_query(*, service_: Optional["ThriftServiceName"] = None, name: Optional["ThriftIdentifier"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftFunctionName":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.TypeSpecification.7 {angle_for(__env, arg)}", TypeSpecification

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ThriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespaceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.NamespaceName.7 {angle_for(__env, arg)}", NamespaceName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftNamespaceName":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ExceptionVal.7 {angle_for(__env, arg)}", ExceptionVal

  @staticmethod
  def angle_query(*, arg: Optional["ThriftStructVal"] = None) -> "ThriftExceptionVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackRecord.7 {{ name = {angle_for(__env, name)}, kind = {angle_for(__env, kind)} }}", HackRecord

  @staticmethod
  def angle_query(*, name: Optional["ThriftHackName"] = None, kind: Optional[Tuple[()]] = None) -> "ThriftHackRecord":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, class_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonMethod.7 {{ name = {angle_for(__env, name)}, class_ = {angle_for(__env, class_)} }}", PythonMethod

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, class_: Optional["ThriftPythonClass"] = None) -> "ThriftPythonMethod":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonName.7 {angle_for(__env, arg)}", PythonName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftPythonName":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, name: ast.Expr, namespace_: ast.Expr, quoted: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Namespace.7 {{ target = {angle_for(__env, target)}, name = {angle_for(__env, name)}, namespace_ = {angle_for(__env, namespace_)}, quoted = {angle_for(__env, quoted)} }}", Namespace

  @staticmethod
  def angle_query(*, target: Optional["ThriftFile"] = None, name: Optional["ThriftNamespaceName"] = None, namespace_: Optional["ThriftNamespaceValue"] = None, quoted: Optional[bool] = None) -> "ThriftNamespace":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ServiceName.7 {{ name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", ServiceName

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftServiceName":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr, locSource: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.IncludeStatement.7 {{ target = {angle_for(__env, target)}, source = {angle_for(__env, source)}, locSource = {angle_for(__env, locSource)} }}", IncludeStatement

  @staticmethod
  def angle_query(*, target: Optional["ThriftFile"] = None, source: Optional["ThriftFile"] = None, locSource: Optional[Tuple[()]] = None) -> "ThriftIncludeStatement":
    raise Exception("this function can only be called from @angle_query")

class ThriftTargetX(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, fileRef: ast.Expr, locRef: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.TargetX.7 {{ target = {angle_for(__env, target)}, fileRef = {angle_for(__env, fileRef)}, locRef = {angle_for(__env, locRef)} }}", TargetX

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, fileRef: Optional["ThriftFile"] = None, locRef: Optional[Tuple[()]] = None) -> "ThriftTargetX":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonClassContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr, methods: ast.Expr, fields: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonClassContains.7 {{ class_ = {angle_for(__env, class_)}, methods = {angle_for(__env, methods)}, fields = {angle_for(__env, fields)} }}", PythonClassContains

  @staticmethod
  def angle_query(*, class_: Optional["ThriftPythonClass"] = None, methods: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None) -> "ThriftPythonClassContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonFileModule.7 {{ file = {angle_for(__env, file)}, module = {angle_for(__env, module)} }}", PythonFileModule

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, module: Optional["ThriftPythonModule"] = None) -> "ThriftPythonFileModule":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], alias: ast.Expr, type_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.TypeDefException.7 {{ alias = {angle_for(__env, alias)}, type_ = {angle_for(__env, type_)} }}", TypeDefException

  @staticmethod
  def angle_query(*, alias: Optional["ThriftNamedDecl"] = None, type_: Optional[Tuple[()]] = None) -> "ThriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], enum_: ast.Expr, name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.EnumValue.7 {{ enum_ = {angle_for(__env, enum_)}, name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", EnumValue

  @staticmethod
  def angle_query(*, enum_: Optional[Tuple[()]] = None, name: Optional["ThriftIdentifier"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftEnumValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, functions: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ServiceDefinition.7 {{ name = {angle_for(__env, name)}, functions = {angle_for(__env, functions)} }}", ServiceDefinition

  @staticmethod
  def angle_query(*, name: Optional["ThriftServiceName"] = None, functions: Optional[Tuple[()]] = None) -> "ThriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespaceValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.NamespaceValue.7 {angle_for(__env, arg)}", NamespaceValue

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftNamespaceValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftCompileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], includes: ast.Expr, lang: ast.Expr, compile: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.CompileTarget.7 {{ includes = {angle_for(__env, includes)}, lang = {angle_for(__env, lang)}, compile = {angle_for(__env, compile)} }}", CompileTarget

  @staticmethod
  def angle_query(*, includes: Optional["ThriftFileTarget"] = None, lang: Optional[Tuple[()]] = None, compile: Optional["BuckTarget"] = None) -> "ThriftCompileTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModuleFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonModuleFile.7 {{ module = {angle_for(__env, module)}, file = {angle_for(__env, file)} }}", PythonModuleFile

  @staticmethod
  def angle_query(*, module: Optional["ThriftPythonModule"] = None, file: Optional["SrcFile"] = None) -> "ThriftPythonModuleFile":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileTarget.7 {{ file = {angle_for(__env, file)}, target = {angle_for(__env, target)} }}", FileTarget

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, target: Optional["BuckTarget"] = None) -> "ThriftFileTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.EnumValueDef.7 {{ name = {angle_for(__env, name)}, value = {angle_for(__env, value)} }}", EnumValueDef

  @staticmethod
  def angle_query(*, name: Optional["ThriftEnumValue"] = None, value: Optional[Tuple[()]] = None) -> "ThriftEnumValueDef":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackRecordContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], record: ast.Expr, methods: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackRecordContains.7 {{ record = {angle_for(__env, record)}, methods = {angle_for(__env, methods)} }}", HackRecordContains

  @staticmethod
  def angle_query(*, record: Optional["ThriftHackRecord"] = None, methods: Optional[Tuple[()]] = None) -> "ThriftHackRecordContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonValue.7 {{ name = {angle_for(__env, name)}, module = {angle_for(__env, module)} }}", PythonValue

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, module: Optional["ThriftPythonModule"] = None) -> "ThriftPythonValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, structuredAnnotations: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.StructType.7 {{ name = {angle_for(__env, name)}, fields = {angle_for(__env, fields)}, structuredAnnotations = {angle_for(__env, structuredAnnotations)} }}", StructType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, fields: Optional[Tuple[()]] = None, structuredAnnotations: Optional[Tuple[()]] = None) -> "ThriftStructType":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ServiceChild.7 {{ parent = {angle_for(__env, parent)}, child = {angle_for(__env, child)} }}", ServiceChild

  @staticmethod
  def angle_query(*, parent: Optional["ThriftServiceName"] = None, child: Optional["ThriftServiceName"] = None) -> "ThriftServiceChild":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludes(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Includes.7 {{ source = {angle_for(__env, source)}, target = {angle_for(__env, target)} }}", Includes

  @staticmethod
  def angle_query(*, source: Optional["ThriftFile"] = None, target: Optional["ThriftFile"] = None) -> "ThriftIncludes":
    raise Exception("this function can only be called from @angle_query")

class ThriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.QualName.7 {{ file = {angle_for(__env, file)}, name = {angle_for(__env, name)} }}", QualName

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, name: Optional["ThriftIdentifier"] = None) -> "ThriftQualName":
    raise Exception("this function can only be called from @angle_query")

class ThriftOutputTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], compile: ast.Expr, output: ast.Expr, out: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.OutputTarget.7 {{ compile = {angle_for(__env, compile)}, output = {angle_for(__env, output)}, out = {angle_for(__env, out)} }}", OutputTarget

  @staticmethod
  def angle_query(*, compile: Optional["ThriftCompileTarget"] = None, output: Optional["BuckTarget"] = None, out: Optional["SrcFile"] = None) -> "ThriftOutputTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonFunction.7 {{ name = {angle_for(__env, name)}, module = {angle_for(__env, module)} }}", PythonFunction

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, module: Optional["ThriftPythonModule"] = None) -> "ThriftPythonFunction":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructuredAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type_: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.StructuredAnnotation.7 {{ type_ = {angle_for(__env, type_)}, value = {angle_for(__env, value)} }}", StructuredAnnotation

  @staticmethod
  def angle_query(*, type_: Optional["ThriftTypeSpecification"] = None, value: Optional["ThriftStructVal"] = None) -> "ThriftStructuredAnnotation":
    raise Exception("this function can only be called from @angle_query")

class ThriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.File.7 {angle_for(__env, arg)}", File

  @staticmethod
  def angle_query(*, arg: Optional["SrcFile"] = None) -> "ThriftFile":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, record: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackMethod.7 {{ name = {angle_for(__env, name)}, record = {angle_for(__env, record)} }}", HackMethod

  @staticmethod
  def angle_query(*, name: Optional["ThriftHackName"] = None, record: Optional["ThriftHackRecord"] = None) -> "ThriftHackMethod":
    raise Exception("this function can only be called from @angle_query")

class ThriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Constant.7 {{ name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", Constant

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftConstant":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackName.7 {angle_for(__env, arg)}", HackName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftHackName":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.NamedDecl.7 {{ name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", NamedDecl

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "ThriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModuleContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, classes: ast.Expr, functions: ast.Expr, values: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonModuleContains.7 {{ module = {angle_for(__env, module)}, classes = {angle_for(__env, classes)}, functions = {angle_for(__env, functions)}, values = {angle_for(__env, values)} }}", PythonModuleContains

  @staticmethod
  def angle_query(*, module: Optional["ThriftPythonModule"] = None, classes: Optional[Tuple[()]] = None, functions: Optional[Tuple[()]] = None, values: Optional[Tuple[()]] = None) -> "ThriftPythonModuleContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, repoCode: ast.Expr, path: ast.Expr, kind: ast.Expr, mangledsvcs: ast.Expr, rest: ast.Expr, server: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackMap.7 {{ source = {angle_for(__env, source)}, repoCode = {angle_for(__env, repoCode)}, path = {angle_for(__env, path)}, kind = {angle_for(__env, kind)}, mangledsvcs = {angle_for(__env, mangledsvcs)}, rest = {angle_for(__env, rest)}, server = {angle_for(__env, server)} }}", HackMap

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, repoCode: Optional["ScmRepoName"] = None, path: Optional[str] = None, kind: Optional[Tuple[()]] = None, mangledsvcs: Optional[bool] = None, rest: Optional[bool] = None, server: Optional[bool] = None) -> "ThriftHackMap":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeSpecial(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, special: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.IncludeSpecial.7 {{ source = {angle_for(__env, source)}, special = {angle_for(__env, special)}, target = {angle_for(__env, target)} }}", IncludeSpecial

  @staticmethod
  def angle_query(*, source: Optional["ThriftFile"] = None, special: Optional[str] = None, target: Optional["ThriftIncludeSplice"] = None) -> "ThriftIncludeSpecial":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, targets: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileXRefs.7 {{ file = {angle_for(__env, file)}, targets = {angle_for(__env, targets)}, xrefs = {angle_for(__env, xrefs)} }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, targets: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "ThriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class ThriftMangle(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], _from: ast.Expr, to: ast.Expr, lang: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Mangle.7 {{ from = {angle_for(__env, _from)}, to = {angle_for(__env, to)}, lang = {angle_for(__env, lang)} }}", Mangle

  @staticmethod
  def angle_query(*, _from: Optional["ScmCommit"] = None, to: Optional["ScmCommit"] = None, lang: Optional["ThriftMangleLang"] = None) -> "ThriftMangle":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fields: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.StructVal.7 {{ fields = {angle_for(__env, fields)} }}", StructVal

  @staticmethod
  def angle_query(*, fields: Optional[Tuple[()]] = None) -> "ThriftStructVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeSplice(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.IncludeSplice.7 {angle_for(__env, arg)}", IncludeSplice

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftIncludeSplice":
    raise Exception("this function can only be called from @angle_query")

class ThriftLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Lang.7 {angle_for(__env, arg)}", Lang

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftLang":
    raise Exception("this function can only be called from @angle_query")

class ThriftLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Literal.7 {angle_for(__env, arg)}", Literal

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

class ThriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Identifier.7 {angle_for(__env, arg)}", Identifier

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftIdentifier":
    raise Exception("this function can only be called from @angle_query")

class ThriftUnionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, alts: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.UnionType.7 {{ name = {angle_for(__env, name)}, alts = {angle_for(__env, alts)} }}", UnionType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, alts: Optional[Tuple[()]] = None) -> "ThriftUnionType":
    raise Exception("this function can only be called from @angle_query")

class ThriftDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, name: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.DeclarationNameSpan.7 {{ decl = {angle_for(__env, decl)}, name = {angle_for(__env, name)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, name: Optional["ThriftIdentifier"] = None, file: Optional["ThriftFile"] = None, span: Optional[Tuple[()]] = None) -> "ThriftDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], python: ast.Expr, thrift: ast.Expr, role: ast.Expr, lang: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FromPython.7 {{ python = {angle_for(__env, python)}, thrift = {angle_for(__env, thrift)}, role = {angle_for(__env, role)}, lang = {angle_for(__env, lang)} }}", FromPython

  @staticmethod
  def angle_query(*, python: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None, lang: Optional["ThriftLang"] = None) -> "ThriftFromPython":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonClass.7 {{ name = {angle_for(__env, name)}, module = {angle_for(__env, module)} }}", PythonClass

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, module: Optional["ThriftPythonModule"] = None) -> "ThriftPythonClass":
    raise Exception("this function can only be called from @angle_query")

class ThriftToCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], thrift: ast.Expr, cpp2: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ToCpp2.7 {{ thrift = {angle_for(__env, thrift)}, cpp2 = {angle_for(__env, cpp2)} }}", ToCpp2

  @staticmethod
  def angle_query(*, thrift: Optional["ThriftFile"] = None, cpp2: Optional[Tuple[()]] = None) -> "ThriftToCpp2":
    raise Exception("this function can only be called from @angle_query")

class ThriftToHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], thrift: ast.Expr, hack: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ToHack.7 {{ thrift = {angle_for(__env, thrift)}, hack = {angle_for(__env, hack)} }}", ToHack

  @staticmethod
  def angle_query(*, thrift: Optional["ThriftFile"] = None, hack: Optional[Tuple[()]] = None) -> "ThriftToHack":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumerationType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.EnumerationType.7 {{ name = {angle_for(__env, name)}, value = {angle_for(__env, value)} }}", EnumerationType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, value: Optional[Tuple[()]] = None) -> "ThriftEnumerationType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileOutput(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], output: ast.Expr, origin: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileOutput.7 {{ output = {angle_for(__env, output)}, origin = {angle_for(__env, origin)} }}", FileOutput

  @staticmethod
  def angle_query(*, output: Optional["SrcFile"] = None, origin: Optional["ThriftOutputTarget"] = None) -> "ThriftFileOutput":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ServiceParent.7 {{ child = {angle_for(__env, child)}, parent = {angle_for(__env, parent)} }}", ServiceParent

  @staticmethod
  def angle_query(*, child: Optional["ThriftServiceName"] = None, parent: Optional["ThriftServiceName"] = None) -> "ThriftServiceParent":
    raise Exception("this function can only be called from @angle_query")

class ThriftMangleLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.MangleLang.7 {angle_for(__env, arg)}", MangleLang

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftMangleLang":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ExceptionName.7 {{ name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", ExceptionName

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftExceptionName":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.EnumVal.7 {{ name = {angle_for(__env, name)}, loc = {angle_for(__env, loc)} }}", EnumVal

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, loc: Optional[Tuple[()]] = None) -> "ThriftEnumVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftUnionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], field: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.UnionVal.7 {{ field = {angle_for(__env, field)} }}", UnionVal

  @staticmethod
  def angle_query(*, field: Optional[Tuple[()]] = None) -> "ThriftUnionVal":
    raise Exception("this function can only be called from @angle_query")


