# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just
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
    return f"thrift.ToPython.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, thrift, 'thrift'), angle_for(__env, lang, 'lang'), angle_for(__env, python, 'python')])) or '_' } }}", ToPython

  @staticmethod
  def angle_query(*, thrift: Optional["ThriftFile"] = None, lang: Optional["ThriftLang"] = None, python: Optional[List["ThriftFromPython"]] = None) -> "ThriftToPython":
    raise Exception("this function can only be called from @angle_query")



class ThriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, result: ast.Expr, arguments: ast.Expr, throws_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FunctionSpecification.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, result, 'result'), angle_for(__env, arguments, 'arguments'), angle_for(__env, throws_, 'throws_')])) or '_' } }}", FunctionSpecification

  @staticmethod
  def angle_query(*, name: Optional["ThriftFunctionName"] = None, result: Optional[Tuple[()]] = None, arguments: Optional[List[Tuple[()]]] = None, throws_: Optional[List[Tuple[()]]] = None) -> "ThriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")



class ThriftExceptionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, structuredAnnotations: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ExceptionType.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, fields, 'fields'), angle_for(__env, structuredAnnotations, 'structuredAnnotations')])) or '_' } }}", ExceptionType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, fields: Optional[List[Tuple[()]]] = None, structuredAnnotations: Optional[List["ThriftStructuredAnnotation"]] = None) -> "ThriftExceptionType":
    raise Exception("this function can only be called from @angle_query")



class ThriftTypeDefType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], alias: ast.Expr, type_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.TypeDefType.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, alias, 'alias'), angle_for(__env, type_, 'type_')])) or '_' } }}", TypeDefType

  @staticmethod
  def angle_query(*, alias: Optional["ThriftQualName"] = None, type_: Optional["ThriftTypeSpecification"] = None) -> "ThriftTypeDefType":
    raise Exception("this function can only be called from @angle_query")



class ThriftFromCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cpp2: ast.Expr, thrift: ast.Expr, role: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FromCpp2.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, cpp2, 'cpp2'), angle_for(__env, thrift, 'thrift'), angle_for(__env, role, 'role')])) or '_' } }}", FromCpp2

  @staticmethod
  def angle_query(*, cpp2: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None) -> "ThriftFromCpp2":
    raise Exception("this function can only be called from @angle_query")



class ThriftFromHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], hack: ast.Expr, thrift: ast.Expr, role: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FromHack.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, hack, 'hack'), angle_for(__env, thrift, 'thrift'), angle_for(__env, role, 'role')])) or '_' } }}", FromHack

  @staticmethod
  def angle_query(*, hack: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None) -> "ThriftFromHack":
    raise Exception("this function can only be called from @angle_query")



class ThriftFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileDeclaration.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, decl, 'decl')])) or '_' } }}", FileDeclaration

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, decl: Optional[Tuple[()]] = None) -> "ThriftFileDeclaration":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, class_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonField.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, class_, 'class_')])) or '_' } }}", PythonField

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, class_: Optional["ThriftPythonClass"] = None) -> "ThriftPythonField":
    raise Exception("this function can only be called from @angle_query")



class ThriftFileError(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, error: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileError.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, error, 'error')])) or '_' } }}", FileError

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, error: Optional[str] = None) -> "ThriftFileError":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonModule.7 { angle_for(__env, arg, None) or '_' }", PythonModule

  @staticmethod
  def angle_query(*, arg: Optional["ThriftPythonName"] = None) -> "ThriftPythonModule":
    raise Exception("this function can only be called from @angle_query")



class ThriftConstantType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ConstantType.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type_, 'type_')])) or '_' } }}", ConstantType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, type_: Optional["ThriftTypeSpecification"] = None) -> "ThriftConstantType":
    raise Exception("this function can only be called from @angle_query")



class ThriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], service_: ast.Expr, name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FunctionName.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, service_, 'service_'), angle_for(__env, name, 'name'), angle_for(__env, locName, 'locName')])) or '_' } }}", FunctionName

  @staticmethod
  def angle_query(*, service_: Optional["ThriftServiceName"] = None, name: Optional["ThriftIdentifier"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftFunctionName":
    raise Exception("this function can only be called from @angle_query")



class ThriftTypeSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], primitive: ast.Expr, container: ast.Expr, named: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.TypeSpecification.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, primitive, 'primitive'), angle_for(__env, container, 'container'), angle_for(__env, named, 'named')])) or '_' } }}", TypeSpecification

  @staticmethod
  def angle_query_primitive(*, primitive: Tuple[()]) -> "ThriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_container(*, container: Tuple[()]) -> "ThriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_named(*, named: Tuple[()]) -> "ThriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")




class ThriftNamespaceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.NamespaceName.7 { angle_for(__env, arg, None) or '_' }", NamespaceName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftNamespaceName":
    raise Exception("this function can only be called from @angle_query")



class ThriftExceptionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ExceptionVal.7 { angle_for(__env, arg, None) or '_' }", ExceptionVal

  @staticmethod
  def angle_query(*, arg: Optional["ThriftStructVal"] = None) -> "ThriftExceptionVal":
    raise Exception("this function can only be called from @angle_query")



class ThriftHackRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackRecord.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, kind, 'kind')])) or '_' } }}", HackRecord

  @staticmethod
  def angle_query(*, name: Optional["ThriftHackName"] = None, kind: Optional[Tuple[()]] = None) -> "ThriftHackRecord":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, class_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonMethod.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, class_, 'class_')])) or '_' } }}", PythonMethod

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, class_: Optional["ThriftPythonClass"] = None) -> "ThriftPythonMethod":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonName.7 { angle_for(__env, arg, None) or '_' }", PythonName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftPythonName":
    raise Exception("this function can only be called from @angle_query")



class ThriftNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, name: ast.Expr, namespace_: ast.Expr, quoted: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Namespace.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, name, 'name'), angle_for(__env, namespace_, 'namespace_'), angle_for(__env, quoted, 'quoted')])) or '_' } }}", Namespace

  @staticmethod
  def angle_query(*, target: Optional["ThriftFile"] = None, name: Optional["ThriftNamespaceName"] = None, namespace_: Optional["ThriftNamespaceValue"] = None, quoted: Optional[bool] = None) -> "ThriftNamespace":
    raise Exception("this function can only be called from @angle_query")



class ThriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ServiceName.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, locName, 'locName')])) or '_' } }}", ServiceName

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftServiceName":
    raise Exception("this function can only be called from @angle_query")



class ThriftIncludeStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr, locSource: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.IncludeStatement.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source'), angle_for(__env, locSource, 'locSource')])) or '_' } }}", IncludeStatement

  @staticmethod
  def angle_query(*, target: Optional["ThriftFile"] = None, source: Optional["ThriftFile"] = None, locSource: Optional[Tuple[()]] = None) -> "ThriftIncludeStatement":
    raise Exception("this function can only be called from @angle_query")



class ThriftTargetX(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, fileRef: ast.Expr, locRef: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.TargetX.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, fileRef, 'fileRef'), angle_for(__env, locRef, 'locRef')])) or '_' } }}", TargetX

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, fileRef: Optional["ThriftFile"] = None, locRef: Optional[Tuple[()]] = None) -> "ThriftTargetX":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonClassContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr, methods: ast.Expr, fields: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonClassContains.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_'), angle_for(__env, methods, 'methods'), angle_for(__env, fields, 'fields')])) or '_' } }}", PythonClassContains

  @staticmethod
  def angle_query(*, class_: Optional["ThriftPythonClass"] = None, methods: Optional[List["ThriftPythonMethod"]] = None, fields: Optional[List["ThriftPythonField"]] = None) -> "ThriftPythonClassContains":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonFileModule.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, module, 'module')])) or '_' } }}", PythonFileModule

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, module: Optional["ThriftPythonModule"] = None) -> "ThriftPythonFileModule":
    raise Exception("this function can only be called from @angle_query")



class ThriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], alias: ast.Expr, type_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.TypeDefException.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, alias, 'alias'), angle_for(__env, type_, 'type_')])) or '_' } }}", TypeDefException

  @staticmethod
  def angle_query(*, alias: Optional["ThriftNamedDecl"] = None, type_: Optional[Tuple[()]] = None) -> "ThriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")



class ThriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], enum_: ast.Expr, name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.EnumValue.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, enum_, 'enum_'), angle_for(__env, name, 'name'), angle_for(__env, locName, 'locName')])) or '_' } }}", EnumValue

  @staticmethod
  def angle_query(*, enum_: Optional[Tuple[()]] = None, name: Optional["ThriftIdentifier"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftEnumValue":
    raise Exception("this function can only be called from @angle_query")



class ThriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, functions: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ServiceDefinition.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, functions, 'functions')])) or '_' } }}", ServiceDefinition

  @staticmethod
  def angle_query(*, name: Optional["ThriftServiceName"] = None, functions: Optional[List["ThriftFunctionSpecification"]] = None) -> "ThriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")



class ThriftNamespaceValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.NamespaceValue.7 { angle_for(__env, arg, None) or '_' }", NamespaceValue

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftNamespaceValue":
    raise Exception("this function can only be called from @angle_query")



class ThriftCompileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], includes: ast.Expr, lang: ast.Expr, compile: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.CompileTarget.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, includes, 'includes'), angle_for(__env, lang, 'lang'), angle_for(__env, compile, 'compile')])) or '_' } }}", CompileTarget

  @staticmethod
  def angle_query(*, includes: Optional["ThriftFileTarget"] = None, lang: Optional[Tuple[()]] = None, compile: Optional["BuckTarget"] = None) -> "ThriftCompileTarget":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonModuleFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonModuleFile.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, module, 'module'), angle_for(__env, file, 'file')])) or '_' } }}", PythonModuleFile

  @staticmethod
  def angle_query(*, module: Optional["ThriftPythonModule"] = None, file: Optional["SrcFile"] = None) -> "ThriftPythonModuleFile":
    raise Exception("this function can only be called from @angle_query")



class ThriftFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileTarget.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, target, 'target')])) or '_' } }}", FileTarget

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, target: Optional["BuckTarget"] = None) -> "ThriftFileTarget":
    raise Exception("this function can only be called from @angle_query")



class ThriftEnumValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.EnumValueDef.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, value, 'value')])) or '_' } }}", EnumValueDef

  @staticmethod
  def angle_query(*, name: Optional["ThriftEnumValue"] = None, value: Optional[Tuple[()]] = None) -> "ThriftEnumValueDef":
    raise Exception("this function can only be called from @angle_query")



class ThriftHackRecordContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], record: ast.Expr, methods: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackRecordContains.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, record, 'record'), angle_for(__env, methods, 'methods')])) or '_' } }}", HackRecordContains

  @staticmethod
  def angle_query(*, record: Optional["ThriftHackRecord"] = None, methods: Optional[List["ThriftHackMethod"]] = None) -> "ThriftHackRecordContains":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonValue.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, module, 'module')])) or '_' } }}", PythonValue

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, module: Optional["ThriftPythonModule"] = None) -> "ThriftPythonValue":
    raise Exception("this function can only be called from @angle_query")



class ThriftStructType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, structuredAnnotations: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.StructType.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, fields, 'fields'), angle_for(__env, structuredAnnotations, 'structuredAnnotations')])) or '_' } }}", StructType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, fields: Optional[List[Tuple[()]]] = None, structuredAnnotations: Optional[List["ThriftStructuredAnnotation"]] = None) -> "ThriftStructType":
    raise Exception("this function can only be called from @angle_query")



class ThriftServiceChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ServiceChild.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')])) or '_' } }}", ServiceChild

  @staticmethod
  def angle_query(*, parent: Optional["ThriftServiceName"] = None, child: Optional["ThriftServiceName"] = None) -> "ThriftServiceChild":
    raise Exception("this function can only be called from @angle_query")



class ThriftIncludes(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Includes.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, target, 'target')])) or '_' } }}", Includes

  @staticmethod
  def angle_query(*, source: Optional["ThriftFile"] = None, target: Optional["ThriftFile"] = None) -> "ThriftIncludes":
    raise Exception("this function can only be called from @angle_query")



class ThriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.QualName.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, name, 'name')])) or '_' } }}", QualName

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, name: Optional["ThriftIdentifier"] = None) -> "ThriftQualName":
    raise Exception("this function can only be called from @angle_query")



class ThriftOutputTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], compile: ast.Expr, output: ast.Expr, out: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.OutputTarget.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, compile, 'compile'), angle_for(__env, output, 'output'), angle_for(__env, out, 'out')])) or '_' } }}", OutputTarget

  @staticmethod
  def angle_query(*, compile: Optional["ThriftCompileTarget"] = None, output: Optional["BuckTarget"] = None, out: Optional["SrcFile"] = None) -> "ThriftOutputTarget":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonFunction.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, module, 'module')])) or '_' } }}", PythonFunction

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, module: Optional["ThriftPythonModule"] = None) -> "ThriftPythonFunction":
    raise Exception("this function can only be called from @angle_query")



class ThriftStructuredAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type_: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.StructuredAnnotation.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, type_, 'type_'), angle_for(__env, value, 'value')])) or '_' } }}", StructuredAnnotation

  @staticmethod
  def angle_query(*, type_: Optional["ThriftTypeSpecification"] = None, value: Optional["ThriftStructVal"] = None) -> "ThriftStructuredAnnotation":
    raise Exception("this function can only be called from @angle_query")



class ThriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.File.7 { angle_for(__env, arg, None) or '_' }", File

  @staticmethod
  def angle_query(*, arg: Optional["SrcFile"] = None) -> "ThriftFile":
    raise Exception("this function can only be called from @angle_query")



class ThriftHackMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, record: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackMethod.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, record, 'record')])) or '_' } }}", HackMethod

  @staticmethod
  def angle_query(*, name: Optional["ThriftHackName"] = None, record: Optional["ThriftHackRecord"] = None) -> "ThriftHackMethod":
    raise Exception("this function can only be called from @angle_query")



class ThriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Constant.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, locName, 'locName')])) or '_' } }}", Constant

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftConstant":
    raise Exception("this function can only be called from @angle_query")



class ThriftHackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackName.7 { angle_for(__env, arg, None) or '_' }", HackName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftHackName":
    raise Exception("this function can only be called from @angle_query")



class ThriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.NamedDecl.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, locName, 'locName')])) or '_' } }}", NamedDecl

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "ThriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonModuleContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, classes: ast.Expr, functions: ast.Expr, values: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonModuleContains.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, module, 'module'), angle_for(__env, classes, 'classes'), angle_for(__env, functions, 'functions'), angle_for(__env, values, 'values')])) or '_' } }}", PythonModuleContains

  @staticmethod
  def angle_query(*, module: Optional["ThriftPythonModule"] = None, classes: Optional[List["ThriftPythonClass"]] = None, functions: Optional[List["ThriftPythonFunction"]] = None, values: Optional[List["ThriftPythonValue"]] = None) -> "ThriftPythonModuleContains":
    raise Exception("this function can only be called from @angle_query")



class ThriftHackMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, repoCode: ast.Expr, path: ast.Expr, kind: ast.Expr, mangledsvcs: ast.Expr, rest: ast.Expr, server: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.HackMap.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, repoCode, 'repoCode'), angle_for(__env, path, 'path'), angle_for(__env, kind, 'kind'), angle_for(__env, mangledsvcs, 'mangledsvcs'), angle_for(__env, rest, 'rest'), angle_for(__env, server, 'server')])) or '_' } }}", HackMap

  @staticmethod
  def angle_query(*, source: Optional[Union[Just["SrcFile"], Just[None]]] = None, repoCode: Optional["ScmRepoName"] = None, path: Optional[str] = None, kind: Optional[Tuple[()]] = None, mangledsvcs: Optional[bool] = None, rest: Optional[bool] = None, server: Optional[bool] = None) -> "ThriftHackMap":
    raise Exception("this function can only be called from @angle_query")



class ThriftIncludeSpecial(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, special: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.IncludeSpecial.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, special, 'special'), angle_for(__env, target, 'target')])) or '_' } }}", IncludeSpecial

  @staticmethod
  def angle_query(*, source: Optional["ThriftFile"] = None, special: Optional[str] = None, target: Optional["ThriftIncludeSplice"] = None) -> "ThriftIncludeSpecial":
    raise Exception("this function can only be called from @angle_query")



class ThriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, targets: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileXRefs.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, targets, 'targets'), angle_for(__env, xrefs, 'xrefs')])) or '_' } }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["ThriftFile"] = None, targets: Optional[List[Tuple[()]]] = None, xrefs: Optional[List[Tuple[()]]] = None) -> "ThriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")



class ThriftMangle(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], _from: ast.Expr, to: ast.Expr, lang: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Mangle.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, _from, 'from'), angle_for(__env, to, 'to'), angle_for(__env, lang, 'lang')])) or '_' } }}", Mangle

  @staticmethod
  def angle_query(*, _from: Optional["ScmCommit"] = None, to: Optional["ScmCommit"] = None, lang: Optional["ThriftMangleLang"] = None) -> "ThriftMangle":
    raise Exception("this function can only be called from @angle_query")



class ThriftStructVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fields: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.StructVal.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fields, 'fields')])) or '_' } }}", StructVal

  @staticmethod
  def angle_query(*, fields: Optional[List[Tuple[()]]] = None) -> "ThriftStructVal":
    raise Exception("this function can only be called from @angle_query")



class ThriftIncludeSplice(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.IncludeSplice.7 { angle_for(__env, arg, None) or '_' }", IncludeSplice

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftIncludeSplice":
    raise Exception("this function can only be called from @angle_query")



class ThriftLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Lang.7 { angle_for(__env, arg, None) or '_' }", Lang

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftLang":
    raise Exception("this function can only be called from @angle_query")



class ThriftLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], byte_: ast.Expr, i16_: ast.Expr, i32_: ast.Expr, i64_: ast.Expr, float_: ast.Expr, double_: ast.Expr, bool_: ast.Expr, string_: ast.Expr, binary_: ast.Expr, set_: ast.Expr, hashSet_: ast.Expr, list_: ast.Expr, map_: ast.Expr, hashMap_: ast.Expr, newtype_: ast.Expr, struct_: ast.Expr, exception_: ast.Expr, union_: ast.Expr, enum_: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Literal.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, byte_, 'byte_'), angle_for(__env, i16_, 'i16_'), angle_for(__env, i32_, 'i32_'), angle_for(__env, i64_, 'i64_'), angle_for(__env, float_, 'float_'), angle_for(__env, double_, 'double_'), angle_for(__env, bool_, 'bool_'), angle_for(__env, string_, 'string_'), angle_for(__env, binary_, 'binary_'), angle_for(__env, set_, 'set_'), angle_for(__env, hashSet_, 'hashSet_'), angle_for(__env, list_, 'list_'), angle_for(__env, map_, 'map_'), angle_for(__env, hashMap_, 'hashMap_'), angle_for(__env, newtype_, 'newtype_'), angle_for(__env, struct_, 'struct_'), angle_for(__env, exception_, 'exception_'), angle_for(__env, union_, 'union_'), angle_for(__env, enum_, 'enum_')])) or '_' } }}", Literal

  @staticmethod
  def angle_query_byte_(*, byte_: Tuple[()]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_i16_(*, i16_: Tuple[()]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_i32_(*, i32_: Tuple[()]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_i64_(*, i64_: Tuple[()]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_float_(*, float_: Tuple[()]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_double_(*, double_: Tuple[()]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_bool_(*, bool_: bool) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_string_(*, string_: str) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_binary_(*, binary_: bytes) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_set_(*, set_: List[Tuple[()]]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hashSet_(*, hashSet_: List[Tuple[()]]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_list_(*, list_: List[Tuple[()]]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_map_(*, map_: List[Tuple[()]]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hashMap_(*, hashMap_: List[Tuple[()]]) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_newtype_(*, newtype_: "ThriftLiteral") -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_struct_(*, struct_: "ThriftStructVal") -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_exception_(*, exception_: "ThriftExceptionVal") -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_union_(*, union_: "ThriftUnionVal") -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enum_(*, enum_: "ThriftEnumVal") -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")




class ThriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.Identifier.7 { angle_for(__env, arg, None) or '_' }", Identifier

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftIdentifier":
    raise Exception("this function can only be called from @angle_query")



class ThriftUnionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, alts: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.UnionType.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, alts, 'alts')])) or '_' } }}", UnionType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, alts: Optional[List[Tuple[()]]] = None) -> "ThriftUnionType":
    raise Exception("this function can only be called from @angle_query")



class ThriftDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, name: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.DeclarationNameSpan.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, name, 'name'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, name: Optional["ThriftIdentifier"] = None, file: Optional["ThriftFile"] = None, span: Optional[Tuple[()]] = None) -> "ThriftDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")



class ThriftFromPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], python: ast.Expr, thrift: ast.Expr, role: ast.Expr, lang: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FromPython.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, python, 'python'), angle_for(__env, thrift, 'thrift'), angle_for(__env, role, 'role'), angle_for(__env, lang, 'lang')])) or '_' } }}", FromPython

  @staticmethod
  def angle_query(*, python: Optional[Tuple[()]] = None, thrift: Optional[Tuple[()]] = None, role: Optional[Tuple[()]] = None, lang: Optional["ThriftLang"] = None) -> "ThriftFromPython":
    raise Exception("this function can only be called from @angle_query")



class ThriftPythonClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.PythonClass.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, module, 'module')])) or '_' } }}", PythonClass

  @staticmethod
  def angle_query(*, name: Optional["ThriftPythonName"] = None, module: Optional["ThriftPythonModule"] = None) -> "ThriftPythonClass":
    raise Exception("this function can only be called from @angle_query")



class ThriftToCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], thrift: ast.Expr, cpp2: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ToCpp2.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, thrift, 'thrift'), angle_for(__env, cpp2, 'cpp2')])) or '_' } }}", ToCpp2

  @staticmethod
  def angle_query(*, thrift: Optional["ThriftFile"] = None, cpp2: Optional[List["ThriftFromCpp2"]] = None) -> "ThriftToCpp2":
    raise Exception("this function can only be called from @angle_query")



class ThriftToHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], thrift: ast.Expr, hack: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ToHack.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, thrift, 'thrift'), angle_for(__env, hack, 'hack')])) or '_' } }}", ToHack

  @staticmethod
  def angle_query(*, thrift: Optional["ThriftFile"] = None, hack: Optional[List["ThriftFromHack"]] = None) -> "ThriftToHack":
    raise Exception("this function can only be called from @angle_query")



class ThriftEnumerationType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.EnumerationType.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, value, 'value')])) or '_' } }}", EnumerationType

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, value: Optional[List["ThriftEnumValueDef"]] = None) -> "ThriftEnumerationType":
    raise Exception("this function can only be called from @angle_query")



class ThriftFileOutput(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], output: ast.Expr, origin: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.FileOutput.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, output, 'output'), angle_for(__env, origin, 'origin')])) or '_' } }}", FileOutput

  @staticmethod
  def angle_query(*, output: Optional["SrcFile"] = None, origin: Optional["ThriftOutputTarget"] = None) -> "ThriftFileOutput":
    raise Exception("this function can only be called from @angle_query")



class ThriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ServiceParent.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, child, 'child'), angle_for(__env, parent, 'parent')])) or '_' } }}", ServiceParent

  @staticmethod
  def angle_query(*, child: Optional["ThriftServiceName"] = None, parent: Optional["ThriftServiceName"] = None) -> "ThriftServiceParent":
    raise Exception("this function can only be called from @angle_query")



class ThriftMangleLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.MangleLang.7 { angle_for(__env, arg, None) or '_' }", MangleLang

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ThriftMangleLang":
    raise Exception("this function can only be called from @angle_query")



class ThriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.ExceptionName.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, locName, 'locName')])) or '_' } }}", ExceptionName

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "ThriftExceptionName":
    raise Exception("this function can only be called from @angle_query")



class ThriftEnumVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.EnumVal.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, loc, 'loc')])) or '_' } }}", EnumVal

  @staticmethod
  def angle_query(*, name: Optional["ThriftQualName"] = None, loc: Optional[Tuple[()]] = None) -> "ThriftEnumVal":
    raise Exception("this function can only be called from @angle_query")



class ThriftUnionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], field: ast.Expr) -> Tuple[str, Struct]:
    return f"thrift.UnionVal.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, field, 'field')])) or '_' } }}", UnionVal

  @staticmethod
  def angle_query(*, field: Optional[Tuple[()]] = None) -> "ThriftUnionVal":
    raise Exception("this function can only be called from @angle_query")




