# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class ThriftToPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ToPython.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftToPython":
    raise Exception("this function can only be called from @angle_query")

class ThriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FunctionSpecification.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ExceptionType.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftExceptionType":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeDefType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.TypeDefType.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftTypeDefType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FromCpp2.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFromCpp2":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FromHack.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFromHack":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FileDeclaration.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonField.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonField":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileError(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FileError.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFileError":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonModule.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftPythonModule":
    raise Exception("this function can only be called from @angle_query")

class ThriftConstantType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ConstantType.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftConstantType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FunctionName.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFunctionName":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.TypeSpecification.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespaceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.NamespaceName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftNamespaceName":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ExceptionVal.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftExceptionVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.HackRecord.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftHackRecord":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonMethod.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonMethod":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftPythonName":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.Namespace.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftNamespace":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ServiceName.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftServiceName":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.IncludeStatement.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftIncludeStatement":
    raise Exception("this function can only be called from @angle_query")

class ThriftTargetX(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.TargetX.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftTargetX":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonClassContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonClassContains.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonClassContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonFileModule.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonFileModule":
    raise Exception("this function can only be called from @angle_query")

class ThriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.TypeDefException.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.EnumValue.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftEnumValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ServiceDefinition.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamespaceValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.NamespaceValue.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftNamespaceValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftCompileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.CompileTarget.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftCompileTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModuleFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonModuleFile.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonModuleFile":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FileTarget.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFileTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.EnumValueDef.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftEnumValueDef":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackRecordContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.HackRecordContains.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftHackRecordContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonValue.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonValue":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.StructType.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftStructType":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ServiceChild.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftServiceChild":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludes(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.Includes.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftIncludes":
    raise Exception("this function can only be called from @angle_query")

class ThriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.QualName.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftQualName":
    raise Exception("this function can only be called from @angle_query")

class ThriftOutputTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.OutputTarget.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftOutputTarget":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonFunction.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonFunction":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructuredAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.StructuredAnnotation.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftStructuredAnnotation":
    raise Exception("this function can only be called from @angle_query")

class ThriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.File.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftFile":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.HackMethod.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftHackMethod":
    raise Exception("this function can only be called from @angle_query")

class ThriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.Constant.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftConstant":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.HackName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftHackName":
    raise Exception("this function can only be called from @angle_query")

class ThriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.NamedDecl.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonModuleContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonModuleContains.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonModuleContains":
    raise Exception("this function can only be called from @angle_query")

class ThriftHackMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.HackMap.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftHackMap":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeSpecial(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.IncludeSpecial.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftIncludeSpecial":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FileXRefs.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class ThriftMangle(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.Mangle.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftMangle":
    raise Exception("this function can only be called from @angle_query")

class ThriftStructVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.StructVal.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftStructVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftIncludeSplice(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.IncludeSplice.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftIncludeSplice":
    raise Exception("this function can only be called from @angle_query")

class ThriftLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.Lang.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftLang":
    raise Exception("this function can only be called from @angle_query")

class ThriftLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.Literal.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

class ThriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.Identifier.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftIdentifier":
    raise Exception("this function can only be called from @angle_query")

class ThriftUnionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.UnionType.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftUnionType":
    raise Exception("this function can only be called from @angle_query")

class ThriftDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.DeclarationNameSpan.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class ThriftFromPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FromPython.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFromPython":
    raise Exception("this function can only be called from @angle_query")

class ThriftPythonClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.PythonClass.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftPythonClass":
    raise Exception("this function can only be called from @angle_query")

class ThriftToCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ToCpp2.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftToCpp2":
    raise Exception("this function can only be called from @angle_query")

class ThriftToHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ToHack.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftToHack":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumerationType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.EnumerationType.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftEnumerationType":
    raise Exception("this function can only be called from @angle_query")

class ThriftFileOutput(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.FileOutput.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftFileOutput":
    raise Exception("this function can only be called from @angle_query")

class ThriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ServiceParent.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftServiceParent":
    raise Exception("this function can only be called from @angle_query")

class ThriftMangleLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.MangleLang.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ThriftMangleLang":
    raise Exception("this function can only be called from @angle_query")

class ThriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.ExceptionName.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftExceptionName":
    raise Exception("this function can only be called from @angle_query")

class ThriftEnumVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.EnumVal.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftEnumVal":
    raise Exception("this function can only be called from @angle_query")

class ThriftUnionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"thrift.UnionVal.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ThriftUnionVal":
    raise Exception("this function can only be called from @angle_query")


