# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSThriftToPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ToPython.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftToPython":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FunctionSpecification.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")

class GSThriftExceptionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ExceptionType.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftExceptionType":
    raise Exception("this function can only be called from @angle_query")

class GSThriftTypeDefType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.TypeDefType.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftTypeDefType":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFromCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FromCpp2.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFromCpp2":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFromHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FromHack.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFromHack":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FileDeclaration.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonField.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonField":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFileError(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FileError.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFileError":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonModule.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonModule":
    raise Exception("this function can only be called from @angle_query")

class GSThriftConstantType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ConstantType.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftConstantType":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FunctionName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFunctionName":
    raise Exception("this function can only be called from @angle_query")

class GSThriftTypeSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.TypeSpecification.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

class GSThriftNamespaceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.NamespaceName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftNamespaceName":
    raise Exception("this function can only be called from @angle_query")

class GSThriftExceptionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ExceptionVal.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftExceptionVal":
    raise Exception("this function can only be called from @angle_query")

class GSThriftHackRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.HackRecord.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftHackRecord":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonMethod.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonMethod":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonName":
    raise Exception("this function can only be called from @angle_query")

class GSThriftNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.Namespace.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftNamespace":
    raise Exception("this function can only be called from @angle_query")

class GSThriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ServiceName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftServiceName":
    raise Exception("this function can only be called from @angle_query")

class GSThriftIncludeStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.IncludeStatement.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftIncludeStatement":
    raise Exception("this function can only be called from @angle_query")

class GSThriftTargetX(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.TargetX.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftTargetX":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonClassContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonClassContains.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonClassContains":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonFileModule.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonFileModule":
    raise Exception("this function can only be called from @angle_query")

class GSThriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.TypeDefException.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")

class GSThriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.EnumValue.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftEnumValue":
    raise Exception("this function can only be called from @angle_query")

class GSThriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ServiceDefinition.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSThriftNamespaceValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.NamespaceValue.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftNamespaceValue":
    raise Exception("this function can only be called from @angle_query")

class GSThriftCompileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.CompileTarget.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftCompileTarget":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonModuleFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonModuleFile.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonModuleFile":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FileTarget.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFileTarget":
    raise Exception("this function can only be called from @angle_query")

class GSThriftEnumValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.EnumValueDef.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftEnumValueDef":
    raise Exception("this function can only be called from @angle_query")

class GSThriftHackRecordContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.HackRecordContains.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftHackRecordContains":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonValue.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonValue":
    raise Exception("this function can only be called from @angle_query")

class GSThriftStructType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.StructType.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftStructType":
    raise Exception("this function can only be called from @angle_query")

class GSThriftServiceChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ServiceChild.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftServiceChild":
    raise Exception("this function can only be called from @angle_query")

class GSThriftIncludes(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.Includes.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftIncludes":
    raise Exception("this function can only be called from @angle_query")

class GSThriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.QualName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftQualName":
    raise Exception("this function can only be called from @angle_query")

class GSThriftOutputTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.OutputTarget.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftOutputTarget":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonFunction.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonFunction":
    raise Exception("this function can only be called from @angle_query")

class GSThriftStructuredAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.StructuredAnnotation.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftStructuredAnnotation":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.File.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFile":
    raise Exception("this function can only be called from @angle_query")

class GSThriftHackMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.HackMethod.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftHackMethod":
    raise Exception("this function can only be called from @angle_query")

class GSThriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.Constant.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftConstant":
    raise Exception("this function can only be called from @angle_query")

class GSThriftHackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.HackName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftHackName":
    raise Exception("this function can only be called from @angle_query")

class GSThriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.NamedDecl.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonModuleContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonModuleContains.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonModuleContains":
    raise Exception("this function can only be called from @angle_query")

class GSThriftHackMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.HackMap.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftHackMap":
    raise Exception("this function can only be called from @angle_query")

class GSThriftIncludeSpecial(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.IncludeSpecial.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftIncludeSpecial":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FileXRefs.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSThriftMangle(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.Mangle.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftMangle":
    raise Exception("this function can only be called from @angle_query")

class GSThriftStructVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.StructVal.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftStructVal":
    raise Exception("this function can only be called from @angle_query")

class GSThriftIncludeSplice(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.IncludeSplice.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftIncludeSplice":
    raise Exception("this function can only be called from @angle_query")

class GSThriftLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.Lang.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftLang":
    raise Exception("this function can only be called from @angle_query")

class GSThriftLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.Literal.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftLiteral":
    raise Exception("this function can only be called from @angle_query")

class GSThriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.Identifier.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftIdentifier":
    raise Exception("this function can only be called from @angle_query")

class GSThriftUnionType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.UnionType.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftUnionType":
    raise Exception("this function can only be called from @angle_query")

class GSThriftDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.DeclarationNameSpan.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFromPython(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FromPython.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFromPython":
    raise Exception("this function can only be called from @angle_query")

class GSThriftPythonClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.PythonClass.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftPythonClass":
    raise Exception("this function can only be called from @angle_query")

class GSThriftToCpp2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ToCpp2.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftToCpp2":
    raise Exception("this function can only be called from @angle_query")

class GSThriftToHack(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ToHack.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftToHack":
    raise Exception("this function can only be called from @angle_query")

class GSThriftEnumerationType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.EnumerationType.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftEnumerationType":
    raise Exception("this function can only be called from @angle_query")

class GSThriftFileOutput(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.FileOutput.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftFileOutput":
    raise Exception("this function can only be called from @angle_query")

class GSThriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ServiceParent.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftServiceParent":
    raise Exception("this function can only be called from @angle_query")

class GSThriftMangleLang(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.MangleLang.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftMangleLang":
    raise Exception("this function can only be called from @angle_query")

class GSThriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.ExceptionName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftExceptionName":
    raise Exception("this function can only be called from @angle_query")

class GSThriftEnumVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.EnumVal.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftEnumVal":
    raise Exception("this function can only be called from @angle_query")

class GSThriftUnionVal(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"thrift.UnionVal.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSThriftUnionVal":
    raise Exception("this function can only be called from @angle_query")


