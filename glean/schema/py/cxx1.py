# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCxx1DeclInObjcContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclInObjcContainer.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclInObjcContainer":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1XRefIndirectTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.XRefIndirectTarget.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1XRefIndirectTarget":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclByName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclByName":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationTargets(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationTargets.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationTargets":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationLocationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationLocationName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationLocationName":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1EnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.EnumDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1EnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FileXRefMap.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FileXRefMap":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcContainerDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcContainerDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcMethodDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1MethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.MethodOverrides.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1MethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FunctionDeclAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FunctionDeclAttribute.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FunctionDeclAttribute":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcInterfaceToImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcInterfaceToImplementation.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcInterfaceToImplementation":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1UsingDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.UsingDirective.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1UsingDirective":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1Trace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.Trace.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1Trace":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcPropertyImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcPropertyImplementation.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcPropertyImplementation":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1MethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.MethodOverridden.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1MethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1PPTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.PPTrace.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1PPTrace":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclFamilyOf(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclFamilyOf.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclFamilyOf":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1TargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.TargetUses.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1TargetUses":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1NamespaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.NamespaceDefinition.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1NamespaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FunctionAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FunctionAttribute.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FunctionAttribute":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FilePPUseTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FilePPUseTraceXRefs.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FilePPUseTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1TypeAliasDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.TypeAliasDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1TypeAliasDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DefToBaseDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DefToBaseDecl.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DefToBaseDecl":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1Name(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.Name.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1Name":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FilePPUseXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FilePPUseXRefs.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FilePPUseXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1Enumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.Enumerator.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1Enumerator":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FunctionQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FunctionQName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FunctionQName":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1TranslationUnitTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.TranslationUnitTrace.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1TranslationUnitTrace":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1RecordDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.RecordDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1RecordDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FilePPTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FilePPTraceXRefs.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FilePPTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1NamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.NamespaceQName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1NamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationSrcRange.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationSrcRange":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationLocation.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjContainerIdName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjContainerIdName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjContainerIdName":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FunctionDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1Same(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.Same.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1Same":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1NamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.NamespaceDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1NamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcPropertyDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationComment.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationNameSpan.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1Declarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.Declarations.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1Declarations":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclToFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclToFamily.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclToFamily":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclFamily.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclFamily":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FunctionDeclarationNameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FunctionDeclarationNameString.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FunctionDeclarationNameString":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1EnumeratorInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.EnumeratorInEnum.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1EnumeratorInEnum":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1RecordDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.RecordDefinition.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1RecordDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1Type(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.Type.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1Type":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1RecordDerived(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.RecordDerived.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1RecordDerived":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1PPDefineLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.PPDefineLocation.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1PPDefineLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcImplements(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcImplements.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcImplements":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcSelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcSelector.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcSelector":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcMethodDefinition.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationInTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationInTrace.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationInTrace":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1PPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.PPEntityLocation.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1PPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1UsingDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.UsingDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1UsingDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1QName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.QName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1QName":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcContainerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcContainerDefinition.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcContainerDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1EnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.EnumDefinition.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1EnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1VariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.VariableDeclaration.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1VariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationLocationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationLocationNameSpan.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationLocationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcContainerBase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcContainerBase.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcContainerBase":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclarationSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclarationSources.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclarationSources":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1TranslationUnitXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.TranslationUnitXRefs.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1TranslationUnitXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcPropertyIVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcPropertyIVar.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcPropertyIVar":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1ObjcContainerInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.ObjcContainerInheritance.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1ObjcContainerInheritance":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FunctionDefinition.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1Signature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.Signature.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1Signature":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1DeclInRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.DeclInRecord.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1DeclInRecord":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FileXRefs.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FileXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1Attribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.Attribute.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1Attribute":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1NamespaceDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.NamespaceDeclarationName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1NamespaceDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FunctionDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FunctionDeclarationName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FunctionDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GSCxx1FunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"cxx1.FunctionName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")


