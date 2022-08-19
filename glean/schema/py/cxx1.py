# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.cxx1.types import (
    DeclInObjcContainer,
    XRefIndirectTarget,
    DeclByName,
    DeclarationTargets,
    DeclarationLocationName,
    EnumDeclaration,
    FileXRefMap,
    ObjcContainerDeclaration,
    ObjcMethodDeclaration,
    MethodOverrides,
    FunctionDeclAttribute,
    ObjcInterfaceToImplementation,
    UsingDirective,
    Trace,
    ObjcPropertyImplementation,
    MethodOverridden,
    PPTrace,
    DeclFamilyOf,
    TargetUses,
    NamespaceDefinition,
    FunctionAttribute,
    FilePPUseTraceXRefs,
    TypeAliasDeclaration,
    DefToBaseDecl,
    Name,
    FilePPUseXRefs,
    Enumerator,
    FunctionQName,
    TranslationUnitTrace,
    RecordDeclaration,
    FilePPTraceXRefs,
    NamespaceQName,
    DeclarationSrcRange,
    DeclarationLocation,
    ObjContainerIdName,
    FunctionDeclaration,
    Same,
    NamespaceDeclaration,
    ObjcPropertyDeclaration,
    DeclarationComment,
    DeclarationNameSpan,
    Declarations,
    DeclToFamily,
    DeclFamily,
    FunctionDeclarationNameString,
    EnumeratorInEnum,
    RecordDefinition,
    Type,
    RecordDerived,
    PPDefineLocation,
    ObjcImplements,
    ObjcSelector,
    ObjcMethodDefinition,
    DeclarationInTrace,
    PPEntityLocation,
    UsingDeclaration,
    QName,
    ObjcContainerDefinition,
    EnumDefinition,
    VariableDeclaration,
    DeclarationLocationNameSpan,
    ObjcContainerBase,
    DeclarationSources,
    TranslationUnitXRefs,
    ObjcPropertyIVar,
    ObjcContainerInheritance,
    FunctionDefinition,
    Signature,
    DeclInRecord,
    FileXRefs,
    Attribute,
    NamespaceDeclarationName,
    FunctionDeclarationName,
    FunctionName,
)


class Cxx1DeclInObjcContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclInObjcContainer.5 { { } }", DeclInObjcContainer

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclInObjcContainer":
    raise Exception("this function can only be called from @angle_query")

class Cxx1XRefIndirectTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.XRefIndirectTarget.5 { { } }", XRefIndirectTarget

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1XRefIndirectTarget":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclByName.5 { { } }", DeclByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclByName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationTargets(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationTargets.5 { { } }", DeclarationTargets

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationTargets":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocationName.5 { { } }", DeclarationLocationName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationLocationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.EnumDeclaration.5 { { } }", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1EnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FileXRefMap.5 { { } }", FileXRefMap

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FileXRefMap":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerDeclaration.5 { { } }", ObjcContainerDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcMethodDeclaration.5 { { } }", ObjcMethodDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1MethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.MethodOverrides.5 { { } }", MethodOverrides

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1MethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclAttribute.5 { { } }", FunctionDeclAttribute

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FunctionDeclAttribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcInterfaceToImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcInterfaceToImplementation.5 { { } }", ObjcInterfaceToImplementation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcInterfaceToImplementation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1UsingDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.UsingDirective.5 { { } }", UsingDirective

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1UsingDirective":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Trace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Trace.5 { { } }", Trace

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1Trace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyImplementation.5 { { } }", ObjcPropertyImplementation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcPropertyImplementation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1MethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.MethodOverridden.5 { { } }", MethodOverridden

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1MethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.PPTrace.5 { { } }", PPTrace

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1PPTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclFamilyOf(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclFamilyOf.5 { { } }", DeclFamilyOf

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclFamilyOf":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.TargetUses.5 { { } }", TargetUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1TargetUses":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDefinition.5 { { } }", NamespaceDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1NamespaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionAttribute.5 { { } }", FunctionAttribute

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FunctionAttribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPUseTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FilePPUseTraceXRefs.5 { { } }", FilePPUseTraceXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FilePPUseTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TypeAliasDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.TypeAliasDeclaration.5 { { } }", TypeAliasDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1TypeAliasDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DefToBaseDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DefToBaseDecl.5 { { } }", DefToBaseDecl

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DefToBaseDecl":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Name(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Name.5 { json.dumps(key) }", Name

  @staticmethod
  def angle_query(*, name: str) -> "Cxx1Name":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPUseXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FilePPUseXRefs.5 { { } }", FilePPUseXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FilePPUseXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Enumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Enumerator.5 { { } }", Enumerator

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1Enumerator":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionQName.5 { { } }", FunctionQName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FunctionQName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TranslationUnitTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.TranslationUnitTrace.5 { { } }", TranslationUnitTrace

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1TranslationUnitTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.RecordDeclaration.5 { { } }", RecordDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1RecordDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FilePPTraceXRefs.5 { { } }", FilePPTraceXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FilePPTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceQName.5 { { } }", NamespaceQName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1NamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationSrcRange.5 { { } }", DeclarationSrcRange

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationSrcRange":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocation.5 { { } }", DeclarationLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjContainerIdName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjContainerIdName.5 { { } }", ObjContainerIdName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjContainerIdName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclaration.5 { { } }", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Same(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Same.5 { { } }", Same

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1Same":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDeclaration.5 { { } }", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1NamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyDeclaration.5 { { } }", ObjcPropertyDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationComment.5 { { } }", DeclarationComment

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationNameSpan.5 { { } }", DeclarationNameSpan

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Declarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Declarations.5 { json.dumps(key) }", Declarations

  @staticmethod
  def angle_query(*, name: str) -> "Cxx1Declarations":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclToFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclToFamily.5 { { } }", DeclToFamily

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclToFamily":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclFamily.5 { json.dumps(key) }", DeclFamily

  @staticmethod
  def angle_query(*, name: str) -> "Cxx1DeclFamily":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclarationNameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclarationNameString.5 { { } }", FunctionDeclarationNameString

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FunctionDeclarationNameString":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumeratorInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.EnumeratorInEnum.5 { { } }", EnumeratorInEnum

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1EnumeratorInEnum":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.RecordDefinition.5 { { } }", RecordDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1RecordDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Type(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Type.5 { json.dumps(key) }", Type

  @staticmethod
  def angle_query(*, name: str) -> "Cxx1Type":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDerived(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.RecordDerived.5 { { } }", RecordDerived

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1RecordDerived":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPDefineLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.PPDefineLocation.5 { { } }", PPDefineLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1PPDefineLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcImplements(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcImplements.5 { { } }", ObjcImplements

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcImplements":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcSelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcSelector.5 { json.dumps(key) }", ObjcSelector

  @staticmethod
  def angle_query(*, name: str) -> "Cxx1ObjcSelector":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcMethodDefinition.5 { json.dumps(key) }", ObjcMethodDefinition

  @staticmethod
  def angle_query(*, name: str) -> "Cxx1ObjcMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationInTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationInTrace.5 { { } }", DeclarationInTrace

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationInTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.PPEntityLocation.5 { { } }", PPEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1PPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1UsingDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.UsingDeclaration.5 { { } }", UsingDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1UsingDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1QName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.QName.5 { { } }", QName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1QName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerDefinition.5 { { } }", ObjcContainerDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcContainerDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.EnumDefinition.5 { { } }", EnumDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1EnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1VariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.VariableDeclaration.5 { { } }", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1VariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocationNameSpan.5 { { } }", DeclarationLocationNameSpan

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationLocationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerBase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerBase.5 { { } }", ObjcContainerBase

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcContainerBase":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationSources.5 { { } }", DeclarationSources

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclarationSources":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TranslationUnitXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.TranslationUnitXRefs.5 { { } }", TranslationUnitXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1TranslationUnitXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyIVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyIVar.5 { { } }", ObjcPropertyIVar

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcPropertyIVar":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerInheritance.5 { { } }", ObjcContainerInheritance

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1ObjcContainerInheritance":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDefinition.5 { { } }", FunctionDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Signature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Signature.5 { { } }", Signature

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1Signature":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclInRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclInRecord.5 { { } }", DeclInRecord

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1DeclInRecord":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FileXRefs.5 { { } }", FileXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FileXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Attribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Attribute.5 { json.dumps(key) }", Attribute

  @staticmethod
  def angle_query(*, name: str) -> "Cxx1Attribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDeclarationName.5 { { } }", NamespaceDeclarationName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1NamespaceDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclarationName.5 { { } }", FunctionDeclarationName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Cxx1FunctionDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionName.5 { json.dumps(key) }", FunctionName

  @staticmethod
  def angle_query(*, name: str) -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")


