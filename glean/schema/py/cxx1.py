# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclInObjcContainer.5 {{ }}", DeclInObjcContainer
    return f"cxx1.DeclInObjcContainer.5 { concatenateFields(key) }", DeclInObjcContainer

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, record: Optional[Tuple[()]] = None) -> "Cxx1DeclInObjcContainer":
    raise Exception("this function can only be called from @angle_query")

class Cxx1XRefIndirectTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.XRefIndirectTarget.5 {{ }}", XRefIndirectTarget
    return f"cxx1.XRefIndirectTarget.5 { concatenateFields(key) }", XRefIndirectTarget

  @staticmethod
  def angle_query(*, via: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "Cxx1XRefIndirectTarget":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclByName.5 {{ }}", DeclByName
    return f"cxx1.DeclByName.5 { concatenateFields(key) }", DeclByName

  @staticmethod
  def angle_query(*, name_lowercase: Optional[str] = None, kind: Optional[Tuple[()]] = None, ident: Optional[Tuple[()]] = None) -> "Cxx1DeclByName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationTargets(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationTargets.5 {{ }}", DeclarationTargets
    return f"cxx1.DeclarationTargets.5 { concatenateFields(key) }", DeclarationTargets

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, targets: Optional[Tuple[()]] = None) -> "Cxx1DeclarationTargets":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationLocationName.5 {{ }}", DeclarationLocationName
    return f"cxx1.DeclarationLocationName.5 { concatenateFields(key) }", DeclarationLocationName

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional[str] = None) -> "Cxx1DeclarationLocationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.EnumDeclaration.5 {{ }}", EnumDeclaration
    return f"cxx1.EnumDeclaration.5 { concatenateFields(key) }", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, isScoped: Optional[bool] = None, type: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1EnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FileXRefMap.5 {{ }}", FileXRefMap
    return f"cxx1.FileXRefMap.5 { concatenateFields(key) }", FileXRefMap

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, fixed: Optional[Tuple[()]] = None, variable: Optional[Tuple[()]] = None) -> "Cxx1FileXRefMap":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcContainerDeclaration.5 {{ }}", ObjcContainerDeclaration
    return f"cxx1.ObjcContainerDeclaration.5 { concatenateFields(key) }", ObjcContainerDeclaration

  @staticmethod
  def angle_query(*, id: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcMethodDeclaration.5 {{ }}", ObjcMethodDeclaration
    return f"cxx1.ObjcMethodDeclaration.5 { concatenateFields(key) }", ObjcMethodDeclaration

  @staticmethod
  def angle_query(*, selector: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None, isInstance: Optional[bool] = None, isOptional: Optional[bool] = None, isAccessor: Optional[bool] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1MethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.MethodOverrides.5 {{ }}", MethodOverrides
    return f"cxx1.MethodOverrides.5 { concatenateFields(key) }", MethodOverrides

  @staticmethod
  def angle_query(*, derived: Optional[Tuple[()]] = None, base: Optional[Tuple[()]] = None) -> "Cxx1MethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FunctionDeclAttribute.5 {{ }}", FunctionDeclAttribute
    return f"cxx1.FunctionDeclAttribute.5 { concatenateFields(key) }", FunctionDeclAttribute

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, attr: Optional[Tuple[()]] = None) -> "Cxx1FunctionDeclAttribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcInterfaceToImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcInterfaceToImplementation.5 {{ }}", ObjcInterfaceToImplementation
    return f"cxx1.ObjcInterfaceToImplementation.5 { concatenateFields(key) }", ObjcInterfaceToImplementation

  @staticmethod
  def angle_query(*, interface_: Optional[Tuple[()]] = None, implementation: Optional[Tuple[()]] = None) -> "Cxx1ObjcInterfaceToImplementation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1UsingDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.UsingDirective.5 {{ }}", UsingDirective
    return f"cxx1.UsingDirective.5 { concatenateFields(key) }", UsingDirective

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1UsingDirective":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Trace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.Trace.5 {{ }}", Trace
    return f"cxx1.Trace.5 { concatenateFields(key) }", Trace

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, declarations: Optional[Tuple[()]] = None, preprocessor: Optional[Tuple[()]] = None) -> "Cxx1Trace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcPropertyImplementation.5 {{ }}", ObjcPropertyImplementation
    return f"cxx1.ObjcPropertyImplementation.5 { concatenateFields(key) }", ObjcPropertyImplementation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, ivar: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcPropertyImplementation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1MethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.MethodOverridden.5 {{ }}", MethodOverridden
    return f"cxx1.MethodOverridden.5 { concatenateFields(key) }", MethodOverridden

  @staticmethod
  def angle_query(*, base: Optional[Tuple[()]] = None, derived: Optional[Tuple[()]] = None) -> "Cxx1MethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.PPTrace.5 {{ }}", PPTrace
    return f"cxx1.PPTrace.5 { concatenateFields(key) }", PPTrace

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, events: Optional[Tuple[()]] = None) -> "Cxx1PPTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclFamilyOf(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclFamilyOf.5 {{ }}", DeclFamilyOf
    return f"cxx1.DeclFamilyOf.5 { concatenateFields(key) }", DeclFamilyOf

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, family: Optional[Tuple[()]] = None) -> "Cxx1DeclFamilyOf":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.TargetUses.5 {{ }}", TargetUses
    return f"cxx1.TargetUses.5 { concatenateFields(key) }", TargetUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None) -> "Cxx1TargetUses":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.NamespaceDefinition.5 {{ }}", NamespaceDefinition
    return f"cxx1.NamespaceDefinition.5 { concatenateFields(key) }", NamespaceDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, members: Optional[Tuple[()]] = None) -> "Cxx1NamespaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FunctionAttribute.5 {{ }}", FunctionAttribute
    return f"cxx1.FunctionAttribute.5 { concatenateFields(key) }", FunctionAttribute

  @staticmethod
  def angle_query(*, attr: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "Cxx1FunctionAttribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPUseTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FilePPUseTraceXRefs.5 {{ }}", FilePPUseTraceXRefs
    return f"cxx1.FilePPUseTraceXRefs.5 { concatenateFields(key) }", FilePPUseTraceXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, define: Optional[Tuple[()]] = None) -> "Cxx1FilePPUseTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TypeAliasDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.TypeAliasDeclaration.5 {{ }}", TypeAliasDeclaration
    return f"cxx1.TypeAliasDeclaration.5 { concatenateFields(key) }", TypeAliasDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1TypeAliasDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DefToBaseDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DefToBaseDecl.5 {{ }}", DefToBaseDecl
    return f"cxx1.DefToBaseDecl.5 { concatenateFields(key) }", DefToBaseDecl

  @staticmethod
  def angle_query(*, defn: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "Cxx1DefToBaseDecl":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Name(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.Name.5 {{ }}", Name
    return f"cxx1.Name.5 {key}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Name":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPUseXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FilePPUseXRefs.5 {{ }}", FilePPUseXRefs
    return f"cxx1.FilePPUseXRefs.5 { concatenateFields(key) }", FilePPUseXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, define: Optional[Tuple[()]] = None) -> "Cxx1FilePPUseXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Enumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.Enumerator.5 {{ }}", Enumerator
    return f"cxx1.Enumerator.5 { concatenateFields(key) }", Enumerator

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, enumeration: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1Enumerator":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FunctionQName.5 {{ }}", FunctionQName
    return f"cxx1.FunctionQName.5 { concatenateFields(key) }", FunctionQName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None) -> "Cxx1FunctionQName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TranslationUnitTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.TranslationUnitTrace.5 {{ }}", TranslationUnitTrace
    return f"cxx1.TranslationUnitTrace.5 { concatenateFields(key) }", TranslationUnitTrace

  @staticmethod
  def angle_query(*, tunit: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None) -> "Cxx1TranslationUnitTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.RecordDeclaration.5 {{ }}", RecordDeclaration
    return f"cxx1.RecordDeclaration.5 { concatenateFields(key) }", RecordDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1RecordDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FilePPTraceXRefs.5 {{ }}", FilePPTraceXRefs
    return f"cxx1.FilePPTraceXRefs.5 { concatenateFields(key) }", FilePPTraceXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, ppEntity: Optional[Tuple[()]] = None) -> "Cxx1FilePPTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.NamespaceQName.5 {{ }}", NamespaceQName
    return f"cxx1.NamespaceQName.5 { concatenateFields(key) }", NamespaceQName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "Cxx1NamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationSrcRange.5 {{ }}", DeclarationSrcRange
    return f"cxx1.DeclarationSrcRange.5 { concatenateFields(key) }", DeclarationSrcRange

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1DeclarationSrcRange":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationLocation.5 {{ }}", DeclarationLocation
    return f"cxx1.DeclarationLocation.5 { concatenateFields(key) }", DeclarationLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None) -> "Cxx1DeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjContainerIdName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjContainerIdName.5 {{ }}", ObjContainerIdName
    return f"cxx1.ObjContainerIdName.5 { concatenateFields(key) }", ObjContainerIdName

  @staticmethod
  def angle_query(*, id: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None) -> "Cxx1ObjContainerIdName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FunctionDeclaration.5 {{ }}", FunctionDeclaration
    return f"cxx1.FunctionDeclaration.5 { concatenateFields(key) }", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None, method: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1FunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Same(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.Same.5 {{ }}", Same
    return f"cxx1.Same.5 { concatenateFields(key) }", Same

  @staticmethod
  def angle_query(*, declaration1: Optional[Tuple[()]] = None, declaration2: Optional[Tuple[()]] = None) -> "Cxx1Same":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.NamespaceDeclaration.5 {{ }}", NamespaceDeclaration
    return f"cxx1.NamespaceDeclaration.5 { concatenateFields(key) }", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1NamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcPropertyDeclaration.5 {{ }}", ObjcPropertyDeclaration
    return f"cxx1.ObjcPropertyDeclaration.5 { concatenateFields(key) }", ObjcPropertyDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, isInstance: Optional[bool] = None, isOptional: Optional[bool] = None, isReadOnly: Optional[bool] = None, isAtomic: Optional[bool] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationComment.5 {{ }}", DeclarationComment
    return f"cxx1.DeclarationComment.5 { concatenateFields(key) }", DeclarationComment

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationNameSpan.5 {{ }}", DeclarationNameSpan
    return f"cxx1.DeclarationNameSpan.5 { concatenateFields(key) }", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Declarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.Declarations.5 {{ }}", Declarations
    return f"cxx1.Declarations.5 {key}", Declarations

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1Declarations":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclToFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclToFamily.5 {{ }}", DeclToFamily
    return f"cxx1.DeclToFamily.5 { concatenateFields(key) }", DeclToFamily

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, family: Optional[Tuple[()]] = None) -> "Cxx1DeclToFamily":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclFamily.5 {{ }}", DeclFamily
    return f"cxx1.DeclFamily.5 {key}", DeclFamily

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1DeclFamily":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclarationNameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FunctionDeclarationNameString.5 {{ }}", FunctionDeclarationNameString
    return f"cxx1.FunctionDeclarationNameString.5 { concatenateFields(key) }", FunctionDeclarationNameString

  @staticmethod
  def angle_query(*, fname: Optional[Tuple[()]] = None, name: Optional[str] = None) -> "Cxx1FunctionDeclarationNameString":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumeratorInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.EnumeratorInEnum.5 {{ }}", EnumeratorInEnum
    return f"cxx1.EnumeratorInEnum.5 { concatenateFields(key) }", EnumeratorInEnum

  @staticmethod
  def angle_query(*, enumerator: Optional[Tuple[()]] = None, enum_: Optional[Tuple[()]] = None) -> "Cxx1EnumeratorInEnum":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.RecordDefinition.5 {{ }}", RecordDefinition
    return f"cxx1.RecordDefinition.5 { concatenateFields(key) }", RecordDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, bases: Optional[Tuple[()]] = None, members: Optional[Tuple[()]] = None) -> "Cxx1RecordDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Type(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.Type.5 {{ }}", Type
    return f"cxx1.Type.5 {key}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Type":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDerived(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.RecordDerived.5 {{ }}", RecordDerived
    return f"cxx1.RecordDerived.5 { concatenateFields(key) }", RecordDerived

  @staticmethod
  def angle_query(*, base: Optional[Tuple[()]] = None, derived: Optional[Tuple[()]] = None) -> "Cxx1RecordDerived":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPDefineLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.PPDefineLocation.5 {{ }}", PPDefineLocation
    return f"cxx1.PPDefineLocation.5 { concatenateFields(key) }", PPDefineLocation

  @staticmethod
  def angle_query(*, define: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "Cxx1PPDefineLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcImplements(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcImplements.5 {{ }}", ObjcImplements
    return f"cxx1.ObjcImplements.5 { concatenateFields(key) }", ObjcImplements

  @staticmethod
  def angle_query(*, implementation: Optional[Tuple[()]] = None, interface_: Optional[Tuple[()]] = None) -> "Cxx1ObjcImplements":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcSelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcSelector.5 {{ }}", ObjcSelector
    return f"cxx1.ObjcSelector.5 {key}", ObjcSelector

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1ObjcSelector":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcMethodDefinition.5 {{ }}", ObjcMethodDefinition
    return f"cxx1.ObjcMethodDefinition.5 {key}", ObjcMethodDefinition

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1ObjcMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationInTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationInTrace.5 {{ }}", DeclarationInTrace
    return f"cxx1.DeclarationInTrace.5 { concatenateFields(key) }", DeclarationInTrace

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None) -> "Cxx1DeclarationInTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.PPEntityLocation.5 {{ }}", PPEntityLocation
    return f"cxx1.PPEntityLocation.5 { concatenateFields(key) }", PPEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "Cxx1PPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1UsingDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.UsingDeclaration.5 {{ }}", UsingDeclaration
    return f"cxx1.UsingDeclaration.5 { concatenateFields(key) }", UsingDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1UsingDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1QName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.QName.5 {{ }}", QName
    return f"cxx1.QName.5 { concatenateFields(key) }", QName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None) -> "Cxx1QName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcContainerDefinition.5 {{ }}", ObjcContainerDefinition
    return f"cxx1.ObjcContainerDefinition.5 { concatenateFields(key) }", ObjcContainerDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, protocols: Optional[Tuple[()]] = None, members: Optional[Tuple[()]] = None) -> "Cxx1ObjcContainerDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.EnumDefinition.5 {{ }}", EnumDefinition
    return f"cxx1.EnumDefinition.5 { concatenateFields(key) }", EnumDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, enumerators: Optional[Tuple[()]] = None) -> "Cxx1EnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1VariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.VariableDeclaration.5 {{ }}", VariableDeclaration
    return f"cxx1.VariableDeclaration.5 { concatenateFields(key) }", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1VariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationLocationNameSpan.5 {{ }}", DeclarationLocationNameSpan
    return f"cxx1.DeclarationLocationNameSpan.5 { concatenateFields(key) }", DeclarationLocationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationLocationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerBase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcContainerBase.5 {{ }}", ObjcContainerBase
    return f"cxx1.ObjcContainerBase.5 { concatenateFields(key) }", ObjcContainerBase

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, base: Optional[Tuple[()]] = None) -> "Cxx1ObjcContainerBase":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclarationSources.5 {{ }}", DeclarationSources
    return f"cxx1.DeclarationSources.5 { concatenateFields(key) }", DeclarationSources

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, sources: Optional[Tuple[()]] = None) -> "Cxx1DeclarationSources":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TranslationUnitXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.TranslationUnitXRefs.5 {{ }}", TranslationUnitXRefs
    return f"cxx1.TranslationUnitXRefs.5 { concatenateFields(key) }", TranslationUnitXRefs

  @staticmethod
  def angle_query(*, tunit: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "Cxx1TranslationUnitXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyIVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcPropertyIVar.5 {{ }}", ObjcPropertyIVar
    return f"cxx1.ObjcPropertyIVar.5 { concatenateFields(key) }", ObjcPropertyIVar

  @staticmethod
  def angle_query(*, property: Optional[Tuple[()]] = None, ivar: Optional[Tuple[()]] = None) -> "Cxx1ObjcPropertyIVar":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.ObjcContainerInheritance.5 {{ }}", ObjcContainerInheritance
    return f"cxx1.ObjcContainerInheritance.5 { concatenateFields(key) }", ObjcContainerInheritance

  @staticmethod
  def angle_query(*, base: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "Cxx1ObjcContainerInheritance":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FunctionDefinition.5 {{ }}", FunctionDefinition
    return f"cxx1.FunctionDefinition.5 { concatenateFields(key) }", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, isInline: Optional[bool] = None) -> "Cxx1FunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Signature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.Signature.5 {{ }}", Signature
    return f"cxx1.Signature.5 { concatenateFields(key) }", Signature

  @staticmethod
  def angle_query(*, returns: Optional[Tuple[()]] = None, parameters: Optional[Tuple[()]] = None) -> "Cxx1Signature":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclInRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.DeclInRecord.5 {{ }}", DeclInRecord
    return f"cxx1.DeclInRecord.5 { concatenateFields(key) }", DeclInRecord

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, record: Optional[Tuple[()]] = None) -> "Cxx1DeclInRecord":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FileXRefs.5 {{ }}", FileXRefs
    return f"cxx1.FileXRefs.5 { concatenateFields(key) }", FileXRefs

  @staticmethod
  def angle_query(*, xmap: Optional[Tuple[()]] = None, externals: Optional[Tuple[()]] = None) -> "Cxx1FileXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Attribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.Attribute.5 {{ }}", Attribute
    return f"cxx1.Attribute.5 {key}", Attribute

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Attribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.NamespaceDeclarationName.5 {{ }}", NamespaceDeclarationName
    return f"cxx1.NamespaceDeclarationName.5 { concatenateFields(key) }", NamespaceDeclarationName

  @staticmethod
  def angle_query(*, qname: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None) -> "Cxx1NamespaceDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FunctionDeclarationName.5 {{ }}", FunctionDeclarationName
    return f"cxx1.FunctionDeclarationName.5 { concatenateFields(key) }", FunctionDeclarationName

  @staticmethod
  def angle_query(*, fname: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None) -> "Cxx1FunctionDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"cxx1.FunctionName.5 {{ }}", FunctionName
    return f"cxx1.FunctionName.5 {key}", FunctionName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")


