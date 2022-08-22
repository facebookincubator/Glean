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
    return f"cxx1.DeclInObjcContainer.5 {{ decl = _, record = _ }}", DeclInObjcContainer

  @staticmethod
  def angle_query(*, decl: Tuple[()], record: Tuple[()]) -> "Cxx1DeclInObjcContainer":
    raise Exception("this function can only be called from @angle_query")

class Cxx1XRefIndirectTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.XRefIndirectTarget.5 {{ via = _, target = _ }}", XRefIndirectTarget

  @staticmethod
  def angle_query(*, via: Tuple[()], target: Tuple[()]) -> "Cxx1XRefIndirectTarget":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclByName.5 {{ name_lowercase = _, kind = _, ident = _ }}", DeclByName

  @staticmethod
  def angle_query(*, name_lowercase: str, kind: Tuple[()], ident: Tuple[()]) -> "Cxx1DeclByName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationTargets(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationTargets.5 {{ source = _, targets = _ }}", DeclarationTargets

  @staticmethod
  def angle_query(*, source: Tuple[()], targets: Tuple[()]) -> "Cxx1DeclarationTargets":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocationName.5 {{ decl = _, source = _, name = _ }}", DeclarationLocationName

  @staticmethod
  def angle_query(*, decl: Tuple[()], source: Tuple[()], name: str) -> "Cxx1DeclarationLocationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.EnumDeclaration.5 {{ name = _, isScoped = _, type = _, source = _ }}", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], isScoped: bool, type: Tuple[()], source: Tuple[()]) -> "Cxx1EnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FileXRefMap.5 {{ file = _, fixed = _, variable = _ }}", FileXRefMap

  @staticmethod
  def angle_query(*, file: Tuple[()], fixed: Tuple[()], variable: Tuple[()]) -> "Cxx1FileXRefMap":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerDeclaration.5 {{ id = _, source = _ }}", ObjcContainerDeclaration

  @staticmethod
  def angle_query(*, id: Tuple[()], source: Tuple[()]) -> "Cxx1ObjcContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcMethodDeclaration.5 {{ selector = _, container = _, signature = _, isInstance = _, isOptional = _, isAccessor = _, source = _ }}", ObjcMethodDeclaration

  @staticmethod
  def angle_query(*, selector: Tuple[()], container: Tuple[()], signature: Tuple[()], isInstance: bool, isOptional: bool, isAccessor: bool, source: Tuple[()]) -> "Cxx1ObjcMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1MethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.MethodOverrides.5 {{ derived = _, base = _ }}", MethodOverrides

  @staticmethod
  def angle_query(*, derived: Tuple[()], base: Tuple[()]) -> "Cxx1MethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclAttribute.5 {{ decl = _, attr = _ }}", FunctionDeclAttribute

  @staticmethod
  def angle_query(*, decl: Tuple[()], attr: Tuple[()]) -> "Cxx1FunctionDeclAttribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcInterfaceToImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcInterfaceToImplementation.5 {{ interface_ = _, implementation = _ }}", ObjcInterfaceToImplementation

  @staticmethod
  def angle_query(*, interface_: Tuple[()], implementation: Tuple[()]) -> "Cxx1ObjcInterfaceToImplementation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1UsingDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.UsingDirective.5 {{ name = _, source = _ }}", UsingDirective

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "Cxx1UsingDirective":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Trace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Trace.5 {{ file = _, declarations = _, preprocessor = _ }}", Trace

  @staticmethod
  def angle_query(*, file: Tuple[()], declarations: Tuple[()], preprocessor: Tuple[()]) -> "Cxx1Trace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyImplementation.5 {{ declaration = _, kind = _, ivar = _, source = _ }}", ObjcPropertyImplementation

  @staticmethod
  def angle_query(*, declaration: Tuple[()], kind: Tuple[()], ivar: Tuple[()], source: Tuple[()]) -> "Cxx1ObjcPropertyImplementation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1MethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.MethodOverridden.5 {{ base = _, derived = _ }}", MethodOverridden

  @staticmethod
  def angle_query(*, base: Tuple[()], derived: Tuple[()]) -> "Cxx1MethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.PPTrace.5 {{ file = _, events = _ }}", PPTrace

  @staticmethod
  def angle_query(*, file: Tuple[()], events: Tuple[()]) -> "Cxx1PPTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclFamilyOf(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclFamilyOf.5 {{ decl = _, family = _ }}", DeclFamilyOf

  @staticmethod
  def angle_query(*, decl: Tuple[()], family: Tuple[()]) -> "Cxx1DeclFamilyOf":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.TargetUses.5 {{ target = _, file = _, uses = _ }}", TargetUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], uses: Tuple[()]) -> "Cxx1TargetUses":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDefinition.5 {{ declaration = _, members = _ }}", NamespaceDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], members: Tuple[()]) -> "Cxx1NamespaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionAttribute.5 {{ attr = _, declaration = _ }}", FunctionAttribute

  @staticmethod
  def angle_query(*, attr: Tuple[()], declaration: Tuple[()]) -> "Cxx1FunctionAttribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPUseTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FilePPUseTraceXRefs.5 {{ file = _, trace = _, source = _, define = _ }}", FilePPUseTraceXRefs

  @staticmethod
  def angle_query(*, file: Tuple[()], trace: Tuple[()], source: Tuple[()], define: Tuple[()]) -> "Cxx1FilePPUseTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TypeAliasDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.TypeAliasDeclaration.5 {{ name = _, type = _, kind = _, source = _ }}", TypeAliasDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], type: Tuple[()], kind: Tuple[()], source: Tuple[()]) -> "Cxx1TypeAliasDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DefToBaseDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DefToBaseDecl.5 {{ defn = _, decl = _ }}", DefToBaseDecl

  @staticmethod
  def angle_query(*, defn: Tuple[()], decl: Tuple[()]) -> "Cxx1DefToBaseDecl":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Name(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Name.5 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: str) -> "Cxx1Name":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPUseXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FilePPUseXRefs.5 {{ file = _, source = _, define = _ }}", FilePPUseXRefs

  @staticmethod
  def angle_query(*, file: Tuple[()], source: Tuple[()], define: Tuple[()]) -> "Cxx1FilePPUseXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Enumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Enumerator.5 {{ name = _, enumeration = _, source = _ }}", Enumerator

  @staticmethod
  def angle_query(*, name: Tuple[()], enumeration: Tuple[()], source: Tuple[()]) -> "Cxx1Enumerator":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionQName.5 {{ name = _, scope = _ }}", FunctionQName

  @staticmethod
  def angle_query(*, name: Tuple[()], scope: Tuple[()]) -> "Cxx1FunctionQName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TranslationUnitTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.TranslationUnitTrace.5 {{ tunit = _, trace = _ }}", TranslationUnitTrace

  @staticmethod
  def angle_query(*, tunit: Tuple[()], trace: Tuple[()]) -> "Cxx1TranslationUnitTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.RecordDeclaration.5 {{ name = _, kind = _, source = _ }}", RecordDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], kind: Tuple[()], source: Tuple[()]) -> "Cxx1RecordDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FilePPTraceXRefs.5 {{ file = _, trace = _, source = _, ppEntity = _ }}", FilePPTraceXRefs

  @staticmethod
  def angle_query(*, file: Tuple[()], trace: Tuple[()], source: Tuple[()], ppEntity: Tuple[()]) -> "Cxx1FilePPTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceQName.5 {{ name = _, parent = _ }}", NamespaceQName

  @staticmethod
  def angle_query(*, name: Tuple[()], parent: Tuple[()]) -> "Cxx1NamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationSrcRange.5 {{ decl = _, source = _ }}", DeclarationSrcRange

  @staticmethod
  def angle_query(*, decl: Tuple[()], source: Tuple[()]) -> "Cxx1DeclarationSrcRange":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocation.5 {{ decl = _, source = _, name = _ }}", DeclarationLocation

  @staticmethod
  def angle_query(*, decl: Tuple[()], source: Tuple[()], name: Tuple[()]) -> "Cxx1DeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjContainerIdName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjContainerIdName.5 {{ id = _, name = _ }}", ObjContainerIdName

  @staticmethod
  def angle_query(*, id: Tuple[()], name: Tuple[()]) -> "Cxx1ObjContainerIdName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclaration.5 {{ name = _, signature = _, method = _, source = _ }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], signature: Tuple[()], method: Tuple[()], source: Tuple[()]) -> "Cxx1FunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Same(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Same.5 {{ declaration1 = _, declaration2 = _ }}", Same

  @staticmethod
  def angle_query(*, declaration1: Tuple[()], declaration2: Tuple[()]) -> "Cxx1Same":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDeclaration.5 {{ name = _, source = _ }}", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "Cxx1NamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyDeclaration.5 {{ name = _, container = _, type = _, isInstance = _, isOptional = _, isReadOnly = _, isAtomic = _, source = _ }}", ObjcPropertyDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], container: Tuple[()], type: Tuple[()], isInstance: bool, isOptional: bool, isReadOnly: bool, isAtomic: bool, source: Tuple[()]) -> "Cxx1ObjcPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationComment.5 {{ declaration = _, file = _, span = _ }}", DeclarationComment

  @staticmethod
  def angle_query(*, declaration: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "Cxx1DeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationNameSpan.5 {{ decl = _, file = _, span = _ }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "Cxx1DeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Declarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Declarations.5 {json.dumps(key)}", Declarations

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "Cxx1Declarations":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclToFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclToFamily.5 {{ decl = _, family = _ }}", DeclToFamily

  @staticmethod
  def angle_query(*, decl: Tuple[()], family: Tuple[()]) -> "Cxx1DeclToFamily":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclFamily.5 {json.dumps(key)}", DeclFamily

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "Cxx1DeclFamily":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclarationNameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclarationNameString.5 {{ fname = _, name = _ }}", FunctionDeclarationNameString

  @staticmethod
  def angle_query(*, fname: Tuple[()], name: str) -> "Cxx1FunctionDeclarationNameString":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumeratorInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.EnumeratorInEnum.5 {{ enumerator = _, enum_ = _ }}", EnumeratorInEnum

  @staticmethod
  def angle_query(*, enumerator: Tuple[()], enum_: Tuple[()]) -> "Cxx1EnumeratorInEnum":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.RecordDefinition.5 {{ declaration = _, bases = _, members = _ }}", RecordDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], bases: Tuple[()], members: Tuple[()]) -> "Cxx1RecordDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Type(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Type.5 {json.dumps(key)}", Type

  @staticmethod
  def angle_query(*, arg: str) -> "Cxx1Type":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDerived(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.RecordDerived.5 {{ base = _, derived = _ }}", RecordDerived

  @staticmethod
  def angle_query(*, base: Tuple[()], derived: Tuple[()]) -> "Cxx1RecordDerived":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPDefineLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.PPDefineLocation.5 {{ define = _, name = _, file = _, range = _ }}", PPDefineLocation

  @staticmethod
  def angle_query(*, define: Tuple[()], name: str, file: Tuple[()], range: Tuple[()]) -> "Cxx1PPDefineLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcImplements(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcImplements.5 {{ implementation = _, interface_ = _ }}", ObjcImplements

  @staticmethod
  def angle_query(*, implementation: Tuple[()], interface_: Tuple[()]) -> "Cxx1ObjcImplements":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcSelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcSelector.5 {json.dumps(key)}", ObjcSelector

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "Cxx1ObjcSelector":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcMethodDefinition.5 {json.dumps(key)}", ObjcMethodDefinition

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "Cxx1ObjcMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationInTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationInTrace.5 {{ decl = _, trace = _ }}", DeclarationInTrace

  @staticmethod
  def angle_query(*, decl: Tuple[()], trace: Tuple[()]) -> "Cxx1DeclarationInTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.PPEntityLocation.5 {{ entity = _, name = _, file = _, range = _ }}", PPEntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], name: str, file: Tuple[()], range: Tuple[()]) -> "Cxx1PPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1UsingDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.UsingDeclaration.5 {{ name = _, source = _ }}", UsingDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "Cxx1UsingDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1QName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.QName.5 {{ name = _, scope = _ }}", QName

  @staticmethod
  def angle_query(*, name: Tuple[()], scope: Tuple[()]) -> "Cxx1QName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerDefinition.5 {{ declaration = _, protocols = _, members = _ }}", ObjcContainerDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], protocols: Tuple[()], members: Tuple[()]) -> "Cxx1ObjcContainerDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.EnumDefinition.5 {{ declaration = _, enumerators = _ }}", EnumDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], enumerators: Tuple[()]) -> "Cxx1EnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1VariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.VariableDeclaration.5 {{ name = _, type = _, kind = _, source = _ }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], type: Tuple[()], kind: Tuple[()], source: Tuple[()]) -> "Cxx1VariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocationNameSpan.5 {{ decl = _, source = _, name = _, file = _, span = _ }}", DeclarationLocationNameSpan

  @staticmethod
  def angle_query(*, decl: Tuple[()], source: Tuple[()], name: str, file: Tuple[()], span: Tuple[()]) -> "Cxx1DeclarationLocationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerBase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerBase.5 {{ declaration = _, base = _ }}", ObjcContainerBase

  @staticmethod
  def angle_query(*, declaration: Tuple[()], base: Tuple[()]) -> "Cxx1ObjcContainerBase":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationSources.5 {{ target = _, sources = _ }}", DeclarationSources

  @staticmethod
  def angle_query(*, target: Tuple[()], sources: Tuple[()]) -> "Cxx1DeclarationSources":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TranslationUnitXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.TranslationUnitXRefs.5 {{ tunit = _, xrefs = _ }}", TranslationUnitXRefs

  @staticmethod
  def angle_query(*, tunit: Tuple[()], xrefs: Tuple[()]) -> "Cxx1TranslationUnitXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyIVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyIVar.5 {{ property = _, ivar = _ }}", ObjcPropertyIVar

  @staticmethod
  def angle_query(*, property: Tuple[()], ivar: Tuple[()]) -> "Cxx1ObjcPropertyIVar":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerInheritance.5 {{ base = _, declaration = _ }}", ObjcContainerInheritance

  @staticmethod
  def angle_query(*, base: Tuple[()], declaration: Tuple[()]) -> "Cxx1ObjcContainerInheritance":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDefinition.5 {{ declaration = _, isInline = _ }}", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], isInline: bool) -> "Cxx1FunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Signature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Signature.5 {{ returns = _, parameters = _ }}", Signature

  @staticmethod
  def angle_query(*, returns: Tuple[()], parameters: Tuple[()]) -> "Cxx1Signature":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclInRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.DeclInRecord.5 {{ decl = _, record = _ }}", DeclInRecord

  @staticmethod
  def angle_query(*, decl: Tuple[()], record: Tuple[()]) -> "Cxx1DeclInRecord":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FileXRefs.5 {{ xmap = _, externals = _ }}", FileXRefs

  @staticmethod
  def angle_query(*, xmap: Tuple[()], externals: Tuple[()]) -> "Cxx1FileXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Attribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.Attribute.5 {json.dumps(key)}", Attribute

  @staticmethod
  def angle_query(*, arg: str) -> "Cxx1Attribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDeclarationName.5 {{ qname = _, name = _ }}", NamespaceDeclarationName

  @staticmethod
  def angle_query(*, qname: Tuple[()], name: Tuple[()]) -> "Cxx1NamespaceDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclarationName.5 {{ fname = _, name = _ }}", FunctionDeclarationName

  @staticmethod
  def angle_query(*, fname: Tuple[()], name: Tuple[()]) -> "Cxx1FunctionDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"cxx1.FunctionName.5 {json.dumps(key)}", FunctionName

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")


