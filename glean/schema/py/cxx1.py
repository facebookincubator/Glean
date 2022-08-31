# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.buck import *
from glean.schema.py.pp1 import *
from glean.schema.py.src import *


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
  def build_angle(__env: Dict[str, R], decl: ast.Expr, record: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclInObjcContainer.5 {{ decl = {angle_for(__env, decl)}, record = {angle_for(__env, record)} }}", DeclInObjcContainer

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, record: Optional["Cxx1ObjcContainerDefinition"] = None) -> "Cxx1DeclInObjcContainer":
    raise Exception("this function can only be called from @angle_query")

class Cxx1XRefIndirectTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], via: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.XRefIndirectTarget.5 {{ via = {angle_for(__env, via)}, target = {angle_for(__env, target)} }}", XRefIndirectTarget

  @staticmethod
  def angle_query(*, via: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "Cxx1XRefIndirectTarget":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name_lowercase: ast.Expr, kind: ast.Expr, ident: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclByName.5 {{ name_lowercase = {angle_for(__env, name_lowercase)}, kind = {angle_for(__env, kind)}, ident = {angle_for(__env, ident)} }}", DeclByName

  @staticmethod
  def angle_query(*, name_lowercase: Optional[str] = None, kind: Optional[Tuple[()]] = None, ident: Optional[Tuple[()]] = None) -> "Cxx1DeclByName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationTargets(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, targets: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationTargets.5 {{ source = {angle_for(__env, source)}, targets = {angle_for(__env, targets)} }}", DeclarationTargets

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, targets: Optional[Tuple[()]] = None) -> "Cxx1DeclarationTargets":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, source: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocationName.5 {{ decl = {angle_for(__env, decl)}, source = {angle_for(__env, source)}, name = {angle_for(__env, name)} }}", DeclarationLocationName

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional[str] = None) -> "Cxx1DeclarationLocationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, isScoped: ast.Expr, type: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.EnumDeclaration.5 {{ name = {angle_for(__env, name)}, isScoped = {angle_for(__env, isScoped)}, type = {angle_for(__env, type)}, source = {angle_for(__env, source)} }}", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, isScoped: Optional[bool] = None, type: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1EnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, fixed: ast.Expr, variable: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FileXRefMap.5 {{ file = {angle_for(__env, file)}, fixed = {angle_for(__env, fixed)}, variable = {angle_for(__env, variable)} }}", FileXRefMap

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, fixed: Optional[Tuple[()]] = None, variable: Optional[Tuple[()]] = None) -> "Cxx1FileXRefMap":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], id: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerDeclaration.5 {{ id = {angle_for(__env, id)}, source = {angle_for(__env, source)} }}", ObjcContainerDeclaration

  @staticmethod
  def angle_query(*, id: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], selector: ast.Expr, container: ast.Expr, signature: ast.Expr, isInstance: ast.Expr, isOptional: ast.Expr, isAccessor: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcMethodDeclaration.5 {{ selector = {angle_for(__env, selector)}, container = {angle_for(__env, container)}, signature = {angle_for(__env, signature)}, isInstance = {angle_for(__env, isInstance)}, isOptional = {angle_for(__env, isOptional)}, isAccessor = {angle_for(__env, isAccessor)}, source = {angle_for(__env, source)} }}", ObjcMethodDeclaration

  @staticmethod
  def angle_query(*, selector: Optional["Cxx1ObjcSelector"] = None, container: Optional[Tuple[()]] = None, signature: Optional["Cxx1Signature"] = None, isInstance: Optional[bool] = None, isOptional: Optional[bool] = None, isAccessor: Optional[bool] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1MethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], derived: ast.Expr, base: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.MethodOverrides.5 {{ derived = {angle_for(__env, derived)}, base = {angle_for(__env, base)} }}", MethodOverrides

  @staticmethod
  def angle_query(*, derived: Optional["Cxx1FunctionDeclaration"] = None, base: Optional["Cxx1FunctionDeclaration"] = None) -> "Cxx1MethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, attr: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclAttribute.5 {{ decl = {angle_for(__env, decl)}, attr = {angle_for(__env, attr)} }}", FunctionDeclAttribute

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1FunctionDeclaration"] = None, attr: Optional["Cxx1Attribute"] = None) -> "Cxx1FunctionDeclAttribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcInterfaceToImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], interface_: ast.Expr, implementation: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcInterfaceToImplementation.5 {{ interface_ = {angle_for(__env, interface_)}, implementation = {angle_for(__env, implementation)} }}", ObjcInterfaceToImplementation

  @staticmethod
  def angle_query(*, interface_: Optional["Cxx1ObjcContainerDeclaration"] = None, implementation: Optional["Cxx1ObjcContainerDeclaration"] = None) -> "Cxx1ObjcInterfaceToImplementation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1UsingDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.UsingDirective.5 {{ name = {angle_for(__env, name)}, source = {angle_for(__env, source)} }}", UsingDirective

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, source: Optional[Tuple[()]] = None) -> "Cxx1UsingDirective":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Trace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, declarations: ast.Expr, preprocessor: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Trace.5 {{ file = {angle_for(__env, file)}, declarations = {angle_for(__env, declarations)}, preprocessor = {angle_for(__env, preprocessor)} }}", Trace

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, declarations: Optional["Cxx1Declarations"] = None, preprocessor: Optional["Cxx1PPTrace"] = None) -> "Cxx1Trace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, kind: ast.Expr, ivar: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyImplementation.5 {{ declaration = {angle_for(__env, declaration)}, kind = {angle_for(__env, kind)}, ivar = {angle_for(__env, ivar)}, source = {angle_for(__env, source)} }}", ObjcPropertyImplementation

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1ObjcPropertyDeclaration"] = None, kind: Optional[Tuple[()]] = None, ivar: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcPropertyImplementation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1MethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, derived: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.MethodOverridden.5 {{ base = {angle_for(__env, base)}, derived = {angle_for(__env, derived)} }}", MethodOverridden

  @staticmethod
  def angle_query(*, base: Optional["Cxx1FunctionDeclaration"] = None, derived: Optional["Cxx1FunctionDeclaration"] = None) -> "Cxx1MethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, events: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.PPTrace.5 {{ file = {angle_for(__env, file)}, events = {angle_for(__env, events)} }}", PPTrace

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, events: Optional[Tuple[()]] = None) -> "Cxx1PPTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclFamilyOf(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, family: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclFamilyOf.5 {{ decl = {angle_for(__env, decl)}, family = {angle_for(__env, family)} }}", DeclFamilyOf

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, family: Optional[Tuple[()]] = None) -> "Cxx1DeclFamilyOf":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.TargetUses.5 {{ target = {angle_for(__env, target)}, file = {angle_for(__env, file)}, uses = {angle_for(__env, uses)} }}", TargetUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, uses: Optional[Tuple[()]] = None) -> "Cxx1TargetUses":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, members: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDefinition.5 {{ declaration = {angle_for(__env, declaration)}, members = {angle_for(__env, members)} }}", NamespaceDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1NamespaceDeclaration"] = None, members: Optional["Cxx1Declarations"] = None) -> "Cxx1NamespaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], attr: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionAttribute.5 {{ attr = {angle_for(__env, attr)}, declaration = {angle_for(__env, declaration)} }}", FunctionAttribute

  @staticmethod
  def angle_query(*, attr: Optional["Cxx1Attribute"] = None, declaration: Optional["Cxx1FunctionDeclaration"] = None) -> "Cxx1FunctionAttribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPUseTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, source: ast.Expr, define: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FilePPUseTraceXRefs.5 {{ file = {angle_for(__env, file)}, trace = {angle_for(__env, trace)}, source = {angle_for(__env, source)}, define = {angle_for(__env, define)} }}", FilePPUseTraceXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, source: Optional[Tuple[()]] = None, define: Optional["Pp1Define"] = None) -> "Cxx1FilePPUseTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TypeAliasDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, kind: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.TypeAliasDeclaration.5 {{ name = {angle_for(__env, name)}, type = {angle_for(__env, type)}, kind = {angle_for(__env, kind)}, source = {angle_for(__env, source)} }}", TypeAliasDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, type: Optional["Cxx1Type"] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1TypeAliasDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DefToBaseDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DefToBaseDecl.5 {{ defn = {angle_for(__env, defn)}, decl = {angle_for(__env, decl)} }}", DefToBaseDecl

  @staticmethod
  def angle_query(*, defn: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "Cxx1DefToBaseDecl":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Name(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Name.5 {angle_for(__env, arg)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Name":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPUseXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, source: ast.Expr, define: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FilePPUseXRefs.5 {{ file = {angle_for(__env, file)}, source = {angle_for(__env, source)}, define = {angle_for(__env, define)} }}", FilePPUseXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, source: Optional[Tuple[()]] = None, define: Optional["Pp1Define"] = None) -> "Cxx1FilePPUseXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Enumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, enumeration: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Enumerator.5 {{ name = {angle_for(__env, name)}, enumeration = {angle_for(__env, enumeration)}, source = {angle_for(__env, source)} }}", Enumerator

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, enumeration: Optional["Cxx1EnumDeclaration"] = None, source: Optional[Tuple[()]] = None) -> "Cxx1Enumerator":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionQName.5 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)} }}", FunctionQName

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionName"] = None, scope: Optional[Tuple[()]] = None) -> "Cxx1FunctionQName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TranslationUnitTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], tunit: ast.Expr, trace: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.TranslationUnitTrace.5 {{ tunit = {angle_for(__env, tunit)}, trace = {angle_for(__env, trace)} }}", TranslationUnitTrace

  @staticmethod
  def angle_query(*, tunit: Optional["BuckTranslationUnit"] = None, trace: Optional["Cxx1Trace"] = None) -> "Cxx1TranslationUnitTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, kind: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.RecordDeclaration.5 {{ name = {angle_for(__env, name)}, kind = {angle_for(__env, kind)}, source = {angle_for(__env, source)} }}", RecordDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1RecordDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FilePPTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, source: ast.Expr, ppEntity: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FilePPTraceXRefs.5 {{ file = {angle_for(__env, file)}, trace = {angle_for(__env, trace)}, source = {angle_for(__env, source)}, ppEntity = {angle_for(__env, ppEntity)} }}", FilePPTraceXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, source: Optional[Tuple[()]] = None, ppEntity: Optional[Tuple[()]] = None) -> "Cxx1FilePPTraceXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceQName.5 {{ name = {angle_for(__env, name)}, parent = {angle_for(__env, parent)} }}", NamespaceQName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "Cxx1NamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationSrcRange.5 {{ decl = {angle_for(__env, decl)}, source = {angle_for(__env, source)} }}", DeclarationSrcRange

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1DeclarationSrcRange":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, source: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocation.5 {{ decl = {angle_for(__env, decl)}, source = {angle_for(__env, source)}, name = {angle_for(__env, name)} }}", DeclarationLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional["Cxx1Name"] = None) -> "Cxx1DeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjContainerIdName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], id: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjContainerIdName.5 {{ id = {angle_for(__env, id)}, name = {angle_for(__env, name)} }}", ObjContainerIdName

  @staticmethod
  def angle_query(*, id: Optional[Tuple[()]] = None, name: Optional["Cxx1Name"] = None) -> "Cxx1ObjContainerIdName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, signature: ast.Expr, method: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclaration.5 {{ name = {angle_for(__env, name)}, signature = {angle_for(__env, signature)}, method = {angle_for(__env, method)}, source = {angle_for(__env, source)} }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionQName"] = None, signature: Optional["Cxx1Signature"] = None, method: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1FunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Same(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration1: ast.Expr, declaration2: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Same.5 {{ declaration1 = {angle_for(__env, declaration1)}, declaration2 = {angle_for(__env, declaration2)} }}", Same

  @staticmethod
  def angle_query(*, declaration1: Optional[Tuple[()]] = None, declaration2: Optional[Tuple[()]] = None) -> "Cxx1Same":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDeclaration.5 {{ name = {angle_for(__env, name)}, source = {angle_for(__env, source)} }}", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1NamespaceQName"] = None, source: Optional[Tuple[()]] = None) -> "Cxx1NamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr, type: ast.Expr, isInstance: ast.Expr, isOptional: ast.Expr, isReadOnly: ast.Expr, isAtomic: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyDeclaration.5 {{ name = {angle_for(__env, name)}, container = {angle_for(__env, container)}, type = {angle_for(__env, type)}, isInstance = {angle_for(__env, isInstance)}, isOptional = {angle_for(__env, isOptional)}, isReadOnly = {angle_for(__env, isReadOnly)}, isAtomic = {angle_for(__env, isAtomic)}, source = {angle_for(__env, source)} }}", ObjcPropertyDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, container: Optional[Tuple[()]] = None, type: Optional["Cxx1Type"] = None, isInstance: Optional[bool] = None, isOptional: Optional[bool] = None, isReadOnly: Optional[bool] = None, isAtomic: Optional[bool] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationComment.5 {{ declaration = {angle_for(__env, declaration)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationComment

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationNameSpan.5 {{ decl = {angle_for(__env, decl)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Declarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Declarations.5 {angle_for(__env, arg)}", Declarations

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1Declarations":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclToFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, family: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclToFamily.5 {{ decl = {angle_for(__env, decl)}, family = {angle_for(__env, family)} }}", DeclToFamily

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, family: Optional["Cxx1DeclFamily"] = None) -> "Cxx1DeclToFamily":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclFamily.5 {angle_for(__env, arg)}", DeclFamily

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1DeclFamily":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclarationNameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fname: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclarationNameString.5 {{ fname = {angle_for(__env, fname)}, name = {angle_for(__env, name)} }}", FunctionDeclarationNameString

  @staticmethod
  def angle_query(*, fname: Optional["Cxx1FunctionName"] = None, name: Optional[str] = None) -> "Cxx1FunctionDeclarationNameString":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumeratorInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], enumerator: ast.Expr, enum_: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.EnumeratorInEnum.5 {{ enumerator = {angle_for(__env, enumerator)}, enum_ = {angle_for(__env, enum_)} }}", EnumeratorInEnum

  @staticmethod
  def angle_query(*, enumerator: Optional["Cxx1Enumerator"] = None, enum_: Optional["Cxx1EnumDefinition"] = None) -> "Cxx1EnumeratorInEnum":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, bases: ast.Expr, members: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.RecordDefinition.5 {{ declaration = {angle_for(__env, declaration)}, bases = {angle_for(__env, bases)}, members = {angle_for(__env, members)} }}", RecordDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1RecordDeclaration"] = None, bases: Optional[Tuple[()]] = None, members: Optional["Cxx1Declarations"] = None) -> "Cxx1RecordDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Type(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Type.5 {angle_for(__env, arg)}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Type":
    raise Exception("this function can only be called from @angle_query")

class Cxx1RecordDerived(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, derived: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.RecordDerived.5 {{ base = {angle_for(__env, base)}, derived = {angle_for(__env, derived)} }}", RecordDerived

  @staticmethod
  def angle_query(*, base: Optional["Cxx1RecordDeclaration"] = None, derived: Optional["Cxx1RecordDeclaration"] = None) -> "Cxx1RecordDerived":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPDefineLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], define: ast.Expr, name: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.PPDefineLocation.5 {{ define = {angle_for(__env, define)}, name = {angle_for(__env, name)}, file = {angle_for(__env, file)}, range = {angle_for(__env, range)} }}", PPDefineLocation

  @staticmethod
  def angle_query(*, define: Optional["Pp1Define"] = None, name: Optional[str] = None, file: Optional["SrcFile"] = None, range: Optional[Tuple[()]] = None) -> "Cxx1PPDefineLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcImplements(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], implementation: ast.Expr, interface_: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcImplements.5 {{ implementation = {angle_for(__env, implementation)}, interface_ = {angle_for(__env, interface_)} }}", ObjcImplements

  @staticmethod
  def angle_query(*, implementation: Optional["Cxx1ObjcContainerDeclaration"] = None, interface_: Optional["Cxx1ObjcContainerDeclaration"] = None) -> "Cxx1ObjcImplements":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcSelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcSelector.5 {angle_for(__env, arg)}", ObjcSelector

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1ObjcSelector":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcMethodDefinition.5 {angle_for(__env, arg)}", ObjcMethodDefinition

  @staticmethod
  def angle_query(*, arg: Optional["Cxx1ObjcMethodDeclaration"] = None) -> "Cxx1ObjcMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationInTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, trace: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationInTrace.5 {{ decl = {angle_for(__env, decl)}, trace = {angle_for(__env, trace)} }}", DeclarationInTrace

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, trace: Optional["Cxx1Trace"] = None) -> "Cxx1DeclarationInTrace":
    raise Exception("this function can only be called from @angle_query")

class Cxx1PPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, name: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.PPEntityLocation.5 {{ entity = {angle_for(__env, entity)}, name = {angle_for(__env, name)}, file = {angle_for(__env, file)}, range = {angle_for(__env, range)} }}", PPEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional["SrcFile"] = None, range: Optional[Tuple[()]] = None) -> "Cxx1PPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class Cxx1UsingDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.UsingDeclaration.5 {{ name = {angle_for(__env, name)}, source = {angle_for(__env, source)} }}", UsingDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionQName"] = None, source: Optional[Tuple[()]] = None) -> "Cxx1UsingDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1QName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.QName.5 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)} }}", QName

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional[Tuple[()]] = None) -> "Cxx1QName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, protocols: ast.Expr, members: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerDefinition.5 {{ declaration = {angle_for(__env, declaration)}, protocols = {angle_for(__env, protocols)}, members = {angle_for(__env, members)} }}", ObjcContainerDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1ObjcContainerDeclaration"] = None, protocols: Optional[Tuple[()]] = None, members: Optional["Cxx1Declarations"] = None) -> "Cxx1ObjcContainerDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1EnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, enumerators: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.EnumDefinition.5 {{ declaration = {angle_for(__env, declaration)}, enumerators = {angle_for(__env, enumerators)} }}", EnumDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1EnumDeclaration"] = None, enumerators: Optional[Tuple[()]] = None) -> "Cxx1EnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1VariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, kind: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.VariableDeclaration.5 {{ name = {angle_for(__env, name)}, type = {angle_for(__env, type)}, kind = {angle_for(__env, kind)}, source = {angle_for(__env, source)} }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, type: Optional["Cxx1Type"] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1VariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationLocationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, source: ast.Expr, name: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocationNameSpan.5 {{ decl = {angle_for(__env, decl)}, source = {angle_for(__env, source)}, name = {angle_for(__env, name)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationLocationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationLocationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerBase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, base: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerBase.5 {{ declaration = {angle_for(__env, declaration)}, base = {angle_for(__env, base)} }}", ObjcContainerBase

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1ObjcContainerDeclaration"] = None, base: Optional["Cxx1ObjcContainerDeclaration"] = None) -> "Cxx1ObjcContainerBase":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclarationSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, sources: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationSources.5 {{ target = {angle_for(__env, target)}, sources = {angle_for(__env, sources)} }}", DeclarationSources

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, sources: Optional[Tuple[()]] = None) -> "Cxx1DeclarationSources":
    raise Exception("this function can only be called from @angle_query")

class Cxx1TranslationUnitXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], tunit: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.TranslationUnitXRefs.5 {{ tunit = {angle_for(__env, tunit)}, xrefs = {angle_for(__env, xrefs)} }}", TranslationUnitXRefs

  @staticmethod
  def angle_query(*, tunit: Optional["BuckTranslationUnit"] = None, xrefs: Optional[Tuple[()]] = None) -> "Cxx1TranslationUnitXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcPropertyIVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], property: ast.Expr, ivar: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyIVar.5 {{ property = {angle_for(__env, property)}, ivar = {angle_for(__env, ivar)} }}", ObjcPropertyIVar

  @staticmethod
  def angle_query(*, property: Optional["Cxx1ObjcPropertyDeclaration"] = None, ivar: Optional["Cxx1VariableDeclaration"] = None) -> "Cxx1ObjcPropertyIVar":
    raise Exception("this function can only be called from @angle_query")

class Cxx1ObjcContainerInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerInheritance.5 {{ base = {angle_for(__env, base)}, declaration = {angle_for(__env, declaration)} }}", ObjcContainerInheritance

  @staticmethod
  def angle_query(*, base: Optional["Cxx1ObjcContainerDeclaration"] = None, declaration: Optional["Cxx1ObjcContainerDeclaration"] = None) -> "Cxx1ObjcContainerInheritance":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, isInline: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDefinition.5 {{ declaration = {angle_for(__env, declaration)}, isInline = {angle_for(__env, isInline)} }}", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1FunctionDeclaration"] = None, isInline: Optional[bool] = None) -> "Cxx1FunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Signature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], returns: ast.Expr, parameters: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Signature.5 {{ returns = {angle_for(__env, returns)}, parameters = {angle_for(__env, parameters)} }}", Signature

  @staticmethod
  def angle_query(*, returns: Optional["Cxx1Type"] = None, parameters: Optional[Tuple[()]] = None) -> "Cxx1Signature":
    raise Exception("this function can only be called from @angle_query")

class Cxx1DeclInRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, record: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclInRecord.5 {{ decl = {angle_for(__env, decl)}, record = {angle_for(__env, record)} }}", DeclInRecord

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, record: Optional["Cxx1RecordDefinition"] = None) -> "Cxx1DeclInRecord":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], xmap: ast.Expr, externals: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FileXRefs.5 {{ xmap = {angle_for(__env, xmap)}, externals = {angle_for(__env, externals)} }}", FileXRefs

  @staticmethod
  def angle_query(*, xmap: Optional["Cxx1FileXRefMap"] = None, externals: Optional[Tuple[()]] = None) -> "Cxx1FileXRefs":
    raise Exception("this function can only be called from @angle_query")

class Cxx1Attribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Attribute.5 {angle_for(__env, arg)}", Attribute

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Attribute":
    raise Exception("this function can only be called from @angle_query")

class Cxx1NamespaceDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], qname: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDeclarationName.5 {{ qname = {angle_for(__env, qname)}, name = {angle_for(__env, name)} }}", NamespaceDeclarationName

  @staticmethod
  def angle_query(*, qname: Optional["Cxx1NamespaceQName"] = None, name: Optional["Cxx1Name"] = None) -> "Cxx1NamespaceDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fname: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclarationName.5 {{ fname = {angle_for(__env, fname)}, name = {angle_for(__env, name)} }}", FunctionDeclarationName

  @staticmethod
  def angle_query(*, fname: Optional["Cxx1FunctionName"] = None, name: Optional["Cxx1Name"] = None) -> "Cxx1FunctionDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class Cxx1FunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionName.5 {angle_for(__env, arg)}", FunctionName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")


