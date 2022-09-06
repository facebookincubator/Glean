# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just
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
    USRToDeclaration,
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
    return f"cxx1.DeclInObjcContainer.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, record, 'record')])) or '_' } }}", DeclInObjcContainer

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, record: Optional["Cxx1ObjcContainerDefinition"] = None) -> "Cxx1DeclInObjcContainer":
    raise Exception("this function can only be called from @angle_query")



class Cxx1XRefIndirectTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], via: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.XRefIndirectTarget.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, via, 'via'), angle_for(__env, target, 'target')])) or '_' } }}", XRefIndirectTarget

  @staticmethod
  def angle_query(*, via: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "Cxx1XRefIndirectTarget":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name_lowercase: ast.Expr, kind: ast.Expr, ident: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclByName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name_lowercase, 'name_lowercase'), angle_for(__env, kind, 'kind'), angle_for(__env, ident, 'ident')])) or '_' } }}", DeclByName

  @staticmethod
  def angle_query(*, name_lowercase: Optional[str] = None, kind: Optional[Tuple[()]] = None, ident: Optional[Tuple[()]] = None) -> "Cxx1DeclByName":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationTargets(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, targets: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationTargets.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, targets, 'targets')])) or '_' } }}", DeclarationTargets

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, targets: Optional[List[Tuple[()]]] = None) -> "Cxx1DeclarationTargets":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationLocationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, source: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocationName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, source, 'source'), angle_for(__env, name, 'name')])) or '_' } }}", DeclarationLocationName

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional[str] = None) -> "Cxx1DeclarationLocationName":
    raise Exception("this function can only be called from @angle_query")



class Cxx1EnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, isScoped: ast.Expr, type: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.EnumDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, isScoped, 'isScoped'), angle_for(__env, type, 'type'), angle_for(__env, source, 'source')])) or '_' } }}", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, isScoped: Optional[bool] = None, type: Optional[Union[Just["Cxx1Type"], Just[None]]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1EnumDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, fixed: ast.Expr, variable: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FileXRefMap.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, fixed, 'fixed'), angle_for(__env, variable, 'variable')])) or '_' } }}", FileXRefMap

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, fixed: Optional[List[Tuple[()]]] = None, variable: Optional[List[Tuple[()]]] = None) -> "Cxx1FileXRefMap":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcContainerDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], id: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, id, 'id'), angle_for(__env, source, 'source')])) or '_' } }}", ObjcContainerDeclaration

  @staticmethod
  def angle_query(*, id: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], selector: ast.Expr, container: ast.Expr, signature: ast.Expr, isInstance: ast.Expr, isOptional: ast.Expr, isAccessor: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcMethodDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, selector, 'selector'), angle_for(__env, container, 'container'), angle_for(__env, signature, 'signature'), angle_for(__env, isInstance, 'isInstance'), angle_for(__env, isOptional, 'isOptional'), angle_for(__env, isAccessor, 'isAccessor'), angle_for(__env, source, 'source')])) or '_' } }}", ObjcMethodDeclaration

  @staticmethod
  def angle_query(*, selector: Optional["Cxx1ObjcSelector"] = None, container: Optional[Tuple[()]] = None, signature: Optional["Cxx1Signature"] = None, isInstance: Optional[bool] = None, isOptional: Optional[bool] = None, isAccessor: Optional[bool] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1MethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], derived: ast.Expr, base: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.MethodOverrides.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, derived, 'derived'), angle_for(__env, base, 'base')])) or '_' } }}", MethodOverrides

  @staticmethod
  def angle_query(*, derived: Optional["Cxx1FunctionDeclaration"] = None, base: Optional["Cxx1FunctionDeclaration"] = None) -> "Cxx1MethodOverrides":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FunctionDeclAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, attr: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclAttribute.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, attr, 'attr')])) or '_' } }}", FunctionDeclAttribute

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1FunctionDeclaration"] = None, attr: Optional["Cxx1Attribute"] = None) -> "Cxx1FunctionDeclAttribute":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcInterfaceToImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], interface_: ast.Expr, implementation: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcInterfaceToImplementation.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, interface_, 'interface_'), angle_for(__env, implementation, 'implementation')])) or '_' } }}", ObjcInterfaceToImplementation

  @staticmethod
  def angle_query(*, interface_: Optional["Cxx1ObjcContainerDeclaration"] = None, implementation: Optional["Cxx1ObjcContainerDeclaration"] = None) -> "Cxx1ObjcInterfaceToImplementation":
    raise Exception("this function can only be called from @angle_query")



class Cxx1UsingDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.UsingDirective.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", UsingDirective

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, source: Optional[Tuple[()]] = None) -> "Cxx1UsingDirective":
    raise Exception("this function can only be called from @angle_query")



class Cxx1Trace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, declarations: ast.Expr, preprocessor: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Trace.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, declarations, 'declarations'), angle_for(__env, preprocessor, 'preprocessor')])) or '_' } }}", Trace

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, declarations: Optional["Cxx1Declarations"] = None, preprocessor: Optional["Cxx1PPTrace"] = None) -> "Cxx1Trace":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcPropertyImplementation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, kind: ast.Expr, ivar: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyImplementation.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, kind, 'kind'), angle_for(__env, ivar, 'ivar'), angle_for(__env, source, 'source')])) or '_' } }}", ObjcPropertyImplementation

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1ObjcPropertyDeclaration"] = None, kind: Optional[Tuple[()]] = None, ivar: Optional[Union[Just["Cxx1Name"], Just[None]]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcPropertyImplementation":
    raise Exception("this function can only be called from @angle_query")



class Cxx1MethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, derived: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.MethodOverridden.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, derived, 'derived')])) or '_' } }}", MethodOverridden

  @staticmethod
  def angle_query(*, base: Optional["Cxx1FunctionDeclaration"] = None, derived: Optional["Cxx1FunctionDeclaration"] = None) -> "Cxx1MethodOverridden":
    raise Exception("this function can only be called from @angle_query")



class Cxx1PPTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, events: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.PPTrace.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, events, 'events')])) or '_' } }}", PPTrace

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, events: Optional[List[Tuple[()]]] = None) -> "Cxx1PPTrace":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclFamilyOf(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, family: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclFamilyOf.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, family, 'family')])) or '_' } }}", DeclFamilyOf

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, family: Optional[Tuple[()]] = None) -> "Cxx1DeclFamilyOf":
    raise Exception("this function can only be called from @angle_query")



class Cxx1TargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.TargetUses.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, uses, 'uses')])) or '_' } }}", TargetUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, uses: Optional[Tuple[()]] = None) -> "Cxx1TargetUses":
    raise Exception("this function can only be called from @angle_query")



class Cxx1NamespaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, members: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDefinition.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, members, 'members')])) or '_' } }}", NamespaceDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1NamespaceDeclaration"] = None, members: Optional["Cxx1Declarations"] = None) -> "Cxx1NamespaceDefinition":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FunctionAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], attr: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionAttribute.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, attr, 'attr'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", FunctionAttribute

  @staticmethod
  def angle_query(*, attr: Optional["Cxx1Attribute"] = None, declaration: Optional["Cxx1FunctionDeclaration"] = None) -> "Cxx1FunctionAttribute":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FilePPUseTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, source: ast.Expr, define: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FilePPUseTraceXRefs.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, source, 'source'), angle_for(__env, define, 'define')])) or '_' } }}", FilePPUseTraceXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, source: Optional[Tuple[()]] = None, define: Optional["Pp1Define"] = None) -> "Cxx1FilePPUseTraceXRefs":
    raise Exception("this function can only be called from @angle_query")



class Cxx1TypeAliasDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, kind: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.TypeAliasDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, kind, 'kind'), angle_for(__env, source, 'source')])) or '_' } }}", TypeAliasDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, type: Optional["Cxx1Type"] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1TypeAliasDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1USRToDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], hash: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.USRToDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, hash, 'hash'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", USRToDeclaration

  @staticmethod
  def angle_query(*, hash: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "Cxx1USRToDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DefToBaseDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DefToBaseDecl.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, decl, 'decl')])) or '_' } }}", DefToBaseDecl

  @staticmethod
  def angle_query(*, defn: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "Cxx1DefToBaseDecl":
    raise Exception("this function can only be called from @angle_query")



class Cxx1Name(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Name.5 { angle_for(__env, arg, None) or '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Name":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FilePPUseXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, source: ast.Expr, define: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FilePPUseXRefs.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, source, 'source'), angle_for(__env, define, 'define')])) or '_' } }}", FilePPUseXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, source: Optional[Tuple[()]] = None, define: Optional["Pp1Define"] = None) -> "Cxx1FilePPUseXRefs":
    raise Exception("this function can only be called from @angle_query")



class Cxx1Enumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, enumeration: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Enumerator.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, enumeration, 'enumeration'), angle_for(__env, source, 'source')])) or '_' } }}", Enumerator

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, enumeration: Optional["Cxx1EnumDeclaration"] = None, source: Optional[Tuple[()]] = None) -> "Cxx1Enumerator":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FunctionQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionQName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope')])) or '_' } }}", FunctionQName

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionName"] = None, scope: Optional[Tuple[()]] = None) -> "Cxx1FunctionQName":
    raise Exception("this function can only be called from @angle_query")



class Cxx1TranslationUnitTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], tunit: ast.Expr, trace: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.TranslationUnitTrace.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, tunit, 'tunit'), angle_for(__env, trace, 'trace')])) or '_' } }}", TranslationUnitTrace

  @staticmethod
  def angle_query(*, tunit: Optional["BuckTranslationUnit"] = None, trace: Optional["Cxx1Trace"] = None) -> "Cxx1TranslationUnitTrace":
    raise Exception("this function can only be called from @angle_query")



class Cxx1RecordDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, kind: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.RecordDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, kind, 'kind'), angle_for(__env, source, 'source')])) or '_' } }}", RecordDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1RecordDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FilePPTraceXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, source: ast.Expr, ppEntity: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FilePPTraceXRefs.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, source, 'source'), angle_for(__env, ppEntity, 'ppEntity')])) or '_' } }}", FilePPTraceXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, source: Optional[Tuple[()]] = None, ppEntity: Optional[Tuple[()]] = None) -> "Cxx1FilePPTraceXRefs":
    raise Exception("this function can only be called from @angle_query")



class Cxx1NamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceQName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent')])) or '_' } }}", NamespaceQName

  @staticmethod
  def angle_query(*, name: Optional[Union[Just["Cxx1Name"], Just[None]]] = None, parent: Optional[Union[Just["Cxx1NamespaceQName"], Just[None]]] = None) -> "Cxx1NamespaceQName":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationSrcRange.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, source, 'source')])) or '_' } }}", DeclarationSrcRange

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1DeclarationSrcRange":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, source: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocation.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, source, 'source'), angle_for(__env, name, 'name')])) or '_' } }}", DeclarationLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional["Cxx1Name"] = None) -> "Cxx1DeclarationLocation":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjContainerIdName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], id: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjContainerIdName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, id, 'id'), angle_for(__env, name, 'name')])) or '_' } }}", ObjContainerIdName

  @staticmethod
  def angle_query(*, id: Optional[Tuple[()]] = None, name: Optional["Cxx1Name"] = None) -> "Cxx1ObjContainerIdName":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, signature: ast.Expr, method: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, signature, 'signature'), angle_for(__env, method, 'method'), angle_for(__env, source, 'source')])) or '_' } }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionQName"] = None, signature: Optional["Cxx1Signature"] = None, method: Optional[Union[Just[Tuple[()]], Just[None]]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1FunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1Same(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration1: ast.Expr, declaration2: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Same.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration1, 'declaration1'), angle_for(__env, declaration2, 'declaration2')])) or '_' } }}", Same

  @staticmethod
  def angle_query(*, declaration1: Optional[Tuple[()]] = None, declaration2: Optional[Tuple[()]] = None) -> "Cxx1Same":
    raise Exception("this function can only be called from @angle_query")



class Cxx1NamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1NamespaceQName"] = None, source: Optional[Tuple[()]] = None) -> "Cxx1NamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr, type: ast.Expr, isInstance: ast.Expr, isOptional: ast.Expr, isReadOnly: ast.Expr, isAtomic: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container'), angle_for(__env, type, 'type'), angle_for(__env, isInstance, 'isInstance'), angle_for(__env, isOptional, 'isOptional'), angle_for(__env, isReadOnly, 'isReadOnly'), angle_for(__env, isAtomic, 'isAtomic'), angle_for(__env, source, 'source')])) or '_' } }}", ObjcPropertyDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, container: Optional[Tuple[()]] = None, type: Optional["Cxx1Type"] = None, isInstance: Optional[bool] = None, isOptional: Optional[bool] = None, isReadOnly: Optional[bool] = None, isAtomic: Optional[bool] = None, source: Optional[Tuple[()]] = None) -> "Cxx1ObjcPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationComment.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationComment

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationComment":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationNameSpan.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")



class Cxx1Declarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Declarations.5 { angle_for(__env, arg, None) or '_' }", Declarations

  @staticmethod
  def angle_query(*, arg: Optional[List[Tuple[()]]] = None) -> "Cxx1Declarations":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclToFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, family: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclToFamily.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, family, 'family')])) or '_' } }}", DeclToFamily

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, family: Optional["Cxx1DeclFamily"] = None) -> "Cxx1DeclToFamily":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclFamily(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclFamily.5 { angle_for(__env, arg, None) or '_' }", DeclFamily

  @staticmethod
  def angle_query(*, arg: Optional[List[Tuple[()]]] = None) -> "Cxx1DeclFamily":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FunctionDeclarationNameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fname: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclarationNameString.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fname, 'fname'), angle_for(__env, name, 'name')])) or '_' } }}", FunctionDeclarationNameString

  @staticmethod
  def angle_query(*, fname: Optional["Cxx1FunctionName"] = None, name: Optional[str] = None) -> "Cxx1FunctionDeclarationNameString":
    raise Exception("this function can only be called from @angle_query")



class Cxx1EnumeratorInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], enumerator: ast.Expr, enum_: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.EnumeratorInEnum.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, enumerator, 'enumerator'), angle_for(__env, enum_, 'enum_')])) or '_' } }}", EnumeratorInEnum

  @staticmethod
  def angle_query(*, enumerator: Optional["Cxx1Enumerator"] = None, enum_: Optional["Cxx1EnumDefinition"] = None) -> "Cxx1EnumeratorInEnum":
    raise Exception("this function can only be called from @angle_query")



class Cxx1RecordDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, bases: ast.Expr, members: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.RecordDefinition.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, bases, 'bases'), angle_for(__env, members, 'members')])) or '_' } }}", RecordDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1RecordDeclaration"] = None, bases: Optional[List[Tuple[()]]] = None, members: Optional["Cxx1Declarations"] = None) -> "Cxx1RecordDefinition":
    raise Exception("this function can only be called from @angle_query")



class Cxx1Type(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Type.5 { angle_for(__env, arg, None) or '_' }", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Type":
    raise Exception("this function can only be called from @angle_query")



class Cxx1RecordDerived(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, derived: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.RecordDerived.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, derived, 'derived')])) or '_' } }}", RecordDerived

  @staticmethod
  def angle_query(*, base: Optional["Cxx1RecordDeclaration"] = None, derived: Optional["Cxx1RecordDeclaration"] = None) -> "Cxx1RecordDerived":
    raise Exception("this function can only be called from @angle_query")



class Cxx1PPDefineLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], define: ast.Expr, name: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.PPDefineLocation.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, define, 'define'), angle_for(__env, name, 'name'), angle_for(__env, file, 'file'), angle_for(__env, range, 'range')])) or '_' } }}", PPDefineLocation

  @staticmethod
  def angle_query(*, define: Optional["Pp1Define"] = None, name: Optional[str] = None, file: Optional["SrcFile"] = None, range: Optional[Tuple[()]] = None) -> "Cxx1PPDefineLocation":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcImplements(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], implementation: ast.Expr, interface_: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcImplements.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, implementation, 'implementation'), angle_for(__env, interface_, 'interface_')])) or '_' } }}", ObjcImplements

  @staticmethod
  def angle_query(*, implementation: Optional["Cxx1ObjcContainerDeclaration"] = None, interface_: Optional["Cxx1ObjcContainerDeclaration"] = None) -> "Cxx1ObjcImplements":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcSelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcSelector.5 { angle_for(__env, arg, None) or '_' }", ObjcSelector

  @staticmethod
  def angle_query(*, arg: Optional[List[str]] = None) -> "Cxx1ObjcSelector":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcMethodDefinition.5 { angle_for(__env, arg, None) or '_' }", ObjcMethodDefinition

  @staticmethod
  def angle_query(*, arg: Optional["Cxx1ObjcMethodDeclaration"] = None) -> "Cxx1ObjcMethodDefinition":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationInTrace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, trace: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationInTrace.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, trace, 'trace')])) or '_' } }}", DeclarationInTrace

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, trace: Optional["Cxx1Trace"] = None) -> "Cxx1DeclarationInTrace":
    raise Exception("this function can only be called from @angle_query")



class Cxx1PPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, name: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.PPEntityLocation.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, name, 'name'), angle_for(__env, file, 'file'), angle_for(__env, range, 'range')])) or '_' } }}", PPEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional["SrcFile"] = None, range: Optional[Tuple[()]] = None) -> "Cxx1PPEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class Cxx1UsingDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.UsingDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", UsingDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionQName"] = None, source: Optional[Tuple[()]] = None) -> "Cxx1UsingDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1QName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.QName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope')])) or '_' } }}", QName

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional[Tuple[()]] = None) -> "Cxx1QName":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcContainerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, protocols: ast.Expr, members: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerDefinition.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, protocols, 'protocols'), angle_for(__env, members, 'members')])) or '_' } }}", ObjcContainerDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1ObjcContainerDeclaration"] = None, protocols: Optional[List["Cxx1ObjcContainerDeclaration"]] = None, members: Optional["Cxx1Declarations"] = None) -> "Cxx1ObjcContainerDefinition":
    raise Exception("this function can only be called from @angle_query")



class Cxx1EnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, enumerators: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.EnumDefinition.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, enumerators, 'enumerators')])) or '_' } }}", EnumDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1EnumDeclaration"] = None, enumerators: Optional[List["Cxx1Enumerator"]] = None) -> "Cxx1EnumDefinition":
    raise Exception("this function can only be called from @angle_query")



class Cxx1VariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, kind: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.VariableDeclaration.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, kind, 'kind'), angle_for(__env, source, 'source')])) or '_' } }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1QName"] = None, type: Optional["Cxx1Type"] = None, kind: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Cxx1VariableDeclaration":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationLocationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, source: ast.Expr, name: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationLocationNameSpan.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, source, 'source'), angle_for(__env, name, 'name'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationLocationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "Cxx1DeclarationLocationNameSpan":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcContainerBase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, base: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerBase.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, base, 'base')])) or '_' } }}", ObjcContainerBase

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1ObjcContainerDeclaration"] = None, base: Optional["Cxx1ObjcContainerDeclaration"] = None) -> "Cxx1ObjcContainerBase":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclarationSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, sources: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclarationSources.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, sources, 'sources')])) or '_' } }}", DeclarationSources

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, sources: Optional[List[Tuple[()]]] = None) -> "Cxx1DeclarationSources":
    raise Exception("this function can only be called from @angle_query")



class Cxx1TranslationUnitXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], tunit: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.TranslationUnitXRefs.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, tunit, 'tunit'), angle_for(__env, xrefs, 'xrefs')])) or '_' } }}", TranslationUnitXRefs

  @staticmethod
  def angle_query(*, tunit: Optional["BuckTranslationUnit"] = None, xrefs: Optional[List["Cxx1FileXRefs"]] = None) -> "Cxx1TranslationUnitXRefs":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcPropertyIVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], property: ast.Expr, ivar: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcPropertyIVar.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, property, 'property'), angle_for(__env, ivar, 'ivar')])) or '_' } }}", ObjcPropertyIVar

  @staticmethod
  def angle_query(*, property: Optional["Cxx1ObjcPropertyDeclaration"] = None, ivar: Optional["Cxx1VariableDeclaration"] = None) -> "Cxx1ObjcPropertyIVar":
    raise Exception("this function can only be called from @angle_query")



class Cxx1ObjcContainerInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.ObjcContainerInheritance.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", ObjcContainerInheritance

  @staticmethod
  def angle_query(*, base: Optional["Cxx1ObjcContainerDeclaration"] = None, declaration: Optional["Cxx1ObjcContainerDeclaration"] = None) -> "Cxx1ObjcContainerInheritance":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, isInline: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDefinition.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, isInline, 'isInline')])) or '_' } }}", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["Cxx1FunctionDeclaration"] = None, isInline: Optional[bool] = None) -> "Cxx1FunctionDefinition":
    raise Exception("this function can only be called from @angle_query")



class Cxx1Signature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], returns: ast.Expr, parameters: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Signature.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, returns, 'returns'), angle_for(__env, parameters, 'parameters')])) or '_' } }}", Signature

  @staticmethod
  def angle_query(*, returns: Optional["Cxx1Type"] = None, parameters: Optional[List[Tuple[()]]] = None) -> "Cxx1Signature":
    raise Exception("this function can only be called from @angle_query")



class Cxx1DeclInRecord(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, record: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.DeclInRecord.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, record, 'record')])) or '_' } }}", DeclInRecord

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, record: Optional["Cxx1RecordDefinition"] = None) -> "Cxx1DeclInRecord":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], xmap: ast.Expr, externals: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FileXRefs.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, xmap, 'xmap'), angle_for(__env, externals, 'externals')])) or '_' } }}", FileXRefs

  @staticmethod
  def angle_query(*, xmap: Optional["Cxx1FileXRefMap"] = None, externals: Optional[List[Tuple[()]]] = None) -> "Cxx1FileXRefs":
    raise Exception("this function can only be called from @angle_query")



class Cxx1Attribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.Attribute.5 { angle_for(__env, arg, None) or '_' }", Attribute

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Cxx1Attribute":
    raise Exception("this function can only be called from @angle_query")



class Cxx1NamespaceDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], qname: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.NamespaceDeclarationName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, qname, 'qname'), angle_for(__env, name, 'name')])) or '_' } }}", NamespaceDeclarationName

  @staticmethod
  def angle_query(*, qname: Optional["Cxx1NamespaceQName"] = None, name: Optional["Cxx1Name"] = None) -> "Cxx1NamespaceDeclarationName":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FunctionDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fname: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionDeclarationName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fname, 'fname'), angle_for(__env, name, 'name')])) or '_' } }}", FunctionDeclarationName

  @staticmethod
  def angle_query(*, fname: Optional["Cxx1FunctionName"] = None, name: Optional["Cxx1Name"] = None) -> "Cxx1FunctionDeclarationName":
    raise Exception("this function can only be called from @angle_query")



class Cxx1FunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, operator_: ast.Expr, literalOperator: ast.Expr, constructor: ast.Expr, destructor: ast.Expr, conversionOperator: ast.Expr) -> Tuple[str, Struct]:
    return f"cxx1.FunctionName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, operator_, 'operator_'), angle_for(__env, literalOperator, 'literalOperator'), angle_for(__env, constructor, 'constructor'), angle_for(__env, destructor, 'destructor'), angle_for(__env, conversionOperator, 'conversionOperator')])) or '_' } }}", FunctionName

  @staticmethod
  def angle_query_name(*, name: "Cxx1Name") -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_operator_(*, operator_: Tuple[()]) -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_literalOperator(*, literalOperator: Tuple[()]) -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_constructor(*, constructor: Tuple[()]) -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_destructor(*, destructor: Tuple[()]) -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_conversionOperator(*, conversionOperator: "Cxx1Type") -> "Cxx1FunctionName":
    raise Exception("this function can only be called from @angle_query")





