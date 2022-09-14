# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.src import *


from glean.schema.hack.types import (
    MethodDefinition,
    symbolNamespace,
    TraitDeclaration,
    FunctionDeclaration,
    TypedefDefinition,
    AttributeToDefinition,
    NamespaceMember,
    GlobalConstDefinition,
    ContainerParent,
    InterfaceDefinition,
    Context_,
    ContainerDeclarationQName,
    TargetUses,
    TargetUsesAbs,
    MethodOverridden,
    ClassDeclaration,
    QName,
    filename,
    kind,
    TypedefDeclaration,
    ModuleDefinition,
    AttributeHasParameter,
    Name,
    MethodDeclaration,
    AttributeToDeclaration,
    FileXRefs,
    Enumerator,
    identifier,
    DeclarationSpan,
    Signature,
    ContainerChild,
    NamespaceQName,
    InterfaceDeclaration,
    DeclarationSource,
    UserAttribute,
    ModuleDeclaration,
    PropertyDefinition,
    ClassConstDeclaration,
    EnumDeclaration,
    DeclarationComment,
    NamespaceDeclaration,
    ClassDefinition,
    MethodOccurrence,
    MethodOverrides,
    Type,
    NameLowerCase,
    TypeConstDefinition,
    DeclarationTarget,
    symbol,
    EnumDefinition,
    ClassConstDefinition,
    StringLiteral,
    GlobalConstDeclaration,
    FunctionDefinition,
    TraitDefinition,
    DeclarationName,
    PropertyDeclaration,
    FileCall,
    FileDeclarations,
    TypeConstDeclaration,
    DeclarationLocation,
    Argument,
    context,
    TypeParameter,
    Definition,
    Declaration,
    XRefTarget,
    ModuleMembership,
    ReifyKind,
    XRef,
    Parameter,
    CallArgument,
    Visibility,
    Occurrence,
    ContainerDeclaration,
    TypeConstKind,
    Constraint,
    Variance,
    ConstraintKind,
)


class HackMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, signature: ast.Expr, visibility: ast.Expr, isAbstract: ast.Expr, isAsync: ast.Expr, isFinal: ast.Expr, isStatic: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, signature, 'signature'), angle_for(__env, visibility, 'visibility'), angle_for(__env, isAbstract, 'isAbstract'), angle_for(__env, isAsync, 'isAsync'), angle_for(__env, isFinal, 'isFinal'), angle_for(__env, isStatic, 'isStatic'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams')]))
    return f"hack.MethodDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", MethodDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackMethodDeclaration"] = None, signature: Optional["HackSignature"] = None, visibility: Optional["HackVisibility"] = None, isAbstract: Optional[bool] = None, isAsync: Optional[bool] = None, isFinal: Optional[bool] = None, isStatic: Optional[bool] = None, attributes: Optional[List["HackUserAttribute"]] = None, typeParams: Optional[List["HackTypeParameter"]] = None) -> "HackMethodDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackSymbolNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], namespace_id: ast.Expr, namespace_name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, namespace_id, 'namespace_id'), angle_for(__env, namespace_name, 'namespace_name')]))
    return f"hack.symbolNamespace.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", symbolNamespace

  @staticmethod
  def angle_query(*, namespace_id: Optional[int] = None, namespace_name: Optional[str] = None) -> "HackSymbolNamespace":
    raise Exception("this function can only be called from @angle_query")



class HackTraitDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.TraitDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TraitDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackTraitDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.FunctionDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackTypedefDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, isTransparent: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, isTransparent, 'isTransparent'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, module_, 'module_')]))
    return f"hack.TypedefDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TypedefDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackTypedefDeclaration"] = None, isTransparent: Optional[bool] = None, attributes: Optional[List["HackUserAttribute"]] = None, typeParams: Optional[List["HackTypeParameter"]] = None, module_: Optional[Union[Just["HackModuleMembership"], Just[None]]] = None) -> "HackTypedefDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackAttributeToDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], attribute: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, attribute, 'attribute'), angle_for(__env, definition, 'definition')]))
    return f"hack.AttributeToDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", AttributeToDefinition

  @staticmethod
  def angle_query(*, attribute: Optional["HackUserAttribute"] = None, definition: Optional["HackDefinition"] = None) -> "HackAttributeToDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackNamespaceMember(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], namespace_: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, namespace_, 'namespace_'), angle_for(__env, decl, 'decl')]))
    return f"hack.NamespaceMember.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", NamespaceMember

  @staticmethod
  def angle_query(*, namespace_: Optional["HackNamespaceQName"] = None, decl: Optional["HackDeclaration"] = None) -> "HackNamespaceMember":
    raise Exception("this function can only be called from @angle_query")



class HackGlobalConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type'), angle_for(__env, value, 'value')]))
    return f"hack.GlobalConstDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", GlobalConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackGlobalConstDeclaration"] = None, type: Optional[Union[Just["HackType"], Just[None]]] = None, value: Optional[str] = None) -> "HackGlobalConstDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackContainerParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], container: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, container, 'container'), angle_for(__env, parent, 'parent')]))
    return f"hack.ContainerParent.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContainerParent

  @staticmethod
  def angle_query(*, container: Optional["HackContainerDeclaration"] = None, parent: Optional["HackContainerDeclaration"] = None) -> "HackContainerParent":
    raise Exception("this function can only be called from @angle_query")



class HackInterfaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, members: ast.Expr, extends_: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, requireExtends: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, members, 'members'), angle_for(__env, extends_, 'extends_'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, requireExtends, 'requireExtends'), angle_for(__env, module_, 'module_')]))
    return f"hack.InterfaceDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", InterfaceDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackInterfaceDeclaration"] = None, members: Optional[List["HackDeclaration"]] = None, extends_: Optional[List["HackInterfaceDeclaration"]] = None, attributes: Optional[List["HackUserAttribute"]] = None, typeParams: Optional[List["HackTypeParameter"]] = None, requireExtends: Optional[List["HackClassDeclaration"]] = None, module_: Optional[Union[Just["HackModuleMembership"], Just[None]]] = None) -> "HackInterfaceDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackContext_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"hack.Context_.6 { query_fields if query_fields else '_' }", Context_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackContext_":
    raise Exception("this function can only be called from @angle_query")



class HackContainerDeclarationQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"hack.ContainerDeclarationQName.6 { query_fields if query_fields else '_' }", ContainerDeclarationQName

  @staticmethod
  def angle_query(*, arg: Optional["HackContainerDeclaration"] = None) -> "HackContainerDeclarationQName":
    raise Exception("this function can only be called from @angle_query")



class HackTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, uses, 'uses')]))
    return f"hack.TargetUses.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetUses

  @staticmethod
  def angle_query(*, target: Optional["HackXRefTarget"] = None, file: Optional["SrcFile"] = None, uses: Optional["SrcByteSpans"] = None) -> "HackTargetUses":
    raise Exception("this function can only be called from @angle_query")



class HackTargetUsesAbs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, uses, 'uses')]))
    return f"hack.TargetUsesAbs.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetUsesAbs

  @staticmethod
  def angle_query(*, target: Optional["HackXRefTarget"] = None, file: Optional["SrcFile"] = None, uses: Optional["SrcByteSpans"] = None) -> "HackTargetUsesAbs":
    raise Exception("this function can only be called from @angle_query")



class HackMethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, derived: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, derived, 'derived')]))
    return f"hack.MethodOverridden.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", MethodOverridden

  @staticmethod
  def angle_query(*, base: Optional["HackMethodDeclaration"] = None, derived: Optional["HackMethodDeclaration"] = None) -> "HackMethodOverridden":
    raise Exception("this function can only be called from @angle_query")



class HackClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.ClassDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackClassDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, namespace_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, namespace_, 'namespace_')]))
    return f"hack.QName.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", QName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, namespace_: Optional[Union[Just["HackNamespaceQName"], Just[None]]] = None) -> "HackQName":
    raise Exception("this function can only be called from @angle_query")



class HackFilename(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], filename: ast.Expr, filehash_id: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, filename, 'filename'), angle_for(__env, filehash_id, 'filehash_id')]))
    return f"hack.filename.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", filename

  @staticmethod
  def angle_query(*, filename: Optional[str] = None, filehash_id: Optional[str] = None) -> "HackFilename":
    raise Exception("this function can only be called from @angle_query")



class HackKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], id: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, id, 'id'), angle_for(__env, name, 'name')]))
    return f"hack.kind.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", kind

  @staticmethod
  def angle_query(*, id: Optional[int] = None, name: Optional[str] = None) -> "HackKind":
    raise Exception("this function can only be called from @angle_query")



class HackTypedefDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.TypedefDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TypedefDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackTypedefDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, attributes: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, attributes, 'attributes')]))
    return f"hack.ModuleDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ModuleDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackModuleDeclaration"] = None, attributes: Optional[List["HackUserAttribute"]] = None) -> "HackModuleDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackAttributeHasParameter(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameter: ast.Expr, attribute: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parameter, 'parameter'), angle_for(__env, attribute, 'attribute')]))
    return f"hack.AttributeHasParameter.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", AttributeHasParameter

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, parameter: Optional[str] = None, attribute: Optional["HackUserAttribute"] = None) -> "HackAttributeHasParameter":
    raise Exception("this function can only be called from @angle_query")



class HackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"hack.Name.6 { query_fields if query_fields else '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackName":
    raise Exception("this function can only be called from @angle_query")



class HackMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container')]))
    return f"hack.MethodDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, container: Optional["HackContainerDeclaration"] = None) -> "HackMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackAttributeToDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], attribute: ast.Expr, declaration: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, attribute, 'attribute'), angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file')]))
    return f"hack.AttributeToDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", AttributeToDeclaration

  @staticmethod
  def angle_query(*, attribute: Optional["HackUserAttribute"] = None, declaration: Optional["HackDeclaration"] = None, file: Optional["SrcFile"] = None) -> "HackAttributeToDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')]))
    return f"hack.FileXRefs.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[List["HackXRef"]] = None) -> "HackFileXRefs":
    raise Exception("this function can only be called from @angle_query")



class HackEnumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, enumeration: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, enumeration, 'enumeration')]))
    return f"hack.Enumerator.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Enumerator

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, enumeration: Optional["HackEnumDeclaration"] = None) -> "HackEnumerator":
    raise Exception("this function can only be called from @angle_query")



class HackIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind'), angle_for(__env, name, 'name')]))
    return f"hack.identifier.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", identifier

  @staticmethod
  def angle_query(*, kind: Optional[int] = None, name: Optional[str] = None) -> "HackIdentifier":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"hack.DeclarationSpan.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationSpan

  @staticmethod
  def angle_query(*, declaration: Optional["HackDeclaration"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "HackDeclarationSpan":
    raise Exception("this function can only be called from @angle_query")



class HackSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], returns: ast.Expr, parameters: ast.Expr, contexts: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, returns, 'returns'), angle_for(__env, parameters, 'parameters'), angle_for(__env, contexts, 'contexts')]))
    return f"hack.Signature.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Signature

  @staticmethod
  def angle_query(*, returns: Optional[Union[Just["HackType"], Just[None]]] = None, parameters: Optional[List["HackParameter"]] = None, contexts: Optional[Union[Just[List["HackContext_"]], Just[None]]] = None) -> "HackSignature":
    raise Exception("this function can only be called from @angle_query")



class HackContainerChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], container: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, container, 'container'), angle_for(__env, child, 'child')]))
    return f"hack.ContainerChild.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContainerChild

  @staticmethod
  def angle_query(*, container: Optional["HackContainerDeclaration"] = None, child: Optional["HackContainerDeclaration"] = None) -> "HackContainerChild":
    raise Exception("this function can only be called from @angle_query")



class HackNamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent')]))
    return f"hack.NamespaceQName.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", NamespaceQName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, parent: Optional[Union[Just["HackNamespaceQName"], Just[None]]] = None) -> "HackNamespaceQName":
    raise Exception("this function can only be called from @angle_query")



class HackInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.InterfaceDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationSource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')]))
    return f"hack.DeclarationSource.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationSource

  @staticmethod
  def angle_query(*, target: Optional["HackDeclaration"] = None, source: Optional["HackDeclaration"] = None) -> "HackDeclarationSource":
    raise Exception("this function can only be called from @angle_query")



class HackUserAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameters: ast.Expr, qname: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parameters, 'parameters'), angle_for(__env, qname, 'qname')]))
    return f"hack.UserAttribute.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", UserAttribute

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, parameters: Optional[List[str]] = None, qname: Optional[Union[Just["HackQName"], Just[None]]] = None) -> "HackUserAttribute":
    raise Exception("this function can only be called from @angle_query")



class HackModuleDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.ModuleDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ModuleDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None) -> "HackModuleDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackPropertyDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, visibility: ast.Expr, isFinal: ast.Expr, isAbstract: ast.Expr, isStatic: ast.Expr, attributes: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type'), angle_for(__env, visibility, 'visibility'), angle_for(__env, isFinal, 'isFinal'), angle_for(__env, isAbstract, 'isAbstract'), angle_for(__env, isStatic, 'isStatic'), angle_for(__env, attributes, 'attributes')]))
    return f"hack.PropertyDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PropertyDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackPropertyDeclaration"] = None, type: Optional[Union[Just["HackType"], Just[None]]] = None, visibility: Optional["HackVisibility"] = None, isFinal: Optional[bool] = None, isAbstract: Optional[bool] = None, isStatic: Optional[bool] = None, attributes: Optional[List["HackUserAttribute"]] = None) -> "HackPropertyDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackClassConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container')]))
    return f"hack.ClassConstDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ClassConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, container: Optional["HackContainerDeclaration"] = None) -> "HackClassConstDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.EnumDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"hack.DeclarationComment.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationComment

  @staticmethod
  def angle_query(*, declaration: Optional["HackDeclaration"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "HackDeclarationComment":
    raise Exception("this function can only be called from @angle_query")



class HackNamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.NamespaceDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackNamespaceQName"] = None) -> "HackNamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, isAbstract: ast.Expr, isFinal: ast.Expr, members: ast.Expr, extends_: ast.Expr, implements_: ast.Expr, uses: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, isAbstract, 'isAbstract'), angle_for(__env, isFinal, 'isFinal'), angle_for(__env, members, 'members'), angle_for(__env, extends_, 'extends_'), angle_for(__env, implements_, 'implements_'), angle_for(__env, uses, 'uses'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, module_, 'module_')]))
    return f"hack.ClassDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackClassDeclaration"] = None, isAbstract: Optional[bool] = None, isFinal: Optional[bool] = None, members: Optional[List["HackDeclaration"]] = None, extends_: Optional[Union[Just["HackClassDeclaration"], Just[None]]] = None, implements_: Optional[List["HackInterfaceDeclaration"]] = None, uses: Optional[List["HackTraitDeclaration"]] = None, attributes: Optional[List["HackUserAttribute"]] = None, typeParams: Optional[List["HackTypeParameter"]] = None, module_: Optional[Union[Just["HackModuleMembership"], Just[None]]] = None) -> "HackClassDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackMethodOccurrence(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, className: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, className, 'className')]))
    return f"hack.MethodOccurrence.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", MethodOccurrence

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, className: Optional[Union[Just["HackName"], Just[None]]] = None) -> "HackMethodOccurrence":
    raise Exception("this function can only be called from @angle_query")



class HackMethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], derived: ast.Expr, base: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, derived, 'derived'), angle_for(__env, base, 'base')]))
    return f"hack.MethodOverrides.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", MethodOverrides

  @staticmethod
  def angle_query(*, derived: Optional["HackMethodDeclaration"] = None, base: Optional["HackMethodDeclaration"] = None) -> "HackMethodOverrides":
    raise Exception("this function can only be called from @angle_query")



class HackType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"hack.Type.6 { query_fields if query_fields else '_' }", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackType":
    raise Exception("this function can only be called from @angle_query")



class HackNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowercase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowercase, 'nameLowercase'), angle_for(__env, name, 'name')]))
    return f"hack.NameLowerCase.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowercase: Optional[str] = None, name: Optional["HackName"] = None) -> "HackNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class HackTypeConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, kind: ast.Expr, attributes: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type'), angle_for(__env, kind, 'kind'), angle_for(__env, attributes, 'attributes')]))
    return f"hack.TypeConstDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TypeConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackTypeConstDeclaration"] = None, type: Optional[Union[Just["HackType"], Just[None]]] = None, kind: Optional["HackTypeConstKind"] = None, attributes: Optional[List["HackUserAttribute"]] = None) -> "HackTypeConstDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, target, 'target')]))
    return f"hack.DeclarationTarget.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationTarget

  @staticmethod
  def angle_query(*, source: Optional["HackDeclaration"] = None, target: Optional["HackDeclaration"] = None) -> "HackDeclarationTarget":
    raise Exception("this function can only be called from @angle_query")



class HackSymbol(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name_lowercase: ast.Expr, valid: ast.Expr, kind_id: ast.Expr, ns_id: ast.Expr, filehash_id: ast.Expr, is_abstract: ast.Expr, is_final: ast.Expr, canonical_name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name_lowercase, 'name_lowercase'), angle_for(__env, valid, 'valid'), angle_for(__env, kind_id, 'kind_id'), angle_for(__env, ns_id, 'ns_id'), angle_for(__env, filehash_id, 'filehash_id'), angle_for(__env, is_abstract, 'is_abstract'), angle_for(__env, is_final, 'is_final'), angle_for(__env, canonical_name, 'canonical_name')]))
    return f"hack.symbol.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", symbol

  @staticmethod
  def angle_query(*, name_lowercase: Optional[str] = None, valid: Optional["Hackcontext"] = None, kind_id: Optional[int] = None, ns_id: Optional[int] = None, filehash_id: Optional[str] = None, is_abstract: Optional[bool] = None, is_final: Optional[bool] = None, canonical_name: Optional[str] = None) -> "HackSymbol":
    raise Exception("this function can only be called from @angle_query")



class HackEnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, enumBase: ast.Expr, enumConstraint: ast.Expr, enumerators: ast.Expr, attributes: ast.Expr, includes: ast.Expr, isEnumClass: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, enumBase, 'enumBase'), angle_for(__env, enumConstraint, 'enumConstraint'), angle_for(__env, enumerators, 'enumerators'), angle_for(__env, attributes, 'attributes'), angle_for(__env, includes, 'includes'), angle_for(__env, isEnumClass, 'isEnumClass'), angle_for(__env, module_, 'module_')]))
    return f"hack.EnumDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EnumDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackEnumDeclaration"] = None, enumBase: Optional["HackType"] = None, enumConstraint: Optional[Union[Just["HackType"], Just[None]]] = None, enumerators: Optional[List["HackEnumerator"]] = None, attributes: Optional[List["HackUserAttribute"]] = None, includes: Optional[List["HackEnumDeclaration"]] = None, isEnumClass: Optional[bool] = None, module_: Optional[Union[Just["HackModuleMembership"], Just[None]]] = None) -> "HackEnumDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackClassConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type'), angle_for(__env, value, 'value')]))
    return f"hack.ClassConstDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ClassConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackClassConstDeclaration"] = None, type: Optional[Union[Just["HackType"], Just[None]]] = None, value: Optional[Union[Just[str], Just[None]]] = None) -> "HackClassConstDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackStringLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"hack.StringLiteral.6 { query_fields if query_fields else '_' }", StringLiteral

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackStringLiteral":
    raise Exception("this function can only be called from @angle_query")



class HackGlobalConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"hack.GlobalConstDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", GlobalConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackGlobalConstDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, signature: ast.Expr, isAsync: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, signature, 'signature'), angle_for(__env, isAsync, 'isAsync'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, module_, 'module_')]))
    return f"hack.FunctionDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackFunctionDeclaration"] = None, signature: Optional["HackSignature"] = None, isAsync: Optional[bool] = None, attributes: Optional[List["HackUserAttribute"]] = None, typeParams: Optional[List["HackTypeParameter"]] = None, module_: Optional[Union[Just["HackModuleMembership"], Just[None]]] = None) -> "HackFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackTraitDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, members: ast.Expr, implements_: ast.Expr, uses: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, requireExtends: ast.Expr, requireImplements: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, members, 'members'), angle_for(__env, implements_, 'implements_'), angle_for(__env, uses, 'uses'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, requireExtends, 'requireExtends'), angle_for(__env, requireImplements, 'requireImplements'), angle_for(__env, module_, 'module_')]))
    return f"hack.TraitDefinition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TraitDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackTraitDeclaration"] = None, members: Optional[List["HackDeclaration"]] = None, implements_: Optional[List["HackInterfaceDeclaration"]] = None, uses: Optional[List["HackTraitDeclaration"]] = None, attributes: Optional[List["HackUserAttribute"]] = None, typeParams: Optional[List["HackTypeParameter"]] = None, requireExtends: Optional[List["HackClassDeclaration"]] = None, requireImplements: Optional[List["HackInterfaceDeclaration"]] = None, module_: Optional[Union[Just["HackModuleMembership"], Just[None]]] = None) -> "HackTraitDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"hack.DeclarationName.6 { query_fields if query_fields else '_' }", DeclarationName

  @staticmethod
  def angle_query(*, arg: Optional["HackDeclaration"] = None) -> "HackDeclarationName":
    raise Exception("this function can only be called from @angle_query")



class HackPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container')]))
    return f"hack.PropertyDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PropertyDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, container: Optional["HackContainerDeclaration"] = None) -> "HackPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackFileCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, callee_span: ast.Expr, call_args: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, callee_span, 'callee_span'), angle_for(__env, call_args, 'call_args')]))
    return f"hack.FileCall.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileCall

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, callee_span: Optional["SrcByteSpan"] = None, call_args: Optional[List["HackCallArgument"]] = None) -> "HackFileCall":
    raise Exception("this function can only be called from @angle_query")



class HackFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, declarations: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, declarations, 'declarations')]))
    return f"hack.FileDeclarations.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileDeclarations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, declarations: Optional[List["HackDeclaration"]] = None) -> "HackFileDeclarations":
    raise Exception("this function can only be called from @angle_query")



class HackTypeConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container')]))
    return f"hack.TypeConstDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TypeConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, container: Optional["HackContainerDeclaration"] = None) -> "HackTypeConstDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"hack.DeclarationLocation.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional["HackDeclaration"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "HackDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")





class HackArgument(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lit: ast.Expr, xref: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, lit, 'lit'), angle_for(__env, xref, 'xref')]))
    return f"hack.Argument.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Argument

  @staticmethod
  def angle_query_lit(*, lit: "HackStringLiteral") -> "HackArgument":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_xref(*, xref: "HackXRefTarget") -> "HackArgument":
    raise Exception("this function can only be called from @angle_query")




class HackContext(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], acid: ast.Expr, actype: ast.Expr, acnew: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, acid, 'acid'), angle_for(__env, actype, 'actype'), angle_for(__env, acnew, 'acnew')]))
    return f"hack.context.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", context

  @staticmethod
  def angle_query(*, acid: Optional[bool] = None, actype: Optional[bool] = None, acnew: Optional[bool] = None) -> "HackContext":
    raise Exception("this function can only be called from @angle_query")



class HackTypeParameter(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, variance: ast.Expr, reifyKind: ast.Expr, constraints: ast.Expr, attributes: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, variance, 'variance'), angle_for(__env, reifyKind, 'reifyKind'), angle_for(__env, constraints, 'constraints'), angle_for(__env, attributes, 'attributes')]))
    return f"hack.TypeParameter.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TypeParameter

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, variance: Optional["HackVariance"] = None, reifyKind: Optional["HackReifyKind"] = None, constraints: Optional[List["HackConstraint"]] = None, attributes: Optional[List["HackUserAttribute"]] = None) -> "HackTypeParameter":
    raise Exception("this function can only be called from @angle_query")



class HackDefinition(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr, classConst: ast.Expr, enum_: ast.Expr, function_: ast.Expr, globalConst: ast.Expr, interface_: ast.Expr, trait: ast.Expr, method: ast.Expr, property_: ast.Expr, typeConst: ast.Expr, typedef_: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_'), angle_for(__env, classConst, 'classConst'), angle_for(__env, enum_, 'enum_'), angle_for(__env, function_, 'function_'), angle_for(__env, globalConst, 'globalConst'), angle_for(__env, interface_, 'interface_'), angle_for(__env, trait, 'trait'), angle_for(__env, method, 'method'), angle_for(__env, property_, 'property_'), angle_for(__env, typeConst, 'typeConst'), angle_for(__env, typedef_, 'typedef_'), angle_for(__env, module, 'module')]))
    return f"hack.Definition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Definition

  @staticmethod
  def angle_query_class_(*, class_: "HackClassDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_classConst(*, classConst: "HackClassConstDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enum_(*, enum_: "HackEnumDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_function_(*, function_: "HackFunctionDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_globalConst(*, globalConst: "HackGlobalConstDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_interface_(*, interface_: "HackInterfaceDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_trait(*, trait: "HackTraitDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_method(*, method: "HackMethodDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_property_(*, property_: "HackPropertyDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_typeConst(*, typeConst: "HackTypeConstDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_typedef_(*, typedef_: "HackTypedefDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_module(*, module: "HackModuleDefinition") -> "HackDefinition":
    raise Exception("this function can only be called from @angle_query")




class HackDeclaration(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], classConst: ast.Expr, container: ast.Expr, enumerator: ast.Expr, function_: ast.Expr, globalConst: ast.Expr, namespace_: ast.Expr, method: ast.Expr, property_: ast.Expr, typeConst: ast.Expr, typedef_: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, classConst, 'classConst'), angle_for(__env, container, 'container'), angle_for(__env, enumerator, 'enumerator'), angle_for(__env, function_, 'function_'), angle_for(__env, globalConst, 'globalConst'), angle_for(__env, namespace_, 'namespace_'), angle_for(__env, method, 'method'), angle_for(__env, property_, 'property_'), angle_for(__env, typeConst, 'typeConst'), angle_for(__env, typedef_, 'typedef_'), angle_for(__env, module, 'module')]))
    return f"hack.Declaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Declaration

  @staticmethod
  def angle_query_classConst(*, classConst: "HackClassConstDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_container(*, container: "HackContainerDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enumerator(*, enumerator: "HackEnumerator") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_function_(*, function_: "HackFunctionDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_globalConst(*, globalConst: "HackGlobalConstDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_namespace_(*, namespace_: "HackNamespaceDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_method(*, method: "HackMethodDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_property_(*, property_: "HackPropertyDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_typeConst(*, typeConst: "HackTypeConstDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_typedef_(*, typedef_: "HackTypedefDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_module(*, module: "HackModuleDeclaration") -> "HackDeclaration":
    raise Exception("this function can only be called from @angle_query")




class HackXRefTarget(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, occurrence: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, occurrence, 'occurrence')]))
    return f"hack.XRefTarget.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRefTarget

  @staticmethod
  def angle_query_declaration(*, declaration: "HackDeclaration") -> "HackXRefTarget":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_occurrence(*, occurrence: "HackOccurrence") -> "HackXRefTarget":
    raise Exception("this function can only be called from @angle_query")




class HackModuleMembership(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, internal: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, internal, 'internal')]))
    return f"hack.ModuleMembership.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ModuleMembership

  @staticmethod
  def angle_query(*, declaration: Optional["HackModuleDeclaration"] = None, internal: Optional[bool] = None) -> "HackModuleMembership":
    raise Exception("this function can only be called from @angle_query")



class HackReifyKind(Enum):
  Erased = 0
  Reified = 1
  SoftReified = 2

class HackXRef(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, ranges: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, ranges, 'ranges')]))
    return f"hack.XRef.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRef

  @staticmethod
  def angle_query(*, target: Optional["HackXRefTarget"] = None, ranges: Optional["SrcByteSpans"] = None) -> "HackXRef":
    raise Exception("this function can only be called from @angle_query")



class HackParameter(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, isInout: ast.Expr, isVariadic: ast.Expr, defaultValue: ast.Expr, attributes: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, isInout, 'isInout'), angle_for(__env, isVariadic, 'isVariadic'), angle_for(__env, defaultValue, 'defaultValue'), angle_for(__env, attributes, 'attributes')]))
    return f"hack.Parameter.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Parameter

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, type: Optional[Union[Just["HackType"], Just[None]]] = None, isInout: Optional[bool] = None, isVariadic: Optional[bool] = None, defaultValue: Optional[Union[Just[str], Just[None]]] = None, attributes: Optional[List["HackUserAttribute"]] = None) -> "HackParameter":
    raise Exception("this function can only be called from @angle_query")



class HackCallArgument(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], span: ast.Expr, argument: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, span, 'span'), angle_for(__env, argument, 'argument')]))
    return f"hack.CallArgument.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CallArgument

  @staticmethod
  def angle_query(*, span: Optional["SrcRelByteSpan"] = None, argument: Optional[Union[Just["HackArgument"], Just[None]]] = None) -> "HackCallArgument":
    raise Exception("this function can only be called from @angle_query")



class HackVisibility(Enum):
  Private = 0
  Protected = 1
  Public = 2
  Internal = 3

class HackOccurrence(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], method: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, method, 'method')]))
    return f"hack.Occurrence.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Occurrence

  @staticmethod
  def angle_query_method(*, method: "HackMethodOccurrence") -> "HackOccurrence":
    raise Exception("this function can only be called from @angle_query")




class HackContainerDeclaration(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr, enum_: ast.Expr, interface_: ast.Expr, trait: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_'), angle_for(__env, enum_, 'enum_'), angle_for(__env, interface_, 'interface_'), angle_for(__env, trait, 'trait')]))
    return f"hack.ContainerDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContainerDeclaration

  @staticmethod
  def angle_query_class_(*, class_: "HackClassDeclaration") -> "HackContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enum_(*, enum_: "HackEnumDeclaration") -> "HackContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_interface_(*, interface_: "HackInterfaceDeclaration") -> "HackContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_trait(*, trait: "HackTraitDeclaration") -> "HackContainerDeclaration":
    raise Exception("this function can only be called from @angle_query")




class HackTypeConstKind(Enum):
  Abstract = 0
  Concrete = 1
  PartiallyAbstract = 2

class HackConstraint(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], constraintKind: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, constraintKind, 'constraintKind'), angle_for(__env, type, 'type')]))
    return f"hack.Constraint.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Constraint

  @staticmethod
  def angle_query(*, constraintKind: Optional["HackConstraintKind"] = None, type: Optional["HackType"] = None) -> "HackConstraint":
    raise Exception("this function can only be called from @angle_query")



class HackVariance(Enum):
  Contravariant = 0
  Covariant = 1
  Invariant = 2

class HackConstraintKind(Enum):
  As = 0
  Equal = 1
  Super = 2


