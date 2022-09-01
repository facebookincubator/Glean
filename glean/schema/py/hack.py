# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
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
)


class HackMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, signature: ast.Expr, visibility: ast.Expr, isAbstract: ast.Expr, isAsync: ast.Expr, isFinal: ast.Expr, isStatic: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.MethodDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, signature, 'signature'), angle_for(__env, visibility, 'visibility'), angle_for(__env, isAbstract, 'isAbstract'), angle_for(__env, isAsync, 'isAsync'), angle_for(__env, isFinal, 'isFinal'), angle_for(__env, isStatic, 'isStatic'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams')])) or '_' } }}", MethodDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackMethodDeclaration"] = None, signature: Optional["HackSignature"] = None, visibility: Optional[Tuple[()]] = None, isAbstract: Optional[bool] = None, isAsync: Optional[bool] = None, isFinal: Optional[bool] = None, isStatic: Optional[bool] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None) -> "HackMethodDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackSymbolNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], namespace_id: ast.Expr, namespace_name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.symbolNamespace.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, namespace_id, 'namespace_id'), angle_for(__env, namespace_name, 'namespace_name')])) or '_' } }}", symbolNamespace

  @staticmethod
  def angle_query(*, namespace_id: Optional[int] = None, namespace_name: Optional[str] = None) -> "HackSymbolNamespace":
    raise Exception("this function can only be called from @angle_query")



class HackTraitDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.TraitDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", TraitDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackTraitDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.FunctionDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackTypedefDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, isTransparent: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.TypedefDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, isTransparent, 'isTransparent'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, module_, 'module_')])) or '_' } }}", TypedefDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackTypedefDeclaration"] = None, isTransparent: Optional[bool] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackTypedefDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackAttributeToDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], attribute: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.AttributeToDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, attribute, 'attribute'), angle_for(__env, definition, 'definition')])) or '_' } }}", AttributeToDefinition

  @staticmethod
  def angle_query(*, attribute: Optional["HackUserAttribute"] = None, definition: Optional[Tuple[()]] = None) -> "HackAttributeToDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackNamespaceMember(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], namespace_: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.NamespaceMember.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, namespace_, 'namespace_'), angle_for(__env, decl, 'decl')])) or '_' } }}", NamespaceMember

  @staticmethod
  def angle_query(*, namespace_: Optional["HackNamespaceQName"] = None, decl: Optional[Tuple[()]] = None) -> "HackNamespaceMember":
    raise Exception("this function can only be called from @angle_query")



class HackGlobalConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.GlobalConstDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type'), angle_for(__env, value, 'value')])) or '_' } }}", GlobalConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackGlobalConstDeclaration"] = None, type: Optional[Tuple[()]] = None, value: Optional[str] = None) -> "HackGlobalConstDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackContainerParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], container: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ContainerParent.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, container, 'container'), angle_for(__env, parent, 'parent')])) or '_' } }}", ContainerParent

  @staticmethod
  def angle_query(*, container: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "HackContainerParent":
    raise Exception("this function can only be called from @angle_query")



class HackInterfaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, members: ast.Expr, extends_: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, requireExtends: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.InterfaceDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, members, 'members'), angle_for(__env, extends_, 'extends_'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, requireExtends, 'requireExtends'), angle_for(__env, module_, 'module_')])) or '_' } }}", InterfaceDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackInterfaceDeclaration"] = None, members: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, requireExtends: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackInterfaceDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackContext_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.Context_.6 { angle_for(__env, arg, None) or '_' }", Context_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackContext_":
    raise Exception("this function can only be called from @angle_query")



class HackContainerDeclarationQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ContainerDeclarationQName.6 { angle_for(__env, arg, None) or '_' }", ContainerDeclarationQName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HackContainerDeclarationQName":
    raise Exception("this function can only be called from @angle_query")



class HackTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.TargetUses.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, uses, 'uses')])) or '_' } }}", TargetUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, uses: Optional[Tuple[()]] = None) -> "HackTargetUses":
    raise Exception("this function can only be called from @angle_query")



class HackTargetUsesAbs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.TargetUsesAbs.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, uses, 'uses')])) or '_' } }}", TargetUsesAbs

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, uses: Optional[Tuple[()]] = None) -> "HackTargetUsesAbs":
    raise Exception("this function can only be called from @angle_query")



class HackMethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, derived: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.MethodOverridden.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, derived, 'derived')])) or '_' } }}", MethodOverridden

  @staticmethod
  def angle_query(*, base: Optional["HackMethodDeclaration"] = None, derived: Optional["HackMethodDeclaration"] = None) -> "HackMethodOverridden":
    raise Exception("this function can only be called from @angle_query")



class HackClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ClassDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackClassDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, namespace_: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.QName.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, namespace_, 'namespace_')])) or '_' } }}", QName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, namespace_: Optional[Tuple[()]] = None) -> "HackQName":
    raise Exception("this function can only be called from @angle_query")



class HackFilename(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], filename: ast.Expr, filehash_id: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.filename.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, filename, 'filename'), angle_for(__env, filehash_id, 'filehash_id')])) or '_' } }}", filename

  @staticmethod
  def angle_query(*, filename: Optional[str] = None, filehash_id: Optional[str] = None) -> "HackFilename":
    raise Exception("this function can only be called from @angle_query")



class HackKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], id: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.kind.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, id, 'id'), angle_for(__env, name, 'name')])) or '_' } }}", kind

  @staticmethod
  def angle_query(*, id: Optional[int] = None, name: Optional[str] = None) -> "HackKind":
    raise Exception("this function can only be called from @angle_query")



class HackTypedefDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.TypedefDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", TypedefDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackTypedefDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, attributes: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ModuleDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, attributes, 'attributes')])) or '_' } }}", ModuleDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackModuleDeclaration"] = None, attributes: Optional[Tuple[()]] = None) -> "HackModuleDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackAttributeHasParameter(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameter: ast.Expr, attribute: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.AttributeHasParameter.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parameter, 'parameter'), angle_for(__env, attribute, 'attribute')])) or '_' } }}", AttributeHasParameter

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, parameter: Optional[str] = None, attribute: Optional["HackUserAttribute"] = None) -> "HackAttributeHasParameter":
    raise Exception("this function can only be called from @angle_query")



class HackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.Name.6 { angle_for(__env, arg, None) or '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackName":
    raise Exception("this function can only be called from @angle_query")



class HackMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.MethodDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container')])) or '_' } }}", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, container: Optional[Tuple[()]] = None) -> "HackMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.FileXRefs.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')])) or '_' } }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[Tuple[()]] = None) -> "HackFileXRefs":
    raise Exception("this function can only be called from @angle_query")



class HackEnumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, enumeration: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.Enumerator.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, enumeration, 'enumeration')])) or '_' } }}", Enumerator

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, enumeration: Optional["HackEnumDeclaration"] = None) -> "HackEnumerator":
    raise Exception("this function can only be called from @angle_query")



class HackIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.identifier.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind'), angle_for(__env, name, 'name')])) or '_' } }}", identifier

  @staticmethod
  def angle_query(*, kind: Optional[int] = None, name: Optional[str] = None) -> "HackIdentifier":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.DeclarationSpan.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationSpan

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "HackDeclarationSpan":
    raise Exception("this function can only be called from @angle_query")



class HackSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], returns: ast.Expr, parameters: ast.Expr, contexts: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.Signature.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, returns, 'returns'), angle_for(__env, parameters, 'parameters'), angle_for(__env, contexts, 'contexts')])) or '_' } }}", Signature

  @staticmethod
  def angle_query(*, returns: Optional[Tuple[()]] = None, parameters: Optional[Tuple[()]] = None, contexts: Optional[Tuple[()]] = None) -> "HackSignature":
    raise Exception("this function can only be called from @angle_query")



class HackContainerChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], container: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ContainerChild.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, container, 'container'), angle_for(__env, child, 'child')])) or '_' } }}", ContainerChild

  @staticmethod
  def angle_query(*, container: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "HackContainerChild":
    raise Exception("this function can only be called from @angle_query")



class HackNamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.NamespaceQName.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent')])) or '_' } }}", NamespaceQName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, parent: Optional[Tuple[()]] = None) -> "HackNamespaceQName":
    raise Exception("this function can only be called from @angle_query")



class HackInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.InterfaceDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationSource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.DeclarationSource.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')])) or '_' } }}", DeclarationSource

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HackDeclarationSource":
    raise Exception("this function can only be called from @angle_query")



class HackUserAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameters: ast.Expr, qname: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.UserAttribute.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parameters, 'parameters'), angle_for(__env, qname, 'qname')])) or '_' } }}", UserAttribute

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, parameters: Optional[Tuple[()]] = None, qname: Optional[Tuple[()]] = None) -> "HackUserAttribute":
    raise Exception("this function can only be called from @angle_query")



class HackModuleDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ModuleDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", ModuleDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None) -> "HackModuleDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackPropertyDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, visibility: ast.Expr, isFinal: ast.Expr, isAbstract: ast.Expr, isStatic: ast.Expr, attributes: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.PropertyDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type'), angle_for(__env, visibility, 'visibility'), angle_for(__env, isFinal, 'isFinal'), angle_for(__env, isAbstract, 'isAbstract'), angle_for(__env, isStatic, 'isStatic'), angle_for(__env, attributes, 'attributes')])) or '_' } }}", PropertyDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackPropertyDeclaration"] = None, type: Optional[Tuple[()]] = None, visibility: Optional[Tuple[()]] = None, isFinal: Optional[bool] = None, isAbstract: Optional[bool] = None, isStatic: Optional[bool] = None, attributes: Optional[Tuple[()]] = None) -> "HackPropertyDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackClassConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ClassConstDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container')])) or '_' } }}", ClassConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, container: Optional[Tuple[()]] = None) -> "HackClassConstDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.EnumDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.DeclarationComment.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationComment

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "HackDeclarationComment":
    raise Exception("this function can only be called from @angle_query")



class HackNamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.NamespaceDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackNamespaceQName"] = None) -> "HackNamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, isAbstract: ast.Expr, isFinal: ast.Expr, members: ast.Expr, extends_: ast.Expr, implements_: ast.Expr, uses: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ClassDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, isAbstract, 'isAbstract'), angle_for(__env, isFinal, 'isFinal'), angle_for(__env, members, 'members'), angle_for(__env, extends_, 'extends_'), angle_for(__env, implements_, 'implements_'), angle_for(__env, uses, 'uses'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, module_, 'module_')])) or '_' } }}", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackClassDeclaration"] = None, isAbstract: Optional[bool] = None, isFinal: Optional[bool] = None, members: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackClassDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackMethodOccurrence(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, className: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.MethodOccurrence.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, className, 'className')])) or '_' } }}", MethodOccurrence

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, className: Optional[Tuple[()]] = None) -> "HackMethodOccurrence":
    raise Exception("this function can only be called from @angle_query")



class HackMethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], derived: ast.Expr, base: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.MethodOverrides.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, derived, 'derived'), angle_for(__env, base, 'base')])) or '_' } }}", MethodOverrides

  @staticmethod
  def angle_query(*, derived: Optional["HackMethodDeclaration"] = None, base: Optional["HackMethodDeclaration"] = None) -> "HackMethodOverrides":
    raise Exception("this function can only be called from @angle_query")



class HackType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.Type.6 { angle_for(__env, arg, None) or '_' }", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackType":
    raise Exception("this function can only be called from @angle_query")



class HackNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowercase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.NameLowerCase.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowercase, 'nameLowercase'), angle_for(__env, name, 'name')])) or '_' } }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowercase: Optional[str] = None, name: Optional["HackName"] = None) -> "HackNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class HackTypeConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, kind: ast.Expr, attributes: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.TypeConstDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type'), angle_for(__env, kind, 'kind'), angle_for(__env, attributes, 'attributes')])) or '_' } }}", TypeConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackTypeConstDeclaration"] = None, type: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None) -> "HackTypeConstDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.DeclarationTarget.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, target, 'target')])) or '_' } }}", DeclarationTarget

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "HackDeclarationTarget":
    raise Exception("this function can only be called from @angle_query")



class HackSymbol(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name_lowercase: ast.Expr, valid: ast.Expr, kind_id: ast.Expr, ns_id: ast.Expr, filehash_id: ast.Expr, is_abstract: ast.Expr, is_final: ast.Expr, canonical_name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.symbol.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name_lowercase, 'name_lowercase'), angle_for(__env, valid, 'valid'), angle_for(__env, kind_id, 'kind_id'), angle_for(__env, ns_id, 'ns_id'), angle_for(__env, filehash_id, 'filehash_id'), angle_for(__env, is_abstract, 'is_abstract'), angle_for(__env, is_final, 'is_final'), angle_for(__env, canonical_name, 'canonical_name')])) or '_' } }}", symbol

  @staticmethod
  def angle_query(*, name_lowercase: Optional[str] = None, valid: Optional[Tuple[()]] = None, kind_id: Optional[int] = None, ns_id: Optional[int] = None, filehash_id: Optional[str] = None, is_abstract: Optional[bool] = None, is_final: Optional[bool] = None, canonical_name: Optional[str] = None) -> "HackSymbol":
    raise Exception("this function can only be called from @angle_query")



class HackEnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, enumBase: ast.Expr, enumConstraint: ast.Expr, enumerators: ast.Expr, attributes: ast.Expr, includes: ast.Expr, isEnumClass: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.EnumDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, enumBase, 'enumBase'), angle_for(__env, enumConstraint, 'enumConstraint'), angle_for(__env, enumerators, 'enumerators'), angle_for(__env, attributes, 'attributes'), angle_for(__env, includes, 'includes'), angle_for(__env, isEnumClass, 'isEnumClass'), angle_for(__env, module_, 'module_')])) or '_' } }}", EnumDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackEnumDeclaration"] = None, enumBase: Optional["HackType"] = None, enumConstraint: Optional[Tuple[()]] = None, enumerators: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None, includes: Optional[Tuple[()]] = None, isEnumClass: Optional[bool] = None, module_: Optional[Tuple[()]] = None) -> "HackEnumDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackClassConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.ClassConstDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type'), angle_for(__env, value, 'value')])) or '_' } }}", ClassConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackClassConstDeclaration"] = None, type: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "HackClassConstDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackStringLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.StringLiteral.6 { angle_for(__env, arg, None) or '_' }", StringLiteral

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackStringLiteral":
    raise Exception("this function can only be called from @angle_query")



class HackGlobalConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.GlobalConstDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", GlobalConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackQName"] = None) -> "HackGlobalConstDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, signature: ast.Expr, isAsync: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.FunctionDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, signature, 'signature'), angle_for(__env, isAsync, 'isAsync'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, module_, 'module_')])) or '_' } }}", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackFunctionDeclaration"] = None, signature: Optional["HackSignature"] = None, isAsync: Optional[bool] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackTraitDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, members: ast.Expr, implements_: ast.Expr, uses: ast.Expr, attributes: ast.Expr, typeParams: ast.Expr, requireExtends: ast.Expr, requireImplements: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.TraitDefinition.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, members, 'members'), angle_for(__env, implements_, 'implements_'), angle_for(__env, uses, 'uses'), angle_for(__env, attributes, 'attributes'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, requireExtends, 'requireExtends'), angle_for(__env, requireImplements, 'requireImplements'), angle_for(__env, module_, 'module_')])) or '_' } }}", TraitDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["HackTraitDeclaration"] = None, members: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, requireExtends: Optional[Tuple[()]] = None, requireImplements: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackTraitDefinition":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.DeclarationName.6 { angle_for(__env, arg, None) or '_' }", DeclarationName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HackDeclarationName":
    raise Exception("this function can only be called from @angle_query")



class HackPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.PropertyDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container')])) or '_' } }}", PropertyDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, container: Optional[Tuple[()]] = None) -> "HackPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackFileCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, callee_span: ast.Expr, call_args: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.FileCall.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, callee_span, 'callee_span'), angle_for(__env, call_args, 'call_args')])) or '_' } }}", FileCall

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, callee_span: Optional[Tuple[()]] = None, call_args: Optional[Tuple[()]] = None) -> "HackFileCall":
    raise Exception("this function can only be called from @angle_query")



class HackFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, declarations: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.FileDeclarations.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, declarations, 'declarations')])) or '_' } }}", FileDeclarations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, declarations: Optional[Tuple[()]] = None) -> "HackFileDeclarations":
    raise Exception("this function can only be called from @angle_query")



class HackTypeConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.TypeConstDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, container, 'container')])) or '_' } }}", TypeConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, container: Optional[Tuple[()]] = None) -> "HackTypeConstDeclaration":
    raise Exception("this function can only be called from @angle_query")



class HackDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"hack.DeclarationLocation.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "HackDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")




