# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.hack.types import (
    MethodDefinition,
    SymbolNamespace,
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
    Filename,
    Kind,
    TypedefDeclaration,
    ModuleDefinition,
    AttributeHasParameter,
    Name,
    MethodDeclaration,
    FileXRefs,
    Enumerator,
    Identifier,
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
    Symbol,
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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.MethodDefinition.6 {{ }}", MethodDefinition
    return f"hack.MethodDefinition.6 { concatenateFields(key) }", MethodDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None, visibility: Optional[Tuple[()]] = None, isAbstract: Optional[bool] = None, isAsync: Optional[bool] = None, isFinal: Optional[bool] = None, isStatic: Optional[bool] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None) -> "HackMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackSymbolNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.symbolNamespace.1 {{ }}", SymbolNamespace
    return f"hack.symbolNamespace.1 { concatenateFields(key) }", SymbolNamespace

  @staticmethod
  def angle_query(*, namespace_id: Optional[int] = None, namespace_name: Optional[str] = None) -> "HackSymbolNamespace":
    raise Exception("this function can only be called from @angle_query")

class HackTraitDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.TraitDeclaration.6 {{ }}", TraitDeclaration
    return f"hack.TraitDeclaration.6 { concatenateFields(key) }", TraitDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackTraitDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.FunctionDeclaration.6 {{ }}", FunctionDeclaration
    return f"hack.FunctionDeclaration.6 { concatenateFields(key) }", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackTypedefDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.TypedefDefinition.6 {{ }}", TypedefDefinition
    return f"hack.TypedefDefinition.6 { concatenateFields(key) }", TypedefDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, isTransparent: Optional[bool] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackTypedefDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackAttributeToDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.AttributeToDefinition.6 {{ }}", AttributeToDefinition
    return f"hack.AttributeToDefinition.6 { concatenateFields(key) }", AttributeToDefinition

  @staticmethod
  def angle_query(*, attribute: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "HackAttributeToDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceMember(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.NamespaceMember.6 {{ }}", NamespaceMember
    return f"hack.NamespaceMember.6 { concatenateFields(key) }", NamespaceMember

  @staticmethod
  def angle_query(*, namespace_: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "HackNamespaceMember":
    raise Exception("this function can only be called from @angle_query")

class HackGlobalConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.GlobalConstDefinition.6 {{ }}", GlobalConstDefinition
    return f"hack.GlobalConstDefinition.6 { concatenateFields(key) }", GlobalConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, value: Optional[str] = None) -> "HackGlobalConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackContainerParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ContainerParent.6 {{ }}", ContainerParent
    return f"hack.ContainerParent.6 { concatenateFields(key) }", ContainerParent

  @staticmethod
  def angle_query(*, container: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "HackContainerParent":
    raise Exception("this function can only be called from @angle_query")

class HackInterfaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.InterfaceDefinition.6 {{ }}", InterfaceDefinition
    return f"hack.InterfaceDefinition.6 { concatenateFields(key) }", InterfaceDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, members: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, requireExtends: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackInterfaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackContext_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.Context_.6 {{ }}", Context_
    return f"hack.Context_.6 {key}", Context_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackContext_":
    raise Exception("this function can only be called from @angle_query")

class HackContainerDeclarationQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ContainerDeclarationQName.6 {{ }}", ContainerDeclarationQName
    return f"hack.ContainerDeclarationQName.6 {key}", ContainerDeclarationQName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HackContainerDeclarationQName":
    raise Exception("this function can only be called from @angle_query")

class HackTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.TargetUses.6 {{ }}", TargetUses
    return f"hack.TargetUses.6 { concatenateFields(key) }", TargetUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None) -> "HackTargetUses":
    raise Exception("this function can only be called from @angle_query")

class HackTargetUsesAbs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.TargetUsesAbs.6 {{ }}", TargetUsesAbs
    return f"hack.TargetUsesAbs.6 { concatenateFields(key) }", TargetUsesAbs

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None) -> "HackTargetUsesAbs":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.MethodOverridden.6 {{ }}", MethodOverridden
    return f"hack.MethodOverridden.6 { concatenateFields(key) }", MethodOverridden

  @staticmethod
  def angle_query(*, base: Optional[Tuple[()]] = None, derived: Optional[Tuple[()]] = None) -> "HackMethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class HackClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ClassDeclaration.6 {{ }}", ClassDeclaration
    return f"hack.ClassDeclaration.6 { concatenateFields(key) }", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.QName.6 {{ }}", QName
    return f"hack.QName.6 { concatenateFields(key) }", QName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, namespace_: Optional[Tuple[()]] = None) -> "HackQName":
    raise Exception("this function can only be called from @angle_query")

class HackFilename(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.filename.1 {{ }}", Filename
    return f"hack.filename.1 { concatenateFields(key) }", Filename

  @staticmethod
  def angle_query(*, filename: Optional[str] = None, filehash_id: Optional[str] = None) -> "HackFilename":
    raise Exception("this function can only be called from @angle_query")

class HackKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.kind.1 {{ }}", Kind
    return f"hack.kind.1 { concatenateFields(key) }", Kind

  @staticmethod
  def angle_query(*, id: Optional[int] = None, name: Optional[str] = None) -> "HackKind":
    raise Exception("this function can only be called from @angle_query")

class HackTypedefDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.TypedefDeclaration.6 {{ }}", TypedefDeclaration
    return f"hack.TypedefDeclaration.6 { concatenateFields(key) }", TypedefDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackTypedefDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ModuleDefinition.6 {{ }}", ModuleDefinition
    return f"hack.ModuleDefinition.6 { concatenateFields(key) }", ModuleDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None) -> "HackModuleDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackAttributeHasParameter(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.AttributeHasParameter.6 {{ }}", AttributeHasParameter
    return f"hack.AttributeHasParameter.6 { concatenateFields(key) }", AttributeHasParameter

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parameter: Optional[str] = None, attribute: Optional[Tuple[()]] = None) -> "HackAttributeHasParameter":
    raise Exception("this function can only be called from @angle_query")

class HackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.Name.6 {{ }}", Name
    return f"hack.Name.6 {key}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackName":
    raise Exception("this function can only be called from @angle_query")

class HackMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.MethodDeclaration.6 {{ }}", MethodDeclaration
    return f"hack.MethodDeclaration.6 { concatenateFields(key) }", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "HackMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.FileXRefs.6 {{ }}", FileXRefs
    return f"hack.FileXRefs.6 { concatenateFields(key) }", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "HackFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class HackEnumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.Enumerator.6 {{ }}", Enumerator
    return f"hack.Enumerator.6 { concatenateFields(key) }", Enumerator

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, enumeration: Optional[Tuple[()]] = None) -> "HackEnumerator":
    raise Exception("this function can only be called from @angle_query")

class HackIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.identifier.1 {{ }}", Identifier
    return f"hack.identifier.1 { concatenateFields(key) }", Identifier

  @staticmethod
  def angle_query(*, kind: Optional[int] = None, name: Optional[str] = None) -> "HackIdentifier":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.DeclarationSpan.6 {{ }}", DeclarationSpan
    return f"hack.DeclarationSpan.6 { concatenateFields(key) }", DeclarationSpan

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "HackDeclarationSpan":
    raise Exception("this function can only be called from @angle_query")

class HackSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.Signature.6 {{ }}", Signature
    return f"hack.Signature.6 { concatenateFields(key) }", Signature

  @staticmethod
  def angle_query(*, returns: Optional[Tuple[()]] = None, parameters: Optional[Tuple[()]] = None, contexts: Optional[Tuple[()]] = None) -> "HackSignature":
    raise Exception("this function can only be called from @angle_query")

class HackContainerChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ContainerChild.6 {{ }}", ContainerChild
    return f"hack.ContainerChild.6 { concatenateFields(key) }", ContainerChild

  @staticmethod
  def angle_query(*, container: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "HackContainerChild":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.NamespaceQName.6 {{ }}", NamespaceQName
    return f"hack.NamespaceQName.6 { concatenateFields(key) }", NamespaceQName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "HackNamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class HackInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.InterfaceDeclaration.6 {{ }}", InterfaceDeclaration
    return f"hack.InterfaceDeclaration.6 { concatenateFields(key) }", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationSource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.DeclarationSource.6 {{ }}", DeclarationSource
    return f"hack.DeclarationSource.6 { concatenateFields(key) }", DeclarationSource

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HackDeclarationSource":
    raise Exception("this function can only be called from @angle_query")

class HackUserAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.UserAttribute.6 {{ }}", UserAttribute
    return f"hack.UserAttribute.6 { concatenateFields(key) }", UserAttribute

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parameters: Optional[Tuple[()]] = None, qname: Optional[Tuple[()]] = None) -> "HackUserAttribute":
    raise Exception("this function can only be called from @angle_query")

class HackModuleDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ModuleDeclaration.6 {{ }}", ModuleDeclaration
    return f"hack.ModuleDeclaration.6 { concatenateFields(key) }", ModuleDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackModuleDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackPropertyDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.PropertyDefinition.6 {{ }}", PropertyDefinition
    return f"hack.PropertyDefinition.6 { concatenateFields(key) }", PropertyDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, visibility: Optional[Tuple[()]] = None, isFinal: Optional[bool] = None, isAbstract: Optional[bool] = None, isStatic: Optional[bool] = None, attributes: Optional[Tuple[()]] = None) -> "HackPropertyDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackClassConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ClassConstDeclaration.6 {{ }}", ClassConstDeclaration
    return f"hack.ClassConstDeclaration.6 { concatenateFields(key) }", ClassConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "HackClassConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.EnumDeclaration.6 {{ }}", EnumDeclaration
    return f"hack.EnumDeclaration.6 { concatenateFields(key) }", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.DeclarationComment.6 {{ }}", DeclarationComment
    return f"hack.DeclarationComment.6 { concatenateFields(key) }", DeclarationComment

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "HackDeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.NamespaceDeclaration.6 {{ }}", NamespaceDeclaration
    return f"hack.NamespaceDeclaration.6 { concatenateFields(key) }", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackNamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ClassDefinition.6 {{ }}", ClassDefinition
    return f"hack.ClassDefinition.6 { concatenateFields(key) }", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, isAbstract: Optional[bool] = None, isFinal: Optional[bool] = None, members: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackClassDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOccurrence(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.MethodOccurrence.6 {{ }}", MethodOccurrence
    return f"hack.MethodOccurrence.6 { concatenateFields(key) }", MethodOccurrence

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, className: Optional[Tuple[()]] = None) -> "HackMethodOccurrence":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.MethodOverrides.6 {{ }}", MethodOverrides
    return f"hack.MethodOverrides.6 { concatenateFields(key) }", MethodOverrides

  @staticmethod
  def angle_query(*, derived: Optional[Tuple[()]] = None, base: Optional[Tuple[()]] = None) -> "HackMethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class HackType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.Type.6 {{ }}", Type
    return f"hack.Type.6 {key}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackType":
    raise Exception("this function can only be called from @angle_query")

class HackNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.NameLowerCase.6 {{ }}", NameLowerCase
    return f"hack.NameLowerCase.6 { concatenateFields(key) }", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowercase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "HackNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HackTypeConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.TypeConstDefinition.6 {{ }}", TypeConstDefinition
    return f"hack.TypeConstDefinition.6 { concatenateFields(key) }", TypeConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None) -> "HackTypeConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.DeclarationTarget.6 {{ }}", DeclarationTarget
    return f"hack.DeclarationTarget.6 { concatenateFields(key) }", DeclarationTarget

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "HackDeclarationTarget":
    raise Exception("this function can only be called from @angle_query")

class HackSymbol(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.symbol.1 {{ }}", Symbol
    return f"hack.symbol.1 { concatenateFields(key) }", Symbol

  @staticmethod
  def angle_query(*, name_lowercase: Optional[str] = None, valid: Optional[Tuple[()]] = None, kind_id: Optional[int] = None, ns_id: Optional[int] = None, filehash_id: Optional[str] = None, is_abstract: Optional[bool] = None, is_final: Optional[bool] = None, canonical_name: Optional[str] = None) -> "HackSymbol":
    raise Exception("this function can only be called from @angle_query")

class HackEnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.EnumDefinition.6 {{ }}", EnumDefinition
    return f"hack.EnumDefinition.6 { concatenateFields(key) }", EnumDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, enumBase: Optional[Tuple[()]] = None, enumConstraint: Optional[Tuple[()]] = None, enumerators: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None, includes: Optional[Tuple[()]] = None, isEnumClass: Optional[bool] = None, module_: Optional[Tuple[()]] = None) -> "HackEnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackClassConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.ClassConstDefinition.6 {{ }}", ClassConstDefinition
    return f"hack.ClassConstDefinition.6 { concatenateFields(key) }", ClassConstDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "HackClassConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackStringLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.StringLiteral.6 {{ }}", StringLiteral
    return f"hack.StringLiteral.6 {key}", StringLiteral

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackStringLiteral":
    raise Exception("this function can only be called from @angle_query")

class HackGlobalConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.GlobalConstDeclaration.6 {{ }}", GlobalConstDeclaration
    return f"hack.GlobalConstDeclaration.6 { concatenateFields(key) }", GlobalConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "HackGlobalConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.FunctionDefinition.6 {{ }}", FunctionDefinition
    return f"hack.FunctionDefinition.6 { concatenateFields(key) }", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None, isAsync: Optional[bool] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackTraitDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.TraitDefinition.6 {{ }}", TraitDefinition
    return f"hack.TraitDefinition.6 { concatenateFields(key) }", TraitDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, members: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None, attributes: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, requireExtends: Optional[Tuple[()]] = None, requireImplements: Optional[Tuple[()]] = None, module_: Optional[Tuple[()]] = None) -> "HackTraitDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.DeclarationName.6 {{ }}", DeclarationName
    return f"hack.DeclarationName.6 {key}", DeclarationName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HackDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class HackPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.PropertyDeclaration.6 {{ }}", PropertyDeclaration
    return f"hack.PropertyDeclaration.6 { concatenateFields(key) }", PropertyDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "HackPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFileCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.FileCall.6 {{ }}", FileCall
    return f"hack.FileCall.6 { concatenateFields(key) }", FileCall

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, callee_span: Optional[Tuple[()]] = None, call_args: Optional[Tuple[()]] = None) -> "HackFileCall":
    raise Exception("this function can only be called from @angle_query")

class HackFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.FileDeclarations.6 {{ }}", FileDeclarations
    return f"hack.FileDeclarations.6 { concatenateFields(key) }", FileDeclarations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, declarations: Optional[Tuple[()]] = None) -> "HackFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class HackTypeConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.TypeConstDeclaration.6 {{ }}", TypeConstDeclaration
    return f"hack.TypeConstDeclaration.6 { concatenateFields(key) }", TypeConstDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "HackTypeConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hack.DeclarationLocation.6 {{ }}", DeclarationLocation
    return f"hack.DeclarationLocation.6 { concatenateFields(key) }", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "HackDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")


