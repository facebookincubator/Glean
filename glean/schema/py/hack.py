# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.MethodDefinition.6 {{ declaration = _, signature = _, visibility = _, isAbstract = _, isAsync = _, isFinal = _, isStatic = _, attributes = _, typeParams = _ }}", MethodDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], signature: Tuple[()], visibility: Tuple[()], isAbstract: bool, isAsync: bool, isFinal: bool, isStatic: bool, attributes: Tuple[()], typeParams: Tuple[()]) -> "HackMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackSymbolNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.symbolNamespace.1 {{ namespace_id = _, namespace_name = _ }}", SymbolNamespace

  @staticmethod
  def angle_query(*, namespace_id: int, namespace_name: str) -> "HackSymbolNamespace":
    raise Exception("this function can only be called from @angle_query")

class HackTraitDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.TraitDeclaration.6 {{ name = _ }}", TraitDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTraitDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.FunctionDeclaration.6 {{ name = _ }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackTypedefDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.TypedefDefinition.6 {{ declaration = _, isTransparent = _, attributes = _, typeParams = _, module_ = _ }}", TypedefDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], isTransparent: bool, attributes: Tuple[()], typeParams: Tuple[()], module_: Tuple[()]) -> "HackTypedefDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackAttributeToDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.AttributeToDefinition.6 {{ attribute = _, definition = _ }}", AttributeToDefinition

  @staticmethod
  def angle_query(*, attribute: Tuple[()], definition: Tuple[()]) -> "HackAttributeToDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceMember(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.NamespaceMember.6 {{ namespace_ = _, decl = _ }}", NamespaceMember

  @staticmethod
  def angle_query(*, namespace_: Tuple[()], decl: Tuple[()]) -> "HackNamespaceMember":
    raise Exception("this function can only be called from @angle_query")

class HackGlobalConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.GlobalConstDefinition.6 {{ declaration = _, type = _, value = _ }}", GlobalConstDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], type: Tuple[()], value: str) -> "HackGlobalConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackContainerParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ContainerParent.6 {{ container = _, parent = _ }}", ContainerParent

  @staticmethod
  def angle_query(*, container: Tuple[()], parent: Tuple[()]) -> "HackContainerParent":
    raise Exception("this function can only be called from @angle_query")

class HackInterfaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.InterfaceDefinition.6 {{ declaration = _, members = _, extends_ = _, attributes = _, typeParams = _, requireExtends = _, module_ = _ }}", InterfaceDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], members: Tuple[()], extends_: Tuple[()], attributes: Tuple[()], typeParams: Tuple[()], requireExtends: Tuple[()], module_: Tuple[()]) -> "HackInterfaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackContext_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.Context_.6 {json.dumps(key)}", Context_

  @staticmethod
  def angle_query(*, arg: str) -> "HackContext_":
    raise Exception("this function can only be called from @angle_query")

class HackContainerDeclarationQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ContainerDeclarationQName.6 {json.dumps(key)}", ContainerDeclarationQName

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "HackContainerDeclarationQName":
    raise Exception("this function can only be called from @angle_query")

class HackTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.TargetUses.6 {{ target = _, file = _, uses = _ }}", TargetUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], uses: Tuple[()]) -> "HackTargetUses":
    raise Exception("this function can only be called from @angle_query")

class HackTargetUsesAbs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.TargetUsesAbs.6 {{ target = _, file = _, uses = _ }}", TargetUsesAbs

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], uses: Tuple[()]) -> "HackTargetUsesAbs":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.MethodOverridden.6 {{ base = _, derived = _ }}", MethodOverridden

  @staticmethod
  def angle_query(*, base: Tuple[()], derived: Tuple[()]) -> "HackMethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class HackClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ClassDeclaration.6 {{ name = _ }}", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.QName.6 {{ name = _, namespace_ = _ }}", QName

  @staticmethod
  def angle_query(*, name: Tuple[()], namespace_: Tuple[()]) -> "HackQName":
    raise Exception("this function can only be called from @angle_query")

class HackFilename(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.filename.1 {{ filename = _, filehash_id = _ }}", Filename

  @staticmethod
  def angle_query(*, filename: str, filehash_id: str) -> "HackFilename":
    raise Exception("this function can only be called from @angle_query")

class HackKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.kind.1 {{ id = _, name = _ }}", Kind

  @staticmethod
  def angle_query(*, id: int, name: str) -> "HackKind":
    raise Exception("this function can only be called from @angle_query")

class HackTypedefDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.TypedefDeclaration.6 {{ name = _ }}", TypedefDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTypedefDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ModuleDefinition.6 {{ declaration = _, attributes = _ }}", ModuleDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], attributes: Tuple[()]) -> "HackModuleDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackAttributeHasParameter(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.AttributeHasParameter.6 {{ name = _, parameter = _, attribute = _ }}", AttributeHasParameter

  @staticmethod
  def angle_query(*, name: Tuple[()], parameter: str, attribute: Tuple[()]) -> "HackAttributeHasParameter":
    raise Exception("this function can only be called from @angle_query")

class HackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.Name.6 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: str) -> "HackName":
    raise Exception("this function can only be called from @angle_query")

class HackMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.MethodDeclaration.6 {{ name = _, container = _ }}", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], container: Tuple[()]) -> "HackMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.FileXRefs.6 {{ file = _, xrefs = _ }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Tuple[()], xrefs: Tuple[()]) -> "HackFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class HackEnumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.Enumerator.6 {{ name = _, enumeration = _ }}", Enumerator

  @staticmethod
  def angle_query(*, name: Tuple[()], enumeration: Tuple[()]) -> "HackEnumerator":
    raise Exception("this function can only be called from @angle_query")

class HackIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.identifier.1 {{ kind = _, name = _ }}", Identifier

  @staticmethod
  def angle_query(*, kind: int, name: str) -> "HackIdentifier":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.DeclarationSpan.6 {{ declaration = _, file = _, span = _ }}", DeclarationSpan

  @staticmethod
  def angle_query(*, declaration: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "HackDeclarationSpan":
    raise Exception("this function can only be called from @angle_query")

class HackSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.Signature.6 {{ returns = _, parameters = _, contexts = _ }}", Signature

  @staticmethod
  def angle_query(*, returns: Tuple[()], parameters: Tuple[()], contexts: Tuple[()]) -> "HackSignature":
    raise Exception("this function can only be called from @angle_query")

class HackContainerChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ContainerChild.6 {{ container = _, child = _ }}", ContainerChild

  @staticmethod
  def angle_query(*, container: Tuple[()], child: Tuple[()]) -> "HackContainerChild":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.NamespaceQName.6 {{ name = _, parent = _ }}", NamespaceQName

  @staticmethod
  def angle_query(*, name: Tuple[()], parent: Tuple[()]) -> "HackNamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class HackInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.InterfaceDeclaration.6 {{ name = _ }}", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationSource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.DeclarationSource.6 {{ target = _, source = _ }}", DeclarationSource

  @staticmethod
  def angle_query(*, target: Tuple[()], source: Tuple[()]) -> "HackDeclarationSource":
    raise Exception("this function can only be called from @angle_query")

class HackUserAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.UserAttribute.6 {{ name = _, parameters = _, qname = _ }}", UserAttribute

  @staticmethod
  def angle_query(*, name: Tuple[()], parameters: Tuple[()], qname: Tuple[()]) -> "HackUserAttribute":
    raise Exception("this function can only be called from @angle_query")

class HackModuleDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ModuleDeclaration.6 {{ name = _ }}", ModuleDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackModuleDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackPropertyDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.PropertyDefinition.6 {{ declaration = _, type = _, visibility = _, isFinal = _, isAbstract = _, isStatic = _, attributes = _ }}", PropertyDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], type: Tuple[()], visibility: Tuple[()], isFinal: bool, isAbstract: bool, isStatic: bool, attributes: Tuple[()]) -> "HackPropertyDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackClassConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ClassConstDeclaration.6 {{ name = _, container = _ }}", ClassConstDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], container: Tuple[()]) -> "HackClassConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.EnumDeclaration.6 {{ name = _ }}", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.DeclarationComment.6 {{ declaration = _, file = _, span = _ }}", DeclarationComment

  @staticmethod
  def angle_query(*, declaration: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "HackDeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.NamespaceDeclaration.6 {{ name = _ }}", NamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackNamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ClassDefinition.6 {{ declaration = _, isAbstract = _, isFinal = _, members = _, extends_ = _, implements_ = _, uses = _, attributes = _, typeParams = _, module_ = _ }}", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], isAbstract: bool, isFinal: bool, members: Tuple[()], extends_: Tuple[()], implements_: Tuple[()], uses: Tuple[()], attributes: Tuple[()], typeParams: Tuple[()], module_: Tuple[()]) -> "HackClassDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOccurrence(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.MethodOccurrence.6 {{ name = _, className = _ }}", MethodOccurrence

  @staticmethod
  def angle_query(*, name: Tuple[()], className: Tuple[()]) -> "HackMethodOccurrence":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.MethodOverrides.6 {{ derived = _, base = _ }}", MethodOverrides

  @staticmethod
  def angle_query(*, derived: Tuple[()], base: Tuple[()]) -> "HackMethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class HackType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.Type.6 {json.dumps(key)}", Type

  @staticmethod
  def angle_query(*, arg: str) -> "HackType":
    raise Exception("this function can only be called from @angle_query")

class HackNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.NameLowerCase.6 {{ nameLowercase = _, name = _ }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowercase: str, name: Tuple[()]) -> "HackNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HackTypeConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.TypeConstDefinition.6 {{ declaration = _, type = _, kind = _, attributes = _ }}", TypeConstDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], type: Tuple[()], kind: Tuple[()], attributes: Tuple[()]) -> "HackTypeConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.DeclarationTarget.6 {{ source = _, target = _ }}", DeclarationTarget

  @staticmethod
  def angle_query(*, source: Tuple[()], target: Tuple[()]) -> "HackDeclarationTarget":
    raise Exception("this function can only be called from @angle_query")

class HackSymbol(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.symbol.1 {{ name_lowercase = _, valid = _, kind_id = _, ns_id = _, filehash_id = _, is_abstract = _, is_final = _, canonical_name = _ }}", Symbol

  @staticmethod
  def angle_query(*, name_lowercase: str, valid: Tuple[()], kind_id: int, ns_id: int, filehash_id: str, is_abstract: bool, is_final: bool, canonical_name: str) -> "HackSymbol":
    raise Exception("this function can only be called from @angle_query")

class HackEnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.EnumDefinition.6 {{ declaration = _, enumBase = _, enumConstraint = _, enumerators = _, attributes = _, includes = _, isEnumClass = _, module_ = _ }}", EnumDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], enumBase: Tuple[()], enumConstraint: Tuple[()], enumerators: Tuple[()], attributes: Tuple[()], includes: Tuple[()], isEnumClass: bool, module_: Tuple[()]) -> "HackEnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackClassConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.ClassConstDefinition.6 {{ declaration = _, type = _, value = _ }}", ClassConstDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], type: Tuple[()], value: Tuple[()]) -> "HackClassConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackStringLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.StringLiteral.6 {json.dumps(key)}", StringLiteral

  @staticmethod
  def angle_query(*, arg: str) -> "HackStringLiteral":
    raise Exception("this function can only be called from @angle_query")

class HackGlobalConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.GlobalConstDeclaration.6 {{ name = _ }}", GlobalConstDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackGlobalConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.FunctionDefinition.6 {{ declaration = _, signature = _, isAsync = _, attributes = _, typeParams = _, module_ = _ }}", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], signature: Tuple[()], isAsync: bool, attributes: Tuple[()], typeParams: Tuple[()], module_: Tuple[()]) -> "HackFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackTraitDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.TraitDefinition.6 {{ declaration = _, members = _, implements_ = _, uses = _, attributes = _, typeParams = _, requireExtends = _, requireImplements = _, module_ = _ }}", TraitDefinition

  @staticmethod
  def angle_query(*, declaration: Tuple[()], members: Tuple[()], implements_: Tuple[()], uses: Tuple[()], attributes: Tuple[()], typeParams: Tuple[()], requireExtends: Tuple[()], requireImplements: Tuple[()], module_: Tuple[()]) -> "HackTraitDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.DeclarationName.6 {json.dumps(key)}", DeclarationName

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "HackDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class HackPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.PropertyDeclaration.6 {{ name = _, container = _ }}", PropertyDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], container: Tuple[()]) -> "HackPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFileCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.FileCall.6 {{ file = _, callee_span = _, call_args = _ }}", FileCall

  @staticmethod
  def angle_query(*, file: Tuple[()], callee_span: Tuple[()], call_args: Tuple[()]) -> "HackFileCall":
    raise Exception("this function can only be called from @angle_query")

class HackFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.FileDeclarations.6 {{ file = _, declarations = _ }}", FileDeclarations

  @staticmethod
  def angle_query(*, file: Tuple[()], declarations: Tuple[()]) -> "HackFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class HackTypeConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.TypeConstDeclaration.6 {{ name = _, container = _ }}", TypeConstDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], container: Tuple[()]) -> "HackTypeConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hack.DeclarationLocation.6 {{ declaration = _, file = _, span = _ }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "HackDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")


