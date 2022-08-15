# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class HackMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.MethodDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackSymbolNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.symbolNamespace.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackSymbolNamespace":
    raise Exception("this function can only be called from @angle_query")

class HackTraitDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.TraitDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTraitDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.FunctionDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackTypedefDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.TypedefDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTypedefDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackAttributeToDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.AttributeToDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackAttributeToDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceMember(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.NamespaceMember.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackNamespaceMember":
    raise Exception("this function can only be called from @angle_query")

class HackGlobalConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.GlobalConstDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackGlobalConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackContainerParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ContainerParent.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackContainerParent":
    raise Exception("this function can only be called from @angle_query")

class HackInterfaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.InterfaceDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackInterfaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackContext_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.Context_.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HackContext_":
    raise Exception("this function can only be called from @angle_query")

class HackContainerDeclarationQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ContainerDeclarationQName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HackContainerDeclarationQName":
    raise Exception("this function can only be called from @angle_query")

class HackTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.TargetUses.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTargetUses":
    raise Exception("this function can only be called from @angle_query")

class HackTargetUsesAbs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.TargetUsesAbs.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTargetUsesAbs":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.MethodOverridden.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackMethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class HackClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ClassDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.QName.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackQName":
    raise Exception("this function can only be called from @angle_query")

class HackFilename(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.filename.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackFilename":
    raise Exception("this function can only be called from @angle_query")

class HackKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.kind.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackKind":
    raise Exception("this function can only be called from @angle_query")

class HackTypedefDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.TypedefDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTypedefDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ModuleDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackModuleDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackAttributeHasParameter(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.AttributeHasParameter.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackAttributeHasParameter":
    raise Exception("this function can only be called from @angle_query")

class HackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.Name.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HackName":
    raise Exception("this function can only be called from @angle_query")

class HackMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.MethodDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.FileXRefs.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class HackEnumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.Enumerator.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackEnumerator":
    raise Exception("this function can only be called from @angle_query")

class HackIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.identifier.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackIdentifier":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.DeclarationSpan.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackDeclarationSpan":
    raise Exception("this function can only be called from @angle_query")

class HackSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.Signature.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackSignature":
    raise Exception("this function can only be called from @angle_query")

class HackContainerChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ContainerChild.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackContainerChild":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.NamespaceQName.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackNamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class HackInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.InterfaceDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationSource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.DeclarationSource.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackDeclarationSource":
    raise Exception("this function can only be called from @angle_query")

class HackUserAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.UserAttribute.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackUserAttribute":
    raise Exception("this function can only be called from @angle_query")

class HackModuleDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ModuleDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackModuleDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackPropertyDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.PropertyDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackPropertyDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackClassConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ClassConstDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackClassConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.EnumDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.DeclarationComment.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackDeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class HackNamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.NamespaceDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackNamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ClassDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackClassDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOccurrence(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.MethodOccurrence.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackMethodOccurrence":
    raise Exception("this function can only be called from @angle_query")

class HackMethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.MethodOverrides.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackMethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class HackType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.Type.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HackType":
    raise Exception("this function can only be called from @angle_query")

class HackNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.NameLowerCase.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HackTypeConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.TypeConstDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTypeConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.DeclarationTarget.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackDeclarationTarget":
    raise Exception("this function can only be called from @angle_query")

class HackSymbol(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.symbol.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackSymbol":
    raise Exception("this function can only be called from @angle_query")

class HackEnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.EnumDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackEnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackClassConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.ClassConstDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackClassConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackStringLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.StringLiteral.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HackStringLiteral":
    raise Exception("this function can only be called from @angle_query")

class HackGlobalConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.GlobalConstDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackGlobalConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.FunctionDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackTraitDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.TraitDefinition.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTraitDefinition":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.DeclarationName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HackDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class HackPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.PropertyDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackFileCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.FileCall.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackFileCall":
    raise Exception("this function can only be called from @angle_query")

class HackFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.FileDeclarations.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class HackTypeConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.TypeConstDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackTypeConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class HackDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hack.DeclarationLocation.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")


