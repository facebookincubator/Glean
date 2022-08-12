# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSHackMethodDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.MethodDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackMethodDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackSymbolNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.symbolNamespace.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackSymbolNamespace":
    raise Exception("this function can only be called from @angle_query")

class GSHackTraitDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.TraitDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackTraitDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.FunctionDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackTypedefDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.TypedefDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackTypedefDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackAttributeToDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.AttributeToDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackAttributeToDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackNamespaceMember(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.NamespaceMember.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackNamespaceMember":
    raise Exception("this function can only be called from @angle_query")

class GSHackGlobalConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.GlobalConstDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackGlobalConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackContainerParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ContainerParent.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackContainerParent":
    raise Exception("this function can only be called from @angle_query")

class GSHackInterfaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.InterfaceDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackInterfaceDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackContext_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.Context_.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackContext_":
    raise Exception("this function can only be called from @angle_query")

class GSHackContainerDeclarationQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ContainerDeclarationQName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackContainerDeclarationQName":
    raise Exception("this function can only be called from @angle_query")

class GSHackTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.TargetUses.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackTargetUses":
    raise Exception("this function can only be called from @angle_query")

class GSHackTargetUsesAbs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.TargetUsesAbs.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackTargetUsesAbs":
    raise Exception("this function can only be called from @angle_query")

class GSHackMethodOverridden(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.MethodOverridden.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackMethodOverridden":
    raise Exception("this function can only be called from @angle_query")

class GSHackClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ClassDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.QName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackQName":
    raise Exception("this function can only be called from @angle_query")

class GSHackFilename(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.filename.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackFilename":
    raise Exception("this function can only be called from @angle_query")

class GSHackKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.kind.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackKind":
    raise Exception("this function can only be called from @angle_query")

class GSHackTypedefDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.TypedefDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackTypedefDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ModuleDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackModuleDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackAttributeHasParameter(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.AttributeHasParameter.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackAttributeHasParameter":
    raise Exception("this function can only be called from @angle_query")

class GSHackName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.Name.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackName":
    raise Exception("this function can only be called from @angle_query")

class GSHackMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.MethodDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.FileXRefs.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSHackEnumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.Enumerator.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackEnumerator":
    raise Exception("this function can only be called from @angle_query")

class GSHackIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.identifier.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackIdentifier":
    raise Exception("this function can only be called from @angle_query")

class GSHackDeclarationSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.DeclarationSpan.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackDeclarationSpan":
    raise Exception("this function can only be called from @angle_query")

class GSHackSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.Signature.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackSignature":
    raise Exception("this function can only be called from @angle_query")

class GSHackContainerChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ContainerChild.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackContainerChild":
    raise Exception("this function can only be called from @angle_query")

class GSHackNamespaceQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.NamespaceQName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackNamespaceQName":
    raise Exception("this function can only be called from @angle_query")

class GSHackInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.InterfaceDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackDeclarationSource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.DeclarationSource.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackDeclarationSource":
    raise Exception("this function can only be called from @angle_query")

class GSHackUserAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.UserAttribute.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackUserAttribute":
    raise Exception("this function can only be called from @angle_query")

class GSHackModuleDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ModuleDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackModuleDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackPropertyDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.PropertyDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackPropertyDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackClassConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ClassConstDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackClassConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.EnumDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackDeclarationComment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.DeclarationComment.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackDeclarationComment":
    raise Exception("this function can only be called from @angle_query")

class GSHackNamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.NamespaceDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackNamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ClassDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackClassDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackMethodOccurrence(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.MethodOccurrence.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackMethodOccurrence":
    raise Exception("this function can only be called from @angle_query")

class GSHackMethodOverrides(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.MethodOverrides.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackMethodOverrides":
    raise Exception("this function can only be called from @angle_query")

class GSHackType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.Type.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackType":
    raise Exception("this function can only be called from @angle_query")

class GSHackNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.NameLowerCase.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHackTypeConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.TypeConstDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackTypeConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackDeclarationTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.DeclarationTarget.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackDeclarationTarget":
    raise Exception("this function can only be called from @angle_query")

class GSHackSymbol(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.symbol.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackSymbol":
    raise Exception("this function can only be called from @angle_query")

class GSHackEnumDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.EnumDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackEnumDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackClassConstDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.ClassConstDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackClassConstDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackStringLiteral(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.StringLiteral.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackStringLiteral":
    raise Exception("this function can only be called from @angle_query")

class GSHackGlobalConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.GlobalConstDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackGlobalConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.FunctionDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackTraitDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.TraitDefinition.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackTraitDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHackDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.DeclarationName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GSHackPropertyDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.PropertyDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackPropertyDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackFileCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.FileCall.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackFileCall":
    raise Exception("this function can only be called from @angle_query")

class GSHackFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.FileDeclarations.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class GSHackTypeConstDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.TypeConstDeclaration.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackTypeConstDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSHackDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hack.DeclarationLocation.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")


