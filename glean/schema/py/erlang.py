# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.erlang.types import (
    DeclarationReference,
    DeclarationWithFqn,
    FunctionDeclaration,
    DeclarationToFqn,
    SearchByName,
    DeclarationsByFile,
    DeclarationLocation,
    NameLowerCase,
    XRefsViaFqnByFile,
    DeclarationUses,
)


class ErlangDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.DeclarationReference.1 {{ }}", DeclarationReference
    return f"erlang.DeclarationReference.1 { concatenateFields(key) }", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "ErlangDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationWithFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.DeclarationWithFqn.1 {{ }}", DeclarationWithFqn
    return f"erlang.DeclarationWithFqn.1 { concatenateFields(key) }", DeclarationWithFqn

  @staticmethod
  def angle_query(*, fqn: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "ErlangDeclarationWithFqn":
    raise Exception("this function can only be called from @angle_query")

class ErlangFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.FunctionDeclaration.1 {{ }}", FunctionDeclaration
    return f"erlang.FunctionDeclaration.1 { concatenateFields(key) }", FunctionDeclaration

  @staticmethod
  def angle_query(*, fqn: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "ErlangFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationToFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.DeclarationToFqn.1 {{ }}", DeclarationToFqn
    return f"erlang.DeclarationToFqn.1 {key}", DeclarationToFqn

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ErlangDeclarationToFqn":
    raise Exception("this function can only be called from @angle_query")

class ErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.SearchByName.1 {{ }}", SearchByName
    return f"erlang.SearchByName.1 { concatenateFields(key) }", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, func: Optional[Tuple[()]] = None) -> "ErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.DeclarationsByFile.1 {{ }}", DeclarationsByFile
    return f"erlang.DeclarationsByFile.1 { concatenateFields(key) }", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "ErlangDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.DeclarationLocation.1 {{ }}", DeclarationLocation
    return f"erlang.DeclarationLocation.1 { concatenateFields(key) }", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "ErlangDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class ErlangNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.NameLowerCase.1 {{ }}", NameLowerCase
    return f"erlang.NameLowerCase.1 { concatenateFields(key) }", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowercase: Optional[str] = None, name: Optional[str] = None) -> "ErlangNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class ErlangXRefsViaFqnByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.XRefsViaFqnByFile.1 {{ }}", XRefsViaFqnByFile
    return f"erlang.XRefsViaFqnByFile.1 { concatenateFields(key) }", XRefsViaFqnByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "ErlangXRefsViaFqnByFile":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"erlang.DeclarationUses.1 {{ }}", DeclarationUses
    return f"erlang.DeclarationUses.1 { concatenateFields(key) }", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "ErlangDeclarationUses":
    raise Exception("this function can only be called from @angle_query")


