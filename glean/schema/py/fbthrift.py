# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.fbthrift.types import (
    TypeSpecification,
    FunctionName,
    FunctionSpecification,
    EnumValue,
    ServiceDefinition,
    TypeDefException,
    ServiceName,
    NamedDecl,
    FileXRefs,
    Constant,
    File,
    QualName,
    ServiceParent,
    ExceptionName,
    Identifier,
)


class FbthriftTypeSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.TypeSpecification.1 {{ }}", TypeSpecification
    return f"fbthrift.TypeSpecification.1 {json.dumps(key)}", TypeSpecification

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FbthriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.FunctionName.1 {{ }}", FunctionName
    return f"fbthrift.FunctionName.1 {{ service_ = _, name = _, locName = _ }}", FunctionName

  @staticmethod
  def angle_query(*, service_: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftFunctionName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.FunctionSpecification.1 {{ }}", FunctionSpecification
    return f"fbthrift.FunctionSpecification.1 {{ name = _, result = _, arguments = _, throws_ = _ }}", FunctionSpecification

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, result: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None) -> "FbthriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")

class FbthriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.EnumValue.1 {{ }}", EnumValue
    return f"fbthrift.EnumValue.1 {{ enum_ = _, name = _, locName = _ }}", EnumValue

  @staticmethod
  def angle_query(*, enum_: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftEnumValue":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.ServiceDefinition.1 {{ }}", ServiceDefinition
    return f"fbthrift.ServiceDefinition.1 {{ name = _, functions = _ }}", ServiceDefinition

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, functions: Optional[Tuple[()]] = None) -> "FbthriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")

class FbthriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.TypeDefException.1 {{ }}", TypeDefException
    return f"fbthrift.TypeDefException.1 {{ alias = _, type_ = _ }}", TypeDefException

  @staticmethod
  def angle_query(*, alias: Optional[Tuple[()]] = None, type_: Optional[Tuple[()]] = None) -> "FbthriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.ServiceName.1 {{ }}", ServiceName
    return f"fbthrift.ServiceName.1 {{ name = _, locName = _ }}", ServiceName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftServiceName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.NamedDecl.1 {{ }}", NamedDecl
    return f"fbthrift.NamedDecl.1 {{ name = _, locName = _ }}", NamedDecl

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.FileXRefs.1 {{ }}", FileXRefs
    return f"fbthrift.FileXRefs.1 {{ file = _, targets = _, xrefs = _ }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, targets: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "FbthriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class FbthriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.Constant.1 {{ }}", Constant
    return f"fbthrift.Constant.1 {{ name = _, locName = _ }}", Constant

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftConstant":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.File.1 {{ }}", File
    return f"fbthrift.File.1 {json.dumps(key)}", File

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FbthriftFile":
    raise Exception("this function can only be called from @angle_query")

class FbthriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.QualName.1 {{ }}", QualName
    return f"fbthrift.QualName.1 {{ file = _, name = _ }}", QualName

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None) -> "FbthriftQualName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.ServiceParent.1 {{ }}", ServiceParent
    return f"fbthrift.ServiceParent.1 {{ child = _, parent = _ }}", ServiceParent

  @staticmethod
  def angle_query(*, child: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "FbthriftServiceParent":
    raise Exception("this function can only be called from @angle_query")

class FbthriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.ExceptionName.1 {{ }}", ExceptionName
    return f"fbthrift.ExceptionName.1 {{ name = _, locName = _ }}", ExceptionName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftExceptionName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"fbthrift.Identifier.1 {{ }}", Identifier
    return f"fbthrift.Identifier.1 {json.dumps(key)}", Identifier

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "FbthriftIdentifier":
    raise Exception("this function can only be called from @angle_query")


