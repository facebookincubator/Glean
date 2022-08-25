# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class FbthriftTypeSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.TypeSpecification.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FbthriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.FunctionName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftFunctionName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.FunctionSpecification.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")

class FbthriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.EnumValue.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftEnumValue":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.ServiceDefinition.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")

class FbthriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.TypeDefException.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.ServiceName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftServiceName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.NamedDecl.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.FileXRefs.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class FbthriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.Constant.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftConstant":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.File.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FbthriftFile":
    raise Exception("this function can only be called from @angle_query")

class FbthriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.QualName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftQualName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.ServiceParent.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftServiceParent":
    raise Exception("this function can only be called from @angle_query")

class FbthriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.ExceptionName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FbthriftExceptionName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"fbthrift.Identifier.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FbthriftIdentifier":
    raise Exception("this function can only be called from @angle_query")


