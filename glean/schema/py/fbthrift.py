# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


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
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.TypeSpecification.1 {angle_for(__env, arg)}", TypeSpecification

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FbthriftTypeSpecification":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], service_: ast.Expr, name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.FunctionName.1 {{ service_ = {angle_for(__env, service_)}, name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", FunctionName

  @staticmethod
  def angle_query(*, service_: Optional["FbthriftServiceName"] = None, name: Optional["FbthriftIdentifier"] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftFunctionName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFunctionSpecification(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, result: ast.Expr, arguments: ast.Expr, throws_: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.FunctionSpecification.1 {{ name = {angle_for(__env, name)}, result = {angle_for(__env, result)}, arguments = {angle_for(__env, arguments)}, throws_ = {angle_for(__env, throws_)} }}", FunctionSpecification

  @staticmethod
  def angle_query(*, name: Optional["FbthriftFunctionName"] = None, result: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None) -> "FbthriftFunctionSpecification":
    raise Exception("this function can only be called from @angle_query")

class FbthriftEnumValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], enum_: ast.Expr, name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.EnumValue.1 {{ enum_ = {angle_for(__env, enum_)}, name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", EnumValue

  @staticmethod
  def angle_query(*, enum_: Optional[Tuple[()]] = None, name: Optional["FbthriftIdentifier"] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftEnumValue":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, functions: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.ServiceDefinition.1 {{ name = {angle_for(__env, name)}, functions = {angle_for(__env, functions)} }}", ServiceDefinition

  @staticmethod
  def angle_query(*, name: Optional["FbthriftServiceName"] = None, functions: Optional[Tuple[()]] = None) -> "FbthriftServiceDefinition":
    raise Exception("this function can only be called from @angle_query")

class FbthriftTypeDefException(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], alias: ast.Expr, type_: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.TypeDefException.1 {{ alias = {angle_for(__env, alias)}, type_ = {angle_for(__env, type_)} }}", TypeDefException

  @staticmethod
  def angle_query(*, alias: Optional["FbthriftNamedDecl"] = None, type_: Optional[Tuple[()]] = None) -> "FbthriftTypeDefException":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.ServiceName.1 {{ name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", ServiceName

  @staticmethod
  def angle_query(*, name: Optional["FbthriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftServiceName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftNamedDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.NamedDecl.1 {{ name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", NamedDecl

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftNamedDecl":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, targets: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.FileXRefs.1 {{ file = {angle_for(__env, file)}, targets = {angle_for(__env, targets)}, xrefs = {angle_for(__env, xrefs)} }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["FbthriftFile"] = None, targets: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "FbthriftFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class FbthriftConstant(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.Constant.1 {{ name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", Constant

  @staticmethod
  def angle_query(*, name: Optional["FbthriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftConstant":
    raise Exception("this function can only be called from @angle_query")

class FbthriftFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.File.1 {angle_for(__env, arg)}", File

  @staticmethod
  def angle_query(*, arg: Optional["SrcFile"] = None) -> "FbthriftFile":
    raise Exception("this function can only be called from @angle_query")

class FbthriftQualName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.QualName.1 {{ file = {angle_for(__env, file)}, name = {angle_for(__env, name)} }}", QualName

  @staticmethod
  def angle_query(*, file: Optional["FbthriftFile"] = None, name: Optional["FbthriftIdentifier"] = None) -> "FbthriftQualName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftServiceParent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.ServiceParent.1 {{ child = {angle_for(__env, child)}, parent = {angle_for(__env, parent)} }}", ServiceParent

  @staticmethod
  def angle_query(*, child: Optional["FbthriftServiceName"] = None, parent: Optional["FbthriftServiceName"] = None) -> "FbthriftServiceParent":
    raise Exception("this function can only be called from @angle_query")

class FbthriftExceptionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, locName: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.ExceptionName.1 {{ name = {angle_for(__env, name)}, locName = {angle_for(__env, locName)} }}", ExceptionName

  @staticmethod
  def angle_query(*, name: Optional["FbthriftQualName"] = None, locName: Optional[Tuple[()]] = None) -> "FbthriftExceptionName":
    raise Exception("this function can only be called from @angle_query")

class FbthriftIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"fbthrift.Identifier.1 {angle_for(__env, arg)}", Identifier

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "FbthriftIdentifier":
    raise Exception("this function can only be called from @angle_query")


