# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.searchbuck.types import (
    buckSearchDefinition,
    buckSearchFile,
    buckSearchByFQN,
)


class SearchBuckSearchDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.buck.SearchDefinition.1 {{ module = {angle_for(__env, module)}, name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", buckSearchDefinition

  @staticmethod
  def angle_query(*, module: Optional["SrcFile"] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchBuckSearchDefinition":
    raise Exception("this function can only be called from @angle_query")

class SearchBuckSearchFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.buck.SearchFile.1 {{ file = {angle_for(__env, file)}, entity = {angle_for(__env, entity)} }}", buckSearchFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, entity: Optional[Tuple[()]] = None) -> "SearchBuckSearchFile":
    raise Exception("this function can only be called from @angle_query")

class SearchBuckSearchByFQN(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], subdir: ast.Expr, path: ast.Expr, name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.buck.SearchByFQN.1 {{ subdir = {angle_for(__env, subdir)}, path = {angle_for(__env, path)}, name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", buckSearchByFQN

  @staticmethod
  def angle_query(*, subdir: Optional[Tuple[()]] = None, path: Optional[str] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchBuckSearchByFQN":
    raise Exception("this function can only be called from @angle_query")


