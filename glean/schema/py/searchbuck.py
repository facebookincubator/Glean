# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.codebuck import *
from glean.schema.py.src import *


from glean.schema.search_buck.types import (
    SearchDefinition,
    SearchFile,
    SearchByFQN,
)


class SearchBuckSearchDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.buck.SearchDefinition.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, module, 'module'), angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchDefinition

  @staticmethod
  def angle_query(*, module: Optional["SrcFile"] = None, name: Optional[str] = None, entity: Optional["CodeBuckEntity"] = None) -> "SearchBuckSearchDefinition":
    raise Exception("this function can only be called from @angle_query")



class SearchBuckSearchFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.buck.SearchFile.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, entity: Optional["CodeBuckEntity"] = None) -> "SearchBuckSearchFile":
    raise Exception("this function can only be called from @angle_query")



class SearchBuckSearchByFQN(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], subdir: ast.Expr, path: ast.Expr, name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.buck.SearchByFQN.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, subdir, 'subdir'), angle_for(__env, path, 'path'), angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchByFQN

  @staticmethod
  def angle_query(*, subdir: Optional[Union[Just[str], Just[None]]] = None, path: Optional[str] = None, name: Optional[str] = None, entity: Optional["CodeBuckEntity"] = None) -> "SearchBuckSearchByFQN":
    raise Exception("this function can only be called from @angle_query")






