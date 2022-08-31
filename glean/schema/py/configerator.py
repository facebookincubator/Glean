# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.python import *


from glean.schema.configerator.types import (
    ImportFilesStatement,
)


class ConfigeratorImportFilesStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], import_: ast.Expr, is_recursive: ast.Expr) -> Tuple[str, Struct]:
    return f"configerator.ImportFilesStatement.1 {{ import_ = {angle_for(__env, import_)}, is_recursive = {angle_for(__env, is_recursive)} }}", ImportFilesStatement

  @staticmethod
  def angle_query(*, import_: Optional["PythonImportStatement"] = None, is_recursive: Optional[bool] = None) -> "ConfigeratorImportFilesStatement":
    raise Exception("this function can only be called from @angle_query")


