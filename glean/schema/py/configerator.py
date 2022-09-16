# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.python import *


from glean.schema.configerator.types import (
    ImportFilesStatement,
)


class ConfigeratorImportFilesStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], import_: ast.Expr, is_recursive: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, import_, 'import_'), angle_for(__env, is_recursive, 'is_recursive')]))
    return f"configerator.ImportFilesStatement.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportFilesStatement

  @staticmethod
  def angle_query(*, import_: Optional["PythonImportStatement"] = None, is_recursive: Optional[bool] = None) -> "ConfigeratorImportFilesStatement":
    raise Exception("this function can only be called from @angle_query")






