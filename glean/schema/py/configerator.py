# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.configerator.types import (
    ImportFilesStatement,
)


class ConfigeratorImportFilesStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"configerator.ImportFilesStatement.1 {{ import_ = _, is_recursive = _ }}", ImportFilesStatement

  @staticmethod
  def angle_query(*, import_: Tuple[()], is_recursive: bool) -> "ConfigeratorImportFilesStatement":
    raise Exception("this function can only be called from @angle_query")


