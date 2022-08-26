# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.configerator.types import (
    ImportFilesStatement,
)


class ConfigeratorImportFilesStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"configerator.ImportFilesStatement.1 {{ }}", ImportFilesStatement
    return f"configerator.ImportFilesStatement.1 { concatenateFields(key) }", ImportFilesStatement

  @staticmethod
  def angle_query(*, import_: Optional[Tuple[()]] = None, is_recursive: Optional[bool] = None) -> "ConfigeratorImportFilesStatement":
    raise Exception("this function can only be called from @angle_query")


