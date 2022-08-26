# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct

class GleanSchemaPredicate:
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query(*, arg: str) -> "GleanSchemaPredicate":
    raise Exception("this function can only be called from @angle_query")

def concatenateFields(fields: List[Tuple[str,str]]) -> str:
  f = ', '.join(map((lambda f: f'{f[0]} = {f[1]}'), fields))
  return f'{{ {f} }}'

