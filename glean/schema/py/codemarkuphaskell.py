# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just
from glean.schema.py.src import *


from glean.schema.codemarkuphaskell.types import (
    haskellHaskellEntityLocation,
    haskellHaskellResolveLocation,
    haskellHaskellFileEntityXRefLocations,
    haskellHaskellEntityUses,
)


class CodemarkupHaskellHaskellEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellEntityLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", haskellHaskellEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupHaskellHaskellEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHaskellHaskellResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellResolveLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", haskellHaskellResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupHaskellHaskellResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHaskellHaskellFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellFileEntityXRefLocations.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", haskellHaskellFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupHaskellHaskellFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHaskellHaskellEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellEntityUses.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", haskellHaskellEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupHaskellHaskellEntityUses":
    raise Exception("this function can only be called from @angle_query")




