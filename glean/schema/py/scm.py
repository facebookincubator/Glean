# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R


from glean.schema.scm.types import (
    Rev,
    Repo,
    RepoName,
    Timestamp,
    Commit,
    RepoType,
)


class ScmRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"scm.Rev.1 {angle_for(__env, arg)}", Rev

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ScmRev":
    raise Exception("this function can only be called from @angle_query")

class ScmRepo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"scm.Repo.1 {{ name = {angle_for(__env, name)}, type = {angle_for(__env, type)} }}", Repo

  @staticmethod
  def angle_query(*, name: Optional["ScmRepoName"] = None, type: Optional["ScmRepoType"] = None) -> "ScmRepo":
    raise Exception("this function can only be called from @angle_query")

class ScmRepoName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"scm.RepoName.1 {angle_for(__env, arg)}", RepoName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ScmRepoName":
    raise Exception("this function can only be called from @angle_query")

class ScmTimestamp(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"scm.Timestamp.1 {angle_for(__env, arg)}", Timestamp

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "ScmTimestamp":
    raise Exception("this function can only be called from @angle_query")

class ScmCommit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], repo: ast.Expr, rev: ast.Expr, timestamp: ast.Expr) -> Tuple[str, Struct]:
    return f"scm.Commit.1 {{ repo = {angle_for(__env, repo)}, rev = {angle_for(__env, rev)}, timestamp = {angle_for(__env, timestamp)} }}", Commit

  @staticmethod
  def angle_query(*, repo: Optional["ScmRepo"] = None, rev: Optional["ScmRev"] = None, timestamp: Optional["ScmTimestamp"] = None) -> "ScmCommit":
    raise Exception("this function can only be called from @angle_query")

class ScmRepoType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"scm.RepoType.1 {angle_for(__env, arg)}", RepoType

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ScmRepoType":
    raise Exception("this function can only be called from @angle_query")


