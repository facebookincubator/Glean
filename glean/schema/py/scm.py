# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class ScmRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.Rev.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ScmRev":
    raise Exception("this function can only be called from @angle_query")

class ScmRepo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.Repo.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ScmRepo":
    raise Exception("this function can only be called from @angle_query")

class ScmRepoName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.RepoName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ScmRepoName":
    raise Exception("this function can only be called from @angle_query")

class ScmTimestamp(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.Timestamp.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: int) -> "ScmTimestamp":
    raise Exception("this function can only be called from @angle_query")

class ScmCommit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.Commit.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ScmCommit":
    raise Exception("this function can only be called from @angle_query")

class ScmRepoType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.RepoType.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "ScmRepoType":
    raise Exception("this function can only be called from @angle_query")


