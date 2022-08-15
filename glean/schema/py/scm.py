# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSScmRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.Rev.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSScmRev":
    raise Exception("this function can only be called from @angle_query")

class GSScmRepo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.Repo.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSScmRepo":
    raise Exception("this function can only be called from @angle_query")

class GSScmRepoName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.RepoName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSScmRepoName":
    raise Exception("this function can only be called from @angle_query")

class GSScmTimestamp(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.Timestamp.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: int) -> "GSScmTimestamp":
    raise Exception("this function can only be called from @angle_query")

class GSScmCommit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.Commit.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSScmCommit":
    raise Exception("this function can only be called from @angle_query")

class GSScmRepoType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"scm.RepoType.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSScmRepoType":
    raise Exception("this function can only be called from @angle_query")


