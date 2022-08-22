# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"scm.Rev.1 {json.dumps(key)}", Rev

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ScmRev":
    raise Exception("this function can only be called from @angle_query")

class ScmRepo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"scm.Repo.1 {{ name = _, type = _ }}", Repo

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "ScmRepo":
    raise Exception("this function can only be called from @angle_query")

class ScmRepoName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"scm.RepoName.1 {json.dumps(key)}", RepoName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ScmRepoName":
    raise Exception("this function can only be called from @angle_query")

class ScmTimestamp(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"scm.Timestamp.1 {json.dumps(key)}", Timestamp

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "ScmTimestamp":
    raise Exception("this function can only be called from @angle_query")

class ScmCommit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"scm.Commit.1 {{ repo = _, rev = _, timestamp = _ }}", Commit

  @staticmethod
  def angle_query(*, repo: Optional[Tuple[()]] = None, rev: Optional[Tuple[()]] = None, timestamp: Optional[Tuple[()]] = None) -> "ScmCommit":
    raise Exception("this function can only be called from @angle_query")

class ScmRepoType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"scm.RepoType.1 {json.dumps(key)}", RepoType

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ScmRepoType":
    raise Exception("this function can only be called from @angle_query")


