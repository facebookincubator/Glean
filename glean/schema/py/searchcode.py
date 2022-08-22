# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.searchcode.types import (
    CodeCxxSearchByNameAndScopeFact,
    CodeSearchByLowerCaseScope,
    CodePythonSearchByLowerCaseName,
    CodeErlangSearchByName,
    CodeHsSearchByName,
    CodeHackSearchByLowerCaseScope,
    CodeCxxSearchByName,
    CodeSearchByName,
    CodeHackSearchByName,
    CodeHackSearchByScopeWithName,
    CodeCxxSearchByLowerCaseScope,
    CodePythonSearchByLocalNameFact,
    CodeHackSearchByLowerCaseName,
    CodeLsifSearchByLowerCaseName,
    CodeRustSearchByLowerCaseName,
    CodeCxxSearchByLowerCaseName,
    CodePythonSearchByNameFact,
    CodeHsSearchByLowerCaseName,
    CodeErlangSearchByLowerCaseName,
    CodeSearchByScope,
    CodeFlowSearchByLowerCaseName,
    CodeFlowSearchByName,
    CodeRustSearchByName,
    CodeLsifSearchByName,
    CodeSearchByNameAndLanguage,
    CodeCxxSearchByScope,
    CodeSearchByLowerCaseNameAndLanguage,
    CodeSearchByLowerCaseName,
    CodeHackSearchByScope,
    CodePythonSearchByName,
)


class SearchCodeCxxSearchByNameAndScopeFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByNameAndScopeFact.16 {{ name = _, scope = _, entity = _ }}", CodeCxxSearchByNameAndScopeFact

  @staticmethod
  def angle_query(*, name: Tuple[()], scope: Tuple[()], entity: Tuple[()]) -> "SearchCodeCxxSearchByNameAndScopeFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseScope.16 {{ name = _, scope = _, language = _, entity = _ }}", CodeSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: str, scope: Tuple[()], language: Tuple[()], entity: Tuple[()]) -> "SearchCodeSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodePythonSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodePythonSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.ErlangSearchByName.16 {{ name = _, entity = _ }}", CodeErlangSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HsSearchByName.16 {{ name = _, entity = _ }}", CodeHsSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeHsSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByLowerCaseScope.16 {{ name = _, scope = _, entity = _ }}", CodeHackSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: str, scope: Tuple[()], entity: Tuple[()]) -> "SearchCodeHackSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByName.16 {{ name = _, entity = _ }}", CodeCxxSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeCxxSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByName.16 {{ name = _, entity = _ }}", CodeSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByName.16 {{ name = _, entity = _ }}", CodeHackSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByScopeWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByScopeWithName.16 {{ name = _, scope = _, entity = _ }}", CodeHackSearchByScopeWithName

  @staticmethod
  def angle_query(*, name: Tuple[()], scope: Tuple[()], entity: Tuple[()]) -> "SearchCodeHackSearchByScopeWithName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByLowerCaseScope.16 {{ name = _, scope = _, entity = _ }}", CodeCxxSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: str, scope: Tuple[()], entity: Tuple[()]) -> "SearchCodeCxxSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByLocalNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByLocalNameFact.16 {{ name = _, entity = _ }}", CodePythonSearchByLocalNameFact

  @staticmethod
  def angle_query(*, name: Tuple[()], entity: Tuple[()]) -> "SearchCodePythonSearchByLocalNameFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeHackSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeHackSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeLsifSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.LsifSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeLsifSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeLsifSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeRustSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.RustSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeRustSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeRustSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeCxxSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeCxxSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByNameFact.16 {{ name = _, entity = _ }}", CodePythonSearchByNameFact

  @staticmethod
  def angle_query(*, name: Tuple[()], entity: Tuple[()]) -> "SearchCodePythonSearchByNameFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHsSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HsSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeHsSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeHsSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeErlangSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.ErlangSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeErlangSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeErlangSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByScope.16 {{ name = _, scope = _, language = _, entity = _ }}", CodeSearchByScope

  @staticmethod
  def angle_query(*, name: str, scope: Tuple[()], language: Tuple[()], entity: Tuple[()]) -> "SearchCodeSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeFlowSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.FlowSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeFlowSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeFlowSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeFlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.FlowSearchByName.16 {{ name = _, entity = _ }}", CodeFlowSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeFlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeRustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.RustSearchByName.16 {{ name = _, entity = _ }}", CodeRustSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeRustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeLsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.LsifSearchByName.16 {{ name = _, entity = _ }}", CodeLsifSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeLsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByNameAndLanguage.16 {{ name = _, language = _, entity = _ }}", CodeSearchByNameAndLanguage

  @staticmethod
  def angle_query(*, name: str, language: Tuple[()], entity: Tuple[()]) -> "SearchCodeSearchByNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByScope.16 {{ name = _, scope = _, entity = _ }}", CodeCxxSearchByScope

  @staticmethod
  def angle_query(*, name: str, scope: Tuple[()], entity: Tuple[()]) -> "SearchCodeCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseNameAndLanguage.16 {{ name = _, language = _, entity = _ }}", CodeSearchByLowerCaseNameAndLanguage

  @staticmethod
  def angle_query(*, name: str, language: Tuple[()], entity: Tuple[()]) -> "SearchCodeSearchByLowerCaseNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodeSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByScope.16 {{ name = _, scope = _, entity = _ }}", CodeHackSearchByScope

  @staticmethod
  def angle_query(*, name: str, scope: Tuple[()], entity: Tuple[()]) -> "SearchCodeHackSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByName.16 {{ name = _, entity = _ }}", CodePythonSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchCodePythonSearchByName":
    raise Exception("this function can only be called from @angle_query")


