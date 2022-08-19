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
    return f"search.code.CxxSearchByNameAndScopeFact.16 { { } }", CodeCxxSearchByNameAndScopeFact

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeCxxSearchByNameAndScopeFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseScope.16 { { } }", CodeSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByLowerCaseName.16 { { } }", CodePythonSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodePythonSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.ErlangSearchByName.16 { { } }", CodeErlangSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HsSearchByName.16 { { } }", CodeHsSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeHsSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByLowerCaseScope.16 { { } }", CodeHackSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeHackSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByName.16 { { } }", CodeCxxSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeCxxSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByName.16 { { } }", CodeSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByName.16 { { } }", CodeHackSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByScopeWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByScopeWithName.16 { { } }", CodeHackSearchByScopeWithName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeHackSearchByScopeWithName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByLowerCaseScope.16 { { } }", CodeCxxSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeCxxSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByLocalNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByLocalNameFact.16 { { } }", CodePythonSearchByLocalNameFact

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodePythonSearchByLocalNameFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByLowerCaseName.16 { { } }", CodeHackSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeHackSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeLsifSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.LsifSearchByLowerCaseName.16 { { } }", CodeLsifSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeLsifSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeRustSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.RustSearchByLowerCaseName.16 { { } }", CodeRustSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeRustSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByLowerCaseName.16 { { } }", CodeCxxSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeCxxSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByNameFact.16 { { } }", CodePythonSearchByNameFact

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodePythonSearchByNameFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHsSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HsSearchByLowerCaseName.16 { { } }", CodeHsSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeHsSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeErlangSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.ErlangSearchByLowerCaseName.16 { { } }", CodeErlangSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeErlangSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByScope.16 { { } }", CodeSearchByScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeFlowSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.FlowSearchByLowerCaseName.16 { { } }", CodeFlowSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeFlowSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeFlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.FlowSearchByName.16 { { } }", CodeFlowSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeFlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeRustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.RustSearchByName.16 { { } }", CodeRustSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeRustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeLsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.LsifSearchByName.16 { { } }", CodeLsifSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeLsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByNameAndLanguage.16 { { } }", CodeSearchByNameAndLanguage

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeSearchByNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByScope.16 { { } }", CodeCxxSearchByScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseNameAndLanguage.16 { { } }", CodeSearchByLowerCaseNameAndLanguage

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeSearchByLowerCaseNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseName.16 { { } }", CodeSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByScope.16 { { } }", CodeHackSearchByScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodeHackSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByName.16 { { } }", CodePythonSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCodePythonSearchByName":
    raise Exception("this function can only be called from @angle_query")


