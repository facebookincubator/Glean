# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"search.code.CxxSearchByNameAndScopeFact.16 {{ }}", CodeCxxSearchByNameAndScopeFact
    return f"search.code.CxxSearchByNameAndScopeFact.16 {{ name = _, scope = _, entity = _ }}", CodeCxxSearchByNameAndScopeFact

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByNameAndScopeFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.SearchByLowerCaseScope.16 {{ }}", CodeSearchByLowerCaseScope
    return f"search.code.SearchByLowerCaseScope.16 {{ name = _, scope = _, language = _, entity = _ }}", CodeSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.PythonSearchByLowerCaseName.16 {{ }}", CodePythonSearchByLowerCaseName
    return f"search.code.PythonSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodePythonSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.ErlangSearchByName.16 {{ }}", CodeErlangSearchByName
    return f"search.code.ErlangSearchByName.16 {{ name = _, entity = _ }}", CodeErlangSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.HsSearchByName.16 {{ }}", CodeHsSearchByName
    return f"search.code.HsSearchByName.16 {{ name = _, entity = _ }}", CodeHsSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHsSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.HackSearchByLowerCaseScope.16 {{ }}", CodeHackSearchByLowerCaseScope
    return f"search.code.HackSearchByLowerCaseScope.16 {{ name = _, scope = _, entity = _ }}", CodeHackSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.CxxSearchByName.16 {{ }}", CodeCxxSearchByName
    return f"search.code.CxxSearchByName.16 {{ name = _, entity = _ }}", CodeCxxSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.SearchByName.16 {{ }}", CodeSearchByName
    return f"search.code.SearchByName.16 {{ name = _, entity = _ }}", CodeSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.HackSearchByName.16 {{ }}", CodeHackSearchByName
    return f"search.code.HackSearchByName.16 {{ name = _, entity = _ }}", CodeHackSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByScopeWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.HackSearchByScopeWithName.16 {{ }}", CodeHackSearchByScopeWithName
    return f"search.code.HackSearchByScopeWithName.16 {{ name = _, scope = _, entity = _ }}", CodeHackSearchByScopeWithName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByScopeWithName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.CxxSearchByLowerCaseScope.16 {{ }}", CodeCxxSearchByLowerCaseScope
    return f"search.code.CxxSearchByLowerCaseScope.16 {{ name = _, scope = _, entity = _ }}", CodeCxxSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByLocalNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.PythonSearchByLocalNameFact.16 {{ }}", CodePythonSearchByLocalNameFact
    return f"search.code.PythonSearchByLocalNameFact.16 {{ name = _, entity = _ }}", CodePythonSearchByLocalNameFact

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByLocalNameFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.HackSearchByLowerCaseName.16 {{ }}", CodeHackSearchByLowerCaseName
    return f"search.code.HackSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeHackSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeLsifSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.LsifSearchByLowerCaseName.16 {{ }}", CodeLsifSearchByLowerCaseName
    return f"search.code.LsifSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeLsifSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeLsifSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeRustSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.RustSearchByLowerCaseName.16 {{ }}", CodeRustSearchByLowerCaseName
    return f"search.code.RustSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeRustSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeRustSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.CxxSearchByLowerCaseName.16 {{ }}", CodeCxxSearchByLowerCaseName
    return f"search.code.CxxSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeCxxSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.PythonSearchByNameFact.16 {{ }}", CodePythonSearchByNameFact
    return f"search.code.PythonSearchByNameFact.16 {{ name = _, entity = _ }}", CodePythonSearchByNameFact

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByNameFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHsSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.HsSearchByLowerCaseName.16 {{ }}", CodeHsSearchByLowerCaseName
    return f"search.code.HsSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeHsSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHsSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeErlangSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.ErlangSearchByLowerCaseName.16 {{ }}", CodeErlangSearchByLowerCaseName
    return f"search.code.ErlangSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeErlangSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeErlangSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.SearchByScope.16 {{ }}", CodeSearchByScope
    return f"search.code.SearchByScope.16 {{ name = _, scope = _, language = _, entity = _ }}", CodeSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeFlowSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.FlowSearchByLowerCaseName.16 {{ }}", CodeFlowSearchByLowerCaseName
    return f"search.code.FlowSearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeFlowSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeFlowSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeFlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.FlowSearchByName.16 {{ }}", CodeFlowSearchByName
    return f"search.code.FlowSearchByName.16 {{ name = _, entity = _ }}", CodeFlowSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeFlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeRustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.RustSearchByName.16 {{ }}", CodeRustSearchByName
    return f"search.code.RustSearchByName.16 {{ name = _, entity = _ }}", CodeRustSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeRustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeLsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.LsifSearchByName.16 {{ }}", CodeLsifSearchByName
    return f"search.code.LsifSearchByName.16 {{ name = _, entity = _ }}", CodeLsifSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeLsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.SearchByNameAndLanguage.16 {{ }}", CodeSearchByNameAndLanguage
    return f"search.code.SearchByNameAndLanguage.16 {{ name = _, language = _, entity = _ }}", CodeSearchByNameAndLanguage

  @staticmethod
  def angle_query(*, name: Optional[str] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.CxxSearchByScope.16 {{ }}", CodeCxxSearchByScope
    return f"search.code.CxxSearchByScope.16 {{ name = _, scope = _, entity = _ }}", CodeCxxSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.SearchByLowerCaseNameAndLanguage.16 {{ }}", CodeSearchByLowerCaseNameAndLanguage
    return f"search.code.SearchByLowerCaseNameAndLanguage.16 {{ name = _, language = _, entity = _ }}", CodeSearchByLowerCaseNameAndLanguage

  @staticmethod
  def angle_query(*, name: Optional[str] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.SearchByLowerCaseName.16 {{ }}", CodeSearchByLowerCaseName
    return f"search.code.SearchByLowerCaseName.16 {{ name = _, entity = _ }}", CodeSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.HackSearchByScope.16 {{ }}", CodeHackSearchByScope
    return f"search.code.HackSearchByScope.16 {{ name = _, scope = _, entity = _ }}", CodeHackSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.code.PythonSearchByName.16 {{ }}", CodePythonSearchByName
    return f"search.code.PythonSearchByName.16 {{ name = _, entity = _ }}", CodePythonSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByName":
    raise Exception("this function can only be called from @angle_query")


