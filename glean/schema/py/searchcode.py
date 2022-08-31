# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.cxx1 import *
from glean.schema.py.hack import *
from glean.schema.py.python import *


from glean.schema.searchcode.types import (
    codeCxxSearchByNameAndScopeFact,
    codeSearchByLowerCaseScope,
    codePythonSearchByLowerCaseName,
    codeErlangSearchByName,
    codeHsSearchByName,
    codeHackSearchByLowerCaseScope,
    codeCxxSearchByName,
    codeSearchByName,
    codeHackSearchByName,
    codeHackSearchByScopeWithName,
    codeCxxSearchByLowerCaseScope,
    codePythonSearchByLocalNameFact,
    codeHackSearchByLowerCaseName,
    codeLsifSearchByLowerCaseName,
    codeRustSearchByLowerCaseName,
    codeCxxSearchByLowerCaseName,
    codePythonSearchByNameFact,
    codeHsSearchByLowerCaseName,
    codeErlangSearchByLowerCaseName,
    codeSearchByScope,
    codeFlowSearchByLowerCaseName,
    codeFlowSearchByName,
    codeRustSearchByName,
    codeLsifSearchByName,
    codeSearchByNameAndLanguage,
    codeCxxSearchByScope,
    codeSearchByLowerCaseNameAndLanguage,
    codeSearchByLowerCaseName,
    codeHackSearchByScope,
    codePythonSearchByName,
)


class SearchCodeCxxSearchByNameAndScopeFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByNameAndScopeFact.16 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)} }}", codeCxxSearchByNameAndScopeFact

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByNameAndScopeFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseScope.16 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, language = {angle_for(__env, language)}, entity = {angle_for(__env, entity)} }}", codeSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codePythonSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.ErlangSearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeErlangSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HsSearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeHsSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHsSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByLowerCaseScope.16 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)} }}", codeHackSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeCxxSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeHackSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByScopeWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByScopeWithName.16 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)} }}", codeHackSearchByScopeWithName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByScopeWithName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByLowerCaseScope.16 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)} }}", codeCxxSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByLocalNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByLocalNameFact.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codePythonSearchByLocalNameFact

  @staticmethod
  def angle_query(*, name: Optional["PythonLocalName"] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByLocalNameFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeHackSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeLsifSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.LsifSearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeLsifSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeLsifSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeRustSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.RustSearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeRustSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeRustSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeCxxSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByNameFact.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codePythonSearchByNameFact

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByNameFact":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHsSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HsSearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeHsSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHsSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeErlangSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.ErlangSearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeErlangSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeErlangSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByScope.16 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, language = {angle_for(__env, language)}, entity = {angle_for(__env, entity)} }}", codeSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeFlowSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.FlowSearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeFlowSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeFlowSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeFlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.FlowSearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeFlowSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeFlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeRustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.RustSearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeRustSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeRustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeLsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.LsifSearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeLsifSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeLsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByNameAndLanguage.16 {{ name = {angle_for(__env, name)}, language = {angle_for(__env, language)}, entity = {angle_for(__env, entity)} }}", codeSearchByNameAndLanguage

  @staticmethod
  def angle_query(*, name: Optional[str] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByScope.16 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)} }}", codeCxxSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseNameAndLanguage.16 {{ name = {angle_for(__env, name)}, language = {angle_for(__env, language)}, entity = {angle_for(__env, entity)} }}", codeSearchByLowerCaseNameAndLanguage

  @staticmethod
  def angle_query(*, name: Optional[str] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codeSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class SearchCodeHackSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByScope.16 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)} }}", codeHackSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCodePythonSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByName.16 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", codePythonSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByName":
    raise Exception("this function can only be called from @angle_query")


