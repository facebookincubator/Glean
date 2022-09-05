# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just
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
    return f"search.code.CxxSearchByNameAndScopeFact.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeCxxSearchByNameAndScopeFact

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByNameAndScopeFact":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseScope.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, language, 'language'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodePythonSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codePythonSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.ErlangSearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeErlangSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HsSearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeHsSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHsSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByLowerCaseScope.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeHackSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeCxxSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeCxxSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeHackSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByScopeWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByScopeWithName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeHackSearchByScopeWithName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, scope: Optional[List[str]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByScopeWithName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeCxxSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByLowerCaseScope.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeCxxSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodePythonSearchByLocalNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByLocalNameFact.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codePythonSearchByLocalNameFact

  @staticmethod
  def angle_query(*, name: Optional["PythonLocalName"] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByLocalNameFact":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeHackSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeLsifSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.LsifSearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeLsifSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeLsifSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeRustSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.RustSearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeRustSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeRustSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeCxxSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeCxxSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodePythonSearchByNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByNameFact.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codePythonSearchByNameFact

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByNameFact":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHsSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HsSearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeHsSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHsSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeErlangSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.ErlangSearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeErlangSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeErlangSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByScope.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, language, 'language'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeFlowSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.FlowSearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeFlowSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeFlowSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeFlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.FlowSearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeFlowSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeFlowSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeRustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.RustSearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeRustSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeRustSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeLsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.LsifSearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeLsifSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeLsifSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByNameAndLanguage.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, language, 'language'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeSearchByNameAndLanguage

  @staticmethod
  def angle_query(*, name: Optional[str] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.CxxSearchByScope.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeCxxSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByLowerCaseNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseNameAndLanguage.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, language, 'language'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeSearchByLowerCaseNameAndLanguage

  @staticmethod
  def angle_query(*, name: Optional[str] = None, language: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.SearchByLowerCaseName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.HackSearchByScope.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", codeHackSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodeHackSearchByScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodePythonSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.code.PythonSearchByName.16 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", codePythonSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchCodePythonSearchByName":
    raise Exception("this function can only be called from @angle_query")




