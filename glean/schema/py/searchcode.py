# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.code import *
from glean.schema.py.cxx1 import *
from glean.schema.py.hack import *
from glean.schema.py.python import *


from glean.schema.search_code.types import (
    CxxSearchByNameAndScopeFact,
    SearchByLowerCaseScope,
    PythonSearchByLowerCaseName,
    ErlangSearchByName,
    HsSearchByName,
    HackSearchByLowerCaseScope,
    CxxSearchByName,
    SearchByName,
    HackSearchByName,
    HackSearchByScopeWithName,
    CxxSearchByLowerCaseScope,
    PythonSearchByLocalNameFact,
    HackSearchByLowerCaseName,
    LsifSearchByLowerCaseName,
    RustSearchByLowerCaseName,
    CxxSearchByLowerCaseName,
    PythonSearchByNameFact,
    HsSearchByLowerCaseName,
    ErlangSearchByLowerCaseName,
    SearchByScope,
    FlowSearchByLowerCaseName,
    FlowSearchByName,
    RustSearchByName,
    LsifSearchByName,
    SearchByNameAndLanguage,
    CxxSearchByScope,
    SearchByLowerCaseNameAndLanguage,
    SearchByLowerCaseName,
    HackSearchByScope,
    PythonSearchByName,
)


class SearchCodeCxxSearchByNameAndScopeFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')]))
    return f"search.code.CxxSearchByNameAndScopeFact.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxSearchByNameAndScopeFact

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional["Cxx1Scope"] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeCxxSearchByNameAndScopeFact":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, language, 'language'), angle_for(__env, entity, 'entity')]))
    return f"search.code.SearchByLowerCaseScope.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, language: Optional["CodeLanguage"] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodePythonSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.PythonSearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PythonSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodePythonSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.ErlangSearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ErlangSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.HsSearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", HsSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeHsSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')]))
    return f"search.code.HackSearchByLowerCaseScope.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", HackSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeHackSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeCxxSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.CxxSearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeCxxSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.SearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.HackSearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", HackSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeHackSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByScopeWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')]))
    return f"search.code.HackSearchByScopeWithName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", HackSearchByScopeWithName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, scope: Optional[List[str]] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeHackSearchByScopeWithName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeCxxSearchByLowerCaseScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')]))
    return f"search.code.CxxSearchByLowerCaseScope.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxSearchByLowerCaseScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeCxxSearchByLowerCaseScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodePythonSearchByLocalNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.PythonSearchByLocalNameFact.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PythonSearchByLocalNameFact

  @staticmethod
  def angle_query(*, name: Optional["PythonLocalName"] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodePythonSearchByLocalNameFact":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.HackSearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", HackSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeHackSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeLsifSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.LsifSearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LsifSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeLsifSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeRustSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.RustSearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RustSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeRustSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeCxxSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.CxxSearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeCxxSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodePythonSearchByNameFact(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.PythonSearchByNameFact.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PythonSearchByNameFact

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodePythonSearchByNameFact":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHsSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.HsSearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", HsSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeHsSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeErlangSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.ErlangSearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ErlangSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeErlangSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, language, 'language'), angle_for(__env, entity, 'entity')]))
    return f"search.code.SearchByScope.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, language: Optional["CodeLanguage"] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeSearchByScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeFlowSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.FlowSearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowSearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeFlowSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeFlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.FlowSearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeFlowSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeRustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.RustSearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RustSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeRustSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeLsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.LsifSearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LsifSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeLsifSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, language, 'language'), angle_for(__env, entity, 'entity')]))
    return f"search.code.SearchByNameAndLanguage.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByNameAndLanguage

  @staticmethod
  def angle_query(*, name: Optional[str] = None, language: Optional["CodeLanguage"] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeSearchByNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')]))
    return f"search.code.CxxSearchByScope.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByLowerCaseNameAndLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, language: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, language, 'language'), angle_for(__env, entity, 'entity')]))
    return f"search.code.SearchByLowerCaseNameAndLanguage.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByLowerCaseNameAndLanguage

  @staticmethod
  def angle_query(*, name: Optional[str] = None, language: Optional["CodeLanguage"] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeSearchByLowerCaseNameAndLanguage":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeSearchByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.SearchByLowerCaseName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeSearchByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class SearchCodeHackSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')]))
    return f"search.code.HackSearchByScope.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", HackSearchByScope

  @staticmethod
  def angle_query(*, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodeHackSearchByScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCodePythonSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.code.PythonSearchByName.16 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PythonSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None) -> "SearchCodePythonSearchByName":
    raise Exception("this function can only be called from @angle_query")






