# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.rust.types import (
    EnumDef,
    DefinitionUses,
    TraitDef,
    ImplLocation,
    ModuleDef,
    StaticDef,
    Name,
    Impl,
    NameLowerCase,
    StructDef,
    TupleVariantDef,
    ForeignStaticDef,
    DefLocation,
    ConstDef,
    DefinitionName,
    SearchByName,
    FileDefinition,
    FileXRefs,
    UnionDef,
    FieldDef,
    FunctionDef,
    QName,
    TypeDef,
    StructVariantDef,
    Type,
    MethodDef,
    XRef,
    LocalDef,
    ForeignFunctionDef,
)


class RustEnumDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.EnumDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", EnumDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustEnumDef":
    raise Exception("this function can only be called from @angle_query")



class RustDefinitionUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], def_: ast.Expr, file: ast.Expr, spans: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.DefinitionUses.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, def_, 'def_'), angle_for(__env, file, 'file'), angle_for(__env, spans, 'spans')])) or '_' } }}", DefinitionUses

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, spans: Optional[Tuple[()]] = None) -> "RustDefinitionUses":
    raise Exception("this function can only be called from @angle_query")



class RustTraitDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.TraitDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", TraitDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None) -> "RustTraitDef":
    raise Exception("this function can only be called from @angle_query")



class RustImplLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], impl: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.ImplLocation.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, impl, 'impl'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", ImplLocation

  @staticmethod
  def angle_query(*, impl: Optional["RustImpl"] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "RustImplLocation":
    raise Exception("this function can only be called from @angle_query")



class RustModuleDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.ModuleDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", ModuleDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None) -> "RustModuleDef":
    raise Exception("this function can only be called from @angle_query")



class RustStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.StaticDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", StaticDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustStaticDef":
    raise Exception("this function can only be called from @angle_query")



class RustName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.Name.1 { angle_for(__env, arg, None) or '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "RustName":
    raise Exception("this function can only be called from @angle_query")



class RustImpl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.Impl.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind')])) or '_' } }}", Impl

  @staticmethod
  def angle_query(*, kind: Optional[Tuple[()]] = None) -> "RustImpl":
    raise Exception("this function can only be called from @angle_query")



class RustNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.NameLowerCase.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')])) or '_' } }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["RustName"] = None) -> "RustNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class RustStructDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.StructDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", StructDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None) -> "RustStructDef":
    raise Exception("this function can only be called from @angle_query")



class RustTupleVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.TupleVariantDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", TupleVariantDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None) -> "RustTupleVariantDef":
    raise Exception("this function can only be called from @angle_query")



class RustForeignStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.ForeignStaticDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", ForeignStaticDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustForeignStaticDef":
    raise Exception("this function can only be called from @angle_query")



class RustDefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], def_: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.DefLocation.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, def_, 'def_'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DefLocation

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "RustDefLocation":
    raise Exception("this function can only be called from @angle_query")



class RustConstDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.ConstDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", ConstDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustConstDef":
    raise Exception("this function can only be called from @angle_query")



class RustDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], def_: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.DefinitionName.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, def_, 'def_'), angle_for(__env, name, 'name')])) or '_' } }}", DefinitionName

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, name: Optional["RustName"] = None) -> "RustDefinitionName":
    raise Exception("this function can only be called from @angle_query")



class RustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, def_: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.SearchByName.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, def_, 'def_')])) or '_' } }}", SearchByName

  @staticmethod
  def angle_query(*, name: Optional["RustName"] = None, def_: Optional[Tuple[()]] = None) -> "RustSearchByName":
    raise Exception("this function can only be called from @angle_query")



class RustFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, def_: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.FileDefinition.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, def_, 'def_')])) or '_' } }}", FileDefinition

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, def_: Optional[Tuple[()]] = None) -> "RustFileDefinition":
    raise Exception("this function can only be called from @angle_query")



class RustFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.FileXRefs.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')])) or '_' } }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[Tuple[()]] = None) -> "RustFileXRefs":
    raise Exception("this function can only be called from @angle_query")



class RustUnionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.UnionDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", UnionDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None) -> "RustUnionDef":
    raise Exception("this function can only be called from @angle_query")



class RustFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.FieldDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", FieldDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustFieldDef":
    raise Exception("this function can only be called from @angle_query")



class RustFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.FunctionDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", FunctionDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustFunctionDef":
    raise Exception("this function can only be called from @angle_query")



class RustQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local_name: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.QName.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, local_name, 'local_name'), angle_for(__env, parent, 'parent')])) or '_' } }}", QName

  @staticmethod
  def angle_query(*, local_name: Optional["RustName"] = None, parent: Optional[Tuple[()]] = None) -> "RustQName":
    raise Exception("this function can only be called from @angle_query")



class RustTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.TypeDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", TypeDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustTypeDef":
    raise Exception("this function can only be called from @angle_query")



class RustStructVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.StructVariantDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", StructVariantDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None) -> "RustStructVariantDef":
    raise Exception("this function can only be called from @angle_query")



class RustType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], repr: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.Type.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, repr, 'repr')])) or '_' } }}", Type

  @staticmethod
  def angle_query(*, repr: Optional[str] = None) -> "RustType":
    raise Exception("this function can only be called from @angle_query")



class RustMethodDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.MethodDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", MethodDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustMethodDef":
    raise Exception("this function can only be called from @angle_query")



class RustXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, ranges: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.XRef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, ranges, 'ranges')])) or '_' } }}", XRef

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, ranges: Optional[Tuple[()]] = None) -> "RustXRef":
    raise Exception("this function can only be called from @angle_query")



class RustLocalDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.LocalDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", LocalDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustLocalDef":
    raise Exception("this function can only be called from @angle_query")



class RustForeignFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"rust.ForeignFunctionDef.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", ForeignFunctionDef

  @staticmethod
  def angle_query(*, name: Optional["RustQName"] = None, type: Optional["RustType"] = None) -> "RustForeignFunctionDef":
    raise Exception("this function can only be called from @angle_query")




