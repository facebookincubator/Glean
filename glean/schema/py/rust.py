# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.EnumDef.1 {{ name = _, type = _ }}", EnumDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustEnumDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefinitionUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.DefinitionUses.1 {{ def_ = _, file = _, spans = _ }}", DefinitionUses

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, spans: Optional[Tuple[()]] = None) -> "RustDefinitionUses":
    raise Exception("this function can only be called from @angle_query")

class RustTraitDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.TraitDef.1 {{ name = _ }}", TraitDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustTraitDef":
    raise Exception("this function can only be called from @angle_query")

class RustImplLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ImplLocation.1 {{ impl = _, file = _, span = _ }}", ImplLocation

  @staticmethod
  def angle_query(*, impl: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "RustImplLocation":
    raise Exception("this function can only be called from @angle_query")

class RustModuleDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ModuleDef.1 {{ name = _ }}", ModuleDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustModuleDef":
    raise Exception("this function can only be called from @angle_query")

class RustStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.StaticDef.1 {{ name = _, type = _ }}", StaticDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustStaticDef":
    raise Exception("this function can only be called from @angle_query")

class RustName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.Name.1 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "RustName":
    raise Exception("this function can only be called from @angle_query")

class RustImpl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.Impl.1 {{ kind = _ }}", Impl

  @staticmethod
  def angle_query(*, kind: Optional[Tuple[()]] = None) -> "RustImpl":
    raise Exception("this function can only be called from @angle_query")

class RustNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.NameLowerCase.1 {{ nameLowerCase = _, name = _ }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "RustNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class RustStructDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.StructDef.1 {{ name = _ }}", StructDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustStructDef":
    raise Exception("this function can only be called from @angle_query")

class RustTupleVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.TupleVariantDef.1 {{ name = _ }}", TupleVariantDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustTupleVariantDef":
    raise Exception("this function can only be called from @angle_query")

class RustForeignStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ForeignStaticDef.1 {{ name = _, type = _ }}", ForeignStaticDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustForeignStaticDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.DefLocation.1 {{ def_ = _, file = _, span = _ }}", DefLocation

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "RustDefLocation":
    raise Exception("this function can only be called from @angle_query")

class RustConstDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ConstDef.1 {{ name = _, type = _ }}", ConstDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustConstDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.DefinitionName.1 {{ def_ = _, name = _ }}", DefinitionName

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None) -> "RustDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class RustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.SearchByName.1 {{ name = _, def_ = _ }}", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, def_: Optional[Tuple[()]] = None) -> "RustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class RustFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.FileDefinition.1 {{ file = _, def_ = _ }}", FileDefinition

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, def_: Optional[Tuple[()]] = None) -> "RustFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class RustFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.FileXRefs.1 {{ file = _, xrefs = _ }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "RustFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class RustUnionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.UnionDef.1 {{ name = _ }}", UnionDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustUnionDef":
    raise Exception("this function can only be called from @angle_query")

class RustFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.FieldDef.1 {{ name = _, type = _ }}", FieldDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustFieldDef":
    raise Exception("this function can only be called from @angle_query")

class RustFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.FunctionDef.1 {{ name = _, type = _ }}", FunctionDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustFunctionDef":
    raise Exception("this function can only be called from @angle_query")

class RustQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.QName.1 {{ local_name = _, parent = _ }}", QName

  @staticmethod
  def angle_query(*, local_name: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "RustQName":
    raise Exception("this function can only be called from @angle_query")

class RustTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.TypeDef.1 {{ name = _, type = _ }}", TypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustTypeDef":
    raise Exception("this function can only be called from @angle_query")

class RustStructVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.StructVariantDef.1 {{ name = _ }}", StructVariantDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustStructVariantDef":
    raise Exception("this function can only be called from @angle_query")

class RustType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.Type.1 {{ repr = _ }}", Type

  @staticmethod
  def angle_query(*, repr: Optional[str] = None) -> "RustType":
    raise Exception("this function can only be called from @angle_query")

class RustMethodDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.MethodDef.1 {{ name = _, type = _ }}", MethodDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustMethodDef":
    raise Exception("this function can only be called from @angle_query")

class RustXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.XRef.1 {{ target = _, ranges = _ }}", XRef

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, ranges: Optional[Tuple[()]] = None) -> "RustXRef":
    raise Exception("this function can only be called from @angle_query")

class RustLocalDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.LocalDef.1 {{ name = _, type = _ }}", LocalDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustLocalDef":
    raise Exception("this function can only be called from @angle_query")

class RustForeignFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ForeignFunctionDef.1 {{ name = _, type = _ }}", ForeignFunctionDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustForeignFunctionDef":
    raise Exception("this function can only be called from @angle_query")


