# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
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
    return f"rust.EnumDef.1 { { } }", EnumDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustEnumDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefinitionUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.DefinitionUses.1 { { } }", DefinitionUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustDefinitionUses":
    raise Exception("this function can only be called from @angle_query")

class RustTraitDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.TraitDef.1 { { } }", TraitDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustTraitDef":
    raise Exception("this function can only be called from @angle_query")

class RustImplLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ImplLocation.1 { { } }", ImplLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustImplLocation":
    raise Exception("this function can only be called from @angle_query")

class RustModuleDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ModuleDef.1 { { } }", ModuleDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustModuleDef":
    raise Exception("this function can only be called from @angle_query")

class RustStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.StaticDef.1 { { } }", StaticDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustStaticDef":
    raise Exception("this function can only be called from @angle_query")

class RustName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.Name.1 { json.dumps(key) }", Name

  @staticmethod
  def angle_query(*, name: str) -> "RustName":
    raise Exception("this function can only be called from @angle_query")

class RustImpl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.Impl.1 { { } }", Impl

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustImpl":
    raise Exception("this function can only be called from @angle_query")

class RustNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.NameLowerCase.1 { { } }", NameLowerCase

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class RustStructDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.StructDef.1 { { } }", StructDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustStructDef":
    raise Exception("this function can only be called from @angle_query")

class RustTupleVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.TupleVariantDef.1 { { } }", TupleVariantDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustTupleVariantDef":
    raise Exception("this function can only be called from @angle_query")

class RustForeignStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ForeignStaticDef.1 { { } }", ForeignStaticDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustForeignStaticDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.DefLocation.1 { { } }", DefLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustDefLocation":
    raise Exception("this function can only be called from @angle_query")

class RustConstDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ConstDef.1 { { } }", ConstDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustConstDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.DefinitionName.1 { { } }", DefinitionName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class RustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.SearchByName.1 { { } }", SearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class RustFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.FileDefinition.1 { { } }", FileDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class RustFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.FileXRefs.1 { { } }", FileXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class RustUnionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.UnionDef.1 { { } }", UnionDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustUnionDef":
    raise Exception("this function can only be called from @angle_query")

class RustFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.FieldDef.1 { { } }", FieldDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustFieldDef":
    raise Exception("this function can only be called from @angle_query")

class RustFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.FunctionDef.1 { { } }", FunctionDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustFunctionDef":
    raise Exception("this function can only be called from @angle_query")

class RustQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.QName.1 { { } }", QName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustQName":
    raise Exception("this function can only be called from @angle_query")

class RustTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.TypeDef.1 { { } }", TypeDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustTypeDef":
    raise Exception("this function can only be called from @angle_query")

class RustStructVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.StructVariantDef.1 { { } }", StructVariantDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustStructVariantDef":
    raise Exception("this function can only be called from @angle_query")

class RustType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.Type.1 { { } }", Type

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustType":
    raise Exception("this function can only be called from @angle_query")

class RustMethodDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.MethodDef.1 { { } }", MethodDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustMethodDef":
    raise Exception("this function can only be called from @angle_query")

class RustXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.XRef.1 { { } }", XRef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustXRef":
    raise Exception("this function can only be called from @angle_query")

class RustLocalDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.LocalDef.1 { { } }", LocalDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustLocalDef":
    raise Exception("this function can only be called from @angle_query")

class RustForeignFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"rust.ForeignFunctionDef.1 { { } }", ForeignFunctionDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "RustForeignFunctionDef":
    raise Exception("this function can only be called from @angle_query")


