# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSRustEnumDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.EnumDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustEnumDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustDefinitionUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.DefinitionUses.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustDefinitionUses":
    raise Exception("this function can only be called from @angle_query")

class GSRustTraitDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.TraitDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustTraitDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustImplLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.ImplLocation.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustImplLocation":
    raise Exception("this function can only be called from @angle_query")

class GSRustModuleDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.ModuleDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustModuleDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.StaticDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustStaticDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.Name.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustName":
    raise Exception("this function can only be called from @angle_query")

class GSRustImpl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.Impl.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustImpl":
    raise Exception("this function can only be called from @angle_query")

class GSRustNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.NameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSRustStructDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.StructDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustStructDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustTupleVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.TupleVariantDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustTupleVariantDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustForeignStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.ForeignStaticDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustForeignStaticDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustDefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.DefLocation.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustDefLocation":
    raise Exception("this function can only be called from @angle_query")

class GSRustConstDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.ConstDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustConstDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.DefinitionName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class GSRustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.SearchByName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSRustFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.FileDefinition.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSRustFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.FileXRefs.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSRustUnionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.UnionDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustUnionDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.FieldDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.FunctionDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustFunctionDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.QName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustQName":
    raise Exception("this function can only be called from @angle_query")

class GSRustTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.TypeDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustStructVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.StructVariantDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustStructVariantDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.Type.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustType":
    raise Exception("this function can only be called from @angle_query")

class GSRustMethodDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.MethodDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustMethodDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.XRef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustXRef":
    raise Exception("this function can only be called from @angle_query")

class GSRustLocalDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.LocalDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustLocalDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustForeignFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"rust.ForeignFunctionDef.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSRustForeignFunctionDef":
    raise Exception("this function can only be called from @angle_query")


