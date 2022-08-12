# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSRustEnumDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.EnumDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustEnumDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustDefinitionUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.DefinitionUses.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustDefinitionUses":
    raise Exception("this function can only be called from @angle_query")

class GSRustTraitDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.TraitDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustTraitDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustImplLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.ImplLocation.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustImplLocation":
    raise Exception("this function can only be called from @angle_query")

class GSRustModuleDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.ModuleDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustModuleDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.StaticDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustStaticDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.Name.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustName":
    raise Exception("this function can only be called from @angle_query")

class GSRustImpl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.Impl.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustImpl":
    raise Exception("this function can only be called from @angle_query")

class GSRustNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.NameLowerCase.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSRustStructDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.StructDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustStructDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustTupleVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.TupleVariantDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustTupleVariantDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustForeignStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.ForeignStaticDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustForeignStaticDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustDefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.DefLocation.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustDefLocation":
    raise Exception("this function can only be called from @angle_query")

class GSRustConstDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.ConstDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustConstDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.DefinitionName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class GSRustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.SearchByName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSRustFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.FileDefinition.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSRustFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.FileXRefs.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSRustUnionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.UnionDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustUnionDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.FieldDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.FunctionDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustFunctionDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.QName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustQName":
    raise Exception("this function can only be called from @angle_query")

class GSRustTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.TypeDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustStructVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.StructVariantDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustStructVariantDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.Type.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustType":
    raise Exception("this function can only be called from @angle_query")

class GSRustMethodDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.MethodDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustMethodDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.XRef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustXRef":
    raise Exception("this function can only be called from @angle_query")

class GSRustLocalDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.LocalDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustLocalDef":
    raise Exception("this function can only be called from @angle_query")

class GSRustForeignFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"rust.ForeignFunctionDef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSRustForeignFunctionDef":
    raise Exception("this function can only be called from @angle_query")


