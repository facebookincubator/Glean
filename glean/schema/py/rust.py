# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.EnumDef.1 {{ }}", EnumDef
    return f"rust.EnumDef.1 { concatenateFields(key) }", EnumDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustEnumDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefinitionUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.DefinitionUses.1 {{ }}", DefinitionUses
    return f"rust.DefinitionUses.1 { concatenateFields(key) }", DefinitionUses

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, spans: Optional[Tuple[()]] = None) -> "RustDefinitionUses":
    raise Exception("this function can only be called from @angle_query")

class RustTraitDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.TraitDef.1 {{ }}", TraitDef
    return f"rust.TraitDef.1 { concatenateFields(key) }", TraitDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustTraitDef":
    raise Exception("this function can only be called from @angle_query")

class RustImplLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.ImplLocation.1 {{ }}", ImplLocation
    return f"rust.ImplLocation.1 { concatenateFields(key) }", ImplLocation

  @staticmethod
  def angle_query(*, impl: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "RustImplLocation":
    raise Exception("this function can only be called from @angle_query")

class RustModuleDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.ModuleDef.1 {{ }}", ModuleDef
    return f"rust.ModuleDef.1 { concatenateFields(key) }", ModuleDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustModuleDef":
    raise Exception("this function can only be called from @angle_query")

class RustStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.StaticDef.1 {{ }}", StaticDef
    return f"rust.StaticDef.1 { concatenateFields(key) }", StaticDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustStaticDef":
    raise Exception("this function can only be called from @angle_query")

class RustName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.Name.1 {{ }}", Name
    return f"rust.Name.1 {key}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "RustName":
    raise Exception("this function can only be called from @angle_query")

class RustImpl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.Impl.1 {{ }}", Impl
    return f"rust.Impl.1 { concatenateFields(key) }", Impl

  @staticmethod
  def angle_query(*, kind: Optional[Tuple[()]] = None) -> "RustImpl":
    raise Exception("this function can only be called from @angle_query")

class RustNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.NameLowerCase.1 {{ }}", NameLowerCase
    return f"rust.NameLowerCase.1 { concatenateFields(key) }", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "RustNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class RustStructDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.StructDef.1 {{ }}", StructDef
    return f"rust.StructDef.1 { concatenateFields(key) }", StructDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustStructDef":
    raise Exception("this function can only be called from @angle_query")

class RustTupleVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.TupleVariantDef.1 {{ }}", TupleVariantDef
    return f"rust.TupleVariantDef.1 { concatenateFields(key) }", TupleVariantDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustTupleVariantDef":
    raise Exception("this function can only be called from @angle_query")

class RustForeignStaticDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.ForeignStaticDef.1 {{ }}", ForeignStaticDef
    return f"rust.ForeignStaticDef.1 { concatenateFields(key) }", ForeignStaticDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustForeignStaticDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.DefLocation.1 {{ }}", DefLocation
    return f"rust.DefLocation.1 { concatenateFields(key) }", DefLocation

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "RustDefLocation":
    raise Exception("this function can only be called from @angle_query")

class RustConstDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.ConstDef.1 {{ }}", ConstDef
    return f"rust.ConstDef.1 { concatenateFields(key) }", ConstDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustConstDef":
    raise Exception("this function can only be called from @angle_query")

class RustDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.DefinitionName.1 {{ }}", DefinitionName
    return f"rust.DefinitionName.1 { concatenateFields(key) }", DefinitionName

  @staticmethod
  def angle_query(*, def_: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None) -> "RustDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class RustSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.SearchByName.1 {{ }}", SearchByName
    return f"rust.SearchByName.1 { concatenateFields(key) }", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, def_: Optional[Tuple[()]] = None) -> "RustSearchByName":
    raise Exception("this function can only be called from @angle_query")

class RustFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.FileDefinition.1 {{ }}", FileDefinition
    return f"rust.FileDefinition.1 { concatenateFields(key) }", FileDefinition

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, def_: Optional[Tuple[()]] = None) -> "RustFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class RustFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.FileXRefs.1 {{ }}", FileXRefs
    return f"rust.FileXRefs.1 { concatenateFields(key) }", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "RustFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class RustUnionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.UnionDef.1 {{ }}", UnionDef
    return f"rust.UnionDef.1 { concatenateFields(key) }", UnionDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustUnionDef":
    raise Exception("this function can only be called from @angle_query")

class RustFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.FieldDef.1 {{ }}", FieldDef
    return f"rust.FieldDef.1 { concatenateFields(key) }", FieldDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustFieldDef":
    raise Exception("this function can only be called from @angle_query")

class RustFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.FunctionDef.1 {{ }}", FunctionDef
    return f"rust.FunctionDef.1 { concatenateFields(key) }", FunctionDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustFunctionDef":
    raise Exception("this function can only be called from @angle_query")

class RustQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.QName.1 {{ }}", QName
    return f"rust.QName.1 { concatenateFields(key) }", QName

  @staticmethod
  def angle_query(*, local_name: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "RustQName":
    raise Exception("this function can only be called from @angle_query")

class RustTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.TypeDef.1 {{ }}", TypeDef
    return f"rust.TypeDef.1 { concatenateFields(key) }", TypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustTypeDef":
    raise Exception("this function can only be called from @angle_query")

class RustStructVariantDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.StructVariantDef.1 {{ }}", StructVariantDef
    return f"rust.StructVariantDef.1 { concatenateFields(key) }", StructVariantDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "RustStructVariantDef":
    raise Exception("this function can only be called from @angle_query")

class RustType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.Type.1 {{ }}", Type
    return f"rust.Type.1 { concatenateFields(key) }", Type

  @staticmethod
  def angle_query(*, repr: Optional[str] = None) -> "RustType":
    raise Exception("this function can only be called from @angle_query")

class RustMethodDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.MethodDef.1 {{ }}", MethodDef
    return f"rust.MethodDef.1 { concatenateFields(key) }", MethodDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustMethodDef":
    raise Exception("this function can only be called from @angle_query")

class RustXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.XRef.1 {{ }}", XRef
    return f"rust.XRef.1 { concatenateFields(key) }", XRef

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, ranges: Optional[Tuple[()]] = None) -> "RustXRef":
    raise Exception("this function can only be called from @angle_query")

class RustLocalDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.LocalDef.1 {{ }}", LocalDef
    return f"rust.LocalDef.1 { concatenateFields(key) }", LocalDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustLocalDef":
    raise Exception("this function can only be called from @angle_query")

class RustForeignFunctionDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"rust.ForeignFunctionDef.1 {{ }}", ForeignFunctionDef
    return f"rust.ForeignFunctionDef.1 { concatenateFields(key) }", ForeignFunctionDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "RustForeignFunctionDef":
    raise Exception("this function can only be called from @angle_query")


