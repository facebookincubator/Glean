# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.testinfra.types import (
    CoveredFile,
    AssemblyByTag,
    TestId,
    MeasuredFileOnly,
    CoveredOrLoadedFileTestIds,
    CoveredFileByPushBlockingAssembly,
    CoveredFileTestIds,
    DatabaseMetadataField,
    CoveredAssembly,
    CoveredFileTestIds,
    TaggedAssembly,
    FileMetadata,
    DatabaseMetadata,
    CoveredFolder,
    CoveredFileByTagAndAssembly,
    FbId,
    CoveredFileOnly,
    Assemblies,
    FileMetadata,
    Folder,
    AssemblyId,
    MeasuredFile,
    ContainsPushBlockingAssembly,
    Tag,
    CoveredFileAssemblies,
)


class TestinfraCoveredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFile.3 { { } }", CoveredFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyByTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.AssemblyByTag.4 { { } }", AssemblyByTag

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraAssemblyByTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTestId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.TestId.1 { json.dumps(key) }", TestId

  @staticmethod
  def angle_query(*, name: int) -> "TestinfraTestId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.MeasuredFileOnly.4 { { } }", MeasuredFileOnly

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraMeasuredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredOrLoadedFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredOrLoadedFileTestIds.5 { { } }", CoveredOrLoadedFileTestIds

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredOrLoadedFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileByPushBlockingAssembly.4 { { } }", CoveredFileByPushBlockingAssembly

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredFileByPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileTestIds.5 { { } }", CoveredFileTestIds

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadataField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.DatabaseMetadataField.4 { json.dumps(key) }", DatabaseMetadataField

  @staticmethod
  def angle_query(*, name: str) -> "TestinfraDatabaseMetadataField":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredAssembly.4 { { } }", CoveredAssembly

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileTestIds.4 { { } }", CoveredFileTestIds

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTaggedAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.TaggedAssembly.4 { { } }", TaggedAssembly

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraTaggedAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.FileMetadata.2 { { } }", FileMetadata

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.DatabaseMetadata.1 { { } }", DatabaseMetadata

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraDatabaseMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFolder.2 { { } }", CoveredFolder

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByTagAndAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileByTagAndAssembly.4 { { } }", CoveredFileByTagAndAssembly

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredFileByTagAndAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.FbId.4 { json.dumps(key) }", FbId

  @staticmethod
  def angle_query(*, name: int) -> "TestinfraFbId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileOnly.3 { { } }", CoveredFileOnly

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.Assemblies.4 { json.dumps(key) }", Assemblies

  @staticmethod
  def angle_query(*, name: str) -> "TestinfraAssemblies":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.FileMetadata.4 { { } }", FileMetadata

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.Folder.1 { json.dumps(key) }", Folder

  @staticmethod
  def angle_query(*, name: str) -> "TestinfraFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.AssemblyId.4 { json.dumps(key) }", AssemblyId

  @staticmethod
  def angle_query(*, name: str) -> "TestinfraAssemblyId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.MeasuredFile.4 { { } }", MeasuredFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraMeasuredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraContainsPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.ContainsPushBlockingAssembly.4 { { } }", ContainsPushBlockingAssembly

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraContainsPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.Tag.4 { json.dumps(key) }", Tag

  @staticmethod
  def angle_query(*, name: str) -> "TestinfraTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileAssemblies.4 { { } }", CoveredFileAssemblies

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "TestinfraCoveredFileAssemblies":
    raise Exception("this function can only be called from @angle_query")


