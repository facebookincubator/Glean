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
    return f"testinfra.CoveredFile.3 {{ file = _, coverage = _ }}", CoveredFile

  @staticmethod
  def angle_query(*, file: Tuple[()], coverage: Tuple[()]) -> "TestinfraCoveredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyByTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.AssemblyByTag.4 {{ tag = _, testId = _ }}", AssemblyByTag

  @staticmethod
  def angle_query(*, tag: Tuple[()], testId: Tuple[()]) -> "TestinfraAssemblyByTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTestId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.TestId.1 {json.dumps(key)}", TestId

  @staticmethod
  def angle_query(*, arg: int) -> "TestinfraTestId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.MeasuredFileOnly.4 {{ measuredFile = _, file = _ }}", MeasuredFileOnly

  @staticmethod
  def angle_query(*, measuredFile: Tuple[()], file: Tuple[()]) -> "TestinfraMeasuredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredOrLoadedFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredOrLoadedFileTestIds.5 {{ file = _, assemblies = _ }}", CoveredOrLoadedFileTestIds

  @staticmethod
  def angle_query(*, file: Tuple[()], assemblies: Tuple[()]) -> "TestinfraCoveredOrLoadedFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileByPushBlockingAssembly.4 {{ coveredFile = _, assemblyId = _ }}", CoveredFileByPushBlockingAssembly

  @staticmethod
  def angle_query(*, coveredFile: Tuple[()], assemblyId: Tuple[()]) -> "TestinfraCoveredFileByPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileTestIds.5 {{ file = _, assemblies = _ }}", CoveredFileTestIds

  @staticmethod
  def angle_query(*, file: Tuple[()], assemblies: Tuple[()]) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadataField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.DatabaseMetadataField.4 {json.dumps(key)}", DatabaseMetadataField

  @staticmethod
  def angle_query(*, arg: str) -> "TestinfraDatabaseMetadataField":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredAssembly.4 {{ assemblyId = _, root = _ }}", CoveredAssembly

  @staticmethod
  def angle_query(*, assemblyId: Tuple[()], root: Tuple[()]) -> "TestinfraCoveredAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileTestIds.4 {{ file = _, assemblies = _ }}", CoveredFileTestIds

  @staticmethod
  def angle_query(*, file: Tuple[()], assemblies: Tuple[()]) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTaggedAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.TaggedAssembly.4 {{ assemblyId = _, tag = _ }}", TaggedAssembly

  @staticmethod
  def angle_query(*, assemblyId: Tuple[()], tag: Tuple[()]) -> "TestinfraTaggedAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.FileMetadata.2 {{ file = _, hash = _, length = _, nonexecutableRanges = _ }}", FileMetadata

  @staticmethod
  def angle_query(*, file: Tuple[()], hash: Tuple[()], length: Tuple[()], nonexecutableRanges: Tuple[()]) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.DatabaseMetadata.1 {{ field = _, serializedValue = _ }}", DatabaseMetadata

  @staticmethod
  def angle_query(*, field: str, serializedValue: str) -> "TestinfraDatabaseMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFolder.2 {{ folder = _, folders = _, files = _ }}", CoveredFolder

  @staticmethod
  def angle_query(*, folder: Tuple[()], folders: Tuple[()], files: Tuple[()]) -> "TestinfraCoveredFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByTagAndAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileByTagAndAssembly.4 {{ coveredFile = _, tag = _, assemblyId = _ }}", CoveredFileByTagAndAssembly

  @staticmethod
  def angle_query(*, coveredFile: Tuple[()], tag: Tuple[()], assemblyId: Tuple[()]) -> "TestinfraCoveredFileByTagAndAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.FbId.4 {json.dumps(key)}", FbId

  @staticmethod
  def angle_query(*, arg: int) -> "TestinfraFbId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileOnly.3 {{ coveredFile = _, file = _ }}", CoveredFileOnly

  @staticmethod
  def angle_query(*, coveredFile: Tuple[()], file: Tuple[()]) -> "TestinfraCoveredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.Assemblies.4 {json.dumps(key)}", Assemblies

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "TestinfraAssemblies":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.FileMetadata.4 {{ file = _, hash = _, length = _, nonexecutableRanges = _, executableLength = _ }}", FileMetadata

  @staticmethod
  def angle_query(*, file: Tuple[()], hash: Tuple[()], length: Tuple[()], nonexecutableRanges: Tuple[()], executableLength: Tuple[()]) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.Folder.1 {json.dumps(key)}", Folder

  @staticmethod
  def angle_query(*, arg: str) -> "TestinfraFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.AssemblyId.4 {json.dumps(key)}", AssemblyId

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "TestinfraAssemblyId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.MeasuredFile.4 {{ file = _, assemblies = _ }}", MeasuredFile

  @staticmethod
  def angle_query(*, file: Tuple[()], assemblies: Tuple[()]) -> "TestinfraMeasuredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraContainsPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.ContainsPushBlockingAssembly.4 {{ assemblies = _, assembly = _ }}", ContainsPushBlockingAssembly

  @staticmethod
  def angle_query(*, assemblies: Tuple[()], assembly: Tuple[()]) -> "TestinfraContainsPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.Tag.4 {json.dumps(key)}", Tag

  @staticmethod
  def angle_query(*, arg: str) -> "TestinfraTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileAssemblies.4 {{ coveredFile = _, assemblies = _ }}", CoveredFileAssemblies

  @staticmethod
  def angle_query(*, coveredFile: Tuple[()], assemblies: Tuple[()]) -> "TestinfraCoveredFileAssemblies":
    raise Exception("this function can only be called from @angle_query")


