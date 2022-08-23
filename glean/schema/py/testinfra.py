# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"testinfra.CoveredFile.3 {{ }}", CoveredFile
    return f"testinfra.CoveredFile.3 {{ file = _, coverage = _ }}", CoveredFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, coverage: Optional[Tuple[()]] = None) -> "TestinfraCoveredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyByTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.AssemblyByTag.4 {{ }}", AssemblyByTag
    return f"testinfra.AssemblyByTag.4 {{ tag = _, testId = _ }}", AssemblyByTag

  @staticmethod
  def angle_query(*, tag: Optional[Tuple[()]] = None, testId: Optional[Tuple[()]] = None) -> "TestinfraAssemblyByTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTestId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.TestId.1 {{ }}", TestId
    return f"testinfra.TestId.1 {json.dumps(key)}", TestId

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "TestinfraTestId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.MeasuredFileOnly.4 {{ }}", MeasuredFileOnly
    return f"testinfra.MeasuredFileOnly.4 {{ measuredFile = _, file = _ }}", MeasuredFileOnly

  @staticmethod
  def angle_query(*, measuredFile: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "TestinfraMeasuredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredOrLoadedFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredOrLoadedFileTestIds.5 {{ }}", CoveredOrLoadedFileTestIds
    return f"testinfra.CoveredOrLoadedFileTestIds.5 {{ file = _, assemblies = _ }}", CoveredOrLoadedFileTestIds

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraCoveredOrLoadedFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileByPushBlockingAssembly.4 {{ }}", CoveredFileByPushBlockingAssembly
    return f"testinfra.CoveredFileByPushBlockingAssembly.4 {{ coveredFile = _, assemblyId = _ }}", CoveredFileByPushBlockingAssembly

  @staticmethod
  def angle_query(*, coveredFile: Optional[Tuple[()]] = None, assemblyId: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileByPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileTestIds.5 {{ }}", CoveredFileTestIds
    return f"testinfra.CoveredFileTestIds.5 {{ file = _, assemblies = _ }}", CoveredFileTestIds

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadataField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.DatabaseMetadataField.4 {{ }}", DatabaseMetadataField
    return f"testinfra.DatabaseMetadataField.4 {json.dumps(key)}", DatabaseMetadataField

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraDatabaseMetadataField":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredAssembly.4 {{ }}", CoveredAssembly
    return f"testinfra.CoveredAssembly.4 {{ assemblyId = _, root = _ }}", CoveredAssembly

  @staticmethod
  def angle_query(*, assemblyId: Optional[Tuple[()]] = None, root: Optional[Tuple[()]] = None) -> "TestinfraCoveredAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileTestIds.4 {{ }}", CoveredFileTestIds
    return f"testinfra.CoveredFileTestIds.4 {{ file = _, assemblies = _ }}", CoveredFileTestIds

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTaggedAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.TaggedAssembly.4 {{ }}", TaggedAssembly
    return f"testinfra.TaggedAssembly.4 {{ assemblyId = _, tag = _ }}", TaggedAssembly

  @staticmethod
  def angle_query(*, assemblyId: Optional[Tuple[()]] = None, tag: Optional[Tuple[()]] = None) -> "TestinfraTaggedAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.FileMetadata.2 {{ }}", FileMetadata
    return f"testinfra.FileMetadata.2 {{ file = _, hash = _, length = _, nonexecutableRanges = _ }}", FileMetadata

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, hash: Optional[Tuple[()]] = None, length: Optional[Tuple[()]] = None, nonexecutableRanges: Optional[Tuple[()]] = None) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.DatabaseMetadata.1 {{ }}", DatabaseMetadata
    return f"testinfra.DatabaseMetadata.1 {{ field = _, serializedValue = _ }}", DatabaseMetadata

  @staticmethod
  def angle_query(*, field: Optional[str] = None, serializedValue: Optional[str] = None) -> "TestinfraDatabaseMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFolder.2 {{ }}", CoveredFolder
    return f"testinfra.CoveredFolder.2 {{ folder = _, folders = _, files = _ }}", CoveredFolder

  @staticmethod
  def angle_query(*, folder: Optional[Tuple[()]] = None, folders: Optional[Tuple[()]] = None, files: Optional[Tuple[()]] = None) -> "TestinfraCoveredFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByTagAndAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileByTagAndAssembly.4 {{ }}", CoveredFileByTagAndAssembly
    return f"testinfra.CoveredFileByTagAndAssembly.4 {{ coveredFile = _, tag = _, assemblyId = _ }}", CoveredFileByTagAndAssembly

  @staticmethod
  def angle_query(*, coveredFile: Optional[Tuple[()]] = None, tag: Optional[Tuple[()]] = None, assemblyId: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileByTagAndAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.FbId.4 {{ }}", FbId
    return f"testinfra.FbId.4 {json.dumps(key)}", FbId

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "TestinfraFbId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileOnly.3 {{ }}", CoveredFileOnly
    return f"testinfra.CoveredFileOnly.3 {{ coveredFile = _, file = _ }}", CoveredFileOnly

  @staticmethod
  def angle_query(*, coveredFile: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.Assemblies.4 {{ }}", Assemblies
    return f"testinfra.Assemblies.4 {json.dumps(key)}", Assemblies

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "TestinfraAssemblies":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.FileMetadata.4 {{ }}", FileMetadata
    return f"testinfra.FileMetadata.4 {{ file = _, hash = _, length = _, nonexecutableRanges = _, executableLength = _ }}", FileMetadata

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, hash: Optional[Tuple[()]] = None, length: Optional[Tuple[()]] = None, nonexecutableRanges: Optional[Tuple[()]] = None, executableLength: Optional[Tuple[()]] = None) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.Folder.1 {{ }}", Folder
    return f"testinfra.Folder.1 {json.dumps(key)}", Folder

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.AssemblyId.4 {{ }}", AssemblyId
    return f"testinfra.AssemblyId.4 {json.dumps(key)}", AssemblyId

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "TestinfraAssemblyId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.MeasuredFile.4 {{ }}", MeasuredFile
    return f"testinfra.MeasuredFile.4 {{ file = _, assemblies = _ }}", MeasuredFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraMeasuredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraContainsPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.ContainsPushBlockingAssembly.4 {{ }}", ContainsPushBlockingAssembly
    return f"testinfra.ContainsPushBlockingAssembly.4 {{ assemblies = _, assembly = _ }}", ContainsPushBlockingAssembly

  @staticmethod
  def angle_query(*, assemblies: Optional[Tuple[()]] = None, assembly: Optional[Tuple[()]] = None) -> "TestinfraContainsPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.Tag.4 {{ }}", Tag
    return f"testinfra.Tag.4 {json.dumps(key)}", Tag

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileAssemblies.4 {{ }}", CoveredFileAssemblies
    return f"testinfra.CoveredFileAssemblies.4 {{ coveredFile = _, assemblies = _ }}", CoveredFileAssemblies

  @staticmethod
  def angle_query(*, coveredFile: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileAssemblies":
    raise Exception("this function can only be called from @angle_query")


