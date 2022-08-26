# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFile.3 {{ }}", CoveredFile
    return f"testinfra.CoveredFile.3 { concatenateFields(key) }", CoveredFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, coverage: Optional[Tuple[()]] = None) -> "TestinfraCoveredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyByTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.AssemblyByTag.4 {{ }}", AssemblyByTag
    return f"testinfra.AssemblyByTag.4 { concatenateFields(key) }", AssemblyByTag

  @staticmethod
  def angle_query(*, tag: Optional[Tuple[()]] = None, testId: Optional[Tuple[()]] = None) -> "TestinfraAssemblyByTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTestId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.TestId.1 {{ }}", TestId
    return f"testinfra.TestId.1 {key}", TestId

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "TestinfraTestId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.MeasuredFileOnly.4 {{ }}", MeasuredFileOnly
    return f"testinfra.MeasuredFileOnly.4 { concatenateFields(key) }", MeasuredFileOnly

  @staticmethod
  def angle_query(*, measuredFile: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "TestinfraMeasuredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredOrLoadedFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredOrLoadedFileTestIds.5 {{ }}", CoveredOrLoadedFileTestIds
    return f"testinfra.CoveredOrLoadedFileTestIds.5 { concatenateFields(key) }", CoveredOrLoadedFileTestIds

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraCoveredOrLoadedFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileByPushBlockingAssembly.4 {{ }}", CoveredFileByPushBlockingAssembly
    return f"testinfra.CoveredFileByPushBlockingAssembly.4 { concatenateFields(key) }", CoveredFileByPushBlockingAssembly

  @staticmethod
  def angle_query(*, coveredFile: Optional[Tuple[()]] = None, assemblyId: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileByPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileTestIds.5 {{ }}", CoveredFileTestIds
    return f"testinfra.CoveredFileTestIds.5 { concatenateFields(key) }", CoveredFileTestIds

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadataField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.DatabaseMetadataField.4 {{ }}", DatabaseMetadataField
    return f"testinfra.DatabaseMetadataField.4 {key}", DatabaseMetadataField

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraDatabaseMetadataField":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredAssembly.4 {{ }}", CoveredAssembly
    return f"testinfra.CoveredAssembly.4 { concatenateFields(key) }", CoveredAssembly

  @staticmethod
  def angle_query(*, assemblyId: Optional[Tuple[()]] = None, root: Optional[Tuple[()]] = None) -> "TestinfraCoveredAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileTestIds.4 {{ }}", CoveredFileTestIds
    return f"testinfra.CoveredFileTestIds.4 { concatenateFields(key) }", CoveredFileTestIds

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTaggedAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.TaggedAssembly.4 {{ }}", TaggedAssembly
    return f"testinfra.TaggedAssembly.4 { concatenateFields(key) }", TaggedAssembly

  @staticmethod
  def angle_query(*, assemblyId: Optional[Tuple[()]] = None, tag: Optional[Tuple[()]] = None) -> "TestinfraTaggedAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.FileMetadata.2 {{ }}", FileMetadata
    return f"testinfra.FileMetadata.2 { concatenateFields(key) }", FileMetadata

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, hash: Optional[Tuple[()]] = None, length: Optional[Tuple[()]] = None, nonexecutableRanges: Optional[Tuple[()]] = None) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.DatabaseMetadata.1 {{ }}", DatabaseMetadata
    return f"testinfra.DatabaseMetadata.1 { concatenateFields(key) }", DatabaseMetadata

  @staticmethod
  def angle_query(*, field: Optional[str] = None, serializedValue: Optional[str] = None) -> "TestinfraDatabaseMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFolder.2 {{ }}", CoveredFolder
    return f"testinfra.CoveredFolder.2 { concatenateFields(key) }", CoveredFolder

  @staticmethod
  def angle_query(*, folder: Optional[Tuple[()]] = None, folders: Optional[Tuple[()]] = None, files: Optional[Tuple[()]] = None) -> "TestinfraCoveredFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByTagAndAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileByTagAndAssembly.4 {{ }}", CoveredFileByTagAndAssembly
    return f"testinfra.CoveredFileByTagAndAssembly.4 { concatenateFields(key) }", CoveredFileByTagAndAssembly

  @staticmethod
  def angle_query(*, coveredFile: Optional[Tuple[()]] = None, tag: Optional[Tuple[()]] = None, assemblyId: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileByTagAndAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.FbId.4 {{ }}", FbId
    return f"testinfra.FbId.4 {key}", FbId

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "TestinfraFbId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileOnly.3 {{ }}", CoveredFileOnly
    return f"testinfra.CoveredFileOnly.3 { concatenateFields(key) }", CoveredFileOnly

  @staticmethod
  def angle_query(*, coveredFile: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.Assemblies.4 {{ }}", Assemblies
    return f"testinfra.Assemblies.4 {key}", Assemblies

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "TestinfraAssemblies":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.FileMetadata.4 {{ }}", FileMetadata
    return f"testinfra.FileMetadata.4 { concatenateFields(key) }", FileMetadata

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, hash: Optional[Tuple[()]] = None, length: Optional[Tuple[()]] = None, nonexecutableRanges: Optional[Tuple[()]] = None, executableLength: Optional[Tuple[()]] = None) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.Folder.1 {{ }}", Folder
    return f"testinfra.Folder.1 {key}", Folder

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.AssemblyId.4 {{ }}", AssemblyId
    return f"testinfra.AssemblyId.4 {key}", AssemblyId

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "TestinfraAssemblyId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.MeasuredFile.4 {{ }}", MeasuredFile
    return f"testinfra.MeasuredFile.4 { concatenateFields(key) }", MeasuredFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraMeasuredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraContainsPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.ContainsPushBlockingAssembly.4 {{ }}", ContainsPushBlockingAssembly
    return f"testinfra.ContainsPushBlockingAssembly.4 { concatenateFields(key) }", ContainsPushBlockingAssembly

  @staticmethod
  def angle_query(*, assemblies: Optional[Tuple[()]] = None, assembly: Optional[Tuple[()]] = None) -> "TestinfraContainsPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.Tag.4 {{ }}", Tag
    return f"testinfra.Tag.4 {key}", Tag

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"testinfra.CoveredFileAssemblies.4 {{ }}", CoveredFileAssemblies
    return f"testinfra.CoveredFileAssemblies.4 { concatenateFields(key) }", CoveredFileAssemblies

  @staticmethod
  def angle_query(*, coveredFile: Optional[Tuple[()]] = None, assemblies: Optional[Tuple[()]] = None) -> "TestinfraCoveredFileAssemblies":
    raise Exception("this function can only be called from @angle_query")


