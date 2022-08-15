# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSTestinfraCoveredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredFile.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredFile":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraAssemblyByTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.AssemblyByTag.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraAssemblyByTag":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraTestId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.TestId.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: int) -> "GSTestinfraTestId":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraMeasuredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.MeasuredFileOnly.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraMeasuredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredOrLoadedFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredOrLoadedFileTestIds.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredOrLoadedFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileByPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredFileByPushBlockingAssembly.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredFileByPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredFileTestIds.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraDatabaseMetadataField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.DatabaseMetadataField.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraDatabaseMetadataField":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredAssembly.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredFileTestIds.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraTaggedAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.TaggedAssembly.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraTaggedAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.FileMetadata.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraDatabaseMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.DatabaseMetadata.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraDatabaseMetadata":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredFolder.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredFolder":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileByTagAndAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredFileByTagAndAssembly.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredFileByTagAndAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.FbId.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: int) -> "GSTestinfraFbId":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredFileOnly.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.Assemblies.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraAssemblies":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.FileMetadata.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.Folder.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraFolder":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraAssemblyId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.AssemblyId.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraAssemblyId":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraMeasuredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.MeasuredFile.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraMeasuredFile":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraContainsPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.ContainsPushBlockingAssembly.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraContainsPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.Tag.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraTag":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"testinfra.CoveredFileAssemblies.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSTestinfraCoveredFileAssemblies":
    raise Exception("this function can only be called from @angle_query")


