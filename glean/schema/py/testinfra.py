# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSTestinfraCoveredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredFile.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredFile":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraAssemblyByTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.AssemblyByTag.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraAssemblyByTag":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraTestId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.TestId.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraTestId":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraMeasuredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.MeasuredFileOnly.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraMeasuredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredOrLoadedFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredOrLoadedFileTestIds.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredOrLoadedFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileByPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredFileByPushBlockingAssembly.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredFileByPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredFileTestIds.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraDatabaseMetadataField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.DatabaseMetadataField.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraDatabaseMetadataField":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredAssembly.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredFileTestIds.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraTaggedAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.TaggedAssembly.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraTaggedAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.FileMetadata.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraDatabaseMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.DatabaseMetadata.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraDatabaseMetadata":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredFolder.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredFolder":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileByTagAndAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredFileByTagAndAssembly.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredFileByTagAndAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.FbId.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraFbId":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredFileOnly.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.Assemblies.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraAssemblies":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.FileMetadata.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.Folder.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraFolder":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraAssemblyId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.AssemblyId.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraAssemblyId":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraMeasuredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.MeasuredFile.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraMeasuredFile":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraContainsPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.ContainsPushBlockingAssembly.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraContainsPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.Tag.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraTag":
    raise Exception("this function can only be called from @angle_query")

class GSTestinfraCoveredFileAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"testinfra.CoveredFileAssemblies.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSTestinfraCoveredFileAssemblies":
    raise Exception("this function can only be called from @angle_query")


