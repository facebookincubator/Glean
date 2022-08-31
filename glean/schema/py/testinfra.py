# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


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
  def build_angle(__env: Dict[str, R], file: ast.Expr, coverage: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFile.3 {{ file = {angle_for(__env, file)}, coverage = {angle_for(__env, coverage)} }}", CoveredFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, coverage: Optional[Tuple[()]] = None) -> "TestinfraCoveredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyByTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], tag: ast.Expr, testId: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.AssemblyByTag.4 {{ tag = {angle_for(__env, tag)}, testId = {angle_for(__env, testId)} }}", AssemblyByTag

  @staticmethod
  def angle_query(*, tag: Optional["TestinfraTag"] = None, testId: Optional["TestinfraAssemblyId"] = None) -> "TestinfraAssemblyByTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTestId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.TestId.1 {angle_for(__env, arg)}", TestId

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "TestinfraTestId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], measuredFile: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.MeasuredFileOnly.4 {{ measuredFile = {angle_for(__env, measuredFile)}, file = {angle_for(__env, file)} }}", MeasuredFileOnly

  @staticmethod
  def angle_query(*, measuredFile: Optional["TestinfraMeasuredFile"] = None, file: Optional["SrcFile"] = None) -> "TestinfraMeasuredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredOrLoadedFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, assemblies: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredOrLoadedFileTestIds.5 {{ file = {angle_for(__env, file)}, assemblies = {angle_for(__env, assemblies)} }}", CoveredOrLoadedFileTestIds

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, assemblies: Optional["TestinfraTestId"] = None) -> "TestinfraCoveredOrLoadedFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], coveredFile: ast.Expr, assemblyId: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileByPushBlockingAssembly.4 {{ coveredFile = {angle_for(__env, coveredFile)}, assemblyId = {angle_for(__env, assemblyId)} }}", CoveredFileByPushBlockingAssembly

  @staticmethod
  def angle_query(*, coveredFile: Optional["TestinfraCoveredFile"] = None, assemblyId: Optional["TestinfraAssemblyId"] = None) -> "TestinfraCoveredFileByPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, assemblies: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileTestIds.5 {{ file = {angle_for(__env, file)}, assemblies = {angle_for(__env, assemblies)} }}", CoveredFileTestIds

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, assemblies: Optional["TestinfraTestId"] = None) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadataField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.DatabaseMetadataField.4 {angle_for(__env, arg)}", DatabaseMetadataField

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraDatabaseMetadataField":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], assemblyId: ast.Expr, root: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredAssembly.4 {{ assemblyId = {angle_for(__env, assemblyId)}, root = {angle_for(__env, root)} }}", CoveredAssembly

  @staticmethod
  def angle_query(*, assemblyId: Optional["TestinfraAssemblyId"] = None, root: Optional["TestinfraCoveredFolder"] = None) -> "TestinfraCoveredAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileTestIds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, assemblies: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileTestIds.4 {{ file = {angle_for(__env, file)}, assemblies = {angle_for(__env, assemblies)} }}", CoveredFileTestIds

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, assemblies: Optional["TestinfraTestId"] = None) -> "TestinfraCoveredFileTestIds":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTaggedAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], assemblyId: ast.Expr, tag: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.TaggedAssembly.4 {{ assemblyId = {angle_for(__env, assemblyId)}, tag = {angle_for(__env, tag)} }}", TaggedAssembly

  @staticmethod
  def angle_query(*, assemblyId: Optional["TestinfraAssemblyId"] = None, tag: Optional["TestinfraTag"] = None) -> "TestinfraTaggedAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, hash: ast.Expr, length: ast.Expr, nonexecutableRanges: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.FileMetadata.2 {{ file = {angle_for(__env, file)}, hash = {angle_for(__env, hash)}, length = {angle_for(__env, length)}, nonexecutableRanges = {angle_for(__env, nonexecutableRanges)} }}", FileMetadata

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, hash: Optional[Tuple[()]] = None, length: Optional[Tuple[()]] = None, nonexecutableRanges: Optional[Tuple[()]] = None) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraDatabaseMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], field: ast.Expr, serializedValue: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.DatabaseMetadata.1 {{ field = {angle_for(__env, field)}, serializedValue = {angle_for(__env, serializedValue)} }}", DatabaseMetadata

  @staticmethod
  def angle_query(*, field: Optional[str] = None, serializedValue: Optional[str] = None) -> "TestinfraDatabaseMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], folder: ast.Expr, folders: ast.Expr, files: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFolder.2 {{ folder = {angle_for(__env, folder)}, folders = {angle_for(__env, folders)}, files = {angle_for(__env, files)} }}", CoveredFolder

  @staticmethod
  def angle_query(*, folder: Optional["TestinfraFolder"] = None, folders: Optional[Tuple[()]] = None, files: Optional[Tuple[()]] = None) -> "TestinfraCoveredFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileByTagAndAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], coveredFile: ast.Expr, tag: ast.Expr, assemblyId: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileByTagAndAssembly.4 {{ coveredFile = {angle_for(__env, coveredFile)}, tag = {angle_for(__env, tag)}, assemblyId = {angle_for(__env, assemblyId)} }}", CoveredFileByTagAndAssembly

  @staticmethod
  def angle_query(*, coveredFile: Optional["TestinfraCoveredFile"] = None, tag: Optional["TestinfraTag"] = None, assemblyId: Optional["TestinfraAssemblyId"] = None) -> "TestinfraCoveredFileByTagAndAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.FbId.4 {angle_for(__env, arg)}", FbId

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "TestinfraFbId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileOnly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], coveredFile: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileOnly.3 {{ coveredFile = {angle_for(__env, coveredFile)}, file = {angle_for(__env, file)} }}", CoveredFileOnly

  @staticmethod
  def angle_query(*, coveredFile: Optional["TestinfraCoveredFile"] = None, file: Optional["SrcFile"] = None) -> "TestinfraCoveredFileOnly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.Assemblies.4 {angle_for(__env, arg)}", Assemblies

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "TestinfraAssemblies":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFileMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, hash: ast.Expr, length: ast.Expr, nonexecutableRanges: ast.Expr, executableLength: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.FileMetadata.4 {{ file = {angle_for(__env, file)}, hash = {angle_for(__env, hash)}, length = {angle_for(__env, length)}, nonexecutableRanges = {angle_for(__env, nonexecutableRanges)}, executableLength = {angle_for(__env, executableLength)} }}", FileMetadata

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, hash: Optional[Tuple[()]] = None, length: Optional[Tuple[()]] = None, nonexecutableRanges: Optional[Tuple[()]] = None, executableLength: Optional[Tuple[()]] = None) -> "TestinfraFileMetadata":
    raise Exception("this function can only be called from @angle_query")

class TestinfraFolder(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.Folder.1 {angle_for(__env, arg)}", Folder

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraFolder":
    raise Exception("this function can only be called from @angle_query")

class TestinfraAssemblyId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.AssemblyId.4 {angle_for(__env, arg)}", AssemblyId

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "TestinfraAssemblyId":
    raise Exception("this function can only be called from @angle_query")

class TestinfraMeasuredFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, assemblies: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.MeasuredFile.4 {{ file = {angle_for(__env, file)}, assemblies = {angle_for(__env, assemblies)} }}", MeasuredFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, assemblies: Optional["TestinfraAssemblies"] = None) -> "TestinfraMeasuredFile":
    raise Exception("this function can only be called from @angle_query")

class TestinfraContainsPushBlockingAssembly(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], assemblies: ast.Expr, assembly: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.ContainsPushBlockingAssembly.4 {{ assemblies = {angle_for(__env, assemblies)}, assembly = {angle_for(__env, assembly)} }}", ContainsPushBlockingAssembly

  @staticmethod
  def angle_query(*, assemblies: Optional["TestinfraAssemblies"] = None, assembly: Optional["TestinfraAssemblyId"] = None) -> "TestinfraContainsPushBlockingAssembly":
    raise Exception("this function can only be called from @angle_query")

class TestinfraTag(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.Tag.4 {angle_for(__env, arg)}", Tag

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "TestinfraTag":
    raise Exception("this function can only be called from @angle_query")

class TestinfraCoveredFileAssemblies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], coveredFile: ast.Expr, assemblies: ast.Expr) -> Tuple[str, Struct]:
    return f"testinfra.CoveredFileAssemblies.4 {{ coveredFile = {angle_for(__env, coveredFile)}, assemblies = {angle_for(__env, assemblies)} }}", CoveredFileAssemblies

  @staticmethod
  def angle_query(*, coveredFile: Optional["TestinfraCoveredFile"] = None, assemblies: Optional["TestinfraAssemblies"] = None) -> "TestinfraCoveredFileAssemblies":
    raise Exception("this function can only be called from @angle_query")


