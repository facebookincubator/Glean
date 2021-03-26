-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Src.Types


instance Glean.Predicate Glean.Schema.Testinfra.Types.TestId where
  type KeyType Glean.Schema.Testinfra.Types.TestId = Glean.Nat
  getName _proxy  = Glean.PredicateRef "testinfra.TestId"1
  getIndex _proxy  = 490
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.testId_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.TestId x k
  getFactKey = Glean.Schema.Testinfra.Types.testId_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.TestId where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Testinfra.Types.FbId where
  type KeyType Glean.Schema.Testinfra.Types.FbId = Glean.Nat
  getName _proxy  = Glean.PredicateRef "testinfra.FbId"4
  getIndex _proxy  = 433
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.fbId_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.FbId x k
  getFactKey = Glean.Schema.Testinfra.Types.fbId_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.FbId where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFolder_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoveredFolder_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoveredFolder_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoveredFolder_key = 'Angle.TField "folder" (Glean.KeyType Glean.Schema.Testinfra.Types.Folder) ('Angle.TField "folders" ([Glean.KeyType Glean.Schema.Testinfra.Types.CoveredFolder]) ('Angle.TField "files" ([Glean.KeyType Glean.Schema.Testinfra.Types.CoveredFile]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Testinfra.Types.CoveredFolder where
  type KeyType Glean.Schema.Testinfra.Types.CoveredFolder =
    Glean.Schema.Testinfra.Types.CoveredFolder_key
  getName _proxy  = Glean.PredicateRef "testinfra.CoveredFolder"2
  getIndex _proxy  = 414
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.coveredFolder_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.CoveredFolder x k
  getFactKey = Glean.Schema.Testinfra.Types.coveredFolder_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFolder where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.MeasuredFile_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.MeasuredFile_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.MeasuredFile_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.MeasuredFile_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "assemblies" (Glean.KeyType Glean.Schema.Testinfra.Types.Assemblies) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.MeasuredFile where
  type KeyType Glean.Schema.Testinfra.Types.MeasuredFile =
    Glean.Schema.Testinfra.Types.MeasuredFile_key
  getName _proxy  = Glean.PredicateRef "testinfra.MeasuredFile"4
  getIndex _proxy  = 412
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.measuredFile_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.MeasuredFile x k
  getFactKey = Glean.Schema.Testinfra.Types.measuredFile_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.MeasuredFile where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Testinfra.Types.Tag where
  type KeyType Glean.Schema.Testinfra.Types.Tag = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "testinfra.Tag"4
  getIndex _proxy  = 405
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.tag_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.Tag x k
  getFactKey = Glean.Schema.Testinfra.Types.tag_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.Tag where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key = 'Angle.TField "assemblies" (Glean.KeyType Glean.Schema.Testinfra.Types.Assemblies) ('Angle.TField "assembly" (Glean.KeyType Glean.Schema.Testinfra.Types.AssemblyId) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly where
  type KeyType Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly =
    Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key
  getName _proxy  =
    Glean.PredicateRef "testinfra.ContainsPushBlockingAssembly"4
  getIndex _proxy  = 400
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.containsPushBlockingAssembly_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly x k
  getFactKey = Glean.Schema.Testinfra.Types.containsPushBlockingAssembly_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key = 'Angle.TField "coveredFile" (Glean.KeyType Glean.Schema.Testinfra.Types.CoveredFile) ('Angle.TField "assemblies" (Glean.KeyType Glean.Schema.Testinfra.Types.Assemblies) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.CoveredFileAssemblies where
  type KeyType Glean.Schema.Testinfra.Types.CoveredFileAssemblies =
    Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key
  getName _proxy  = Glean.PredicateRef "testinfra.CoveredFileAssemblies"4
  getIndex _proxy  = 396
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.coveredFileAssemblies_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.CoveredFileAssemblies x k
  getFactKey = Glean.Schema.Testinfra.Types.coveredFileAssemblies_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileAssemblies where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key = 'Angle.TField "coveredFile" (Glean.KeyType Glean.Schema.Testinfra.Types.CoveredFile) ('Angle.TField "assemblyId" (Glean.KeyType Glean.Schema.Testinfra.Types.AssemblyId) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly where
  type KeyType Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly =
    Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key
  getName _proxy  =
    Glean.PredicateRef "testinfra.CoveredFileByPushBlockingAssembly"4
  getIndex _proxy  = 335
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.coveredFileByPushBlockingAssembly_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly x k
  getFactKey = Glean.Schema.Testinfra.Types.coveredFileByPushBlockingAssembly_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.MeasuredFileOnly_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.MeasuredFileOnly_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.MeasuredFileOnly_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.MeasuredFileOnly_key = 'Angle.TField "measuredFile" (Glean.KeyType Glean.Schema.Testinfra.Types.MeasuredFile) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.MeasuredFileOnly where
  type KeyType Glean.Schema.Testinfra.Types.MeasuredFileOnly =
    Glean.Schema.Testinfra.Types.MeasuredFileOnly_key
  getName _proxy  = Glean.PredicateRef "testinfra.MeasuredFileOnly"4
  getIndex _proxy  = 304
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.measuredFileOnly_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.MeasuredFileOnly x k
  getFactKey = Glean.Schema.Testinfra.Types.measuredFileOnly_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.MeasuredFileOnly where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.FileMetadata_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.FileMetadata_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Testinfra.Types.FileMetadata_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.FileMetadata_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "hash" ([Glean.Schema.Testinfra.Types.FileHash]) ('Angle.TField "length" (Prelude.Maybe Glean.Schema.Testinfra.Types.FileLength) ('Angle.TField "nonexecutableRanges" (Prelude.Maybe Glean.Schema.Testinfra.Types.CoverageRange) ('Angle.TField "executableLength" (Prelude.Maybe Glean.Schema.Testinfra.Types.FileLength) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Testinfra.Types.FileMetadata where
  type KeyType Glean.Schema.Testinfra.Types.FileMetadata =
    Glean.Schema.Testinfra.Types.FileMetadata_key
  getName _proxy  = Glean.PredicateRef "testinfra.FileMetadata"4
  getIndex _proxy  = 268
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.fileMetadata_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.FileMetadata x k
  getFactKey = Glean.Schema.Testinfra.Types.fileMetadata_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.FileMetadata where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.FileMetadata_2_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.FileMetadata_2_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Testinfra.Types.FileMetadata_2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.FileMetadata_2_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "hash" ([Glean.Schema.Testinfra.Types.FileHash]) ('Angle.TField "length" (Prelude.Maybe Glean.Schema.Testinfra.Types.FileLength) ('Angle.TField "nonexecutableRanges" (Prelude.Maybe Glean.Schema.Testinfra.Types.CoverageRange) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Testinfra.Types.FileMetadata_2 where
  type KeyType Glean.Schema.Testinfra.Types.FileMetadata_2 =
    Glean.Schema.Testinfra.Types.FileMetadata_2_key
  getName _proxy  = Glean.PredicateRef "testinfra.FileMetadata"2
  getIndex _proxy  = 267
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.fileMetadata_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.FileMetadata_2 x k
  getFactKey = Glean.Schema.Testinfra.Types.fileMetadata_2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.FileMetadata_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.AssemblyId_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.AssemblyId_key_testId x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Testinfra.Types.AssemblyId_key_fbId x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Testinfra.Types.AssemblyId_key_testId
    , Glean.mapD Glean.Schema.Testinfra.Types.AssemblyId_key_fbId
    ]

type instance Angle.SumFields Glean.Schema.Testinfra.Types.AssemblyId_key = 'Angle.TField "testId" (Glean.KeyType Glean.Schema.Testinfra.Types.TestId) ('Angle.TField "fbId" (Glean.KeyType Glean.Schema.Testinfra.Types.FbId) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.Testinfra.Types.TestId Glean.Schema.Testinfra.Types.AssemblyId_key where
  injectBranch = Glean.Schema.Testinfra.Types.AssemblyId_key_testId
  projectBranch (Glean.Schema.Testinfra.Types.AssemblyId_key_testId x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Testinfra.Types.FbId Glean.Schema.Testinfra.Types.AssemblyId_key where
  injectBranch = Glean.Schema.Testinfra.Types.AssemblyId_key_fbId
  projectBranch (Glean.Schema.Testinfra.Types.AssemblyId_key_fbId x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Predicate Glean.Schema.Testinfra.Types.AssemblyId where
  type KeyType Glean.Schema.Testinfra.Types.AssemblyId =
    Glean.Schema.Testinfra.Types.AssemblyId_key
  getName _proxy  = Glean.PredicateRef "testinfra.AssemblyId"4
  getIndex _proxy  = 260
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.assemblyId_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.AssemblyId x k
  getFactKey = Glean.Schema.Testinfra.Types.assemblyId_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.AssemblyId where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileTestIds_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoveredFileTestIds_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoveredFileTestIds_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoveredFileTestIds_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "assemblies" (Glean.KeyType Glean.Schema.Testinfra.Types.TestId) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.CoveredFileTestIds where
  type KeyType Glean.Schema.Testinfra.Types.CoveredFileTestIds =
    Glean.Schema.Testinfra.Types.CoveredFileTestIds_key
  getName _proxy  = Glean.PredicateRef "testinfra.CoveredFileTestIds"4
  getIndex _proxy  = 185
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.coveredFileTestIds_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.CoveredFileTestIds x k
  getFactKey = Glean.Schema.Testinfra.Types.coveredFileTestIds_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileTestIds where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.TaggedAssembly_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.TaggedAssembly_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.TaggedAssembly_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.TaggedAssembly_key = 'Angle.TField "assemblyId" (Glean.KeyType Glean.Schema.Testinfra.Types.AssemblyId) ('Angle.TField "tag" (Glean.KeyType Glean.Schema.Testinfra.Types.Tag) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.TaggedAssembly where
  type KeyType Glean.Schema.Testinfra.Types.TaggedAssembly =
    Glean.Schema.Testinfra.Types.TaggedAssembly_key
  getName _proxy  = Glean.PredicateRef "testinfra.TaggedAssembly"4
  getIndex _proxy  = 164
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.taggedAssembly_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.TaggedAssembly x k
  getFactKey = Glean.Schema.Testinfra.Types.taggedAssembly_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.TaggedAssembly where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFile_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoveredFile_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoveredFile_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoveredFile_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "coverage" (Glean.Schema.Testinfra.Types.CoverageGranularity) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.CoveredFile where
  type KeyType Glean.Schema.Testinfra.Types.CoveredFile =
    Glean.Schema.Testinfra.Types.CoveredFile_key
  getName _proxy  = Glean.PredicateRef "testinfra.CoveredFile"3
  getIndex _proxy  = 159
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.coveredFile_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.CoveredFile x k
  getFactKey = Glean.Schema.Testinfra.Types.coveredFile_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFile where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Testinfra.Types.DatabaseMetadataField where
  type KeyType Glean.Schema.Testinfra.Types.DatabaseMetadataField =
    Data.Text.Text
  getName _proxy  = Glean.PredicateRef "testinfra.DatabaseMetadataField"4
  getIndex _proxy  = 157
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.databaseMetadataField_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.DatabaseMetadataField x k
  getFactKey = Glean.Schema.Testinfra.Types.databaseMetadataField_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.DatabaseMetadataField where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredAssembly_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoveredAssembly_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoveredAssembly_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoveredAssembly_key = 'Angle.TField "assemblyId" (Glean.KeyType Glean.Schema.Testinfra.Types.AssemblyId) ('Angle.TField "root" (Glean.KeyType Glean.Schema.Testinfra.Types.CoveredFolder) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.CoveredAssembly where
  type KeyType Glean.Schema.Testinfra.Types.CoveredAssembly =
    Glean.Schema.Testinfra.Types.CoveredAssembly_key
  getName _proxy  = Glean.PredicateRef "testinfra.CoveredAssembly"4
  getIndex _proxy  = 154
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.coveredAssembly_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.CoveredAssembly x k
  getFactKey = Glean.Schema.Testinfra.Types.coveredAssembly_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredAssembly where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.AssemblyByTag_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.AssemblyByTag_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.AssemblyByTag_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.AssemblyByTag_key = 'Angle.TField "tag" (Glean.KeyType Glean.Schema.Testinfra.Types.Tag) ('Angle.TField "testId" (Glean.KeyType Glean.Schema.Testinfra.Types.AssemblyId) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.AssemblyByTag where
  type KeyType Glean.Schema.Testinfra.Types.AssemblyByTag =
    Glean.Schema.Testinfra.Types.AssemblyByTag_key
  getName _proxy  = Glean.PredicateRef "testinfra.AssemblyByTag"4
  getIndex _proxy  = 147
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.assemblyByTag_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.AssemblyByTag x k
  getFactKey = Glean.Schema.Testinfra.Types.assemblyByTag_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.AssemblyByTag where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.DatabaseMetadata_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.DatabaseMetadata_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.DatabaseMetadata_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.DatabaseMetadata_key = 'Angle.TField "field" (Data.Text.Text) ('Angle.TField "serializedValue" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.DatabaseMetadata where
  type KeyType Glean.Schema.Testinfra.Types.DatabaseMetadata =
    Glean.Schema.Testinfra.Types.DatabaseMetadata_key
  getName _proxy  = Glean.PredicateRef "testinfra.DatabaseMetadata"1
  getIndex _proxy  = 101
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.databaseMetadata_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.DatabaseMetadata x k
  getFactKey = Glean.Schema.Testinfra.Types.databaseMetadata_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.DatabaseMetadata where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Testinfra.Types.Assemblies where
  type KeyType Glean.Schema.Testinfra.Types.Assemblies =
    [Glean.Schema.Testinfra.Types.CoveredAssembly]
  getName _proxy  = Glean.PredicateRef "testinfra.Assemblies"4
  getIndex _proxy  = 98
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.assemblies_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.Assemblies x k
  getFactKey = Glean.Schema.Testinfra.Types.assemblies_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.Assemblies where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key = 'Angle.TField "coveredFile" (Glean.KeyType Glean.Schema.Testinfra.Types.CoveredFile) ('Angle.TField "tag" (Glean.KeyType Glean.Schema.Testinfra.Types.Tag) ('Angle.TField "assemblyId" (Glean.KeyType Glean.Schema.Testinfra.Types.AssemblyId) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly where
  type KeyType Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly =
    Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key
  getName _proxy  =
    Glean.PredicateRef "testinfra.CoveredFileByTagAndAssembly"4
  getIndex _proxy  = 44
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.coveredFileByTagAndAssembly_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly x k
  getFactKey = Glean.Schema.Testinfra.Types.coveredFileByTagAndAssembly_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Testinfra.Types.Folder where
  type KeyType Glean.Schema.Testinfra.Types.Folder = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "testinfra.Folder"1
  getIndex _proxy  = 33
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.folder_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.Folder x k
  getFactKey = Glean.Schema.Testinfra.Types.folder_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.Folder where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileOnly_key where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoveredFileOnly_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoveredFileOnly_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoveredFileOnly_key = 'Angle.TField "coveredFile" (Glean.KeyType Glean.Schema.Testinfra.Types.CoveredFile) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Testinfra.Types.CoveredFileOnly where
  type KeyType Glean.Schema.Testinfra.Types.CoveredFileOnly =
    Glean.Schema.Testinfra.Types.CoveredFileOnly_key
  getName _proxy  = Glean.PredicateRef "testinfra.CoveredFileOnly"3
  getIndex _proxy  = 22
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Testinfra.Types.coveredFileOnly_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Testinfra.Types.CoveredFileOnly x k
  getFactKey = Glean.Schema.Testinfra.Types.coveredFileOnly_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Testinfra.Types.CoveredFileOnly where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Testinfra.Types.OffsetSpan where
  buildRtsValue b (Glean.Schema.Testinfra.Types.OffsetSpan x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.OffsetSpan
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.OffsetSpan = 'Angle.TField "offsetFromZero" (Glean.Nat) ('Angle.TField "lengthAtLeastZero" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Testinfra.Types.CoverageRange where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoverageRange_lineRanges x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoverageRange_byteRanges x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Testinfra.Types.CoverageRange_lineRanges
    , Glean.mapD Glean.Schema.Testinfra.Types.CoverageRange_byteRanges
    ]

type instance Angle.SumFields Glean.Schema.Testinfra.Types.CoverageRange = 'Angle.TField "lineRanges" ([Glean.Schema.Testinfra.Types.OffsetSpan]) ('Angle.TField "byteRanges" ([Glean.Schema.Testinfra.Types.OffsetSpan]) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Testinfra.Types.FileLength where
  buildRtsValue b (Glean.Schema.Testinfra.Types.FileLength_lines x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Testinfra.Types.FileLength_offset x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Testinfra.Types.FileLength_lineOffsets x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Testinfra.Types.FileLength_linesAndOffset x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Testinfra.Types.FileLength_lines
    , Glean.mapD Glean.Schema.Testinfra.Types.FileLength_offset
    , Glean.mapD Glean.Schema.Testinfra.Types.FileLength_lineOffsets
    , Glean.mapD Glean.Schema.Testinfra.Types.FileLength_linesAndOffset
    ]

type instance Angle.SumFields Glean.Schema.Testinfra.Types.FileLength = 'Angle.TField "lines" (Glean.Nat) ('Angle.TField "offset" (Glean.Nat) ('Angle.TField "lineOffsets" ([Glean.Nat]) ('Angle.TField "linesAndOffset" (Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_) ('Angle.TNoFields))))

instance Glean.Type Glean.Schema.Testinfra.Types.CoverageGranularity where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoverageGranularity_file x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoverageGranularity_range x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Testinfra.Types.CoverageGranularity_file
    , Glean.mapD Glean.Schema.Testinfra.Types.CoverageGranularity_range
    ]

type instance Angle.SumFields Glean.Schema.Testinfra.Types.CoverageGranularity = 'Angle.TField "file" (Prelude.Bool) ('Angle.TField "range" (Glean.Schema.Testinfra.Types.CoverageGranularity_range_) ('Angle.TNoFields))

instance Glean.SumBranches Prelude.Bool Glean.Schema.Testinfra.Types.CoverageGranularity where
  injectBranch = Glean.Schema.Testinfra.Types.CoverageGranularity_file
  projectBranch (Glean.Schema.Testinfra.Types.CoverageGranularity_file x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Testinfra.Types.CoverageGranularity_range_ Glean.Schema.Testinfra.Types.CoverageGranularity where
  injectBranch = Glean.Schema.Testinfra.Types.CoverageGranularity_range
  projectBranch (Glean.Schema.Testinfra.Types.CoverageGranularity_range x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Testinfra.Types.FileHash where
  buildRtsValue b (Glean.Schema.Testinfra.Types.FileHash x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.FileHash
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.FileHash = 'Angle.TField "algo" (Glean.Schema.Testinfra.Types.HashAlgo) ('Angle.TField "hash" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Testinfra.Types.HashAlgo where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Testinfra.Types.HashAlgo = 'Angle.TField "crc32" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "md5" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "sha1" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_ where
  buildRtsValue b (Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_ x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_ = 'Angle.TField "lines" (Glean.Nat) ('Angle.TField "offset" (Glean.Nat) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Testinfra.Types.CoverageGranularity_range_ where
  buildRtsValue b (Glean.Schema.Testinfra.Types.CoverageGranularity_range_ x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Testinfra.Types.CoverageGranularity_range_
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Testinfra.Types.CoverageGranularity_range_ = 'Angle.TField "coveredRanges" (Glean.Schema.Testinfra.Types.CoverageRange) ('Angle.TField "uncoveredRanges" (Prelude.Maybe Glean.Schema.Testinfra.Types.CoverageRange) ('Angle.TNoFields))
