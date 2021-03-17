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
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Testinfra.Types


instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.TestId where
  toQueryId = Glean.Schema.Query.Testinfra.Types.TestId_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.TestId_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.TestId = Glean.Schema.Testinfra.Types.TestId
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.TestId = Glean.Schema.Query.Testinfra.Types.TestId

instance Glean.ToQuery Glean.Schema.Testinfra.Types.TestId

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.FbId where
  toQueryId = Glean.Schema.Query.Testinfra.Types.FbId_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.FbId_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.FbId = Glean.Schema.Testinfra.Types.FbId
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.FbId = Glean.Schema.Query.Testinfra.Types.FbId

instance Glean.ToQuery Glean.Schema.Testinfra.Types.FbId

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFolder_key = Glean.Schema.Testinfra.Types.CoveredFolder_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFolder_key = Glean.Schema.Query.Testinfra.Types.CoveredFolder_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFolder_key where
  toQuery (Glean.Schema.Testinfra.Types.CoveredFolder_key x1 x2 x3) = Glean.Schema.Query.Testinfra.Types.CoveredFolder_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Testinfra.Types.CoveredFolder_folders_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Testinfra.Types.CoveredFolder_files_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.CoveredFolder where
  toQueryId = Glean.Schema.Query.Testinfra.Types.CoveredFolder_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.CoveredFolder_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFolder = Glean.Schema.Testinfra.Types.CoveredFolder
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFolder = Glean.Schema.Query.Testinfra.Types.CoveredFolder

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFolder

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.MeasuredFile_key = Glean.Schema.Testinfra.Types.MeasuredFile_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.MeasuredFile_key = Glean.Schema.Query.Testinfra.Types.MeasuredFile_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.MeasuredFile_key where
  toQuery (Glean.Schema.Testinfra.Types.MeasuredFile_key x1 x2) = Glean.Schema.Query.Testinfra.Types.MeasuredFile_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.MeasuredFile where
  toQueryId = Glean.Schema.Query.Testinfra.Types.MeasuredFile_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.MeasuredFile_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.MeasuredFile = Glean.Schema.Testinfra.Types.MeasuredFile
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.MeasuredFile = Glean.Schema.Query.Testinfra.Types.MeasuredFile

instance Glean.ToQuery Glean.Schema.Testinfra.Types.MeasuredFile

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.Tag where
  toQueryId = Glean.Schema.Query.Testinfra.Types.Tag_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.Tag_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.Tag = Glean.Schema.Testinfra.Types.Tag
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.Tag = Glean.Schema.Query.Testinfra.Types.Tag

instance Glean.ToQuery Glean.Schema.Testinfra.Types.Tag

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.ContainsPushBlockingAssembly_key = Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key = Glean.Schema.Query.Testinfra.Types.ContainsPushBlockingAssembly_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key where
  toQuery (Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly_key x1 x2) = Glean.Schema.Query.Testinfra.Types.ContainsPushBlockingAssembly_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly where
  toQueryId = Glean.Schema.Query.Testinfra.Types.ContainsPushBlockingAssembly_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.ContainsPushBlockingAssembly_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.ContainsPushBlockingAssembly = Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly = Glean.Schema.Query.Testinfra.Types.ContainsPushBlockingAssembly

instance Glean.ToQuery Glean.Schema.Testinfra.Types.ContainsPushBlockingAssembly

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileAssemblies_key = Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key = Glean.Schema.Query.Testinfra.Types.CoveredFileAssemblies_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key where
  toQuery (Glean.Schema.Testinfra.Types.CoveredFileAssemblies_key x1 x2) = Glean.Schema.Query.Testinfra.Types.CoveredFileAssemblies_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.CoveredFileAssemblies where
  toQueryId = Glean.Schema.Query.Testinfra.Types.CoveredFileAssemblies_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.CoveredFileAssemblies_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileAssemblies = Glean.Schema.Testinfra.Types.CoveredFileAssemblies
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileAssemblies = Glean.Schema.Query.Testinfra.Types.CoveredFileAssemblies

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileAssemblies

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileByPushBlockingAssembly_key = Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key = Glean.Schema.Query.Testinfra.Types.CoveredFileByPushBlockingAssembly_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key where
  toQuery (Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly_key x1 x2) = Glean.Schema.Query.Testinfra.Types.CoveredFileByPushBlockingAssembly_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly where
  toQueryId = Glean.Schema.Query.Testinfra.Types.CoveredFileByPushBlockingAssembly_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.CoveredFileByPushBlockingAssembly_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileByPushBlockingAssembly = Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly = Glean.Schema.Query.Testinfra.Types.CoveredFileByPushBlockingAssembly

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileByPushBlockingAssembly

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.MeasuredFileOnly_key = Glean.Schema.Testinfra.Types.MeasuredFileOnly_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.MeasuredFileOnly_key = Glean.Schema.Query.Testinfra.Types.MeasuredFileOnly_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.MeasuredFileOnly_key where
  toQuery (Glean.Schema.Testinfra.Types.MeasuredFileOnly_key x1 x2) = Glean.Schema.Query.Testinfra.Types.MeasuredFileOnly_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.MeasuredFileOnly where
  toQueryId = Glean.Schema.Query.Testinfra.Types.MeasuredFileOnly_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.MeasuredFileOnly_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.MeasuredFileOnly = Glean.Schema.Testinfra.Types.MeasuredFileOnly
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.MeasuredFileOnly = Glean.Schema.Query.Testinfra.Types.MeasuredFileOnly

instance Glean.ToQuery Glean.Schema.Testinfra.Types.MeasuredFileOnly

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.FileMetadata_key = Glean.Schema.Testinfra.Types.FileMetadata_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.FileMetadata_key = Glean.Schema.Query.Testinfra.Types.FileMetadata_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.FileMetadata_key where
  toQuery (Glean.Schema.Testinfra.Types.FileMetadata_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Testinfra.Types.FileMetadata_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Testinfra.Types.FileMetadata_hash_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_length_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_length_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_nonexecutableRanges_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_nonexecutableRanges_just = Prelude.Just (Glean.toQuery x)})) x4)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_executableLength_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_executableLength_just = Prelude.Just (Glean.toQuery x)})) x5))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.FileMetadata where
  toQueryId = Glean.Schema.Query.Testinfra.Types.FileMetadata_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.FileMetadata_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.FileMetadata = Glean.Schema.Testinfra.Types.FileMetadata
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.FileMetadata = Glean.Schema.Query.Testinfra.Types.FileMetadata

instance Glean.ToQuery Glean.Schema.Testinfra.Types.FileMetadata

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.FileMetadata_2_key = Glean.Schema.Testinfra.Types.FileMetadata_2_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.FileMetadata_2_key = Glean.Schema.Query.Testinfra.Types.FileMetadata_2_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.FileMetadata_2_key where
  toQuery (Glean.Schema.Testinfra.Types.FileMetadata_2_key x1 x2 x3 x4) = Glean.Schema.Query.Testinfra.Types.FileMetadata_2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Testinfra.Types.FileMetadata_2_hash_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_2_length_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_2_length_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_2_nonexecutableRanges_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileMetadata_2_nonexecutableRanges_just = Prelude.Just (Glean.toQuery x)})) x4))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.FileMetadata_2 where
  toQueryId = Glean.Schema.Query.Testinfra.Types.FileMetadata_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.FileMetadata_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.FileMetadata_2 = Glean.Schema.Testinfra.Types.FileMetadata_2
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.FileMetadata_2 = Glean.Schema.Query.Testinfra.Types.FileMetadata_2

instance Glean.ToQuery Glean.Schema.Testinfra.Types.FileMetadata_2

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.AssemblyId_key = Glean.Schema.Testinfra.Types.AssemblyId_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.AssemblyId_key = Glean.Schema.Query.Testinfra.Types.AssemblyId_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.AssemblyId_key where
  toQuery (Glean.Schema.Testinfra.Types.AssemblyId_key_testId x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.assemblyId_key_testId = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Testinfra.Types.AssemblyId_key_fbId x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.assemblyId_key_fbId = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Testinfra.Types.TestId Glean.Schema.Query.Testinfra.Types.AssemblyId_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Testinfra.Types.assemblyId_key_testId = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Testinfra.Types.FbId Glean.Schema.Query.Testinfra.Types.AssemblyId_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Testinfra.Types.assemblyId_key_fbId = Prelude.Just q }

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.AssemblyId where
  toQueryId = Glean.Schema.Query.Testinfra.Types.AssemblyId_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.AssemblyId_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.AssemblyId = Glean.Schema.Testinfra.Types.AssemblyId
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.AssemblyId = Glean.Schema.Query.Testinfra.Types.AssemblyId

instance Glean.ToQuery Glean.Schema.Testinfra.Types.AssemblyId

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileTestIds_key = Glean.Schema.Testinfra.Types.CoveredFileTestIds_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileTestIds_key = Glean.Schema.Query.Testinfra.Types.CoveredFileTestIds_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileTestIds_key where
  toQuery (Glean.Schema.Testinfra.Types.CoveredFileTestIds_key x1 x2) = Glean.Schema.Query.Testinfra.Types.CoveredFileTestIds_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.CoveredFileTestIds where
  toQueryId = Glean.Schema.Query.Testinfra.Types.CoveredFileTestIds_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.CoveredFileTestIds_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileTestIds = Glean.Schema.Testinfra.Types.CoveredFileTestIds
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileTestIds = Glean.Schema.Query.Testinfra.Types.CoveredFileTestIds

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileTestIds

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.TaggedAssembly_key = Glean.Schema.Testinfra.Types.TaggedAssembly_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.TaggedAssembly_key = Glean.Schema.Query.Testinfra.Types.TaggedAssembly_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.TaggedAssembly_key where
  toQuery (Glean.Schema.Testinfra.Types.TaggedAssembly_key x1 x2) = Glean.Schema.Query.Testinfra.Types.TaggedAssembly_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.TaggedAssembly where
  toQueryId = Glean.Schema.Query.Testinfra.Types.TaggedAssembly_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.TaggedAssembly_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.TaggedAssembly = Glean.Schema.Testinfra.Types.TaggedAssembly
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.TaggedAssembly = Glean.Schema.Query.Testinfra.Types.TaggedAssembly

instance Glean.ToQuery Glean.Schema.Testinfra.Types.TaggedAssembly

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFile_key = Glean.Schema.Testinfra.Types.CoveredFile_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFile_key = Glean.Schema.Query.Testinfra.Types.CoveredFile_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFile_key where
  toQuery (Glean.Schema.Testinfra.Types.CoveredFile_key x1 x2) = Glean.Schema.Query.Testinfra.Types.CoveredFile_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.CoveredFile where
  toQueryId = Glean.Schema.Query.Testinfra.Types.CoveredFile_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.CoveredFile_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFile = Glean.Schema.Testinfra.Types.CoveredFile
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFile = Glean.Schema.Query.Testinfra.Types.CoveredFile

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFile

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.DatabaseMetadataField where
  toQueryId = Glean.Schema.Query.Testinfra.Types.DatabaseMetadataField_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.DatabaseMetadataField_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.DatabaseMetadataField = Glean.Schema.Testinfra.Types.DatabaseMetadataField
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.DatabaseMetadataField = Glean.Schema.Query.Testinfra.Types.DatabaseMetadataField

instance Glean.ToQuery Glean.Schema.Testinfra.Types.DatabaseMetadataField

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredAssembly_key = Glean.Schema.Testinfra.Types.CoveredAssembly_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredAssembly_key = Glean.Schema.Query.Testinfra.Types.CoveredAssembly_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredAssembly_key where
  toQuery (Glean.Schema.Testinfra.Types.CoveredAssembly_key x1 x2) = Glean.Schema.Query.Testinfra.Types.CoveredAssembly_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.CoveredAssembly where
  toQueryId = Glean.Schema.Query.Testinfra.Types.CoveredAssembly_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.CoveredAssembly_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredAssembly = Glean.Schema.Testinfra.Types.CoveredAssembly
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredAssembly = Glean.Schema.Query.Testinfra.Types.CoveredAssembly

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredAssembly

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.AssemblyByTag_key = Glean.Schema.Testinfra.Types.AssemblyByTag_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.AssemblyByTag_key = Glean.Schema.Query.Testinfra.Types.AssemblyByTag_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.AssemblyByTag_key where
  toQuery (Glean.Schema.Testinfra.Types.AssemblyByTag_key x1 x2) = Glean.Schema.Query.Testinfra.Types.AssemblyByTag_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.AssemblyByTag where
  toQueryId = Glean.Schema.Query.Testinfra.Types.AssemblyByTag_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.AssemblyByTag_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.AssemblyByTag = Glean.Schema.Testinfra.Types.AssemblyByTag
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.AssemblyByTag = Glean.Schema.Query.Testinfra.Types.AssemblyByTag

instance Glean.ToQuery Glean.Schema.Testinfra.Types.AssemblyByTag

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.DatabaseMetadata_key = Glean.Schema.Testinfra.Types.DatabaseMetadata_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.DatabaseMetadata_key = Glean.Schema.Query.Testinfra.Types.DatabaseMetadata_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.DatabaseMetadata_key where
  toQuery (Glean.Schema.Testinfra.Types.DatabaseMetadata_key x1 x2) = Glean.Schema.Query.Testinfra.Types.DatabaseMetadata_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.DatabaseMetadata where
  toQueryId = Glean.Schema.Query.Testinfra.Types.DatabaseMetadata_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.DatabaseMetadata_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.DatabaseMetadata = Glean.Schema.Testinfra.Types.DatabaseMetadata
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.DatabaseMetadata = Glean.Schema.Query.Testinfra.Types.DatabaseMetadata

instance Glean.ToQuery Glean.Schema.Testinfra.Types.DatabaseMetadata

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.Assemblies where
  toQueryId = Glean.Schema.Query.Testinfra.Types.Assemblies_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.Assemblies_with_key . (Glean.Schema.Query.Testinfra.Types.Assemblies_array_exact . Prelude.map Glean.toQuery)

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.Assemblies = Glean.Schema.Testinfra.Types.Assemblies
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.Assemblies = Glean.Schema.Query.Testinfra.Types.Assemblies

instance Glean.ToQuery Glean.Schema.Testinfra.Types.Assemblies

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileByTagAndAssembly_key = Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key = Glean.Schema.Query.Testinfra.Types.CoveredFileByTagAndAssembly_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key where
  toQuery (Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly_key x1 x2 x3) = Glean.Schema.Query.Testinfra.Types.CoveredFileByTagAndAssembly_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly where
  toQueryId = Glean.Schema.Query.Testinfra.Types.CoveredFileByTagAndAssembly_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.CoveredFileByTagAndAssembly_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileByTagAndAssembly = Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly = Glean.Schema.Query.Testinfra.Types.CoveredFileByTagAndAssembly

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileByTagAndAssembly

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.Folder where
  toQueryId = Glean.Schema.Query.Testinfra.Types.Folder_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.Folder_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.Folder = Glean.Schema.Testinfra.Types.Folder
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.Folder = Glean.Schema.Query.Testinfra.Types.Folder

instance Glean.ToQuery Glean.Schema.Testinfra.Types.Folder

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileOnly_key = Glean.Schema.Testinfra.Types.CoveredFileOnly_key
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileOnly_key = Glean.Schema.Query.Testinfra.Types.CoveredFileOnly_key

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileOnly_key where
  toQuery (Glean.Schema.Testinfra.Types.CoveredFileOnly_key x1 x2) = Glean.Schema.Query.Testinfra.Types.CoveredFileOnly_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Testinfra.Types.CoveredFileOnly where
  toQueryId = Glean.Schema.Query.Testinfra.Types.CoveredFileOnly_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Testinfra.Types.CoveredFileOnly_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoveredFileOnly = Glean.Schema.Testinfra.Types.CoveredFileOnly
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoveredFileOnly = Glean.Schema.Query.Testinfra.Types.CoveredFileOnly

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoveredFileOnly

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.OffsetSpan = Glean.Schema.Testinfra.Types.OffsetSpan
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.OffsetSpan = Glean.Schema.Query.Testinfra.Types.OffsetSpan

instance Glean.ToQuery Glean.Schema.Testinfra.Types.OffsetSpan where
  toQuery (Glean.Schema.Testinfra.Types.OffsetSpan x1 x2) = Glean.Schema.Query.Testinfra.Types.OffsetSpan (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoverageRange = Glean.Schema.Testinfra.Types.CoverageRange
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoverageRange = Glean.Schema.Query.Testinfra.Types.CoverageRange

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoverageRange where
  toQuery (Glean.Schema.Testinfra.Types.CoverageRange_lineRanges x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.coverageRange_lineRanges = Prelude.Just ((Glean.Schema.Query.Testinfra.Types.CoverageRange_lineRanges__array_exact . Prelude.map Glean.toQuery) x) }
  toQuery (Glean.Schema.Testinfra.Types.CoverageRange_byteRanges x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.coverageRange_byteRanges = Prelude.Just ((Glean.Schema.Query.Testinfra.Types.CoverageRange_byteRanges__array_exact . Prelude.map Glean.toQuery) x) }

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.FileLength = Glean.Schema.Testinfra.Types.FileLength
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.FileLength = Glean.Schema.Query.Testinfra.Types.FileLength

instance Glean.ToQuery Glean.Schema.Testinfra.Types.FileLength where
  toQuery (Glean.Schema.Testinfra.Types.FileLength_lines x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileLength_lines = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Testinfra.Types.FileLength_offset x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileLength_offset = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Testinfra.Types.FileLength_lineOffsets x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileLength_lineOffsets = Prelude.Just ((Glean.Schema.Query.Testinfra.Types.FileLength_lineOffsets__array_exact . Prelude.map Glean.toQuery) x) }
  toQuery (Glean.Schema.Testinfra.Types.FileLength_linesAndOffset x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.fileLength_linesAndOffset = Prelude.Just (Glean.toQuery x) }

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoverageGranularity = Glean.Schema.Testinfra.Types.CoverageGranularity
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoverageGranularity = Glean.Schema.Query.Testinfra.Types.CoverageGranularity

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoverageGranularity where
  toQuery (Glean.Schema.Testinfra.Types.CoverageGranularity_file x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.coverageGranularity_file = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Testinfra.Types.CoverageGranularity_range x) = Data.Default.def { Glean.Schema.Query.Testinfra.Types.coverageGranularity_range = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Prelude.Bool Glean.Schema.Query.Testinfra.Types.CoverageGranularity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Testinfra.Types.coverageGranularity_file = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Testinfra.Types.CoverageGranularity_range_ Glean.Schema.Query.Testinfra.Types.CoverageGranularity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Testinfra.Types.coverageGranularity_range = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.FileHash = Glean.Schema.Testinfra.Types.FileHash
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.FileHash = Glean.Schema.Query.Testinfra.Types.FileHash

instance Glean.ToQuery Glean.Schema.Testinfra.Types.FileHash where
  toQuery (Glean.Schema.Testinfra.Types.FileHash x1 x2) = Glean.Schema.Query.Testinfra.Types.FileHash (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.HashAlgo = Glean.Schema.Testinfra.Types.HashAlgo
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.HashAlgo = Glean.Schema.Query.Testinfra.Types.HashAlgo

instance Glean.ToQuery Glean.Schema.Testinfra.Types.HashAlgo where
  toQuery Glean.Schema.Testinfra.Types.HashAlgo_crc32 = Glean.Schema.Query.Testinfra.Types.HashAlgo_crc32
  toQuery Glean.Schema.Testinfra.Types.HashAlgo_md5 = Glean.Schema.Query.Testinfra.Types.HashAlgo_md5
  toQuery Glean.Schema.Testinfra.Types.HashAlgo_sha1 = Glean.Schema.Query.Testinfra.Types.HashAlgo_sha1

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.FileLength_linesAndOffset_ = Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_ = Glean.Schema.Query.Testinfra.Types.FileLength_linesAndOffset_

instance Glean.ToQuery Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_ where
  toQuery (Glean.Schema.Testinfra.Types.FileLength_linesAndOffset_ x1 x2) = Glean.Schema.Query.Testinfra.Types.FileLength_linesAndOffset_ (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Testinfra.Types.CoverageGranularity_range_ = Glean.Schema.Testinfra.Types.CoverageGranularity_range_
type instance Glean.QueryOf Glean.Schema.Testinfra.Types.CoverageGranularity_range_ = Glean.Schema.Query.Testinfra.Types.CoverageGranularity_range_

instance Glean.ToQuery Glean.Schema.Testinfra.Types.CoverageGranularity_range_ where
  toQuery (Glean.Schema.Testinfra.Types.CoverageGranularity_range_ x1 x2) = Glean.Schema.Query.Testinfra.Types.CoverageGranularity_range_ (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Testinfra.Types.coverageGranularity_range__uncoveredRanges_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Testinfra.Types.coverageGranularity_range__uncoveredRanges_just = Prelude.Just (Glean.toQuery x)})) x2))
