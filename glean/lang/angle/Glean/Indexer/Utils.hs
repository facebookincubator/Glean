{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Indexer.Utils
  (toFileInfoMap,
  toResolvedSchemaMap,
  sourceFileInfos,
  SchemaFileInfo(..),
  SourceFileInfo(..),
  ToByteSpan,
  FileInfoMap,
  fromSrcSpan,
  ) where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HashMap
import Control.Exception
import System.FilePath
import Util.IO ( listDirectoryRecursive )

import Glean
import Glean.Util.Range (srcRangeToByteRange, rangeToByteSpan)
import qualified Glean.Schema.Src.Types as Src
import Glean.Angle.Types
import Glean.Angle.Parser (parseSchema)
import Glean.Schema.Types

data SourceFileInfo = SourceFileInfo {
  name :: Name
  , filepath :: FilePath
  , bytestr :: B.ByteString
}

data SchemaFileInfo = SchemaFileInfo {
  sourceFileInfo :: SourceFileInfo
  , toByteSpan :: ToByteSpan
}

type ToByteSpan = SrcSpan -> Src.ByteSpan
type FileInfoMap = HashMap.HashMap Name SchemaFileInfo

-- Collect bytestrings from source code
-- we need this to create the Src.FileLines,
-- and for SrcSpan -> Src.ByteSpan conversion
sourceFileInfos :: FilePath -> FilePath -> IO [SourceFileInfo]
sourceFileInfos repoPath dir  = do
  let indexDir = repoPath  </> dir
  files <- filter ((== ".angle") . takeExtension)
    <$> listDirectoryRecursive indexDir
  sourceFileInfo <- mapM  (\file -> do
    bytestr <- B.readFile file
    case parseSchema bytestr of
        Left err -> throwIO $ ErrorCall err
        Right SourceSchemas{..} -> return $ map (\s -> do
          let name = sourceRefName $ schemaName s
              relFilePath = makeRelative repoPath file
          SourceFileInfo name relFilePath bytestr
          )
          srcSchemas) files
  return $ concat sourceFileInfo

toFileInfoMap :: [SchemaFileInfo] -> FileInfoMap
toFileInfoMap fileInfos =
  HashMap.fromList $ map (\schemaInfo ->
    (name $ sourceFileInfo schemaInfo, schemaInfo)) fileInfos

toResolvedSchemaMap :: ResolvedSchemas -> HashMap.HashMap Name ResolvedSchemaRef
toResolvedSchemaMap schemas = HashMap.fromList $ map (\schema ->
  (resolvedSchemaName schema, schema)) $ schemasResolved schemas

fromSrcSpan :: Src.FileLines_key -> B.ByteString -> (SrcSpan -> Src.ByteSpan)
fromSrcSpan filelineskey bytestr = do
  let file = Src.fileLines_key_file filelineskey
      rangeToByteRange = srcRangeToByteRange filelineskey bytestr
      toByteSpan =  rangeToByteSpan . rangeToByteRange
  toByteSpan . toSourceRange file

toSourceRange :: Src.File -> SrcSpan -> Src.Range
toSourceRange file SrcSpan{..} =
  Src.Range {
    range_file = file
    , range_lineBegin = toNat $ locLine spanStart
    , range_columnBegin = toNat $ locCol spanStart
    , range_lineEnd = toNat $ locLine spanEnd
    , range_columnEnd = toNat $ locCol spanEnd
  }
   where toNat = Glean.Nat . fromIntegral
