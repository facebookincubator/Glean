{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module Glean.Glass.Range
  (
  -- * bytespans to ranges
    fileByteSpanToExclusiveRange
  , exclusiveRangeToFileByteSpan
  -- * range conversions
  , inclusiveRangeToExclusiveRange
  , exclusiveRangeToInclusiveRange
  -- * locations
  , toLocation
  , toLocationRange
  , resolveLocationToRange
  -- * Glass Thrift types to/from Angle
  , spanToSpan
  , spanFromSpan
  , rangeSpanToRange
  -- * Converting raw Glean rangespans to locations and paths
  , locationFromCodeLocation
  , locationRangeFromCodeLocation
  -- * File metadata
  , FileInfo(..)
  , getFileAndLines
  , getFile
  , memoLineOffsets
  ) where

import Data.Default ( Default(def) )

import qualified Glean
import qualified Glean.Util.Range as Range

import qualified Glean.Glass.Types as Glass
import qualified Glean.Glass.Query as Query
import qualified Glean.Schema.Src.Types as Src

import Glean.Glass.Types
    ( LocationRange(..),
      ByteSpan(..),
      Location(..),
      RepoName,
      Range(range_columnEnd, range_lineEnd, range_columnBegin,
            range_lineBegin) )
import Glean.Glass.Base ( GleanPath(..) )
import Glean.Glass.Path ( toGleanPath, fromGleanPath )
import Glean.Glass.Logging
    ( ErrorTy(NoSrcFileFact) )
import qualified Haxl.Core.Memo as Haxl
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import Glean.Angle ( query )
import qualified Glean.Haxl.Repos as Glean

unexpected :: a
unexpected = error "unexpected RangeSpan"

-- | Converts Glean-specific bytespan into client-visible line/col offsets
-- convert Glean ranges into customer range types, with exclusive line end
-- Warning: if the file doesn't have src.FileLines facts and the language
-- schema uses span instead of range, the exclusive range will be
-- incorrect (see type RangeSpan in codemarkup.angle)
fileByteSpanToExclusiveRange
  :: Src.File -> Maybe Range.LineOffsets -> Src.ByteSpan -> Glass.Range
fileByteSpanToExclusiveRange _ Nothing _ = def
fileByteSpanToExclusiveRange file (Just lineoffs) bytespan =
  let range = Range.byteRangeToRange
        file lineoffs
        (Range.byteSpanToRange bytespan)
  in inclusiveRangeToExclusiveRange range

-- | Convert from line/col offet ranges to bytespans.
-- converts customer range types (exclusive end) back into Glean Src.Ranges, and
-- then to spans
exclusiveRangeToFileByteSpan
  :: Src.File -> Maybe Range.LineOffsets -> Glass.Range -> Src.ByteSpan
exclusiveRangeToFileByteSpan file mlineoffs range_exclusive =
  inclusiveRangeToFileByteSpan mlineoffs range
  where
    range = exclusiveRangeToInclusiveRange file range_exclusive

-- | Glean's Src.Range is inclusive of start/end. Glass is exclusive of end
inclusiveRangeToExclusiveRange :: Src.Range -> Glass.Range
inclusiveRangeToExclusiveRange Src.Range{..} =
  Glass.Range {
    range_lineBegin = Glean.unNat range_lineBegin,
    range_columnBegin = Glean.unNat range_columnBegin,
    range_lineEnd = Glean.unNat range_lineEnd,
    range_columnEnd = Glean.unNat range_columnEnd + 1 -- n.b. exclusive end
  }

-- | Range exlucsive of end to Glean's inclusive range
exclusiveRangeToInclusiveRange :: Src.File -> Glass.Range -> Src.Range
exclusiveRangeToInclusiveRange file Glass.Range{..} =
  Src.Range {
    range_file = file,
    range_lineBegin = i64ToNat range_lineBegin,
    range_columnBegin = i64ToNat range_columnBegin,
    range_lineEnd = i64ToNat range_lineEnd,
    range_columnEnd = i64ToNat (max 1 (range_columnEnd - 1))
  }
  where
    i64ToNat = Glean.toNat . fromIntegral

-- | Convert a src.Range from glean to a src.ByteSpan
inclusiveRangeToFileByteSpan
  :: Maybe Range.LineOffsets -> Src.Range -> Src.ByteSpan
inclusiveRangeToFileByteSpan Nothing _ = def
inclusiveRangeToFileByteSpan (Just lineoffs) range =
  Range.rangeToByteSpan (Range.srcRangeToSimpleByteRange lineoffs range)

-- | Target cross-references. these are unresolved (raw) spans of locations.
-- They can be resolved to Ranges via a FileLines call (client or server-side).
toLocation
  :: RepoName -> Src.File -> Src.ByteSpan -> Glean.RepoHaxl u w Location
toLocation repo file bytespan = do
  path <- GleanPath <$> Glean.keyOf file
  let (location_repository, location_filepath) =
        fromGleanPath repo path
  return $ Location {
       location_repository = location_repository,
       location_filepath = location_filepath,
       location_span = spanToSpan bytespan
     }

-- | Like toLocation, but for ranges.
toLocationRange
  :: RepoName -> Src.File -> Range -> Glean.RepoHaxl u w LocationRange
toLocationRange repo file range = do
  path <- GleanPath <$> Glean.keyOf file
  let (locationRange_repository, locationRange_filepath) =
        fromGleanPath repo path
  return $ LocationRange {
       locationRange_repository = locationRange_repository,
       locationRange_filepath = locationRange_filepath,
       locationRange_range = range
     }

data FileInfo = FileInfo {
    fileRepo :: Glean.Repo,
    fileId :: Glean.IdOf Src.File,
    srcFile :: Src.File,
    offsets :: Maybe Range.LineOffsets
  }

-- | Get file metadata. Throw if we have no src.File
-- This is the first point we might encounter an unindexed file path
getFileAndLines
  :: Glean.Repo -> GleanPath -> Glean.RepoHaxl u w (Either ErrorTy FileInfo)
getFileAndLines fileRepo path = do
  efile <- getFile path
  case efile of
    Left err -> return $ Left err
    Right srcFile -> do
      offsets <- memoLineOffsets srcFile
      return $ do
        let fileId = Glean.getId srcFile
        Right FileInfo{..}

-- | Just get the src.File fact for a path
getFile :: GleanPath -> Glean.RepoHaxl u w (Either ErrorTy Src.File)
getFile path = do
  mfile <- Glean.getFirstResult (query (Query.srcFile path))
  return $ case mfile of
    Nothing -> Left $ NoSrcFileFact $
      "No src.File fact for " <> gleanPath path
    Just srcFile -> Right srcFile

-- | Get the line offsets associated with a file
toLineOffsets :: Src.File -> Glean.RepoHaxl u w (Maybe Range.LineOffsets)
toLineOffsets file = do
  let fileId = Glean.getId file
  mlines <- Glean.getFirstResult (query (Query.fileLines fileId))
  case mlines of
    Nothing -> return Nothing
    Just offs -> Just . Range.lengthsToLineOffsets <$> Glean.keyOf offs

-- | Memorize the result of computing the line offsets on a file
-- This provides up to 15x win for xrefs on large files
memoLineOffsets :: Src.File -> Glean.RepoHaxl u w (Maybe Range.LineOffsets)
memoLineOffsets file = do
  key <- Glean.keyOf file
  Haxl.memo key $ toLineOffsets file

-- | Glean provides either a bytespan or a range for a symbol location.
-- Normalize it to Glass.Range (exclusive of end line/col spans)
rangeSpanToRange
  :: Src.File -> Maybe Range.LineOffsets -> Code.RangeSpan -> Range
rangeSpanToRange file offsets (Code.RangeSpan_span span) =
  fileByteSpanToExclusiveRange file offsets span
rangeSpanToRange _ _ (Code.RangeSpan_range range) =
  inclusiveRangeToExclusiveRange range
rangeSpanToRange _ _ Code.RangeSpan_EMPTY =
  unexpected

-- | Convert a client-side target locator to a specific line/col range
resolveLocationToRange
  :: Glean.Repo -> Location -> Glean.RepoHaxl u w (Either ErrorTy Range)
resolveLocationToRange repo Location{..} = do
  let path = toGleanPath location_repository location_filepath
  efile <- getFileAndLines repo path
  return $ flip fmap efile $ \ FileInfo{..} ->
    fileByteSpanToExclusiveRange srcFile offsets
      (spanFromSpan location_span)

-- | Span conversion. We hide any Glean-specific types
spanToSpan :: Src.ByteSpan -> ByteSpan
spanToSpan Src.ByteSpan{..} =
  ByteSpan {
    byteSpan_start = Glean.unNat byteSpan_start,
    byteSpan_length = Glean.unNat byteSpan_length
  }

-- | Span un-conversion
spanFromSpan :: ByteSpan -> Src.ByteSpan
spanFromSpan ByteSpan{..} =
  Src.ByteSpan{
    byteSpan_start = Glean.Nat byteSpan_start,
    byteSpan_length = Glean.Nat byteSpan_length
  }

-- | Convert Glean-side Location markers to the Glass bytespan locations
locationFromCodeLocation
  :: RepoName -> Src.File -> Code.RangeSpan -> Glean.RepoHaxl u w Glass.Location
locationFromCodeLocation repo file rangespan = case rangespan of
  Code.RangeSpan_span span -> toLocation repo file span
  Code.RangeSpan_range range -> do -- expensive:  converting back to bytespans
    mOffsets <- memoLineOffsets file
    let span = inclusiveRangeToFileByteSpan mOffsets range
    toLocation repo file span
  Code.RangeSpan_EMPTY -> unexpected

-- | Convert Glean-side Location markers to the Glass range locations
-- Like the composition of locationFromCodeLocation and resolveLocationRoRange,
-- but avoids intermediate conversion via bytespans
locationRangeFromCodeLocation
  :: RepoName
  -> Src.File
  -> Code.RangeSpan
  -> Glean.RepoHaxl u w Glass.LocationRange
locationRangeFromCodeLocation repo file rangespan = do
  range <- case rangespan of
    Code.RangeSpan_span span -> do
      mOffsets <- memoLineOffsets file
      return $ fileByteSpanToExclusiveRange file mOffsets span
    Code.RangeSpan_range incRange -> do
      return $ inclusiveRangeToExclusiveRange incRange
    Code.RangeSpan_EMPTY -> unexpected
  toLocationRange repo file range
