{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Range
  (
  -- * working with ranges and bytespans, to and from Glean representations
     rangeContains
  -- ** high level
  , rangeSpanToLocation
  , rangeSpanToLocationRange
  -- ** lower level
  , memoRangeSpanToRange
  , rangeSpanToRange
  , inclusiveRangeToExclusiveRange

  -- * locations, these have paths and repo names
  , resolveLocationToRange

  -- * File metadata
  , FileInfo(..)
  , getFileAndLines
  , getFile

  ) where

import Data.Default ( Default(def) )
import Data.Function (on)

import qualified Glean
import qualified Glean.Util.Range as Range

import qualified Glean.Glass.Types as Glass
import qualified Glean.Glass.Query as Query
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Digest.Types as Digest

import Glean.Glass.Types
    ( LocationRange(..),
      ByteSpan(..),
      Location(..),
      RepoName,
      Range(range_columnEnd, range_lineEnd, range_columnBegin, range_lineBegin),
      GlassExceptionReason (GlassExceptionReason_noSrcFileFact)
    )
import Glean.Glass.Base ( GleanPath(..),  SymbolRepoPath(..))
import Glean.Glass.Path ( toGleanPath, fromGleanPath )
import qualified Haxl.Core.Memo as Haxl
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import Glean.Angle ( query )
import qualified Glean.Haxl.Repos as Glean

-- | @rangeContains x y@ is True iff the range of y is the same as x, or
-- inside x.
rangeContains :: Range -> Range -> Bool
rangeContains big small =
  checkLE range_lineBegin range_columnBegin big small &&
  checkLE range_lineEnd range_columnEnd small big
  where
    checkLE line col a b = GT /= compareLineCol line col a b
    compareLineCol line col a b =
      (compare `on` line) a b <> (compare `on` col) a b

-- | Convert Glean-side Location markers to the Glass bytespan locations
-- Like locationRangeFromCodeLocation, memoizes the file offsets needed to do
-- span conversion.
rangeSpanToLocation
  :: RepoName -> Src.File -> Code.RangeSpan -> Glean.RepoHaxl u w Glass.Location
rangeSpanToLocation repo file rangespan = do
  span <- case rangespan of
    Code.RangeSpan_span span -> pure span
    Code.RangeSpan_range range -> do -- expensive:  converting back to bytespans
      mOffsets <- memoLineOffsets file
      return $ inclusiveRangeToFileByteSpan mOffsets range
    Code.RangeSpan_EMPTY -> unexpected
  toLocation repo file span

-- | Convert Glean-side range span and file to the Glass range locations, with
-- ranges in exclusive-end form, and paths adjusted to be repo-relative
rangeSpanToLocationRange -- locationRangeFromCodeLocation
  :: RepoName
  -> Src.File
  -> Code.RangeSpan
  -> Glean.RepoHaxl u w Glass.LocationRange
rangeSpanToLocationRange repo file rangespan = do
  range <- memoRangeSpanToRange file rangespan
  toLocationRange repo file range

-- | Convert Glean-side rangeSpan to Glass Range, with memo filelines if needed
-- Like rangeSpanToRange, but memoizes the file offsets for the span case.
memoRangeSpanToRange
  :: Src.File
  -> Code.RangeSpan
  -> Glean.RepoHaxl u w Range
memoRangeSpanToRange file rangespan@Code.RangeSpan_span{} = do
  mOffsets <- memoLineOffsets file
  return $ rangeSpanToRange mOffsets rangespan
memoRangeSpanToRange _ rangespan =
  pure $ rangeSpanToRange Nothing rangespan

-- | Glean provides either a bytespan or a range for a symbol location.
-- Normalize it to Glass.Range (exclusive of end line/col spans)
-- Only use this if you know the offsets are already memoized.
rangeSpanToRange :: Maybe Range.LineOffsets -> Code.RangeSpan -> Range
rangeSpanToRange offsets (Code.RangeSpan_span span) =
  fileByteSpanToExclusiveRange offsets span
rangeSpanToRange _ (Code.RangeSpan_range range) =
  inclusiveRangeToExclusiveRange range
rangeSpanToRange _ Code.RangeSpan_EMPTY =
  unexpected

-- | Convert a client-side target locator to a specific line/col range
-- Used to implement location jumpTo
resolveLocationToRange
  :: Glean.Repo
  -> Location
  -> Glean.RepoHaxl u w (Either GlassExceptionReason Range)
resolveLocationToRange repo Location{..} = do
  let path = toGleanPath $ SymbolRepoPath location_repository location_filepath
  efile <- getFileAndLines repo path
  return $ flip fmap efile $ \ FileInfo{..} ->
    fileByteSpanToExclusiveRange offsets (spanFromSpan location_span)
  where
    -- Span un-conversion
    spanFromSpan :: ByteSpan -> Src.ByteSpan
    spanFromSpan ByteSpan{..} =
      Src.ByteSpan{
        byteSpan_start = Glean.Nat byteSpan_start,
        byteSpan_length = Glean.Nat byteSpan_length
      }

-- | Package of src.File metadata we typically need
data FileInfo = FileInfo {
    fileRepo :: Glean.Repo,
    fileId :: {-# UNPACK #-} !(Glean.IdOf Src.File),
    srcFile :: !Src.File,
    offsets :: !(Maybe Range.LineOffsets),
    isIndexed :: !Bool,
    fileDigest :: Maybe Digest.Digest
  }

-- | Get file metadata. Throw if we have no src.File
-- This is the first point we might encounter an unindexed file path
-- Note: we may not have lineoffsets, but this is not necessarily fatal as
-- they are only needed for bytespan conversions
getFileAndLines
  :: Glean.Repo
  -> GleanPath
  -> Glean.RepoHaxl u w (Either GlassExceptionReason FileInfo)
getFileAndLines fileRepo path = do
  mfile <- Glean.getFirstResult (query (Query.indexedFile (Query.srcFile path)))
  case mfile of
    Nothing -> return $ Left $
      GlassExceptionReason_noSrcFileFact $ "No src.File fact for "
        <> gleanPath path
    Just (srcFile, isIndexed, fileDigest) -> do
      offsets <- memoLineOffsets srcFile
      return $ do
        let fileId = Glean.getId srcFile
        Right FileInfo{..}

-- | Get the src.File fact for a path
getFile :: GleanPath -> Glean.RepoHaxl u w (Either GlassExceptionReason Src.File)
getFile path = do
  mfile <- Glean.getFirstResult (query (Query.srcFile path))
  return $ case mfile of
    Nothing -> Left $
      GlassExceptionReason_noSrcFileFact $ "No src.File fact for "
        <> gleanPath path
    Just srcFile -> Right srcFile

-- | Glean's Src.Range is inclusive of start/end. Glass is exclusive
-- of end
inclusiveRangeToExclusiveRange :: Src.Range -> Glass.Range
inclusiveRangeToExclusiveRange Src.Range{..} =
  Glass.Range {
    range_lineBegin = Glean.unNat range_lineBegin,
    range_columnBegin = Glean.unNat range_columnBegin,
    range_lineEnd = Glean.unNat range_lineEnd,
    range_columnEnd = Glean.unNat range_columnEnd + 1 -- n.b. exclusive end
  }

--
-- Internal stuff
--

unexpected :: a
unexpected = error "unexpected RangeSpan"

-- | (internal) Converts Glean-specific bytespan into client-visible line/col
-- offsets convert Glean ranges into customer range types, with exclusive line
-- end Warning: if the file doesn't have src.FileLines facts and the language
-- schema uses span instead of range, the exclusive range will be incorrect (see
-- type RangeSpan in codemarkup.angle)
-- Note: This logic is similar to `Range.byteRangeToRange` except this function
-- handles empty spans correctly. Inclusive bounds can't support empty spans.
fileByteSpanToExclusiveRange
  :: Maybe Range.LineOffsets -> Src.ByteSpan -> Glass.Range
fileByteSpanToExclusiveRange Nothing _ = def
fileByteSpanToExclusiveRange (Just lineoffs) bytespan =
  let -- be careful subtracting Word64 to avoid wrap-around
      be@Range.ByteRange{..} = Range.byteSpanToRange bytespan
      -- Convert from 0-based col to 1-based col for Src.Range
      (range_lineBegin, range_columnBegin) =
        Range.byteOffsetToLineCol lineoffs byteRange_begin
      (range_lineEnd, range_columnEnd) =
        Range.byteOffsetToLineCol
          lineoffs (Range.byteRangeExclusiveEnd be)
  in Glass.Range {
    range_lineBegin = fromIntegral range_lineBegin,
    range_columnBegin = fromIntegral $ range_columnBegin + 1,
    range_lineEnd = fromIntegral range_lineEnd,
    range_columnEnd = fromIntegral $ range_columnEnd + 1
  }


-- | (internal) Convert a src.Range from glean to a src.ByteSpan
inclusiveRangeToFileByteSpan
  :: Maybe Range.LineOffsets -> Src.Range -> Src.ByteSpan
inclusiveRangeToFileByteSpan Nothing _ = def
inclusiveRangeToFileByteSpan (Just lineoffs) range =
  Range.rangeToByteSpan (Range.srcRangeToSimpleByteRange lineoffs range)

-- | Memoize the result of computing the line offsets on a file
-- This provides up to 15x win for xrefs on large files
-- (internal)
memoLineOffsets :: Src.File -> Glean.RepoHaxl u w (Maybe Range.LineOffsets)
memoLineOffsets file = do
  repo <- Glean.haxlRepo
  key <- Glean.keyOf file
  Haxl.memo (repo, key) $ toLineOffsets file

-- | (internal) Get the line offsets associated with a file
-- Use the memoized version, memoLineOffsets
toLineOffsets :: Src.File -> Glean.RepoHaxl u w (Maybe Range.LineOffsets)
toLineOffsets file = do
  let fileId = Glean.getId file
  mlines <- Glean.getFirstResult (query (Query.fileLines fileId))
  case mlines of
    Nothing -> return Nothing
    Just offs -> Just . Range.lengthsToLineOffsets <$> Glean.keyOf offs

-- | (internal) Target cross-references. these are unresolved (raw) spans of
-- locations.  They can be resolved to Ranges via a FileLines call (client or
-- server-side).
toLocation
  :: RepoName -> Src.File -> Src.ByteSpan -> Glean.RepoHaxl u w Location
toLocation repo file bytespan = do
  path <- GleanPath <$> Glean.keyOf file
  let SymbolRepoPath location_repository location_filepath =
        fromGleanPath repo path
  return $ Location {
       location_repository = location_repository,
       location_filepath = location_filepath,
       location_span = spanToSpan bytespan
     }
  where
    -- | Span conversion. We hide any Glean-specific types
    spanToSpan :: Src.ByteSpan -> ByteSpan
    spanToSpan Src.ByteSpan{..} =
      ByteSpan {
        byteSpan_start = Glean.unNat byteSpan_start,
        byteSpan_length = Glean.unNat byteSpan_length
      }

-- | (internal) Like toLocation, but for ranges.
toLocationRange
  :: RepoName -> Src.File -> Range -> Glean.RepoHaxl u w LocationRange
toLocationRange repo file range = do
  path <- GleanPath <$> Glean.keyOf file
  let SymbolRepoPath locationRange_repository locationRange_filepath =
        fromGleanPath repo path
  return $ LocationRange {
       locationRange_repository = locationRange_repository,
       locationRange_filepath = locationRange_filepath,
       locationRange_range = range
     }
