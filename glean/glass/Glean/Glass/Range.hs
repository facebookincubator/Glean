{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Range
  (
  -- * working with ranges and bytespans, to and from Glean representations
  -- ** high level
    rangeSpanToLocation
  , rangeSpanToLocationRange
  -- ** lower level
  , memoRangeSpanToRange
  , rangeSpanToRange

  -- * locations, these have paths and repo names
  , resolveLocationToRange

  -- * File metadata
  , FileInfo(..)
  , getFileAndLines
  , getFile

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
-- ranges in exlclusive-end form, and paths adjusted to be repo-relative
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
  return $ rangeSpanToRange  file mOffsets rangespan
memoRangeSpanToRange file rangespan =
  pure $ rangeSpanToRange file Nothing rangespan

-- | Glean provides either a bytespan or a range for a symbol location.
-- Normalize it to Glass.Range (exclusive of end line/col spans)
-- Only use this if you know the offsets are already memoized.
rangeSpanToRange
  :: Src.File -> Maybe Range.LineOffsets -> Code.RangeSpan -> Range
rangeSpanToRange file offsets (Code.RangeSpan_span span) =
  fileByteSpanToExclusiveRange file offsets span
rangeSpanToRange _ _ (Code.RangeSpan_range range) =
  inclusiveRangeToExclusiveRange range
rangeSpanToRange _ _ Code.RangeSpan_EMPTY =
  unexpected

-- | Convert a client-side target locator to a specific line/col range
-- Used to implement location jumpTo
resolveLocationToRange
  :: Glean.Repo -> Location -> Glean.RepoHaxl u w (Either ErrorTy Range)
resolveLocationToRange repo Location{..} = do
  let path = toGleanPath location_repository location_filepath
  efile <- getFileAndLines repo path
  return $ flip fmap efile $ \ FileInfo{..} ->
    fileByteSpanToExclusiveRange srcFile offsets
      (spanFromSpan location_span)
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
    offsets :: !(Maybe Range.LineOffsets)
  }

-- | Get file metadata. Throw if we have no src.File
-- This is the first point we might encounter an unindexed file path
-- Note: we may not have lineoffsets, but this is not necessarily fatal
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
    Nothing -> Left $ NoSrcFileFact $ "No src.File fact for " <> gleanPath path
    Just srcFile -> Right srcFile

--
-- Internal stuff
--

unexpected :: a
unexpected = error "unexpected RangeSpan"

-- | (internal) Converts Glean-specific bytespan into client-visible line/col offsets
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

-- | (internal) Glean's Src.Range is inclusive of start/end. Glass is exclusive
-- of end
inclusiveRangeToExclusiveRange :: Src.Range -> Glass.Range
inclusiveRangeToExclusiveRange Src.Range{..} =
  Glass.Range {
    range_lineBegin = Glean.unNat range_lineBegin,
    range_columnBegin = Glean.unNat range_columnBegin,
    range_lineEnd = Glean.unNat range_lineEnd,
    range_columnEnd = Glean.unNat range_columnEnd + 1 -- n.b. exclusive end
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
  key <- Glean.keyOf file
  Haxl.memo key $ toLineOffsets file

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
  let (location_repository, location_filepath) =
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
  let (locationRange_repository, locationRange_filepath) =
        fromGleanPath repo path
  return $ LocationRange {
       locationRange_repository = locationRange_repository,
       locationRange_filepath = locationRange_filepath,
       locationRange_range = range
     }
