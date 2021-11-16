{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
-- | There are several concepts of ranges to convert between.
module Glean.Util.Range
  ( -- * source range get/set
    HasSrcRange(..)
    -- * byte range
  , ByteRange(..)
  , byteRangeExclusiveEnd
  , byteRangeContains
  , srcRangeToByteRange
  , relByteSpansToRanges
  , rangesToRelSpans
  , byteSpanToRange
  , rangeToByteSpan
  -- * comparisons
  , compareLineCol
  , rangeContains
  , compareRange
  , topLevelRanges
  -- * LineOffsets
  , LineOffsets(..), getLineOffsets, lengthsToLineOffsets, firstLineOffsetsDiff
  , sameUpToLastNewline, debugLineOffsets
  , srcRangeToFileLocation
  , srcRangeToSimpleByteRange
  -- ** converting with LineOffsets
  , byteOffsetToLineCol
  , byteRangeToRange
  , byteRangesToLineNumbers
  -- * Src.Loc
  , locRange
  ) where

import Control.DeepSeq
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromMaybe)
import qualified Data.String.UTF8 as UTF8 hiding (take)
import qualified Data.Vector as BoxedVector
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import Data.Word

import Glean (toNat, fromNat)
import Glean.Schema.Pp1.Types as Pp
import Glean.Schema.Src.Types as Src hiding (ByteRange(..))
import Glean.Schema.Cxx1.Types as Cxx

-- | Make a single character Range from a Loc. This utility is needed
-- because the java schema stores 'Src.Loc' and we need to upscale to
-- 'Src.Range'
locRange :: Src.Loc -> Src.Range
locRange Src.Loc{..} = Src.Range
  { range_file = loc_file
  , range_lineBegin = loc_line
  , range_columnBegin = loc_column
  , range_lineEnd = loc_line
  , range_columnEnd = loc_column
  }

class HasSrcRange a where
  srcRange :: a -> Src.Range
  setSrcRange :: Src.Range -> a -> a

instance HasSrcRange Cxx.NamespaceDeclaration_key where
  srcRange = namespaceDeclaration_key_source
  setSrcRange r a = a{ namespaceDeclaration_key_source = r }

instance HasSrcRange Cxx.RecordDeclaration_key where
  srcRange = recordDeclaration_key_source
  setSrcRange r a = a{ recordDeclaration_key_source = r }

instance HasSrcRange Cxx.EnumDeclaration_key where
  srcRange = enumDeclaration_key_source
  setSrcRange r a = a{ enumDeclaration_key_source = r }

instance HasSrcRange Cxx.FunctionDeclaration_key where
  srcRange = functionDeclaration_key_source
  setSrcRange r a = a{ functionDeclaration_key_source = r }

instance HasSrcRange Cxx.ObjcContainerDeclaration_key where
  srcRange = objcContainerDeclaration_key_source
  setSrcRange r a = a{ objcContainerDeclaration_key_source = r }

instance HasSrcRange Cxx.ObjcMethodDeclaration_key where
  srcRange = objcMethodDeclaration_key_source
  setSrcRange r a = a{ objcMethodDeclaration_key_source = r }

instance HasSrcRange Cxx.ObjcPropertyDeclaration_key where
  srcRange = objcPropertyDeclaration_key_source
  setSrcRange r a = a{ objcPropertyDeclaration_key_source = r }

instance HasSrcRange Cxx.UsingDeclaration_key where
  srcRange = usingDeclaration_key_source
  setSrcRange r a = a{ usingDeclaration_key_source = r }

instance HasSrcRange Cxx.UsingDirective_key where
  srcRange = usingDirective_key_source
  setSrcRange r a = a{ usingDirective_key_source = r }

instance HasSrcRange Cxx.TypeAliasDeclaration_key where
  srcRange = typeAliasDeclaration_key_source
  setSrcRange r a = a{ typeAliasDeclaration_key_source = r }

instance HasSrcRange Cxx.VariableDeclaration_key where
  srcRange = variableDeclaration_key_source
  setSrcRange r a = a{ variableDeclaration_key_source = r }

instance HasSrcRange Cxx.Enumerator_key where
  srcRange = enumerator_key_source
  setSrcRange r a = a{ enumerator_key_source = r }

instance HasSrcRange Pp.Define_key where
  srcRange = define_key_source
  setSrcRange r a = a{ define_key_source = r }

instance HasSrcRange Pp.Use_key where
  srcRange = use_key_source
  setSrcRange r a = a{ use_key_source = r }

data ByteRange = ByteRange
  { byteRange_begin :: {-# UNPACK #-}!Word64
  , byteRange_length :: {-# UNPACK #-}!Word64
  }
  deriving (Show, Eq, Ord)

instance NFData ByteRange where
  rnf = (`seq` ())

-- | Compute the (exclusive) end offset
byteRangeExclusiveEnd :: ByteRange -> Word64
byteRangeExclusiveEnd ByteRange{..} = byteRange_begin + byteRange_length

-- | byteRangeContains r1 r2 is True iff r1 contains the range r2
byteRangeContains :: ByteRange -> ByteRange -> Bool
byteRangeContains r1 r2 =
  byteRange_begin r1 <= byteRange_begin r2 &&
  byteRangeExclusiveEnd r1 >= byteRangeExclusiveEnd r2

-- | Positions of start of each line in bytes.
--
-- Index `lineOffsets` with 0-based line number to get start of that line.
--
-- Never empty, the first value is always zero, strictly monotonically
-- ascending.
data LineOffsets = LineOffsets
  { lineOffsets :: Vector Word64
  , lineOffsets_endsInNewline :: Bool
  , lineOffsets_hasUnicodeOrTabs :: Bool
  } deriving ( Eq, Show, Ord )

-- | Help with debugging by showin a multi-line friendly view of 'LineOffsets'
-- prefixed with the provided 'String' label.
debugLineOffsets :: String -> LineOffsets -> String
debugLineOffsets msg x =
  let prefix = msg <> ": "
      header =
        [ "Length of lineOffsets :: Vector Word64 = "
          <> show (Vector.length (lineOffsets x))
        , "Ends in newline: " <> show (lineOffsets_endsInNewline x)
        , "Has unicode or tabs: " <> show (lineOffsets_hasUnicodeOrTabs x) ]
      lengths = map show (chunksOf 10 (Vector.toList
        (lineOffsets x)))
  in unlines (map (prefix<>) (header ++ lengths))

-- | This finds the newlines (each line includes terminating newline) as well
-- as the final line.
getLineOffsets :: ByteString -> LineOffsets
getLineOffsets bs
  | B.null bs = LineOffsets (Vector.singleton 0) False False
  | otherwise =
    let newlines = map (succ . fromIntegral) (B.elemIndices '\n' bs)
        lineOffsets_hasUnicodeOrTabs = B.any (\c -> c == '\t' || c >= big) bs
          where big = toEnum 128
    in if B.last bs == '\n'
        then LineOffsets
          { lineOffsets = Vector.fromList . (0:) $ newlines
          , lineOffsets_endsInNewline = True
          , lineOffsets_hasUnicodeOrTabs }
        else LineOffsets
          { lineOffsets = Vector.fromList . (0:)
             . (++ [fromIntegral (B.length bs)])
             $ newlines
          , lineOffsets_endsInNewline = False
          , lineOffsets_hasUnicodeOrTabs }

-- | This assumes that the fileLines_key_lengths are strictly positive
lengthsToLineOffsets :: Src.FileLines_key -> LineOffsets
lengthsToLineOffsets flk = LineOffsets
  { lineOffsets = Vector.fromList $
      scanl (+) 0 (map fromNat (fileLines_key_lengths flk))
  , lineOffsets_endsInNewline = fileLines_key_endsInNewline flk
  , lineOffsets_hasUnicodeOrTabs = fileLines_key_hasUnicodeOrTabs flk }

-- | For 0-based lines, the index of the first line to disagree, if any.
--
-- If one vector is an incomplete prefix of the other vector, then the
-- index of first difference will be the (length-1) of the shorter vector.
--
-- The returned Int is always greater than or equal to zero
-- (a good 0-based line number).
firstLineOffsetsDiff :: LineOffsets -> LineOffsets -> Maybe Int
firstLineOffsetsDiff lo1 lo2 =
    case Vector.findIndex not (Vector.zipWith (==) v1 v2) of
      Nothing -> if Vector.length v1 == Vector.length v2
        then Nothing -- assert than 0 < length v1 && 0 < length v2
        else Just (pred (min (Vector.length v1) (Vector.length v2)))
      Just i -> Just (pred i) -- assert that 0 < i
  where
    v1 = lineOffsets lo1
    v2 = lineOffsets lo2

-- | Diffusion is crazy and omits the trailing newline.  So cope with
-- only differing at the trailing newline. Returns 'True' when the two
-- arguments have identical length lines, or if they merely disagree about the
-- final newline and so thus one (with final newline) has the last line length
-- of 1 greater than the other.
sameUpToLastNewline :: LineOffsets -> LineOffsets -> Bool
sameUpToLastNewline a b = case firstLineOffsetsDiff a b of
  Nothing -> True
  Just line0 -> lineOffsets_endsInNewline x
    && not (lineOffsets_endsInNewline y)
    && (Vector.length (lineOffsets x) == Vector.length (lineOffsets y))
    && (2 + line0 == Vector.length (lineOffsets x))
    && (Vector.last (lineOffsets x) == 1 + Vector.last (lineOffsets y))
  where
    -- If at least one of a or b ends in newline then x ends in newline
    (x, y) | lineOffsets_endsInNewline a = (a, b)
           | otherwise = (b, a)


-- | Using the information in 'LineOffsets' this converts a 'Word64' byte
-- offset into a pair of (1-based line, 0-based column).
--
-- This is a total function, but there are numerous unspecified conditions
-- (which may result in the line number being 0).
--
-- This requires 'lineOffsets' to be non-empty and ascending with no duplicates
-- to get a sane return value.
--
-- This does handle 'lineOffsets_endsInNewline' correctly.
--
-- This does not handle 'lineOffsets_hasUnicodeOrTabs' yet, so column
-- number is actually byte offset along that line and only correct for Ascii.
-- ( To handle this would require the actual 'Text' or utf8 'ByteString' )
-- Tracked in T62313027
--
-- If you ask for a byte offset smaller than the first 'lineOffsets' value then
-- then return is unspecified.
-- If you ask for a byte offset larger than the last in 'lineOffsets' value then
-- the return is unspecified.
-- If 'lineOffsets' is empty then the line return is unspecified.
-- If 'lineOffsets' has duplicates then the line return is unspecified.
-- if 'lineOffsets' is ever descending then the line returned is unspecified.
--
-- Unicode versus column number
--
-- This needs to become utf-8 aware, and make column count codepoints.
-- utf-8 bytes can be classified as
-- * 0 .. 127 : Ascii
-- * 128..191 : continue multibyte codepoint
-- * 192 .. 193 : invalid
-- * 194 .. 244 : Start of multibyte codepoint
-- * 245 .. 255 : invalid
-- Need to count Ascii and Start characters from the start of the line to
-- the current position.
--
-- For bonus points, ignore utf-8 BOM in the first line (0xEF, 0xBB, 0xBF).
byteOffsetToLineCol :: LineOffsets -> Word64 -> (Word64, Word64)
byteOffsetToLineCol LineOffsets{lineOffsets=a, ..}
    | Vector.length a == 0 = (0,)   -- should not happen, unspecified answer
    | Vector.length a == 1 = \p ->
        if p < a!0 then (0, p)      -- before first line, unspecified answer
                   else done 0 p    -- only one line, you are on it
    | otherwise = search
  where
    search p | p < a!0 = (0, p)     -- before first line, unspecified answer
             | a!e <= p = done e p  -- after final newline (not impossible)
             | otherwise = go 0 e   -- know 0 < e due to above guards
      where
        -- know 0 <= e since 2 <= Vector.length a due to original guards
        e | lineOffsets_endsInNewline = pred (Vector.length a) -- final \n
          | otherwise = pred (pred (Vector.length a)) -- final \n is earlier
        go :: Int -> Int -> (Word64, Word64)
        go i j = -- know (a!i) <= p < (a!j) and i < j
          if succ i == j -- found precise line
          then done i p
          else  -- know i < succ i < j
               let mid = i + ((j - i + 1) `quot` 2) -- know i < mid < j
               in if p < a!mid
                  then go i mid -- know (a!i) <= p < (a!mid) and i < mid
                  else go mid j -- know (a!mid) <= p < (a!j) and mid < j
    -- Does not handle tabs or unicode yet
    done :: Int -> Word64 -> (Word64, Word64)
    done line_zero_based bytes =
      let line_one_based = fromIntegral (succ line_zero_based)
          column_zero_based = bytes - a!line_zero_based
      in (line_one_based, column_zero_based )

-- | Convert a Src.Range to a ByteRange, given a Src.FileLines
srcRangeToByteRange
  :: Src.FileLines_key
  -> ByteString
  -> (Src.Range -> ByteRange)
srcRangeToByteRange flk contents =
  let
    LineOffsets{..} = lengthsToLineOffsets flk

    -- Takes a single line and computes vector of utf-8 byte offsets of each
    -- unicode character (codepoint) in the row.
    -- Index the vector with a 0-based character-col
    -- and the values is the 0-based byte-offset-col
    --
    -- tabs characters are not expanded, so count as 1 byte wide.
    charOffsets :: ByteString -> Vector Word64
    charOffsets b = Vector.fromList $ 0 : go 0 (UTF8.fromRep b)
      where
      go :: Int -> UTF8.UTF8 ByteString -> [Word64]
      go off rest = case UTF8.uncons rest of
        Nothing -> []
        Just (_, more) -> fromIntegral newOffset : go newOffset more
          where newOffset = off + B.length (UTF8.toRep rest)
                  - B.length (UTF8.toRep more)

    -- Take 0-based line and character-column, return 0-based byte-offset-column
    -- This is always identity when there are no unicode or tab characters
    charToByteCol :: Int -> Int -> Word64
    charToByteCol
      | lineOffsets_hasUnicodeOrTabs = \line col ->
         let
           offsets = lineVec BoxedVector.! line
           size = Vector.length offsets
         in
         -- Don't return _|_ if the column is out of range, just clamp
         -- to the end of the line.
         if col < size
            then offsets ! col
            else offsets ! (size-1)
      | otherwise = \_ col -> fromIntegral col
      where
      lineVec = BoxedVector.fromList $ map charOffsets (B.lines contents)

  in
    srcRangeToAdjustedByteRange (Just charToByteCol) lineOffsets

-- | Convert a 'Src.Range' to a 'ByteRange', given a 'LineOffsets{lineOffsets}'
-- and an optional function to correct for unicode and tab characters
-- (where bytes and characters are not the same width).
--
-- If Nothing is passed then this assumes characters are one byte wide.
-- In particular this assumes the 'columnBegin' and 'columnEnd' in the
-- 'Src.Range' are the same as byte offsets. Thus Nothing is equivalent to
-- passing @(\ _ col -> fromIntegral col)@ as the function.
srcRangeToAdjustedByteRange
  :: Maybe (Int -> Int -> Word64)
    -- ^ converts column from character count to byte count.
    -- Takes line (0-based) and column (0-based character count)
    -- to 0-based byte-count column.
  -> Vector Word64
  -> Src.Range
  -> ByteRange
srcRangeToAdjustedByteRange mCharToByte lineOffsets Src.Range{..} =
  let
    -- Convert from 1-based line and col (from Src.Range) to 0-based, and
    -- be careful subtracting Word64 to avoid wrap-around
    prevNat n = let w = fromNat n in fromIntegral (pred (max 1 w))
    charToByteCol = fromMaybe (\ _ col -> fromIntegral col) mCharToByte

    -- compute 0-based lines and column coordinates from Src.Range
    -- * lines need to be 0-based to be an index into lineOffsets
    --     and to be passed to charToByteCol
    -- * columns need to be 0-based to be passed to charToByteCol
    --     (and then add to the byte offset)
    lineBegin = prevNat range_lineBegin
    lineEnd = prevNat range_lineEnd
    colBegin = prevNat range_columnBegin
    colEnd = prevNat range_columnEnd
    -- Note: range_columnEnd is the column of the last char (inclusive)
    -- so colEnd is inclusive, and so is inclusiveEnd
    begin = (lineOffsets ! lineBegin) + charToByteCol lineBegin colBegin
    inclusiveEnd = (lineOffsets ! lineEnd) + charToByteCol lineEnd colEnd
    -- be careful subtracting Word64 to avoid wrap-around
    len | begin <= inclusiveEnd = (inclusiveEnd - begin) + 1
        | otherwise = 0
  in
  ByteRange{ byteRange_begin = begin, byteRange_length = len }

-- | Convert a Src.Range to a ByteRange, given a LineOffsets, and assuming
-- columns count bytes (assuming characters and bytes and the same width).
srcRangeToSimpleByteRange :: LineOffsets -> Src.Range -> ByteRange
srcRangeToSimpleByteRange = srcRangeToAdjustedByteRange Nothing . lineOffsets

-- | Convert a Src.Range to a Src.FileLocation, given the length of each line
-- of the file. CAN'T be used with files that have unicode or tab characters.
srcRangeToFileLocation
  :: [Int]
  -> (Src.Range -> Src.FileLocation)
srcRangeToFileLocation lengths =
  let
    lineOffsets :: Vector Word64
    lineOffsets = Vector.fromList $
      scanl (+) 0 (map fromIntegral lengths)
  in
    \ sr ->
      let
        br = srcRangeToAdjustedByteRange Nothing lineOffsets sr
      in
        Src.FileLocation
          { fileLocation_file = range_file sr
          , fileLocation_span = Src.ByteSpan
            { byteSpan_start = toNat (byteRange_begin br)
            , byteSpan_length = toNat (byteRange_length br)
            }
          }

relByteSpansToRanges :: [Src.RelByteSpan] -> [ByteRange]
relByteSpansToRanges spans = go 0 spans
  where
  go _ [] = []
  go !prev (Src.RelByteSpan{..} : rest) = br : go (byteRange_begin br) rest
    where
      br = ByteRange
        { byteRange_begin = fromNat relByteSpan_offset + prev
        , byteRange_length = fromNat relByteSpan_length }

rangesToRelSpans :: [ByteRange] -> [Src.RelByteSpan]
rangesToRelSpans = go 0
  where
  go !m (ByteRange{byteRange_begin = begin, byteRange_length = len}: spans) =
    Src.RelByteSpan (toNat (begin-m)) (toNat len) : go begin spans
  go _ [] = []

-- | Convert Src schema bytespans to ranges
byteSpanToRange :: Src.ByteSpan -> ByteRange
byteSpanToRange Src.ByteSpan{..} = ByteRange
  { byteRange_begin = fromNat byteSpan_start
  , byteRange_length = fromNat byteSpan_length
  }

-- | Convert byte ranges to Src schema bytespans
rangeToByteSpan :: ByteRange -> Src.ByteSpan
rangeToByteSpan ByteRange{..} = Src.ByteSpan
  { byteSpan_start = toNat byteRange_begin
  , byteSpan_length = toNat byteRange_length
  }

-- | Convert from byte coordinates to (line, col) coordinates.
--
-- This does not yet handle uncode or tabs, which would require access
-- the contents of the file.
byteRangeToRange :: Src.File -> LineOffsets -> ByteRange -> Src.Range
byteRangeToRange range_file lineOffsets be@ByteRange{..} =
  let -- be careful subtracting Word64 to avoid wrap-around
      inclusiveEnd = pred (max 1 (byteRangeExclusiveEnd be))
      -- Convert from 0-based col to 1-based col for Src.Range
      nextNat w = toNat (succ w)
      (range_lineBegin, range_columnBegin) = bimap toNat nextNat $
        byteOffsetToLineCol lineOffsets byteRange_begin
      (range_lineEnd, range_columnEnd) = bimap toNat nextNat $
        byteOffsetToLineCol lineOffsets inclusiveEnd
  in Src.Range{..}

byteRangesToLineNumbers :: LineOffsets -> [ByteRange] -> [Word64]
byteRangesToLineNumbers lineOffsets =  map
  (fst . byteOffsetToLineCol lineOffsets . byteRange_begin)

-- | @compareLineCol line col a b@ is a helper for comparing @(line, col)@
-- coordinates of @a@ and @b@
compareLineCol :: Ord x => (a -> x) -> (a -> x) -> a -> a -> Ordering
compareLineCol line col a b = (compare `on` line) a b <> (compare `on` col) a b

-- | @rangeContains x y@ is True iff the range of y is the same as x, or
-- inside x.
rangeContains :: Src.Range -> Src.Range -> Bool
rangeContains big small =
  ((==) `on` Src.range_file) big small &&
  checkLE Src.range_lineBegin Src.range_columnBegin big small &&
  checkLE Src.range_lineEnd Src.range_columnEnd small big
  where
    checkLE line col a b = GT /= compareLineCol line col a b

-- | Order is by file id, ascending begin position, descending end position.
compareRange :: Src.Range -> Src.Range -> Ordering
compareRange r1 r2 =
  (compare `on` (Src.file_id . Src.range_file)) r1 r2 <>
  compareLineCol Src.range_lineBegin Src.range_columnBegin r1 r2 <>
  compareLineCol Src.range_lineEnd Src.range_columnEnd r2 r1

-- | Sort and filter the @[a]@ removing contained items. Order is by file,
-- ascending begin position, descending end position.
topLevelRanges :: (a -> Src.Range) -> [a] -> [a]
topLevelRanges f = map head . groupBy (rangeContains `on` f)
  . sortBy (compareRange `on` f)
