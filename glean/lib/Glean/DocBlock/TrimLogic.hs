{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


-- | Pure functions for doc block formatting as 'ByteString' (typically UTF-8)
module Glean.DocBlock.TrimLogic
  ( -- * process comment
    trimArtAndIndent
    -- * annotations
  , parseAnnotations
    -- * convert
    -- ** Text
  , toText
    -- ** list of lines
  , toLines, fromLines
  ) where

import Data.Array ( (!) )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Text.Regex.Base as RE
import qualified Text.Regex.PCRE.ByteString as RE
import Data.Word (Word8)

-- -----------------------------------------------------------------------------
-- Processing the DocBlock

-- | Utf8 'ByteString' to 'Text' (with replacement character if ill-formed)
toText :: ByteString -> Text
toText = decodeUtf8With lenientDecode

-- | Reversable splitting into list of lines (no newline in list of results)
--
--  > forall x. fromLines (toLines x) == x
toLines :: ByteString -> [ByteString]
toLines = BS.split newline
  where newline = 10

-- | Reverse 'toLines' by inserting newline.
--
--  > forall x. fromLines (toLines x) == x
fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate newline
  where newline = BS.singleton 10

-- 7-bit ASCII safe check as 'Word8' for 'ByteString' (safe subset of utf-8).
-- Cannot use 'Data.Word8.isSpace' or 'Data.ByteString.Char8' which
-- use Latin-1 (ISO-8859-1) 8-bit ASCII that includes NBSP 0xA0.
--
-- This matches the @\s@ whitespace character class in PCRE in default @"C"@
-- locale.
isSpace :: Word8 -> Bool
isSpace w = w == 32 || w == 9 || w == 10 || w == 11 || w == 12  || w == 13

trimRight :: ByteString -> ByteString
trimRight = fst . BS.spanEnd isSpace

trimLeft :: ByteString -> ByteString
trimLeft = BS.dropWhile isSpace

trimBlanks :: [ByteString] -> [ByteString]
trimBlanks = reverse . dropWhile BS.null . reverse . dropWhile BS.null

-- Tabs expand to 2 spaces
expandTabs :: ByteString -> ByteString
expandTabs b | tab `BS.elem` b = BS.intercalate "  " (BS.split tab b)
             | otherwise = b
  where tab = 9


-- All leading space in first line removed (doc block bytespan did not
-- have to start at first column).  The remaining lines get their common
-- leading spaces removed in a uniform shift (blank lines are unchanged).
trimIndent :: [ByteString] -> [ByteString]
trimIndent [] = []
trimIndent (h:ts) =
  let h' = trimLeft h
      ts' = case mapMaybe countSpace ts of
        [] -> ts
        xs@(_:_) -> let d = minimum xs
                    in if d == 0 then ts else map (BS.drop d) ts
  in h' : ts'
  where
    startSpaces = RE.makeRegexOpts RE.compBlank RE.execBlank
      ("^\\s*" :: ByteString) -- this always matches, but may be zero length
    countSpace s | BS.null s = Nothing -- empty lines do not contribute
                 | otherwise = snd . (!0) <$> RE.matchOnce startSpaces s

-- Input expects no tabs
--
-- * Many single line comments
--    * if first line starts // /// //! //< ///< //!<
-- then trim from each line, if present
-- * Block comment on one line as in /* foo */ or /** foo */ or /**< foo */
-- * Block comment on multiple lines
--    * Require first line to start /* /** /*! /*< /**< /*!< (or more *)
--    * The mid lines all have leading * trimmed, or none of them are changed
--    * Require last line to end */ and optionally have leading *
trimDocBlockArt :: [ByteString] -> [ByteString]
trimDocBlockArt bsIn = maybe trimIn trimBlanks $ case trimIn of
  xs@(h:_) | RE.matchTest slashSlashDoc h -> Just (trimSlashSlashDoc xs)
  [x] | Just y <- trimSlashStarsDoc x >>= trimStarsSlash -> Just [y]
  (h:ts@(_:_)) -> do
    h' <- trimSlashStarsDoc h
    let mid' = trimLeadingStar (init ts) -- change all lines or no lines
    tTmp <- trimStarsSlash (last ts) -- safe by ts@(_:_)
    -- also check for leading * from last line, separately from mid lines
    let t' = fromMaybe tTmp (dropLenM leadingStar tTmp)
    Just (h' : (mid' ++ [t']))
  _ -> Nothing
  where
    trimIn = trimBlanks (map trimRight bsIn)
    -- compile regex CAF
    -- We assert that removing any of these regex preserves trimRight:
    -- let y = trimRight x; z = "remove regex from y" in z == trimRight z
    mk :: ByteString -> RE.Regex
    mk = RE.makeRegexOpts RE.compBlank RE.execBlank
    slashSlashDoc = mk "^\\s*//[/!]?<?\\s?"
    slashStarsDoc = mk "^\\s*/\\*+!?<?\\s?"
    starsSlash = mk "\\s*\\*+/$"
    leadingStar = mk "^\\s*\\*"
    -- helpers
    wholeMatch regex source = (!0) <$> RE.matchOnce regex source
    takeStartM r s = case wholeMatch r s of -- assert start+len is BS.length
          Nothing -> Nothing
          Just (start, _len) -> Just (BS.take start s)
    dropLenM r s = case wholeMatch r s of  -- assert start is always 0
          Nothing -> Nothing
          Just (_start, len) -> Just (BS.drop len s)
    -- building blocks
    trimSlashSlashDoc = map (\s -> fromMaybe s (dropLenM slashSlashDoc s))
    trimSlashStarsDoc = dropLenM slashStarsDoc
    trimLeadingStar xs = fromMaybe xs (mapM (dropLenM leadingStar) xs)
    trimStarsSlash = takeStartM starsSlash

-- | Remove comment markers, trailing space, tabs, and leading indentation.
--
-- Input is list of lines (e.g. from 'toLines'), with no newlines.
--
-- Output is list of same length.
trimArtAndIndent :: [ByteString] -> [ByteString]
trimArtAndIndent = trimIndent . trimDocBlockArt . map expandTabs

-- | Parse all @-tag (single-line) annotations.
--
-- Input is a list of lines (no newlines, see 'toLines') from
-- 'trimDocBlockArt' (to remove comment markers).
--
-- Output is '(key, value)' pairs, in input order.
-- 'key' is non-empty and free of whitespaces.
-- 'value' may be empty, has no leading or trailing spaces,
-- may contain whitespace.
parseAnnotations :: [ByteString] -> [(Text, Text)]
parseAnnotations = mapMaybe parseSingleLine

parseSingleLine:: ByteString -> Maybe (Text, Text)
parseSingleLine bs = extract <$> RE.matchOnce keyValue bs
  where
    -- tolerate and ignore a leading '*' (optional capture index 1)
    -- ignore leading whitespace and match literal @ then
    -- non-empty, non-whitespace key (capture index 2)
    -- rest of line is raw value (capture index 3)
    keyValue :: RE.Regex
    keyValue = RE.makeRegexOpts RE.compBlank RE.execBlank
      ("^(\\s*\\*)?\\s*@(\\S+)(.*)$" :: ByteString)

    pick :: (RE.MatchOffset, RE.MatchLength) -> ByteString
    pick (offset, len) = BS.take len (BS.drop offset bs)

    extract :: RE.MatchArray -> (Text, Text)
    extract a =
      let key = pick (a ! 2)
          value = trimRight $ trimLeft $ pick (a ! 3)
      in (toText key, toText value)
