-- Copyright 2004-present Facebook. All Rights Reserved.

-- | Pure functions for doc block formatting as 'ByteString' (typically UTF-8)
module Glean.DocBlock.TrimLogic
  ( -- * BS/Text
    toText, fromText
    -- * lines
  , toLines, fromLines
    -- * white space
  , trimRight, trimLeft, trimBlanks, trimIndent
    -- ** tabs
  , expandTabs
    -- * process
  , trimDocBlockArt
  , trimArtAndIndent
  ) where

import Data.Array ( (!) )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Text.Regex.Base as RE
import qualified Text.Regex.PCRE.ByteString as RE
import Data.Word (Word8)

-- -----------------------------------------------------------------------------
-- Processing the DocBlock

-- | Utf8 'ByteString' from 'Text'
fromText :: Text -> ByteString
fromText = encodeUtf8

-- | Utf8 'ByteString' to 'Text' (with replacement character if ill-formed)
toText :: ByteString -> Text
toText = decodeUtf8With lenientDecode

-- | Reversable splitting into list of lines (no newline in results)
--
--  > forall x. fromLines (toLines x) == x
toLines :: ByteString -> [ByteString]
toLines = BS.split newline
  where newline = 10

-- | Reverse 'toLines'
--
--  > forall x. fromLines (toLines x) == x
fromLines :: [ByteString] -> ByteString
fromLines = BS.intercalate newline
  where newline = BS.singleton 10

-- | 7-bit ASCII safe check as 'Word8' for 'ByteString' (safe subset of utf-8).
-- Cannot use 'Data.Word8.isSpace' which uses Latin-1 (ISO-8859-1) 8-bit ASCII.
isSpace :: Word8 -> Bool
isSpace w = w == 32 || w == 9 || w == 10 || w == 11 || w == 12  || w == 13

trimRight :: ByteString -> ByteString
trimRight = fst . BS.spanEnd isSpace

trimLeft :: ByteString -> ByteString
trimLeft = BS.dropWhile isSpace

trimBlanks :: [ByteString] -> [ByteString]
trimBlanks = reverse . dropWhile BS.null . reverse . dropWhile BS.null

-- | Tabs expand to 2 spaces
expandTabs :: ByteString -> ByteString
expandTabs b | BS.any (tab ==) b = BS.intercalate "  " (BS.split tab b)
             | otherwise = b
  where tab = 9

-- | All leading space in first line removed (doc block bytespan did not
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

-- |
-- Input expects no tabs
--
-- * Many single line comments
--    * if first line starts // /// //! then trim from each line, if present
-- * Block comment on one line as in /** foo */
-- * Block comment on multiple lines
--    * Require first line to start /* /** /*!
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
    slashSlashDoc = mk "^\\s*//[/!]?\\s?"
    slashStarsDoc = mk "^\\s*/\\*+!?\\s?"
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

-- | Input is 'toLines'.
-- Remove comment markers, trailing space, tabs, and indent
trimArtAndIndent :: [ByteString] -> [ByteString]
trimArtAndIndent = trimIndent . trimDocBlockArt . map expandTabs
