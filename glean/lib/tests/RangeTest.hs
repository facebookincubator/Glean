-- Copyright (c) Facebook, Inc. and its affiliates.

-- | Can you write a binary search?
module RangeTest
  ( main
  ) where

import qualified Data.ByteString as B
import Data.Int ( Int64 )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word ( Word64 )
import Test.HUnit
import Test.QuickCheck

import TestRunner
import Util.Testing

import Glean
import Glean.Init
import Glean.Util.Range
import Glean.Schema.Src.Types as Src hiding (ByteRange(..))

nats :: [Int64] -> [Nat]
nats = map Nat

f :: Src.File
f = Src.File 12345 Nothing

-- | The lengths must be positive
flkOf :: [Int64] -> Bool -> Src.FileLines_key
flkOf lengths endsInNewline
  | any (<=0) lengths = error "lengths must be positive"
  | otherwise = Src.FileLines_key
      { fileLines_key_file = f
      , fileLines_key_lengths = nats lengths
      , fileLines_key_endsInNewline = endsInNewline
      , fileLines_key_hasUnicodeOrTabs = False
      }

testEnc :: Text -> [Int64] -> Bool -> Test
testEnc text lengths endsInNewline =
  let name = "testEnc " <> show text
  in TestLabel name $ TestCase $
        assertEqual name
          (getLineOffsets (Text.encodeUtf8 text))
          (lengthsToLineOffsets (flkOf lengths endsInNewline))

testsManual :: [Test]
testsManual =
  [ testEnc "" [] False
  , testEnc "x" [1] False
  , testEnc "\n" [1] True
  , testEnc "xx" [2] False
  , testEnc "x\n" [2] True
  , testEnc "\nx" [1,1] False
  , testEnc "\n\n" [1,1] True
  , testEnc "xxx" [3] False
  , testEnc "xx\n" [3] True
  , testEnc "x\nx" [2,1] False
  , testEnc "x\n\n" [2,1] True
  , testEnc "\nxx" [1,2] False
  , testEnc "\nx\n" [1,2] True
  , testEnc "\n\nx" [1,1,1] False
  , testEnc "\n\n\n" [1,1,1] True
  , testEnc "xx\n\n" [3,1] True
  , testEnc "x\n\nx" [2,1,1] False
  , testEnc "\n\nxx" [1,1,2] False
  , testEnc "x\nx\n" [2,2] True
  , testEnc "\nx\nx" [1,2,1] False
  , testEnc "\nxx\n" [1,3] True
  ]

toFileLinesKey :: Id -> Text -> Src.FileLines_key
toFileLinesKey fileId text =
  let lenUtf8 = Nat . fromIntegral . B.length . Text.encodeUtf8
      toLengths :: Text -> [Nat]
      toLengths t
          | Text.null t = []
          | otherwise = maybe end hit (Text.findIndex ('\n'==) t)
        where end = [lenUtf8 t]
              hit i = let (x, y) = Text.splitAt (succ i) t
                      in lenUtf8 x : toLengths y
      big = toEnum 128
  in Src.FileLines_key
      { fileLines_key_file = Src.File fileId Nothing
      , fileLines_key_lengths = toLengths text
      , fileLines_key_endsInNewline =
          if Text.null text then False else '\n' == Text.last text
      , fileLines_key_hasUnicodeOrTabs =
          Text.any (\c -> c == '\t' ||  big <= c) text
      }

sameLineOffsets :: Text -> Property
sameLineOffsets text = (===)
    (getLineOffsets (Text.encodeUtf8 text))
    (lengthsToLineOffsets (toFileLinesKey 12345 text))

genFile1 :: (String, Gen Text)
genFile1 = ("half",) $ Text.pack <$> listOf (frequency x)
  where x = [ (1, pure 'x')
            , (1, pure '\n') ]

genFile2 :: (String, Gen Text)
genFile2 = ("tabs_unicode",) $ Text.pack <$> listOf (frequency x)
  where x = [ (1, pure 'x')
            , (1, pure '\n')
            , (1, pure '\t')
            , (1, pure '\x142')  -- latin small letter l with stroke
            , (1, pure '\x26FB') -- bank
            ]

genFile3 :: (String, Gen Text)
genFile3 = ("long",) $ Text.pack <$> listOf (frequency x)
  where x = [ (100, pure 'x')
            , (1, pure '\n') ]

genFile4 :: (String, Gen Text)
genFile4 = ("short",) $ Text.pack <$> listOf (frequency x)
  where x = [ (1, pure 'x')
            , (100, pure '\n') ]

testsQuick :: [Test]
testsQuick =
  [ TestLabel name $ TestCase $ assertProperty name
      $ forAll (resize 100 g) sameLineOffsets
  | (n, g) <- [ genFile1, genFile2, genFile3, genFile4 ]
  , let name = "sameLineOffsets: " <> n
  ]

test_byteOffsetToLineCol :: [Test]
test_byteOffsetToLineCol = map TestList
    [ let t = m "x"
      in [ t 0 (1, 0)
         ]
    , let t = m "\n"
      in [ t 0 (1, 0)
         ]
    , let t = m "xx"
      in [ t 0 (1, 0)
         , t 1 (1, 1)
         ]
    , let t = m "x\n"
      in [ t 0 (1, 0)
         , t 1 (1, 1)
         ]
    , let t = m "\nx"
      in [ t 0 (1, 0)
         , t 1 (2, 0)
         ]
    , let t = m "\n\n"
      in [ t 0 (1, 0)
         , t 1 (2, 0)
         ]
    , let t = m "x\nx\n"
      in [ t 0 (1, 0)
         , t 1 (1, 1)
         , t 2 (2, 0)
         , t 3 (2, 1)
         ]
    , let t = m "x\nx\nx"
      in [ t 0 (1, 0)
         , t 1 (1, 1)
         , t 2 (2, 0)
         , t 3 (2, 1)
         , t 4 (3, 0)
         ]
    ]
  where
    m :: Text -> Word64 -> (Word64, Word64) -> Test
    m text = tt text (getLineOffsets (Text.encodeUtf8 text))

    tt :: Text -> LineOffsets -> Word64 -> (Word64, Word64) -> Test
    tt text lo b (l, c) =
      let name :: String
          name = "test_byteOffsetToLineCol " <> show text <> " " <> show b
      in TestLabel name $ TestCase $
          assertEqual name (byteOffsetToLineCol lo b) (l, c)

testSrcRangeToByteRange :: [Test]
testSrcRangeToByteRange =
  [ let name = "test_srcRangeToByteRange " ++ show text
    in
      TestLabel name $ TestCase $
        let
          lines = toFileLinesKey fileId text
          f = srcRangeToByteRange lines (Text.encodeUtf8 text)
        in do
        print lines
        assertEqual name byteRange (f srcRange)
  | (text, srcRange, byteRange) <- tests
  ]
  where
  fileId = 12345
  file = Src.File 12345 Nothing
  mkSrcRange a b c d = Src.Range file (toNat a) (toNat b) (toNat c) (toNat d)
  tests =
    [ ("a\na", mkSrcRange 1 1 1 1, ByteRange 0 1)
    , ("b\nb", mkSrcRange 2 1 2 1, ByteRange 2 1)
    , ("\nx\9312x\n", mkSrcRange 2 1 2 3, ByteRange 1 5)
      -- code point with 3-byte UTF-8 encoding
    , ("\ny\9312y\n", mkSrcRange 2 1 2 5, ByteRange 1 6)
      -- end column is out-of-range: clamped to the EOL
    ]

main :: IO ()
main = withUnitTest $ testRunner $
  TestList $
    testsManual ++
    testsQuick ++
    test_byteOffsetToLineCol ++
    testSrcRangeToByteRange
