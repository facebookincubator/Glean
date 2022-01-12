{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module RTSJSONTest (main) where

import Control.Monad.ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Arbitrary ()
import qualified Data.Text.Encoding as Text
import Test.HUnit hiding (assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import TestRunner
import qualified Util.Buffer as Buffer
import Util.Testing

import Glean.Init (withUnitTest)
import qualified Glean.RTS as RTS
import qualified Glean.RTS.Foreign.JSON as RTS

prop_number :: Int64 -> Property
prop_number n =
  runST (Buffer.fillByteString 21 $ Buffer.alloc 21 $ RTS.encodeNumber n)
  ===
  Char8.pack (show n)

prop_number_roundtrip :: Int64 -> Property
prop_number_roundtrip n = monadicIO $ do
  let !enc = runST $
        Buffer.fillByteString 21 $ Buffer.alloc 21 $ RTS.encodeNumber n
  m <- run $ RTS.withParsed enc $ \val -> case val of
    RTS.Int m -> return m
    _ -> fail "not a number"
  assert (m == n)

prop_string_roundtrip :: Text -> Property
prop_string_roundtrip t = monadicIO $ do
  enc <- run $ RTS.unsafeWithByteStringRef utf8 $ \ref -> stToIO $ do
    n <- RTS.getStringEscapedSize ref
    Buffer.fillByteString n $ do
      Buffer.ascii '\"'
      Buffer.alloc n $ \p -> do
        RTS.escapeString ref p n
        return n
      Buffer.ascii '\"'
  s <- run $ RTS.withParsed enc $ \val -> case val of
    RTS.String ref -> RTS.derefByteString ref
    _ -> fail "not a string"
  assert (s == utf8)
  where
    utf8 = Text.encodeUtf8 t

prop_mangled_string_roundtrip :: Text -> Property
prop_mangled_string_roundtrip t = monadicIO $ do
  enc <- run $ RTS.unsafeWithMangledStringRef (mangle utf8)
    $ \ref -> stToIO $ do
      n <- RTS.getMangledStringEscapedSize ref
      Buffer.fillByteString n $ do
        Buffer.ascii '\"'
        Buffer.alloc n $ \p -> do
          RTS.escapeMangledString ref p n
          return n
        Buffer.ascii '\"'
  s <- run $ RTS.withParsed enc $ \val -> case val of
    RTS.String ref -> RTS.derefByteString ref
    _ -> fail "not a string"
  assert (s == utf8)
  where
    utf8 = Text.encodeUtf8 t

    mangle s =
      BS.pack (concatMap (\c -> if c == 0 then [0,1] else [c]) $ BS.unpack s)
      <>
      BS.pack [0,0]

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "number" $ TestCase $ assertProperty "mismatch"
      prop_number
  , TestLabel "number_roundtrip" $ TestCase $ assertProperty "mismatch"
      prop_number_roundtrip
  , TestLabel "string_roundtrip" $ TestCase $ assertProperty "mismatch"
      prop_string_roundtrip
  , TestLabel "mangled_string_roundtrip" $ TestCase $ assertProperty "mismatch"
      prop_mangled_string_roundtrip
  ]
