{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module UriTest (main) where

import Test.HUnit
import TestRunner

import Data.Text
import Network.URI

import Glean.Init (withUnitTest)
import Glean.Util.URI

testURI :: String
testURI =
  "https://phabricator.internmc.facebook.com/diffusion/FBS/browse/master/fbcode/glean/hs/Glean/Util/Test/UriTest.hs"

testFile :: Text
testFile = "fbcode/glean/hs/Glean/Util/Test/UriTest.hs"

fbsUriTest :: Test
fbsUriTest =
  let tc = TestCase $ do
        let uri = fbsDiffusionURI testFile Nothing
        let expectedUri = parseURI testURI
        assertEqual "FBS URI building" (Just uri) expectedUri
  in TestLabel "fbsUriTest" tc

fbsUriLineTest :: Test
fbsUriLineTest =
  let tc = TestCase $ do
        let uri = fbsDiffusionURI testFile $ Just 8
        let expectedUri = parseURI $ testURI ++ "$8"
        assertEqual "FBS URI building" (Just uri) expectedUri
  in TestLabel "fbsUriLineTest" tc


tests :: Test
tests = TestList [ fbsUriTest, fbsUriLineTest ]

main :: IO ()
main = withUnitTest $ do
  testRunner tests
