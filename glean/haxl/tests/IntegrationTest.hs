{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module IntegrationTest where

import Data.List
import Data.Maybe
import Test.HUnit

import Haxl.Core
import Haxl.DataSource.Glean as Glean
import Haxl.Testing.Skip
import TestRunner

import Glean hiding (Predicate, runHaxl, query)
import Glean.Angle
import Glean.Init
import Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Schema.Sys.Types as Sys
import TestDB

testEnv :: Backend b => b -> Repo -> IO (Env Repo ())
testEnv backend repo = do
  (state1,stage2) <- initGlobalState backend
  let st = stateSet state1 $ stateSet stage2 stateEmpty
  initEnv st repo

-- Actual tests ------

getTest :: Backend b => b -> Repo -> Test
getTest env repo = TestLabel "GetRec" $
  TestCase $ do
    e <- testEnv env repo

    rs <- runHaxl e $ Glean.search_ $ recursive $ query $
      predicate @Glean.Test.Predicate $
        rec $ field @"pred" (byteArray "hello") end
    assertBool "Glean.query 1" $ (==2) $ length
      [ r | r@Glean.Test.Predicate { predicate_key = Just
              Glean.Test.KitchenSink { kitchenSink_pred =
                Sys.Blob { blob_key = Just "hello" }}} <- rs ]

    (rs, trunc) <- runHaxl e $ Glean.search $ limit 2 $ query $
      predicate @Glean.Test.Predicate wild
    assertBool "Glean.query 1 limit" (length rs == 2 && trunc)

    rs <- runHaxl e $ Glean.search_ $ recursive $ query $
      predicate @Glean.Test.Predicate wild
    assertBool "Glean.query 2" (length rs == 4)
    let
      stubs :: [Glean.Test.Predicate]
      stubs = map (justId . getId) rs

    rs <- runHaxl e $ mapM Glean.get stubs
    assertBool "Glean.get" $
      length (filter (isJust . Glean.Test.predicate_key) rs) == 4

    rs <- runHaxl e $ mapM Glean.getRec stubs
    assertBool "Glean.getRec" (length rs == 4)

    rs <- runHaxl e $ mapM Glean.getKey stubs
    assertBool "Glean.getKey" $
      sort (map (fromNat . Glean.Test.kitchenSink_nat) rs) == [ 0, 0, 0, 42 ] &&
      all (isNothing . Sys.blob_key . Glean.Test.kitchenSink_pred) rs

    rs <- runHaxl e $ mapM Glean.getKeyRec stubs
    assertBool "Glean.getKeyRec" $
      let keyOf = Sys.blob_key . Glean.Test.kitchenSink_pred in
      sort (map (fromMaybe "" . keyOf) rs) == ["bye", "bye", "hello", "hello"]

    -- test streaming
    rs <- runHaxl e $ Glean.search_ $ recursive $ limit 1 $ query $
      predicate @Glean.Test.Predicate wild
    assertBool "Glean.query stream" (length rs == 4)


main :: IO ()
main =
  withUnitTest $
  withTestDB [] $ \env repo -> do
  testRunner $ mkSkipOnTransientTest $ TestList
    [ getTest env repo
    ]
