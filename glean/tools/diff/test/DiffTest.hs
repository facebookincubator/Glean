{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module DiffTest (main) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Glean (Repo(..), JsonFactBatch, PredicateRef(..))
import Data.String.Interpolate (i)
import Test.HUnit
import System.IO.Temp (withSystemTempDirectory)

import TestRunner

import Diff (diff, Result(..), DiffOptions(..))
import Schema.Lib (mkBatch, withSchemaFile)
import Glean.Angle.Types (latestAngleVersion)
import Glean.Init (withUnitTest)
import Glean.Database.Config (fileDataStore, Config(..))
import Glean.Database.Types (Env)
import Glean.Database.Test
  ( kickOffTestDB
  , completeTestDB
  , setRoot
  , setSchemaPath
  , withTestEnv
  )
import Glean.Write.JSON (syncWriteJsonBatch)

main :: IO ()
main = withUnitTest $ testRunner diffTest

diffTest :: Test
diffTest = TestList
  [ TestLabel "identical dbs" $ TestCase $ withEnv $ \env -> do
      let content = take 1000 facts
      one <- create env "one" content
      two <- create env "two" content
      r <- diff env opts one two
      assertEqual "result" (Result 1000 0 0) r

  , TestLabel "first one has more facts" $ TestCase $ withEnv $ \env -> do
      one <- create env "one" $ take 1000 facts
      two <- create env "two" $ take 900 facts
      r <- diff env opts one two
      assertEqual "result" (Result 900 0 100) r

  , TestLabel "second one has more facts" $ TestCase $ withEnv $ \env -> do
      one <- create env "one" $ take 900 facts
      two <- create env "two" $ take 1000 facts
      r <- diff env opts one two
      assertEqual "result" (Result 900 100 0) r

  , TestLabel "same facts, different ids" $ TestCase $ withEnv $ \env -> do
      let (before, after) = splitAt 3 $ take 1000 facts
      one <- create env "one" $ before ++ after
      two <- create env "two" $ after ++ before
      r <- diff env opts one two
      assertEqual "result" (Result 1000 0 0) r

  , TestLabel "different facts" $ TestCase $ withEnv $ \env -> do
      let (facts1, facts2) = splitAt 1200 $ take 2400 facts
      one <- create env "one" facts1
      two <- create env "two" facts2
      r <- diff env opts one two
      assertEqual "result" (Result 0 1200 1200) r

  , TestLabel "different inventories" $ TestCase $ do
      withSystemTempDirectory "glean" $ \tmp -> do
      let setStore config = config { cfgDataStore = fileDataStore tmp }
          settings = [setStore]
          facts' = take 1000 facts
          new = (PredicateRef "x.A" 1, bs [i|{ "key": "A" }|])
      one <- withSchema settings schema1 $ \env -> create env "one" facts'
      two <- withSchema settings schema2 $ \env -> create env "two" (new : facts')
      r   <- withSchema settings schema2 $ \env -> diff env opts one two
      assertEqual "result" (Result 1000 1 0) r
  ]
  where
    opts = DiffOptions
        { opt_logAdded = False
        -- use a small batchSize to ensure the parallel machinery is
        -- used despite the small amount of facts in the test dbs.
        , opt_batchSize = 100
        }

    withSchema settings schema act =
      withSchemaFile latestAngleVersion schema $ \root file ->
      withTestEnv
        ([ setRoot root
        , setSchemaPath file ] ++ settings)
        act

    withEnv act = withSchema [] schema1 act

type Fact = (PredicateRef, ByteString)

create :: Env -> Text -> [Fact] -> IO Repo
create env hash content = do
  let repo = Repo "diff" hash
  kickOffTestDB env repo id
  syncWriteJsonBatch env repo (batches content) Nothing
  completeTestDB env repo
  return repo
  where
    batches :: [Fact] -> [JsonFactBatch]
    batches =
      map (uncurry mkBatch)
      . Map.toList
      . Map.fromListWith (++)
      . map (fmap pure)

schema1 :: String
schema1 =
  [i| schema x.1 {
        predicate P : nat
        predicate Q : { a : P, b : P }
      }
      schema all.1 : x.1 {}
  |]

schema2 :: String
schema2 =
  [i| schema x.1 {
        predicate A : string
        predicate P : nat
        predicate Q : { a : P, b : P }
      }
      schema all.1 : x.1 {}
  |]

-- Infinite stream of facts. Groups of 3 reference each-other.
facts :: [Fact]
facts = concat
  [ [ (PredicateRef "x.P" 1, bs [i|{ "id": #{one}, "key": #{one} }|])
    , (PredicateRef "x.P" 1, bs [i|{ "id": #{two}, "key": #{two} }|])
    , (PredicateRef "x.Q" 1, bs [i|{ "key" : { "a": #{one}, "b": #{two} } }|]) ]
  | n <- [1 :: Int,3..]
  , let one = show n
        two = show (n + 1)
  ]
bs :: String -> ByteString
bs = Text.encodeUtf8 . Text.pack
