{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes, TypeApplications #-}

module CompileBench (main) where

import Control.Monad
import Criterion.Measurement (secs)
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Vector.Unboxed as V
import Statistics.Sample (mean)
import System.Mem

import Util.String.Quasi

import Glean
import Glean.Database.Open
import Glean.Database.Test
import Glean.Query.Thrift.Internal
import qualified Glean.Schema.CodeCxx.Types as Code.Cxx
import qualified Glean.Schema.Codemarkup.Types as Codemarkup
import Glean.Types

data Cfg = Cfg
  { cfgIters :: Int
  , cfgStop :: Double
  }

main :: IO ()
main = withEmptyTestDB [] $ \env repo -> do
  withOpenDatabase env repo $ void . return
  forM_ (zip [1..] queries) $ \(i,q) -> do
    xs <- measure (query env repo q) Cfg{cfgIters = 10, cfgStop = 1}
    let (compile, codegen) = V.unzip xs
    putStrLn $ line i "compile" compile
    putStrLn $ line i "codegen" codegen
  where
    getq (Query q) = q

    queries =
      [ getq $ angleData @Code.Cxx.Entity $
          "E where search.cxx.SearchByNameAndScope { \"malloc\", global_, E }"
      , getq $ angleData @Codemarkup.EntityUses $
          [s|
              codemarkup.ResolveLocation {
              entity = E,
              location = {
                  location = { span = { length = 6, start = 1308 } },
                  file = "foo"
              }
              };
              codemarkup.EntityUses { target = E }
          |]
      , getq $ angleData @Codemarkup.FileEntityXRefLocations $
          [s|
            codemarkup.FileEntityXRefLocations { file = "foo" }
          |]
      ]

    measure :: IO (Double,Double) -> Cfg -> IO (V.Vector (Double,Double))
    measure action = V.unfoldrM $ \Cfg{..} ->
      if cfgStop <= 0 && cfgIters <= 0
        then return Nothing
        else do
          performMinorGC
          (compile, codegen) <- action
          return $ Just ((compile, codegen), Cfg
            { cfgIters = max 0 $ cfgIters - 1
            , cfgStop = if compile >= cfgStop then 0 else cfgStop - compile
            })

    query env repo q = do
      UserQueryResults
        { userQueryResults_stats = Just UserQueryStats
            { userQueryStats_compile_time_ns = Just compile
            , userQueryStats_codegen_time_ns = Just codegen }
        } <- userQuery env repo q
      return (fromNS compile, fromNS codegen)
      where
        fromNS n = fromIntegral n / 1000000000

    line :: Int -> String -> V.Vector Double -> String
    line i h xs =
      s ++ replicate (6 - length s) ' ' ++ h ++ " " ++ display xs
      where
        s = show i

    display :: V.Vector Double -> String
    display xs = unwords
      [field "min" $ secs $ V.head ts
      ,field "p50" $ secs $ ts V.! (V.length ts `div` 2 + 1)
      ,field "avg" $ secs $ mean ts
      ,field "max" $ secs $ V.last ts
      ,field "its" $ show $ V.length xs
      ]
      where
        ts = V.modify V.sort xs
        field s t = s ++ ": " ++ t
