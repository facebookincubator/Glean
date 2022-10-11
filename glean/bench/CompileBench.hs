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
import Data.Text (Text)
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Vector.Unboxed as V
import Statistics.Sample (mean)
import System.Mem
import System.IO.Temp
import System.FilePath ((</>))

import Util.String.Quasi

import Glean
import Glean.Angle.Types (latestAngleVersion)
import Glean.Database.Open
import Glean.Database.Test
import Glean.Database.Types
import Glean.Query.Thrift.Internal
import qualified Glean.Schema.CodeCxx.Types as Code.Cxx
import qualified Glean.Schema.Codemarkup.Types as Codemarkup
import Glean.Types
import Glean.Write.JSON (syncWriteJsonBatch)

data Cfg = Cfg
  { cfgIters :: Int
  , cfgStop :: Double
  }

type Description = String

main :: IO ()
main = do
  benchLargeDerivations
  benchTransformations

benchTransformations :: IO ()
benchTransformations =
  withSchemaFile latestAngleVersion schema $ \root file -> do
  let settings = [setRoot root, setSchemaPath file]
  repo <- withEmptyTestDB settings $ \env repo -> do
    void $ syncWriteJsonBatch env repo facts Nothing
    completeTestDB env repo
    return repo
  -- Open db for querying. We need to open the db again because schema
  -- evolutions and pruning are only triggered when the db is read-only.
  withTestEnv settings $ \env -> do
  bench env repo queries
  where
    queries =
      [ ("base predicate - no transformation"
        , getq $ angleData @Text $
          [s|
            S where
              # bind lhs                    # bind rhs
              X1 = x.P.2 {_ , _, _, _, _ }; X1 = x.P.2 {S1, _, _, _, _ };
              X2 = x.P.2 {S1, _, _, _, _ }; X2 = x.P.2 {S2, _, _, _, _ };
              X3 = x.P.2 {S2, _, _, _, _ }; X3 = x.P.2 {S3, _, _, _, _ };
              X4 = x.P.2 {S3, _, _, _, _ }; X4 = x.P.2 {S4, _, _, _, _ };
              X5 = x.P.2 {S4, _, _, _, _ }; X5 = x.P.2 {S5, _, _, _, _ };
              X6 = x.P.2 {S5, _, _, _, _ }; X6 = x.P.2 {S , _, _, _, _ };
          |]
        )
      , ("base predicate - with transformation"
        , getq $ angleData @Text $
          [s|
            S where
              # bind lhs          # bind rhs
              X1 = x.P.1 {_, _ }; X1 = x.P.1 {_, S1};
              X2 = x.P.1 {_, S1}; X2 = x.P.1 {_, S2};
              X3 = x.P.1 {_, S2}; X3 = x.P.1 {_, S3};
              X4 = x.P.1 {_, S3}; X4 = x.P.1 {_, S4};
              X5 = x.P.1 {_, S4}; X5 = x.P.1 {_, S5};
              X6 = x.P.1 {_, S5}; X6 = x.P.1 {_, S };
          |]
        )
      , ("derived predicate - no transformation"
        , getq $ angleData @Text $
          [s|
            S where x.Q.2 S =
              [ x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo"
              , x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo"
              , x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo"
              , x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo", x.Q.2 "foo"
              ][..];
          |]
        )
      , ("derived predicate - with transformation"
        , getq $ angleData @Text $
          [s|
            S where x.Q.1 S =
              [ x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo"
              , x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo"
              , x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo"
              , x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo", x.Q.1 "foo"
              ][..];
          |]
        )
      ]
    schema =
      [s|
        schema x.1 {
          predicate P : { x : nat, y : string }
          predicate Q : string
            S where
              # bind lhs      # bind rhs
              X1 = P {_, _ }; X1 = P {_, S1};
              X2 = P {_, S1}; X2 = P {_, S2};
              X3 = P {_, S2}; X3 = P {_, S3};
              X4 = P {_, S3}; X4 = P {_, S4};
              X5 = P {_, S4}; X5 = P {_, S5};
              X6 = P {_, S5}; X6 = P {_, S };
        }
        schema x.2 : x.1 {
          predicate P :
            { y : string
            , x : nat
            , a: maybe bool
            , b: maybe string
            , c: maybe string
            }

          predicate Q : string
            S where
              # bind lhs                # bind rhs
              X1 = P {_ , _, _, _, _ }; X1 = P {S1, _, _, _, _ };
              X2 = P {S1, _, _, _, _ }; X2 = P {S2, _, _, _, _ };
              X3 = P {S2, _, _, _, _ }; X3 = P {S3, _, _, _, _ };
              X4 = P {S3, _, _, _, _ }; X4 = P {S4, _, _, _, _ };
              X5 = P {S4, _, _, _, _ }; X5 = P {S5, _, _, _, _ };
              X6 = P {S5, _, _, _, _ }; X6 = P {S , _, _, _, _ };
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]

    facts =
      [ mkBatch (PredicateRef "x.P" 2)
        [ [s|{ "key": { "x" : 1, "y": "A", "a": true, "b": "", "c": "" } }|] ]
      ]

    mkBatch ref facts = JsonFactBatch
      { jsonFactBatch_predicate = ref
      , jsonFactBatch_facts = facts
      , jsonFactBatch_unit = Nothing
      }

    withSchemaFile :: Int -> String -> (FilePath -> FilePath -> IO a) -> IO a
    withSchemaFile version str action = do
      withSystemTempDirectory "glean-dbtest" $ \root -> do
        let newSchemaFile = root </> "schema"
        appendFile newSchemaFile $ "version: " <> show version
        appendFile newSchemaFile str
        action root newSchemaFile

benchLargeDerivations :: IO ()
benchLargeDerivations =
  withEmptyTestDB [] $ \env repo -> do
  -- NB.Pruning and evolutions do not take place because the
  -- tests are run on a writable db.
  withOpenDatabase env repo $ void . return
  bench env repo queries
  where
    queries =
      [ ("code.cxx.SearchByNameAndScope"
        , getq $ angleData @Code.Cxx.Entity $
          "E where search.cxx.SearchByNameAndScope { \"malloc\", global_, E }"
        )
      , ("codemarkup.EntityUses"
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
        )
      , ("FileEntityXRefLocations"
        , getq $ angleData @Codemarkup.FileEntityXRefLocations $
          [s|
            codemarkup.FileEntityXRefLocations { file = "foo" }
          |]
        )
      ]

getq :: Query a -> UserQuery
getq (Query q) = q

bench :: Env -> Repo -> [(Description, UserQuery)] -> IO ()
bench env repo queries =
  forM_ queries $ \(description,q) -> do
    xs <- measure (query env repo q) Cfg{cfgIters = 10, cfgStop = 1}
    let (compile, codegen) = V.unzip xs
    putStrLn $ " + " <> description
    putStrLn $ line "compile" compile
    putStrLn $ line "codegen" codegen
  where
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

    line :: String -> V.Vector Double -> String
    line h xs = replicate 6 ' ' ++ h ++ " " ++ display xs
      where

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
