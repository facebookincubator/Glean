{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

This file implements a set of benchmarks for FactSets which can use real-world
data. It copies all facts from a DB (selected as usual via `--db-root` and
`--db`) and then runs the benchmarks on FactSets initialised with these facts.

For best results, the DB shouldn't be too small but also shouldn't be huge
(since all facts will be stored in RAM multiple times).

For comparison, it can also run the same benchmarks on the DB it obtained the
facts from.

Run each benchmark once:
  factset-bench --db-root=... --db=... --schema=dir

Run only FactSet (but not DB) benchmarks:
  factset-bench ... --only-factset

Run specific benchmark 6 times:
  factset-bench ... --repeat=6 redefine

Run specific benchmark forever (useful for `perf` etc.):
  factset-bench ... --forever redefine

-}

{-# LANGUAGE ApplicativeDo #-}
module FactSetBench (main) where

import Control.Monad
import qualified Data.Vector.Storable as V
import Options.Applicative
import System.Exit (die)

import Glean (Repo, parseRepo)
import qualified Glean.Database.Config as Database (Config, options)
import Glean.Database.Env (withDatabases)
import Glean.Database.Open (readDatabase)
import Glean.Impl.ConfigProvider
import qualified Glean.RTS.Foreign.Benchmarking as B
import qualified Glean.RTS.Foreign.FactSet as FactSet
import qualified Glean.RTS.Foreign.Lookup as Lookup

import Glean.Util.ConfigProvider
import Util.EventBase (withEventBaseDataplane)
import Util.Timing

data Config = Config
  { configOptions :: Database.Config
  , configRepo :: Repo
  , configBenchmarks :: [[Benchmark]]
  , configRunner :: Runner
  , configOnlyFactSet :: Bool
  }

data Ctx = Ctx
  { ctxReport :: String -> IO ()
  , ctxFail :: IO ()
  , ctxDB :: Lookup.Lookup
  , ctxFactBlock :: B.FactBlock
  , ctxFactSet :: FactSet.FactSet
  , ctxRunner :: Runner
  }

type Runner = (String -> IO ()) -> IO Double -> IO ()

runOnce :: Runner
runOnce report action = do
  t <- action
  report $ showTime t

runRepeat :: Int -> Runner
runRepeat n report action = when (n /= 0) $ do
  ts <- replicateM n action
  let min_time = minimum ts
      max_time = maximum ts
      avg_time = sum ts / fromIntegral n
  report $ unwords
    [ "min:", showTime min_time
    , "avg:", showTime avg_time
    , "max:", showTime max_time
    ]

runForever :: Runner
runForever _ = forever

time :: (Ctx -> IO Bool) -> Ctx -> IO ()
time action ctx = ctxRunner ctx (ctxReport ctx) $ do
  (time, _, ok) <- timeIt $ action ctx
  when (not ok) $ ctxFail ctx
  return time

data Benchmark = Benchmark
  { bName :: String
  , bEnabled :: Config -> Bool
  , bRun :: Ctx -> IO ()
  }

bench :: String -> (Ctx -> IO ()) -> [Benchmark]
bench name f = [Benchmark name (const True) f]

benchBoth
  :: String
  -> (forall l. Lookup.CanLookup l => l -> Ctx -> IO ())
  -> [Benchmark]
benchBoth name f =
  [ Benchmark (name ++ "-db") (not . configOnlyFactSet) $ \ctx -> f (ctxDB ctx) ctx
  , Benchmark name (const True) $ \ctx -> f (ctxFactSet ctx) ctx ]

lookupB
  :: Lookup.CanLookup l => (l -> B.FactBlock -> IO Bool) -> l -> Ctx -> IO ()
lookupB f l = time $ f l <$> ctxFactBlock

benchmarks :: [Benchmark]
benchmarks = concat
  [ bench "define" $ \ctx@Ctx{..} -> do
      id <- Lookup.startingId ctxDB
      time (const $  do
        factset <- FactSet.new id
        B.defineEach factset ctxFactBlock) ctx
  
  , bench "redefine" $ time $ B.defineEach <$> ctxFactSet <*> ctxFactBlock

  , benchBoth "type" $ lookupB B.lookupEachType
  , benchBoth "byId" $ lookupB B.lookupEachById
  , benchBoth "byKey" $ lookupB B.lookupEachByKey
  , benchBoth "seekToEach" $ lookupB B.seekToEach
  -- The first seekToEach initialised things in the FactSet so lets' measure
  -- again
  , benchBoth "seekToEach2" $ lookupB B.seekToEach
  , benchBoth "seekAll" $ \l ctx@Ctx{..} -> do
      count <- FactSet.factCount ctxFactSet
      pids <- V.fromList . map fst <$> FactSet.predicateStats ctxFactSet
      time (const $ (count == ) <$> B.seekCount l pids) ctx
  ]

options :: ParserInfo Config
options = info (parser <**> helper)
  (fullDesc <> progDesc "Generate a random batch of facts")
  where
    parser = do
      configOptions <- Database.options
      configRepo <- option (maybeReader Glean.parseRepo)
        (  long "db"
        <> metavar "NAME/INSTANCE"
        <> help "identifies the database"
        )
      configBenchmarks <- many $ argument
        (maybeReader $ \s -> case filter (\b -> bName b == s) benchmarks of
          [] -> Nothing
          xs -> Just xs)
        ( metavar "BENCHMARK"
        <> help "selects benchmark"
        )
      configRunner <-
        fmap runRepeat (option auto
          (  long "repeat"
          <> metavar "N"
          <> help "repeat each benchmark N times" ))
        <|> flag' runForever (long "forever")
        <|> pure runOnce
      configOnlyFactSet <- switch
        ( long "only-factset"
        <> help "skip DB benchmarks"
        )
      return Config{..}

main :: IO ()
main =
  withConfigOptions options $ \(config@Config{..}, cfg) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfg $ \(cfgAPI :: ConfigAPI) ->
  withDatabases evb configOptions cfgAPI $ \env ->
  readDatabase env configRepo $ \_ db -> do
    block <- B.createFactBlock db
    startingId <- Lookup.startingId db
    factset <- FactSet.new startingId
    ok <- B.defineEach factset block
    when (not ok) $ die "FactSet creation failed"

    block_count <- B.factCount block
    block_memory <- B.factMemory block
    putStrLn $ unwords
      [ "FactBlock facts:", show block_count
      , "factmem:", showAllocs $ fromIntegral block_memory
      ]
    count <- FactSet.factCount factset
    mem <- FactSet.allocatedMemory factset
    factmem <- FactSet.factMemory factset
    putStrLn $ unwords
      [ "FactSet facts:", show count
      , "mem:", showAllocs $ fromIntegral mem
      , "factmem:", showAllocs $ fromIntegral factmem
      ]

    let bs = case configBenchmarks of
          [] -> benchmarks
          _ -> concat configBenchmarks

        width = maximum $ map (length . bName) bs

    forM_ bs $ \Benchmark{..} -> when (bEnabled config) $ bRun Ctx
      { ctxReport = \s ->
          putStrLn $ bName ++ replicate (width - length bName + 1) ' ' ++ s
      , ctxFail = die $ bName ++ " failed"
      , ctxDB = db
      , ctxFactBlock = block
      , ctxFactSet = factset
      , ctxRunner = configRunner
      }
