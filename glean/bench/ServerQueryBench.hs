{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Run query benchmarks against a spawned or existing server.
--
-- For each benchmark, this script will do 'cfgWarmup' warm-up queries and then
-- between 'cfgMinIter' and 'cfgMaxIter' queries, stopping when the difference
-- between the two *fastest* iterations is bellow 'cfgMaxDiff'. It will then
-- output data in the following format:
--
-- > NAME MIN_TIME ERROR ITERATIONS
--
-- It will prefix NAME with * if the error is above the 'cfgMaxDiff' threshold.
--
-- TODO: The queries shouldn't be hardcoded here and there should be a lot more
-- of them.
--
-- TODO: Better statistical analysis.
--
-- TODO: More knobs.
--
{-# LANGUAGE ApplicativeDo #-}
module ServerQueryBench (main) where

import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Scientific as Scientific
import qualified Data.Text as Text
import Options.Applicative
import System.Exit (die)
import System.IO
import System.IO.Temp
import System.Process
import Text.Printf

import Glean.Init
import Glean.Server.Spawn (withServer)

data Service = Service String | DBRoot FilePath

data Config = Config
  { cfgService :: Service
  , cfgRepo :: String
  , cfgServerBinary :: FilePath
  , cfgGleanBinary :: FilePath
  , cfgStats :: Maybe FilePath
  , cfgWarmup :: Int
  , cfgMaxIter :: Int
  , cfgMinIter :: Int
  , cfgMaxDiff :: Double
  , cfgWhat :: String
  , cfgOnly :: Maybe String
  }

options :: ParserInfo Config
options = info (parser <**> helper) fullDesc
  where
    parser = do
      cfgService <-
        fmap DBRoot (strOption $
          long "db-root"
          <> metavar "PATH"
          <> help "database directory path")
        <|>
        fmap Service (strOption $
          long "service"
          <> metavar "TIER | HOST:PORT"
          <> help "Use existing service instead of starting server")
      cfgRepo <- strOption $
        long "repo"
        <> metavar "NAME/HASH"
      cfgServerBinary <- strOption $
        long "server-binary"
        <> metavar "PATH"
        <> value "glean-server"
        <> help "Glean server binary"
      cfgGleanBinary <- strOption $
        long "glean-binary"
        <> metavar "PATH"
        <> value "glean"
        <> help "Glean binary"
      cfgStats <- optional $ strOption $
        long "stats"
        <> metavar "PATH"
        <> help "Log raw stats to a file ('-' means stdout)"
      cfgWarmup <- option auto $
        long "warmup"
        <> metavar "N"
        <> value 1
        <> help "Number of warm-up iterations"
      cfgMinIter <- option auto $
        long "min-iterations"
        <> metavar "N"
        <> value 3
        <> help "Minimum number of iterations"
      cfgMaxIter <- option auto $
        long "max-iterations"
        <> metavar "N"
        <> value 8
        <> help "Maximum number of iterations"
      cfgMaxDiff <- option auto $
        long "max-difference"
        <> metavar "N"
        <> value 0.02
        <> help "Maximum difference between 2 iterations"
      cfgWhat <- strOption $
        long "what"
        <> metavar "FIELD"
        <> value "execute_time_ns"
        <> help "What parameter to benchmark"
      cfgOnly <- optional $ strOption $
        long "only"
        <> metavar "BENCHMARK"
        <> help "Which benchmark(s) to run"
      return Config{..}

main :: IO ()
main =
  withOptions options $ \cfg@Config{..} ->
  let
    with_server f = case cfgService of
      Service service -> f service
      DBRoot dbroot ->
        withServer
          (proc cfgServerBinary ["--db-root=" ++ dbroot, "+RTS", "-N2", "-A32m", "-RTS"])
          $ \port _ _ _ _ -> f $ "localhost:" ++ show port

    with_stats f = case cfgStats of
      Nothing -> f Nothing
      Just "-" -> f $ Just stdout
      Just path -> withFile path WriteMode $ f . Just

  in
  with_server $ \service ->
  with_stats $ \hstats -> do
  let
    enabled = case cfgOnly of
      Nothing -> const True
      Just str -> (str `isInfixOf`)
  forM_ benchmarks $ \benchmark ->
    when (enabled (benchmarkName benchmark)) $
      runBenchmark cfg service hstats benchmark

runBenchmark :: Config -> String -> Maybe Handle -> Benchmark -> IO ()
runBenchmark Config{..} service hstats Benchmark{..} = do
  start <-
    last <$> mapM iteration ["warmup " ++ show n | n <- [1 .. max 1 cfgWarmup]]
  (val, err, iters) <- go start (1.0/0.0) 1
  printf "%s %d %.4f %d%s\n" benchmarkName val err iters
    (if err > cfgMaxDiff then " *" else "" :: String)
  where
    iteration it = withSystemTempFile "glean-bench" $ \path h -> do
      hClose h
      callProcess cfgGleanBinary $
        [ "--service=" ++ service
        , "query"
        , "--repo=" ++ cfgRepo
        , "-o", "/dev/null"
        , "--stats=" ++ path
        ]
        ++ benchmarkFlags
        ++ [benchmarkQuery]
      s <- BSL.readFile path
      forM_ hstats $ \h -> dump_stats h benchmarkName it s
      xs <- forM (BSL.lines s) $ \line -> case Aeson.decode' line of
        Just (Aeson.Object obj)
          | Just (Aeson.Number m) <- HashMap.lookup (Text.pack cfgWhat) obj
          , Just n <- Scientific.toBoundedInteger m -> return n
        _ -> die $ "couldn't parse output from glean query: " ++ BSL.unpack line
      return $ sum (xs :: [Int])

    go !prev !prev_err !n = do
      this <- iteration $ "iteration " ++ show n
      let err a b = 1 - fromIntegral a / fromIntegral b
          (new, new_err)
            | prev <= this = (prev, min prev_err (err prev this))
            | otherwise = (this, err this prev)
      if (new_err <= cfgMaxDiff && n >= cfgMinIter) || n >= cfgMaxIter
        then return (new, new_err, n)
        else go new new_err (n+1)

    dump_stats h name iter stats = do
      hPutStrLn h $ name ++ " " ++ iter
      BSL.hPutStr h stats
      hPutChar h '\n'

data Benchmark = Benchmark
  { benchmarkName :: String
  , benchmarkQuery :: String
  , benchmarkFlags :: [String]
  }

benchmarks :: [Benchmark]
benchmarks =
  [
    -- simple fetching, with string keys
    Benchmark "src.File (1MB page)"
      "src.File _"
      ["--limit=1000000", "--page-bytes=1000000"]
  , Benchmark "src.File (20MB page)"
      "src.File _"
      ["--limit=1000000", "--page-bytes=20000000"]
    -- simple fetching, with smaller keys
  , Benchmark "hack.ClassDefinition"
      "hack.ClassDefinition _"
      ["--limit=100000", "--page-facts=10000" ]
    -- slightly more complex searching
  , Benchmark "search.hack.SearchByName"
      "search.hack.SearchByName _"
      ["--limit=100000", "--page-facts=10000" ]
  -- Two queries that represent the kind of queries that Glass makes
  , Benchmark "codemarkup.FileEntityInfos"
      "codemarkup.FileEntityInfos { file = \"www/flib/si/sigma/SigmaContext.php\" }"
      []
  , Benchmark "codemarkup.FileEntityXRefInfos"
      "codemarkup.FileEntityXRefInfos { file = \"www/flib/intern/typeahead/InternTypeaheadType.php\" }"
      []
  -- Exercise fetching of nested facts
  , Benchmark "hack.ClassDefinition-recursive"
      "hack.ClassDefinition _"
      ["--limit=10000", "--page-facts=1000", "--recursive" ]
  -- Query that spends most of its time searching and rejecting facts
  , Benchmark "hack.ClassDefinition-search"
      "D where C = hack.ClassDeclaration { name = { name = \"Glean\" }}; D = hack.ClassDefinition { extends_ = { just = C }}"
      []
  ]
