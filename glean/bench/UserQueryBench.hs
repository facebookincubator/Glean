-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}

{- | This is mostly intended to measure the performance of serialisation but
 - also (necessarily) includes collecting nested facts. It is *not* a benchmark
 - for actual query performance.
 -}

module UserQueryBench (main) where

import Control.Monad
import Criterion.Types
import Data.Default
import Data.Maybe
import Data.Proxy
import qualified Data.Text as Text

import Glean
import Glean.Database.Stuff
import Glean.Database.Test
import Glean.Database.Types (Env)
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Src.Types as Src
import Glean.Types
import Glean.Util.Benchmark

import TestBatch

data Q = Q
  { qEncoding :: Bool -> Maybe UserQueryEncoding
  , qTag :: String
  }

compactQ :: Q
compactQ = Q
  { qEncoding = \expand -> Just $ UserQueryEncoding_compact def
      { userQueryEncodingCompact_expand_results = expand }
  , qTag = "compact"
  }

jsonQ :: Q
jsonQ = Q
  { qEncoding = \expand -> Just $ UserQueryEncoding_json def
      { userQueryEncodingJSON_expand_results = expand }
  , qTag = "json"
  }

binQ :: Q
binQ = Q
  { qEncoding = \expand ->
      if expand
        then Nothing
        else Just $ UserQueryEncoding_bin def
  , qTag = "bin"
  }

qbench :: [Q] -> Env -> Repo -> PredicateRef -> [Benchmark]
qbench qs env repo (PredicateRef name ver) =
  catMaybes
    [ bench (bname qTag <> sfx) . whnfIO . exec recursive <$> qEncoding expanded
      | (sfx, recursive, expanded)
          <- [ ("", False, False)
              , ("-recursive", True, False)
              , ("-expanded", True, True) ]
      , Q{..} <- qs ]
  where
    exec recursive enc = userQuery env repo def
      { userQuery_predicate = name
      , userQuery_predicate_version = Just ver
      , userQuery_query = "{ \"get\" : {} }"
      , userQuery_options = Just def { userQueryOptions_recursive = recursive }
      , userQuery_encodings = [enc]
      }

    bname tag = Text.unpack name <> "-" <> tag

factbench :: [Q] -> Env -> Repo -> ([Fid], PredicateRef) -> [Benchmark]
factbench qs env repo (ids, ref) = do
  let
    exec recursive enc = userQueryFacts env repo def
      { userQueryFacts_facts = [ def { factQuery_id = id } | Fid id <- ids ]
      , userQueryFacts_options =
        Just def { userQueryOptions_recursive = recursive }
      , userQueryFacts_encodings = [enc]
      }

    bname tag = Text.unpack (predicateRef_name ref) <> "-" <> tag

  catMaybes
    [ bench (bname qTag <> sfx) . whnfIO . exec recursive <$>
        qEncoding expanded
      | (sfx, recursive, expanded)
          <- [ ("", False, False)
              , ("-recursive", True, False)
              , ("-expanded", True, True) ]
      , Q{..} <- qs ]


ref :: forall p. Predicate p => PredicateRef
ref = getName (Proxy :: Proxy p)

qbenchmarks :: Env -> Repo -> [PredicateRef] -> [Benchmark]
qbenchmarks env repo = concatMap $ qbench [binQ, jsonQ, compactQ] env repo

factbenchmarks :: Env -> Repo -> [([Fid], PredicateRef)] -> [Benchmark]
factbenchmarks env repo = concatMap (factbench [binQ, jsonQ, compactQ] env repo)

main :: IO ()
main = benchmarkMain $ \run -> withEmptyTestDB [] $ \env repo -> do
  withOpenDatabase env repo $ \odb ->
    void $ return odb

  batch <- testBatch 100000 env repo

  void $ syncWriteDatabase env repo batch

  let
    facts1000 :: forall p . Predicate p => IO [Fid]
    facts1000 =
      fmap (map (idOf . getId) . fst) $
      runQuery env repo $
      limit 1000 $
      allFacts @p

  file_ids <- facts1000 @Src.File
  name_ids <- facts1000 @Cxx.Name
  fname_ids <- facts1000 @Cxx.FunctionName
  xref_ids <- facts1000 @Cxx.FileXRefMap

  run
    [ bgroup "query" $
      qbenchmarks env repo
        [ ref @Src.File
        , ref @Cxx.Name
        , ref @Cxx.FunctionName
        , ref @Cxx.FileXRefMap
        ]
    , bgroup "facts1" $
      factbenchmarks env repo
        [ (take 1 file_ids, ref @Src.File)
        , (take 1 name_ids, ref @Cxx.Name)
        , (take 1 fname_ids, ref @Cxx.FunctionName)
        , (take 1 xref_ids, ref @Cxx.FileXRefMap)
        ]
    , bgroup "facts1000" $
      factbenchmarks env repo
        [ (file_ids, ref @Src.File)
        , (name_ids, ref @Cxx.Name)
        , (fname_ids, ref @Cxx.FunctionName)
        , (xref_ids, ref @Cxx.FileXRefMap)
        ]
    ]
