{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module GleanCLI.Derive (DeriveCommand) where

import Control.Concurrent
import Control.Concurrent.Async (async, wait)
import Control.Exception (bracket_, throwIO)
import Control.Monad (forM, unless)
import Data.Foldable (for_)
import Data.Graph (graphFromEdges, vertices)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

import Util.Log.Text
import Util.OptParse
import Util.Text

import Glean
import Glean.Database.Schema
import Glean.Derive (derivePredicate)
import Glean.Schema.Util (parseRef)
import Glean.Types

import GleanCLI.Types
import GleanCLI.Common
import Text.Printf

data DeriveCommand
  = Derive
      { deriveRepo :: Repo
      , predicates :: [(Text, Maybe ParallelDerivation)]
      , derivePageOptions :: PageOptions
      , deriveMaxConcurrency :: Int
      }

instance Plugin DeriveCommand where
  parseCommand =
    commandParser "derive" (progDesc desc) $ do
      deriveRepo <- dbOpts
      deriveMaxConcurrency <- maxConcurrencyOpt
      derivePageOptions <- pageOpts
      predicates <- many (serial <|> parallel)
      return Derive{..}
    where
    desc = "Concurrently derive and store predicates"
    parallel = option parseParallel
      ( long "parallel"
      <> metavar "PREDICATE,OUTER[,SIZE],QUERY"
      <> help "Derive a predicate in parallel"
      )

    parseParallel = maybeReader $ \s ->
      case Text.splitOn "," (Text.pack s) of
        pred : outer : size : rest | Right n <- textToInt size -> Just
          (pred,
             Just ParallelDerivation
               { parallelDerivation_outer_predicate = outer
               , parallelDerivation_inner_query = Text.intercalate "," rest
               , parallelDerivation_min_batch_size = Just (fromIntegral n) })
        pred : outer : rest -> Just
          (pred,
             Just ParallelDerivation
               { parallelDerivation_outer_predicate = outer
               , parallelDerivation_inner_query = Text.intercalate "," rest
               , parallelDerivation_min_batch_size = Nothing })
        _ -> Nothing

    serial = (,Nothing) <$> strArgument
      ( metavar "PREDICATE"
      <> help "predicates to derive"
      )

  runCommand _ _ backend Derive{..}
    | [(pred, parallel)] <- predicates
    = deriveOne pred parallel
    | otherwise
    = mdo
      -- get the schema from the db
      SchemaInfo{..} <- Glean.getSchemaInfo backend deriveRepo $
        GetSchemaInfo (SelectSchema_stored Empty) False
      -- get the typechecked predicates from the schema
      DbSchema{predicatesById} <-
        mkDbSchemaFromSource Nothing Nothing readWriteContent schemaInfo_schema
      -- create the derivations dependency graph
      let (graph, getNode, _) = graphFromEdges (derivationEdges predicatesById)
          derivations =
            [ (predicateRef_name pred, predicateRef_name . fst <$> deps)
            | (_, (pred, _), deps) <- getNode <$> vertices graph
            ]

      concurrencyAvailable <- newQSem deriveMaxConcurrency

      -- set up the parallel evaluation graph
      let
        waitFor predicate = mapM_ wait (ivars Map.!? predicate)
        -- mapping predicates to evaluation asyncs
        ivars = Map.fromList $ zip (fst <$> derivations) asyncs

      -- sanity check: all the requested derivations must be in the graph
      for_ predicates $ \(pred,_) ->
        unless (pred `elem` map fst derivations) $ do
          throwIO $ userError $ printf
            ("abort: %s is not in the schema or is not derived. This is not" ++
              " supported, try deriving one predicate at a time instead.\n")
            pred

      asyncs <- forM derivations $ \(pred, deps) -> async $ do
        -- wait for my dependencies
        mapM_ waitFor deps

        -- if I am one of the requested predicates, derive me
        for_ (lookup pred predicates) $ \parallel -> do
          bracket_
            (waitQSem concurrencyAvailable)
            (signalQSem concurrencyAvailable) $ do
              logInfo $ "Kicking off: " <> pred
              deriveOne pred parallel
              logInfo $ "Done: " <> pred

      -- evaluate the nodes corresponding to the requested predicates
      mapM_ waitFor (fst <$> predicates)
    where
      deriveOne pred parallel =
        derivePredicate backend deriveRepo
          (Just $ fromIntegral $ pageBytes derivePageOptions)
          (fromIntegral <$> pageFacts derivePageOptions)
          (parseRef pred)
          parallel
