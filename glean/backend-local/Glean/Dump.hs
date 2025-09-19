{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Dump
  ( dump
  ) where

import Control.Exception
import Control.Monad
import Data.Int (Int64)
import Text.Printf

import Util.Control.Exception (catchAll)
import Util.Log

import Glean.Backend.Types
import Glean.Backend.Local
import Glean.Database.Schema
import Glean.Query.JSON
import Glean.RTS.Types
import Glean.Types

dump :: Backend b => b -> Repo -> (JsonFactBatch -> IO ()) -> IO ()
dump backend repo withBatch = doDump =<< loadDbSchema backend repo
  where
    doDump dbSchema = do
      FactIdRange{..} <- factIdRange backend repo
      go iNVALID_ID [] 0 factIdRange_start factIdRange_finish
      where

      doneBatch :: Id -> [(Id,Fact)] -> IO ()
      doneBatch _ [] = return ()
      doneBatch pid facts = do
        vlog 1 (printf "%d facts for predicate %d\n" (length facts) pid)
        case lookupPid (Pid pid) dbSchema of
          Nothing -> throwIO $ ErrorCall $ "unknown predicate Id: " <> show pid
          Just details@PredicateDetails{} -> do
            jsonFacts <- forM (reverse facts) $ \(fid, Fact{..}) ->
                factToJSON
                  True
                  mempty
                  details
                  (Fid fid)
                  fact_key
                  fact_value
                `catchAll` \exc -> throwIO $ ErrorCall $ show exc
            withBatch JsonFactBatch
              { jsonFactBatch_predicate = predicateRef details
              , jsonFactBatch_facts = jsonFacts
              , jsonFactBatch_unit = Nothing -- TODO?
              }

      -- limit the size of batches we give back, so that the writer
      -- can run in constant space.
      maxBatchSize :: Id
      maxBatchSize = 10000

      go :: Id -> [(Id,Fact)] -> Int64 -> Id -> Id ->  IO ()
      go !currentPid facts !batchSize !nextId !finalId
        | nextId < finalId = do
            r <- queryFact backend repo nextId
            case r of
              Nothing -> go currentPid facts batchSize (nextId+1) finalId
              Just fact@Fact{..} ->
                if fact_type == currentPid && batchSize < maxBatchSize
                  then go
                    currentPid
                    ((nextId,fact):facts)
                    (batchSize + 1)
                    (nextId+1)
                    finalId
                  else do
                    doneBatch currentPid facts
                    go fact_type [(nextId,fact)] 1 (nextId+1) finalId
        | otherwise = doneBatch currentPid facts
