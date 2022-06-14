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

import Glean.Types as Thrift
import Glean.Database.Schema
import Glean.RTS as RTS
import Glean.Backend (Backend(..), loadDbSchema, factIdRange)
import Glean.Query.JSON

dump :: Backend b => b -> Repo -> (JsonFactBatch -> IO ()) -> IO ()
dump backend repo withBatch = doDump =<< loadDbSchema backend repo
  where
    doDump dbSchema = do
      Thrift.FactIdRange{..} <- factIdRange backend repo
      go Thrift.iNVALID_ID [] 0 factIdRange_start factIdRange_finish
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

      go :: Thrift.Id -> [(Id,Fact)] -> Int64 -> Id -> Id ->  IO ()
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
