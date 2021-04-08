module Glean.RTS.Foreign.Query
  ( CompiledQuery(..)
  , executeCompiled
  , restartCompiled
  , interruptRunningQueries
  , QueryRuntimeOptions(..)
  , Depth(..)
  , QueryResults(..)
  ) where

#include "glean/rts/ffi.h"
#include "glean/rts/query.h"

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Int
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Foreign.C
import Foreign hiding (with)

import Foreign.CPP.HsStruct.Types

import Glean.FFI
import Glean.RTS.Foreign.Bytecode
import Glean.RTS.Foreign.Define
import Glean.RTS.Foreign.Inventory
import Glean.RTS.Traverse
import Glean.RTS.Types (Fid(..), Pid(..))
import Glean.Types as Thrift

newtype Results = Results (Ptr Results)
  deriving(Storable)

instance Static Results where
  destroyStatic = glean_free_query_results

-- | Specifies the depth to which we expand nested facts in the results
data Depth
  = ResultsOnly
    -- ^ Don't expand any nested facts in the results
  | ExpandRecursive
    -- ^ Expand all nested facts recursively
  | ExpandPartial (Set Pid)
    -- ^ Only expand nested facts with these Pids

data QueryRuntimeOptions = QueryRuntimeOptions
  { queryMaxResults :: Maybe Int64
  , queryMaxBytes :: Maybe Int64
  , queryMaxTimeMs :: Maybe Int64
  , queryDepth :: Depth
  , queryWantStats :: Bool
  }

data QueryResults = QueryResults
  { queryResultsFacts :: Vector (Fid, Thrift.Fact)
  , queryResultsNestedFacts :: Vector (Fid, Thrift.Fact)
  , queryResultsStats :: Maybe (Map Int64 Int64)
  , queryResultsElapsedNs :: Word64
  , queryResultsCont :: Maybe ByteString
  }

data CompiledQuery = CompiledQuery
  { compiledQuerySub :: Subroutine CompiledQuery
  , compiledQueryResultPid :: Maybe Pid
    -- ^ If all the result facts have the same Pid, this is it.
  , compiledQueryResultTraversal :: Maybe (Subroutine CompiledTraversal)
    -- ^ The query engine needs to know how to traverse the query
    -- results to expand nested facts. If the result is not an
    -- existing predicate type then we have to pass in a bespoke
    -- CompiledTraversal subroutine.
  }

executeCompiled
  :: CanDefine a
  => Inventory
  -> a
  -> CompiledQuery
  -> QueryRuntimeOptions
  -> IO QueryResults
executeCompiled inventory facts CompiledQuery{..} QueryRuntimeOptions{..} =
  withDefine facts $ \facts_ptr ->
  with inventory $ \inventory_ptr ->
  with compiledQuerySub $ \sub_ptr ->
  let
    maxr = fromIntegral (fromMaybe 0 queryMaxResults)
    maxb = fromIntegral (fromMaybe 0 queryMaxBytes)
    maxt = fromIntegral (fromMaybe 0 queryMaxTimeMs)
    withTraversal = case compiledQueryResultTraversal of
       Nothing -> ($ nullPtr)
       Just sub -> with sub
  in
  withTraversal $ \traversal_ptr ->
  withDepth queryDepth $ \(depth, expand_pids, num_expand_pids) ->
  using
    (invoke $ \presults -> glean_query_execute_compiled
      inventory_ptr
      facts_ptr
      sub_ptr
      (maybe 0 (fromIntegral . fromPid) compiledQueryResultPid)
      traversal_ptr
      maxr
      maxb
      maxt
      depth
      expand_pids
      num_expand_pids
      (if queryWantStats then 1 else 0)
      presults)
    (unpackResults queryWantStats compiledQueryResultPid)

restartCompiled
  :: CanDefine a
  => Inventory
  -> a
  -> Maybe Pid
  -> QueryRuntimeOptions
  -> ByteString   -- serialized thrift::internal::QueryCont
  -> IO QueryResults
restartCompiled inventory facts pid QueryRuntimeOptions{..} serializedCont =
  withDefine facts $ \facts_ptr ->
  with inventory $ \inventory_ptr ->
  unsafeWithBytes serializedCont $ \cont_ptr cont_size ->
  let
    maxr = fromIntegral (fromMaybe 0 queryMaxResults)
    maxb = fromIntegral (fromMaybe 0 queryMaxBytes)
    maxt = fromIntegral (fromMaybe 0 queryMaxTimeMs)
  in
  withDepth queryDepth $ \(depth, expand_pids, num_expand_pids) ->
  using
    (invoke $ \presults -> glean_query_restart_compiled
      inventory_ptr
      facts_ptr
      cont_ptr
      cont_size
      maxr
      maxb
      maxt
      depth
      expand_pids
      num_expand_pids
      (if queryWantStats then 1 else 0)
      presults)
    (unpackResults queryWantStats pid)

withDepth :: Depth -> ((Word64, Ptr Word64, Word64) -> IO a) -> IO a
withDepth depth f = case depth of
  ResultsOnly -> f (depth_ResultsOnly, nullPtr, 0)
  ExpandRecursive -> f (depth_ExpandRecursive, nullPtr, 0)
  ExpandPartial pids -> do
    let list = [ fromIntegral (fromPid p) | p <- Set.toList pids ]
    withArrayLen list $ \len p ->
      f (depth_ExpandPartial, p, fromIntegral len)

unpackResults :: Bool -> Maybe Pid -> Results -> IO QueryResults
unpackResults wantStats maybePid (Results p) = do
  fact_ids <- (# peek facebook::glean::rts::QueryResults, fact_ids) p
  -- don't marshal the pids if we know what they are
  fact_pids <-
    if isNothing maybePid
      then (# peek facebook::glean::rts::QueryResults, fact_pids) p
      else return (HsArray Vector.empty)
  fact_keys <- (# peek facebook::glean::rts::QueryResults, fact_keys) p
  fact_values <- (# peek facebook::glean::rts::QueryResults, fact_values) p
  nested_fact_ids <-
    (# peek facebook::glean::rts::QueryResults, nested_fact_ids) p
  nested_fact_pids <-
    (# peek facebook::glean::rts::QueryResults, nested_fact_pids) p
  nested_fact_keys <-
    (# peek facebook::glean::rts::QueryResults, nested_fact_keys) p
  nested_fact_values <-
    (# peek facebook::glean::rts::QueryResults, nested_fact_values) p

  let
    mkResultFact pid id key value =
      (Fid (fromIntegral (id::Word64)),
        Fact pid (hsByteString key) (hsByteString value))

    mkFact id pid key value =
      (Fid (fromIntegral (id::Word64)),
        Fact pid (hsByteString key) (hsByteString value))

    resultFacts = case maybePid of
      Nothing -> Vector.zipWith4 mkFact
        (hsArray fact_ids)
        (hsArray fact_pids)
        (hsArray fact_keys)
        (hsArray fact_values)
      Just (Pid pid) -> Vector.zipWith3 (mkResultFact pid)
        (hsArray fact_ids)
        (hsArray fact_keys)
        (hsArray fact_values)

    nestedFacts = Vector.zipWith4 mkFact
      (hsArray nested_fact_ids)
      (hsArray nested_fact_pids)
      (hsArray nested_fact_keys)
      (hsArray nested_fact_values)

  stats <-
    if wantStats
      then Just . hsMap <$> (# peek facebook::glean::rts::QueryResults, stats) p
      else return Nothing
  elapsed_ns <- (# peek facebook::glean::rts::QueryResults, elapsed_ns) p
  cont <- (# peek facebook::glean::rts::QueryResults, continuation) p

  return QueryResults
    { queryResultsFacts = resultFacts
    , queryResultsNestedFacts = nestedFacts
    , queryResultsStats = stats
    , queryResultsElapsedNs = elapsed_ns
    , queryResultsCont =
        let contBytes = hsByteString cont in
        if ByteString.length contBytes == 0
          then Nothing
          else Just contBytes
    }

interruptRunningQueries :: IO ()
interruptRunningQueries = glean_interrupt_running_queries

depth_ResultsOnly :: Word64
depth_ResultsOnly =
  (# const (int)facebook::glean::rts::Depth::ResultsOnly)

depth_ExpandRecursive :: Word64
depth_ExpandRecursive =
  (# const (int)facebook::glean::rts::Depth::ExpandRecursive)

depth_ExpandPartial :: Word64
depth_ExpandPartial =
  (# const (int)facebook::glean::rts::Depth::ExpandPartial)

foreign import ccall safe glean_query_execute_compiled
  :: Ptr Inventory
  -> Define
  -> Ptr (Subroutine CompiledQuery)
  -> Word64 -- pid
  -> Ptr (Subroutine CompiledTraversal) -- traverse
  -> Word64 -- max_results
  -> Word64 -- max_bytes
  -> Word64 -- max_time_ms
  -> Word64 -- depth
  -> Ptr Word64 -- expand_pids
  -> Word64 -- num_expand_pids
  -> Word64 -- want_stats
  -> Ptr Results
  -> IO CString

foreign import ccall safe glean_query_restart_compiled
  :: Ptr Inventory
  -> Define
  -> Ptr () -- cont
  -> CSize  -- cont_size
  -> Word64 -- max_results
  -> Word64 -- max_bytes
  -> Word64 -- max_time_ms
  -> Word64 -- depth
  -> Ptr Word64 -- expand_pids
  -> Word64 -- num_expand_pids
  -> Word64 -- want_stats
  -> Ptr Results
  -> IO CString

foreign import ccall unsafe glean_interrupt_running_queries
  :: IO ()

foreign import ccall unsafe glean_free_query_results
  :: Results -> IO ()
