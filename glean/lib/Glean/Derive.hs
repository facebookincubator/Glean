module Glean.Derive
  ( derivePredicate
  ) where

import Control.Concurrent
import Data.Default
import Data.Int

import Glean.Angle.Types
import qualified Glean.Backend as Backend
import Glean.Types
import Glean.Query.Thrift.Internal

-- | Compute and store the specified derived predicate
derivePredicate
  :: Backend.Backend b
  => b
  -> Repo
  -> Maybe Int64  -- ^ page size (bytes)
  -> Maybe Int64  -- ^ page size (results)
  -> SourceRef    -- ^ predicate to derive
  -> IO ()

derivePredicate backend repo maxBytes maxResults
    (SourceRef name version) = do
  DerivePredicateResponse handle <- Backend.derivePredicate backend repo query
  checkProgress handle
  where
    query = def
      { derivePredicateQuery_predicate = name
      , derivePredicateQuery_predicate_version = version
      , derivePredicateQuery_options = Just def
        { derivePredicateOptions_max_results_per_query = maxResults
        , derivePredicateOptions_max_bytes_per_query = maxBytes
        }
      }

    checkProgress handle = do
      result <- Backend.pollDerivation backend handle
      case result of
        DerivationProgress_ongoing stats -> do
          reportUserQueryStats stats
          retry $ checkProgress handle
        DerivationProgress_complete stats -> do
          reportUserQueryStats  stats
          return ()

    retry :: IO a -> IO a
    retry action = do
      let sec = 1000000
      threadDelay $ 1 * sec
      action

