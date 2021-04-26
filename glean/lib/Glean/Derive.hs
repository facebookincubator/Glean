module Glean.Derive
  ( derivePredicate
  ) where

import Control.Concurrent
import Control.Exception
import Data.Default
import Data.Int
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text

import Glean.Angle.Types
import Glean.Backend hiding (derivePredicate)
import Glean.Schema.Util
import Glean.Types
import Glean.Write.SendQueue
import Glean.Query.Thrift.Internal

-- | Compute and store the specified derived predicate
derivePredicate
  :: Backend b
  => b
  -> Repo
  -> SendQueue  -- ^ async sender to send the results
  -> Maybe Int64  -- ^ page size (bytes)
  -> Maybe Int64  -- ^ page size (results)
  -> SourceRef    -- ^ predicate to derive
  -> IO ()

derivePredicate backend repo sender maxBytes maxResults
    ref@(SourceRef name version) = do
  let
    q0 = def
      { userQuery_predicate = name
      , userQuery_predicate_version = version
      , userQuery_query = Text.encodeUtf8 $ showSourceRef ref <> " _"
      , userQuery_options = Just opts
      , userQuery_encodings = [UserQueryEncoding_bin def]
      }

    opts = def
      { userQueryOptions_expand_results = False
      , userQueryOptions_syntax = QuerySyntax_ANGLE
      , userQueryOptions_store_derived_facts = True
      , userQueryOptions_max_bytes = maxBytes
      , userQueryOptions_max_results = maxResults
      , userQueryOptions_omit_results = True
      }

    retry :: Double -> IO a -> IO a
    retry secs action = do
      threadDelay $ round $ max secs 1 * 1000000
      action

    loop q = do
      result <- try $ userQuery backend repo q
      case result of
        Left Retry{..} -> retry retry_seconds (loop q)
        Right UserQueryResults{..} -> do
          mapM_ reportUserQueryStats userQueryResults_stats
          let
            size = case userQueryResults_results of
              UserQueryEncodedResults_bin UserQueryResultsBin{..} ->
                Map.size userQueryResultsBin_facts
              _other -> 0
          case userQueryResults_handle of
            Nothing -> return ()
            Just handle -> addSendQueueWait sender handle size
          let
            q' = q
              { userQuery_options = Just opts
                { userQueryOptions_continuation =
                  userQueryResults_continuation } }
          case userQueryResults_continuation of
            Nothing -> return ()
            Just _ -> loop q'

  loop q0
