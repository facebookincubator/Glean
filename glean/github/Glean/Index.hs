module Glean.Index
  (index)
  where

import qualified Glean.Types as Thrift
import qualified Glean.Index.Types as Thrift
import Control.Exception (throwIO)

type Port = Int

index
  :: IO Port
  -> a
  -> Thrift.IndexRequest
  -> IO Thrift.IndexResponse
index _ _ =
{- do
    if indexer for repo is not available
      error

    if base repo is not available
      download it from Backup in the background
      error with "downloading base repo, try again later"

    if revision is not indexed
      in the background:
        get diffs between base and revision from version control
        run indexer on (base repo, revision diffs)
      error with "indexing revision, try again later"

    if there is no repo indexed with the received diffs
      run indexer on (revision, received diffs)

    execute eviction policy on repos from incremental indexing

    return indexed repo
    where
      runIndexer (repo, diffs) =
        create new incremental repo
        for each diff in diffs
          fetch full file content from version control
          apply diff
          exclude file from repo
          invoke indexer process on file
        finish repo
-}
  throwIO $ Thrift.Exception "not implemented"
