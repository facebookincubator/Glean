{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Digest
  (
    fetchFileDigests,
    toDigest,
    toDigestMap
  ) where

import Data.Text ( Text )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import qualified Glean
import Glean.Haxl.Repos as Glean
import qualified Glean.Glass.Query as Query
import Glean.Glass.Utils ( searchWithLimit )

import Glean.Glass.Types

import qualified Glean.Schema.Digest.Types as Digest
import qualified Glean.Schema.Src.Types as Src

type RepoName_ = Text -- avoid newtype double-quoting in thrift

fetchFileDigests
  :: Int -> [Glean.IdOf Src.File] -> Glean.RepoHaxl u w [Digest.FileDigest]
fetchFileDigests _ [] = pure [] -- avoid an empty call
fetchFileDigests n srcFiles = searchWithLimit (Just n) query
  where
    query = Query.fileDigests srcFiles

-- Export Src.Digest to Glass type
toDigest :: Digest.Digest -> FileDigest
toDigest Digest.Digest{..} = FileDigest{..}
  where
    fileDigest_hash = digest_hash
    fileDigest_size = Glean.unNat digest_size

-- Join the fileDigest predicate (keyed by id) back onto the full repo/path
toDigestMap
  :: Map (Glean.IdOf Src.File) (RepoName, Path)
  -> [Digest.FileDigest]
  -> Glean.RepoHaxl u w (Map RepoName_ FileDigestMap)
toDigestMap fileIdMap digests = go digests mempty
  where
    go [] !acc = pure acc
    go (pred:ps) !acc = do
      Digest.FileDigest_key fileId digest <- Glean.keyOf pred
      case Map.lookup (Glean.getId fileId) fileIdMap of
        Nothing -> go ps acc -- digest with no associated file
        Just (RepoName repo, Path path) -> do
          let v = Map.singleton path (toDigest digest)
          go ps $! Map.insertWith mappend repo v acc
