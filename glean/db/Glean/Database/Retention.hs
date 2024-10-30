{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Retention (
    dbIndex, DbIndex(..),
    retentionChanges,
    RetentionChanges(..),
    repoRetention,
    hasProperties,
  ) where

import Control.Monad
import Control.Monad.Extra
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.List.Extra (nubOrdOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time

import Util.List

import Glean.Database.Catalog.Filter
import Glean.Database.Meta
import Glean.Database.Restore (restorable)
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types
import Glean.Util.ShardManager
import Glean.Util.TransitiveClosure

data RetentionChanges shard = RetentionChanges
  { retentionLocal :: [(Item,shard)]
  , retentionDelete :: [Item]
  , retentionRestore :: [Item]
  , retentionElsewhere :: [Item]
  , localMissingDependencies :: [Item]
  , allMissingDependencies :: [Item]
  }

-- | Compute the changes to the current set of local DBs based on the
-- set of known DBs (DbIndex), the retention/restore policy, the local
-- shard, and an oracle to determine whether a DB is available elsewhere.

retentionChanges
  :: forall m shard . (Monad m, Show shard, Ord shard)
  => ServerConfig.DatabaseRetentionPolicy
    -- ^ Retention policy
  -> ServerConfig.DatabaseRestorePolicy
    -- ^ Restore policy
  -> UTCTime
    -- ^ Current time
  -> DbIndex
    -- ^ All DBs
  -> (Item -> m Bool)
    -- ^ Is a DB available on another server?
  -> (BaseOfStack -> Repo -> shard)
    -- ^ Compute shard for DB
  -> Set.Set shard
    -- ^ Shards to retain in this node
  -> m (RetentionChanges shard)

retentionChanges
    retentionPolicy restorePolicy
    now
    index
    isAvailable
    dbToShard myShards = do

  keep <- computeRetentionSet
    retentionPolicy restorePolicy now isAvailable index

  let
    itemToShard :: Item -> Maybe shard
    itemToShard item = do
      stack <- repoStack index item
      return $
        dbToShard (BaseOfStack $ last $ itemRepo item : stack) (itemRepo item)

    keepAnnotatedWithShard =
      [ (item, guard (shard `Set.member` myShards) >> pure shard)
      | item <- keep
      -- We get Nothing when a dependency is missing, which should never happen.
      -- If it does a 'logWarning' will be emitted by 'checkDependencies' below
      , Just shard <- [itemToShard item]
      ]

    keepInThisNode =
      [ (item, shard)
      | (item, Just shard) <- keepAnnotatedWithShard ]

    delete =
      [ item
      | item@(Item repo Local _ _) <- Map.elems (byRepo index)
      , repo `notElem` map (itemRepo . fst) keepInThisNode ]

    fetch =
      [ item
      | (item@Item{..},_) <- keepInThisNode
      , itemLocality == Cloud ]

    elsewhere =
      [ item
        -- Nothing means it is not in any of the shards assigned to this node
      | (item, Nothing) <- keepAnnotatedWithShard
      ]

    missingDeps dbs =
      nubOrdOn itemRepo [db | db <- dbs, Nothing <- dependencies index db]

  return RetentionChanges {
    retentionLocal = keepInThisNode,
    retentionDelete = delete,
    retentionRestore = fetch,
    retentionElsewhere = elsewhere,
    localMissingDependencies = missingDeps (map fst keepInThisNode),
    allMissingDependencies = missingDeps keep
  }

repoStack :: DbIndex -> Item -> Maybe [Repo]
repoStack index@DbIndex{..} Item{..} =
  case metaDependencies itemMeta of
    Just (Dependencies_stacked Stacked{..}) -> do
      let base = Repo stacked_name stacked_hash
      baseItem <- Map.lookup base byRepo
      rest <- repoStack index baseItem
      return (base : rest)

    Just (Dependencies_pruned Pruned{..}) -> do
      baseItem <- Map.lookup pruned_base byRepo
      rest <- repoStack index baseItem
      return (pruned_base : rest)
    Nothing -> return []

-- | Information about the set of DBs that we have
data DbIndex = DbIndex
  { byRepo :: Map.Map Repo Item
    -- ^ Items indexed by Repo
  , byRepoName :: [(Text, NonEmpty Item)]
    -- ^ Items grouped by Repo name
  , dependencies :: Item -> [Maybe Item]
    -- ^ DB dependencies, Nothing indicates that there is a dependency
    -- but it is missing in the catalog.
  }

dbIndex :: [Item] -> DbIndex
dbIndex items = DbIndex{..}
  where
    byRepo =
      Map.fromListWith pickFromDuplicates
        $ [(itemRepo item, item) | item <- items]
    pickFromDuplicates x y = -- Pick local one in a conflict
      minimumBy (\x y -> compare (itemLocality x) (itemLocality y)) [x, y]

    byRepoName =
      HashMap.toList $ HashMap.fromListWith (<>)
        [ (repo_name $ itemRepo item, item :| []) | item <- items ]

    dependencies = stacked . metaDependencies . itemMeta

    stacked (Just (Dependencies_stacked Stacked{..})) =
      [ Repo stacked_name stacked_hash `Map.lookup` byRepo ]
    stacked (Just (Dependencies_pruned update)) =
      [ pruned_base update `Map.lookup` byRepo ]
    stacked Nothing = []

-- | The final set of DBs we want usable on disk.
-- This is the set of 'keepRoots' DBs extended with all the stacked dependencies
computeRetentionSet
  :: forall m . Monad m
  => ServerConfig.DatabaseRetentionPolicy
  -> ServerConfig.DatabaseRestorePolicy
  -> UTCTime
  -> (Item -> m Bool)
  -> DbIndex
  -> m [Item]
computeRetentionSet config_retention config_restore
    time isAvailableM dbIndex@DbIndex{..} =
  transitiveClosureBy itemRepo (catMaybes . depsRestored) <$>
    concatMapM allRetention byRepoName
  where
    -- Add transitive dependencies of a DB to the retention set only
    -- if the DB is local or will be restored.
    depsRestored :: Item -> [Maybe Item]
    depsRestored item@Item{..}
      | itemLocality == Local
        || restorable config_restore itemRepo = dependencies item
      | otherwise = []

    allRetention :: (Text, NonEmpty Item) -> m [Item]
    allRetention (repo, dbs) = do
      let policies = repoRetention config_retention repo
      uniqBy (comparing itemRepo) . concat <$>
        mapM (\pol ->
          dbRetentionForRepo pol time isAvailableM dbs dbIndex) policies


-- | The target set of DBs we want usable on the disk. This is a set of
-- DBs that satisfies the policy.
dbRetentionForRepo
  :: Monad m
  => ServerConfig.Retention
  -> UTCTime
  -> (Item -> m Bool)
  -> NonEmpty Item
  -> DbIndex
  -> m [Item]
dbRetentionForRepo ServerConfig.Retention{..} t isAvailableM dbs dbIndex = do
  let
    -- retention policy parameters
    retainAtLeast' = fromIntegral $ fromMaybe 0 retention_retain_at_least
    retainAtMost' = fmap fromIntegral retention_retain_at_most
    -- enforce invariant: atLeast <= atMost
    retainAtLeast = min retainAtLeast' (fromMaybe maxBound retainAtMost')
    retainAtMost  = max retainAtLeast' <$> retainAtMost'
    deleteIfOlder = fmap fromIntegral retention_delete_if_older
    deleteIncompleteIfOlder =
      fmap fromIntegral retention_delete_incomplete_if_older

    f &&& g = \x -> f x && g x
    f ||| g = \x -> f x || g x
    f &&&> g = \x -> if not(f x) then return False else g x
    f |||> g = \x -> if f x then return True else g x

    ifSet (Just a) f = f a
    ifSet Nothing _ = const False

    -- predicates
    isLocal Item{..} = itemLocality == Local
    isComplete Item{..} =
      completenessStatus itemMeta == DatabaseStatus_Complete
    isOlderThan secs Item{..} = dbAge t itemMeta >= secs
    isAvailable = isLocal |||> isAvailableM
    hasDependencies = not . missingDependencies dbIndex

    -- all DBs with the required properties, sorted by most recent first
    sorted =
      sortOn (Down . dbTime . itemMeta) $
      filter (hasProperties retention_required_properties) $
      NonEmpty.toList dbs

    -- whether to delete a DB according to the deletion policy
    delete =
      ifSet deleteIfOlder isOlderThan |||
      (ifSet deleteIncompleteIfOlder $ \secs ->
        (not . isComplete) &&& isOlderThan secs)

    -- ensure we have retain_at_least DBs from the available set
  atLeast <- takeFilterM
    retainAtLeast
    (isComplete &&& hasDependencies &&&> isAvailable)
    -- bound the search since isAvailable is expensive
    -- this matters only for tier bootstraps where all DBs are unavailable
    (take (retainAtLeast*10) sorted)

    -- delete DBs according to the deletion policy, and keep retain_at_most
  let atMost = maybe id take retainAtMost (filter (not . delete) sorted)

  return $ uniqBy (comparing itemRepo) (atLeast ++ atMost)

missingDependencies :: DbIndex -> Item -> Bool
missingDependencies dbIndex item = any isNothing (dependencies dbIndex item)

hasProperties :: HashMap.HashMap Text Text -> Item -> Bool
hasProperties req Item{..} = all has (HashMap.toList req)
  where
  has (name,val) =
    HashMap.lookup name (metaProperties itemMeta) == Just val

repoRetention
  :: ServerConfig.DatabaseRetentionPolicy
  -> Text
  -> NonEmpty ServerConfig.Retention
repoRetention ServerConfig.DatabaseRetentionPolicy{..} repoNm =
  case NonEmpty.nonEmpty (old_retention <> new_retention) of
    Nothing -> databaseRetentionPolicy_default_retention :| []
    Just some -> some
  where
  old_retention =
    maybeToList $
      Map.lookup
        repoNm
        databaseRetentionPolicy_repos
  new_retention =
    Map.findWithDefault
      []
      repoNm
      databaseRetentionPolicy_by_repo

-- | Take the first n items that satisfy the predicate
takeFilterM :: (Monad m) => Int -> (a -> m Bool) -> [a] -> m [a]
takeFilterM n pred = loop [] n where
  loop acc _ [] = return (reverse acc)
  loop acc 0 _ = return (reverse acc)
  loop acc n (x:xx) = do
    accept <- pred x
    if accept
      then loop (x:acc) (n-1) xx
      else loop acc n xx
