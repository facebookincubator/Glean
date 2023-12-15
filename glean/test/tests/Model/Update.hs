{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
module Model.Update (
  stepModel,
) where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (evalState)
import Data.Default
import Data.Functor.Identity (Identity (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as Set
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Text (pack, unpack)
import Glean.Database.Backup (bestRestore, newestByRepo)
import Glean.Database.Catalog (
  EntriesF (..),
  EntryF (entryStatus),
  list',
 )
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter (
  Item (..),
  ItemStatus (ItemComplete),
  Locality (Cloud, Local, Restoring),
  everythingF,
  statusV,
  (.==.),
 )
import Glean.Database.Janitor
import Glean.Database.Meta (posixEpochTimeToUTCTime)
import Glean.Internal.Types (Meta (..))
import Glean.Types (
  DatabaseStatus (DatabaseStatus_Complete),
  Repo,
  repo_hash,
  repo_name,
 )
import Model.Command (
  Command (..),
  ShardingAssignmentChange (ShardAdded, ShardRemoved),
 )
import Model.Model (Model (..), addTime, mkEntry)
import System.FilePath ((<.>), (</>))
import Text.Printf

-- | Update function for the model, applying a command and returning a new model
stepModel :: Model -> Command -> Model
stepModel model (NewRemoteDB repo meta) =
  recomputeEntries
    model
      { modelRestorableDBs =
          HM.insert
            repo
            meta
              { metaBackup =
                  Just $
                    pack $
                      "mock:"
                        <> modelBackupDir model
                          </> unpack (repo_name repo)
                          <.> unpack (repo_hash repo)
              }
            (modelRestorableDBs model)
      }
stepModel model (TimeElapsed s) =
  recomputeEntries
    model
      { -- we need to recompute in case of a carry-over previous download
        modelTime = addTime s $ modelTime model
      }
stepModel model (ShardingAssignmentChange (ShardAdded x)) =
  recomputeEntries
    model
      { modelShardAssignment = Set.insert x $ modelShardAssignment model
      }
stepModel model (ShardingAssignmentChange (ShardRemoved x)) =
  recomputeEntries
    model
      { modelShardAssignment = Set.delete x $ modelShardAssignment model
      }
stepModel m DBDownloaded =
  case modelDownloadingDB m of
    Nothing ->
      -- we need to recompute in case of a carry-over previous download
      recomputeEntries m
    Just repo ->
      let meta = case HM.lookup repo $ entriesRestoring (modelEntries m) of
            Just x -> x
            Nothing ->
              error $
                printf
                  "model error: cannot find %s in entriesRestoring (%s)\n"
                  (show repo)
                  (show $ HM.keys $ entriesRestoring $ modelEntries m)
          -- recompute the state before async download
          m' = recomputeEntries m
          -- update the state with the downloaded db
          modelEntries' =
            (modelEntries m')
              { entriesLiveHere =
                  HM.insert
                    repo
                    (mkEntry $ Item repo Local meta ItemComplete)
                      { entryStatus = pure ItemComplete
                      }
                    (entriesLiveHere $ modelEntries m')
              , entriesRestoring =
                  HM.delete repo $ entriesRestoring (modelEntries m')
              }
       in m'
            { modelEntries = modelEntries'
            , modelDownloadingDB = nextDownload modelEntries'
            }

nextDownload :: EntriesF (EntryF Identity) -> Maybe Repo
nextDownload modelEntries =
  itemRepo <$> listToMaybe (bestRestore restoring available)
  where
    list status f = runIdentity $ list' id status f modelEntries
    restoring = list [Restoring] newestByRepo
    available = list [Local] $ do
      newestByRepo
      statusV .==. DatabaseStatus_Complete

recomputeEntries :: Model -> Model
recomputeEntries model@Model {..} =
  model
    { modelEntries = modelEntries'
    , modelDownloadingDB = modelDownloadingDB <|> nextDownload modelEntries'
    }
  where
    modelEntries' =
      modelEntries
        { entriesLiveHere = entriesLiveHere'
        , entriesLiveElsewhere = entriesLiveElsewhere'
        , entriesRestoring = entriesRestoring'
        }
    time = posixEpochTimeToUTCTime modelTime
    Identity localAndRestoring =
      Catalog.list' id [Local, Restoring] everythingF modelEntries
    allDBs = mergeLocalAndRemote
      (HM.toList modelRestorableDBs)
      localAndRestoring
    index = dbIndex allDBs
    retentionSet =
      computeRetentionSet modelRetentionPolicy def time
        (const $ pure True) index `evalState` mempty
    keepHereSet = [(itemRepo, itemMeta) | Item {..} <- retentionSet]
    entriesLiveHere' =
      foldl'
        (flip HM.delete)
        (entriesLiveHere modelEntries)
        [ itemRepo
        | item@Item {..} <- allDBs
        , itemLocality == Local
        , not $
            (itemRepo, itemMeta) `elem` keepHereSet && itemInShardAssignment item
        , Just itemRepo /= modelDownloadingDB
        ]
    entriesLiveElsewhere' =
      HM.fromList
        [ (itemRepo item, mkEntry item)
        | item <- retentionSet
        , not $ itemInShardAssignment item
        ]
    entriesRestoring' =
      entriesRestoring modelEntries
        <> HM.fromList
          [ (itemRepo item, itemMeta item)
          | item <- retentionSet
          , itemInShardAssignment item
          , itemLocality item == Cloud
          ]
    itemShard Item {itemRepo} = repoShard itemRepo
    repoShard = repo_hash
    itemInShardAssignment i = itemShard i `Set.member` modelShardAssignment
