{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Storage
  ( Mode(..)
  , Storage(..)
  , DBVersion(..)
  , AxiomOwnership
  , canOpenVersion
  , currentVersion
  , writableVersions
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector.Storable as VS

import Glean.RTS.Foreign.FactSet (FactSet)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup (CanLookup)
import Glean.RTS.Foreign.Ownership
import Glean.RTS.Types (Fid, Pid)
import Glean.ServerConfig.Types (DBVersion(..))
import Glean.Types (PredicateStats, Repo, SchemaInfo)

-- | List of binary representation versions we can read
readableVersions :: [DBVersion]
readableVersions = [DBVersion 3]

-- | List of binary representation versions we can write
writableVersions :: [DBVersion]
writableVersions = [DBVersion 3]

-- | Check whether we can open a particular database version
canOpenVersion :: Mode -> DBVersion -> Bool
canOpenVersion mode version = version `elem` versions
  where
    versions = case mode of
      ReadOnly -> readableVersions
      ReadWrite -> writableVersions
      Create{} -> writableVersions

-- | Default current binary representation version
currentVersion :: DBVersion
currentVersion = maximum writableVersions

-- | Database opening mode
data Mode
  = ReadOnly
  | ReadWrite
  | Create
      Fid  -- starting fact id
      (Maybe SchemaInfo)  -- initial schema to merge with the configured one
  deriving(Show)

-- | Raw ownership data for axiomatic (non-derived) facts: a mapping
-- from unit name to ranges of fact IDs.
type AxiomOwnership = HashMap ByteString (VS.Vector Fid)

-- | An abstract storage for fact database
class CanLookup (Database s) => Storage s where
  -- | A fact database
  data Database s

  -- | Open a database
  open :: s -> Repo -> Mode -> DBVersion -> IO (Database s)

  -- | Close a database
  close :: Database s -> IO ()

  -- | Delete a database if it exists
  delete :: s -> Repo -> IO ()

  -- | Obtain the 'PredicateStats' for each predicate
  predicateStats :: Database s -> IO [(Pid, PredicateStats)]

  -- | Store an arbitrary binary key/value pair in the database. This data is
  -- completely separate from the facts.
  --
  -- NOTE: It is expected that 'store' and 'retrieve' are used sparingly and
  -- there are no performance guarantees. A typical use case for this is
  -- storing the serialised schema in the database.
  store :: Database s -> ByteString -> ByteString -> IO ()

  -- | Retrieve the value of a key previously stored with 'store'.
  retrieve :: Database s -> ByteString -> IO (Maybe ByteString)

  -- | Commit a set of facts to the database. The facts must have the right ids,
  -- they are NOT renamed.
  commit
    :: Database s
    -> FactSet
    -> AxiomOwnership
    -> IO ()

  -- | Optimise a database for reading. This is typically done before backup.
  optimize :: Database s -> IO ()

  computeOwnership :: Database s -> Inventory -> IO ComputedOwnership

  storeOwnership :: Database s -> ComputedOwnership -> IO ()

  -- | Fetch the 'Ownership' interface for this DB. This is used to
  -- make a 'Slice' (a view of a subset of the facts in the DB).
  --
  -- Can return 'Nothing' if this database backend doesn't support
  -- ownership. (TODO: support ownership in the memory backend and
  -- remove this 'Maybe').
  getOwnership :: Database s -> IO (Maybe Ownership)

  getUnitId :: Database s -> ByteString -> IO (Maybe UnitId)
  getUnit :: Database s -> UnitId -> IO (Maybe ByteString)

  addDefineOwnership :: Database s -> DefineOwnership -> IO ()

  computeDerivedOwnership
    :: Database s
    -> Ownership
    -> Pid
    -> IO ComputedOwnership

  -- | Backup a database. The scratch directory which can be used for storing
  -- intermediate files is guaranteed to be empty and will be deleted after
  -- the operation completes.
  backup
    :: Database s  -- ^ database
    -> FilePath  -- ^ scratch directory
    -> (Lazy.ByteString -> IO a)
          -- ^ function which expects the serialised database
    -> IO a

  -- | Restore a database. The scratch directory which can be used for
  -- storing intermediate files is guaranteed to be empty and will be
  -- deleted after the operation completes. The implementation may
  -- delete the serialized database file after it has been consumed,
  -- to reduce the number of copies of the DB on disk during a restore.
  restore
    :: s   -- ^ storage
    -> Repo  -- ^ repo
    -> FilePath  -- ^ scratch directory
    -> FilePath  -- ^ file containing the serialiased database (produced by 'backup')
    -> IO ()
