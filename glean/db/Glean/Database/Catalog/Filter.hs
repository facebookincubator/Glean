-- Copyright (c) Facebook, Inc. and its affiliates.

-- | This module implements a simple query language for Catalog entries. The
-- main goal is to have something that can be used for in-memory parts and SQL
-- databases.
--
-- This is very much work in progress.
{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
module Glean.Database.Catalog.Filter
  ( Locality(..)
  , ItemStatus(..)
  , Filter
  , Item(..)
  , Value
  , Order(..)
  , everythingF
  , queryableF
  , (.==.)
  , inF
  , notInF
  , groupF
  , sortF
  , limitF
  , repoV
  , localityV
  , repoNameV
  , createdV
  , statusV
  , completenessV
  , CompletenessTag(..)
  , backedUpV
  , runFilter
  ) where

import qualified Control.Monad.State.Strict as S
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.Maybe
import Data.Ord (Down(..))
import Data.Text (Text)
import GHC.Generics hiding (Meta)

import Glean.Database.Meta
import Glean.Types (Repo(..))
import qualified Glean.Internal.Types as Thrift
import qualified Glean.Types as Thrift

-- | How a DB is available
data Locality = Local | Restoring | Cloud
  deriving(Eq,Ord,Enum,Bounded,Show)

-- The status of a stacked database, ordered by increasing severity
data ItemStatus
  = ItemComplete
  | ItemIncomplete
  | ItemBroken
  | ItemRestoring
  | ItemMissing
  deriving (Eq,Ord,Show,Generic,Hashable)

instance Semigroup ItemStatus where
  (<>) = max

instance Monoid ItemStatus where
  mempty = ItemComplete


data Item = Item
  { itemRepo :: Repo
  , itemLocality :: Locality
  , itemMeta :: Meta
  , itemStatus :: ItemStatus
  } deriving(Show)

-- | A field that can be used in queries
newtype Value a = Value (Item -> a)

repoV :: Value Repo
repoV = Value itemRepo

repoNameV :: Value Text
repoNameV = Value (repo_name . itemRepo)

localityV :: Value Locality
localityV = Value itemLocality

createdV :: Value Thrift.PosixEpochTime
createdV = Value (metaCreated . itemMeta)

statusV :: Value Thrift.DatabaseStatus
statusV = Value (completenessStatus . itemMeta)

-- | DatabaseStatus doesn't distinguish Complete from Finalizing, so
-- to expose that distinction we need another form of status based on
-- the metaCompleteness.
data CompletenessTag
  = CompletenessIncomplete
  | CompletenessComplete
  | CompletenessBroken
  | CompletenessFinalizing
  deriving Eq

completenessTag :: Thrift.Completeness -> CompletenessTag
completenessTag Thrift.Incomplete{} = CompletenessIncomplete
completenessTag Thrift.Complete{} = CompletenessComplete
completenessTag Thrift.Broken{} = CompletenessBroken
completenessTag Thrift.Finalizing{} = CompletenessFinalizing

completenessV :: Value CompletenessTag
completenessV = Value (completenessTag . metaCompleteness . itemMeta)

entryStatusV :: Value ItemStatus
entryStatusV = Value itemStatus

backedUpV :: Value Bool
backedUpV = Value (isJust . metaBackup . itemMeta)

newtype Filter a = Filter (S.State [Item] a)
  deriving newtype (Functor,Applicative,Monad)

runFilter :: Filter () -> [Item] -> [Item]
runFilter (Filter m) = S.execState m

modify :: ([Item] -> [Item]) -> Filter ()
modify = Filter . S.modify'

filter_ :: (Item -> Bool) -> Filter ()
filter_ = modify . filter

everythingF :: Filter ()
everythingF = return ()

queryableF :: Filter ()
queryableF =
  inF entryStatusV $
    HashSet.fromList [ItemComplete, ItemIncomplete, ItemBroken]

-- | Require that a field has a specific value
(.==.) :: Eq a => Value a -> a -> Filter ()
Value f .==. x = filter_ $ \q -> f q == x

-- | Require that a field's value is in a set
inF :: (Eq a, Hashable a) => Value a -> HashSet a -> Filter ()
inF (Value f) xs = filter_ $ \q -> f q `HashSet.member` xs

-- | Require that a field's value is not in a set
notInF :: (Eq a, Hashable a) => Value a -> HashSet a -> Filter ()
notInF (Value f) xs = filter_ $ \q -> not $ f q `HashSet.member` xs

-- | Group the data by a field and apply the clauses to each group separately
groupF :: (Eq a, Hashable a) => Value a -> Filter () -> Filter ()
groupF (Value f) (Filter p) = modify $ \qs ->
  concatMap (S.execState p . reverse)
  $ HashMap.elems
  $ HashMap.fromListWith (++) [(f q, [q]) | q <- qs]

data Order = Ascending | Descending
  deriving(Eq,Ord,Enum,Bounded,Read,Show)

-- | Sort the data by a field
sortF :: Ord a => Value a -> Order -> Filter ()
sortF (Value f) Ascending = modify $ sortOn f
sortF (Value f) Descending = modify $ sortOn $ Down . f

-- | Only keep the first n data items
limitF :: Int -> Filter ()
limitF = modify . take
