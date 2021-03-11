module Glean.Database.Catalog.Store
  ( Store(..)
  ) where

import Data.HashMap.Strict

import Glean.Database.Meta (Meta)
import Glean.Database.Repo (Repo)

-- | A metadata store
class Store a where
  -- | List all available entries.
  list :: a -> IO (HashMap Repo Meta)

  -- | Create an entry if it doesn't exist. Return `True` if entry was created
  -- and `False` if it already existed.
  create :: a -> Repo -> Meta -> IO Bool

  -- | Delete an entry. Return `True` if entry was deleted and `False` if it
  -- didn't exist
  delete :: a -> Repo -> IO Bool

  -- | Update the metadata of an entry. Return `True` if metadata was updated
  -- and `False` if the entry didn't exist.
  put :: a -> Repo -> Meta -> IO Bool

  -- | Read the metadata of an entry, returning `Nothing` if the entry didn't
  -- exist.
  get :: a -> Repo -> IO (Maybe Meta)
