{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Ownership
  ( factOwnership
  , OwnerExpr(..)
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Maybe
import qualified Data.Vector.Storable as Vector
import Data.Word

import Util.STM

import Glean.Database.Open
import Glean.Database.Storage as Storage
import Glean.Database.Types
import Glean.RTS.Foreign.Ownership
import Glean.RTS.Types
import Glean.Types

data OwnerExpr
  = Unit ByteString
  | AndOwners [OwnerExpr]
  | OrOwners [OwnerExpr]
  deriving Show

-- | Return the 'OwnerExpr' for a fact, if it has one. This is only for
-- introspection in the shell, and for tests.
factOwnership
  :: Env
  -> Repo
  -> Fid
  -> IO (Maybe OwnerExpr)
factOwnership env repo fid = do
  maybeSet <- readDatabase env repo $ \_ lookup -> getFactOwner lookup fid
  stack <- withOpenDatabaseStack env repo $ \OpenDB{..} -> do
    ownership <- readTVarIO odbOwnership
    return (ownership, Storage.getUnit odbHandle)
  let
    ownerships = mapMaybe fst stack

    getUnit unitId [] = getUset (coerce unitId) ownerships
    getUnit unitId (getUnitDB : rest) = do
      maybeUnit <- getUnitDB unitId
      case maybeUnit of
        Just unit -> return (Unit unit)
        Nothing -> getUnit unitId rest

    getUset usetId [] = throwIO $ ErrorCall $
      "unknown UsetId: " <> show (coerce usetId :: Word32)
    getUset usetId (own : owns) = do
      maybeExpr <- getOwnershipSet own usetId
      case maybeExpr of
        Nothing -> getUset usetId owns
        Just (op, vec) -> do
          contents <- mapM unpackSet (Vector.toList vec)
          case op of
            And -> return (AndOwners contents)
            Or -> return (OrOwners contents)

    unpackSet :: UsetId -> IO OwnerExpr
    unpackSet usetId = getUnit (coerce usetId) (map snd stack)

  mapM unpackSet maybeSet
