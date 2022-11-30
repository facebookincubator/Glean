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

import Control.Concurrent.STM
import Control.Exception
import Data.ByteString (ByteString)
import Data.Coerce
import qualified Data.Vector.Storable as Vector
import Data.Word

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
factOwnership env repo fid =
  readDatabase env repo $ \OpenDB{..} lookup -> do
    maybeOwnership <- readTVarIO odbOwnership
    flip (maybe (return Nothing)) maybeOwnership $ \ownership -> do
      let
        unpackSet :: UsetId -> IO OwnerExpr
        unpackSet usetId = do
          maybeUnit <- Storage.getUnit odbHandle (coerce usetId)
          -- TODO: try the other layers when this is a stack
          case maybeUnit of
            Just unit -> return (Unit unit)
            Nothing -> do
              maybeExpr <- getOwnershipSet ownership usetId
              case maybeExpr of
                Nothing -> throwIO $ ErrorCall $
                  "unknown UsetId: " <> show (coerce usetId :: Word32)
                Just (op, vec) -> do
                  contents <- mapM unpackSet (Vector.toList vec)
                  case op of
                    And -> return (AndOwners contents)
                    Or -> return (OrOwners contents)

      maybeSet <- getFactOwner lookup fid
      mapM unpackSet maybeSet
