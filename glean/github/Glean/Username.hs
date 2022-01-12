{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Username (getUsername) where

import System.Posix.User

getUsername :: IO (Maybe String)
getUsername = do
    user <- getUserEntryForID =<< getRealUserID
    return $ Just $ userName user
