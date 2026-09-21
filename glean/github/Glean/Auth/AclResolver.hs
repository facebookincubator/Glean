{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Auth.AclResolver
  ( initAclResolver
  ) where

import Data.Text (Text)

-- | Fail closed: caller is in no groups (public-only).
resolveAclGroups :: [Text] -> IO [Text]
resolveAclGroups = const (pure [])

initAclResolver :: IO ([Text] -> IO [Text])
initAclResolver = pure resolveAclGroups
