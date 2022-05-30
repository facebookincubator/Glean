{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module A
  ( a
  ) where

import Data.Char (toLower)

a :: String
a = map toLower "A"
