{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.SymbolSig
  (
    ToSymbolSignature(..)
  ) where

import Data.Text (Text)

import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Code.Types as Code

import Glean.Glass.Pretty.Cxx as Cxx ( prettyCxxSignature )
import Glean.Glass.Pretty.Hack as Hack ( prettyHackSignature )

-- signature of symbols
class ToSymbolSignature a where
  toSymbolSignature :: a -> Glean.RepoHaxl u w (Maybe Text)

instance ToSymbolSignature Code.Entity where
  toSymbolSignature e = case e of
    Code.Entity_cxx x -> return $ case Cxx.prettyCxxSignature x of
      "" -> Nothing
      s -> Just s
    Code.Entity_hack x -> Hack.prettyHackSignature x
    Code.Entity_pp{} -> return Nothing
    _ -> return Nothing
