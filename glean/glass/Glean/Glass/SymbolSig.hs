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
import qualified Glean.Schema.CodeLsif.Types as Lsif

import Glean.Glass.Pretty.Cxx as Cxx ( prettyCxxSignature )
import Glean.Glass.Pretty.Hack as Hack ( prettyHackSignature )
import Glean.Glass.Pretty.LSIF as LSIF ( prettyLsifSignature )

-- signature of symbols
class ToSymbolSignature a where
  toSymbolSignature :: a -> Glean.RepoHaxl u w (Maybe Text)

instance ToSymbolSignature Code.Entity where
  toSymbolSignature e = case e of
    -- cxx pretty signatures
    Code.Entity_cxx x -> pure $ case Cxx.prettyCxxSignature x of
      "" -> Nothing
      s -> Just s
    Code.Entity_pp{} -> pure Nothing
    -- hack pretty signatures
    Code.Entity_hack x -> Hack.prettyHackSignature x
    -- lsif languages
    Code.Entity_lsif e -> case e of
      Lsif.Entity_go x -> LSIF.prettyLsifSignature x
      Lsif.Entity_typescript x -> LSIF.prettyLsifSignature x
      Lsif.Entity_rust x -> LSIF.prettyLsifSignature x
      Lsif.Entity_java x -> LSIF.prettyLsifSignature x
      Lsif.Entity_EMPTY -> pure Nothing
    -- otherwise
    _ -> return Nothing
