{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.SymbolSig
  (
    ToSymbolSignature(..),
    toSymbolSignatureText,
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
  (SimpleDocStream, LayoutOptions(..), PageWidth(..))
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodeLsif.Types as Lsif

import Glean.Glass.Pretty.Cxx as Cxx ( prettyCxxSignature )
import Glean.Glass.Pretty.Hack as Hack ( prettyHackSignature )
import Glean.Glass.Pretty.LSIF as LSIF ( prettyLsifSignature )

toSymbolSignatureText
  :: ToSymbolSignature a => a -> Glean.RepoHaxl u w (Maybe Text)
toSymbolSignatureText x = do
  maybeDoc <- toSymbolSignature (LayoutOptions (AvailablePerLine 80 1)) x
  return $ renderStrict <$> maybeDoc


-- signature of symbols
class ToSymbolSignature a where
  toSymbolSignature
    :: LayoutOptions -> a -> Glean.RepoHaxl u w (Maybe (SimpleDocStream ()))


instance ToSymbolSignature Code.Entity where
  toSymbolSignature opts e = case e of
    -- cxx pretty signatures
    Code.Entity_cxx x -> pure $ Cxx.prettyCxxSignature opts x
    Code.Entity_pp{} -> pure Nothing
    -- hack pretty signatures
    Code.Entity_hack x -> Hack.prettyHackSignature opts x
    -- lsif languages, just enumerate completely to stay total
    Code.Entity_lsif e -> case e of
      Lsif.Entity_erlang x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_fsharp x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_go x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_haskell x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_java x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_kotlin x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_ocaml x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_python x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_rust x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_scala x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_swift x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_typescript x -> LSIF.prettyLsifSignature opts x
      Lsif.Entity_EMPTY -> pure Nothing
    -- otherwise
    _ -> return Nothing
