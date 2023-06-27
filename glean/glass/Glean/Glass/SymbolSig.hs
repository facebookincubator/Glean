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
    -- testing
    renderTest
  ) where

import Data.Text (Text, replicate, length)
import Data.Text.Prettyprint.Doc

import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodeLsif.Types as Lsif
import qualified Glean.Schema.CodeScip.Types as Scip

import Glean.Glass.Pretty.Cxx as Cxx ( prettyCxxSignature, Qualified(..) )
import Glean.Glass.Pretty.Flow as Flow ( prettyFlowSignature )
import Glean.Glass.Pretty.Hack as Hack ( prettyHackSignature )
import Glean.Glass.Pretty.LSIF as LSIF ( prettyLsifSignature )
import Glean.Glass.Pretty.SCIP as SCIP ( prettyScipSignature )
import Glean.Glass.Pretty.Python as Python ( prettyPythonSignature )
import Glean.Glass.Types
    ( RepoName, SymbolId(..), TypeSymSpan(..), ByteSpan(..) )
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy as TL

-- This is equivalent to renderStrict but extract the symbol spans
-- from the DocStream. Offsets are relative to the returned text.
render :: SimpleDocStream (Maybe SymbolId) -> (Text, [TypeSymSpan])
render stream =
    let (txt, xrefs) = render' stream 0 Nothing (mempty, []) in
    (TL.toStrict $ TLB.toLazyText txt, reverse xrefs)
  where
    -- curAnnot is the starting offset of the last symbol seen
    -- we know its length and generate the span when the annotation
    -- is popped
    render' stream curLen curAnnot res =
      let (txt, xrefs) = res in
      case stream of
        SFail -> error "render: the impossible has happened"
        SEmpty -> res
        SChar c rest ->
          render' rest (curLen + 1) curAnnot (txt <> TLB.singleton c, xrefs)
        SText _l t rest ->
          let res' = (txt <> TLB.fromText t, xrefs) in
          render' rest (curLen + Data.Text.length t) curAnnot res'
        SLine i rest ->
          let res' = (txt <>  TLB.singleton '\n' <>
                      TLB.fromText (Data.Text.replicate i " "), xrefs) in
          render' rest (curLen + 1 + i) curAnnot res'
        SAnnPush ann rest ->
          case ann of
            Just sym ->
              render' rest curLen (Just (sym, curLen)) res
            Nothing ->
              render' rest curLen Nothing res
        SAnnPop rest ->
          case curAnnot of
            Just (sym, xrefStart) ->
              let span = ByteSpan
                    (fromIntegral xrefStart)
                    (fromIntegral $ curLen - xrefStart) in
              let xref = TypeSymSpan sym span in
              render' rest curLen Nothing (txt, xref : xrefs)
            Nothing ->
              render' rest curLen Nothing res


renderTest :: SimpleDocStream ann  -> Text
renderTest stream =
  fst $ render $ reAnnotateS (const Nothing) stream

sIGNATURE_LINEWRAP :: Int
sIGNATURE_LINEWRAP = 120

toSymbolSignatureText
  :: ToSymbolSignature a
  => a
  -> RepoName
  -> SymbolId
  -> Cxx.Qualified
  -> Glean.RepoHaxl u w (Maybe Text, [TypeSymSpan])
toSymbolSignatureText entity repo sym qualified = do
  maybeDoc <- toSymbolSignature (LayoutOptions
      (AvailablePerLine sIGNATURE_LINEWRAP 1)) entity repo sym qualified
  case maybeDoc of
    Nothing -> return (Nothing, [])
    Just doc ->
      let (txt, xrefs) = render doc in
      return (Just txt, xrefs)

-- signature of symbols
-- The signature SimpleDocStream is annotated with SymbolId.
-- The renderer can extract them with their span so as to
-- generate type xrefs
--
class ToSymbolSignature a where
  toSymbolSignature
    :: LayoutOptions
    -> a
    -> RepoName -- ^ repo name of this entity
    -> SymbolId -- ^ symbol id of this entity if you need to link to it
    -> Cxx.Qualified
    -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))

-- signature of symbols
-- for supported languages (c++), qualified indicates whether to
-- fully qualify the name of the code entity in question. Other
-- symbols in the signature will always remain fully qualified.
instance ToSymbolSignature Code.Entity where
  toSymbolSignature opts e repo sym qualified = case e of
    -- cxx pretty signatures
    Code.Entity_cxx x -> pure $ Cxx.prettyCxxSignature opts x qualified
    Code.Entity_pp{} -> pure Nothing
    -- hack pretty signatures
    Code.Entity_hack x -> Hack.prettyHackSignature opts repo sym x
    -- Flow signatures direct from the DB
    Code.Entity_flow x -> Flow.prettyFlowSignature opts x
    -- python pretty signatures
    Code.Entity_python x -> Python.prettyPythonSignature opts repo sym x
    -- scip languages, just enumerate completely to stay total
    Code.Entity_scip e -> case e of
      Scip.Entity_rust x -> SCIP.prettyScipSignature opts x
      Scip.Entity_go x -> SCIP.prettyScipSignature opts x
      Scip.Entity_EMPTY -> pure Nothing
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
