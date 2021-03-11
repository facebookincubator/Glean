-- Copyright 2004-present Facebook. All Rights Reserved.

{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Pretty.Code () where

import Data.Text.Prettyprint.Doc

import Glean.Pretty.Cxx ()
import Glean.Pretty.Hs ()
import Glean.Pretty.Java ()
import Glean.Pretty.Hack ()
import Glean.Schema.Code.Types as Code


instance Pretty Code.Entity where
  pretty (Entity_cxx ent) = pretty ent
  pretty (Entity_pp ent) = pretty ent
  pretty (Entity_java ent) = pretty ent
  pretty (Entity_hs ent) = pretty ent
  pretty (Entity_python _ent) = mempty -- TODO
  pretty (Entity_hack ent) = pretty ent
  pretty (Entity_flow _ent) = mempty -- TODO
