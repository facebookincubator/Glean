-- Copyright (c) Facebook, Inc. and its affiliates.


{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Pretty.Hack () where

import Data.Text.Prettyprint.Doc

import Glean.Schema.CodeHack.Types
import Glean.Schema.Hack.Types
import Glean.Pretty.HackAnn

instance Pretty Declaration where
  pretty = unAnnotate . prettyScopedDeclaration

instance Pretty Entity where
  pretty (Entity_decl d) = pretty d
