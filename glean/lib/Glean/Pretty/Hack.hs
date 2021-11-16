{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


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
