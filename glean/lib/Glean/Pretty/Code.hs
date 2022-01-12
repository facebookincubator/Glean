{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Pretty.Code () where

import Data.Text.Prettyprint.Doc

import Glean.Pretty.Cxx ()
import Glean.Pretty.Hs ()
import Glean.Pretty.Java ()
import Glean.Pretty.Hack ()
import Glean.Schema.Code.Types as Code
import Glean.Schema.CodePp.Types as Pp


instance Pretty Code.Entity where
  pretty (Entity_cxx ent) = pretty ent
  pretty (Entity_pp (Pp.Entity_define ent)) = pretty ent
  pretty (Entity_java ent) = pretty ent
  pretty (Entity_hs ent) = pretty ent
  pretty (Entity_hack ent) = pretty ent
  pretty _ent = mempty
