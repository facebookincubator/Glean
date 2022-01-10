{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Pretty.Java
  (
  ) where

import Data.Text.Prettyprint.Doc

import Glean.Pretty.Src ()
import Glean.Schema.CodeJava.Types as Java
import Glean.Schema.Java.Types as Java
import Glean.Util.Range (locRange)
import Glean.Util.URI


instance Pretty Java.Entity where
  pretty (Java.Entity_class_ decl) = pretty decl

instance Pretty Java.ClassDeclaration where
  pretty Java.ClassDeclaration{..} = maybe "" pretty classDeclaration_key

instance Pretty Java.ClassDeclaration_key where
  pretty Java.ClassDeclaration_key{..} = vsep
    [ "class" <+> pretty classDeclaration_key_name
    , "at" <+> pretty classDeclaration_key_loc
    , pretty $ show <$>
        fbsDiffusionURIForRange (locRange classDeclaration_key_loc)
    ]

instance Pretty Java.Name where
  pretty Java.Name{..} = maybe "" pretty name_key

instance Pretty Java.QName where
  pretty Java.QName{..} = maybe "" (pretty . qName_key_name) qName_key
