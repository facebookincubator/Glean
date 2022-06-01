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

import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Glean.Pretty.Src ()
import Glean.Schema.CodeJava.Types as Java
import Glean.Schema.Java.Types as Java
import Glean.Util.Range (locRange)
import Glean.Util.URI


intentionallyEmpty :: Doc a
intentionallyEmpty = pretty ("" :: Text)

instance Pretty Java.Entity where
  pretty (Java.Entity_class_ decl) = pretty decl
  pretty (Java.Entity_definition_ def) = pretty def
  pretty Java.Entity_EMPTY = intentionallyEmpty

instance Pretty Java.Definition where
  pretty (Java.Definition_class_ decl) = pretty decl
  pretty (Java.Definition_enum_ decl) = pretty decl
  pretty (Java.Definition_interface_ decl) = pretty decl
  pretty Java.Definition_EMPTY  = intentionallyEmpty

instance Pretty Java.ClassDeclaration where
  pretty Java.ClassDeclaration{..} = maybe "" pretty classDeclaration_key

instance Pretty Java.ClassDeclaration_key where
  pretty Java.ClassDeclaration_key{..} = vsep
    [ "class" <+> pretty classDeclaration_key_name
    , "at" <+> pretty classDeclaration_key_loc
    , pretty $ show <$>
        fbsDiffusionURIForRange (locRange classDeclaration_key_loc)
    ]

instance Pretty Java.EnumDeclaration where
  pretty Java.EnumDeclaration{..} = maybe "" pretty enumDeclaration_key

instance Pretty Java.EnumDeclaration_key where
  pretty Java.EnumDeclaration_key{..} = vsep
    [ "enum" <+> pretty enumDeclaration_key_name
    , "at" <+> pretty enumDeclaration_key_loc
    , pretty $ show <$>
        fbsDiffusionURIForRange (locRange enumDeclaration_key_loc)
    ]
instance Pretty Java.InterfaceDeclaration where
  pretty Java.InterfaceDeclaration{..} = maybe "" pretty interfaceDeclaration_key

instance Pretty Java.InterfaceDeclaration_key where
  pretty Java.InterfaceDeclaration_key{..} = vsep
    [ "interface" <+> pretty interfaceDeclaration_key_name
    , "at" <+> pretty interfaceDeclaration_key_loc
    , pretty $ show <$>
        fbsDiffusionURIForRange (locRange interfaceDeclaration_key_loc)
    ]

instance Pretty Java.Name where
  pretty Java.Name{..} = maybe "" pretty name_key

instance Pretty Java.QName where
  pretty Java.QName{..} = maybe "" (pretty . qName_key_name) qName_key
