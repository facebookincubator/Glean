{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Pretty.Hs (
  ) where

import Data.Text.Prettyprint.Doc

import Glean.Pretty.Src ()
import Glean.Schema.CodeHs.Types as Hs
import Glean.Schema.Hs.Types as Hs

instance Pretty Hs.Entity where
  pretty (Hs.Entity_definition defn) = pretty defn
  pretty (Hs.Entity_function_ decl) = pretty decl
  pretty (Hs.Entity_class_ _cls) = mempty -- TODO

instance Pretty Hs.Definition where
  pretty Definition{..}
    | Just
        Definition_key
          { definition_key_name =
            DefinitionName{definitionName_key = defName}
          , ..
          } <-
        definition_key =
      pretty defName <+> "at"
        <+> pretty definition_key_source
    | otherwise = ""

instance Pretty Hs.FunctionDefinition where
  pretty FunctionDefinition{..}
    | Just FunctionDefinition_key{..} <- functionDefinition_key =
      pretty functionDefinition_key_name <+> "at"
        <+> pretty functionDefinition_key_source
    | otherwise = ""

instance Pretty Hs.FunctionName where
  pretty FunctionName{..}
    | Just name <- functionName_key = pretty name
    | otherwise = "{" <> pretty functionName_id <> "}"
