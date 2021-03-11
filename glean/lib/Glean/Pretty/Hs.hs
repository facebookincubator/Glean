-- Copyright 2004-present Facebook. All Rights Reserved.
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Pretty.Hs
  (
  ) where

import Data.Text.Prettyprint.Doc

import Glean.Schema.CodeHs.Types as Hs
import Glean.Schema.Hs.Types as Hs
import Glean.Pretty.Src ()

instance Pretty Hs.Entity where
  pretty (Hs.Entity_function_ decl) = pretty decl
  pretty (Hs.Entity_class_ _cls) = mempty -- TODO

instance Pretty Hs.FunctionDefinition where
  pretty FunctionDefinition{..}
    | Just FunctionDefinition_key{..} <- functionDefinition_key =
      pretty functionDefinition_key_name <+> "at" <+>
        pretty functionDefinition_key_source
    | otherwise = ""

instance Pretty Hs.FunctionName where
  pretty FunctionName{..}
    | Just name <- functionName_key = pretty name
    | otherwise = "{" <> pretty functionName_id <> "}"
