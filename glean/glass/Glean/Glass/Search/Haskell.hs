{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Haskell
  ( {- instances -}
  ) where

import Data.Text ( Text )
import qualified Data.Text as Text ( intercalate )
import Util.Text

import Glean.Angle as Angle

import Glean.Glass.Search.Class

import qualified Glean.Schema.CodeHs.Types as Haskell
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Hs.Types as Hs

instance Search (ResultLocation Haskell.Entity) where
  symbolSearch [] = return $ None "Haskell.symbolSearch: empty"
  symbolSearch toks@(pkg : rest) = do
    case reverse rest of
      end : start : ident : namespace : mod
        | Right e <- textToInt end,
          Right s <- textToInt start,
          Just ns <- fromNamespace namespace ->
          searchSymbolId toks $
            symbolIdQuery pkg (Text.intercalate "." (reverse mod)) ident ns
              (Just (s,e))
      ident : namespace : mod
        | Just ns <- fromNamespace namespace ->
          searchSymbolId toks $
            symbolIdQuery pkg (Text.intercalate "." (reverse mod)) ident ns
              Nothing
      _ -> return $ None "Haskell.symbolSearch: empty"
    where
    fromNamespace "var" = Just Hs.Namespace_var_
    fromNamespace "ty" = Just Hs.Namespace_tycon
    fromNamespace "con" = Just Hs.Namespace_datacon
    fromNamespace "tyvar" = Just Hs.Namespace_tyvar
    fromNamespace _ = Nothing


symbolIdQuery
  :: Text  -- ^ package (ignored (TODO))
  -> Text  -- ^ module name
  -> Text  -- ^ identifier
  -> Hs.Namespace -- ^ namespace (var, datacon, tycon, tyvar)
  -> Maybe (Int, Int)  -- ^ span, for local names
  -> Angle (ResultLocation Haskell.Entity)
symbolIdQuery _pkg mod ident ns sort =
  vars $ \name file span ->
    tuple (name, file, sig (alt @"span" span :: Angle Code.RangeSpan), string ident)
      `where_` [
        name .= predicate @Hs.Name (
          rec $
            field @"occ" (rec $
              field @"name" (string ident) $
              field @"namespace_" (enum ns) end) $
            field @"mod" (rec $
              field @"name" (string mod) end) $
            field @"sort" (
              case sort of
                Nothing -> alt @"external" wild
                Just (s,l) -> alt @"internal" (rec $
                  field @"start" (nat (fromIntegral s)) $
                  field @"length" (nat (fromIntegral l)) end)
            ) end),
        stmt $ predicate @Hs.DeclarationLocation (
          rec $
            field @"name" (asPredicate name) $
            field @"file" (asPredicate file) $
            field @"span" span end
        )
    ]
