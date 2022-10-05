{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveTraversable #-}
module Glean.Query.Typecheck.Types
  ( TypecheckedQuery
  , TcQuery(..)
  , TcStatement(..)
  , TcPat
  , TcTerm(..)
  ) where

import Data.Text.Prettyprint.Doc hiding ((<>), enclose)

import Glean.Query.Codegen.Types
  (Match(..), Var(..), QueryWithInfo(..), Typed(..))
import Glean.Angle.Types hiding (Type)
import Glean.RTS.Types as RTS
import Glean.RTS.Term as RTS hiding (Match(..))

-- | The typechecking phase turns 'ParsedPat' (source-level terms) into
-- 'TcPat' (representational terms), but it doesn't flatten nested
-- generators, so it leaves the query as a 'SourceQuery'.
data TcQuery = TcQuery Type TcPat (Maybe TcPat) [TcStatement]
  deriving Show

instance Pretty TcQuery where
  pretty (TcQuery _ key maybeVal stmts) = case stmts of
    [] -> head
    _ -> hang 2 (sep (head <+> "where" : punctuate ";" (map pretty stmts)))
    where
   head = pretty key <> maybe mempty (\val -> " -> " <> pretty val) maybeVal

data TcStatement = TcStatement Type TcPat TcPat
  deriving Show

instance Pretty TcStatement where
  pretty (TcStatement _ lhs rhs) = prettyStatement lhs rhs

type TcPat = Term (Match (Typed TcTerm) Var)

data TcTerm
  = TcOr TcPat TcPat
  | TcFactGen PidRef TcPat TcPat
  | TcElementsOfArray TcPat
  | TcQueryGen TcQuery
  | TcNegation [TcStatement]
  | TcPrimCall PrimOp [TcPat]
  | TcIf { cond :: Typed TcPat, then_ :: TcPat, else_ :: TcPat }
  deriving Show

instance Pretty TcTerm where
  pretty (TcOr a b) = pretty a <+> "++" <+> pretty b
  pretty (TcIf (Typed _ cond) then_ else_) = sep
    [ nest 2 $ sep ["if", prettyArg cond ]
    , nest 2 $ sep ["then", prettyArg then_]
    , nest 2 $ sep ["else", prettyArg else_]
    ]
  pretty (TcFactGen pid kpat vpat)
    | isWild vpat = pretty pid <+> prettyArg kpat
    | otherwise = pretty pid <+> prettyArg kpat <+> "->" <+> prettyArg vpat
  pretty (TcElementsOfArray arr) = prettyArg arr <> "[..]"
  pretty (TcQueryGen q) = parens (pretty q)
  pretty (TcNegation q) = "!" <> parens (pretty q)
  pretty (TcPrimCall op args) = hsep (pretty op : map prettyArg args)

prettyArg :: TcPat -> Doc ann
prettyArg pat = case pat of
  Ref (MatchExt (Typed _ TcOr{})) -> parens (pretty pat)
  Ref (MatchExt (Typed _ TcFactGen{})) -> parens (pretty pat)
  Ref (MatchExt (Typed _ TcPrimCall{})) -> parens (pretty pat)
  Ref (MatchExt (Typed _ TcQueryGen{})) -> parens (pretty pat)
  Ref (MatchExt (Typed _ TcNegation{})) -> pretty pat
  Ref (MatchExt (Typed _ TcIf{})) -> parens (pretty pat)
  _ -> pretty pat

type TypecheckedQuery = QueryWithInfo TcQuery
