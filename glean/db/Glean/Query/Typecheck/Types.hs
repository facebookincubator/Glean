-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Query.Typecheck.Types
  ( TypecheckedQuery
  , TcQuery(..)
  , TcStatement(..)
  , Typed(..)
  , TcPat
  , TcTerm(..)
  , mapExt
  ) where

import Data.Text.Prettyprint.Doc hiding ((<>), enclose)

import Glean.Query.Codegen
import Glean.Query.Types as Parser
import Glean.RTS.Types as RTS
import Glean.RTS.Term as RTS hiding (Match(..))

-- | The typechecking phase turns 'ParsedPat' (source-level terms) into
-- 'TcPat' (representational terms), but it doesn't flatten nested
-- generators, so it leaves the query as a 'SourceQuery'.
data TcQuery = TcQuery Type TcPat (Maybe TcPat) [TcStatement]

instance Pretty TcQuery where
  pretty (TcQuery _ key maybeVal stmts) = case stmts of
    [] -> head
    _ -> hang 2 (sep (head <+> "where" : punctuate ";" (map pretty stmts)))
    where
   head = pretty key <> maybe mempty (\val -> " -> " <> pretty val) maybeVal

data TcStatement = TcStatement Type TcPat TcPat

instance Pretty TcStatement where
  pretty (TcStatement _ lhs rhs) = pretty (SourceStatement lhs rhs)

data Typed x = Typed Type x

instance Pretty x => Pretty (Typed x) where
  pretty (Typed _ x) = pretty x

type TcPat = Term (Match (Typed TcTerm) Var)

data TcTerm
  = TcOr TcPat TcPat
  | TcFactGen PidRef TcPat TcPat
  | TcElementsOfArray TcPat
  | TcQueryGen TcQuery
  | TcPrimCall PrimOp [TcPat]

mapExt :: (ext -> ext') -> Match ext var -> Match ext' var
mapExt f t = case t of
  MatchWild ty -> MatchWild ty
  MatchNever ty -> MatchNever ty
  MatchFid fid -> MatchFid fid
  MatchBind v -> MatchBind v
  MatchVar v -> MatchVar v
  MatchAnd a b -> MatchAnd (fmap (mapExt f) a) (fmap (mapExt f) b)
  MatchPrefix str t -> MatchPrefix str (fmap (mapExt f) t)
  MatchSum alts -> MatchSum $ map (fmap (fmap (mapExt f))) alts
  MatchExt ext -> MatchExt (f ext)

instance Pretty TcTerm where
  pretty (TcOr a b) = pretty a <+> "++" <+> pretty b
  pretty (TcFactGen pid kpat vpat)
    | isWild vpat = pretty pid <+> prettyArg kpat
    | otherwise = pretty pid <+> prettyArg kpat <+> "->" <+> prettyArg vpat
  pretty (TcElementsOfArray arr) = prettyArg arr <> "[..]"
  pretty (TcQueryGen q) = parens (pretty q)
  pretty (TcPrimCall op args) = hsep (pretty op : map prettyArg args)

prettyArg :: TcPat -> Doc ann
prettyArg pat = case pat of
  Ref (MatchExt (Typed _ TcOr{})) -> parens (pretty pat)
  Ref (MatchExt (Typed _ TcFactGen{})) -> parens (pretty pat)
  Ref (MatchExt (Typed _ TcPrimCall{})) -> parens (pretty pat)
  Ref (MatchExt (Typed _ TcQueryGen{})) -> parens (pretty pat)
  _ -> pretty pat

type TypecheckedQuery = QueryWithInfo TcQuery
