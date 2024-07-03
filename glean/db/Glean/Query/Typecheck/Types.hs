{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
import Glean.Display
import Glean.RTS.Types as RTS
import Glean.RTS.Term as RTS

-- | The typechecking phase turns 'ParsedPat' (source-level terms) into
-- 'TcPat' (representational terms), but it doesn't flatten nested
-- generators, so it leaves the query as a 'SourceQuery'.
data TcQuery = TcQuery Type TcPat (Maybe TcPat) [TcStatement]
  deriving Show

instance Display TcQuery where
  display opts (TcQuery _ key maybeVal stmts) = case stmts of
    [] -> head
    _ ->
      hang 2 $ sep $ head <+>
        "where" : punctuate ";" (map (display opts) stmts)
    where
   head = display opts key <>
     maybe mempty (\val -> " -> " <> display opts val) maybeVal

data TcStatement = TcStatement Type TcPat TcPat
  deriving Show

instance Display TcStatement where
  display opts (TcStatement _ lhs rhs) = displayStatement opts lhs rhs

type TcPat = Term (Match (Typed TcTerm) Var)

data TcTerm
  = TcOr TcPat TcPat
  | TcFactGen PidRef TcPat TcPat SeekSection
  | TcElementsOfArray TcPat
  | TcAll TcPat
  | TcQueryGen TcQuery
  | TcNegation [TcStatement]
  | TcPrimCall PrimOp [TcPat]
  | TcIf { cond :: Typed TcPat, then_ :: TcPat, else_ :: TcPat }
  deriving Show

instance Display TcTerm where
  display opts (TcOr a b) = display opts a <+> "++" <+> display opts b
  display opts (TcIf (Typed _ cond) then_ else_) = sep
    [ nest 2 $ sep ["if", displayAtom opts cond ]
    , nest 2 $ sep ["then", displayAtom opts then_]
    , nest 2 $ sep ["else", displayAtom opts else_]
    ]
  display opts (TcFactGen pid kpat vpat _range)
    | isWild vpat || isUnit vpat =
      nest 2 $ sep [display opts pid, displayAtom opts kpat]
    | otherwise =
      nest 2 $ sep
        [ display opts pid
        , displayAtom opts kpat
        , "->" <+> displayAtom opts vpat ]
    where
    isUnit (RTS.Tuple []) = True
    isUnit _ = False
  display opts (TcElementsOfArray arr) = displayAtom opts arr <> "[..]"
  display opts (TcAll set) = "all" <+> display opts set
  display opts (TcQueryGen q) = parens (display opts q)
  display opts (TcNegation q) =
    "!" <> parens (sep (punctuate ";" (map (display opts) q)))
  display opts (TcPrimCall op args) =
    hsep (display opts op : map (displayAtom opts) args)

  displayAtom opts pat = case pat of
    TcOr{} -> parens (display opts pat)
    TcFactGen{} -> parens (display opts pat)
    TcPrimCall{} -> parens (display opts pat)
    TcQueryGen{} -> parens (display opts pat)
    TcNegation{} -> display opts pat
    TcIf{} -> parens (display opts pat)
    _ -> display opts pat

type TypecheckedQuery = QueryWithInfo TcQuery
