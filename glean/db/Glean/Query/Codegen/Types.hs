{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveTraversable #-}
module Glean.Query.Codegen.Types
  ( Match(..)
  , matchVar
  , Var(..)
  , CodegenQuery
  , QueryWithInfo(..)
  , CgQuery(..)
  , CgStatement_(..)
  , CgStatement
  , Generator_(..)
  , Generator
  , SeekSection(..)
  , Pat
  , Expr
  , PrimOp(..)
  , Typed(..)
  , Output
  , TransformAndBind(..)
  ) where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Glean.Angle.Types (IsWild(..), PrimOp(..), SeekSection(..))
import Glean.Bytecode.Types (Register, Ty(..))
import Glean.Display
import Glean.RTS.Term hiding (Match)
import Glean.RTS.Types (Type, Fid, PidRef)

-- | A Query with flat generators, ready for compilation
type CodegenQuery = QueryWithInfo CgQuery

data QueryWithInfo q = QueryWithInfo
  { qiQuery :: q
  , qiNumVars :: Int    -- ^ maximum index of any Var + 1
  , qiReturnType :: Type
  }

-- -----------------------------------------------------------------------------
-- Flattened Query types

-- | A query produces a finite series of terms
data CgQuery = CgQuery
  { -- | For each result produced by the query body, build this
    -- term. It cannot contain 'MatchWild' or 'MatchBind'.
    flatQueryHead :: Expr

    -- | A sequence of statements, like a list
    -- comprehension. Variables bound by each statement scope over the
    -- statements that follow it.
  , flatQueryBody :: [CgStatement]
  }
  deriving Show

-- | A statement is either a single generator:
--
-- >  pat = gen
--
-- or a disjunction:
--
-- > (stmt; ...) | ... | (stmt; ...)
--
data CgStatement_ var
  = CgStatement (Pat_ var) (Generator_ var)
  | CgNegation [CgStatement_ var]
  | CgDisjunction [[CgStatement_ var]]
  -- ^ For rationale, see Note [why do we have sequential composition?]
  | CgConditional
    { cond :: [CgStatement_ var]
    , then_ :: [CgStatement_ var]
    , else_ :: [CgStatement_ var]
    }
  deriving (Show, Functor, Foldable, Traversable)


type CgStatement = CgStatement_ Var

{- Note [why do we have sequential composition?]

The issue is that queries for sum types can't necessarily be handled
by nested generators. Consider

  v = cxx1.FunctionName (name(cxx1.Name "xyz" ) | operator(cxx1.Name "+"))

If we flattened this into nested generators we would get

  x = cxx1.Name "xyz"
  y = cxx1.Name "+"
  z = cxx1.FunctionName (name x | operator y)

Now suppose there is no name xyz. This query will match nothing,
because the generator for cxx1.Name "xyz" would be empty.  (even if
the generator matched, flattening out the generators like this will
test too many combinations and do too much work).

With sequential composition of queries we can do it like this:

 n = (name x where x = cxx1.Name "xyz") |
     (operator x where x = cxx1.Name "+")
 v = cxx1.FunctionName n

(Note that this query won't work if you write it because we can't
typecheck the sub-query "name x where ...", but we can generate the
AST for it in the JSON query compiler.)
-}

type Generator = Generator_ Var

-- | A generator produces a finite series of terms
data Generator_ var
    -- | Enumerate facts of a predicate matching a pattern
    --
    -- > v = pred pat
    -- > ...
    --
    -- The left-hand side can be any pattern that matches the fact ID,
    -- so for example to look up a specific fact we can do
    --
    -- > 1234 = pred _
    --
  = FactGenerator
    { -- | Predicate to search
      generatorPid :: PidRef

      -- | match the fact key
    , generatorPat :: Pat_ var

      -- | match the fact value
    , generatorValue :: Pat_ var

      -- | restrict the range of fact ids this generator can produce.
    , generatorSeekSection :: SeekSection
    }

    -- | Generate values
  | TermGenerator (Expr_ var)

    -- | Produces facts of a derived predicate. The values of
    -- this generator are fact IDs that refer to facts in the
    -- temporary Environment produced by a query.
  | DerivedFactGenerator
    { generatorPid :: PidRef
    , generatorKey :: Expr_ var
    , generatorValue :: Expr_ var
    }

    -- | Produces all elements of an array
  | ArrayElementGenerator
    { generatorEltTy :: Type        -- type of the elements
    , generatorArray :: Expr_ var
    }

  | PrimCall
    { primOp :: PrimOp
    , primOpArgs :: [Expr_ var]
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Pat = Pat_ Var
type Expr = Expr_ Var

type Pat_ var = Term (Match () var)
type Expr_ var = Term (Match () var)

type Output = Typed (Register 'BinaryOutputPtr)

data Var = Var
  { varType :: Type
  , varId :: {-# UNPACK #-}!Int
  , varOrigName :: Maybe Text
  } deriving Show

instance Eq Var where
  Var _ x _ == Var _ y _  = x == y

instance Display Var where
  display _ (Var _ v nm) = pretty (fromMaybe "" nm) <> "_" <> pretty v

instance IsWild (Term (Match ext v)) where
  isWild (Ref MatchWild{}) = True
  isWild _ = False

data Match ext var
    -- | Always matches
  = MatchWild Type

    -- | Never matches
  | MatchNever Type

    -- | Match a literal Fact ID
  | MatchFid Fid

    -- | Match a value and bind it to this variable
  | MatchBind var

    -- | Match a value bound earlier. Note that this means we can have
    -- patterns like pred(X,X), where the second X matches the value
    -- bound by the first X.
  | MatchVar var

    -- | Match two patterns simultaneously. Fails if either of them
    -- fails to match, and binds the MatchBinds from both sides if
    -- both patterns match. Not valid in expressions, only patterns.
  | MatchAnd (Term (Match ext var)) (Term (Match ext var))

    -- | Match a prefix of a string.
  | MatchPrefix
      ByteString  -- the prefix of the string (utf-8 encoded)
      (Term (Match ext var))  -- the rest of the string

  | MatchArrayPrefix
      Type                     -- ^ The type of elements
      [Term (Match ext var)]   -- ^ The prefix

    -- | placeholder for extending this type
  | MatchExt ext

  deriving (Eq, Functor, Foldable, Traversable, Show)

-- | Bind a value of one type into an output of a different type by first
-- performing a type transformation.
data TransformAndBind
  = TransformAndBind Type Output
  deriving Show

instance Bifunctor Match where
  bimap f g = \case
    MatchWild x -> MatchWild x
    MatchNever x -> MatchNever x
    MatchFid x -> MatchFid x
    MatchExt ext -> MatchExt (f ext)
    MatchVar var -> MatchVar (g var)
    MatchBind var -> MatchBind (g var)
    MatchAnd a b -> MatchAnd (fmap (bimap f g) a) (fmap (bimap f g) b)
    MatchPrefix b term -> MatchPrefix b $ fmap (bimap f g) term
    MatchArrayPrefix ty prefix ->
      MatchArrayPrefix ty ((fmap.fmap) (bimap f g) prefix)

instance Bifoldable Match where
  bifoldMap f g = \case
    MatchWild _ -> mempty
    MatchNever _ -> mempty
    MatchFid _ -> mempty
    MatchExt ext -> f ext
    MatchVar var -> g var
    MatchBind var -> g var
    MatchAnd a b -> foldMap (bifoldMap f g) a <> foldMap (bifoldMap f g) b
    MatchPrefix _ term -> foldMap (bifoldMap f g) term
    MatchArrayPrefix _ty pre ->
      (foldMap.foldMap) (bifoldMap f g) pre

instance Bitraversable Match where
  bitraverse f g = \case
    MatchWild x -> pure $ MatchWild x
    MatchNever x -> pure $ MatchNever x
    MatchFid x -> pure $ MatchFid x
    MatchExt ext -> MatchExt <$> f ext
    MatchVar var -> MatchVar <$> g var
    MatchBind var -> MatchBind <$> g var
    MatchAnd a b -> MatchAnd
      <$> traverse (bitraverse f g) a
      <*> traverse (bitraverse f g) b
    MatchPrefix b term -> MatchPrefix b <$> traverse (bitraverse f g) term
    MatchArrayPrefix ty prefix ->
      MatchArrayPrefix ty <$> traverse (traverse (bitraverse f g)) prefix

matchVar :: Match ext var -> Maybe var
matchVar (MatchVar v) = Just v
matchVar (MatchBind v ) = Just v
matchVar _ = Nothing

data Typed x = Typed Type x
  deriving (Show, Functor, Traversable, Foldable)

instance Display x => Display (Typed x) where
  display opts (Typed _ x) = display opts x

-- -----------------------------------------------------------------------------
-- Pretty-printing

instance Display CgQuery where
  display opts (CgQuery expr []) = display opts expr
  display opts (CgQuery expr stmts) =
    hang 2 $ sep $
      display opts expr <+>
        "where" : punctuate ";" (map (display opts) stmts)

instance Display CgStatement where
  display opts = \case
    CgStatement pat gen ->
      hang 2 $ sep [display opts pat <+> "=", display opts gen]
    CgNegation stmts -> "!" <> doStmts stmts
    CgDisjunction stmtss -> sep (punctuate " |" (map doStmts stmtss))
    CgConditional cond then_ else_ -> sep
      [ nest 2 $ sep ["if", doStmts cond ]
      , nest 2 $ sep ["then", doStmts then_]
      , nest 2 $ sep ["else", doStmts else_]
      ]
    where
      doStmts stmts = hang 2 $
        sep [sep ("(" : punctuate ";" (map (display opts) stmts)), ")"]

instance Display Generator where
  display opts (FactGenerator pref kpat vpat section)
    | isWild vpat || isUnit vpat = hang 2 (sep [pred, display opts kpat])
    | otherwise =
      hang 2 (sep [pred, display opts kpat <+> "->", display opts vpat])
    where
    pred = display opts pref <> prettySection
    prettySection = case section of
      SeekOnAllFacts -> ""
      SeekOnBase -> "<base>"
      SeekOnStacked -> "<stacked>"
    isUnit (Tuple []) = True
    isUnit _ = False
  display opts (TermGenerator q) = display opts q
  display opts (DerivedFactGenerator pid k (Tuple [])) =
    hang 2 (sep [display opts pid <> "<-", displayAtom opts k ])
  display opts (DerivedFactGenerator pid k v) =
    display opts pid <> "<- (" <>
      displayAtom opts k <> " -> " <> displayAtom opts v <> ")"
  display opts (ArrayElementGenerator _ arr) = display opts arr <> "[..]"
  display opts (PrimCall op args) =
    hsep (display opts op : map (displayAtom opts) args)

instance Display ext => Display (Match ext Var) where
  display opts (MatchAnd l r) = display opts l <+> "@" <+> display opts r
  display opts (MatchBind v@(Var ty _ _)) =
    display opts v  <> ":" <> display opts ty
  display opts (MatchPrefix str rest) =
    display opts (show str) <> ".." <> display opts rest
  display opts (MatchExt ext) = display opts ext
  display opts other = displayAtom opts other

  displayAtom _ (MatchWild _) = "_"
  displayAtom _ (MatchNever _) = "()"
  displayAtom _ (MatchFid fid) = pretty fid
  displayAtom opts (MatchVar v) = display opts v
  displayAtom opts (MatchPrefix str rest) =
    pretty (show str) <> ".." <> display opts rest
  displayAtom opts (MatchArrayPrefix _ty pre) =
    align $ encloseSep "[" "..]" "," $ map (display opts) pre
  displayAtom opts other@MatchAnd{} = parens (display opts other)
  displayAtom opts other@MatchBind{} = parens (display opts other)
  displayAtom opts (MatchExt ext) = displayAtom opts ext
