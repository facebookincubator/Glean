{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.SymbolId.Cxx.Parse (
    validateSymbolId,
    SymbolEnv(..),
    SymbolTag(..),
    Name(..)
  ) where

import Data.Text ( Text )
import qualified Data.Text as Text
import Control.Monad.State.Strict
import Util.Text ( textShow )

-- "lexer"

newtype Name = Name Text
  deriving (Eq, Show)

-- | Tokenize each fragment
data Token
  = TName !Name
  | TDecl
  | TCtor
  | TDtor
  | TCtorSignature
  deriving Show

tokenize :: Text -> Token
tokenize ".decl" = TDecl
tokenize ".ctor" = TCtor
tokenize ".dtor" = TDtor
tokenize ".c" = TCtorSignature
tokenize n = TName (Name $ Text.replace "+" " " n)

-- "parser"

type Parse a = State SymbolEnv a

-- | Process the symbol id left to right accumulating information
data SymbolEnv = SymbolEnv {
  path :: Text,
  scopes :: [Name], -- accumulated scope terms
  localname :: Maybe Name, -- the local name of the identifier
  declaration :: Bool, -- .decl tag occurs
  tag :: Maybe SymbolTag,
  params :: [Name], -- maybe parameter signature
  errors :: [Text] -- any errors we find
} deriving (Eq, Show)

-- Any tags that help to classify the sort of symbol we have
data SymbolTag
  = Constructor
  | CTorSignature -- .ctor with type signature of params
  | Destructor
  deriving (Eq, Show)

initState :: Text -> SymbolEnv
initState p = SymbolEnv {
    path = p,
    scopes = mempty,
    localname = Nothing,
    declaration = False,
    tag = Nothing,
    params = [],
    errors = []
  }

pushScope :: Name -> Parse ()
pushScope s = modify' $ \env -> env { scopes = s : scopes env }

pushParam :: Name -> Parse ()
pushParam s = modify' $ \env -> env { params = s : params env }

setName :: Name -> Parse ()
setName n = modify' $ \env -> env { localname = Just n }

setTag :: SymbolTag -> Parse ()
setTag t = modify' $ \env -> env { tag = Just t }

setDecl :: Parse ()
setDecl = modify' $ \env -> env { declaration = True }

setErr :: Text -> Parse ()
setErr s = modify' $ \env -> env { errors = s : errors env }

--
-- Parse the symbol id term
--

validateSymbolId :: [Text] -> Either [Text] SymbolEnv
validateSymbolId toks = case toks of
  [] -> Left ["Cxx.parseSymbolId: empty symbol specification"]
  [_] -> Left ["Cxx.parseSymbolId: incomplete symbol specification"]
  path:name:rest ->
    let env = execState
               (parseOneName (tokenize name) (map tokenize rest))
               (initState path)
    in case errors env of
        [] -> Right $ env { scopes = reverse (scopes env)
                          , params = reverse (params env)
                          }
        errs -> Left errs

-- | at least one name is required
parseOneName :: Token -> [Token] -> Parse ()
parseOneName (TName name) rest = parseScopeOrName name rest
parseOneName tok _ = setErr $
  "Cxx.parseScopedSymbol: expected identifier, found tag:" <> textShow tok

-- | scope* name (tag*)
parseScopeOrName :: Name -> [Token] -> Parse ()
parseScopeOrName name [] = setName name -- last identifier is always name
parseScopeOrName name (n : ns) = case n of
  TName n -> pushScope name >> parseScopeOrName n ns
  TCtor -> pushScope name >> setTag Constructor >> parseCtor ns
  TDtor -> pushScope name >> setTag Destructor >> parseDtor ns
  TCtorSignature -> pushScope name >> setTag CTorSignature >> parseCtorSig ns
  TDecl -> do
    setName name >> setDecl
    case ns of
      [] -> pure ()
      _ -> setErr $
        "Cxx.parseScopedSymbol: unexpected tokens after .decl in symbol id: "
          <> textShow ns

-- | following /.ctor/ can only be:
--
-- > name
-- > name / .decl
-- > .
--
parseCtor :: [Token] -> Parse ()
parseCtor [] = return () -- ctor definition occurence
parseCtor [TDecl] = setDecl -- a ctor decl
parseCtor [TName name, TDecl] = setName name >> setDecl -- ctor parameter decl
parseCtor rest = setErr $
  "Cxx.parseCtor: unexpected trailing tokens in .ctor signature: " <>
    textShow rest

parseDtor :: [Token] -> Parse ()
parseDtor [] = return ()
parseDtor [TDecl] = setDecl
parseDtor rest = setErr $
  "Cxx.parseDtor: unexpected trailing tokens in .dtor signature: " <>
    textShow rest

parseCtorSig :: [Token] -> Parse ()
-- constructor with no params
parseCtorSig [] = return ()
-- one or more param signatures
parseCtorSig [TName (Name name)] = mapM_ (pushParam . Name) params
  where
    params = Text.splitOn "," name
-- nullary construct decl
parseCtorSig [TDecl] = setDecl
-- one or more param signatures decl
parseCtorSig [TName (Name name), TDecl] =
    mapM_ (pushParam . Name) params >> setDecl
  where
    params = Text.splitOn "," name
parseCtorSig rest = setErr $
  "Cxx.parseCtorSig: unexpected trailing tokens in .ctor signature: " <>
    textShow rest
