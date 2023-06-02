{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveGeneric #-}

module Glean.Glass.SymbolId.Cxx.Parse (
    validateSymbolId,
    compileSymbolEnv, -- valid symbol ids can be compiled to a search term
    SymbolEnv(..),
    SymbolTag(..),
    Name(..),
    Qualifier(..),
    RefQualifier(..),
    unName,
    CxxSymbolExpr(..)

    -- testing
    , toQualifier
  ) where

import Control.Monad.State.Strict
import Data.Aeson.Types ( ToJSON )
import Data.Set ( Set )
import Data.Text ( Text )
import GHC.Generics
import Util.Text ( textShow )
import qualified Data.Set as Set
import qualified Data.Text as Text

-- "lexer"

-- | Tokenize each fragment
data Token
  = TName !Text
  | TDecl
  | TCtor
  | TDtor
  | TCtorSignature
  | TFunction
  deriving Show

tokenize :: Text -> Token
tokenize ".decl" = TDecl
tokenize ".ctor" = TCtor
tokenize ".dtor" = TDtor
tokenize ".c" = TCtorSignature
tokenize ".f" = TFunction
tokenize n = TName n

name :: Text -> Name
name = Name . Text.replace "+" " "

-- | in type signatures literal "," (as in std::pair<a, b>) is replaced with " "
-- to avoid the "," param seperator
sig :: Text -> Name
sig = name . Text.replace " " ","

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
  qualifiers :: Set Qualifier, -- optional list of qualifiers (const, &&, etc)
  errors :: [Text] -- any errors we find
} deriving (Eq, Ord, Show, Generic)

newtype Name = Name Text
  deriving (Eq, Ord, Show, Generic)

data Qualifier
  = Virtual
  | Const
  | Volatile
  | RefQual RefQualifier
  deriving (Eq, Ord, Show, Generic)

data RefQualifier = LValue | RValue
  deriving (Eq, Ord, Show, Generic)

-- standalone to avoid having it appear in the generic JSON writer
unName :: Name -> Text
unName (Name n) = n

-- Any tags that help to classify the sort of symbol we have
-- These will be variants of an ADT
data SymbolTag
  = Constructor
  | CTorSignature -- .ctor with type signature of params
  | Destructor
  | Function
  deriving (Eq, Ord, Show, Generic)

-- for regression testing
instance ToJSON Name
instance ToJSON SymbolTag
instance ToJSON SymbolEnv
instance ToJSON Qualifier
instance ToJSON RefQualifier

-- Compile SymbolEnv to a typed ADT that makes bugs harder
data CxxSymbolExpr
  = CxxConstructor {
      cPath :: Text,
      cScope :: [Name],
      cParams :: [Name],
      cDecl :: Bool
    }
  | CxxDestructor {
      cPath :: Text,
      cScope :: [Name],
      cDecl :: Bool
    }
  | CxxFunction {
      cPath :: Text,
      cScope :: [Name],
      cName :: Name,
      cParams :: [Name],
      cQuals :: Set Qualifier,
      cDecl :: Bool
    }
  -- Legacy: constructor params (TODO replace with general local var case)
  | CxxLegacyCTorParams {
      cPath :: Text,
      cScope :: [Name],
      cName :: Name,
      cDecl :: Bool
    }
  -- Fall back: any symbol
  | CxxAny {
      cPath :: Text,
      cScope :: [Name],
      cName :: Name,
      cDecl :: Bool
    }

--
-- Refine the parsed symbol tokens into a more precise symbol id query ADT
--
compileSymbolEnv :: SymbolEnv -> Either Text CxxSymbolExpr
compileSymbolEnv env@SymbolEnv{..} = case tag of
  Just CTorSignature ->
    Right $ CxxConstructor { -- .c
        cPath = path,
        cScope = scopes,
        cParams = params,
        cDecl = declaration
    }
  Just Destructor -> -- .dtor
    Right $ CxxDestructor {
        cPath = path,
        cScope = scopes,
        cDecl = declaration
    }
  Just Function  -- .f
    | Just name <- localname ->
      Right $ CxxFunction {
          cPath = path,
          cScope = scopes,
          cName = name,
          cParams = params,
          cQuals = qualifiers,
          cDecl = declaration
      }
    | otherwise ->
      Left $ "compileSymbolEnv: Function missing local name: " <> textShow env
  Just Constructor -- legacy .ctor params, aka local vars in ctor scope
    | Just name <- localname ->
    Right $ CxxLegacyCTorParams {
        cPath = path,
        cScope = scopes,
        cName = name,
        cDecl = declaration
    }
    | otherwise ->
      Left "compileSymbolEnv: constructor params missing local name"
  Nothing
    | Just name <- localname ->
      Right $ CxxAny {
          cPath = path,
          cScope = scopes,
          cName = name,
          cDecl = declaration
      }
    | otherwise ->
      Left "compileSymbolEnv: CxxAny: missing local name for entity"

initState :: Text -> SymbolEnv
initState p = SymbolEnv {
    path = p,
    scopes = mempty,
    localname = Nothing,
    declaration = False,
    tag = Nothing,
    params = [],
    qualifiers = mempty,
    errors = []
  }

pushScope :: Text -> Parse ()
pushScope s = modify' $ \env -> env { scopes = name s : scopes env }

pushParam :: Text -> Parse ()
pushParam s = modify' $ \env -> env { params = sig s : params env }

setName :: Text -> Parse ()
setName n = modify' $ \env -> env { localname = Just (name n) }

pushQualifier :: Qualifier -> Parse ()
pushQualifier q = modify' $ \env ->
    env { qualifiers = Set.insert q (qualifiers env)
  }

setTag :: SymbolTag -> Parse ()
setTag t = modify' $ \env -> env { tag = Just t }

setDecl :: Parse ()
setDecl = modify' $ \env -> env { declaration = True }

setErr :: Text -> Parse ()
setErr s = modify' $ \env -> env { errors = s : errors env }

--
-- | Parse the symbol id term
--
validateSymbolId :: [Text] -> Either [Text] SymbolEnv
validateSymbolId toks = case toks of
  [] -> Left ["Cxx.parseSymbolId: empty symbol"]
  [_] -> Left ["Cxx.parseSymbolId: incomplete symbol:" <> textShow toks]
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
parseScopeOrName :: Text -> [Token] -> Parse ()
parseScopeOrName name [] = setName name -- last identifier is always name
parseScopeOrName name (n : ns) = case n of
  TName n -> pushScope name >> parseScopeOrName n ns
  TCtor -> pushScope name >> setTag Constructor >> parseCtor ns
  TDtor -> pushScope name >> setTag Destructor >> parseDtor ns
  TCtorSignature -> pushScope name >> setTag CTorSignature >> parseCtorSig ns
  TFunction -> setName name >> setTag Function >> parseFunctionSig ns
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
parseCtor [TDecl] = setDecl -- a ctor decl . subsumed by CtorSig.
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
parseCtorSig [TName name] = mapM_ pushParam params
  where
    params = splitCommas name

-- nullary construct decl
parseCtorSig [TDecl] = setDecl

-- one or more param signatures decl
parseCtorSig [TName name, TDecl] =
    mapM_ pushParam params >> setDecl
  where
    params = splitCommas name
parseCtorSig rest = setErr $
  "Cxx.parseCtorSig: unexpected trailing tokens in .ctor signature: " <>
    textShow rest

--
-- Function signatures always have a return type
--
parseFunctionSig :: [Token] -> Parse ()
-- nullary definition
parseFunctionSig [] = pure ()
-- nullary construct decl
parseFunctionSig [TDecl] =
  setDecl
parseFunctionSig [TName name] = do -- single set of params (or trailing "/")
  mapM_ pushParam (splitCommas name)
parseFunctionSig [TName name, TDecl] = do -- decl variant
  mapM_ pushParam (splitCommas name) >> setDecl

parseFunctionSig [TName name, TName quals] = do -- set of params and quals
  mapM_ pushParam (splitCommas name)
  mapM_ parseQualifier (splitCommas quals)
parseFunctionSig [TName name, TName quals, TDecl] = do -- decl variant
  mapM_ pushParam (splitCommas name)
  mapM_ parseQualifier (splitCommas quals)
  setDecl

parseFunctionSig rest = setErr $
  "Cxx.parseCtorSig: unexpected trailing tokens in .ctor signature: " <>
    textShow rest

-- | type signatures are a single token separated by commas
splitCommas :: Text -> [Text]
splitCommas "" = []
splitCommas xs = Text.splitOn "," xs

parseQualifier :: Text -> Parse ()
parseQualifier s = case toQualifier s of
  Just x -> pushQualifier x
  Nothing -> setErr $ "parseQualifier: invalid qualifier: " <> s

-- | Parse cv qualfier encodings
toQualifier :: Text -> Maybe Qualifier
toQualifier s = case s of
  "virtual" -> Just Virtual
  "const" -> Just Const
  "volatile" -> Just Volatile
  "lvalue" -> Just (RefQual LValue)
  "rvalue" -> Just (RefQual RValue)
  _ -> Nothing
