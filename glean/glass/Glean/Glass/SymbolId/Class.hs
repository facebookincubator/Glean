{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}

module Glean.Glass.SymbolId.Class (
  -- * classes
    Symbol(..)
  , SymbolError(..)

  -- ** qualified names
  , ToQName(..)

  -- ** labels and other attributes
  , ToSymbolParent(..)

  -- * for predicate types
  , toSymbolPredicate
  , SymbolKeyType

  -- * builders
  , (<:>)
  , symbolPairToQName

  ,ToNativeSymbol(..)) where

import Data.Text (Text, intercalate)
import Control.Exception ( Exception(..) )
import Data.Typeable ( Typeable )

import qualified Haxl.Core.Exception as Haxl

import Glean ( keyOf, Predicate(KeyType) )
import qualified Glean.Haxl.Repos as Glean
import Glean.Glass.Types as Glass ( Name(Name), Path(..) )

--
-- Codex-like qname ids for Hack, Flow and Python
--
-- This creates a dependency in Glass on the Entity schema structure.
--
-- * move into an Entity-based search layer, something like
-- >      nameToEntity { Name, QName, Entity }
-- >      entityToName { Entity, Name, QName }
-- >      entityToDeclaration { Entity, Declaration }
--
-- * round-trip encode/decode
--
-- Encode/decode should work
--

newtype SymbolError = SymbolError Text
  deriving Show

instance Exception SymbolError where
  toException = Haxl.logicErrorToException
  fromException = Haxl.logicErrorFromException

-- | An encoded Entity.
--
-- e.g. Glean/getRepoName -- method
--      GleanRecursive   -- enum
--
class Symbol a where
  toSymbol :: a -> Glean.RepoHaxl u w [Text]

  toSymbolWithPath :: a ->  Glass.Path -> Glean.RepoHaxl u w [Text]
  toSymbolWithPath entity _ = toSymbol entity

-- | Symbols that have qualified names can be searched
class Symbol a => ToQName a where
  toQName :: a -> Glean.RepoHaxl u w (Either Text (Name, Name))

-- first level parent identifier
class ToSymbolParent a where
  toSymbolParent :: a -> Glean.RepoHaxl u w (Maybe Name)

class ToNativeSymbol a where
  toNativeSymbol :: a -> Glean.RepoHaxl u w (Maybe Text)

type SymbolKeyType p =
  ( Typeable p
  , Typeable (KeyType p)
  , Show p
  , Show (KeyType p)
  , Predicate p
  , Symbol (KeyType p)
  )

-- | Generically traverse predicates for keys
toSymbolPredicate :: (SymbolKeyType p) => p -> Glean.RepoHaxl u w [Text]
toSymbolPredicate k = Glean.keyOf k >>= toSymbol

-- TODO: use a short string builder
(<:>) :: (Symbol a, Symbol b) => a -> b -> Glean.RepoHaxl u w [Text]
container <:> name = do
  xs <- toSymbol container
  x <- toSymbol name
  return $ xs ++ x

-- Useful generic instannce
instance Symbol a => Symbol (Maybe a) where
  toSymbol Nothing = return []
  toSymbol (Just a) = toSymbol a

-- | Build qualified name pairs (of name and container) from the
-- symbol id pieces of the name. The separator is the language-specific
-- concept of namespace or identifier separator.
symbolPairToQName
  :: (Symbol name, Symbol container)
  => Text  -- ^ qualified name separator
  -> name
  -> container
  -> Glean.RepoHaxl u w (Name, Name)
symbolPairToQName separator name container = do
  cSym <- Name . intercalate separator <$> toSymbol container
  nSym <- Name . intercalate separator <$> toSymbol name
  return (nSym, cSym)
