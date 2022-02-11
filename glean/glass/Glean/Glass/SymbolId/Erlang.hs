{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.SymbolId.Erlang
  ({- instances -})
  where

import TextShow

import Glean.Angle as Angle
import Glean (Nat, fromNat, keyOf)
import Glean.Glass.SymbolId.Class
import Glean.Glass.Types (Name(..))
import qualified Glean.Haxl.Repos as Glean

import qualified Glean.Schema.Erlang.Types as Erlang
import Data.Text (Text, intercalate)

import Glean.Schema.CodeErlang.Types as CodeErlang
    ( Entity(..) )

instance Symbol CodeErlang.Entity where
  toSymbol e = case e of
    CodeErlang.Entity_decl decl -> toSymbol decl

instance Symbol Erlang.Declaration where
  toSymbol decl = case decl of
    Erlang.Declaration_func func -> toSymbolPredicate func

instance Symbol Erlang.FunctionDeclaration_key where
  toSymbol (Erlang.FunctionDeclaration_key fqn _file _span) = toSymbol fqn

instance Symbol Erlang.Fqn where
  toSymbol (Erlang.Fqn module_ name arity) =
    return [module_, name, showt (fromNat arity)]

instance Symbol Text where
  toSymbol module_ = return [module_]

instance Symbol (Text, Nat) where
  toSymbol (name, arity) =
      return [intercalate "." [name, showt (fromNat arity)]]

instance ToAngle Erlang.Declaration where
  toAngle (Erlang.Declaration_func x) = alt @"func" (mkKey x)

instance ToQName CodeErlang.Entity where
  toQName e = case e of
    CodeErlang.Entity_decl x -> toQName x

instance ToQName Erlang.Declaration where
  toQName e = case e of
    Erlang.Declaration_func x -> Glean.keyOf x >>= toQName

instance ToQName Erlang.FunctionDeclaration_key where
  toQName e = case e of
    Erlang.FunctionDeclaration_key fqn _file _span -> toQName fqn

instance ToQName Erlang.Fqn where
  toQName e = case e of
    Erlang.Fqn module_ name arity -> pairToQName (name, arity) module_

pairToQName
  :: (Symbol name, Symbol container)
  => name
  -> container
  -> Glean.RepoHaxl u w (Either a (Name, Name))
pairToQName a b = Right <$> symbolPairToQName "." a b
