{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Java
  ({- instances -})
  where

import Data.Maybe

import qualified Glean
import Glean.Glass.Types
import Glean.Glass.SymbolId.Class

import qualified Glean.Schema.CodeJava.Types as Java
import qualified Glean.Schema.Java.Types as Java

instance Symbol Java.Entity where
  toSymbol e = case e of
    Java.Entity_definition_ d -> toSymbol d
    Java.Entity_class_{} -> return []
    Java.Entity_EMPTY -> return []

instance Symbol Java.Definition where
  toSymbol e = case e of
    Java.Definition_class_ c -> toSymbolPredicate c -- ambiguous constructor
    Java.Definition_interface_ i -> toSymbolPredicate i
    Java.Definition_enum_ e -> toSymbolPredicate e
    Java.Definition_EMPTY -> return []

instance Symbol Java.ClassDeclaration_key where
  toSymbol Java.ClassDeclaration_key{..} =
    toSymbolPredicate classDeclaration_key_name

instance Symbol Java.InterfaceDeclaration_key where
  toSymbol Java.InterfaceDeclaration_key{..} =
    toSymbolPredicate interfaceDeclaration_key_name

instance Symbol Java.EnumDeclaration_key where
  toSymbol Java.EnumDeclaration_key{..} =
    toSymbolPredicate enumDeclaration_key_name

instance Symbol Java.QName_key where
  toSymbol Java.QName_key{..} = return [fromMaybe "<anonymous>" qName_key_fqn]

instance ToQName Java.Entity where
  toQName e = case e of
    Java.Entity_definition_ d -> toQName d
    Java.Entity_class_ d  -> Glean.keyOf d >>= toQName
    Java.Entity_EMPTY -> return $ Left "unknown Java.Entity"

instance ToQName Java.Definition where
  toQName e = case e of
    Java.Definition_class_ c -> Glean.keyOf c >>= toQName
    Java.Definition_interface_ i -> Glean.keyOf i >>= toQName
    Java.Definition_enum_ e -> Glean.keyOf e >>= toQName
    Java.Definition_EMPTY -> return $ Left "unknown Java.Definition"

instance ToQName Java.ClassDeclaration_key where
  toQName Java.ClassDeclaration_key{..} =
    Glean.keyOf classDeclaration_key_name >>= toQName

instance ToQName Java.InterfaceDeclaration_key where
  toQName Java.InterfaceDeclaration_key{..} =
    Glean.keyOf interfaceDeclaration_key_name >>= toQName

instance ToQName Java.EnumDeclaration_key where
  toQName Java.EnumDeclaration_key{..} =
    Glean.keyOf enumDeclaration_key_name >>= toQName

instance ToQName Java.QName_key where
  toQName Java.QName_key{..} = do
    case qName_key_path of
      Nothing -> return $ Right (Name qName_key_name, Name "")
      -- tbd should this be the dotted fqn of the base + container?
      Just path -> do
        Java.Path_key base _ <- Glean.keyOf path
        baseStr <- Glean.keyOf base
        return $ Right (Name qName_key_name, Name baseStr)
