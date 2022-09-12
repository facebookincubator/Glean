{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.SymbolId.Python () where

import Data.Text (Text, splitOn)
import Data.List (nub)
import Data.Maybe ( maybeToList )

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Python.Types as Py
import Glean.Util.ToAngle

import Glean.Glass.SymbolId.Class
import Glean.Glass.Types (Name(..))
import Glean.Glass.Utils ( fetchData )

instance Symbol Py.Name where
  toSymbol k = do
    v <- Glean.keyOf k
    return [v]

instance Symbol Py.Declaration where
  toSymbol d = do
    locations <- maybeToList <$> fetchData (declarationLocation $ toAngle d)
    let loc = case nub $ map (head . splitOn "/") locations of
              [location] -> location
              _ -> "."
    sym <- case d of
      Py.Declaration_cls x -> toSymbolPredicate x
      Py.Declaration_func x -> toSymbolPredicate x
      Py.Declaration_module x -> toSymbolPredicate x
      Py.Declaration_variable x -> toSymbolPredicate x
      Py.Declaration_imp x -> toSymbolPredicate x
      Py.Declaration_EMPTY -> return []
    return $ loc:sym

instance Symbol Py.ImportStatement_key where
  toSymbol (Py.ImportStatement_key _ asName) = toSymbol asName

instance Symbol Py.VariableDeclaration_key where
  toSymbol (Py.VariableDeclaration_key n) = toSymbol n

instance Symbol Py.FunctionDeclaration_key where
  toSymbol (Py.FunctionDeclaration_key n) = toSymbol n

instance Symbol Py.ClassDeclaration_key where
  toSymbol (Py.ClassDeclaration_key n _) = toSymbol n

instance Symbol Py.Module_key where
  toSymbol (Py.Module_key n) = toSymbol n

declarationLocation :: Angle Py.Declaration -> Angle Text
declarationLocation decl = var $ \file ->
  file `where_` [
    wild .= predicate @Py.DeclarationLocation (
      rec $
        field @"declaration" decl $
        field @"file" file
      end)
  ]

instance ToQName Py.Declaration where
  toQName (Py.Declaration_cls x) = Glean.keyOf x >>= toQName
  toQName (Py.Declaration_func x) = Glean.keyOf x >>= toQName
  toQName (Py.Declaration_module x) = Glean.keyOf x >>= toQName
  toQName (Py.Declaration_variable x) = Glean.keyOf x >>= toQName
  toQName (Py.Declaration_imp x) = Glean.keyOf x >>= toQName
  toQName Py.Declaration_EMPTY = return $ Left "unknown Declaration"

instance ToQName Py.ImportStatement_key where
  toQName (Py.ImportStatement_key _ asName) = toQName asName

instance ToQName Py.VariableDeclaration_key where
  toQName (Py.VariableDeclaration_key x) = toQName x

instance ToQName Py.FunctionDeclaration_key where
  toQName (Py.FunctionDeclaration_key x) = toQName x

instance ToQName Py.ClassDeclaration_key where
  toQName (Py.ClassDeclaration_key x _) = toQName x

instance ToQName Py.Module_key where
  toQName (Py.Module_key x) = toQName x

instance ToQName Py.Name where
  toQName name = do
    let nameId = Glean.getId name
    (mr :: Maybe (Py.Name, Py.Name)) <- fetchData $ qNameQuery nameId
    case mr of
      Nothing -> Left . ("No qualified names found for " <>) <$>
        Glean.keyOf name
      Just (local, container) ->
        fmap Right $ (,) <$> pyNameToName local <*> pyNameToName container

pyNameToName :: Py.Name -> Glean.RepoHaxl u w Name
pyNameToName name = Name <$> Glean.keyOf name

qNameQuery :: Glean.IdOf Py.Name -> Angle (Py.Name, Py.Name)
qNameQuery nameId = vars $
  \ (lname :: Angle Py.Name)
    (pname :: Angle Py.Name)
    (psname :: Angle Py.SName)
    (maybe_psname :: Angle (Maybe Py.SName)) ->
    tuple (lname, pname) `where_` [
        wild .= predicate @Py.NameToSName (factId nameId
          .-> predicate @Py.SName (rec $
              field @"local_name" (asPredicate lname) $
              field @"parent" maybe_psname
            end))
      , wild .=
          or_ [ just psname .= maybe_psname,
                wild .= (predicate @Py.SNameToName $ psname .-> pname) ]
              [ nothing .= maybe_psname, pname .= predicate @Py.Name "" ]
      ]
