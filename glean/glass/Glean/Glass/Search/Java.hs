{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Java
  ( {- instances -}
    toQName,
    toMName
  ) where

import Data.Maybe
import Data.Text as Text ( Text )
import qualified Data.Text as Text
import Data.List.NonEmpty  ( NonEmpty((:|)) )

import Glean.Angle as Angle
import Glean.Haxl.Repos (ReposHaxl)

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeJava.Types as Java
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.JavaAlpha.Types as Java
import qualified Glean.Schema.JavakotlinAlpha.Types as JavaKotlin
import qualified Glean.Schema.SymbolidJava.Types as Java
import qualified Glean.Schema.Src.Types as Src

instance Search (ResultLocation Java.Entity) where
  symbolSearch toks = case toks of
    [] -> return $ None "Java.symbolSearch: empty"
    [_] -> return $ None "Java.symbolSearch: singleton: not a symbolid"
    (x:y:rest) -> runQuery x y rest

runQuery ::
  Text
  -> Text
  -> [Text]
  -> ReposHaxl u w (SearchResult (ResultLocation Java.Entity))
runQuery x y toks =
  let q = parse x y (map tokenize toks) in
  case q of
    Nothing -> pure $ None "No entity found"
    Just (Term name path Nothing) -> do -- either qname or mname witout sig
      result <- searchSymbolId (x:y:toks) $ searchByQName (toQName name path)
      case result of
        None{} -> searchSymbolId (x:y:toks) $
           searchByMName (toMName name path Nothing)
        x -> return x
    Just (Term name path msig) -> -- definitely a method name with sig
      searchSymbolId (x:y:toks) $ searchByMName (toMName name path msig)

--
-- Parsing the symbol id format
--

-- | Two flavors of query
data Term = Term {
    _base :: Text,
    _path :: NonEmpty Text,
    _sig :: Maybe Signature
  }
  deriving Show

newtype Signature = Signature [Param]
  deriving Show

newtype Param = Param [Text]
  deriving Show

data Token
  = TName !Text
  | TSignature

tokenize :: Text -> Token
tokenize ".t" = TSignature
tokenize n = TName n

-- | format: name/name/name*/(.t/name+)?
parse :: Text -> Text -> [Token] -> Maybe Term
parse base path [] = Just (Term path (base :| []) Nothing)
parse base path rest = case path' ++ [path, base] of
    (name:r:rs) -> Just (Term name (r :| rs) msig) -- inside out order
    _ -> Nothing
  where
    (path', msig) = parsePath rest

parsePath :: [Token] -> ([Text], Maybe Signature)
parsePath [] = ([], Nothing)
parsePath ((TName n):rest) = parseNames rest [n]
parsePath (TSignature:rest) = ([], parseSig rest [])

parseNames :: [Token] -> [Text] -> ([Text], Maybe Signature)
parseNames [] acc = (acc, Nothing)
parseNames (TName n:rest) acc = parseNames rest (n:acc)
parseNames (TSignature:rest) acc = (acc, parseSig rest [])

parseSig :: [Token] -> [Text] -> Maybe Signature
parseSig [] [] = Nothing
parseSig [] acc = Just (Signature (map split (reverse acc)))
  where
    split tok = Param (Text.splitOn "." tok)
parseSig (TName n:rest) acc = parseSig rest (n:acc)
parseSig (TSignature:_) _ = Nothing -- invalid signature token

--
-- Actually doing the search
--

searchByQName :: Angle JavaKotlin.QName -> Angle (ResultLocation Java.Entity)
searchByQName qname =
  vars $ \(entity :: Angle Java.Entity) (file :: Angle Src.File)
    (decl :: Angle Java.Declaration) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      wild .= predicate @Java.LookupDeclaration (
        rec $
          field @"qname" (asPredicate qname) $
          field @"decl" decl
        end),
      alt @"decl" decl .= sig entity,
      entityLocation (alt @"java" entity) file rangespan lname
    ]

-- methods and cosntructors with empty signatures
searchByMName
  :: Angle JavaKotlin.MethodName -> Angle (ResultLocation Java.Entity)
searchByMName mname =
  vars $ \(entity :: Angle Java.Entity) (file :: Angle Src.File)
    (decl :: Angle Java.Declaration) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      wild .= predicate @Java.LookupMethodDeclaration (
        rec $
          field @"mname" (asPredicate mname) $
          field @"decl" decl
        end),
      alt @"decl" decl .= sig entity,
      entityLocation (alt @"java" entity) file rangespan lname
    ]

--
-- Generic to Java and Kotlin
--

toMName
 :: Text -> NonEmpty Text -> Maybe Signature -> Angle JavaKotlin.MethodName
toMName name (base :| rest) Nothing =
  predicate @JavaKotlin.MethodName $
    rec $
      field @"name" (asPredicate (toQName name (base :| rest)))
    end

toMName name (base :| rest) (Just (Signature tys)) =
  predicate @JavaKotlin.MethodName $
    rec $
      field @"name" (asPredicate (toQName name (base :| rest))) $
      field @"signature" (array (mapMaybe toType tys))
    end

toType :: Param -> Maybe (Angle JavaKotlin.Type)
toType (Param st) = case reverse st of
  [] -> Nothing
  x:xs -> Just (toSimpleTypeOuter x xs)

toSimpleTypeOuter :: Text -> [Text] -> Angle JavaKotlin.Type
toSimpleTypeOuter x [] = predicate @JavaKotlin.Type
  (alt @"primitive" (string x)) -- non-qualified name. has to be a primitive
toSimpleTypeOuter x xs
  | Just x' <- Text.stripSuffix "[]" x -- definitely an array
  = predicate @JavaKotlin.Type (
      alt @"array" (asPredicate (toSimpleType x' xs)) -- array of xs.x
    )
  | otherwise = toSimpleType x xs

toSimpleType :: Text -> [Text] -> Angle JavaKotlin.Type
toSimpleType x xs = predicate @JavaKotlin.Type (
  alt @"object" (
    rec $
      field @"base" (string x) $
      field @"container" (case xs of
        [] -> nothing
        y:ys -> just (toPath y ys)
      )
    end)
  )

toQName :: Text -> NonEmpty Text -> Angle JavaKotlin.QName
toQName name (base :| rest) = predicate @JavaKotlin.QName $
  rec $
    field @"name" (string name) $
    field @"context" (asPredicate (toPath base rest))
  end

toPath :: Text -> [Text] -> Angle JavaKotlin.Path
toPath base [] = predicate @JavaKotlin.Path $
  rec $
    field @"base" (string base) $
    field @"container" nothing
  end
toPath base (x:xs) = predicate @JavaKotlin.Path $
  rec $
    field @"base" (string base) $
    field @"container" (just (toPath x xs))
  end
