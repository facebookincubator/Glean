{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns, DefaultSignatures, TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-} -- class QueryDefDecl
module Glean.Util.Same
  ( -- * Family
    queryDeclFamily, queryDeclFamilyOf
    -- * Definitions
    -- ** type
  , DeclToDef
  , DefDecl(..)
    -- ** query
  , queryDefDeclOf
  , queryDefOf
  , queryDeclToFamilyDefs
  , queryDeclToDef
    -- ** helper class
  , DefCheck(..)

  ) where

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Proxy

import Glean
import qualified Glean.Schema.CodeCxx.Types as CodeCxx
import qualified Glean.Schema.Query.CodeCxx.Types as Q.CodeCxx
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Query.Cxx1.Types as Q.Cxx
import Glean.Util.Declarations

-- | Use 'Cxx.DeclToFamily' to find the same facts (shallow query)
--
-- Always returns non-empty list, in same order as in 'Cxx.DeclFamily' fact
queryDeclFamily :: Cxx.Declaration -> Haxl w [Cxx.Declaration]
queryDeclFamily decl = do
  fams <- search_ $ query $
    Q.Cxx.DeclToFamily_with_key $ def
      { Q.Cxx.declToFamily_key_decl = Just $ matchDeclaration decl }
  case fams of
    [] -> return [decl]
    (Cxx.DeclToFamily _ (Just key):_) ->
      getKey (Cxx.declToFamily_key_family key)
    _ -> error ("queryDeclFamily: " ++ show decl)

-- | Type-specific 'queryDeclFamily' that returns the same type as passed in
queryDeclFamilyOf :: DeclBranch p => IdOf p -> Haxl w [IdOf p]
queryDeclFamilyOf
  = fmap (map getId . mapMaybe projectBranch)
  . queryDeclFamily
  . injectBranch . justId


-- -----------------------------------------------------------------------------
-- | Classifying using 'SameAs' to find tasty definitions
data DefDecl
  = Def -- ^ The declaration has associated definition predicate and fact
  | Decl -- ^ The declaration has an associated definition predicate but no fact
  | OnlyDecl -- ^ The declaration has no associated definition predicate at all
  deriving (Eq, Ord, Enum)

-- -----------------------------------------------------------------------------

-- | Map to Cxx1 definition type, or @p@ if it does not exist. Injective
type family DeclToDef p = d | d -> p

-- main DeclToDef types
type instance DeclToDef Cxx.RecordDeclaration = Cxx.RecordDefinition
type instance DeclToDef Cxx.EnumDeclaration = Cxx.EnumDefinition
type instance DeclToDef Cxx.FunctionDeclaration = Cxx.FunctionDefinition
type instance DeclToDef Cxx.ObjcContainerDeclaration =
  Cxx.ObjcContainerDefinition
type instance DeclToDef Cxx.ObjcMethodDeclaration = Cxx.ObjcMethodDefinition
type instance DeclToDef Cxx.NamespaceDeclaration = Cxx.NamespaceDefinition
-- some variables are global
type instance DeclToDef Cxx.VariableDeclaration = Cxx.VariableDeclaration
-- questionable DeclToDef type
type instance DeclToDef Cxx.ObjcPropertyDeclaration =
  Cxx.ObjcPropertyImplementation
-- identity DeclToDef x = x, to make extra branches compile
type instance DeclToDef Cxx.UsingDeclaration = Cxx.UsingDeclaration
type instance DeclToDef Cxx.UsingDirective = Cxx.UsingDirective
type instance DeclToDef Cxx.TypeAliasDeclaration =
  Cxx.TypeAliasDeclaration

-- | For the 11 'Cxx.Declaration' branches there are 5 branches without
-- definitions and 6 with definitions.
--
-- 'DefCheck' helps do the query to check if a decl is a def.
class DefCheck p where

  -- | Calculate the definition query to match the Fid, if this @DeclToDef2
  -- exists
  declToDef :: IdOf p -> Maybe (QueryOf (DeclToDef p))
  declToDef _ = Nothing

  defToDecl :: DeclToDef p -> Maybe p
  defToDecl = error "Absurd default defToDecl instance"

  -- | Some type @p@  have no partner definition @`DeclToDef` p@ in
  -- `CodeCxx.Definition`, and these always return Nothing. For types @p@
  -- with  @`DeclToDef` p@ in `CodeCxx.Definition`, this projects that
  -- branch to @Just (DeclToDef p)@ and other branches of `CodeCxx.Definition`
  -- to Nothing.
  codeDefToDef :: Proxy p -> CodeCxx.Definition -> Maybe (DeclToDef p)
  codeDefToDef _ _ = Nothing

instance DefCheck Cxx.NamespaceDeclaration where
  declToDef i = Just $ Q.Cxx.NamespaceDefinition_with_key def
    { Q.Cxx.namespaceDefinition_key_declaration =
        Just (toQueryId i) }
  defToDecl = fmap Cxx.namespaceDefinition_key_declaration . getFactKey
  codeDefToDef _ (CodeCxx.Definition_namespace_ x) = Just x
  codeDefToDef _ _ = Nothing

instance DefCheck Cxx.UsingDeclaration
instance DefCheck Cxx.UsingDirective

instance DefCheck Cxx.RecordDeclaration where
  declToDef i = Just $ Q.Cxx.RecordDefinition_with_key def
    { Q.Cxx.recordDefinition_key_declaration =
        Just (toQueryId i) }
  defToDecl = fmap Cxx.recordDefinition_key_declaration . getFactKey
  codeDefToDef _ (CodeCxx.Definition_record_ x) = Just x
  codeDefToDef _ _ = Nothing

instance DefCheck Cxx.EnumDeclaration where
  declToDef i = Just $ Q.Cxx.EnumDefinition_with_key def
    { Q.Cxx.enumDefinition_key_declaration =
        Just (toQueryId i) }
  defToDecl = fmap Cxx.enumDefinition_key_declaration . getFactKey
  codeDefToDef _ (CodeCxx.Definition_enum_ x) = Just x
  codeDefToDef _ _ = Nothing

instance DefCheck Cxx.FunctionDeclaration where
  declToDef i = Just $ Q.Cxx.FunctionDefinition_with_key def
    { Q.Cxx.functionDefinition_key_declaration =
        Just (toQueryId i) }
  defToDecl = fmap Cxx.functionDefinition_key_declaration . getFactKey
  codeDefToDef _ (CodeCxx.Definition_function_ x) = Just x
  codeDefToDef _ _ = Nothing

instance DefCheck Cxx.VariableDeclaration where
  -- | Support 'queryDeclToDef' for 'Cxx.VariableDeclaration'
  defToDecl = Just
  codeDefToDef _ (CodeCxx.Definition_variable x) = Just x
  codeDefToDef _ _ = Nothing

instance DefCheck Cxx.ObjcContainerDeclaration where
  declToDef i = Just $ Q.Cxx.ObjcContainerDefinition_with_key def
    { Q.Cxx.objcContainerDefinition_key_declaration =
        Just (toQueryId i) }
  defToDecl = fmap Cxx.objcContainerDefinition_key_declaration . getFactKey
  codeDefToDef _ (CodeCxx.Definition_objcContainer x) = Just x
  codeDefToDef _ _ = Nothing

instance DefCheck Cxx.ObjcMethodDeclaration where
  declToDef i = Just $ Q.Cxx.ObjcMethodDefinition_with_key
    (toQueryId i)
  defToDecl = getFactKey
  codeDefToDef _ (CodeCxx.Definition_objcMethod x) = Just x
  codeDefToDef _ _ = Nothing

-- | This instance is questionable. One ObjcPropertyDeclaration can
-- have many ObjcPropertyImplementation, which is backwards compared
-- to the other instances.
instance DefCheck Cxx.ObjcPropertyDeclaration where
  declToDef i = Just $ Q.Cxx.ObjcPropertyImplementation_with_key def
    { Q.Cxx.objcPropertyImplementation_key_declaration =
        Just (toQueryId i) }
  defToDecl = fmap Cxx.objcPropertyImplementation_key_declaration . getFactKey
  codeDefToDef _ _ = Nothing

instance DefCheck Cxx.TypeAliasDeclaration where

-- -----------------------------------------------------------------------------

-- | Use 'DefCheck' to determine if the 'IdOf' declaration is a definition
queryDefDeclOf
  :: (HaxlQuery (DeclToDef p), DefCheck p)
  => IdOf p
  -> Haxl w DefDecl
queryDefDeclOf i = case declToDef i of
  Nothing -> return OnlyDecl
  Just q -> do
    xs <- getFirstResult (query q)
    return $ if null xs then Decl else Def

-- | Use 'DefCheck' to get matching definition, if possible and present
queryDefOf
  :: (HaxlQuery (DeclToDef p), DefCheck p)
  => IdOf p
  -> Haxl w (Maybe (DeclToDef p))
queryDefOf = fmap join . traverse (getFirstResult . query) . declToDef

-- | Explore whole family and return the corresponding definitions.
-- The new 'queryDeclToDef' may be more efficient for most cases.
--
-- Note, does not support 'Cxx.VariableDeclaration', and does support
-- questionable 'Cxx.ObjcPropertyDeclaration'
-- to 'Cxx.ObjcPropertyImplementation' lookup.
queryDeclToFamilyDefs
  :: (HaxlQuery (DeclToDef p), DefCheck p, DeclBranch p)
  => IdOf p
  -> Haxl w [DeclToDef p]
queryDeclToFamilyDefs xIn = case declToDef xIn of
  Nothing -> return [] -- shortcut
  Just _ -> do
    xs <- queryDeclFamilyOf xIn
    catMaybes <$> mapM queryDefOf xs

-- | Replaces the older 'queryDeclToFamilyDefs'
--
-- Note, does support 'Cxx.VariableDeclaration', and not support
-- questionable 'Cxx.ObjcPropertyDeclaration'
-- to 'Cxx.ObjcPropertyImplementation' lookup.
queryDeclToDef
  :: forall p w. ( DeclBranch p, HaxlQuery (DeclToDef p), DefCheck p )
  => Int
  -> IdOf p
  -> Haxl w [DeclToDef p]
queryDeclToDef limitDeclToDef i = do
  let queryDecl :: Q.Cxx.Declaration
      queryDecl = toQuery (injectBranch (justId i))
      q :: Q.CodeCxx.DeclToDef
      q = Q.CodeCxx.DeclToDef_with_key def
        { Q.CodeCxx.declToDef_key_decl = Just queryDecl }
      pick :: CodeCxx.Definition -> Maybe (DeclToDef p)
      pick = codeDefToDef (Proxy :: Proxy p)
      peek :: CodeCxx.DeclToDef -> Maybe (DeclToDef p)
      peek = CodeCxx.declToDef_key >=> (pick . CodeCxx.declToDef_key_defn)
  fmap (mapMaybe peek . fst) $ search $ limit limitDeclToDef $ recursive $
    query q
