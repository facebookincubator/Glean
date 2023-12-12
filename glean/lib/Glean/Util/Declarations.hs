{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ConstraintKinds #-}

-- | The 'Cxx.Declaration' type has so many cases.  Try to capture some
-- using 'IsDeclaration' so we can reuse them.
module Glean.Util.Declarations
  ( -- * Visiting branches
    DeclBranch, applyDeclaration
  , applyConstrainedDeclaration
    -- * Query utils
  , queryNamespace
  , queryRecord
    -- * Ranges
  , declarationSrcRange
    -- * CxxName
  , CxxName(..)
  , ObjcContainerName(..)
  , declarationCxxName
  , objcContainerName
  , enumeratorCxxName
    -- * Fact Id
  , getDeclId
  ) where

import Data.Int (Int64)
import Data.Proxy
import Data.Kind (Constraint)
import Data.List.Extra (foldl')
import Data.Typeable
import Data.Text (Text)

import Glean.Angle
import Glean.Schema.Cxx1.Types as Cxx -- gen
import Glean.Schema.Src.Types as Src -- gen
import Glean.Typed.Id (IdOf(..), Fid(..))
import Glean.Typed.Predicate (Predicate(..), SumBranches(..))
import Glean.Util.Range (HasSrcRange(..))

-- -----------------------------------------------------------------------------

-- | 'DeclBranch' is the class constraint for writing the case handler
-- of 'applyDeclaration' combining the 'HaxlQuery' class
-- with the 'Cxx.Declaration' specific classes
-- 'HasSrcRange' and 'SumBranches' and 'SumQuery'
type DeclBranch p =
  ( Predicate p
  , Typeable p
  , Show p
  , Typeable (KeyType p), Show (KeyType p)
  , HasSrcRange (KeyType p)
  , SumBranches p Cxx.Declaration )

-- -----------------------------------------------------------------------------

{-# INLINE applyDeclaration #-}
-- | Provide a case handler.  If the 'DeclBranch' does not provide enough
-- constraints then please extend it, or use 'applyConstainedDeclaration'
applyDeclaration
  :: (forall a . DeclBranch a => a -> b)
  -> Cxx.Declaration
  -> b
applyDeclaration f = \case
  Cxx.Declaration_namespace_ d -> f d
  Cxx.Declaration_usingDeclaration d -> f d
  Cxx.Declaration_usingDirective d -> f d
  Cxx.Declaration_record_ d -> f d
  Cxx.Declaration_enum_ d -> f d
  Cxx.Declaration_function_ d -> f d
  Cxx.Declaration_variable d -> f d
  Cxx.Declaration_objcContainer d -> f d
  Cxx.Declaration_objcMethod d -> f d
  Cxx.Declaration_objcProperty d -> f d
  Cxx.Declaration_typeAlias d -> f d
  Cxx.Declaration_namespaceAlias d -> f d
  Cxx.Declaration_EMPTY -> error "unknown declaration branch"


{-# INLINE applyConstrainedDeclaration #-}
-- | Provide a case handle, and specify additional constraint maker with
-- the Proxy.
applyConstrainedDeclaration
  :: ( k Cxx.NamespaceDeclaration
     , k Cxx.UsingDeclaration
     , k Cxx.UsingDirective
     , k Cxx.RecordDeclaration
     , k Cxx.EnumDeclaration
     , k Cxx.FunctionDeclaration
     , k Cxx.VariableDeclaration
     , k Cxx.ObjcContainerDeclaration
     , k Cxx.ObjcMethodDeclaration
     , k Cxx.ObjcPropertyDeclaration
     , k Cxx.TypeAliasDeclaration
     , k Cxx.NamespaceAliasDeclaration )
  => Proxy (k :: * -> Constraint)
  -> (forall a . (k a) => a -> b)
  -> Cxx.Declaration
  -> b
applyConstrainedDeclaration Proxy f = \case
  Cxx.Declaration_namespace_ d -> f d
  Cxx.Declaration_usingDeclaration d -> f d
  Cxx.Declaration_usingDirective d -> f d
  Cxx.Declaration_record_ d -> f d
  Cxx.Declaration_enum_ d -> f d
  Cxx.Declaration_function_ d -> f d
  Cxx.Declaration_variable d -> f d
  Cxx.Declaration_objcContainer d -> f d
  Cxx.Declaration_objcMethod d -> f d
  Cxx.Declaration_objcProperty d -> f d
  Cxx.Declaration_typeAlias d -> f d
  Cxx.Declaration_namespaceAlias d -> f d
  Cxx.Declaration_EMPTY -> error "unknown declaration"

-- -----------------------------------------------------------------------------

declarationSrcRange :: Cxx.Declaration -> Maybe Src.Range
declarationSrcRange = applyDeclaration (fmap srcRange . getFactKey)

-- -----------------------------------------------------------------------------

{-# INLINE getDeclId #-}
-- | The fact id from 'getDeclId' can be from the type of any branch, so
-- it cannot be 'IdOf'
getDeclId :: Cxx.Declaration -> Int64
getDeclId = applyDeclaration (fromFid . idOf . getId)

-- | Input list of names is in human order (outermost name at head of list).
-- Empty @""@ names are interpreted as anonymous namespaces components.
queryNamespace
  :: IdOf Src.File -> Text -> [Text] -> Angle Cxx.NamespaceDeclaration
queryNamespace sfId outer rest =
  predicate @Cxx.NamespaceDeclaration $
    rec $
      field @"name" (foldl' addNamespace init rest) $
      field @"source" (
        rec $
          field @"file" (asPredicate (factId sfId))
        end)
    end
  where
    init :: Angle Cxx.NamespaceQName_key
    init =
        rec $
          field @"name" (nqnOf outer) $
          field @"parent" nothing
        end
    -- Empty names mean anonymous which are encoded as Nothing
    nqnOf :: Text -> Angle (Maybe Cxx.Name)
    nqnOf "" = nothing
    nqnOf name = just $ predicate $ string name
    addNamespace :: Angle Cxx.NamespaceQName_key -> Text ->
      Angle Cxx.NamespaceQName_key
    addNamespace angle text =
      rec $
        field @"name" (nqnOf text) $
        field @"parent" (just (predicate angle))
      end

-- | Provide the 'Cxx.NamespaceQName' from 'queryNamespace' and the record name
-- to get query to look for matching facts.
queryRecord
 :: IdOf Src.File -> IdOf Cxx.NamespaceQName -> Text
 -> Angle Cxx.RecordDeclaration
queryRecord sfId nqnId name =
  predicate @Cxx.RecordDeclaration $
    rec $
      field @"name" (
        rec $
          field @"name" (string name) $
          field @"scope" (alt @"namespace_" (asPredicate (factId nqnId)))
        end) $
      field @"source" (
        rec $
          field @"file" (asPredicate (factId sfId))
        end)
    end

-- | Reduction of Cxx.ObjcContainerId where we only want its name
data ObjcContainerName
  = ObjcContainerNameId Cxx.ObjcCategoryId
  | ObjcContainerName Cxx.Name

-- | Reduce Cxx.ObjcContainerId to its name
objcContainerName :: Cxx.ObjcContainerId -> ObjcContainerName
objcContainerName = \case
  Cxx.ObjcContainerId_protocol name -> ObjcContainerName name
  Cxx.ObjcContainerId_interface_ name -> ObjcContainerName name
  Cxx.ObjcContainerId_categoryInterface catId -> ObjcContainerNameId catId
  Cxx.ObjcContainerId_extensionInterface name -> ObjcContainerName name
  Cxx.ObjcContainerId_implementation name -> ObjcContainerName name
  Cxx.ObjcContainerId_categoryImplementation catId -> ObjcContainerNameId catId
  Cxx.ObjcContainerId_EMPTY -> error "unknown ObjcContainerId constructor"

-- | Names from various Cxx things, for common processing
data CxxName
  = CxxNamespaceQName Cxx.NamespaceQName
  | CxxQName Cxx.QName
  | CxxFunctionQName Cxx.FunctionQName
  | CxxObjcContainer Cxx.ObjcContainerId
  | CxxObjcMethodSelector Cxx.ObjcSelector Cxx.ObjcContainerId
  | CxxObjcPropertyName Cxx.Name Cxx.ObjcContainerId
  | CxxEnumerator Cxx.Name Cxx.QName

-- | Reduce Cxx.Declaration to its name
declarationCxxName :: Cxx.Declaration -> Maybe CxxName
declarationCxxName = \case
  Cxx.Declaration_namespace_ d ->
    CxxNamespaceQName . Cxx.namespaceDeclaration_key_name <$> getFactKey d
  Cxx.Declaration_usingDeclaration d ->
    CxxFunctionQName . Cxx.usingDeclaration_key_name <$> getFactKey d
  Cxx.Declaration_usingDirective d ->
    CxxQName . Cxx.usingDirective_key_name <$> getFactKey d
  Cxx.Declaration_record_ d ->
    CxxQName . Cxx.recordDeclaration_key_name <$> getFactKey d
  Cxx.Declaration_enum_ d ->
    CxxQName . Cxx.enumDeclaration_key_name <$> getFactKey d
  Cxx.Declaration_function_ d ->
    CxxFunctionQName . Cxx.functionDeclaration_key_name <$> getFactKey d
  Cxx.Declaration_variable d ->
    CxxQName . Cxx.variableDeclaration_key_name <$> getFactKey d
  Cxx.Declaration_typeAlias d ->
    CxxQName . Cxx.typeAliasDeclaration_key_name <$> getFactKey d
  Cxx.Declaration_namespaceAlias d ->
    CxxNamespaceQName . Cxx.namespaceAliasDeclaration_key_name <$> getFactKey d
  Cxx.Declaration_objcContainer d ->
    CxxObjcContainer . Cxx.objcContainerDeclaration_key_id <$> getFactKey d
  Cxx.Declaration_objcMethod d -> do
    Cxx.ObjcMethodDeclaration_key{..} <- getFactKey d
    return $ CxxObjcMethodSelector objcMethodDeclaration_key_selector
      objcMethodDeclaration_key_container
  Cxx.Declaration_objcProperty d -> do
    Cxx.ObjcPropertyDeclaration_key{..} <- getFactKey d
    return $ CxxObjcPropertyName objcPropertyDeclaration_key_name
      objcPropertyDeclaration_key_container
  Cxx.Declaration_EMPTY -> Nothing

-- | Reduce Cxx.Enumerator to its name
enumeratorCxxName :: Cxx.Enumerator -> Maybe CxxName
enumeratorCxxName e = do
  Cxx.Enumerator_key{..} <- getFactKey e
  Cxx.EnumDeclaration_key{..} <- getFactKey enumerator_key_enumeration
  return $ CxxEnumerator enumerator_key_name enumDeclaration_key_name
