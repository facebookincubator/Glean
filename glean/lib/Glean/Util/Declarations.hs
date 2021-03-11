{-# LANGUAGE TypeApplications, ConstraintKinds #-}
-- Copyright 2004-present Facebook. All Rights Reserved.

-- | The 'Cxx.Declaration' type has so many cases.  Try to capture some
-- using 'IsDeclaration' so we can reuse them.
module Glean.Util.Declarations
  ( -- * Visiting branches
    DeclBranch, applyDeclaration, matchDeclaration
  , applyConstrainedDeclaration
    -- * Query utils
  , queryNamespace
  , queryRecord
  , angleFamilyOfDecls
  , angleOfDeclId
  , angleOfDeclaration
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

import Data.Default
import Data.Int (Int64)
import Data.Proxy
import Data.Kind (Constraint)
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable
import Data.Text (Text)

import Glean (HaxlQuery, justId)
import Glean.Angle
import Glean.Schema.Cxx1.Types as Cxx -- gen
import Glean.Schema.Src.Types as Src -- gen
import Glean.Schema.Query.Cxx1.Types as Q.Cxx -- gen
import Glean.Schema.Query.Src.Types as Q.Src -- gen
import Glean.Typed.Id (IdOf(..), Fid(..))
import Glean.Typed.Predicate (Predicate(..), SumBranches(..))
import Glean.Typed.Query
import Glean.Util.Range (HasSrcRange(..))

-- | Make a single query to fetch the DeclToFamily for all the inputs
angleFamilyOfDecls :: NonEmpty Cxx.Declaration -> Angle Cxx.DeclToFamily
angleFamilyOfDecls ds =
    var $ \ (declaration :: Angle Cxx.Declaration) ->
    predicate @Cxx.DeclToFamily (rec $ field @"decl" declaration end)
      `where_`
      [ declaration .= (foldr1 (.|) fs `hasType` "cxx1.Declaration.2") ]
    where
      fs = angleOfDeclaration <$> ds

-- | Helper for building 'Cxx.Declaration' angle queries from explicit @Id@.
angleOfDeclId :: (DeclBranch p) => IdOf p -> Angle Cxx.Declaration
angleOfDeclId i = angleOfDeclaration (injectBranch (justId i))

-- | Helper for building 'Cxx.Declaration' queries.
angleOfDeclaration :: Cxx.Declaration -> Angle Cxx.Declaration
angleOfDeclaration = \case
  Cxx.Declaration_namespace_ p -> alt @"namespace_"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_usingDeclaration p -> alt @"usingDeclaration"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_usingDirective p -> alt @"usingDirective"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_record_ p -> alt @"record_"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_enum_ p -> alt @"enum_"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_function_ p -> alt @"function_"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_variable p -> alt @"variable"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_objcContainer p -> alt @"objcContainer"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_objcMethod p -> alt @"objcMethod"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_objcProperty p -> alt @"objcProperty"
    (asPredicate (factId (getId p)))
  Cxx.Declaration_typeAlias p -> alt @"typeAlias"
    (asPredicate (factId (getId p)))

-- -----------------------------------------------------------------------------

-- | 'DeclBranch' is the class constraint for writing the case handler
-- of 'applyDeclaration' combining the 'HaxlQuery' class
-- with the 'Cxx.Declaration' specific classes
-- 'HasSrcRange' and 'SumBranches' and 'SumQuery'
type DeclBranch p =
  ( HaxlQuery p
  , PredicateQuery p
  , Typeable (KeyType p), Show (KeyType p)
  , HasSrcRange (KeyType p)
  , SumBranches p Cxx.Declaration
  , SumQuery (QueryOf p) (QueryOf Cxx.Declaration) )

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
     , k Cxx.TypeAliasDeclaration )
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

-- -----------------------------------------------------------------------------

-- | Takes the Fid of the provided delaration and makes a query to match
-- precisely this declaration branch and id
matchDeclaration :: Cxx.Declaration -> Q.Cxx.Declaration
matchDeclaration = applyDeclaration (injectQuery . toQueryId . getId)

-- -----------------------------------------------------------------------------

declarationSrcRange :: Cxx.Declaration -> Maybe Src.Range
declarationSrcRange = applyDeclaration (fmap srcRange . getFactKey)

-- -----------------------------------------------------------------------------

{-# INLINE getDeclId #-}
-- | The fact id from 'getDeclId' can be from the type of any branch, so
-- it cannot be 'IdOf'
getDeclId :: Cxx.Declaration -> Int64
getDeclId = applyDeclaration (fromFid . idOf . getId)

-- -----------------------------------------------------------------------------

-- | Input list of names is in human order (outermost name at head of list).
-- Empty @""@ names are interpreted as anonymous namespaces components.
-- The result is @Nothing@ if and only if the input list is @[]@.
queryNamespace :: IdOf Src.File -> [Text] -> Maybe Q.Cxx.NamespaceDeclaration
queryNamespace sfId = \case
  [] ->  Nothing
  (outer:rest) -> Just $ Q.Cxx.NamespaceDeclaration_with_key def
    { Q.Cxx.namespaceDeclaration_key_name = Just $
        foldl' addNamespace (outerNamespaceOf outer) rest
    , Q.Cxx.namespaceDeclaration_key_source = Just $ def
      { Q.Src.range_file = Just (toQueryId sfId) } }
  where
    -- Empty names mean anonymous which are encoded as Nothing
    nqnnOf "" = def{ Q.Cxx.namespaceQName_name_nothing = Just def }
    nqnnOf name = def{ Q.Cxx.namespaceQName_name_just = Just $
      Q.Cxx.Name_with_key name }
    outerNamespaceOf outer = Q.Cxx.NamespaceQName_with_key def
      { Q.Cxx.namespaceQName_key_name = Just (nqnnOf outer)
      , Q.Cxx.namespaceQName_key_parent = Just def
        { Q.Cxx.namespaceQName_parent_nothing = Just def } }
    addNamespace parent name = Q.Cxx.NamespaceQName_with_key def
      { Q.Cxx.namespaceQName_key_name = Just (nqnnOf name)
      , Q.Cxx.namespaceQName_key_parent = Just def
        { Q.Cxx.namespaceQName_parent_just = Just parent } }

-- | Provide the 'Cxx.NamespaceQName' from 'queryNamespace' and the record name
-- to get query to look for matching facts.
queryRecord
  :: IdOf Src.File -> IdOf Cxx.NamespaceQName -> Text
  -> Q.Cxx.RecordDeclaration
queryRecord sfId nqnId name = Q.Cxx.RecordDeclaration_with_key def
  { Q.Cxx.recordDeclaration_key_name = Just $ Q.Cxx.QName_with_key def
    { Q.Cxx.qName_key_name = Just $ Q.Cxx.Name_with_key name
    , Q.Cxx.qName_key_scope = Just def
      { Q.Cxx.scope_namespace_ = Just (toQueryId nqnId) } }
  , Q.Cxx.recordDeclaration_key_source = Just $ def
      { Q.Src.range_file = Just (toQueryId sfId) } }

-- -----------------------------------------------------------------------------

-- | Reduction of Cxx.ObjcContainerId where we only want its name
data ObjcContainerName = ObjcContainerNameId Cxx.ObjcCategoryId
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

-- | Reduce Cxx.Enumerator to its name
enumeratorCxxName :: Cxx.Enumerator -> Maybe CxxName
enumeratorCxxName e = do
  Cxx.Enumerator_key{..} <- getFactKey e
  Cxx.EnumDeclaration_key{..} <- getFactKey enumerator_key_enumeration
  return $ CxxEnumerator enumerator_key_name enumDeclaration_key_name
