{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Annotations
  ( getAnnotationsForEntity
  ) where


import Data.Text (Text)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Control.Monad (forM )

import qualified Glean
import Glean.Glass.Base ( GleanPath(..) )
import Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodeCxx.Types as Cxx1
import qualified Glean.Schema.CodeHack.Types as Hack
import qualified Glean.Schema.Cxx1.Types as Cxx1
import qualified Glean.Schema.Hack.Types as Hack

import Glean.Glass.SymbolId (entityToAngle, toSymbolId)
import Glean.Glass.Pretty.Annotations
import qualified Glean.Glass.Types as Glass
import Glean.Glass.Utils ( searchRecursiveWithLimit )
import Glean.Glass.Path ( fromGleanPath )

-- | For Hack, the annotations are a single fact.
-- For C++ they're sprinkled across each function decl, forcing us to search
max_annotations_limit :: Int
max_annotations_limit = 10

getAnnotationsForEntity
  :: Glass.RepoName -> Code.Entity
  -> Glean.RepoHaxl u w (Either Text (Maybe [Glass.Annotation]))
getAnnotationsForEntity repo entity = fetch repo `mapM` entityToAngle entity

-- Get all the annotations for a given entity. This may include
-- computing symbolId for these annotations.
fetch
  :: Glass.RepoName
  -> Angle Code.Entity
  -> Glean.RepoHaxl u w (Maybe [Glass.Annotation])
fetch repo ent = do
  entityToAnnotations <- queryAnnotations ent
  let annotations = Code.entityToAnnotations_key_annotations <$>
        catMaybes (Code.entityToAnnotations_key <$> entityToAnnotations)
  annotationsSyms <- forM annotations (annotationsToSymbols repo)
  return $ getAnnotations annotationsSyms

queryAnnotations
  :: Angle Code.Entity -> Glean.RepoHaxl u w [Code.EntityToAnnotations]
queryAnnotations entity = searchRecursiveWithLimit (Just max_annotations_limit)$
  Angle.predicate @Code.EntityToAnnotations $
    rec $
      field @"entity" entity
    end

-- Maps an Annotations (e.g. a list of annotation, to their possible
-- corresponding symbolId). Codemarkup doesn't expose a single Annotation
-- type, so we use zipped list. Currently, the symbolId are only used for
-- Hack. For the other languages, we use an empty list of symbolIds.
newtype AnnotationsSymbolId =
  AnnotationsSymbolId (Code.Annotations, [Maybe Glass.SymbolId])

annotationsToSymbols
  :: Glass.RepoName
  -> Code.Annotations
  -> Glean.RepoHaxl u w AnnotationsSymbolId
annotationsToSymbols repo annotations = case annotations of
  Code.Annotations_hack (Hack.Annotations_attributes attrs) -> do
    attributeToDecl <- queryAttributeToDecl attrs
    decls <- forM attributeToDecl (declarationsToSymbolId repo)
    let mapAnnotSym = Map.fromList decls
        syms = (`Map.lookup` mapAnnotSym) <$> attrs
    return $ AnnotationsSymbolId (annotations, syms)
  _ -> return $ AnnotationsSymbolId (annotations, [])

-- Generate a query of the form
--   hack.AttributeToDeclaration.6 {attribute = X0}
--   where X0 = [$388098 : hack.UserAttribute.6,
--               $41135 : hack.UserAttribute.6,
--               $139800678 : hack.UserAttribute.6][..]"]
-- From AttributeToDeclaration facts, we get the info
-- required to build an Annotation SymbolId
queryAttributeToDecl
  :: [Hack.UserAttribute]
  -> Glean.RepoHaxl u w [Hack.AttributeToDeclaration]
queryAttributeToDecl [] = return []
queryAttributeToDecl attrs = searchRecursiveWithLimit (Just (length attrs)) $
  Angle.predicate @Hack.AttributeToDeclaration $
    rec $
      field @"attribute" (asPredicate (elementsOf factIds))
    end
  where
    factIds = array (factId . Glean.getId <$> attrs)

-- | Unpack the decl and file fields of the annotation lookup and
-- construct a symbol id
declarationsToSymbolId
  :: Glass.RepoName
  -> Hack.AttributeToDeclaration
  -> Glean.RepoHaxl u w (Hack.UserAttribute, Glass.SymbolId)
declarationsToSymbolId repo attrDecl = do
  Hack.AttributeToDeclaration_key{..} <- Glean.keyOf attrDecl
  path <- Glean.keyOf attributeToDeclaration_key_file
  let userAttribute = attributeToDeclaration_key_attribute
  let decl = attributeToDeclaration_key_declaration
      entity = Code.Entity_hack (Hack.Entity_decl decl) -- do this in Angle
  sym <- toSymbolId (fromGleanPath repo (GleanPath path)) entity
  return (userAttribute, sym)

class HasAnnotations a where
  getAnnotations :: a -> Maybe [Glass.Annotation]

instance HasAnnotations a => HasAnnotations [a] where
  getAnnotations [] = Nothing
  getAnnotations anns = Just $ concat $ catMaybes $ getAnnotations <$> anns

instance HasAnnotations AnnotationsSymbolId where
  getAnnotations (AnnotationsSymbolId (ann, syms)) = case (ann, syms) of
    (Code.Annotations_cxx ann, _) -> getAnnotations ann
    (Code.Annotations_hack ann, syms) -> getAnnotations (ann, syms)
    (Code.Annotations_python{}, _) -> Nothing -- Not yet supported
    (Code.Annotations_thrift{}, _) -> Nothing -- Not yet supported
    (Code.Annotations_java{}, _) -> Nothing -- Not yet supported
    (Code.Annotations_EMPTY, _) -> Nothing

instance HasAnnotations Cxx1.Annotations where
  getAnnotations (Cxx1.Annotations_attributes anns) = getAnnotations anns
  getAnnotations Cxx1.Annotations_EMPTY = Nothing

instance HasAnnotations Cxx1.Attribute where
  getAnnotations Cxx1.Attribute{attribute_key=Just key} = Just
    [ Glass.Annotation
        { annotation_source = prettyCxxAnnotation key
        , annotation_name = key
        , annotation_symbol = Nothing
        }
    ]
  getAnnotations _ = Nothing

instance HasAnnotations (Hack.Annotations, [Maybe Glass.SymbolId]) where
  getAnnotations (Hack.Annotations_attributes anns, syms) =
    getAnnotations (anns, syms)
  getAnnotations (Hack.Annotations_EMPTY, _) = Nothing

instance HasAnnotations ([Hack.UserAttribute], [Maybe Glass.SymbolId]) where
  getAnnotations (attrs, syms) = getAnnotations $ zip attrs syms

instance HasAnnotations (Hack.UserAttribute, Maybe Glass.SymbolId) where
  getAnnotations
    (Hack.UserAttribute
      {
        userAttribute_key = Just Hack.UserAttribute_key
          {
            userAttribute_key_name=Hack.Name{name_key=Just name}
          , userAttribute_key_parameters=args
          }
      }, symbol) = Just
          [ Glass.Annotation
              { annotation_source = prettyHackAnnotation name args
              , annotation_name = name
              , annotation_symbol = symbol
              }
          ]
  getAnnotations _ = Nothing
