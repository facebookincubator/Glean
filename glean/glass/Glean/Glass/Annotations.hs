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

  -- * testing
  , prettyHackAnnotation
  ) where


import Data.Text (Text)
import qualified Data.Text as Text
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
import qualified Glean.Schema.CodePython.Types as Python
import qualified Glean.Schema.CodeJava.Types as Java
import qualified Glean.Schema.Cxx1.Types as Cxx1
import qualified Glean.Schema.Hack.Types as Hack
import qualified Glean.Schema.Python.Types as Python
import qualified Glean.Schema.JavaAlpha.Types as Java
import qualified Glean.Schema.JavakotlinAlpha.Types as JavaKotlin

import Glean.Glass.SymbolId (entityToAngle, toSymbolId)
import Glean.Glass.SymbolId.Java ( flattenPath )
import qualified Glean.Glass.Types as Glass
import Glean.Glass.Utils ( searchRecursiveWithLimit )
import Glean.Glass.Path ( fromGleanPath )

-- | For Hack, Java and Python, the annotations are a single array fact.
-- For C++ they're sprinkled across each function decl, forcing us to search
max_annotations_limit :: Int
max_annotations_limit = 10

getAnnotationsForEntity
  :: Glass.RepoName
  -> Code.Entity
  -> Glean.RepoHaxl u w (Either Text (Maybe [Glass.Annotation]))
getAnnotationsForEntity repo entity = fetch repo `mapM` entityToAngle entity

-- Get all the annotations for a given entity. This may include
-- computing symbolId for these annotations.
fetch
  :: Glass.RepoName -> Angle Code.Entity
  -> Glean.RepoHaxl u w (Maybe [Glass.Annotation])
fetch repo ent = do
  anns <- queryAnnotations ent
  annsWithSyms <- forM anns (annotationsToSymbols repo)
  getAnnotations annsWithSyms

-- | Entity to its annotation facts.
queryAnnotations :: Angle Code.Entity -> Glean.RepoHaxl u w [Code.Annotations]
queryAnnotations entity =
  fmap fst <$> searchRecursiveWithLimit (Just max_annotations_limit) $
    entityAnnotations entity

entityAnnotations :: Angle Code.Entity -> Angle Code.Annotations
entityAnnotations ent =
  var $ \anns -> anns `where_` [
    wild .= Angle.predicate @Code.EntityToAnnotations (
        rec $
          field @"entity" ent $
          field @"annotations" anns
        end)
  ]

--
-- Per-language unpacking of annotations
--

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
queryAttributeToDecl attrs = fmap fst <$> searchRecursiveWithLimit
    (Just (length attrs)) $
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

--
-- The type of annotations is language specific. We unwrap here in the client
-- this also lets us do a bit of language-specific processing
--
-- We should think about encoding this in the schema Glean-side
--
class HasAnnotations a where
  getAnnotations :: a -> Glean.RepoHaxl u w (Maybe [Glass.Annotation])

instance HasAnnotations a => HasAnnotations [a] where
  getAnnotations [] = pure Nothing
  getAnnotations anns = do
    result <- mapM getAnnotations anns
    return $ Just (concat (catMaybes result))
    -- we could drop empty results here, and not emit the attr at all?

instance HasAnnotations AnnotationsSymbolId where
  getAnnotations (AnnotationsSymbolId (ann, syms)) = case (ann, syms) of
    (Code.Annotations_cxx ann, _) -> getAnnotations ann
    (Code.Annotations_hack ann, syms) -> getAnnotations (ann, syms)
    (Code.Annotations_python anns, _) -> getAnnotations anns
    (Code.Annotations_java anns, _) -> getAnnotations anns
    (Code.Annotations_thrift{}, _) -> pure Nothing -- Not yet supported
    (Code.Annotations_EMPTY, _) -> pure Nothing

instance HasAnnotations Cxx1.Annotations where
  getAnnotations (Cxx1.Annotations_attributes anns) = getAnnotations anns
  getAnnotations Cxx1.Annotations_EMPTY = pure Nothing

instance HasAnnotations Cxx1.Attribute where
  getAnnotations Cxx1.Attribute{attribute_key=Just key} = return $ Just
    [ Glass.Annotation
        { annotation_source = key
        , annotation_name = key
        , annotation_symbol = Nothing
        }
    ]
  getAnnotations _ = pure Nothing

instance HasAnnotations Python.Annotations where
  getAnnotations (Python.Annotations_decorators anns) = getAnnotations anns
  getAnnotations Python.Annotations_EMPTY = pure Nothing

instance HasAnnotations Python.Decorator where
  getAnnotations key = return $ Just $ pure Glass.Annotation {
          annotation_source = key,
          annotation_symbol = Nothing,
          annotation_name = key
        }

instance HasAnnotations Java.Annotations where
  getAnnotations (Java.Annotations_annotations anns) = getAnnotations anns
  getAnnotations Java.Annotations_EMPTY = pure Nothing

instance HasAnnotations Java.Annotation where
  getAnnotations ann = do
    Java.Annotation_key mname _ctor _constant _span <- Glean.keyOf ann
    JavaKotlin.QName_key qName mcontext <- Glean.keyOf mname
    nameStr <- Glean.keyOf qName
    context <- Glean.keyOf mcontext
    toks <- flattenPath context
    let annKey = Text.intercalate "." (nameStr : reverse toks)

    return $ Just $ pure Glass.Annotation {
          annotation_source = nameStr,
          annotation_symbol = Nothing, -- annotations don't have definitions yet
          annotation_name = annKey
        }

instance HasAnnotations (Hack.Annotations, [Maybe Glass.SymbolId]) where
  getAnnotations (Hack.Annotations_attributes anns, syms) =
    getAnnotations (anns, syms)
  getAnnotations (Hack.Annotations_EMPTY, _) = pure Nothing

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
      }, symbol) = return $ Just
          [ Glass.Annotation
              { annotation_source = prettyHackAnnotation name args
              , annotation_name = name
              , annotation_symbol = symbol
              }
          ]
  getAnnotations _ = pure Nothing

prettyHackAnnotation :: Text -> [Text] -> Text
prettyHackAnnotation name [] = name
prettyHackAnnotation name args = name <> parens (commas (map quotes args))
  where
    parens x = "(" <> x <> ")"
    commas = Text.intercalate ", "
    quotes arg = "\"" <> arg <> "\""
