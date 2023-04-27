{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, PartialTypeSignatures, NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Glean.Search.Search
  ( getSearchRepos

  , findEntities
  , EntityRefs(..)
  , SearchQuery(..)
  , findCxxDecls
  , findHackDecls
  , findHsDecls
  , findJavaDecls
  ) where

import Control.Monad
import Data.Aeson hiding ((.=))
import qualified Data.HashSet as HashSet
import Data.Int
import qualified Data.Map as Map
import Data.Maybe
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Word
import GHC.Generics

import Util.Async (runHereOrConcurrently)

import Glean (limit)
import Glean.Angle as Angle
import qualified Glean
import Glean.Repo
import Glean.Schema.Code.Types as Code
import Glean.Schema.CodeCxx.Types as Cxx
import Glean.Schema.CodeHack.Types as Hack
import Glean.Schema.Hs.Types as Hs
import Glean.Schema.CodeHs.Types as Hs
import Glean.Schema.CodePp.Types as Pp
import Glean.Schema.Cxx1.Types as Cxx
import Glean.Schema.Hack.Types as Hack
import Glean.Schema.Pp1.Types as Pp
import Glean.Schema.SearchCxx.Types as Cxx
import Glean.Schema.SearchHack.Types as Hack
import Glean.Schema.SearchPp.Types as Pp
import Glean.Schema.Src.Types as Src
import Glean.Search.Types as Search
import Glean.Util.Range as Range
import Glean.Util.SchemaRepos
import Glean.Util.Some (Some)
import Glean.Util.ToAngle

-- -----------------------------------------------------------------------------
-- Repository discovery

-- | Build the 'Repos' for the 'defaultSchemaRepos'
getSearchRepos
  :: Glean.Backend e
  => e
  -> (Glean.NoDatabase -> Text -> IO ())
  -> IO Repos
getSearchRepos backend logMissing = do
  let allRepoNames = schemaRepoNames defaultSchemaRepos
  lr <- getLatestRepos backend (`HashSet.member` allRepoNames)
  traverse (resolve lr) defaultSchemaRepos
  where
  resolve :: LatestRepos -> [RepoName] -> IO [Glean.Repo]
  resolve lr = fmap catMaybes . mapM (optionalDB lr)

  optionalDB :: LatestRepos -> RepoName -> IO (Maybe Glean.Repo)
  optionalDB lr name = do
    let mr = Map.lookup name (latestRepos lr)
    when (isNothing mr) $
      logMissing (NoDatabase $ "no database found for: " <> name) name
    return mr

-- -----------------------------------------------------------------------------
-- Searching

data EntityRefs = EntityRefs
  { repo :: Glean.Repo
  , decl :: Code.Entity
  , xrefs :: [FileXRef]
  } deriving Generic

instance ToJSON EntityRefs


findEntities
  :: Maybe Int
  -> Some Glean.Backend
  -> Repos
  -> SearchQuery
  -> Bool -- ^ Also find references?
  -> IO [EntityRefs]
findEntities lim backend SchemaRepos{..} q@SearchQuery{..} refs = do
  let
    ifLang ls x
      | maybe True (\xs -> any (`elem` xs) ls) languages = Just x
      | otherwise = Nothing
    cxxLang = [ Src.Language_C, Src.Language_Cpp
      , Src.Language_ObjC, Src.Language_ObjCpp ]
    todo = catMaybes
      [ ifLang cxxLang $
        mapM (\r -> findCxxDecls lim backend r q refs) cxxRepo
      , ifLang [Src.Language_Haskell] $
        mapM (\r -> findHsDecls lim backend r q) hsRepo
      , ifLang [Src.Language_Java] $
        mapM (\r -> findJavaDecls lim backend r q) javaRepo
      , ifLang [Src.Language_Hack] $
        mapM (\r -> findHackDecls lim backend r q) hackRepo
      ]
  concat . concat <$> runHereOrConcurrently todo

-- | Search for Haskell declarations
findHsDecls
  :: Maybe Int
  -> Some Glean.Backend
  -> Glean.Repo
  -> SearchQuery
  -> IO [EntityRefs]
findHsDecls lim backend repo SearchQuery{..} = do
  let q = predicate @Hs.FunctionDefinition $
            rec $ field @"name" (string query) end
  r <- fmap fst $ Glean.runQuery backend repo $ maybe id limit lim
    (Glean.recursive (Angle.query q))
  return $
    map (\x -> EntityRefs repo (Code.Entity_hs (Hs.Entity_function_ x)) []) r


-- | Search for Java declarations
findJavaDecls
  :: Maybe Int
  -> Some Glean.Backend
  -> Glean.Repo
  -> SearchQuery
  -> IO [EntityRefs]
findJavaDecls _ _ _ _ = return []


-- | Search for C/C++/ObjectiveC entities
findCxxDecls
  :: Maybe Int
  -> Some Glean.Backend
  -> Glean.Repo
  -> SearchQuery
  -> Bool
  -> IO [EntityRefs]
findCxxDecls lim backend repo SearchQuery{..} refs = do
  let
    (namespaces, ident) = case Text.splitOn "::" query of
      [] -> ([], "")
      [name] -> ([], name)
      components -> (init components, last components)

    scopeQuery :: Angle Cxx.Scope
    scopeQuery = scope (reverse namespaces)
      where
      scope :: [Text] -> Angle Cxx.Scope
      scope ns = case ns of
        [] -> wild
        n:ns' ->
            (alt @"namespace_" (namespaceQNameQuery ns))
            .|
            (alt @"recordWithAccess"
              ((rec $
                field @"record"
                  (rec $
                    field @"name" (string n) $
                    field @"scope" (scope ns') end) end)
                ))

    namespaceQNameQuery [] = wild
    namespaceQNameQuery (n:ns) =
      rec $
        field @"name" (alt @"just" (predicate (string n))) $
        field @"parent"
          (if null ns
            then wild -- allow any amount of parents
            else alt @"just" (predicate (namespaceQNameQuery ns))) $
        end

    findXRefs :: Cxx.Entity -> Glean.Haxl w [Search.FileXRef]
    findXRefs _ | not refs = return []
    findXRefs target = do
      let
        findTargetUses :: Cxx.Entity -> Glean.Haxl w [Cxx.TargetUses]
        findTargetUses entity =
          fmap (
            map Cxx.entityUses_key_uses .
            mapMaybe Cxx.entityUses_key .
            fst ) $
            Glean.search $ maybe id limit lim $ Glean.recursive $
              Angle.query $
                predicate @Cxx.EntityUses $ rec $
                  field @"entity" (toAngle entity) end

        findFileXRef :: Cxx.TargetUses -> Glean.Haxl w Search.FileXRef
        findFileXRef targetUses =
          case targetUses of
            Cxx.TargetUses _ (Just Cxx.TargetUses_key
              { targetUses_key_file = file@(Src.File _ (Just name))
              , targetUses_key_uses = uses }) -> do
              -- Load line lengths so we can compute line nos from ranges.
              lines <- fmap fst $ Glean.search $ maybe id limit lim $
                Angle.query $ predicate @Src.FileLines $ rec $
                  field @"file" (asPredicate (factId (Glean.getId file))) end
              case lines of
                Src.FileLines _ (Just flk) : _ ->
                  return $ buildXRef name flk uses
                _ -> return $ FileXRef name Nothing
            _ -> error "TargetUses does not pattern match correctly"

      targetUses <- findTargetUses target
      mapM findFileXRef targetUses

    findDeclarations :: Glean.Haxl w [EntityRefs]
    findDeclarations = do
      let
        searchQuery name =
          predicate @Cxx.SearchByNameAndScope $
            rec $
              field @"name" name $
              field @"scope" scopeQuery
              end

        declByName identPat =
           predicate @Cxx.DeclByName $
              rec $
                field @"name_lowercase" (string (Text.toLower ident)) $
                field @"ident" identPat
                end

      results <-
        fmap fst $ Glean.search $ maybe id limit lim $ Glean.recursive $
          Glean.keys $ Angle.query $
            if case_sensitive
              then
                searchQuery (string ident)
              else
                var $ \(n :: Angle Cxx.Name) ->
                  searchQuery (asPredicate n) `where_`
                    [ wild .= declByName (alt @"name" (asPredicate n))
                    ]

      let cppEntities = map Cxx.searchByNameAndScope_key_entity results

      -- Objc methods are searched by Selector, not by Name.
      let
        searchQuery sel =
          predicate @Cxx.SearchBySelector
            (rec $ field @"selector" sel end)
      objcMethods <-
        if not (null namespaces)
          then return []
            -- TODO: scoped searches for Objc methods
          else
            Glean.search_ $ maybe id limit lim $ Glean.recursive $ Glean.keys $
              Angle.query $
                if case_sensitive
                  then
                    searchQuery (array [string ident])
                    -- TODO: extend this to include all selectors with ident
                    -- as the *first* (rather than *only) component
                  else
                    var $ \(s :: Angle Cxx.ObjcSelector) ->
                      searchQuery (asPredicate s) `where_`
                        [ wild .= declByName (alt @"selector" (asPredicate s)) ]

      let objcMethodEntities = map Cxx.searchBySelector_key_entity objcMethods

      let entities = cppEntities ++ objcMethodEntities

      defs <- forM entities $ \entity ->
        case entity of
          Cxx.Entity_decl decl -> do
            results <- fmap fst $ Glean.search $ maybe id limit lim $
              Glean.recursive $ Glean.keys $ Angle.query $
                predicate @Cxx.DeclIsDefn $
                  rec $ field @"decl" (toAngle decl) end
            return $ listToMaybe $ map Cxx.declIsDefn_key_defn results
          _other -> return Nothing

      xrefs <- mapM findXRefs entities

      let
        toEntityRefs ent Nothing xrefs = EntityRefs
          { repo
          , decl = Code.Entity_cxx ent
          , xrefs = xrefs }
        toEntityRefs _ (Just def) xrefs = EntityRefs
          { repo
          , decl = Code.Entity_cxx (Entity_defn def)
          , xrefs = xrefs }

      return (zipWith3 toEntityRefs entities defs xrefs)

    findMacros :: Glean.Haxl w [EntityRefs]
    findMacros
      | not (null namespaces) = return []
      | otherwise = do
      let
        searchQuery m =
          predicate @Pp.SearchByName $
            rec $ field @"macro" m end
      results <-
        fmap fst $ Glean.search $ maybe id limit lim $ Glean.recursive $
          Glean.keys $ Angle.query $
            if case_sensitive
              then
                searchQuery (string ident)
              else
                var $ \(m :: Angle Pp.Macro) ->
                  searchQuery (asPredicate m) `where_`
                    [ wild .=  predicate @Cxx.DeclByName (rec $
                        field @"name_lowercase" (string (Text.toLower ident)) $
                        field @"ident" (alt @"macro" (asPredicate m))
                        end)
                    ]
      let entities = map Pp.searchByName_key_entity results
      -- TargetUses doesn't have macro xrefs yet
      return
        [ EntityRefs repo (Code.Entity_pp (Pp.Entity_define ent)) []
        | ent <- entities ]

  Glean.runHaxl backend repo $
    (++) <$> findDeclarations <*> findMacros


buildXRef :: Text -> Src.FileLines_key -> [Src.RelByteSpan] -> FileXRef
buildXRef name flk spans = FileXRef
    { fileXRef_file_name = name
    , fileXRef_line_nos = Just $ map byteRangeToLineNo ranges}
  where
    offsetToLineCol :: Word64 -> (Word64, Word64)
    offsetToLineCol = byteOffsetToLineCol $ lengthsToLineOffsets flk

    byteRangeToLineCol :: Range.ByteRange -> (Word64, Word64)
    byteRangeToLineCol range = offsetToLineCol $ Range.byteRange_begin range

    byteRangeToLineNo :: Range.ByteRange -> Int32
    byteRangeToLineNo = fromIntegral . fst . byteRangeToLineCol

    ranges :: [Range.ByteRange]
    ranges = relByteSpansToRanges spans

-- | Small helper type for parsing Hack search queries for `findHackDecls`
data ParsedHackQuery
  = PlainName Text
  | NamespacedName [Text] Text
  | ContainerName [Text] Text Text

-- | Search @hack.Declaration@ to match 'SearchQuery'
--
-- Expects @ident@ or @namespace\ident@ where the optional @namespace@
-- is one or more @\@ separated non-empty names with optional leading @\@,
-- the @ident@ is a non-empty name or of the form @container::name@ with
-- a non-empty @container@ and non-empty @name@ (note there can be only zero
-- or one @::@ in the @ident@).
--
-- The @'SearchQuery'{'case_sensitive'}@ parameter is honored if and only if
-- it is a plain identifier with  no @namespace@ or @container@ or leading @\@,
-- otherwise it is always a case-sensitive search.
--
-- If the input is ill-formed (e.g. more than one @::@ separator) then this
-- will return no results.
findHackDecls
  :: Maybe Int
  -> Some Glean.Backend
  -> Glean.Repo
  -> SearchQuery
  -> IO [EntityRefs]
findHackDecls lim backend repo SearchQuery{..} = Glean.runHaxl backend repo $
  fmap (map toEntityRefs) $ case parsedQuery of
    Nothing -> return []
    Just (PlainName ident) -> plainName ident
    Just (NamespacedName namespaces ident) -> inNamespace namespaces ident
    Just (ContainerName namespaces container ident) ->
      inContainerOrEnum namespaces container ident
  where
    parsedQuery = do
      guard (not (Text.null query))
      let backslashSegments = Text.splitOn "\\" query
          leadingBackslash = take 1 backslashSegments == [""]
          withoutLeadingBlackslash = if leadingBackslash
            then drop 1 backslashSegments
            else backslashSegments
          namespaces = if null withoutLeadingBlackslash
            then []
            else reverse (init withoutLeadingBlackslash)
      guard (not (any Text.null namespaces))
      let parent_ident = if null withoutLeadingBlackslash
            then ""
            else last withoutLeadingBlackslash
          doubleColonComponents = Text.splitOn "::" parent_ident
      guard (length doubleColonComponents `elem` [1, 2])
      guard (not (any Text.null doubleColonComponents))
      let parent = if length doubleColonComponents == 2
            then Just (head doubleColonComponents)
            else Nothing
      let ident = last doubleColonComponents
      pure $ case parent of
        Just container -> ContainerName namespaces container ident
        Nothing -> if not leadingBackslash && null namespaces
          then PlainName ident
          else NamespacedName namespaces ident

    toEntityRefs :: Hack.Declaration -> EntityRefs
    toEntityRefs d = EntityRefs
      { repo
      , decl = Code.Entity_hack (Hack.Entity_decl d), xrefs = []
      }

    -- -------------------------------------------------------------------------
    -- common helpers for plainName, inNamespace, inContainer

    search :: Angle Hack.Declaration -> Glean.Haxl w [Hack.Declaration]
    search q =
      fmap fst $ Glean.search $ maybe id limit lim $ Glean.recursive $
        Angle.query q

    -- The final namespace parent is always a wildcard
    namespaceOf [] = wild
    namespaceOf (x:xs) =
      just $ predicate $
        rec $
          field @"name" (string x) $
          field @"parent" (namespaceOf xs)
        end

    -- -------------------------------------------------------------------------

    -- This honors the SearchQuery{case_sensitive} value
    plainName :: Text -> Glean.Haxl w [Hack.Declaration]
    plainName ident =
      let
        searchByName decl name =
          predicate @Hack.SearchByName $
            rec $
              field @"name" name $
              field @"decl" decl
            end

        searchLower x =
          predicate @Hack.NameLowerCase $
            rec $
              field @"nameLowercase" (string (Text.toLower ident)) $
              field @"name" x
            end
      in
      search $
        if case_sensitive
          then
            var $ \d -> d `where_`
              [ wild .= searchByName d (string ident) ]
          else
            var $ \d -> var $ \(x :: Angle Hack.Name) -> d `where_`
              [ wild .= searchLower (asPredicate x),
                wild .= searchByName d (asPredicate x) ]

    -- \Foo or Foo\Bar or \Foo\Bar
    -- This ignores the SearchQuery{case_sensitive} value
    -- Decls are either namespaces or things in namespaces
    inNamespace :: [Text] -> Text -> Glean.Haxl w [Hack.Declaration]
    inNamespace ns ident =
      search $ var $ \d -> d `where_` [wild .= searchNamespacedDecl d]
      where
        searchNamespacedDecl d =
          predicate @Hack.SearchNamespacedDecl $
            rec $
              field @"name" (string ident) $
              field @"namespace_" (namespaceOf ns) $
              field @"decl" d
            end

    -- Foo\Bar::Baz or Foo::Bar or \Foo\Bar::Baz or \Foo::Bar
    -- This ignores the SearchQuery{case_sensitive} value
    inContainerOrEnum
      :: [Text] -> Text -> Text -> Glean.Haxl w [Hack.Declaration]
    inContainerOrEnum ns context ident =
      search $ var $ \d -> d `where_` [wild .= searchInContext d]
      where
        searchInContext d =
          predicate @Hack.SearchInContainerOrEnum $
            rec $
              field @"name" (string ident) $
              field @"contextName" (string context) $
              field @"contextNamespace" (namespaceOf ns) $
              field @"decl" d
            end
