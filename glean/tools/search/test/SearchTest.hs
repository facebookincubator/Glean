{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module SearchTest (main) where

import Data.Default
import Data.List
import qualified Data.Text as Text
import Test.HUnit

import qualified Glean
import Derive.Lib (DerivePass(..))
import Glean.Clang.Test.DerivePass as DerivePass
import Glean.Regression.Test
import Glean.Schema.CodeCxx.Types as Cxx
import Glean.Schema.Code.Types as Code
import Glean.Schema.Cxx1.Types as Cxx
import Glean.Schema.Query.Code.Types as Query.Code
import Glean.Schema.Query.Cxx1.Types as Query.Cxx
import Glean.Schema.Query.CodeCxx.Types as Query.Cxx
import Glean.Search.Search
import Glean.Util.EntityUtils (entityToQuery)
import Glean.Util.Some
import Glean.Util.SchemaRepos


main :: IO ()
main = do
  let driver = DerivePass.driver [DeriveGeneric "cxx1.DeclByName"]
  mainTestIndexGeneric driver "search-test" $
    \_ platform _ get -> TestCase $ do

      (backend, repo) <- get

      let repos = mempty{ cxxRepo = [repo] }
          search_ s t f cs = do
            print (s, cs)
            let q = def { query = s, case_sensitive = cs }
                lim = Just 100
            rs <- map decl <$> findEntities lim (Some backend) repos q False
            putStrLn $ "Result count: " <> show (length rs)
            print rs
            assertBool t (f rs)
            mapM_ (checkQueryRoundtrip backend repo t) rs
            mapM_ (checkQueryRoundtrip2 backend repo t) rs

          search s t f = do
            search_ s t f True
            case Text.breakOnEnd "::" s of
              (ns, n) -> do
                search_ (ns <> Text.toLower n) t f False
                search_ (ns <> Text.toUpper n) t f False

{- TODO: broken

      search "facebook" "namespace facebook" $ \r ->
       case sort r of
         [ Entity_cxx (Entity_defn Definition_namespace_{}) ] -> True
         _ -> False

      search "glean" "namespace glean" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_namespace_{}) ] -> True
          _ -> False

      search "worklist" "namespace worklist" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_namespace_{}) ] -> True
          _ -> False

      search "glean::worklist" "namespace glean::worklist" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_namespace_{}) ] -> True
          _ -> False

      search "facebook::glean" "namespace facebook::glean" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_namespace_{}) ] -> True
          _ -> False

      search "facebook::glean::worklist"
        "namespace facebook::glean::worlist" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_namespace_{}) ] -> True
          _ -> False
-}

      search "Counter" "struct Counter" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "Value" "struct Value" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "worklist::Counter" "struct worklist::Counter" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "worklist::Counter::Value"
        "struct worklist::Counter::Value" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "glean::worklist::Counter"
        "struct glean::worklist::Counter" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "glean::worklist::Counter::Value"
        "struct glean::worklist::Counter::Value" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "facebook::glean::worklist::Counter"
        "struct facebook::glean::worklist::Counter" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "facebook::glean::worklist::Counter::Value"
        "struct facebook::glean::worklist::Counter::Value" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "f" "function decl/def" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_function_{}),
            Entity_cxx (Entity_defn Definition_function_{}) ] -> True
          _ -> False

      search "facebook::f" "function decl/def (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_function_{}),
            Entity_cxx (Entity_defn Definition_function_{}) ] -> True
          _ -> False

      search "C" "class decl/def" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_record_{}),
            Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "facebook::C" "class decl/def (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_record_{}),
            Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "D" "struct decl/def" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_record_{}),
            Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "facebook::D" "struct decl/def (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_record_{}),
            Entity_cxx (Entity_defn Definition_record_{}) ] -> True
          _ -> False

      search "get" "method decl/def" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_function_{}),
            Entity_cxx (Entity_defn Definition_function_{}) ] -> True
          _ -> False

      search "facebook::C::get" "method decl & def (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_function_{}),
            Entity_cxx (Entity_defn Definition_function_{}) ] -> True
          _ -> False

      search "TEST" "macro def" $ \r ->
        case sort r of
          [ Entity_pp{} ] -> True
          _ -> False

      -- shouldn't find the macro when we give a namespace
      search "facebook::TEST" "macro def (scoped)" $ \r ->
        case r of
          [] -> True
          _ -> False

      search "global" "global variable" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{}),
            Entity_cxx (Entity_defn Definition_variable{}) ] -> True
          _ -> False

      search "facebook::D::x" "public variable (f::D::x)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{}) ] -> True
          _ -> False

      search "D::x" "public variable (D::x)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{})] -> True
          _ -> False

      search "x" "public variable (x)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{})] -> True
          _ -> False

      search "facebook::C::a" "private variable (f::C::a)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{}) ] -> True
          _ -> False

      search "C::a" "private variable (C::a)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{})] -> True
          _ -> False

      search "a" "private variable (a)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{})] -> True
          _ -> False

      search "facebook::global" "global variable (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{}),
            Entity_cxx (Entity_defn Definition_variable{}) ] -> True
          _ -> False

      search "T" "type alias" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_typeAlias{}) ] -> True
          _ -> False

      search "e" "enum" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_enum_{}) ] -> True
          _ -> False

      search "facebook::e" "enum (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_enum_{}) ] -> True
          _ -> False

      search "p" "enumerator" $ \r ->
        case sort r of
          [ Entity_cxx Entity_enumerator{} ] -> True
          _ -> False

      search "facebook::p" "enumerator (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx Entity_enumerator{} ] -> True
          _ -> False

      search "EE" "enum class" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_enum_{}) ] -> True
          _ -> False

      search "facebook::EE" "enum class (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_defn Definition_enum_{}) ] -> True
          _ -> False

      search "EE::r" "enumerator of enum class" $ \r ->
        case sort r of
          [ Entity_cxx Entity_enumerator{} ] -> True
          _ -> False

      search "facebook::EE::r" "enumerator of enum class (scoped)" $ \r ->
        case sort r of
          [ Entity_cxx Entity_enumerator{} ] -> True
          _ -> False

      search "meth1" "objc method" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_objcMethod{}),
            Entity_cxx (Entity_defn Definition_objcMethod{}) ] -> True
          _ -> False

      search "prop1" "objc property" $ \r ->
        case (sort r, platform) of
          -- For some reason we get a method and a property...
          ([ Entity_cxx (Entity_decl Declaration_objcMethod{}),
            Entity_cxx (Entity_decl Declaration_objcProperty{})],
             "platform009") -> True
          -- llvm12-based indexer returns one extra method (property setter)
          ([ Entity_cxx (Entity_decl Declaration_objcMethod{}),
            Entity_cxx (Entity_decl Declaration_objcMethod{}),
            Entity_cxx (Entity_decl Declaration_objcProperty{})],
            "platform009-clang-12" ) -> True
          ([ Entity_cxx (Entity_decl Declaration_objcMethod{}),
            Entity_cxx (Entity_decl Declaration_objcMethod{}),
            Entity_cxx (Entity_decl Declaration_objcProperty{})],
            "platform010" ) -> True
          _ -> False

      -- TODO: protocol
      search "AA" "objc interface/implementation" $ \r ->
        length r == 4 &&
        length (filter (isObjcContainer isInterface) r) == 1 &&
        length (filter (isObjcContainer isImplementation) r) == 1 &&
        length (filter (isObjcContainer isCategoryInterface) r) == 1 &&
        length (filter (isObjcContainer isCategoryImplementation) r) == 1

      search "_i" "objc variable" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{}) ] -> True
          _ -> False

isObjcContainer :: (Cxx.ObjcContainerId -> Bool) -> Code.Entity -> Bool
isObjcContainer test
  (Entity_cxx (Entity_defn
    (Definition_objcContainer
      (ObjcContainerDefinition _ (Just
        Cxx.ObjcContainerDefinition_key
        { objcContainerDefinition_key_declaration =
          (ObjcContainerDeclaration _ (Just
            Cxx.ObjcContainerDeclaration_key {
              objcContainerDeclaration_key_id = id }))}))))) = test id
isObjcContainer _ _ = False

isInterface :: Cxx.ObjcContainerId -> Bool
isInterface ObjcContainerId_interface_{} = True
isInterface _ = False

isImplementation :: Cxx.ObjcContainerId -> Bool
isImplementation ObjcContainerId_implementation{} = True
isImplementation _ = False

isCategoryInterface :: Cxx.ObjcContainerId -> Bool
isCategoryInterface ObjcContainerId_categoryInterface{} = True
isCategoryInterface _ = False

isCategoryImplementation :: Cxx.ObjcContainerId -> Bool
isCategoryImplementation ObjcContainerId_categoryImplementation{} = True
isCategoryImplementation _ = False

-- | Test that we can roundtrip the queries generated by
-- entityToQuery to find the same declarations again. This is
-- a bit painful because we can't query for Entity directly,
-- as it isn't a fact.
checkQueryRoundtrip
  :: Glean.Backend e
  => e
  -> Glean.Repo
  -> String
  -> Code.Entity
  -> IO ()
checkQueryRoundtrip backend repo tag ent@(Entity_cxx e) =
  mapM_ (checkCxx e) (entity_cxx q)
  where
  q = entityToQuery ent

  checkCxx (Entity_decl d) Query.Cxx.Entity{..} = do
    mapM_ (checkDecl d) entity_decl
  checkCxx (Entity_defn d) Query.Cxx.Entity{..} = do
    mapM_ (checkDefn d) entity_defn
  checkCxx (Entity_enumerator d) Query.Cxx.Entity{..} = do
    mapM_ (check d) entity_enumerator
  checkCxx Cxx.Entity_EMPTY _ = return ()

  checkDefn (Definition_record_ d) Query.Cxx.Definition{..} =
    mapM_ (check d) definition_record_
  checkDefn (Definition_function_ d) Query.Cxx.Definition{..} =
    mapM_ (check d) definition_function_
  checkDefn (Definition_enum_ d) Query.Cxx.Definition{..} =
    mapM_ (check d) definition_enum_
  checkDefn (Definition_objcMethod d) Query.Cxx.Definition{..} =
    mapM_ (check d) definition_objcMethod
  checkDefn (Definition_objcContainer d) Query.Cxx.Definition{..} =
    mapM_ (check d) definition_objcContainer
  checkDefn (Definition_variable d) Query.Cxx.Definition{..} =
    mapM_ (check d) definition_variable
  checkDefn _ _ = return ()

  checkDecl (Declaration_function_ d) Declaration{..} =
    mapM_ (check d) declaration_function_
  checkDecl (Declaration_record_ d) Declaration{..} =
    mapM_ (check d) declaration_record_
  checkDecl (Declaration_enum_ d) Declaration{..} =
    mapM_ (check d) declaration_enum_
  checkDecl (Declaration_variable d) Declaration{..} =
    mapM_ (check d) declaration_variable
  checkDecl (Declaration_objcContainer d) Declaration{..} =
    mapM_ (check d) declaration_objcContainer
  checkDecl (Declaration_objcMethod d) Declaration{..} =
    mapM_ (check d) declaration_objcMethod
  checkDecl (Declaration_objcProperty d) Declaration{..} =
    mapM_ (check d) declaration_objcProperty
  checkDecl _ _ = return ()

  -- The query can return more results, because the query might match
  -- multiple declarations and definitions.  We'll just check that the
  -- original one is amongst the results.
  check
    :: (Show p, Glean.ThriftQuery p)
    => p
    -> Glean.QueryOf p
    -> IO ()
  check p d = do
    r <- Glean.runQuery_ backend repo $ Glean.query d
    assertBool ("roundtrip " ++ tag) $
      -- Note: not equality, these values are expanded to different depths.
      Glean.getId p `elem` map Glean.getId r

checkQueryRoundtrip _ _ _ _ = return ()

-- | Test that we can roundtrip the queries generated by
-- entityToQuery to find the same declarations again. This is
-- a bit painful because we can't query for Entity directly,
-- as it isn't a fact.
checkQueryRoundtrip2
  :: Glean.Backend e
  => e
  -> Glean.Repo
  -> String
  -> Code.Entity
  -> IO ()
checkQueryRoundtrip2 backend repo tag ent@Entity_cxx{} = do
  let repoNameToRepo fsr = case fromSchemaRepos fsr defaultSchemaRepos of
        ["fbsource"] -> return repo
        other -> assertFailure $
          "checkQueryRoundtrip2 " <> tag <>
          " requires fbsource, got: " <> show other
  let (fsr, entityQuery) = queryToEntity (entityToQuery ent)
  repo <- repoNameToRepo fsr
  ents <- entityQuery (Some backend) repo (ModifyQuery Glean.recursive)
  assertBool ("checkQueryRoundtrip2 " ++ tag) $
    ent `elem` ents

checkQueryRoundtrip2 _ _ _ _ = return ()
