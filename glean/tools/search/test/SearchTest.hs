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

import Derive.Lib (DerivePass(..))
import Glean.Clang.Test.DerivePass as DerivePass
import Glean.Regression.Test
import Glean.Schema.CodeCxx.Types as Cxx
import Glean.Schema.Code.Types as Code
import Glean.Schema.Cxx1.Types as Cxx
import Glean.Search.Search
import Glean.Util.Some
import Glean.Util.SchemaRepos


main :: IO ()
main = do
  let driver = DerivePass.driver [DeriveGeneric "cxx1.DeclByName"]
  mainTestIndexGeneric driver (pure ()) "search-test" $
    \_ _ _ _ get -> TestCase $ do

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

      search "local" "local variable" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{}) ] -> True
          _ -> False

      search "facebook::D::i" "public variable (f::D::i)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{}) ] -> True
          _ -> False

      search "D::i" "public variable (D::i)" $ \r ->
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_variable{})] -> True
          _ -> False

      search "i" "public variable (i)" $ \r ->
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
        case sort r of
          [ Entity_cxx (Entity_decl Declaration_objcMethod{}),
            Entity_cxx (Entity_decl Declaration_objcMethod{}),
            Entity_cxx (Entity_decl Declaration_objcProperty{}) ] -> True
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
