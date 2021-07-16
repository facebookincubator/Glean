-- Copyright (c) Facebook, Inc. and its affiliates.


module Glean.Search.Handler (searchHandler) where

import Data.Default

import Glean
import Glean.Backend (LoggingBackend(..))
import Glean.Handler as GleanHandler
import Glean.Schema.Code.Types as Code
import Glean.Schema.CodeCxx.Types as Cxx
import Glean.Schema.CodeHs.Types as Hs
import Glean.Schema.CodeJava.Types as Java
import Glean.Schema.Cxx1.Types as Cxx
import Glean.Search.Graph
import Glean.Search.Search
import Glean.Search.SearchService.Service as Service
import Glean.Search.Types as Search
import Glean.Util.Some

searchHandler :: GleanHandler.State -> SearchServiceCommand a -> IO a
searchHandler state@GleanHandler.State{..} req =
  let backend = LoggingBackend stEnv in
  case req of
    Service.FindDecls repo query -> do
      findDeclsImpl (Some backend) repo query
    Service.FindLocalGraph repo query ->
      findLocalGraphImpl (Some backend) repo query
    SuperGleanService c -> GleanHandler.handler state c

findDeclsImpl :: Some Backend -> Repo -> FindDeclsQuery -> IO FindDeclsResult
findDeclsImpl backend repo FindDeclsQuery
  { findDeclsQuery_query = str
  , findDeclsQuery_refs = refs } = do
  let
    q :: SearchQuery
    q = def { Search.query = str }
    -- we choose to limit the number of results from each part of the search
    lim = Just 100
  entities <- case repo_name repo of
    "fbsource" -> findCxxDecls lim backend repo q refs
    "fbsource.fbandroid" -> findJavaDecls lim backend repo q
    "fbsource-hs" -> findHsDecls lim backend repo q
    _ -> return []
  return $ FindDeclsResult (entityRefsToDeclRefs entities)

findLocalGraphImpl
  :: Some Backend
  -> Repo
  -> FindLocalGraphQuery
  -> IO FindLocalGraphResult
findLocalGraphImpl backend repo FindLocalGraphQuery
  { findLocalGraphQuery_fun_id = fun_id
  , findLocalGraphQuery_max_funs = max_funs
  } = findLocalGraph backend repo fun_id (fromIntegral max_funs)


-- | DEPRECATED
entityRefsToDeclRefs :: [EntityRefs] -> [Search.DeclRefs]
entityRefsToDeclRefs results =
  [ DeclRefs decl xrefs
  | EntityRefs _repo ent xrefs <- results
  , decl <- entityToDecl ent
  ]
  where
  -- Legacy conversion from Entity to Decl, can go away once we migrate
  -- clients from findDecls to findEntities.
  --
  -- Not all Entities have equivalent Decls, so this might filter out
  -- some results.
  entityToDecl :: Code.Entity -> [Search.Decl]
  entityToDecl (Code.Entity_cxx cxxEnt) = cxxEntToDecl cxxEnt
  entityToDecl (Code.Entity_java javaEnt) = javaEntToDecl javaEnt
  entityToDecl (Code.Entity_hs hsEnt) = hsEntToDecl hsEnt
  entityToDecl (Code.Entity_pp decl) = [Decl_macro_decl decl]
  entityToDecl _ = []

  javaEntToDecl (Java.Entity_class_ decl) = [Decl_java_class_decl decl]

  hsEntToDecl (Hs.Entity_function_ decl) = [Decl_hs_fun_def decl]
  hsEntToDecl _ = []

  cxxEntToDecl (Cxx.Entity_decl decl) = cxxDeclToDecl decl
  cxxEntToDecl (Cxx.Entity_defn defn) = cxxDefnToDecl defn
  cxxEntToDecl (Cxx.Entity_enumerator enum) = [Decl_enumerator_def enum]

  cxxDeclToDecl (Cxx.Declaration_function_ decl) = [Decl_fun_decl decl]
  cxxDeclToDecl (Cxx.Declaration_record_ decl) = [Decl_rec_decl decl]
  cxxDeclToDecl (Cxx.Declaration_variable decl) = [Decl_var_decl decl]
  cxxDeclToDecl (Cxx.Declaration_enum_ decl) = [Decl_enum_decl decl]
  cxxDeclToDecl _ = []

  cxxDefnToDecl (Cxx.Definition_function_ def) = [Decl_fun_def def]
  cxxDefnToDecl (Cxx.Definition_record_ def) = [Decl_rec_def def]
  cxxDefnToDecl (Cxx.Definition_enum_ def) = [Decl_enum_def def]
  cxxDefnToDecl _ = []
