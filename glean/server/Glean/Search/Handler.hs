-- Copyright 2004-present Facebook. All Rights Reserved.

module Glean.Search.Handler (searchHandler) where

import Data.Default

import Glean
import Glean.Handler as GleanHandler
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
