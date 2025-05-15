{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Handler.Cxx
  (
  -- * C++ specific methods
    fileIncludeLocations
  , clangUSRToDefinition
  , usrToDefinition
  ) where

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( MonadThrow(throwM) )
import Control.Monad ( forM )
import qualified Data.Map.Strict as Map
import Data.Foldable ( find )

import Glean.Glass.Base
import Glean.Glass.SymbolId
import Glean.Haxl as Glean ( keyOf, haxlRepo )
import Glean.Haxl.Repos as Glean ( RepoHaxl )
import qualified Glean.Glass.Env as Glass
import Glean.Glass.Handler.Utils
import Glean.Glass.Repos
import Glean.Glass.Logging ( errorsText, QueryEachRepoLog )
import Glean.Glass.Path
import Glean.Glass.Types
import Glean.Glass.Range

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src

import qualified Glean.Glass.Query.Cxx as Cxx
import Data.Maybe (fromMaybe)

fileIncludeLocations
  :: Glass.Env
  -> FileIncludeLocationRequest
  -> RequestOptions
  -> IO FileIncludeLocationResults
fileIncludeLocations env@Glass.Env{..} req opts = do
  fmap fst $ do
  withRepoFile "fileIncludeLocations" env opts
    req repo rootfile $ \gleanDBs _ _ ->
      backendRunHaxl GleanBackend{..} env $ do
        result <- firstOrErrors $ do
          rev <- getRepoHash <$> Glean.haxlRepo
          efile <- getFile (toGleanPath (SymbolRepoPath repo rootfile))
          case efile of
            Left err -> return (Left err)
            Right file -> do
              includes <- Cxx.fileIncludeLocationsForCxx depth mlimit file
              Right <$> processFileIncludes repo rev includes
        case result of
          Left err -> throwM $ ServerException $ errorsText err
          Right (efile, gleanDataLog) ->
            return ((efile, gleanDataLog), Nothing)
  where
    repo = fileIncludeLocationRequest_repository req
    rootfile = fileIncludeLocationRequest_filepath req
    depth_ = fileIncludeLocationRequest_depth req
    depth | depth_ <= 0 = 1
          | otherwise = fromIntegral depth_
    mlimit = fromIntegral <$> requestOptions_limit opts


-- | Lookup the USR in Glean to yield and entity,
-- compute entity declToDef, return the pairs and other info.
clangUSRToDefinition
  :: Glass.Env
  -> USRHash
  -> RequestOptions
  -> IO (USRSymbolDefinition, QueryEachRepoLog)
clangUSRToDefinition env@Glass.Env{..} usr@(USRHash hash) opts = do
  withRepoLanguage "clangUSRToDefinition" env usr repo mlang opts $
    \gleanDBs _ _ -> do
      backendRunHaxl GleanBackend{..} env $ do
        result <- firstOrErrors $ do
          mdefn <- Cxx.usrHashToDeclaration hash
          defnResToUSRSymbolDefinition mdefn repo
        case result of
          Left err -> throwM $ ServerException $ errorsText err
          Right defn -> return (defn, Nothing)
  where
    repo = RepoName "fbsource"
    mlang = Just Language_Cpp

usrToDefinition ::
  Glass.Env ->
  USRToDefinitionRequest ->
  RequestOptions ->
  IO (USRSymbolDefinition, QueryEachRepoLog)
usrToDefinition
  env@Glass.Env{..}
  req@(USRToDefinitionRequest (USR usr) mRepo)
  opts =
    withRepoLanguage "usrToDefinition" env req repo (Just lang) opts $
      \gleanDBs _ _ -> do
        backendRunHaxl GleanBackend{..} env $ do
          result <- firstOrErrors $ do
            mdefn <- Cxx.usrToDeclaration usr
            defnResToUSRSymbolDefinition mdefn repo
          case result of
            Left err -> throwM $ ServerException $ errorsText err
            Right defn -> return (defn, Nothing)
   where
    lang = Language_Swift
    repo =
      fromMaybe
        (RepoName "fbsource")
        ( mRepo
          <|> fmap fst
              ( find
                  (any ((== lang) . snd) . snd)
                  (Map.toList $ gleanIndices repoMapping)
              )
        )

defnResToUSRSymbolDefinition
  :: Maybe (Code.Location, Code.Entity) -- ^
  -> RepoName -- ^
  -> Glean.RepoHaxl u w (Either GlassExceptionReason USRSymbolDefinition)
defnResToUSRSymbolDefinition mdefn repo = do
  rev <- getRepoHash <$> Glean.haxlRepo
  case mdefn of
    Nothing -> -- either usr is unknown or decl is already defn
      pure (Left (GlassExceptionReason_entitySearchFail
        "No definition result for USR"))
    Just (Code.Location{..}, entity) -> do
      range <- case location_destination of
        Just Src.FileLocation{..} -> do
          let rangeSpan = Code.RangeSpan_span fileLocation_span
          rangeSpanToLocationRange repo fileLocation_file rangeSpan
        Nothing -> rangeSpanToLocationRange repo location_file
          location_location
      path <- GleanPath <$> Glean.keyOf location_file
      sym <- toSymbolId (fromGleanPath repo path) entity
      pure (Right (USRSymbolDefinition {
        uSRSymbolDefinition_location = range,
        uSRSymbolDefinition_revision = rev,
        uSRSymbolDefinition_sym = sym
      }))

-- | Scrub all glean types for export to the client
-- And flatten to lists for GraphQL.
processFileIncludes
  :: RepoName
  -> Revision
  -> Map.Map Src.File [(Src.File , Src.Range)]
  -> Glean.RepoHaxl u w FileIncludeLocationResults
processFileIncludes repo rev xmap = do
  forExport <- forM (Map.toList xmap) $ \(file, xrefs) -> do
    key <- GleanPath <$> Glean.keyOf file
    refs <- forM xrefs $ \(targetFile, srcRange) -> do
      targetPath <- GleanPath <$> Glean.keyOf targetFile
      let range = inclusiveRangeToExclusiveRange srcRange
      pure (FileXRefTarget (symbolPath $ fromGleanPath repo targetPath) range)
    pure FileIncludeXRef {
      fileIncludeXRef_source = symbolPath (fromGleanPath repo key),
      fileIncludeXRef_includes = refs
    }
  pure FileIncludeLocationResults {
    fileIncludeLocationResults_references = XRefFileList forExport,
    fileIncludeLocationResults_revision = rev
  }
