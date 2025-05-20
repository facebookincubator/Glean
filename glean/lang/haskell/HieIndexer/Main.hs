{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module HieIndexer.Main (main) where

import Control.Monad
import Control.Monad.State (StateT, evalStateT, get, liftIO, put)
import qualified Data.ByteString as BS
import Data.Default
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as AMap
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.Directory
import System.FilePath
import qualified Thrift.Protocol.Compact
import Text.Printf

import HieDb (withHieFile)
import HieDb.Utils (makeNc)
import HieDb.Types (runDbM)

import Util.EventBase
import Util.Log

import qualified Glean
import qualified Glean.LocalOrRemote as Glean
import Glean.Impl.ConfigProvider (ConfigAPI)
import Glean.Init (withOptions)
import Glean.Schema.Builtin.Types (schema_id)
import qualified Glean.Schema.Hs as Hs
import qualified Glean.Schema.Src as Src
import qualified Glean.Util.ConfigProvider as Glean

import qualified HieIndexer.Options as Hie
import HieIndexer.Index
import HieIndexer.Options

main :: IO ()
main = do
  let parser =
        (,) <$> infoParser Hie.options <*> Glean.options
      opts = info (helper <*> parser) fullDesc
  withOptions opts $ \((cfg, mode), svc) ->
    withEventBaseDataplane $ \evb ->
      Glean.withConfigProvider Glean.defaultConfigOptions $
        \(cfgAPI :: ConfigAPI) ->
          Glean.withBackendWithDefaultOptions
            evb
            cfgAPI
            svc
            (case mode of
              BinaryMode _ given_schema_id -> Just given_schema_id
              _ -> Just schema_id)
            $ case mode of
                Hie.BinaryMode{..} ->
                  outputMain cfg outputPath schemaId
                Hie.WriteMode{..} ->
                  defaultMain cfg repo

defaultMain
  :: Glean.Backend b
  => HieIndexerOptions
  -> Glean.Repo
  -> b
  -> IO ()
defaultMain cfg repo backend = do
  paths <- getHieFilesIn (NonEmpty.toList (hiePaths cfg))

  logInfo $ printf "Indexing %d files" (HashSet.size paths)

  let Glean.Repo{..} = repo

  logInfo $ printf "Writing to Glean DB %s/%s" repo_name repo_hash

  Glean.withSender backend repo allPredicates def $ \sender -> do
    Glean.withWriter sender def $
      indexHieFiles paths (srcPaths cfg)

  predicates <-
    Glean.schemaInfo_predicateIds
      <$> Glean.getSchemaInfo backend (Just repo)
            def { Glean.getSchemaInfo_omit_source = True }
  repoStats <- Glean.predicateStats backend repo Glean.ExcludeBase
  let readableStats =
        [ printf " - %s: count = %d, size = %d"
          (Text.unpack predicateRef_name)
          predicateStats_count
          predicateStats_size
        | (p, Glean.PredicateStats {..}) <- AMap.toList repoStats
        , Just Glean.PredicateRef {..} <- [AMap.lookup p predicates]
        ]

  logInfo $ unlines $ "Repo stats: " : readableStats

allPredicates :: [Glean.SchemaPredicates]
allPredicates = [Src.allPredicates, Hs.allPredicates]

outputMain
  :: Glean.Backend b
  => HieIndexerOptions
  -> FilePath
  -> Glean.SchemaId
  -> b
  -> IO ()
outputMain cfg out schema_id backend = do
  paths <- getHieFilesIn (NonEmpty.toList (hiePaths cfg))
  ((), batch) <-
    Glean.withBatchWriter backend schema_id Nothing def $
      indexHieFiles paths (srcPaths cfg)
  BS.writeFile out (Thrift.Protocol.Compact.serializeCompact batch)

indexHieFiles
  :: HashSet.HashSet FilePath
  -> NonEmpty Text
  -> Glean.Writer
  -> IO ()
indexHieFiles paths srcs writer =
  forM_ (HashSet.toList paths) $ \f -> do
    nc <- newIORef =<< makeNc
    runDbM nc $ do
      withHieFile f $ \h ->
        liftIO $ indexHieFile writer srcs f h

{- | Recursively search for @.hie@ and @.hie-boot@  files in given directory
   avoiding loops due to symlinks
-}
getHieFilesIn :: [FilePath] -> IO (HashSet.HashSet FilePath)
getHieFilesIn = flip evalStateT mempty . foldMap go
  where
    go path = do
      seen <- get
      path' <- liftIO $ canonicalizePath path
      if Set.member path' seen
        then return HashSet.empty
        else do
          put (Set.insert path' seen)
          isFile <- liftIO $ doesFileExist path'
          if isFile
            && ( "hie" `isExtensionOf` path'
                  || "hie-boot" `isExtensionOf` path'
               )
            then do
              return (HashSet.singleton path')
            else do
              isDir <- liftIO $ doesDirectoryExist path'
              if isDir
                then do
                  cnts <- liftIO $ listDirectory path'
                  foldMap (go . (path' </>)) cnts
                else return HashSet.empty

instance (Monoid a, Monad m) => Semigroup (StateT s m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Monad m) => Monoid (StateT s m a) where
  mempty = pure mempty
