{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Minimal fork of the hiedb package index command to:
     1. work around issues with filesystem cycles in buck-out
     2. record hs_src column
     3. place hie paths in output folder and record their relative paths
-}
module HieDBIndexer.HieDB (mkHieDB) where

import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Control.Monad.State (StateT, evalStateT, get, liftIO, put)
import Data.HashSet (HashSet, toList)
import qualified Data.HashSet as Set
import Data.IORef
import Database.SQLite.Simple (withTransaction)
import GHC.Fingerprint (getFileHash)
import GhcPlugins (mkSplitUniqSupply)
import HieDb (
  HieDb (getConn),
  SourceFile (RealFile),
  addRefsFromLoaded_unsafe,
  initConn,
  runDbM,
  withHieDb,
  withHieFile,
 )
import HieDb.Run (Options (..))
import HieTypes (hie_hs_file)
import NameCache (initNameCache)
import System.Directory
import System.FilePath (
  isExtensionOf,
  (</>),
 )

{- | Scan recursively for @.hie@ files, index them, and place the result in the
   given 'database', creating a new one if it doesn't exist
-}
mkHieDB :: [FilePath] -> Options -> IO ()
mkHieDB pathsToIndex hiedbOptions =
  withHieDb (database hiedbOptions) $ \conn -> do
    files <- getHieFilesIn pathsToIndex
    initConn conn
    doIndex conn $ toList files

doIndex :: Foldable t => HieDb -> t FilePath -> IO ()
doIndex db files =
  withTransaction (getConn db) $ do
    u <- mkSplitUniqSupply 'c'
    nc <- newIORef $ initNameCache u []
    forM_ files $ \f -> do
      fc <- canonicalizePath f
      hash <- getFileHash f
      runDbM nc $
        withHieFile f $ \h ->
          addRefsFromLoaded_unsafe
            db
            fc
            (RealFile $ hie_hs_file h)
            hash
            h

{- | Recursively search for @.hie@ and @.hie-boot@  files in given directory
   avoiding loops due to symlinks
-}
getHieFilesIn :: [FilePath] -> IO (HashSet FilePath)
getHieFilesIn = flip evalStateT mempty . foldMap go
  where
    go path = do
      seen <- get
      path' <- liftIO $ canonicalizePath path
      if Set.member path' seen
        then return []
        else do
          isFile <- liftIO $ doesFileExist path
          if isFile
            && ( "hie" `isExtensionOf` path
                  || "hie-boot" `isExtensionOf` path
               )
            then do
              return [path']
            else do
              isDir <- liftIO $ doesDirectoryExist path
              if isDir
                then do
                  cnts <- liftIO $ listDirectory path
                  put (Set.insert path' seen)
                  foldMap (go . (path </>)) cnts
                else return []

instance (Monoid a, Monad m) => Semigroup (StateT s m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Monad m) => Monoid (StateT s m a) where
  mempty = pure mempty
