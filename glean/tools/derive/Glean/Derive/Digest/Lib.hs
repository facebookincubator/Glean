{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}

-- | A derive function for generating codemarkup.LocationDigest facts
module Glean.Derive.Digest.Lib (
  Config(..),
  FileFact,
  derive,
) where

import Control.Exception (catch, throwIO)
import Control.Monad (forM, when, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as Map
import Data.Foldable (for_, minimumBy)
import Data.Function (on)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import System.Directory (getCurrentDirectory)
import System.IO.Error (isDoesNotExistError)

import Util.Log.String (logWarning)

import Glean (
  Backend,
  Nat (unNat),
  NewFact (withUnit, newFact),
  Repo,
  makeFact,
  basicWriter,
  runHaxl,
  search_,
 )
import Glean.Angle (
  Angle,
  AngleVars (vars),
  asPredicate,
  end,
  field,
  predicate,
  query,
  rec,
  tuple,
  where_,
  wild,
  (.=),
  string,
 )
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Codemarkup as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Src as Src

type SourceCode = Text
type Digest = Text
type FileFact = Text

data Config = Config
  { hashFunction :: SourceCode -> Digest
  , pathAdaptor :: FilePath -> FilePath
  , indexOnly :: Maybe (NonEmpty FileFact)
  }

derive :: Backend b => b -> Repo -> Config -> IO ()
derive backend repo Config{..} = {-# SCC "derive-digests" #-} do
  locationsByFile <- {-# SCC "query" #-} runHaxl backend repo $ do
    locations <- case indexOnly of
      Nothing -> Glean.search_ $ query codeLocations
      Just files ->
        concat <$>
          traverse
            (Glean.search_ . query . codeLocationsForFile)
            (NE.toList files)
    return $ Map.fromListWith (Map.unionWith (<>))
      [(f, Map.singleton e (range :| [])) | (f, range, e) <- locations]

  factsByFile <- {-# SCC "digest" #-}
    forM (Map.toList locationsByFile) $ \(f, locations) -> do
      contents <- do
        let fpath = pathAdaptor $ T.unpack f
        T.readFile fpath `catch` \e ->
          if isDoesNotExistError e
            then do
              cwd <- getCurrentDirectory
              throwIO $ userError $ fpath <> " not found in " <> cwd
            else throwIO e
      entities <- forM (Map.toList locations) $ \(entity, ranges) -> do
        when (length ranges > 1) $
          logWarning $
            "Multiple ranges found for entity, " <>
            "picking the earliest starting one: " <>
            show entity
        let range = minimumBy (compare `on` rangeStart) ranges
        return (entity, range)
      let sorted_entities = sortOn snd entities
          facts = loop 0 contents sorted_entities
          loop pos ptr ((entity, range) : rest) =
            let ptr' = T.drop (pos' - pos) ptr
                pos' = rangeStart range
                !digest = hashFunction (T.take (rangeLength range) ptr')
            in (entity, digest) : loop pos' ptr' rest
          loop _ _ [] = []
      return (f, facts)

  {-# SCC "write" #-} liftIO $
    basicWriter backend repo [Src.allPredicates, Code.allPredicates] $ do
      for_ factsByFile $ \(f, facts) ->
        withUnit (encodeUtf8 f) $ do
          srcFile <- makeFact @Src.File f
          for_ facts $ \(entity, digest) -> do
            let key = Code.FileEntityDigest_key srcFile entity
            void $ newFact @_ @Code.FileEntityDigest key digest

rangeStart :: Code.RangeSpan -> Int
rangeStart (Code.RangeSpan_span Src.ByteSpan{..}) =
  fromIntegral $ unNat byteSpan_start
rangeStart (Code.RangeSpan_range Src.Range{..}) =
  fromIntegral $ unNat range_lineBegin
rangeStart Code.RangeSpan_EMPTY = 0

rangeLength :: Code.RangeSpan -> Int
rangeLength (Code.RangeSpan_span Src.ByteSpan{..}) =
  fromIntegral $ unNat byteSpan_length
rangeLength (Code.RangeSpan_range Src.Range{..}) = error "TODO"
rangeLength Code.RangeSpan_EMPTY = 0

codeLocations :: Angle (Text, Code.RangeSpan, Code.Entity)
codeLocations =
  vars $ \(file :: Angle Src.File)
          (fileVal :: Angle Text)
          (range :: Angle Code.RangeSpan)
          (entity :: Angle Code.Entity) ->
    tuple (fileVal, range, entity)
      `where_` [ wild
                  .= predicate @Code.ResolveLocation
                    ( rec $
                        field @"location"
                          (rec @_ @Code.Location $
                            field @"file" (asPredicate file) $
                            field @"location" range
                            end) $
                        field @"entity" entity
                        end
                    )
               , file .= predicate @Src.File fileVal
               ]

codeLocationsForFile :: FileFact -> Angle (Text, Code.RangeSpan, Code.Entity)
codeLocationsForFile fileFact =
  vars $ \(range :: Angle Code.RangeSpan)
          (entity :: Angle Code.Entity) ->
    tuple (string fileFact, range, entity)
      `where_` [ wild
                  .= predicate @Code.ResolveLocation
                    ( rec $
                        field @"location"
                          (rec @_ @Code.Location $
                            field @"file" (string fileFact) $
                            field @"location" range
                            end) $
                        field @"entity" entity
                        end
                    )
               ]
