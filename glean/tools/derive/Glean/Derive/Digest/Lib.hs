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
  derive,
) where

import Control.Exception (catch, throwIO)
import Control.Monad (forM, when, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as Map
import Data.Foldable (for_, minimumBy)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import System.Directory (getCurrentDirectory)
import System.IO.Error (isDoesNotExistError)

import Util.Log.String (logWarning)
import Util.Text(slice)

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
 )
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Codemarkup as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Src as Src

type SourceCode = Text
type Digest = Text

data Config = Config
  { hashFunction :: SourceCode -> Digest
  , pathAdaptor :: FilePath -> FilePath
  }

derive :: Backend b => b -> Repo -> Config -> IO ()
derive backend repo Config{..} = do
  locationsByFile <- runHaxl backend repo $ do
    locations <- Glean.search_ $ query codeLocations
    return $ Map.fromListWith (Map.unionWith (<>))
      [(f, Map.singleton e (range :| [])) | (f, range, e) <- locations]

  factsByFile <- forM (Map.toList locationsByFile) $ \(f, locations) -> do
    contents <- do
      let fpath = pathAdaptor $ T.unpack f
      T.readFile fpath `catch` \e ->
        if isDoesNotExistError e
          then do
            cwd <- getCurrentDirectory
            throwIO $ userError $ fpath <> " not found in " <> cwd
          else throwIO e
    facts <- forM (Map.toList locations) $ \(entity, ranges) -> do
      when (length ranges > 1) $
        logWarning $
          "Multiple ranges found for entity, " <>
          "picking the earliest starting one: " <>
          show entity
      let srcCode = textAt range contents
          !digest = hashFunction srcCode
          range = minimumBy (compare `on` rangeStart) ranges
      return (entity, digest)
    return (f, facts)
  liftIO $ basicWriter backend repo [Src.allPredicates, Code.allPredicates] $ do
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

textAt :: Code.RangeSpan -> T.Text -> T.Text
textAt (Code.RangeSpan_span Src.ByteSpan {..}) text =
  slice
    (fromIntegral $ unNat byteSpan_start)
    (fromIntegral $ unNat byteSpan_length)
    text
textAt (Code.RangeSpan_range Src.Range {..}) _ =
  error "TODO"
textAt Code.RangeSpan_EMPTY _ = ""

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
