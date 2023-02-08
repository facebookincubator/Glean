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
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as Map
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import System.Directory (getCurrentDirectory)
import System.IO.Error (isDoesNotExistError)

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
    return $ Map.fromListWith (++)
      [(f, [(range, e)]) | (f, range, e) <- locations]

  forM_ (Map.toList locationsByFile) $ \(f, locations) -> do
    contents <- do
      let fpath = pathAdaptor $ T.unpack f
      T.readFile fpath `catch` \e ->
        if isDoesNotExistError e
          then do
            cwd <- getCurrentDirectory
            throwIO $ userError $ fpath <> " not found in " <> cwd
          else throwIO e
    liftIO $
      basicWriter backend repo [Src.allPredicates, Code.allPredicates] $
        withUnit (encodeUtf8 f) $ do
          srcFile <- makeFact @Src.File f
          for_ locations $ \(range, entity) -> do
            let srcCode = textAt range contents
                !digest = hashFunction srcCode
                key = Code.FileEntityDigest_key srcFile entity
            void $ newFact @_ @Code.FileEntityDigest key digest

textAt :: Code.RangeSpan -> T.Text -> T.Text
textAt (Code.RangeSpan_span Src.ByteSpan {..}) text =
  T.take (fromIntegral $ unNat byteSpan_length) $
    T.drop (fromIntegral $ unNat byteSpan_start) text
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
                            field @"name" wild $
                            field @"file" (asPredicate file) $
                            field @"location" range
                            end) $
                        field @"entity" entity
                        end
                    )
               , file .= predicate @Src.File fileVal
               ]
