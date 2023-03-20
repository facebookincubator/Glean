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
  replaceName
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
import Data.Traversable (for)
import Data.Tuple.Extra (thd3)
import System.Directory (getCurrentDirectory)
import System.IO.Error (isDoesNotExistError)

import Util.Log.String (logWarning)
import Util.Regex (substituteNoLimit)

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
import Glean.Haxl.Repos (RepoHaxl)
import qualified Glean.Glass.SymbolId()
import qualified Glean.Glass.Types as Glass
import Glean.Glass.SymbolId.Class (toQName, ToQName)

type SourceCode = Text
type Digest = Text
type FileFact = Text

data Config = Config
  { hashFunction :: Maybe Glass.Name -> SourceCode -> Digest
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

    locationsWithNames <- for locations $ \(f,r,e) -> do
        n <- toName e
        return (f,r,e,n)

    return $ Map.fromListWith (Map.unionWith (<>))
      [ (f, Map.singleton (e,n) (range :| []))
      | (f, range, e, n) <- locationsWithNames]

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
      entities <- forM (Map.toList locations) $ \((entity, name), ranges) -> do
        when (length ranges > 1) $
          logWarning $
            "Multiple ranges found for entity, " <>
            "picking the earliest starting one: " <>
            show entity
        let range = minimumBy (compare `on` rangeStart) ranges
        return (entity, name, range)
      let sorted_entities = sortOn thd3 entities
          facts = loop 0 contents sorted_entities
          loop pos ptr ((entity, name, range) : rest) =
            let ptr' = T.drop (pos' - pos) ptr
                pos' = rangeStart range
                !digest = hashFunction name (T.take (rangeLength range) ptr')
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

toName
  :: ToQName Code.Entity => Code.Entity
  -> RepoHaxl u w (Maybe Glass.Name)
toName entity = do
  eiName <- toQName entity
  case eiName of
    Right (gn@(Glass.Name p), gp)
      | "" == p ->
        return $ Just gp
      | otherwise ->
        return $ Just gn
    Left _e ->
      return Nothing

-- | Helper function to replace an entity name in source code
--   The property we want is:
--
--   > PROP: replaceName name name' src ==
--   >       replace name'' name' (semanticRename name name'' src)
--
--   where semanticRename is fully syntax-aware name replacement.
--   In other words, we want to be able to detect user-driven renames
replaceName :: Maybe Glass.Name -> Glass.Name -> Text -> Text
--   Plain search and replace would hardly satisfy the property above, e.g. when
--   name is very short:
--
--   > replace "f" "NAME" "def f(x):\n ff(x)" = deNAME NAME(x):\n NAMENAME(x)
--   > replace "g" "NAME" "def g(x):\n ff(x)" = def NAME(x):\n ff(x)
  -- So we use a simple regular expression to detect word boundaries
--   TODO a better? option would be to use a lexer for common PL identifiers to
--   tokenize the code and then match-and-replace on the tokens
replaceName (Just (Glass.Name n)) (Glass.Name replacement) haystack =
  substituteNoLimit haystack (mkIdentifierRegex n) replacement
replaceName Nothing _ haystack = haystack

mkIdentifierRegex :: Text -> Text
mkIdentifierRegex ident = "\\b" <> ident <> "\\b"
