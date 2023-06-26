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
import Data.HashMap.Strict (HashMap)
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
import System.Directory (getCurrentDirectory)
import System.IO.Error (isDoesNotExistError)

import Util.Log.String (logWarning)
import Util.Regex (RegexError, substituteNoLimit_safe)

import Glean (
  Backend,
  Nat (unNat),
  NewFact (withUnit, newFact),
  Repo,
  makeFact,
  basicWriter,
  runHaxl,
  search_, getFirstResult, keys
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
  string
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
import Glean.Util.Range (srcRangeToFileLocation)
import Data.Maybe (fromMaybe)

type SourceCode = Text
type Digest = Text
type FileFact = Text

data Config = Config
  { hashFunction :: Maybe Glass.Name -> SourceCode -> Digest
  , pathAdaptor :: FilePath -> FilePath
  , indexOnly :: Maybe (NonEmpty FileFact)
  }

data FileData = FileData
  { locations
    :: HashMap (Code.Entity, Maybe Glass.Name) (NonEmpty Code.RangeSpan)
  , lines :: Maybe Src.FileLines_key
  }

derive :: Backend b => b -> Repo -> Config -> IO ()
derive backend repo Config{..} = {-# SCC "derive-digests" #-} do
  dataByFile <- {-# SCC "query" #-} runHaxl backend repo $ do
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

    let locationsByFile = Map.fromListWith (Map.unionWith (<>))
          [ (f, locations)
          | (f, range, e, n) <- locationsWithNames
          , let locations = Map.singleton (e,n) (range :| [])]
    fileLines <- Map.fromList <$> traverse
      (\f -> (f,) <$> Glean.getFirstResult (keys (query (queryFileLines f))))
      (Map.keys locationsByFile)
    let result = Map.intersectionWith FileData locationsByFile fileLines
    return result

  locationsByFile <- {-# SCC "digest" #-}
    fmap (Map.fromListWith (<>) . concat) $
    forM (Map.toList dataByFile) $ \(f, FileData{..}) -> do
      let
        -- Partially applied for performance on Range-based languages (Clang)
        toLocation = rangeSpanToFileLocation fileFact lines
        fileFact = Src.File 0 (Just f)
      forM (Map.toList locations) $ \((entity, name), ranges) -> do
        when (length ranges > 1) $
          logWarning $
            "Multiple ranges found for entity, " <>
            "picking the earliest starting one: " <>
            show entity
        let range = minimumBy (compare `on` rangeStart) ranges
            loc = toLocation range
            locFile = fromMaybe f $
              Src.file_key $ Src.fileLocation_file loc
        return (locFile, [(f, entity, name, loc)])

  facts <- forM (Map.toList locationsByFile) $ \(f, locations) -> do
    contents <- do
      let fpath = pathAdaptor $ T.unpack f
      T.readFile fpath `catch` \e ->
        if isDoesNotExistError e
          then do
            cwd <- getCurrentDirectory
            throwIO $ userError $ fpath <> " not found in " <> cwd
          else throwIO e
    let sorted_entities = sortOn startPos locations
        startPos (_, _, _, Src.FileLocation{..}) =
          Src.byteSpan_start fileLocation_span
        facts = loop 0 contents sorted_entities
        loop pos ptr ((srcF, entity, name, Src.FileLocation{..}) : rest) =
          let ptr' = T.drop (pos' - pos) ptr
              pos' = natToInt $ Src.byteSpan_start fileLocation_span
              len =  natToInt $ Src.byteSpan_length fileLocation_span
              !digest =
                hashFunction name (T.take len ptr')
          in (srcF, entity, digest) : loop pos' ptr' rest
        loop _ _ [] = []
    return facts

  let factsByFile = Map.toList $ Map.fromListWith (<>)
        [ (srcF, [(entity, digest)])
        | (srcF, entity, digest) <- concat facts]

  {-# SCC "write" #-} liftIO $
    basicWriter backend repo [Src.allPredicates, Code.allPredicates] $ do
      for_ factsByFile $ \(f, facts) ->
        withUnit (encodeUtf8 f) $ do
          srcFile <- makeFact @Src.File f
          for_ facts $ \(entity, digest) -> do
            let key = Code.FileEntityDigest_key srcFile entity
            void $ newFact @_ @Code.FileEntityDigest key digest

rangeSpanToFileLocation
  :: Src.File
  -> Maybe Src.FileLines_key
  -> Code.RangeSpan
  -> Src.FileLocation
rangeSpanToFileLocation f mbFileLines = \case
  Code.RangeSpan_EMPTY -> error "EMPTY"
  Code.RangeSpan_span sp ->
    Src.FileLocation {
      fileLocation_file= f,
      fileLocation_span=sp
      }
  Code.RangeSpan_range range -> toFileLocation range
  where
    toFileLocation =
      maybe
        (error "Missing src.FileLines")
        (srcRangeToFileLocation . map natToInt . Src.fileLines_key_lengths)
        mbFileLines

rangeStart :: Code.RangeSpan -> Int
rangeStart (Code.RangeSpan_span Src.ByteSpan{..}) =
  natToInt byteSpan_start
rangeStart (Code.RangeSpan_range Src.Range{..}) =
  natToInt range_lineBegin
rangeStart Code.RangeSpan_EMPTY = 0

natToInt :: Num c => Nat -> c
natToInt = fromIntegral . unNat

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

queryFileLines :: FileFact -> Angle Src.FileLines
queryFileLines fileFact =
   predicate @Src.FileLines (rec $ field @"file" (string fileFact) end)

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
--
--   NOTE: this function does not quite guarantee the property above,
--         it's only a best-effort that works in most cases
replaceName :: Maybe Glass.Name -> Glass.Name -> Text -> Either RegexError Text
--   Plain search and replace would hardly satisfy the property above, e.g. when
--   name is very short:
--
--   > replace "f" "NAME" "def f(x):\n ff(x)" = deNAME NAME(x):\n NAMENAME(x)
--   > replace "g" "NAME" "def g(x):\n ff(x)" = def NAME(x):\n ff(x)
  -- So we use a simple regular expression to detect word boundaries
--   TODO a better? option would be to use a lexer for common PL identifiers to
--   tokenize the code and then match-and-replace on the tokens
replaceName (Just (Glass.Name n)) (Glass.Name replacement) haystack =
    substituteNoLimit_safe haystack (mkIdentifierRegex n) replacement
replaceName Nothing _ haystack = Right haystack

mkIdentifierRegex :: Text -> Text
mkIdentifierRegex ident = "\\b\\Q" <> ident <> "\\E\\b"
