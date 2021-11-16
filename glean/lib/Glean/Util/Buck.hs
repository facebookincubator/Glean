{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns, RecordWildCards, ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric, TypeApplications #-}
-- | Library of re-usable tools for looking at the buck facts, Locators, and
-- dependency graph
module Glean.Util.Buck
  ( -- * Locator manipulation
    -- ** 'Cell
    Cell(..), cellDir, cellPath, buckCellSplit
    -- ** 'Locator'
  , Locator(..), buckLocToLoc, locToBuckLoc, locToQueryBuckLoc
    -- ** 'Locator' utilities
  , dropFlavor
  , locatorFromText
  , absolute, asFilePath
    -- ** 'Buck.Locator_key' utilities
  , buckLocToText, buckLocAsFilePath
    -- * Locator with optional Label
  , LocatorLabel(..), parseLocatorLabel, absoluteLocatorLabel
    -- * Locator patterns
    -- ** Types
  , LocatorPatternDepth(..), LocatorPattern(..)
    -- ** utilities
  , normalizeBuildTargetString, parseLocatorOrPattern, buckLocPatToText
    -- ** queries
  , locatorPatterns, locatorPatternIds
    -- * For files
  , getSrcFile, getConsumers, getProducers
  ) where

import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Default
import Data.Hashable (Hashable(..))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

import Glean
import Glean.Angle hiding (query)
import qualified Glean.Angle as Angle
import qualified Glean.Schema.Buck.Types as Buck
import qualified Glean.Schema.Query.Buck.Types as Q.Buck
import qualified Glean.Schema.Query.Src.Types as Q.Src -- gen
import qualified Glean.Schema.Src.Types as Src


-- -----------------------------------------------------------------------------
-- The Schema-independent type, moved here from "Runner.Target"

newtype Cell = Cell {fromCell :: Text}
  deriving (Eq, Ord, Show, Hashable)

-- From @buck audit cell@ until we get this dynamically
cellDir :: Cell -> Text
cellDir (Cell c) = case c of
  "bazel_skylib" -> "third-party/bazel-skylib"
  "buck" -> "xplat/build_infra/buck_client"
  "buck_bazel_skylib" ->
    "xplat/build_infra/buck_client/third-party/skylark/bazel-skylib"
  "fbcode" -> "fbcode"
  "fbcode_macros" -> "tools/build_defs/fbcode_macros"
  "fbobjc_dylibs" -> "xplat/configurations/buck/apple/dylibs"
  "fbsource" -> ""
  "ovr_config" -> "arvr/tools/build_defs/config"
  unknown -> unknown

cellPath :: Cell -> FilePath
cellPath = Text.unpack . cellDir

-- See @BuildTargetParser@, the leading at-sign is a skylark thing?
cellNoAt :: Text -> Maybe Cell
cellNoAt cIn = case fromMaybe cIn (Text.stripPrefix "@" cIn) of
  "" -> Nothing
  c -> Just (Cell c)

-- | A target locator of the form subdir//foo/bar:thing with nice Aeson instance
data Locator = Locator
  { locatorCell :: Maybe Cell -- ^ buck cell name (the bit before //)
  , locatorPath :: Text -- ^ path (the bit between // and :)
  , locatorName :: Text -- ^ name (the bit after :)
  }
  deriving (Eq, Ord, Generic, Show)

instance Hashable Locator

-- NOTE: All To/FromJSON instances below are for debugging only
instance Aeson.ToJSON Locator where
  toJSON = Aeson.String . absolute

instance Aeson.ToJSONKey Locator where
  toJSONKey = Aeson.toJSONKeyText absolute

instance Aeson.FromJSON Locator where
  parseJSON (Aeson.String s) = parseLocator s
  parseJSON _ = fail "invalid locator, expected JSON string"

instance Aeson.FromJSONKey Locator where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseLocator

-- -----------------------------------------------------------------------------

locToBuckLoc :: Locator -> Buck.Locator_key
locToBuckLoc Locator{..} = Buck.Locator_key
  { locator_key_subdir = fromCell <$> locatorCell
  , locator_key_path = locatorPath
  , locator_key_name = locatorName }

buckLocToLoc :: Cell -> Buck.Locator_key -> Locator
buckLocToLoc defaultCell Buck.Locator_key{..} = Locator
  { locatorCell = Just $ maybe defaultCell Cell locator_key_subdir
  , locatorPath = locator_key_path
  , locatorName = locator_key_name }

locToQueryBuckLoc :: Locator -> Q.Buck.Locator
locToQueryBuckLoc Locator{locatorCell, locatorPath, locatorName} =
  Q.Buck.Locator_with_key Q.Buck.Locator_key
    { locator_key_subdir = locatorSubPat locatorCell
    , locator_key_path = Just locatorPath
    , locator_key_name = Just locatorName }

-- | @'dropFlavor' target@ is useful to remove a trailing @'#'@ and flavor
dropFlavor :: Text -> Text
dropFlavor = fst . Text.breakOn "#"

-- | Parse @cell//path:name@ or @//path:name@, where the latter gets
-- @defaultCell@
locatorFromText :: Cell -> Text -> Maybe Locator
locatorFromText defaultCell = fmap ensureCell . unsafeLocatorFromText
  where
    ensureCell loc = case locatorCell loc of
      Just{} -> loc
      Nothing -> loc{locatorCell = Just defaultCell}

-- -----------------------------------------------------------------------------

-- | Thus expects the input 'Text' to be a well formed build target, see
-- <https://buck.build/concept/build_target.html> and it should match
--
-- > [A-Za-z0-9._-]*//[A-Za-z0-9/._-]*:[A-Za-z0-9_/.=,@~+-]+
unsafeLocatorFromText :: Text -> Maybe Locator
unsafeLocatorFromText s = do
  let (cell, rest) = Text.breakOn "//" s
  pathAndName <- Text.stripPrefix "//" rest
  let (locatorPath, colonName) = Text.breakOn ":" pathAndName
  locatorName <- Text.stripPrefix ":" colonName
  return Locator
    { locatorCell = cellNoAt cell
    , locatorPath
    , locatorName
    }

parseLocator :: Text -> Aeson.Parser Locator
parseLocator s = maybe (fail ("unsafeLocatorFromText failed: " <> show s))
  return $ unsafeLocatorFromText s

-- | Produce an absolute locator of the form subdir//path:name
absolute :: Locator -> Text
absolute loc = maybe "" fromCell (locatorCell loc) <> relativeToSubdir loc
  where
    -- Produce a locator relative to the subdirectory (i.e., //path:name)
    relativeToSubdir :: Locator -> Text
    relativeToSubdir Locator{..} = Text.concat
      [ "//"
      , locatorPath
      , ":"
      , locatorName
      ]

-- | Convert a locator into a file path: subdir//path:name is converted to
-- subdir/path/name.
asFilePath :: Locator -> Text
asFilePath Locator{..} = Text.intercalate "/" $ filter (not . Text.null)
  [maybe "" cellDir locatorCell, locatorPath, locatorName]

-- -----------------------------------------------------------------------------
-- Make the Buck.Locator_key flavor use the 'Locator' version with isomorphism

-- | Produce an absolute locator of the form @subdir//path:name@
buckLocToText :: Cell -> Buck.Locator_key -> Text
buckLocToText defaultCell = absolute . buckLocToLoc defaultCell

-- | Convert a locator into a file path: subdir//path:name is converted to
-- subdir/path/name.
buckLocAsFilePath :: Cell -> Buck.Locator_key -> Text
buckLocAsFilePath defaultCell = asFilePath . buckLocToLoc defaultCell

-- -----------------------------------------------------------------------------

-- | Locator with optional label (from square brackets) to refer to file in
-- the "outs" attribute of the locator.  When the label is absent this could
-- refer to the "out" file or "default_outs" file.
data LocatorLabel = LocatorLabel Locator (Maybe Text)
  deriving Show

-- | Detect locators with a trailing label in square brackets in their name
parseLocatorLabel :: Locator -> LocatorLabel
parseLocatorLabel loc@Locator{..} = fromMaybe (LocatorLabel loc Nothing) $ do
  withoutClose <- Text.stripSuffix "]" locatorName
  let (endsInOpen, label) = Text.breakOnEnd "[" withoutClose
  newName <- Text.stripSuffix "[" endsInOpen
  let new = Locator{locatorCell, locatorPath, locatorName = newName}
  return (LocatorLabel new (Just label))

-- | Produce an absolute locator of the form subdir//path:name[label] when
-- the label is present, otherwise subdir//path:name
absoluteLocatorLabel :: LocatorLabel -> Text
absoluteLocatorLabel (LocatorLabel loc Nothing) = absolute loc
absoluteLocatorLabel (LocatorLabel loc (Just lab)) =
  absolute loc <> "[" <> lab <> "]"

instance Aeson.ToJSON LocatorLabel where
  toJSON = Aeson.String . absoluteLocatorLabel

instance Aeson.FromJSON LocatorLabel where
  parseJSON = Aeson.withText "LocatorLabel" $
    fmap parseLocatorLabel . parseLocator

-- -----------------------------------------------------------------------------
-- Parsers for comprending locators and buck build command lines

isCellNameC :: Char -> Bool
isCellNameC c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
  || ('0' <= c && c <= '9') || c == '.' || c == '_' || c == '-'

isPackagePathC :: Char -> Bool
isPackagePathC c = isCellNameC c || c == '/'

isRuleNameC :: Char -> Bool
isRuleNameC c = isPackagePathC c
  || c == '=' || c == ',' || c == '@' || c == '~' || c == '+'

-- | There are two kinds of locator patterns
data LocatorPatternDepth
  = LPD_Shallow     -- ^ pattern ends in @:@, from one directory
  | LPD_Deep        -- ^ pattern ends in @...@, from directory and descendents
  deriving (Enum, Eq, Ord, Show)

-- | Represent a build target pattern
--
-- See <https://buck.build/concept/build_target_pattern.html>
data LocatorPattern = LocatorPattern
  { lpCell :: Maybe Cell -- ^ cell name
  , lpPath :: Text -- ^ may be @""@
  , lpDepth :: LocatorPatternDepth
  } deriving (Eq, Ord, Show)

-- | Find cell name before @//@, if present, and target after @//@.
-- See @AbstractBuckCellArg@
buckCellSplit :: Text -> (Maybe Cell, Text)
buckCellSplit textIn =
  let (pre, ss_post) = Text.breakOn "//" textIn
  in if Text.null ss_post then (Nothing, textIn)
      else ( cellNoAt pre
           , Text.drop 2 ss_post )

-- | There are extra conveniences allowed for humans entering buck command lines
-- that are handled by pre-processing the locator arguments.
--
-- See @normalizeBuildTargetString@ in @CommandLineTargetNodeSpecParser@
-- which is used to pre-process the buck cli arguments.  From reading the
-- code here are hypotheses about what it will do:
--
-- > normalizeBuildTargetString "" == "//:"
-- > normalizeBuildTargetString "/" == "//:"
-- > normalizeBuildTargetString "//" == "//:"
-- > normalizeBuildTargetString "///" == "//:"
-- > normalizeBuildTargetString "bar" == "//bar:bar"
-- > normalizeBuildTargetString "bar/" == "//bar:bar"
-- > normalizeBuildTargetString "foo/bar" == "//foo/bar:bar"
-- > normalizeBuildTargetString "/foo/bar/" == "///foo/bar:bar"
-- > normalizeBuildTargetString "..." == "//..."
-- > normalizeBuildTargetString "/..." == "///..."
-- > normalizeBuildTargetString "//..." == "//..."
-- > normalizeBuildTargetString "///..." == "///..."
-- > normalizeBuildTargetString "//...///" == "//..."
-- > normalizeBuildTargetString "//foo/bar//:" == "//foo/bar:"
-- > normalizeBuildTargetString "foo//bar//baz" == "foo//bar//baz:baz"
-- >
-- > normalizeBuildTargetString (normalizeBuildTargetString x) ==
-- >   normalizeBuildTargetString x
--
-- I note that @///@ leads to paths with leading @/@ which are invalid in
-- 'parseLocatorOrPattern'
normalizeBuildTargetString :: Text -> Text
normalizeBuildTargetString textIn =
  let (mCell, arg) = buckCellSplit textIn
      (target1, nameAfterColon1) =
        let (pre, c_post) = Text.breakOn ":" arg
        in if Text.null c_post then (arg, Nothing)
            else (pre, Just (Text.drop 1 c_post))
      target2 = Text.dropWhileEnd ('/' ==) target1
      noColonAndNotTripleDot = isNothing nameAfterColon1
            && not ("/..." `Text.isSuffixOf` target2)
            && target2 /= "..."
      nameAfterColon2
        | noColonAndNotTripleDot = Just (Text.takeWhileEnd ('/' /=) target2)
        | otherwise = nameAfterColon1
      target3 = maybe target2 (\ n -> target2 <> ":" <> n) nameAfterColon2
  in maybe "" fromCell mCell <> "//" <> target3

-- | Loosely ported from logic in buck. Expects well-formed input with @//@ or
-- output of 'normalizeBuildTargetString'.
--
-- See @BuildTargetParser.parse@
parseLocatorOrPattern :: Text -> Maybe (Either LocatorPattern Locator)
parseLocatorOrPattern textIn
  | "/..." `Text.isSuffixOf` textIn = parseLocatorPatternDeep
  | otherwise = parseLocatorOrPatternShallow
  where
    validPath path = Text.null path ||
      (Text.all isPackagePathC path && validPathSegments path)
      where
      -- This is False if there is a leading or trailing "/"
      validPathSegments = all validPathSegment . Text.splitOn "/"
        where
          -- See @checkBaseName@ in @BuildTargetParser@ for validation rules
          validPathSegment "" = False
          validPathSegment "." = False
          validPathSegment ".." = False
          validPathSegment _ = True

    parseCellPost = do
      let (cellIn, ss_post) = Text.breakOn "//" textIn
      cell <- do
        let mc = cellNoAt cellIn
        guard (maybe True (Text.all isCellNameC . fromCell) mc)
        return mc
      guard (not (Text.null ss_post))
      return (cell, Text.drop 2 ss_post)

    parsePathRule post = do
      let (path, c_ruleName) = Text.breakOn ":" post
      guard (validPath path)
      guard (not (Text.null c_ruleName))
      let ruleName = Text.drop 1 c_ruleName
      guard (Text.all isRuleNameC ruleName)
      return (path, ruleName)

    parseLocatorOrPatternShallow = do
      (cell, post) <- parseCellPost
      (path, ruleName) <- parsePathRule post
      if Text.null ruleName
        then return $ Left LocatorPattern
          { lpCell = cell
          , lpPath = path
          , lpDepth = LPD_Shallow }
        else return $ Right Locator
          { locatorCell = cell
          , locatorPath = path
          , locatorName = ruleName }

    parseLocatorPatternDeep = do -- we know textIn ends in /...
      (cell, post) <- parseCellPost
      path <- if post == "..."
        then return ""
        else do
          let path' = Text.dropEnd 4 post
          guard (validPath path')
          return path'
      return $ Left LocatorPattern
          { lpCell = cell
          , lpPath = path
          , lpDepth = LPD_Deep }

-- | Convert 'LocatorPattern' to canonical format
buckLocPatToText :: LocatorPattern -> Text
buckLocPatToText LocatorPattern{..} = maybe id ((<>) . fromCell) lpCell $
  "//" <> lpPath <> (case lpDepth of
    LPD_Shallow -> ":"
    LPD_Deep -> "/...")

-- -----------------------------------------------------------------------------

-- | @locatorsUnder (LocatorPattern cell path _)@ :
-- Helper for finding locater paths (for patterns with @path /= ""@)
-- underneath the @cell//path@, those with @cell//path/parent/@ as a prefix.
-- Note: This does not find locators exactly @cell//path@, only subpaths of it.
locatorsUnder
  :: (Backend be) => be -> Repo -> LocatorPattern -> IO [Buck.Locator]
locatorsUnder be repo LocatorPattern{..} = case lpDepth of
  LPD_Shallow -> return []
  LPD_Deep -> do
    let matchSubdir :: Angle (Maybe Text)
        matchSubdir = case lpCell of
          Nothing -> alt @"nothing" wild
          Just cell -> alt @"just" (string (fromCell cell))

        locator :: Angle Buck.Locator
        locator =
          predicate @Buck.Locator (rec $
            field @"subdir" matchSubdir $
            field @"path" (stringPrefix (lpPath <> "/")) end)

    runQuery_ be repo (Angle.query locator)

-- | Helper for 'locatorPatterns' and 'locatorPatternIds' for cell aka subdir
-- In practice I cannot see the 'Nothing' case in the Glean facts.
locatorSubPat :: Maybe Cell -> Maybe Q.Buck.Locator_subdir
locatorSubPat mc = case mc of
  Nothing -> Just Q.Buck.Locator_subdir
    { locator_subdir_nothing = Just def
    , locator_subdir_just = Nothing
    , Q.Buck.locator_subdir_any = False }
  Just c -> Just Q.Buck.Locator_subdir
    { locator_subdir_nothing = Nothing
    , locator_subdir_just = Just (fromCell c)
    , Q.Buck.locator_subdir_any = False }

-- | Helper for 'locatorPatterns' and 'locatorPatternIds' for specific path
locatorsQueryHere :: Maybe Cell -> Text -> Q.Buck.Locator
locatorsQueryHere mc path =  Q.Buck.Locator_with_key Q.Buck.Locator_key
  { locator_key_subdir = locatorSubPat mc
  , locator_key_path = Just path
  , locator_key_name = Nothing }

-- | Helper for 'locatorPatterns' and 'locatorPatternIds' for all paths
locatorsQueryAll :: Maybe Cell-> Q.Buck.Locator
locatorsQueryAll mc =  Q.Buck.Locator_with_key Q.Buck.Locator_key
  { locator_key_subdir = locatorSubPat mc
  , locator_key_path = Nothing
  , locator_key_name = Nothing }

-- | Helper for 'locatorPatterns' and 'locatorPatternIds' to detect "//..."
allPaths :: LocatorPattern -> Bool
allPaths lp = Text.null (lpPath lp) && case lpDepth lp of
  LPD_Shallow -> False
  LPD_Deep -> True

-- | @locatorsPattern ... subdir parent@ finds all known 'Buck.Locator'
-- that match @cell//path/...@ or @cell//path:@ similar to buck target patterns.
--
-- <https://buck.build/concept/build_target_pattern.html>
locatorPatterns
  :: (Backend be)
  => be -> Repo
  -> LocatorPattern -> IO [Buck.Locator]
locatorPatterns be repo lp@LocatorPattern{..}
  | allPaths lp = do
    runQuery_ be repo $ query $ locatorsQueryAll lpCell
  | otherwise = do
    here <- runQuery_ be repo $ query $ locatorsQueryHere lpCell lpPath
    below <- locatorsUnder be repo lp
    return (here ++ below)

-- | @locatorIdsPattern ... subdir parent@ finds all known 'Buck.Locator'
-- that match @cell//path/...@ or @cell//path:@ similar to buck target patterns.
--
-- <https://buck.build/concept/build_target_pattern.html>
locatorPatternIds
  :: (Backend be) => be -> Repo -> LocatorPattern -> IO [IdOf Buck.Locator]
locatorPatternIds be repo lp@LocatorPattern{..}
  | allPaths lp = do
    fmap (map getId) $ runQuery_ be repo $ query $
      locatorsQueryAll lpCell
  | otherwise = do
    here <- fmap (map getId) $ runQuery_ be repo $ query $
      locatorsQueryHere lpCell lpPath
    below <- map getId <$> locatorsUnder be repo lp
    return (here ++ below)

-- -----------------------------------------------------------------------------

-- | @getSrcFile fileIn@ will return 'Src.File' with @Just fileIn@
getSrcFile :: Text -> Haxl w (Maybe Src.File)
getSrcFile fileIn = fmap (fmap withFileIn) $ getFirstResult $ query $
  Q.Src.File_with_key fileIn
  where
    withFileIn (Src.File i _) = Src.File i (Just fileIn)

-- | Returns 'Buck.Locator' (with key) retrieved that depend on 'Src.File'
getConsumers :: Src.File -> Haxl w [Buck.Locator]
getConsumers srcFile = do
  owners <- search_ $ query $ Q.Buck.Owner_with_key def
    { Q.Buck.owner_key_source = Just $ toQueryId (getId srcFile) -- match
    , Q.Buck.owner_key_owner = Just $ Q.Buck.TargetSources_with_key def
      { Q.Buck.targetSources_key_target = Just $ Q.Buck.Target_with_key def
        { Q.Buck.target_key_locator = Just $ Q.Buck.Locator_with_get def } } }
  return
    [ Buck.target_key_locator tk
    | ok <- mapMaybe Buck.owner_key owners
    , tsk <- maybeToList $ Buck.targetSources_key (Buck.owner_key_owner ok)
    , tk <- maybeToList $ Buck.target_key (Buck.targetSources_key_target tsk) ]

-- | Returns 'Buck.Locator' (with key) retrieved that produce 'Src.File'
getProducers :: Src.File -> Haxl w [Buck.Locator]
getProducers srcFile = do
  outTargets <- search_ $ query $ Q.Buck.OutTarget_with_key def
    { Q.Buck.outTarget_key_file = Just $ toQueryId (getId srcFile)
    , Q.Buck.outTarget_key_target = Just $ Q.Buck.Target_with_key def
      { Q.Buck.target_key_locator = Just $ Q.Buck.Locator_with_get def } }
  return
    [ Buck.target_key_locator tk
    | otk <- mapMaybe Buck.outTarget_key outTargets
    , tk <- maybeToList $ Buck.target_key (Buck.outTarget_key_target otk) ]

-- -----------------------------------------------------------------------------
