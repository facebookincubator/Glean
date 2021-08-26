-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo, RecordWildCards #-}
-- | Explain how every file has been imported, by all possible paths,
-- from the root file.
module IncludeTree (main) where

import Glean hiding (options)
import qualified Glean (options)
import Glean.Impl.ConfigProvider
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.ConfigProvider
import Glean.Util.PredMap (PredMap)
import qualified Glean.Util.PredMap as PredMap
import Glean.Util.PredSet (PredSet)
import qualified Glean.Util.PredSet as PredSet
import Glean.Util.TargetAnalysis

import Util.EventBase (withEventBaseDataplane)

import Data.List (sortOn)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Options.Applicative as O
import TextShow (showt)

data Config = Config
  { cfgService :: ThriftSource ClientConfig
  , cfgRepo :: Glean.Repo
  , cfgTranslationUnitTrace :: IdOf Cxx.TranslationUnitTrace
  }

options :: O.ParserInfo Config
options = O.info (parser O.<**> O.helper) desc
  where
    parser = do
      cfgService <- Glean.options
      cfgRepo <- O.option (O.maybeReader parseRepo) $
        O.long "repo"
        <> O.metavar "REPO"
        <> O.help "repo name/hash"
      cfgTranslationUnitTrace <- O.option (fmap (IdOf . Fid) O.auto) $
        O.long "tu"
        <> O.metavar "FACT_ID"
        <> O.help "cxx1.TranslationUnitTrace fact id"
      return Config{..}
    desc = O.fullDesc
      <> O.progDesc "Print include/import tree of a transation unit"

data Imp = Imp { impInclude, impFile :: Text } deriving (Show, Eq, Ord)

reversePM :: PredMap p [IdOf p] -> PredMap p [IdOf p]
reversePM children = PredMap.fromListWith (++)
  [ (c, [p])
  | (p, cs) <- PredMap.toList children
  , c <- cs ]

-- | Combine keys and values into one 'PredSet'
allIds :: PredMap p [IdOf p] -> PredSet p
allIds children = mconcat
  [ PredSet.insert p (PredSet.fromList cs)
  | (p, cs) <- PredMap.toList children ]

getPathsDown :: Predicate p => PredMap p [IdOf p] -> PredMap p [[IdOf p]]
getPathsDown children = PredSet.fromSet (onId [[]]) . PredSet.keysSet $ children
  where
    onId paths p = case PredMap.lookup p children of
      Nothing -> paths
      Just next -> paths ++ concatMap (\n -> onId (map (++[n]) paths) n) next

-- | Takes a map from traces to the parents (traces that import the key).
--
-- Compiles map from traces to all import paths.  This outer lists of
-- all @[[IdOf p]]@ are non-empty. The inner-lists can be empty for files
-- that have no parents.
getPathsUp :: Predicate p => PredMap p [IdOf p] -> PredMap p [[IdOf p]]
getPathsUp parents = PredSet.fromSet (onId [[]]) . PredSet.keysSet $ parents
  where
    onId paths p = case PredMap.lookup p parents of
      Nothing -> paths
      Just next -> concatMap (\n -> onId (map (++[n]) paths) n) next

displayPath :: Predicate p => PredMap p Text -> (Text, [[IdOf p]]) -> Text
displayPath pm (pRoot, paths) = case paths of
    [] -> pRoot <> " impossible\n"
    paths -> let n = showt (length paths)
             in mconcat $ zipWith (seePaths n) [(1::Int)..] paths
  where
    get k = fromMaybe "impossible" (PredMap.lookup k pm)
    seePaths n i path = pRoot <> " " <> showt i <> "/" <> n <> ":\n" <> p
      where
        m = showt (length path)
        p = mconcat $ zipWith (seeFile m) [(1::Int)..] path
    seeFile m j f = "    " <> showt j <> "/" <> m <> " : " <> get f <> "\n"

displayPaths :: Predicate p => PredMap p Text -> PredMap p [[IdOf p]] -> Text
displayPaths pm = mconcat . map (displayPath pm) . toSorted . PredMap.toList
  where
    toSorted = sortOn fst . map getKey
    getKey (k, v) = (get k, v)
    get k = fromMaybe "impossible" (PredMap.lookup k pm)

processTranslationUnitTrace
  :: Backend be
  => be
  -> Config
  -> IO ()
processTranslationUnitTrace be cfg = do
  (children, allFiles) <- runHaxl be (cfgRepo cfg) $ do
    tutk <- getKeyOfId (cfgTranslationUnitTrace cfg)
    let rootTraceId :: IdOf Cxx.Trace
        rootTraceId = getId (Cxx.translationUnitTrace_key_trace tutk)
    children <- bfsPM getTraceIncludeIds [rootTraceId]
    let fileOfTrace :: IdOf Cxx.Trace -> Haxl () Text
        fileOfTrace t = do
          tk <- getKeyOfId t
          getKeyOfId (getId (Cxx.trace_key_file tk))
    allFiles <- PredSet.fromSetM fileOfTrace (allIds children)
    return ( children :: PredMap Cxx.Trace [IdOf Cxx.Trace]
           , allFiles :: PredMap Cxx.Trace Text)
  let parents = reversePM children :: PredMap Cxx.Trace [IdOf Cxx.Trace]
      allPathsFromKey = getPathsDown children
      allPathsToKey = getPathsUp parents
  Text.IO.putStr (displayPaths allFiles allPathsFromKey)
  Text.IO.putStr "\n------------\n\n"
  Text.IO.putStr (displayPaths allFiles allPathsToKey)

main :: IO ()
main =
  withConfigOptions options $ \(cfg, cfgOpts) ->
    withEventBaseDataplane $ \ebd ->
      withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
        withRemoteBackend ebd cfgAPI (cfgService cfg) $ \be ->
          processTranslationUnitTrace be cfg
