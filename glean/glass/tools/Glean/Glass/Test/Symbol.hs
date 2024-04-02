{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Glean.Glass.Test.Symbol
  ( main
  ) where

import Data.Default ( Default(def) )
import Data.Int (Int64 ,Int32)
import Control.Exception ( handle )
import Control.Monad ( forM_ ,(<=<) )
import Data.Text ( Text )
import Text.Printf ( printf )
import Data.Tuple.Extra (fst3,thd3)
import Options.Applicative

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Glean.Init ( withOptions )
import Util.OptParse ( maybeTextOption, maybeIntOption )
import Util.Timing ( showTime, timeIt )
import Util.Text ( textShow )
import Util.IO ( readFileUTF8Text )

import Glean.Glass.Types
import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Handler as Handle
import qualified Glean.Glass.Main as Glass
import qualified Glean.Glass.Options as Glass
import Glean.Glass.SnapshotBackend (snapshotBackendParser)

data Config = Config
  { cfgGlass :: Glass.Config
  , cfgSymbol :: Maybe SymbolId
  , cfgFile :: Maybe Text
  , cfgCmd :: Cmd
  , cfgLimit :: Maybe Int32
  }

data Cmd
  = FindLocation
  | FindReferences
  | Describe

options :: ParserInfo Config
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser Config
    parser = do
      cfgGlass <- Glass.setSnapshotBackend
        <$> snapshotBackendParser
        <*> Glass.configParser
      cfgCmd <- (\(cmd::String) -> case cmd of
                    "references" -> FindReferences
                    "describe" -> Describe
                    _ -> FindLocation)
            <$> strOption (short 'c' <> long "command" <> metavar "COMMAND")
      cfgSymbol <- fmap SymbolId <$>
        maybeTextOption (long "symbol" <> short 's' <> metavar "SYMBOL")
      cfgFile <-
        maybeTextOption (long "file" <> short 'f' <> metavar "FILE")
      cfgLimit <- fmap fromIntegral <$>
        maybeIntOption (long "limit" <> short 'n' <> metavar "LIMIT")
      return Config{..}

main :: IO ()
main =
  withOptions options $ \Config{cfgGlass = glassConfig, ..} ->
  Glass.withEnv glassConfig Nothing $ \env -> do

    syms <- case (cfgSymbol, cfgFile) of
      (Nothing, Nothing) ->
        map SymbolId . Text.lines <$> Text.getContents

      (Just symid, _) -> return [symid]
      (_, Just file) -> map SymbolId . Text.lines <$>
         readFileUTF8Text (Text.unpack file)

    case cfgCmd of
      FindLocation -> do
        let resolveLocation :: SymbolId -> IO LocationRange
            resolveLocation r =
              Handle.resolveSymbolRange env r (def :: RequestOptions)
        forM_ syms $ Text.putStrLn <=< testResolveSymbol resolveLocation

      FindReferences -> do
        let findReferences :: SymbolId -> IO [LocationRange]
            findReferences r =
              Handle.findReferenceRanges env r
                (def  { requestOptions_limit = cfgLimit } )
        forM_ syms $ mapM_ Text.putStrLn <=< testFindReferences findReferences

      Describe -> do
        let describe :: SymbolId -> IO SymbolDescription
            describe r =
              Handle.describeSymbol env r
                (def  { requestOptions_limit = cfgLimit } )
        forM_ syms $ Text.putStrLn <=< testDescribe describe

testFindReferences
  :: (SymbolId -> IO [LocationRange])
  -> SymbolId
  -> IO [Text]
testFindReferences handler symbol@(SymbolId name) = do
  handle (\(ServerException e) -> return ["FAIL " <> e]) $ do
    res <- runTest handler symbol
    let title = Text.concat
          [ "OK "
          , Text.pack (showTime (fst3 res))
          , " "
          , name
          , Text.pack (printf " (Found %d references)" (length $ thd3 res))
          ]
    let body = map pprLocationRange (thd3 res)

    return $ title : body

testResolveSymbol
  :: (SymbolId -> IO LocationRange)
  -> SymbolId
  -> IO Text
testResolveSymbol handler symbol@(SymbolId name) = do
  handle (\(ServerException e) -> return ("FAIL " <> e)) $ do
    res <- runTest handler symbol
    return $ Text.concat
      [ "OK "
      , Text.pack (showTime (fst3 res))
      , " "
      , name
      ," -> "
      ,pprLocationRange (thd3 res)
      ]

testDescribe
  :: (SymbolId -> IO SymbolDescription)
  -> SymbolId
  -> IO Text
testDescribe handler symbol@(SymbolId name) = do
  handle (\(ServerException e) -> return ("FAIL " <> e)) $ do
    res <- runTest handler symbol
    return $ Text.concat
      [ "OK "
      , Text.pack (showTime (fst3 res))
      , " "
      , name
      ," -> "
      ,pprDescription (thd3 res)
      ]

pprLocationRange :: LocationRange -> Text
pprLocationRange LocationRange{..} =
  Text.concat [
    unRepoName locationRange_repository, "@",
    unPath locationRange_filepath, " ",
    pprRange locationRange_range
  ]

pprSymbolPath :: SymbolPath -> Text
pprSymbolPath SymbolPath{..} = pprLocationRange LocationRange{..}
  where
    locationRange_filepath  = symbolPath_filepath
    locationRange_range = symbolPath_range
    locationRange_repository = symbolPath_repository

pprQName :: QualifiedName -> Text
pprQName QualifiedName{..} = Text.concat
  [ "container: ", unName qualifiedName_container
  , " "
  , "localname: ", unName qualifiedName_localName
  ]

pprKind :: Maybe SymbolKind -> Text
pprKind = textShow

pprDescription :: SymbolDescription -> Text
pprDescription SymbolDescription{..} =
  Text.unlines [
    pprSymbolPath symbolDescription_location,
    "qualified name: "<> pprQName symbolDescription_name,
    "symbol kind: "<> pprKind symbolDescription_kind
  ]

pprRange :: Range -> Text
pprRange Range{..} = Text.concat
  [ "("
  , textShow range_lineBegin, ":"
  , textShow range_columnBegin, "-"
  , textShow range_lineEnd, ":"
  , textShow range_columnEnd
  , ")"
  ]

runTest
  :: (a -> IO r)
  -> a
  -> IO (Double, Int64, r)
runTest handler sym = timeIt $ handler sym
