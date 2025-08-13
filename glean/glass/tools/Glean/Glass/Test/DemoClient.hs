{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Glass.Test.DemoClient ( main ) where

import Glean.Init as Glean ( withOptions )

import Glean.Glass.Types
import Glean.Glass.GlassService.Client

import Control.Exception
import Data.Default ( Default(def) )
import Data.Maybe ( catMaybes )
import Data.Text (Text)
import Options.Applicative
import Options.Applicative.Types ( readerAsk )
import System.FilePath ( pathSeparator )
import Util.Text ( textShow, textToInt )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Glean.Impl.ThriftService
import Glean.Util.ThriftService
import Glean.Util.Service
import Thrift.Protocol
import Thrift.Monad hiding (runThrift)
import Thrift.Channel

import Util.EventBase ( withEventBaseDataplane )

data Options =
  Options {
    optCommand :: Command,
    optHost :: Service
  }

defService :: Service
defService = HostPort "127.0.0.1" 26073

data Command
  = List RepoName Path
  | Describe SymbolId
  | FindRefs SymbolId
  | FindLocation SymbolId
  | Search RepoName Text Bool{- case-insensitve? -}

options :: ParserInfo Options
options = info (helper <*> parser) (fullDesc <>
    progDesc "Glass code navigation and search tool")
  where
    parser :: Parser Options
    parser = do
      optHost <- option readService
        (value defService
        <> long "service"
        <> metavar "HOST:PORT"
        )
      optCommand <- hsubparser $
        command "list" (info listCommand
          (progDesc $ unlines
            ["List symbols in file specified by REPO/PATH"
            ]
          )
        ) <>
        command "describe" (info describeCommand
          (progDesc $ unlines
            ["Describe basic details of a symbol"
            ]
          )
        ) <>
        command "references" (info findRefsCommand
          (progDesc $ unlines
            ["Find references to this symbol"
            ]
          )
        ) <>
        command "location" (info locationCommand
          (progDesc $ unlines
            ["Find the definition location of a symbol"
            ]
          )
        ) <>
        command "search" (info searchCommand
          (progDesc $ unlines
            ["Find entities matching string"
            ]
          )
        )
      return $ Options {..}

    cmd parser help = argument parser help

    symbolHelp = metavar "SYMBOL"
    searchHelp = metavar "REPO/STR"

    listCommand = cmd readListRepoPath (metavar "REPO/PATH")
    describeCommand = cmd (Describe <$> readSymbol) symbolHelp
    findRefsCommand = cmd (FindRefs <$> readSymbol) symbolHelp
    locationCommand = cmd (FindLocation <$> readSymbol) symbolHelp
    searchCommand = do
      ~(repo,needle) <- argument readSearch searchHelp
      ignoreCase <- switch
        (  long "case-insensitive"
        <> short 'i'
        )
      return $ Search repo needle ignoreCase

readService :: ReadM Service
readService = do
  hostPort <- Text.pack <$> readerAsk
  case Text.breakOn ":" hostPort of
    (_, "") -> fail "Not a valid host:port"
    (host, portStr) -> case textToInt (Text.drop 1 portStr) of
      Left _ -> fail ("Not a valid port: " <> show portStr)
      Right port -> return (HostPort host (fromIntegral port))

readSearch :: ReadM (RepoName, Text)
readSearch = do
  repoStr <- Text.pack <$> readerAsk
  case Text.breakOn "/" repoStr of
    (_, "") -> fail "Not a valid repo/string"
    (repo, str) -> return (RepoName repo, Text.tail str)

readListRepoPath :: ReadM Command
readListRepoPath = do
  path <- Text.pack <$> readerAsk
  case Text.breakOn sep path of
    (_, "") -> fail "Not a valid repo/path"
    (repo, file) -> return $ case Text.stripPrefix sep file of
        Nothing -> List (RepoName repo) (Path file)
        Just file' -> List (RepoName repo) (Path file')
    where
      sep = Text.singleton pathSeparator

readSymbol :: ReadM SymbolId
readSymbol = do
  sym <- Text.pack <$> readerAsk
  if length (Text.splitOn "/" sym) < 3
    then err
    else return (SymbolId sym)
  where
    err = fail "Not a valid symbol. Symbols have the form: repo/lang/path-ish"

defDocumentSymbolsReq :: DocumentSymbolsRequest
defDocumentSymbolsReq = def {
  documentSymbolsRequest_repository = RepoName "react",
  documentSymbolsRequest_filepath = Path "packages/react/src/ReactHooks.js",
  documentSymbolsRequest_include_refs = False -- just definitions
}

defCfg :: ThriftServiceOptions
defCfg = def { processingTimeout = Just 15000 }

type GlassM p a = forall c . ClientChannel c => ThriftM p c GlassService a

svc :: Service -> Maybe (ThriftService GlassService)
svc s =  mkThriftService s defCfg

main :: IO ()
main = Glean.withOptions options $ \Options{..} ->
  withEventBaseDataplane $ \evp ->
    case svc optHost of
      Nothing ->
        throwIO $ ErrorCall $
          "Service not supported: " <> serviceToString optHost
      Just thriftService -> do
        res <- runThrift evp thriftService $
          case optCommand of
            List repo path -> runListSymbols repo path
            Describe sym -> runDescribe sym
            FindRefs sym -> runFindRefs sym
            FindLocation sym -> runLocation sym
            Search repo str ignoreCase -> runSearch repo str ignoreCase
        mapM_ Text.putStrLn res

runListSymbols :: Protocol p => RepoName -> Path -> GlassM p [Text]
runListSymbols repo path = do
  DocumentSymbolListXResult{..} <- documentSymbolListX query def
  return $ pprDefs documentSymbolListXResult_definitions
  where
    query = defDocumentSymbolsReq {
            documentSymbolsRequest_repository = repo
          , documentSymbolsRequest_filepath = path
    }

runDescribe :: Protocol p => SymbolId -> GlassM p [Text]
runDescribe sym = do
  SymbolDescription{..} <- describeSymbol sym def
  let loc = pprLocationRange $ locationRange symbolDescription_location
      name = pprQName symbolDescription_name
      kind = textShow <$> symbolDescription_kind
      annot = textShow <$> symbolDescription_annotations
  return $ [loc,name] <> catMaybes [kind,annot]

runSearch :: Protocol p => RepoName -> Text -> Bool -> GlassM p [Text]
runSearch repoName strName ignoreCase = do
  SymbolSearchResult syms _ <- searchSymbol req def
  return (map textShow syms)
  where
    req = SymbolSearchRequest
        strName
        (Just repoName)
        def -- language
        def -- kinds
        def { -- search options
          symbolSearchOptions_ignoreCase = ignoreCase
        }

runLocation :: Protocol p => SymbolId -> GlassM p [Text]
runLocation sym = do
  range <- symbolLocation sym def
  return [pprLocationRange (symbolLocation_location range)]

runFindRefs :: Protocol p => SymbolId -> GlassM p [Text]
runFindRefs sym = do
  ranges <- findReferenceRanges sym def
  return $
    ("Found " <> textShow (length ranges) <> " references") :
    map pprLocationRange ranges

-- List unique definition symbols
pprDefs :: [DefinitionSymbolX] -> [Text]
pprDefs = map (unSymbolId . definitionSymbolX_sym)

pprLocationRange :: LocationRange -> Text
pprLocationRange LocationRange{..} =
   Text.concat [repo, "@", path,":", pprRange range]
  where
    RepoName repo = locationRange_repository
    Path path = locationRange_filepath
    range = locationRange_range

pprRange :: Range -> Text
pprRange (Range lineBegin colBegin lineEnd colEnd) = Text.concat [
    textShow lineBegin, ":", textShow colBegin, "-",
    textShow lineEnd, ":", textShow colEnd
  ]

pprQName :: QualifiedName -> Text
pprQName QualifiedName{..} =
  unName qualifiedName_container <> ":" <> unName qualifiedName_localName

locationRange :: SymbolPath -> LocationRange
locationRange SymbolPath{..} = LocationRange {
    locationRange_repository = symbolPath_repository,
    locationRange_filepath = symbolPath_filepath,
    locationRange_range = symbolPath_range
  }
