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

import Data.Default ( Default(def) )
import Data.Maybe ( catMaybes )
import Data.Text (Text)
import Options.Applicative
import Options.Applicative.Types ( readerAsk )
import System.FilePath ( pathSeparator )
import Util.Text ( textShow, textToInt )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Thrift.Monad as Thrift ( ThriftM )
import Thrift.Protocol ( Protocol )
import Thrift.Channel.HeaderChannel
    ( withHeaderChannel, HeaderConfig(..), HeaderWrappedChannel )
import Thrift.Protocol.Id ( compactProtocolId )

import Util.EventBase ( withEventBaseDataplane )

data Options =
  Options {
    optCommand :: Command,
    optHost :: Service
  }

data Service =
  Service {
    _svcHost :: Text,
    _svcPort :: Int
  }

defService :: Service
defService = Service "127.0.0.1" 26073

data Command
  = List RepoName Path
  | Describe SymbolId
  | FindRefs SymbolId
  | Resolve SymbolId

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
        command "resolve" (info resolveCommand
          (progDesc $ unlines
            ["Find the definition location of a symbol"
            ]
          )
        )
      return $ Options {..}

    cmd parser help = argument parser help

    symbolHelp = metavar "SYMBOL"

    listCommand = cmd readListRepoPath (metavar "REPO/PATH")
    describeCommand = cmd (Describe <$> readSymbol) symbolHelp
    findRefsCommand = cmd (FindRefs <$> readSymbol) symbolHelp
    resolveCommand = cmd (Resolve <$> readSymbol) symbolHelp

readService :: ReadM Service
readService = do
  hostPort <- Text.pack <$> readerAsk
  case Text.breakOn ":" hostPort of
    (_, "") -> fail "Not a valid host:port"
    (host, portStr) -> case textToInt portStr of
      Left _ -> fail ("Not a valid port: " <> show portStr)
      Right port -> return (Service host port)

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

defCfg :: HeaderConfig GlassService
defCfg = HeaderConfig
  { headerHost = "127.0.0.1"
  , headerPort = 26073
  , headerProtocolId = compactProtocolId
  , headerConnTimeout = 15000
  , headerSendTimeout = 15000
  , headerRecvTimeout = 15000
  }

type GlassM p a = ThriftM p HeaderWrappedChannel GlassService a

main :: IO ()
main = Glean.withOptions options $ \Options{..} ->
  withEventBaseDataplane $ \evp -> do
    res <- withHeaderChannel evp defCfg $ case optCommand of
              List repo path -> runListSymbols repo path
              Describe sym -> runDescribe sym
              FindRefs sym -> runFindRefs sym
              Resolve sym -> runResolve sym
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
  let loc = pprLocationRange $ toLocationRange symbolDescription_location
      name = pprQName symbolDescription_name
      kind = textShow <$> symbolDescription_kind
      annot = textShow <$> symbolDescription_annotations
  return $ [loc,name] <> catMaybes [kind,annot]

runResolve :: Protocol p => SymbolId -> GlassM p [Text]
runResolve sym = do
  range <- resolveSymbolRange sym def
  return [pprLocationRange range]

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

toLocationRange :: SymbolPath -> LocationRange
toLocationRange SymbolPath{..} = LocationRange {
    locationRange_repository = symbolPath_repository,
    locationRange_filepath = symbolPath_filepath,
    locationRange_range = symbolPath_range
  }
