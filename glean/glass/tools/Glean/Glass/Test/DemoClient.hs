{-# LANGUAGE ApplicativeDo #-}
{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Test.DemoClient (main) where

import Glean.Init as Glean ( withOptions )

import Glean.Glass.Types
import Glean.Glass.GlassService.Client
    ( documentSymbolListX, GlassService, describeSymbol )

import Options.Applicative
import Options.Applicative.Types ( readerAsk )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Default ( Default(def) )
import Control.Monad.Trans ( MonadIO(liftIO) )
import System.FilePath (pathSeparator)

import Util.OptParse ( maybeTextOption, maybeIntOption, textOption )
import Util.Text ( textShow )

import Thrift.Channel.HeaderChannel ( HeaderConfig(..), withHeaderChannel )
import Thrift.Protocol.Id ( compactProtocolId )
import Util.EventBase ( withEventBaseDataplane )
import Control.Monad.Extra (whenJust)
newtype Options = Options { optCommand :: Command }

data Command
  = List RepoName Path
  | Describe SymbolId

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser Options
    parser = hsubparser $
      command "list" (info listCommand
        (progDesc $ unlines
          ["List symbols in file specified by REPO/PATH"
          ," e.g. glass list react/packages/react/src/ReactHooks.js"
          ]
        )
      ) <>
      command "describe" (info describeCommand
        (progDesc $ unlines
          ["Describe basic details of a symbol"
          ," e.g. glass describe react/js/ReactHooks.js/src/react/packages/react/useEffect"
          ]
        )
      )

    listCommand :: Parser Options
    listCommand = Options <$> argument readListRepoPath (metavar "REPO/PATH")

    describeCommand :: Parser Options
    describeCommand = Options <$> argument readDescribeSymbol (metavar "SYMBOL")

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

readDescribeSymbol :: ReadM Command
readDescribeSymbol = do
  sym <- Text.pack <$> readerAsk
  if length (Text.splitOn "/" sym) < 3
    then err
    else return $ Describe (SymbolId sym)
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

main :: IO ()
main = Glean.withOptions options $ \Options{..} -> do
  case optCommand of
    List repo path -> runListSymbols repo path
    Describe sym -> runDescribe sym

runListSymbols :: RepoName -> Path -> IO ()
runListSymbols repo path =
  withEventBaseDataplane $ \evp -> withHeaderChannel evp defCfg $ do
      DocumentSymbolListXResult{..} <- documentSymbolListX query def
      liftIO $ mapM_ Text.putStrLn $
        pprDefs documentSymbolListXResult_definitions
  where
    query = defDocumentSymbolsReq {
            documentSymbolsRequest_repository = repo
          , documentSymbolsRequest_filepath = path
    }

runDescribe :: SymbolId -> IO ()
runDescribe sym =
  withEventBaseDataplane $ \evp -> withHeaderChannel evp defCfg $ do
      SymbolDescription{..} <- describeSymbol sym def
      liftIO $ Text.putStrLn $ pprSymbolPath symbolDescription_location
      liftIO $ Text.putStrLn $ pprQName symbolDescription_name
      whenJust symbolDescription_kind $ liftIO . print

-- List unique definition symbols
pprDefs :: [DefinitionSymbolX] -> [Text]
pprDefs = map pprDef
  where
    pprDef :: DefinitionSymbolX -> Text
    pprDef DefinitionSymbolX{..} = unSymbolId definitionSymbolX_sym

pprSymbolPath :: SymbolPath -> Text
pprSymbolPath SymbolPath{..} = Text.concat [repo, "@", path,":", pprRange range]
  where
    RepoName repo = symbolPath_repository
    Path path = symbolPath_filepath
    range = symbolPath_range

pprRange :: Range -> Text
pprRange (Range lineBegin colBegin lineEnd colEnd) = Text.concat [
    textShow lineBegin, ":", textShow colBegin, "-",
    textShow lineEnd, ":", textShow colEnd
  ]

pprQName :: QualifiedName -> Text
pprQName QualifiedName{..} =
  unName qualifiedName_container <> ":" <> unName qualifiedName_localName
