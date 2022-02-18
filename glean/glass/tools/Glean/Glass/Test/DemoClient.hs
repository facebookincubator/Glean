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
    ( GlassService, documentSymbolListX )

import Options.Applicative
import Options.Applicative.Types
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Default ( Default(def) )
import Control.Monad.Trans ( MonadIO(liftIO) )
import System.FilePath (pathSeparator)

import Util.OptParse ( maybeTextOption, maybeIntOption, textOption )

import Thrift.Channel.HeaderChannel ( HeaderConfig(..), withHeaderChannel )
import Thrift.Protocol.Id ( compactProtocolId )
import Util.EventBase ( withEventBaseDataplane )

newtype Options = Options { optCommand :: Command }

data Command
  = List RepoName Path

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser Options
    parser = hsubparser
      (command "list" (info listCommand
        (progDesc $ unlines
          ["List symbols in file specified by REPO/PATH"
          ," e.g. glass list react/packages/react/src/ReactHooks.js"
          ]
        )
      ))

    listCommand :: Parser Options
    listCommand = Options <$> argument listRepoPath (metavar "REPO/PATH")

listRepoPath :: ReadM Command
listRepoPath = do
  path <- Text.pack <$> readerAsk
  case Text.breakOn sep path of
    (_, "") -> fail "Not a valid repo/path"
    (repo, file) -> return $ case Text.stripPrefix sep file of
        Nothing -> List (RepoName repo) (Path file)
        Just file' -> List (RepoName repo) (Path file')
    where
      sep = Text.singleton pathSeparator

defDocumentSymbolsReq :: DocumentSymbolsRequest
defDocumentSymbolsReq = def {
  documentSymbolsRequest_repository = RepoName "react",
  documentSymbolsRequest_filepath = Path "packages/react/src/ReactHooks.js",
  documentSymbolsRequest_include_refs = True
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
    List repo path -> do
      let query = defDocumentSymbolsReq {
                  documentSymbolsRequest_repository = repo
                , documentSymbolsRequest_filepath = path
                }
      withEventBaseDataplane $ \evp ->
        withHeaderChannel evp defCfg $ do
          DocumentSymbolListXResult{..} <- documentSymbolListX query def
          liftIO $ mapM_ Text.putStrLn $
            pprDefs documentSymbolListXResult_definitions

-- List unique definition symbols
pprDefs :: [DefinitionSymbolX] -> [Text]
pprDefs = Set.toList . Set.fromList . map pprDef
  where
    pprDef :: DefinitionSymbolX -> Text
    pprDef DefinitionSymbolX{..} = unSymbolId definitionSymbolX_sym
