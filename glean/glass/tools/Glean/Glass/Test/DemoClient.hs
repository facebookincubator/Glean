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
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Default ( Default(def) )
import Control.Monad.Trans ( MonadIO(liftIO) )

import Util.OptParse ( maybeTextOption, maybeIntOption )

import Thrift.Channel.HeaderChannel ( HeaderConfig(..), withHeaderChannel )
import Thrift.Protocol.Id ( compactProtocolId )
import Util.EventBase ( withEventBaseDataplane )

newtype Config = Config { cfgFile :: Maybe Text }

options :: ParserInfo Config
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser Config
    parser = do
      cfgFile <- maybeTextOption (long "file" <> short 'f' <> metavar "FILE")
      return Config{..}

defQuery :: DocumentSymbolsRequest
defQuery = def {
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
main = Glean.withOptions options $ \Config{..} -> do
  let query = case cfgFile of
        Nothing -> defQuery
        Just f -> defQuery { documentSymbolsRequest_filepath = Path f }
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
