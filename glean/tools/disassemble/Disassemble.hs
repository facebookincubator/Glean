{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Disassemble(main) where

import Control.Monad
import Data.Default
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import System.Exit (die)

import Util.OptParse

import Glean.Database.Config (
  schemaLocationOption, schemaLocationToSource, SchemaLocation)
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Impl.ConfigProvider
import Glean.RTS.Bytecode.Disassemble (disassemble)
import Glean.Types (PredicateRef(..))
import Glean.Schema.Util (parseRef)
import Glean.Util.ConfigProvider
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Bytecode.SysCalls (typecheckSysCalls)

data Config = Config
  { cfgSchemaLocation :: SchemaLocation
  , cfgCommand :: Command
  }

newtype Command
  = PTC [Text]

options :: ParserInfo Config
options = info (parser <**> helper)
  (fullDesc <> progDesc "Disassemble bytecode")
  where
    parser :: Parser Config
    parser = do
      cfgSchemaLocation <- schemaLocationOption
      cfgCommand <- typecheckerCmd
      return Config{..}

    typecheckerCmd =
      commandParser "ptc" (progDesc "Disassemble predicate typecheckers")
        $ fmap (PTC . map Text.pack)
        $ many
        $ strArgument
          (  metavar "PREDICATE"
          <> help "Predicate reference"
          )

main :: IO ()
main =
  withConfigOptions options $ \(Config{..}, cfg) ->
  withConfigProvider cfg $ \(cfgAPI :: ConfigAPI) -> do

  schemas <- ThriftSource.load cfgAPI (schemaLocationToSource cfgSchemaLocation)
  db_schema <- newDbSchema Nothing schemas LatestSchema readWriteContent def

  case cfgCommand of
    PTC args -> predicateTypecheckers db_schema args

predicateTypecheckers :: DbSchema -> [Text] -> IO ()
predicateTypecheckers schema args = do
  preds <- case args of
    [] -> return $ schemaPredicates schema
    refs -> forM refs $ \ref -> do
      let sourceRef = parseRef ref
      either (die . Text.unpack) return
        $ lookupPredicateSourceRef sourceRef LatestSchema schema
  forM_ (sortOn predicateRef preds) $ \d@PredicateDetails{..} -> do
    let PredicateRef{..} = predicateRef d
    mapM_ Text.putStrLn $ disassemble
      ("ptc_"
        <> predicateRef_name
        <> "."
        <> Text.pack (show $ predicateRef_version))
      typecheckSysCalls
      predicateTypecheck
    putStrLn ""
