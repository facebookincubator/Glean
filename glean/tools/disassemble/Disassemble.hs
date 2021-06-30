-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo #-}
module Disassemble(main) where

import Control.Monad
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import System.Exit (die)

import Util.OptParse

import Glean.Angle.Types (SourceSchemas)
import Glean.Database.Config (schemaSourceOption)
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Impl.ConfigProvider
import Glean.RTS.Bytecode.Disassemble (disassemble)
import Glean.Types (PredicateRef(..))
import Glean.Schema.Util (parseRef)
import Glean.Schema.Resolve (Schemas)
import Glean.Util.ConfigProvider
import Glean.Util.ThriftSource (ThriftSource)
import qualified Glean.Util.ThriftSource as ThriftSource

data Config = Config
  { cfgSchemaSource :: ThriftSource (SourceSchemas, Schemas)
  , cfgCommand :: Command
  }

newtype Command
  = PTC [Text]

options :: ParserInfo Config
options = info (parser <**> helper)
  (fullDesc <> progDesc "Disassemle bytecode")
  where
    parser :: Parser Config
    parser = do
      cfgSchemaSource <- snd <$> schemaSourceOption
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

  schemas <- ThriftSource.load cfgAPI cfgSchemaSource
  db_schema <- uncurry newDbSchema schemas readWriteContent

  case cfgCommand of
    PTC args -> predicateTypecheckers db_schema args

predicateTypecheckers :: DbSchema -> [Text] -> IO ()
predicateTypecheckers schema args = do
  preds <- case args of
    [] -> return $ schemaPredicates schema
    refs -> forM refs $ \ref -> do
      let sourceRef = parseRef ref
      maybe
        (die $ "unknown predicate '" ++ Text.unpack ref ++ "'")
        return
        $ lookupPredicate sourceRef LatestSchemaAll schema
  forM_ (sortOn predicateRef preds) $ \PredicateDetails{..} -> do
    mapM_ Text.putStrLn $ disassemble
      ("ptc_"
        <> predicateRef_name predicateRef
        <> "."
        <> Text.pack (show $ predicateRef_version predicateRef))
      predicateTypecheck
    putStrLn ""
