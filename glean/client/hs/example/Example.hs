{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-
Example client for Glean to provide a basic "jump to
definition". Given a file name and a byte offset into the file,
returns the target of the identifier at that position, if any.

$ ./example --service localhost:25052 some/File.h 11007
[XRefTarget_unknown (SrcLoc {srcLoc_file = File {file_id = 2718, file_key = Nothing}, srcLoc_line = Nat {unNat = 23}, srcLoc_column = Nat {unNat = 1}})]
-}

module Example where

import qualified Data.Set as Set
import Data.String
import Options.Applicative

import Util.EventBase

import qualified Glean
import Glean.Angle as Angle
import Glean.Impl.ConfigProvider
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.ConfigProvider
import Glean.Util.Range
import Glean.Util.XRefs


data Config = Config
  { cfgService :: Glean.ThriftSource Glean.ClientConfig
  , cfgFile :: FilePath
  , cfgOffset :: Int
  }

options :: ParserInfo Config
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser Config
    parser = do
      cfgService <- Glean.options
      cfgFile <- strArgument (metavar "FILE")
      cfgOffset <- argument auto (metavar "OFFSET")
      return Config{..}

main :: IO ()
main = do
  withConfigOptions options $ \(cfg, cfgOpts) ->
    withEventBaseDataplane $ \evb ->
      withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
        Glean.withRemoteBackend evb cfgAPI (cfgService cfg)
          $ \backend -> do
            repo <- Glean.getLatestRepo backend "fbsource"
            doQuery backend cfg repo

doQuery :: Glean.Backend b => b -> Config -> Glean.Repo -> IO ()
doQuery backend Config{..} repo = do
  let
    printResults results = print
      [ target
      | (ByteRange{..}, target) <- Set.toList $ collectXRefTargets results
      , byteRange_begin == fromIntegral cfgOffset ]

  -- Query using Angle syntax
  --   [+] Concise
  --   [+] Supports arbitrary Angle queries
  --   [-] Not typechecked at compile time
  --   [-] Hard to build queries programmatically
  --   [?] May break if the schema changes (unless you use explicit
  --       predicate versions)
  results <- Glean.runQuery_ backend repo $ Glean.angle $
    "cxx1.FileXRefs { xmap = { file = \"" <> fromString cfgFile <> "\" }}"
  printResults results

  -- Query using Angle combinators
  --   [-] More verbose than Angle syntax
  --   [+] Supports arbitrary Angle queries
  --   [+] Typechecked at compile time
  --   [+] Easy to build queries programmatically
  --   [+] Does not break when the schema changes
  results <- Glean.runQuery_ backend repo $ Angle.query $
    predicate @Cxx.FileXRefs $
      rec $
        field @"xmap" (rec (field @"file" (fromString cfgFile) end))
      end
  printResults results
