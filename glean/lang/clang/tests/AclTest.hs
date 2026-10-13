{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings, TypeApplications #-}
-- | Run the data-driven ACL snapshot tests with the recursive Clang indexer.
module AclTest (main) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO

import Util.IO (listDirectoryRecursive)

import Glean
import Glean.Database.Ownership (factOwnership)
import Glean.Database.Types (Env(..))
import Glean.Indexer
import Glean.LocalOrRemote (BackendKind(..), LocalOrRemote(..))
import Glean.Regression.Snapshot.Acl (aclTestMain)
import Glean.Regression.Snapshot.Driver
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.Some
import qualified Glean.Clang.Test as Clang
import Glean.Clang.Test.DerivePass (derivePasses)

import Derive.Lib (DerivePass(..))

main :: IO ()
main = aclTestMain aclDriver

aclDriver :: Driver Clang.Options
aclDriver = recursiveDriver
  { driverGroups = take 1 . driverGroups recursiveDriver
  , driverIndexer = driverIndexer recursiveDriver
      `indexerThen` deriveAndDumpOwners
  }
  where
  recursiveDriver = Clang.driverWithSourceFiles listDirectoryRecursive

-- | The deriver must read every DeclarationTargets fact, as it does in
-- production, whatever groups the scenario's caller has.
deriveAndDumpOwners :: Clang.Options -> RunIndexer
deriveAndDumpOwners opts (Some backend) repo params =
  case backendKind backend of
    BackendThrift{} -> fail "the ACL test needs a local backend"
    BackendEnv env -> do
      let unfiltered = env { envAclCheckEnabled = return False }
      derivePasses [DeriveFunctionCalls_Pass_2] opts unfiltered repo params
      dumpOwners unfiltered repo

dumpOwners :: Env -> Repo -> IO ()
dumpOwners env repo = do
  dump @Cxx.DeclarationSources "DeclarationSources"
    "{ F, Name, Path } where \
    \F = cxx1.DeclarationSources { target = { function_ = { \
    \name = { name = { name = Name : string } }, \
    \source = { file = File } } } }; \
    \src.File Path = File"
  dump @Cxx.DeclarationTargets "DeclarationTargets"
    "{ F, Name, Path } where \
    \F = cxx1.DeclarationTargets { source = { function_ = { \
    \name = { name = { name = Name : string } }, \
    \source = { file = File } } } }; \
    \src.File Path = File"
  dump @Cxx.FunctionDeclaration "FunctionDeclaration"
    "{ F, Name, Path } where \
    \F = cxx1.FunctionDeclaration { \
    \name = { name = { name = Name : string } }, source = { file = File } }; \
    \src.File Path = File"
  dump @Cxx.FunctionDefinition "FunctionDefinition"
    "{ F, Name, Path } where \
    \F = cxx1.FunctionDefinition { declaration = { \
    \name = { name = { name = Name : string } }, source = { file = File } } }; \
    \src.File Path = File"
  dump @Cxx.FunctionQName "FunctionQName"
    "{ F, Name, \"\" } where \
    \F = cxx1.FunctionQName { name = { name = Name : string } }"
  where
  dump :: forall p. Predicate p => Text -> Text -> IO ()
  dump kind query = do
    rows <- runQuery_ env repo $ angleData @(p, Text, Text) query
    forM_ rows $ \(fact, name, path) -> do
      owner <- factOwnership env repo (idOf (getId fact))
      hPutStrLn stderr $ Text.unpack $ Text.unwords
        ["OWNER", kind, name, "@", path, "=", Text.pack (show owner)]
