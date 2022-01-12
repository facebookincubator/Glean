{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Derive.Lib
  ( dispatchDerive
  , allPredicates
  , DerivePass(..)
  , optionsPasses
  ) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Options.Applicative as O

import qualified Glean.Schema.Cxx1 as Cxx
import qualified Glean.Schema.Src as Src
import Glean.Typed

import Derive.Env

import Derive.CxxDeclarationSources (deriveCxxDeclarationSources)
import Derive.CxxDeclarationTargets (deriveCxxDeclarationTargets)
import Derive.CxxSame (deriveSame)
import Derive.CxxTargetUses (deriveUses)
import Derive.Generic (derivePredicate)

-- | Uniform way to call our standard derived passes.  One can return
-- a subsequent 'DerivePass' to allow further work that can read
-- what was just written.
dispatchDerive :: Env -> DerivePass -> IO ()
dispatchDerive env = \case
  DeriveTargetUses -> exec deriveUses
  DeriveDeclFamilies -> exec deriveSame
  DeriveFunctionCalls -> execN deriveCxxDeclarationTargets
  DeriveFunctionCalls_Pass_2 -> exec deriveCxxDeclarationSources
  DeriveGeneric p -> storedPredicate p
  where
    exec f = withEnvWriter env $ f (envBackend env) (envConfig env)
    execN f = f (envBackend env) (envConfig env) (withEnvWriters env)
    storedPredicate = derivePredicate (envBackend env) (envConfig env)

allPredicates :: [SchemaPredicates]
allPredicates =
  [ Cxx.allPredicates
  , Src.allPredicates
  ]

-- | Central enumeration of all derived passes supported by "Derive"
data DerivePass
  = DeriveTargetUses
  | DeriveDeclFamilies
  | DeriveFunctionCalls
  | DeriveFunctionCalls_Pass_2
  | DeriveGeneric Text
  deriving (Eq,Ord)

allManualPasses :: [DerivePass]
allManualPasses =
  [ DeriveTargetUses
  , DeriveDeclFamilies
  , DeriveFunctionCalls
  , DeriveFunctionCalls_Pass_2
  ]

optionsPasses :: O.Parser (Set DerivePass)
optionsPasses =
  -- With no flags, we run all the derive passes. If you specify
  -- one or more flags, we run only those passes.
  fmap Set.fromList $
    some (foldr1 (<|>)
      [ O.flag' DeriveTargetUses (O.long "target-uses")
      , O.flag' DeriveDeclFamilies (O.long "decl-families")
      , O.flag' DeriveFunctionCalls (O.long "function-calls")
      , O.flag' DeriveFunctionCalls_Pass_2 (O.long "function-calls-pass-2")
      , DeriveGeneric <$> O.option (Text.pack <$> O.str)
          (  O.long "predicate"
          <> O.metavar "NAME[.VERSION]"
          )
      ])
    <|>
      pure allManualPasses
