{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Typecheck.Monad (
    T,
    ToRtsType,
    TypecheckState(..),
    TcOpts(..),
    TcMode(..),
    defaultTcOpts,
    TcEnv(..),
    emptyTcEnv,
    initialTypecheckState,
    whenDebug,
    freshTyVar,
    freshTyVarInt,
    getPredicateDetails,
    mkWild,

    -- * Errors
    prettyError,
    prettyErrorIn,
    prettyErrorAt,
    inPat,
    addErrSpan,
  ) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Text.Prettyprint.Doc hiding ((<>), enclose)

import Glean.Angle.Types hiding (Type)
import qualified Glean.Angle.Types as Schema
import qualified Glean.Database.Config as Config
import Glean.Database.Schema.Types
import Glean.Display
import Glean.Query.Typecheck.Types
import Glean.Query.Codegen.Types
import qualified Glean.RTS.Term as RTS
import Glean.RTS.Types as RTS
import Glean.Util.Some

type T a = StateT TypecheckState (ExceptT (Doc ()) IO) a

type ToRtsType = Schema.Type -> Maybe Type

whenDebug :: T () -> T ()
whenDebug act = do
  lvl <- gets tcDebug
  when lvl act

freshTyVarInt :: T Int
freshTyVarInt = do
  TypecheckState{tcNextTyVar = n} <- get
  modify $ \s -> s { tcNextTyVar = n+1 }
  return n

freshTyVar :: T Type
freshTyVar = TyVar <$> freshTyVarInt

data TcMode = TcModeQuery | TcModePredicate
  deriving Eq

data TcEnv = TcEnv
  { tcEnvTypes :: HashMap TypeId TypeDetails
  , tcEnvPredicates :: HashMap PredicateId PredicateDetails
  }

emptyTcEnv :: TcEnv
emptyTcEnv = TcEnv HashMap.empty HashMap.empty

data TypecheckState = TypecheckState
  { tcEnv :: TcEnv
  , tcAngleVersion :: AngleVersion
  , tcDebug :: !Bool
  , tcRtsType :: ToRtsType
  , tcNextVar :: {-# UNPACK #-} !Int
  , tcNextTyVar :: {-# UNPACK #-} !Int
  , tcScope :: HashMap Name Var
    -- ^ Variables that we have types for, and have allocated a Var
  , tcVisible :: HashSet Name
    -- ^ Variables that are currently visible
  , tcFree :: HashSet Name
    -- ^ Variables that are mentioned only once
  , tcUses :: HashSet Name
    -- ^ Accumulates variables that appear in an ContextExpr context
  , tcBindings :: HashSet Name
    -- ^ Accumulates variables that appear in an ContextPat context
  , tcMode :: TcMode
  , tcDisplayOpts :: DisplayOpts
    -- ^ Options for pretty-printing
  , tcVars :: IntMap Var
  , tcSubst :: IntMap Type
  , tcPromote :: [(Type, Type, Some IsSrcSpan)]
  }

data TcOpts = TcOpts
  { tcOptDebug :: !Config.DebugFlags
  , tcOptAngleVersion :: !AngleVersion
  }

defaultTcOpts :: Config.DebugFlags -> AngleVersion -> TcOpts
defaultTcOpts debug v = TcOpts
  { tcOptDebug = debug
  , tcOptAngleVersion = v
  }

initialTypecheckState
  :: TcEnv
  -> TcOpts
  -> ToRtsType
  -> TcMode
  -> TypecheckState
initialTypecheckState tcEnv TcOpts{..} rtsType mode = TypecheckState
  { tcEnv = tcEnv
  , tcAngleVersion = tcOptAngleVersion
  , tcDebug = Config.tcDebug tcOptDebug
  , tcRtsType = rtsType
  , tcNextVar = 0
  , tcNextTyVar = 0
  , tcScope = HashMap.empty
  , tcVisible = HashSet.empty
  , tcFree = HashSet.empty
  , tcUses = HashSet.empty
  , tcBindings = HashSet.empty
  , tcMode = mode
  , tcDisplayOpts = defaultDisplayOpts
      -- might make this configurable with flags later
  , tcSubst = IntMap.empty
  , tcVars = IntMap.empty
  , tcPromote = []
  }

getPredicateDetails :: PredicateId -> T PredicateDetails
getPredicateDetails pred = do
  TcEnv{..} <- gets tcEnv
  case HashMap.lookup pred tcEnvPredicates of
    Nothing -> error $ "predicateKeyTYpe: " <> show (displayDefault pred)
    Just d -> return d

-- Smart constructor for wildcard patterns; replaces a wildcard that
-- matches the unit type with a concrete pattern.  This is necessary
-- when we have an enum in an expression position: we can't translate
-- @nothing@ into @{ nothing = _ }@ because the wildcard would be
-- illegal in an expression.
mkWild :: Type -> TcPat
mkWild ty
  | RecordTy [] <- derefType ty = RTS.Tuple []
  | otherwise = RTS.Ref (MatchWild ty)

prettyError :: Doc () -> T a
prettyError = throwError

prettyErrorIn :: IsSrcSpan s => SourcePat_ s p t -> Doc () -> T a
prettyErrorIn pat doc = prettyErrorAt (sourcePatSpan pat) doc

prettyErrorAt :: IsSrcSpan span => span -> Doc () -> T a
prettyErrorAt span doc = prettyError $ vcat
  [ pretty span
  , doc
  ]

inPat :: (IsSrcSpan s) => SourcePat_ s p t -> T a -> T a
inPat pat = addErrSpan (sourcePatSpan pat)

addErrSpan :: (IsSrcSpan s) => s -> T a -> T a
addErrSpan span act = do
  act `catchError` \errDoc -> do
    prettyError $ vcat
      [ pretty span
      , errDoc
      ]
