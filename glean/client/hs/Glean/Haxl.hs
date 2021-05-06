{-# LANGUAGE ConstraintKinds #-}
module Glean.Haxl
  ( runHaxl
  , runHaxlWithWrites
  , Haxl
  , get
  , getRec
  , getKey
  , getKeyRec
  , getOfId
  , getRecOfId
  , getKeyOfId
  , getKeyRecOfId
  , keyOf
  , search
  , search_
  , Query
  , query
  , recursive
  , limit
  , trySyncHaxl
  , HaxlQuery
  , getFirstResult
    -- * re-export
  , Backend.initGlobalState
  ) where

import Control.Exception (SomeException)
import Data.Maybe (listToMaybe)
import Data.Typeable

import qualified Haxl.Core as Haxl
import Haxl.Core (GenHaxl)
import Haxl.Core.Monad ( catchIf )
import Haxl.DataSource.Glean
import Haxl.DataSource.Glean.Backend as Backend
import Util.Control.Exception ( isSyncException )

import Glean.Backend
import Glean.Query.Thrift
import Glean.Types
import Glean.Typed

type Haxl = GenHaxl ()

-- | Constraints needed for 'query' or 'search' or 'search_' from
-- "Haxl.DataSource.Glean"
type HaxlQuery p =
  ( Typeable p
  , Show p
  , ThriftQuery p
  )

-- | Initialize for Glean queries
initHaxlEnv :: Backend be => be -> Repo -> IO (Haxl.Env () w)
initHaxlEnv backend repo = do
  (state1,state2) <- initGlobalState backend repo
  let st = Haxl.stateSet state1 $ Haxl.stateSet state2 Haxl.stateEmpty
  Haxl.initEnv st ()

runHaxl :: Backend be => be -> Repo -> Haxl w a -> IO a
runHaxl backend repo h = do
  e <- initHaxlEnv backend repo
  Haxl.runHaxl e h

runHaxlWithWrites :: Backend be => be -> Repo -> Haxl w a -> IO (a, [w])
runHaxlWithWrites backend repo h = do
  e <- initHaxlEnv backend repo
  Haxl.runHaxlWithWrites e h

-- | if the Fact has a key, return it, otherwise fetch it with 'getKey'
keyOf
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p )
  => p
  -> Haxl w (KeyType p)
keyOf f = case getFactKey f of
  Nothing -> getKey f
  Just k -> return k

-- | Catch non-asyncronous exceptions inside Haxl, very good for
-- thrift calls that throw interesting exceptions.
trySyncHaxl :: Haxl w b -> Haxl w (Either SomeException b)
trySyncHaxl act = catchIf isSyncException (Right <$> act) (return . Left)

getFirstResult :: (HaxlQuery p) => Query p -> Haxl w (Maybe p)
getFirstResult = fmap (listToMaybe . fst) . search . limit 1
