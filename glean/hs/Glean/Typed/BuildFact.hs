{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
module Glean.Typed.BuildFact
  ( NewFact(newFact), makeFact, makeFact_, makeFactV, makeFactV_
  , Facts, newFacts, serializeFacts, factsMemory
  , FactBuilder, buildFacts, extendFacts, buildBatch
  ) where

import Data.Maybe
import Control.Monad (void)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Glean.RTS.Builder (sizeOfBuilder, withBuilder)
import Glean.RTS.Foreign.Define (defineFact)
import Glean.RTS.Foreign.FactSet (FactSet)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Types (lowestFid)
import Glean.Typed.Binary
import Glean.Typed.Id
import Glean.Typed.Predicate
import qualified Glean.Types as Thrift

-- -----------------------------------------------------------------------------

-- | Class of monads which support creation of new typed facts
--
-- Include 'MonadFail' constraint to avoid needing 'error'
class (MonadFail m, Monad m) => NewFact m where
  -- | Create a new fact with a the given key and value
  newFact :: Predicate p => KeyType p -> ValueType p -> m (IdOf p)

-- | Create a new fact in a 'NewFact' monad and return the corresponding Thrift
-- structure which will have 'Just' the passed key and value.
makeFactV
  :: forall p m. (Predicate p, NewFact m)
  => KeyType p -> ValueType p -> m p
makeFactV key value =
  (\i -> mkFact i (Just key) (Just value)) <$> newFact key value

-- | Create a new fact in a 'NewFact' monad.
makeFactV_
  :: forall p m. (Predicate p, NewFact m)
  => KeyType p -> ValueType p -> m ()
makeFactV_ key value = void $ newFact @m @p key value

-- | Create a new key-only fact in a 'NewFact' monad and return the
-- corresponding Thrift structure which will have 'Just' the passed key.
makeFact
  :: forall p m. (Predicate p, ValueType p ~ (), NewFact m)
  => KeyType p -> m p
makeFact key = makeFactV key ()

-- | Create a new key-only fact in a 'NewFact' monad.
makeFact_
  :: forall p m. (Predicate p, ValueType p ~ (), NewFact m)
  => KeyType p -> m ()
makeFact_ key = makeFactV_ @p key ()

-- | A collection of facts which can be written to and then serialized.
data Facts = Facts
  { factsPredicates :: Predicates
  , factsData :: FactSet
  }

-- | Create a new empty collection of facts. New facts will be assigned
-- ids >= start id if a start id is supplied; otherwise, they will be
-- assigned ids which are guaranteed not to clash with any pids in the
-- 'Predicates' map.
newFacts
  :: Predicates -- ^ pid map
  -> Maybe Fid -- ^ start id
  -> IO Facts
newFacts ps start = Facts ps <$> FactSet.new (fromMaybe lowestFid start)

-- | Serialize the facts into a batch which can be sent via Thrift.
serializeFacts :: Facts -> IO Thrift.Batch
serializeFacts = FactSet.serialize . factsData

-- | Return a rough estimate of how much memory is used by the facts.
factsMemory :: Facts -> IO Int
factsMemory = FactSet.factMemory . factsData

-- | A monad for creating fact batches.
--
-- We expose 'MonadFail' to avoid needing 'error'.
--
-- NOTE: This is very specifically not an instance of MonadIO or
-- MonadTrans as we want to restrict side effects just to fact creation.
newtype FactsM a = FactsM { runFactsM :: ReaderT Facts IO a }
  deriving(Functor,Applicative,Monad,MonadFail)

instance NewFact FactsM where
  newFact key value = FactsM $ do
    facts <- ask
    lift $ withBuilder $ \builder -> do
      buildRtsValue builder key
      key_size <- sizeOfBuilder builder
      buildRtsValue builder value
      mk facts $ \pid -> IdOf <$> defineFact
        (factsData facts)
        (pidOf pid)
        builder
        key_size
      where
        mk :: Predicate p => Facts -> (PidOf p -> f (IdOf p)) -> f (IdOf p)
        mk facts f = f $ getPid $ factsPredicates facts

-- | A fact builder
type FactBuilder = forall m. NewFact m => m ()

-- | Run a fact builder to produce facts based on the supplied
-- 'Predicates' map. The facts will be assigned ids >= start id if a
-- start id is supplied; otherwise, they will be assigned ids which are
-- guaranteed not to clash with any pids in the 'Predicates' map.
buildFacts
  :: Predicates -- ^ pid map
  -> Maybe Fid -- ^ start id
  -> FactBuilder -- ^ builder
  -> IO Facts
buildFacts ps start builder = do
  facts <- newFacts ps start
  runReaderT (runFactsM builder) facts
  return facts

extendFacts :: Facts -> FactBuilder -> IO ()
extendFacts facts builder = runReaderT (runFactsM builder) facts

-- | Run a fact builder and produce a batch of facts based on
-- the supplied 'Predicates' map. The facts will be assigned
-- ids >= start id if a start id is supplied; otherwise, they will be
-- assigned ids which are guaranteed not to clash with any pids in the
-- 'Predicates' map.
buildBatch
  :: Predicates -- ^ pid map
  -> Maybe Fid -- ^ start id
  -> FactBuilder -- ^ builder
  -> IO Thrift.Batch
buildBatch ps start builder =
  serializeFacts =<< buildFacts ps start builder
