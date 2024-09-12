{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module HieDBIndexer.Trace (
  Tracer (..),
  MonadTrace (..),
  MonadMaskInstance (..),
  logMsg,
  traceMsg,
  Contravariant,
  (>$<),
  vlogTextTracer,
  vlogTracerWithPriority,
) where

import Control.Exception (
  Exception,
 )
import Control.Monad.Catch (
  ExitCase (..),
  MonadCatch,
  MonadMask (generalBracket),
  MonadThrow,
  try,
 )
import Control.Monad.IO.Class (
  MonadIO (..),
 )
import Data.Functor.Contravariant (
  Contravariant (contramap),
  (>$<),
 )
import Data.Text (Text)
import GHC.Stack (
  HasCallStack,
  withFrozenCallStack,
 )
import TextShow (showt)
import qualified Util.Log.String as String
import Util.Log.Text

-- | A contravariant tracing abstraction
data Tracer msg = Tracer
  { -- | Log a message
    logMsg_ :: forall m. (HasCallStack, MonadTrace m) => msg -> m ()
  , -- | Trace the begin and end of a computation
    traceMsg_ :: forall a m. (HasCallStack, MonadTrace m) => msg -> m a -> m a
  }

-- Explicit record accessors to preserve call stacks

logMsg :: (HasCallStack, MonadTrace m) => Tracer msg -> msg -> m ()
logMsg logger msg = withFrozenCallStack $ logMsg_ logger msg

traceMsg :: (HasCallStack, MonadTrace m) => Tracer msg -> msg -> m a -> m a
traceMsg logger msg act = withFrozenCallStack $ traceMsg_ logger msg act

instance Contravariant Tracer where
  contramap f (Tracer logf traceF) = Tracer (logf . f) (traceF . f)

instance Monoid (Tracer msg) where
  mempty = Tracer (\_ -> pure ()) (const id)

instance Semigroup (Tracer msg) where
  l1 <> l2 =
    Tracer
      { logMsg_ = \m -> logMsg_ l1 m *> logMsg_ l2 m
      , traceMsg_ = \msg -> traceMsg_ l1 msg . traceMsg_ l2 msg
      }

-------------------------------------------------------------------------------
-- Exceptions

class MonadIO m => MonadTrace m where
  tryM :: Exception e => m a -> m (Either e a)
  bracketM :: IO a -> (a -> ExitCase b -> IO ()) -> (a -> m b) -> m b

deriving via (MonadMaskInstance IO) instance MonadTrace IO

-- | Deriving 'MonadTrace' via 'MonadMask'
newtype MonadMaskInstance m a = MonadMaskInstance (m a)
  deriving
    (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow)

instance (MonadIO m, MonadMask m) => MonadTrace (MonadMaskInstance m) where
  tryM = try
  bracketM acquire release =
    fmap fst . generalBracket (liftIO acquire) ((liftIO .) . release)

vlogTextTracer :: Int -> Tracer Text
vlogTextTracer p = T p >$< vlogTracerWithPriority

data TraceWithPriority
  = Skip
  | T !Int !Text
  | S !Int !String

vlogTracerWithPriority :: Tracer TraceWithPriority
vlogTracerWithPriority = Tracer {..}
  where
    logMsg_ :: (HasCallStack, MonadIO m) => TraceWithPriority -> m ()
    logMsg_ Skip = pure ()
    logMsg_ x = withFrozenCallStack $ case x of
      T p t -> vlog p t
      S p s -> String.vlog p s
      Skip -> error "unreachable"

    traceMsg_ :: (HasCallStack, MonadTrace m) => TraceWithPriority -> m b -> m b
    traceMsg_ Skip act = act
    traceMsg_ msg act = withFrozenCallStack $ do
      case msg of
        T p t ->
          bracketM
            (vlog p ("BEGIN " <> t))
            ( \() res -> case res of
                ExitCaseSuccess {} -> vlog p ("END " <> t)
                ExitCaseAbort {} -> vlog p ("ABORTED " <> t)
                ExitCaseException e ->
                  vlog p ("FAILED " <> t <> ": " <> showt e)
            )
            (\() -> act)
        S p t ->
          bracketM
            (String.vlog p ("BEGIN " <> t))
            ( \() res -> case res of
                ExitCaseSuccess {} -> String.vlog p ("END " <> t)
                ExitCaseAbort {} -> String.vlog p ("ABORTED " <> t)
                ExitCaseException e ->
                  String.vlog p ("FAILED " <> t <> ": " <> show e)
            )
            (\() -> act)
        Skip -> error "unreachable"
