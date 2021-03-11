module Glean.Util.Future (
  Executor, sequentialExecutor, forkingExecutor,
  Future, exception, spawn, spawn_, await, awaitCatch
) where

import Util.Control.Exception (handleAll)
import Util.Log

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except

-- | An Executor allows IO actions to be asynchronously spawned from STM.
newtype Executor = Executor { execute :: IO () -> STM () }

wrap :: IO () -> IO ()
wrap = handleAll $ \exc ->
  logError $ "uncaught executor exception: " ++ show exc

worker :: TQueue (IO ()) -> IO ()
worker queue = forever $ do
  io <- atomically (readTQueue queue)
  wrap io

-- | A single-threaded Executor which executes actions one after another.
sequentialExecutor :: IO Executor
sequentialExecutor = do
  queue <- newTQueueIO
  void $ forkIO $ worker queue
  return $ Executor $ writeTQueue queue

-- | An Executor which runs each action in a separate thread.
forkingExecutor :: IO Executor
forkingExecutor = do
  queue <- newTQueueIO
  void $ forkIO $ forever $ do
    io <- atomically $ readTQueue queue
    void $ forkIO $ wrap io
  return $ Executor $ writeTQueue queue

-- | A wrapper for a value which is being produced asynchronously.
--
-- This is basically Async without an attached thread *but* with an
-- Applicative instance.
newtype Future a = Future (ExceptT SomeException STM a)
  deriving(Functor, Applicative)

-- | A future which throws when awaited.
exception :: Exception e => e -> Future a
exception = Future . throwE . toException

-- | Spawn a computation on the given Executor and return the Future for its
-- result.
spawn :: Executor -> IO a -> STM (Future a)
spawn exe io = do
  v <- newEmptyTMVar
  execute exe $ try io >>= atomically . putTMVar v
  return $ Future $ ExceptT $ readTMVar v

-- | Spawn a computation on the given Executor and ignore its result.
spawn_ :: Executor -> IO () -> STM ()
spawn_ = execute

-- | Wait for the Future to complete and get the result or rethrow any
-- exceptions.
await :: Future a -> STM a
await (Future get) = either throwSTM return =<< runExceptT get


-- | Wait for the Future to complete and get the result or the exception.
awaitCatch :: Future a -> STM (Either SomeException a)
awaitCatch (Future get) = runExceptT get
