{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Contr.Coro.Scheduler
  ( MonadScheduler (schedule, getNext),
    exhaust,
    SchedulerT (SchedulerT, runSchedulerT),
    runScheduler,
  )
where

import Contr.Cont (MonadCont (callCC))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)

class (Monad m) => MonadScheduler task m | m -> task where
  schedule :: task -> m ()
  getNext :: m (Maybe task)

newtype SchedulerT task m a = SchedulerT {runSchedulerT :: StateT [task] m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runScheduler :: (Monad m) => [task] -> SchedulerT task m a -> m a
runScheduler tasks = flip evalStateT tasks . runSchedulerT

runOne :: (MonadScheduler (m ()) m) => m Bool
runOne = do
  nxt <- getNext
  case nxt of
    Just task -> do
      task
      return True
    Nothing -> return False

instance (Monad m) => MonadScheduler task (SchedulerT task m) where
  schedule task = SchedulerT (modify (++ [task]))
  getNext = SchedulerT $ do
    tasks <- get
    case tasks of
      [] -> return Nothing
      (th : ts) -> do
        put ts
        return $ Just th

exhaust :: (MonadScheduler (m ()) m) => m ()
exhaust = do
  r <- runOne
  when r exhaust
