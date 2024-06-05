{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Contr.Coro.Runtime
  ( CoroRuntimeT,
    yield,
    scheduleCoro,
    runCoroRuntime,
  )
where

import Contr.Cont (MonadCont)
import Contr.Coro.Coroutine (CoroutineT, MonadCoroutineContext (await, resume), runCoroutine)
import Contr.Coro.Scheduler (MonadScheduler (getNext, schedule), SchedulerT, runScheduler)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype CoroRuntimeT r m a = CoroRuntimeT
  { runCoroRuntimeT :: CoroutineT r (SchedulerT (CoroRuntimeT r m ()) m) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCont, MonadCoroutineContext)

instance MonadTrans (CoroRuntimeT r) where
  lift = CoroRuntimeT . lift . lift

instance (Monad m) => MonadScheduler (CoroRuntimeT r m ()) (CoroRuntimeT r m) where
  schedule = CoroRuntimeT . lift . schedule
  getNext = CoroRuntimeT . lift $ getNext

scheduleCoro :: (MonadScheduler (m ()) m, MonadCoroutineContext m) => m () -> m ()
scheduleCoro = schedule . resume

yield :: (MonadScheduler (m ()) m, MonadCoroutineContext m) => m ()
yield = await $ \cc ->
  schedule $ resume cc

runCoroRuntime :: (Monad m) => CoroRuntimeT r m r -> m r
runCoroRuntime rt = runScheduler [] $ runCoroutine $ runCoroRuntimeT rt
