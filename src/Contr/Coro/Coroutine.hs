{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Contr.Coro.Coroutine where

import Contr.Cont (ContT (runContT), MonadCont (callCC))
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, evalStateT, get, put)

class (MonadCont m) => MonadCoroutineContext m where
  resume :: m () -> m ()
  await :: (m b -> m ()) -> m ()

newtype CoroutineT r m a = CoroutineT {runCoroutineT :: ContT r (StateT (CoroutineT r m ()) m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

runCoroutine :: (Monad m) => CoroutineT r m r -> m r
runCoroutine coro = evalStateT (runContT (runCoroutineT coro) return) (return ())

getUp :: (Monad m) => CoroutineT r m (CoroutineT r m ())
getUp = CoroutineT . lift $ get

putUp :: (Monad m) => CoroutineT r m () -> CoroutineT r m ()
putUp newUp = CoroutineT . lift $ put newUp

instance (Monad m) => MonadCoroutineContext (CoroutineT r m) where
  resume coro = do
    callCC $ \cc -> do
      prevUp <- getUp
      putUp $ putUp prevUp >> cc ()
      coro

  await cc = callCC $ \c -> do
    cc $ c ()
    join getUp

instance MonadTrans (CoroutineT r) where
  lift = CoroutineT . lift . lift
