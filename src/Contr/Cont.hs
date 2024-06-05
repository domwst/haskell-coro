module Contr.Cont
  ( ContT (runContT, ContT),
    MonadCont (callCC),
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor.Identity (Identity)

newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r}

type Cont r a = ContT r Identity a

class MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance Functor (ContT r m) where
  fmap f (ContT c) = ContT $ \v -> c (v . f)

instance Applicative (ContT r m) where
  pure = ContT . flip ($)
  (<*>) (ContT c1) (ContT c2) = ContT $ \c -> c1 $ \ab -> c2 $ \a -> c $ ab a

instance Monad (ContT r m) where
  (>>=) (ContT c) f = ContT $ \br -> c $ \a -> runContT (f a) br

cont :: ((a -> m r) -> m r) -> ContT r m a
cont = ContT

instance MonadCont (ContT r m) where
  callCC run = ContT $ \cc -> runContT (run $ \a -> ContT $ const $ cc a) cc

instance MonadTrans (ContT r) where
  lift = ContT . (>>=)

instance (MonadIO m) => MonadIO (ContT r m) where
  liftIO = lift . liftIO
