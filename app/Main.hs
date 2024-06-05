module Main where

import Contr.Coro.Runtime (runCoroRuntime, scheduleCoro, yield)
import Contr.Coro.Scheduler (exhaust)
import Control.Monad.IO.Class (MonadIO (liftIO))

main :: IO ()
main = runCoroRuntime $ do
  scheduleCoro $ do
    scheduleCoro $ do
      liftIO $ print "Spawning first child"
      scheduleCoro $ do
        liftIO $ print (1 :: Int)
        yield
        liftIO $ print (4 :: Int)
      liftIO $ print "Spawning second child"
      scheduleCoro $ do
        scheduleCoro $ do
          liftIO $ print (5 :: Int)
          yield
          liftIO $ print (8 :: Int)
        liftIO $ print (2 :: Int)
        yield
        liftIO $ print (6 :: Int)
      scheduleCoro $ do
        liftIO $ print (3 :: Int)
        yield
        liftIO $ print (7 :: Int)
      liftIO $ print "Dying"

  exhaust
