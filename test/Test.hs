import Contr.Coro.Runtime (CoroRuntimeT, runCoroRuntime, scheduleCoro, yield)
import Contr.Coro.Scheduler (exhaust)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Foldable (forM_)
import System.Exit (exitFailure)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

maybeRunCoroWriter :: (Monoid w) => Bool -> CoroRuntimeT () (Writer w) () -> w
maybeRunCoroWriter doExhaust coro = execWriter $ runCoroRuntime $ do
  scheduleCoro coro
  when doExhaust exhaust

runCoroWriter :: (Monoid w, Eq w) => CoroRuntimeT () (Writer w) () -> w
runCoroWriter = maybeRunCoroWriter True

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "unitTests"
    [ testGroup "simple" [runOneCoro, noScheduling],
      testGroup "advanced" [severalCoroutines, binTreeTraversal]
    ]

runOneCoro :: TestTree
runOneCoro =
  testCase "runOneCoro" $
    runCoroWriter
      ( do
          scheduleCoro $ do
            lift $ tell [1]
            yield
            lift $ tell [2]
            yield
            lift $ tell [3]
            yield
      )
      @?= [1, 2, 3]

noScheduling :: TestTree
noScheduling =
  testCase "noScheduling" $
    maybeRunCoroWriter
      False
      ( lift $
          tell [1]
      )
      @?= []

severalCoroutines :: TestTree
severalCoroutines =
  testCase "severalCoroutines" $
    runCoroWriter
      ( do
          forM_ [0 .. 2] $ \i -> do
            lift $ tell [i]
            scheduleCoro $ forM_ [0 .. 2] $ \j -> do
              lift $ tell [i + 3 * j + 3]
              yield
      )
      @?= [0 .. 11]

binTreeTraversal :: TestTree
binTreeTraversal =
  testCase "binaryTreeTraversal" $
    runCoroWriter
      ( let go i =
              when (i <= 75) $ do
                lift $ tell [i]
                scheduleCoro $ go $ 2 * i + 1
                yield
                go $ 2 * i + 2
         in go 0
      )
      @?= [0 .. 75]
