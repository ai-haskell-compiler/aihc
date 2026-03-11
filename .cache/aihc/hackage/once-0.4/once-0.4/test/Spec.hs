import Test.Hspec
import Control.Monad
import Data.IORef
import Control.Once
import Control.Concurrent.Async
import Control.Concurrent

spec_once0_basic :: IO ()
spec_once0_basic = do
  var <- newIORef 0
  fn  <- once $ modifyIORef' var (1+)
  fn >> fn >> fn
  new <- readIORef var
  new `shouldBe` 1

spec_once0_concurrent :: IO ()
spec_once0_concurrent = do
  var <- newIORef 0
  fn  <- once $ threadDelay 50 >> modifyIORef' var (1+)
  forM_ [1..1000] $ \_ -> do
    race_ fn fn
  readIORef var >>= (`shouldBe` 1)

  forM_ [1..1000] $ \_ -> do
    concurrently_ fn fn
  readIORef var >>= (`shouldBe` 1)

spec_once1_concurrent :: IO ()
spec_once1_concurrent = do
  var <- newIORef (0 :: Int)
  fn  <- once $ \x -> threadDelay 50 >> modifyIORef' var (x +)

  -- Function 'fn' will be called with three different arguments: 0, 1, 2.
  -- In the end, 'var' should be 0 + 0 + 1 + 2 = 3.
  forM_ [1..4] $ \n -> do
    let m1 = n `mod` 3
        m2 = (n + 1) `mod` 3
    concurrently_ (fn m1) (fn m2)
    concurrently_ (fn m1) (fn m1)
  readIORef var >>= (`shouldBe` 3)

spec :: Spec
spec = do
  describe "once0" $ do
    it "basic" spec_once0_basic
    it "concurrent" spec_once0_concurrent
  describe "once1" $ do
    it "concurrent" spec_once1_concurrent

main :: IO ()
main = hspec spec
