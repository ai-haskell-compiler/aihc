import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import GHC.Conc (atomically)
import qualified Ki
import qualified System.Random as Random
import TimerWheel
import Prelude
import Data.Foldable (traverse_)

main :: IO ()
main = do
  Ki.scoped \scope -> do
    mainThread <- Ki.fork scope main1
    atomically (Ki.await mainThread)

main1 :: IO ()
main1 = do
  putStrLn "timer wheel runs scheduled actions"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    var <- newEmptyMVar
    let n = 1000 :: Int
    replicateM_ n (register_ wheel 0 (putMVar var ()))
    replicateM_ n (takeMVar var)

  putStrLn "timers can be canceled"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    var <- newEmptyMVar
    let n = 1000 :: Int
    timers <- replicateM n (register wheel 0 (putMVar var ()))
    successes <- traverse cancel (take (n `div` 2) timers)
    replicateM_ (n - length (filter id successes)) (takeMVar var)

  putStrLn "successful `cancel` returns true, then false"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    timer <- register wheel 1 (pure ())
    cancel timer `is` True
    cancel timer `is` False

  putStrLn "unsuccessful `cancel` returns false"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    var <- newEmptyMVar
    timer <- register wheel 0 (putMVar var ())
    takeMVar var
    cancel timer `is` False

  putStrLn "recurring timers work with delay > resolution work"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    ref <- newIORef (0 :: Int)
    recurring_ wheel 0.2 (modifyIORef' ref (+ 1))
    threadDelay 1_100_000
    readIORef ref `is` (5 :: Int)

  putStrLn "calling `cancel` more than once on a recurring timer is ok"
  with Config {spokes = 4, resolution = 0.05} \wheel -> do
    timer <- recurring wheel 1 (pure ())
    cancel timer
    -- At one time, this one would loop indefinitely :grimace:
    cancel timer

  putStrLn "`with` re-throws exception from background thread"
  ( with Config {spokes = 16, resolution = 0.05} \wheel -> do
      var <- newEmptyMVar
      register_ wheel 0 (throwIO Bye >> putMVar var ())
      takeMVar var
    )
    `is` Exn Bye

  putStrLn "`count` increments on `register`"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    let n = 10 :: Int
    replicateM_ n (register wheel 1 (pure ()))
    count wheel `is` n

  putStrLn "`count` increments on `recurring`"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    let n = 10 :: Int
    replicateM_ n (recurring_ wheel 1 (pure ()))
    count wheel `is` n

  putStrLn "`count` decrements on `cancel` (registered)"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    let n = 10 :: Int
    timers <- replicateM n (register wheel 1 (pure ()))
    traverse cancel timers `is` replicate n True
    count wheel `is` (0 :: Int)

  putStrLn "`count` decrements on `cancel` (recurring)"
  with Config {spokes = 16, resolution = 0.05} \wheel -> do
    let n = 10 :: Int
    timers <- replicateM n (recurring wheel 1 (pure ()))
    traverse_ cancel timers
    count wheel `is` (0 :: Int)

  putStrLn "stress test: register 1m timers into 1k spokes, then cancel them all"
  with Config {spokes = 1000, resolution = 1} \wheel -> do
    firedRef <- newIORef (0 :: Int)
    let registerLoop :: [Timer Bool] -> Random.StdGen -> Int -> IO [Timer Bool]
        registerLoop timers gen0 !i
          | i >= 1_000_000 = pure timers
          | otherwise = do
              let (delay, gen1) = Random.uniformR (0 :: Double, 10_000) gen0
              timer <- register wheel (realToFrac @Double @Seconds delay) (modifyIORef' firedRef (+ 1))
              registerLoop (timer : timers) gen1 (i + 1)
    let cancelLoop :: Int -> [Timer Bool] -> IO Int
        cancelLoop !n = \case
          [] -> pure n
          timer : timers -> do
            success <- cancel timer
            cancelLoop (if success then n + 1 else n) timers
    timers <- registerLoop [] (Random.mkStdGen 0) 0
    canceled <- cancelLoop 0 timers
    fired <- readIORef firedRef
    (fired + canceled) `is` (1_000_000 :: Int)

data Bye = Bye
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newtype Exn a = Exn a
  deriving stock (Eq)

class Assert a b where
  is :: a -> b -> IO ()

instance (Eq e, Exception e) => Assert (IO v) (Exn e) where
  is mx y = do
    try (void mx) >>= \case
      Left ex
        | Exn ex == y -> pure ()
        | otherwise -> throwIO (userError ("expected different exception, but got: " ++ show ex))
      Right _ -> throwIO (userError "expected exception")

instance (Eq a, Show a) => Assert (IO a) a where
  is mx y = do
    x <- mx
    unless (x == y) (throwIO (userError (show x ++ " /= " ++ show y)))

instance (Eq a, Show a) => Assert a a where
  is x y =
    unless (x == y) (throwIO (userError (show x ++ " /= " ++ show y)))
