import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (newChan, writeChan, readChan)
import Control.Concurrent.Chan.Extra (ChanExtra (debounceStatic, throttleStatic, intersperseStatic))
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChan, writeTChan, readTChan)
import Control.Concurrent.STM.TMVar (newEmptyTMVar, putTMVar, takeTMVar)


chanDebounceTest :: IO ()
chanDebounceTest = do
  putStrLn "Debounce ---------"
  outgoing <- newChan
  (incoming, _) <- debounceStatic 1000000 outgoing

  lock <- newEmptyMVar

  _ <- async $ forever $ do
    x <- readChan outgoing
    print x
    putMVar lock ()

  putStrLn "writing 1..."
  writeChan incoming 1

  _ <- takeMVar lock

  putStrLn "writing 2..."
  writeChan incoming 1
  writeChan incoming 2

  _ <- takeMVar lock

  pure ()

chanThrottleTest :: IO ()
chanThrottleTest = do
  putStrLn "Throttle ---------"
  outgoing <- newChan
  (incoming, _) <- throttleStatic 1000000 outgoing

  lock <- newChan

  _ <- async $ forever $ do
    x <- readChan outgoing
    print x
    writeChan lock ()

  putStrLn "writing 1..."
  writeChan incoming 1
  putStrLn "writing 2..."
  writeChan incoming 2
  putStrLn "writing 3..."
  writeChan incoming 3

  _ <- readChan lock
  _ <- readChan lock
  _ <- readChan lock

  pure ()

chanIntersperseTest :: IO ()
chanIntersperseTest = do
  putStrLn "Intersperse ---------"
  outgoing <- newChan
  (incoming, _, _) <- intersperseStatic 1000000 (pure 0) outgoing

  lock <- newEmptyMVar

  _ <- async $ forever $ do
    x <- readChan outgoing
    print x
    putMVar lock ()

  threadDelay 3000000

  putStrLn "Writing 1..."
  writeChan incoming 1
  _ <- takeMVar lock
  putStrLn "writing 2..."
  writeChan incoming 2
  _ <- takeMVar lock

  pure ()


chanTests :: IO ()
chanTests = do
  chanDebounceTest
  chanThrottleTest
  chanIntersperseTest

tchanDebounceTests :: IO ()
tchanDebounceTests = do
  putStrLn "Debounce STM ---------"
  outgoing <- atomically newTChan
  (incoming, _) <- debounceStatic 1000000 outgoing

  lock <- atomically newEmptyTMVar

  _ <- async $ forever $ do
    x <- atomically $ readTChan outgoing
    print x
    atomically $ putTMVar lock ()

  putStrLn "writing 1..."
  atomically $ writeTChan incoming 1

  _ <- atomically $ takeTMVar lock

  putStrLn "writing 2..."
  atomically $ writeTChan incoming 1
  atomically $ writeTChan incoming 2

  _ <- atomically $ takeTMVar lock

  pure ()

tchanThrottleTests :: IO ()
tchanThrottleTests = do
  putStrLn "Throttle STM ---------"
  outgoing <- atomically newTChan
  (incoming, _) <- throttleStatic 1000000 outgoing

  lock <- atomically newTChan

  _ <- async $ forever $ do
    x <- atomically $ readTChan outgoing
    print x
    atomically $ writeTChan lock ()

  putStrLn "writing 1..."
  atomically $ writeTChan incoming 1
  putStrLn "writing 2..."
  atomically $ writeTChan incoming 2
  putStrLn "writing 3..."
  atomically $ writeTChan incoming 3

  _ <- atomically $ readTChan lock
  _ <- atomically $ readTChan lock
  _ <- atomically $ readTChan lock

  pure ()


tchanIntersperseTests :: IO ()
tchanIntersperseTests = do
  putStrLn "Intersperse STM ---------"
  outgoing <- atomically newTChan
  (incoming, _, _) <- intersperseStatic 1000000 (pure 0) outgoing

  lock <- atomically newEmptyTMVar

  _ <- async $ forever $ do
    x <- atomically $ readTChan outgoing
    print x
    atomically $ putTMVar lock ()

  threadDelay 3000000

  putStrLn "Writing 1..."
  atomically $ writeTChan incoming 1
  _ <- atomically $ takeTMVar lock
  putStrLn "Writing 2..."
  atomically $ writeTChan incoming 2
  _ <- atomically $ takeTMVar lock

  pure ()


tchanTests :: IO ()
tchanTests = do
  tchanDebounceTests
  tchanThrottleTests
  tchanIntersperseTests


main :: IO ()
main = do
  chanTests
  threadDelay 2000000
  tchanTests
