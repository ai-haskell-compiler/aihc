{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import OM.Fork (race, runRace, wait)

main :: IO ()
main =
  void . try @SomeException $
    runStdoutLoggingT $ do
      runRace $ do
        race "loop1" $ loop1
        race "loop2" $ loop2
        race "loopThatCrashes" $ loopThatCrashes
        wait

loop1 :: (MonadIO m) => m void
loop1 = do
  liftIO $ do
    threadDelay 1_000_000
    putStrLn "loop1 cycle"
  loop1
  
loop2 :: (MonadIO m) => m void
loop2 = do
  liftIO $ do
    threadDelay 200_000
    putStrLn "loop2 cycle"
  loop2

loopThatCrashes :: (MonadIO m) => m void
loopThatCrashes = do
  liftIO $ do
    threadDelay 5_000_000
    putStrLn "loopThatCrashes about to crash"
  error "crash"
