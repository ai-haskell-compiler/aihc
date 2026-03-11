{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent            (threadDelay)
import Control.Concurrent.STM.TQueue
import Protolude                     hiding (bracket)

import Text.LoggerThread

main :: IO ()
main = do
  args <- getArgs
  putText ("command line arguments: " <> show args)
  numCapabilities <- getNumCapabilities
  putText ("number of cores: " <> show numCapabilities)
  withLoggerThread mainThread

mainThread :: Logger -> IO ()
mainThread logger = do
  logger "logginng stuff"
  threadDelay (1000 * 1000)
  logger "logginng stuff again"
  threadDelay (1000 * 1000)
  logger "final logginng, exiting after this"
