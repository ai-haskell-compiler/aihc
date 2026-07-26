{-# LANGUAGE MagicHash #-}

module Main where

import Control.Concurrent (forkIO, yield)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

data Token = Published | First | Second

puts_ :: Int -> Ptr () -> IO ()
puts_ length message = hPutBuf stdout message length

reader :: MVar Token -> MVar () -> IO ()
reader published done = do
  value <- readMVar published
  case value of
    Published -> putMVar done ()
    First -> puts_ 30 (Ptr "unexpected first reader value\n"#)
    Second -> puts_ 31 (Ptr "unexpected second reader value\n"#)

writer :: MVar Token -> MVar () -> IO ()
writer slot done = do
  putMVar slot Second
  putMVar done ()

main :: IO ()
main = do
  published <- newEmptyMVar
  firstReaderDone <- newEmptyMVar
  secondReaderDone <- newEmptyMVar
  forkIO (reader published firstReaderDone)
  forkIO (reader published secondReaderDone)
  yield

  putMVar published Published
  takeMVar firstReaderDone
  takeMVar secondReaderDone
  puts_ 38 (Ptr "both blocked readers received the put\n"#)

  stillPublished <- takeMVar published
  case stillPublished of
    Published -> puts_ 28 (Ptr "readMVar left the MVar full\n"#)
    First -> puts_ 33 (Ptr "unexpected first published value\n"#)
    Second -> puts_ 34 (Ptr "unexpected second published value\n"#)

  slot <- newMVar First
  writerDone <- newEmptyMVar
  forkIO (writer slot writerDone)
  yield

  oldValue <- takeMVar slot
  case oldValue of
    First -> puts_ 37 (Ptr "takeMVar received the original value\n"#)
    Published -> puts_ 31 (Ptr "unexpected published old value\n"#)
    Second -> puts_ 28 (Ptr "unexpected second old value\n"#)

  takeMVar writerDone
  newValue <- takeMVar slot
  case newValue of
    Second -> puts_ 41 (Ptr "blocked putMVar installed the next value\n"#)
    Published -> puts_ 31 (Ptr "unexpected published new value\n"#)
    First -> puts_ 27 (Ptr "unexpected first new value\n"#)
