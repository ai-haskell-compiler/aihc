{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

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
import Foreign.C.Types (CInt (..))
import GHC.Exts (Ptr (..))

foreign import ccall unsafe puts :: Ptr () -> IO CInt

data Token = Published | First | Second

puts_ :: Ptr () -> IO ()
puts_ message = do
  puts message
  return ()

reader :: MVar Token -> MVar () -> IO ()
reader published done = do
  value <- readMVar published
  case value of
    Published -> putMVar done ()
    First -> puts_ (Ptr "unexpected first reader value"#)
    Second -> puts_ (Ptr "unexpected second reader value"#)

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
  puts_ (Ptr "both blocked readers received the put"#)

  stillPublished <- takeMVar published
  case stillPublished of
    Published -> puts_ (Ptr "readMVar left the MVar full"#)
    First -> puts_ (Ptr "unexpected first published value"#)
    Second -> puts_ (Ptr "unexpected second published value"#)

  slot <- newMVar First
  writerDone <- newEmptyMVar
  forkIO (writer slot writerDone)
  yield

  oldValue <- takeMVar slot
  case oldValue of
    First -> puts_ (Ptr "takeMVar received the original value"#)
    Published -> puts_ (Ptr "unexpected published old value"#)
    Second -> puts_ (Ptr "unexpected second old value"#)

  takeMVar writerDone
  newValue <- takeMVar slot
  case newValue of
    Second -> puts_ (Ptr "blocked putMVar installed the next value"#)
    Published -> puts_ (Ptr "unexpected published new value"#)
    First -> puts_ (Ptr "unexpected first new value"#)
