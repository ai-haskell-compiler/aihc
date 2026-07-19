{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Control.Concurrent (forkIO, yield)
import Foreign.C.Types (CInt)

foreign import ccall unsafe puts :: Addr# -> IO CInt

puts_ :: Addr# -> IO ()
puts_ message = do
  puts message
  return ()

main :: IO ()
main = do
  puts_ "Hello world main green thread"#
  forkIO (puts_ "Hello from forked thread"#)
  puts_ "Still in main"#
  yield
  puts_ "Back in main"#
