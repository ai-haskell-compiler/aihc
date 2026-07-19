{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Control.Concurrent (forkIO, yield)
import Foreign.C.Types (CInt)

foreign import ccall unsafe puts :: Addr# -> IO CInt

main :: IO ()
main =
  puts "Hello world main green thread"#
    >> forkIO
      (puts "Hello from forked thread"#)
    >> puts "Still in main"#
    >> yield
    >> puts "Back in main"#
    >> return ()
