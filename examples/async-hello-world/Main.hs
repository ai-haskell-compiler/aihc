{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

main :: IO ()
main = hPutBuf stdout (Ptr "Hello world!\n"# :: Ptr ()) 13
