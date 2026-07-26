{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

main :: IO ()
main = hPutBuf stdout (Ptr "WASI System.IO\n"# :: Ptr ()) 15
