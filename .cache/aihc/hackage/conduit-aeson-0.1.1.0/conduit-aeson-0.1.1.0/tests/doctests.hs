{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ >= 802
import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-XHaskell2010", "-XCPP", "src/Data/Conduit/Aeson.hs"]
#else

import System.IO

main :: IO ()
main = hPutStrLn stderr "GHC version older than 8.2 are not supported"

#endif
