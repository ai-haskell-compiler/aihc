{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

fibonacci :: Int -> Integer -> Integer -> Integer
fibonacci count older newer =
  case count of
    0 -> older
    _ -> fibonacci (count - 1) newer (older + newer)

main :: IO ()
main =
  if fibonacci 20000 0 1 > 0
    then hPutBuf stdout (Ptr "ok\n"# :: Ptr ()) 3
    else hPutBuf stdout (Ptr "fail\n"# :: Ptr ()) 5
