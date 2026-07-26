{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

factorial :: Int -> Integer -> Integer -> Integer
factorial count factor accumulator =
  case count of
    0 -> accumulator
    _ -> factorial (count - 1) (factor + 1) (accumulator * factor)

main :: IO ()
main =
  if factorial 1500 1 1 > 0
    then hPutBuf stdout (Ptr "ok\n"# :: Ptr ()) 3
    else hPutBuf stdout (Ptr "fail\n"# :: Ptr ()) 5
