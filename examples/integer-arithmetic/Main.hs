{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

power :: Integer -> Int -> Integer
power value count =
  case count of
    0 -> 1
    _ -> value * power value (count - 1)

integerLow :: Integer -> Int
integerLow = fromInteger

main :: IO ()
main =
  case power 2 80 of
    large ->
      case large + 12345 of
        left ->
          case large - 6789 of
            right ->
              if left + right == large * 2 + 5556
                && large == 1208925819614629174706176
                && 12345 + large == left
                && left - left == 0
                && left * right == right * left
                && integerLow (left * right) == negate 83810205
                && negate left < 0
                && abs (negate left) == left
                then hPutBuf stdout (Ptr "ok\n"# :: Ptr ()) 3
                else hPutBuf stdout (Ptr "fail\n"# :: Ptr ()) 5
