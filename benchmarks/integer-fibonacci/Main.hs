{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Foreign.C.Types (CInt (..))
import GHC.Exts (Addr#)

foreign import ccall unsafe puts :: Addr# -> IO CInt

fibonacci :: Int -> Integer -> Integer -> Integer
fibonacci count older newer =
  case count of
    0 -> older
    _ -> fibonacci (count - 1) newer (older + newer)

main :: IO CInt
main =
  if fibonacci 20000 0 1 > 0
    then puts "ok"#
    else puts "fail"#
