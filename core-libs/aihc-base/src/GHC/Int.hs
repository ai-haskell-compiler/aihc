{-# LANGUAGE MagicHash #-}

module GHC.Int
  ( Int (..),
    Int32 (..),
  )
where

foreign import prim (+#) :: Int# -> Int# -> Int#

foreign import prim (-#) :: Int# -> Int# -> Int#

foreign import prim (*#) :: Int# -> Int# -> Int#

data Int = I# Int#

data Int32 = I32# Int32#
