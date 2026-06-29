{-# LANGUAGE MagicHash #-}

module GHC.Int
  ( Int (..),
  )
where

foreign import prim (+#) :: Int# -> Int# -> Int#

foreign import prim (-#) :: Int# -> Int# -> Int#

foreign import prim (*#) :: Int# -> Int# -> Int#

data Int = I# Int#
