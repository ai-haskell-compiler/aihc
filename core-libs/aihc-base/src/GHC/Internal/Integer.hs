{-# LANGUAGE MagicHash #-}

module GHC.Internal.Integer
  ( Integer (..),
    compareInteger#,
    eqInteger#,
  )
where

foreign import prim (==#) :: Int# -> Int# -> Int#

foreign import prim compareInt# :: Int# -> Int# -> Int#

-- Temporary representation: this is a boxed wrapper around machine-sized
-- Int#, not arbitrary precision. Integer needs to become a real
-- arbitrary-precision type once aihc-base has the runtime and primitive
-- support for that representation.
data Integer = IS Int#

compareInteger# :: Integer -> Integer -> Int#
compareInteger# (IS x) (IS y) = compareInt# x y

eqInteger# :: Integer -> Integer -> Int#
eqInteger# (IS x) (IS y) = (==#) x y
