{-# LANGUAGE MagicHash #-}

module GHC.Internal.Integer
  ( Integer (..),
  )
where

-- Temporary representation: this is a boxed wrapper around machine-sized
-- Int#, not arbitrary precision. Integer needs to become a real
-- arbitrary-precision type once aihc-base has the runtime and primitive
-- support for that representation.
data Integer = IS Int#
