{-# LANGUAGE MagicHash #-}

module GHC.Integer
  ( Integer,
  )
where

-- Temporary representation: this is machine-sized Int#, not arbitrary
-- precision. Integer needs to become a real arbitrary-precision type once
-- aihc-base has the runtime and primitive support for that representation.
type Integer = Int#
