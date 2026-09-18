{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
  ( Int (I#),
    iShiftL#,
    iShiftRA#,
    iShiftRL#,
    negateInt#,
    uncheckedIShiftL#,
    uncheckedIShiftRA#,
    uncheckedIShiftRL#,
  )

-- The three unchecked shifts of a signed integer lower to the shift
-- operations of the backend: a left shift, an arithmetic shift right that
-- replicates the sign bit, and a logical shift right that fills with zeros.
-- Data.Bits reaches none of them, so only these primitives and the GHC.Base
-- wrappers around them cover the lowering.
main :: IO ()
main = do
  print (map (\(I# value) -> I# (uncheckedIShiftL# value 4#)) [3, -3])
  print (map (\(I# value) -> I# (uncheckedIShiftRA# value 4#)) [48, -48])
  print (map (\(I# value) -> I# (uncheckedIShiftRL# value 4#)) [48, -48])
  print (I# (uncheckedIShiftL# 1# 63#))
  print (I# (uncheckedIShiftRA# (negateInt# 1#) 63#))
  print (I# (uncheckedIShiftRL# (negateInt# 1#) 60#))
  -- The wrappers guard the shift count, so a shift of the word size or more
  -- never reaches the primitive.
  print (map (\(I# count) -> I# (iShiftL# 3# count)) [4, 64])
  print (map (\(I# count) -> I# (iShiftRA# (negateInt# 48#) count)) [4, 64])
  print (map (\(I# count) -> I# (iShiftRL# 48# count)) [4, 64])
