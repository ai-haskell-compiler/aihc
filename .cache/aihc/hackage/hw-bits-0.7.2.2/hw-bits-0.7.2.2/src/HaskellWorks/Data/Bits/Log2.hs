module HaskellWorks.Data.Bits.Log2
  ( Log2(..)
  ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector.Storable as DVS

class Log2 a where
  -- | Log base of the given value rounded down to the nearest integer
  log2 :: a -> Int

log2_64_tab :: DVS.Vector Int
log2_64_tab = DVS.fromList [
    63,  0, 58,  1, 59, 47, 53,  2,
    60, 39, 48, 27, 54, 33, 42,  3,
    61, 51, 37, 40, 49, 18, 28, 20,
    55, 30, 34, 11, 43, 14, 22,  4,
    62, 57, 46, 52, 38, 26, 32, 41,
    50, 36, 17, 19, 29, 10, 13, 21,
    56, 45, 25, 31, 35, 16,  9, 12,
    44, 24, 15,  8, 23,  7,  6,  5]

log2_32_tab :: DVS.Vector Int
log2_32_tab = DVS.fromList [
     0,  9,  1, 10, 13, 21,  2, 29,
    11, 14, 16, 18, 22, 25,  3, 30,
     8, 12, 20, 28, 15, 17, 24,  7,
    19, 27, 23,  6, 26,  5,  4, 31]

instance Log2 Word64 where
  log2 v0 =
      let v1 = v0 .|. (v0 .>.  1) in
      let v2 = v1 .|. (v1 .>.  2) in
      let v3 = v2 .|. (v2 .>.  4) in
      let v4 = v3 .|. (v3 .>.  8) in
      let v5 = v4 .|. (v4 .>. 16) in
      let v6 = v5 .|. (v5 .>. 32) in
      log2_64_tab !!! fromIntegral (((v6 - (v6 .>. 1)) * 0x07EDD5E59A4E28C2) .>. 58)

instance Log2 Word32 where
  log2 v0 =
      let v1 = v0 .|. (v0 .>.  1) in
      let v2 = v1 .|. (v1 .>.  2) in
      let v3 = v2 .|. (v2 .>.  4) in
      let v4 = v3 .|. (v3 .>.  8) in
      let v5 = v4 .|. (v4 .>. 16) in
      log2_32_tab !!! fromIntegral ((v5 * 0x07C4ACDD) .>. 27)
