{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Excess.MinExcess0
  ( MinExcess0(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.FixedBitSize
import HaskellWorks.Data.Excess.Internal.Table
import HaskellWorks.Data.Excess.MinExcess
import HaskellWorks.Data.Naive

import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless as BL

class MinExcess0 a where
  minExcess0 :: a -> MinExcess

instance MinExcess0 [Bool] where
  minExcess0 = go 0 0
    where go minE allE (x:xs)                    = let ne = if x then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) ne xs
          go minE allE _                         = MinExcess minE allE
  {-# INLINE minExcess0 #-}

instance MinExcess0 (Naive Word8) where
  minExcess0 = go 0 0 0 . naive
    where go minE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) ne (n + 1) w
          go minE allE _ _                       = MinExcess minE allE
  {-# INLINE minExcess0 #-}

instance MinExcess0 (Naive Word16) where
  minExcess0 = go 0 0 0 . naive
    where go minE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) ne (n + 1) w
          go minE allE _ _                       = MinExcess minE allE
  {-# INLINE minExcess0 #-}

instance MinExcess0 (Naive Word32) where
  minExcess0 = go 0 0 0 . naive
    where go minE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) ne (n + 1) w
          go minE allE _ _                       = MinExcess minE allE
  {-# INLINE minExcess0 #-}

instance MinExcess0 (Naive Word64) where
  minExcess0 = go 0 0 0 . naive
    where go minE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) ne (n + 1) w
          go minE allE _ _                       = MinExcess minE allE
  {-# INLINE minExcess0 #-}

instance MinExcess0 Word8 where
  minExcess0 w = MinExcess (DVS.unsafeIndex word8Excess0Min (fromIntegral w))
                            (DVS.unsafeIndex word8Excess0    (fromIntegral w))
  {-# INLINE minExcess0 #-}

instance MinExcess0 Word16 where
  minExcess0 w = MinExcess (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
    where MinExcess minExcessA allExcessA = minExcess0 (fromIntegral  w        :: Word8)
          MinExcess minExcessB allExcessB = minExcess0 (fromIntegral (w .>. 8) :: Word8)
  {-# INLINE minExcess0 #-}

instance MinExcess0 Word32 where
  minExcess0 w = MinExcess (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
    where MinExcess minExcessA allExcessA = minExcess0 (fromIntegral  w         :: Word16)
          MinExcess minExcessB allExcessB = minExcess0 (fromIntegral (w .>. 16) :: Word16)
  {-# INLINE minExcess0 #-}

instance MinExcess0 Word64 where
  minExcess0 w = MinExcess (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
    where MinExcess minExcessA allExcessA = minExcess0 (fromIntegral  w         :: Word32)
          MinExcess minExcessB allExcessB = minExcess0 (fromIntegral (w .>. 32) :: Word32)
  {-# INLINE minExcess0 #-}

instance MinExcess0 (DVS.Vector Word8) where
  minExcess0 = DVS.foldl' gen (MinExcess 0 0)
    where gen :: MinExcess -> Word8 -> MinExcess
          gen (MinExcess minE allE) w  = let MinExcess wMinE wAllE = minExcess0 w  in
                                            MinExcess (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
  {-# INLINE minExcess0 #-}

instance MinExcess0 (DVS.Vector Word16) where
  minExcess0 = DVS.foldl' gen (MinExcess 0 0)
    where gen :: MinExcess -> Word16 -> MinExcess
          gen (MinExcess minE allE) w  = let MinExcess wMinE wAllE = minExcess0 w  in
                                            MinExcess (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
  {-# INLINE minExcess0 #-}

instance MinExcess0 (DVS.Vector Word32) where
  minExcess0 = DVS.foldl' gen (MinExcess 0 0)
    where gen :: MinExcess -> Word32 -> MinExcess
          gen (MinExcess minE allE) w  = let MinExcess wMinE wAllE = minExcess0 w  in
                                            MinExcess (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
  {-# INLINE minExcess0 #-}

instance MinExcess0 (DVS.Vector Word64) where
  minExcess0 = DVS.foldl' gen (MinExcess 0 0)
    where gen :: MinExcess -> Word64 -> MinExcess
          gen (MinExcess minE allE) w  = let MinExcess wMinE wAllE = minExcess0 w  in
                                            MinExcess (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
  {-# INLINE minExcess0 #-}
