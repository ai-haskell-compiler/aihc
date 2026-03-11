{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Excess.MinMaxExcess0
  ( MinMaxExcess0(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.FixedBitSize
import HaskellWorks.Data.Excess.Internal.Table
import HaskellWorks.Data.Excess.Triplet
import HaskellWorks.Data.Naive

import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless as BL

class MinMaxExcess0 a where
  minMaxExcess0 :: a -> Triplet

instance MinMaxExcess0 [Bool] where
  minMaxExcess0 = go 0 0 0
    where go minE maxE allE (x:xs)                    = let ne = if x then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne xs
          go minE maxE allE _                         = Triplet minE allE maxE
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 (Naive Word8) where
  minMaxExcess0 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 (Naive Word16) where
  minMaxExcess0 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 (Naive Word32) where
  minMaxExcess0 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 (Naive Word64) where
  minMaxExcess0 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 Word8 where
  minMaxExcess0 w = Triplet (DVS.unsafeIndex word8Excess0Min (fromIntegral w))
                            (DVS.unsafeIndex word8Excess0    (fromIntegral w))
                            (DVS.unsafeIndex word8Excess0Max (fromIntegral w))
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 Word16 where
  minMaxExcess0 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = minMaxExcess0 (fromIntegral  w        :: Word8)
          Triplet minExcessB allExcessB maxExcessB = minMaxExcess0 (fromIntegral (w .>. 8) :: Word8)
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 Word32 where
  minMaxExcess0 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = minMaxExcess0 (fromIntegral  w         :: Word16)
          Triplet minExcessB allExcessB maxExcessB = minMaxExcess0 (fromIntegral (w .>. 16) :: Word16)
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 Word64 where
  minMaxExcess0 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = minMaxExcess0 (fromIntegral  w         :: Word32)
          Triplet minExcessB allExcessB maxExcessB = minMaxExcess0 (fromIntegral (w .>. 32) :: Word32)
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 (DVS.Vector Word8) where
  minMaxExcess0 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word8 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = minMaxExcess0 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 (DVS.Vector Word16) where
  minMaxExcess0 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word16 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = minMaxExcess0 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 (DVS.Vector Word32) where
  minMaxExcess0 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word32 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = minMaxExcess0 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE minMaxExcess0 #-}

instance MinMaxExcess0 (DVS.Vector Word64) where
  minMaxExcess0 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word64 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = minMaxExcess0 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE minMaxExcess0 #-}
