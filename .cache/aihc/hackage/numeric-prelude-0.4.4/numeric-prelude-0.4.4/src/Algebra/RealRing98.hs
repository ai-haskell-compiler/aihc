module Algebra.RealRing98 where

{-# INLINE fastSplitFraction #-}
fastSplitFraction :: (RealFrac a, Integral b) =>
   (a -> Int) -> (Int -> a) -> a -> (b,a)
fastSplitFraction trunc toFloat x =
   fixSplitFraction $
   if toFloat minBound <= x && x <= toFloat maxBound
     then case trunc x of n -> (fromIntegral n, x - toFloat n)
     else case properFraction x of (n,f) -> (fromInteger n, f)

{-# INLINE fixSplitFraction #-}
fixSplitFraction :: (Num a, Num b, Ord a) => (b,a) -> (b,a)
fixSplitFraction (n,f) =
   --  if x>=0 || f==0
   if f>=0
     then (n,   f)
     else (n-1, f+1)


{-# INLINE fastFraction #-}
fastFraction :: (RealFrac a) => (a -> a) -> a -> a
fastFraction trunc x =
   fixFraction $
   if fromIntegral (minBound :: Int) <= x && x <= fromIntegral (maxBound :: Int)
     then x - trunc x
     else signedFraction x

{-# INLINE signedFraction #-}
signedFraction :: (RealFrac a) => a -> a
signedFraction x =
   let second :: (Integer, a) -> a
       second = snd
   in  second (properFraction x)

{-# INLINE fixFraction #-}
fixFraction :: (Real a) => a -> a
fixFraction y =
   if y>=0 then y else y+1
