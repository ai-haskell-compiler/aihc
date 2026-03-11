
module Data.Paired.Vector
  ( module Data.Paired.Vector
  , module Data.Paired.Common
  ) where

import Data.Vector.Generic as VG

import Data.Paired.Common



-- | Upper triangular elements.

upperTriVG
  :: (Vector v a, Vector w (a,a))
  => OnDiag
  -> v a
  -> (Int, w (a,a))
upperTriVG d as = (z, unfoldrN z go (0,if d == OnDiag then 0 else 1))
  where la = VG.length as
        z  = la * (la + if d == OnDiag then 1 else 0) `div` 2
        go (k,l)
          | k >= la   = Nothing
          | l >= la   = go (k+1,k+1 + if d == OnDiag then 0 else 1)
          | otherwise = Just ((as `VG.unsafeIndex` k, as `VG.unsafeIndex` l), (k,l+1))
{-# Inline upperTriVG #-}

-- | Outer pairing of all @as@ with all @bs@. This one is quasi-trivial,
-- but here for completeness.

rectangularVG
  :: (Vector va a, Vector vb b, Vector w (a,b))
  => va a
  -> vb b
  -> (Int, w (a,b))
rectangularVG as bs = (z, unfoldrN z go (0,0))
  where la = VG.length as
        lb = VG.length bs
        z  = la * lb
        go (k,l)
          | k >= la   = Nothing
          | l >= lb   = go (k+1,0)
          | otherwise = Just ((as `VG.unsafeIndex` k, bs `VG.unsafeIndex` l), (k,l+1))
{-# Inline rectangularVG #-}

