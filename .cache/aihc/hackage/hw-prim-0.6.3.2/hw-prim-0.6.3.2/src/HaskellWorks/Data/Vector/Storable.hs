{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Vector.Storable
  ( padded
  , foldMap
  , mapAccumL
  , mmap
  , constructSI
  , construct2N
  , construct64UnzipN
  , unzipFromListN2
  ) where

import Control.Monad.ST                   (ST, runST)
import Data.Vector.Storable               (Storable)
import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.Vector.AsVector8
import Prelude                            hiding (abs, foldMap)

import qualified Data.ByteString              as BS
import qualified Data.Vector.Generic          as DVG
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified System.IO.MMap               as IO

{- HLINT ignore "Redundant do" -}

padded :: Int -> DVS.Vector Word8 -> DVS.Vector Word8
padded n v = v <> DVS.replicate ((n - DVS.length v) `max` 0) 0
{-# INLINE padded #-}

foldMap :: (DVS.Storable a, Monoid m) => (a -> m) -> DVS.Vector a -> m
foldMap f = DVS.foldl' (\a b -> a <> f b) mempty
{-# INLINE foldMap #-}

mapAccumL :: forall a b c. (Storable b, Storable c)
  => (a -> b -> (a, c))
  -> a
  -> DVS.Vector b
  -> (a, DVS.Vector c)
mapAccumL f a vb = DVS.createT $ do
  vc <- DVSM.unsafeNew (DVS.length vb)
  a' <- go 0 a vc
  return (a', vc)
  where go :: Int -> a -> DVS.MVector s c -> ST s a
        go i a0 vc = if i < DVS.length vb
          then do
            let (a1, c1) = f a0 (DVS.unsafeIndex vb i)
            DVSM.unsafeWrite vc i c1
            go (i + 1) a1 vc
          else return a0
{-# INLINE mapAccumL #-}

-- | MMap the file as a storable vector.  If the size of the file is not a multiple of the element size
-- in bytes, then the last few bytes of the file will not be included in the vector.
mmap :: Storable a => FilePath -> IO (DVS.Vector a)
mmap filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filepath IO.ReadOnly Nothing
  let !v = DVS.unsafeFromForeignPtr fptr offset size
  return (DVS.unsafeCast v)

-- | Construct a vector statefully with index
constructSI :: forall a s. Storable a => Int -> (Int -> s -> (s, a)) -> s -> (s, DVS.Vector a)
constructSI n f state = DVS.createT $ do
  mv <- DVSM.unsafeNew n
  state' <- go 0 state mv
  return (state', mv)
  where go :: Int -> s -> DVSM.MVector t a -> ST t s
        go i s mv = if i < DVSM.length mv
          then do
            let (s', a) = f i s
            DVSM.unsafeWrite mv i a
            go (i + 1) s' mv
          else return s

construct2N :: (Storable b, Storable c)
  => Int
  -> (forall s. a -> DVSM.MVector s b -> ST s Int)
  -> Int
  -> (forall s. a -> DVSM.MVector s c -> ST s Int)
  -> [a]
  -> (DVS.Vector b, DVS.Vector c)
construct2N nb fb nc fc as = runST $ do
  mbs <- DVSM.unsafeNew nb
  mcs <- DVSM.unsafeNew nc
  (mbs2, mcs2) <- go fb 0 mbs fc 0 mcs as
  bs <- DVG.unsafeFreeze mbs2
  cs <- DVG.unsafeFreeze mcs2
  return (bs, cs)
  where go :: (Storable b, Storable c)
          => (forall t. a -> DVSM.MVector t b -> ST t Int)
          -> Int
          -> DVSM.MVector s b
          -> (forall t. a -> DVSM.MVector t c -> ST t Int)
          -> Int
          -> DVSM.MVector s c
          -> [a]
          -> ST s (DVSM.MVector s b, DVSM.MVector s c)
        go   _ bn mbs   _ cn mcs []     = return (DVSM.take bn mbs, DVSM.take cn mcs)
        go fb' bn mbs fc' cn mcs (d:ds) = do
          bi <- fb' d (DVSM.drop bn mbs)
          ci <- fc' d (DVSM.drop cn mcs)
          go fb' (bn + bi) mbs fc' (cn + ci) mcs ds

construct64UnzipN :: Int -> [(BS.ByteString, BS.ByteString)] -> (DVS.Vector Word64, DVS.Vector Word64)
construct64UnzipN nBytes xs = (DVS.unsafeCast ibv, DVS.unsafeCast bpv)
  where [ibv, bpv] = DVS.createT $ do
          let nW8s     = (nBytes + 7) `div` 8 * 8
          ibmv <- DVSM.new nW8s
          bpmv <- DVSM.new nW8s
          (ibmvRemaining, bpmvRemaining) <- go ibmv bpmv xs
          let ibl = ((DVSM.length ibmv - ibmvRemaining + 7) `div` 8) * 8
          let bpl = ((DVSM.length bpmv - bpmvRemaining + 7) `div` 8) * 8
          return [DVSM.take ibl ibmv, DVSM.take bpl bpmv]
        go :: DVSM.MVector s Word8 -> DVSM.MVector s Word8 -> [(BS.ByteString, BS.ByteString)] -> ST s (Int, Int)
        go ibmv bpmv ((ib, bp):ys) = do
          DVS.copy (DVSM.take (BS.length ib) ibmv) (asVector8 ib)
          DVS.copy (DVSM.take (BS.length bp) bpmv) (asVector8 bp)
          go (DVSM.drop (BS.length ib) ibmv) (DVSM.drop (BS.length bp) bpmv) ys
        go ibmv bpmv [] = do
          let ibl = DVSM.length ibmv `mod` 8
          let bpl = DVSM.length bpmv `mod` 8
          DVSM.set (DVSM.take ibl ibmv) 0
          DVSM.set (DVSM.take bpl bpmv) 0
          return (DVSM.length (DVSM.drop ibl ibmv), DVSM.length (DVSM.drop bpl bpmv))

unzipFromListN2 :: (Storable a, Storable b) => Int -> [(a, b)] -> (DVS.Vector a, DVS.Vector b)
unzipFromListN2 n abs = runST $ do
  mas <- DVSM.unsafeNew n
  mbs <- DVSM.unsafeNew n
  len <- go 0 mas mbs abs
  as <- DVG.unsafeFreeze (DVSM.take len mas)
  bs <- DVG.unsafeFreeze (DVSM.take len mbs)
  return (as, bs)
  where go :: (Storable c, Storable d)
          => Int
          -> DVSM.MVector s c
          -> DVSM.MVector s d
          -> [(c, d)]
          -> ST s Int
        go i _   _   []           = return i
        go i mvc mvd ((c, d):cds) = if i < n
          then do
            DVSM.write mvc i c
            DVSM.write mvd i d
            go (i + 1) mvc mvd cds
          else return i
