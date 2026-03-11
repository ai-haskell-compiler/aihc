-- |
-- Module:      Main
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (replicate)
import Control.Monad
import Control.Monad.ST
import Data.Kind
import Data.Proxy
import Test.Tasty hiding (after)
import Test.Tasty.QuickCheck

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector as V

import Data.Vector.Rotcev

main :: IO ()
main
  = defaultMain
  $ vecProps "Vector Int" (Proxy :: Proxy V.Vector) (Proxy :: Proxy Int)

vecProps
  :: forall (v :: Type -> Type) (a :: Type).
     (G.Vector v a, Arbitrary a, Show a, Eq a)
  => String
  -> Proxy v
  -> Proxy a
  -> TestTree
vecProps name pv pa = testGroup name
  [ newLength                  pv pa
  , replicateLength            pv pa
  , sliceLength                pv pa
  , growLength                 pv pa
  , writeRead                  pv pa
  , setRead                    pv pa
  , slicedSetRead              pv pa
  , replicateRead              pv pa
  , sliceOverlaps              pv pa
  , sliceCopy                  pv pa
  , sliceMove                  pv pa
  , writeCopyRead              pv pa
  , writeMoveRead              pv pa
  , writeGrowRead              pv pa
  , slicedWriteCopyRead        pv pa
  , slicedWriteMoveRead        pv pa
  , slicedWriteGrowRead        pv pa
  , writeInitializeAroundRead  pv pa
  , writeClearAroundRead       pv pa
  , writeSetAroundRead         pv pa
  , writeWriteAroundRead       pv pa
  , writeCopyAroundRead        pv pa
  , writeMoveAroundRead        pv pa
  , writeInitializeBetweenRead pv pa
  , writeClearBetweenRead      pv pa
  , writeSetBetweenRead        pv pa
  , writeCopyBetweenRead       pv pa
  , writeMoveBetweenRead       pv pa
  ]

-------------------------------------------------------------------------------
-- Length

newLength :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
newLength _ _ = testProperty "New-Length" $ \dir (NonNegative len) ->
  (=== len) (runST $ MG.length <$> (new dir len :: ST s (MRotcev v s a)))

replicateLength :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
replicateLength _ _ = testProperty "Replicate-Length" $ \dir (a :: a) (NonNegative len) ->
  (=== len) (runST $ MG.length <$> (replicate dir len a :: ST s (MRotcev v s a)))

sliceLength :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
sliceLength _ _ = testProperty "Slice-Length" $ \dir (NonNegative ix) (NonNegative subLen) (Positive excess) ->
  (=== subLen) (runST $ MG.length . MG.slice ix subLen <$> (new dir (ix + subLen + excess) :: ST s (MRotcev v s a)))

growLength :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
growLength _ _ = testProperty "Grow-Length" $ \dir (Positive len) (Positive by) ->
  (=== len + by) $ runST $ do
    arr <- new dir len :: ST s (MRotcev v s a)
    MG.length <$> MG.grow arr by

-------------------------------------------------------------------------------
-- Read

writeRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeRead _ _ = testProperty "Write-Read" $ \dir (a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    arr <- new dir (ix + excess) :: ST s (MRotcev v s a)
    MG.write arr ix a
    MG.read arr ix

setRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
setRead _ _ = testProperty "Set-Read" $ \dir (a :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    arr <- new dir (ix + excess) :: ST s (MRotcev v s a)
    MG.set arr a
    MG.read arr ix

slicedSetRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
slicedSetRead _ _ = testProperty "Sliced-Set-Read" $ \dir (a :: a) (NonNegative ix) (Positive excess) before after ->
  (=== a) $ runST $ do
    arr <- newSlice dir before after (ix + excess) :: ST s (MRotcev v s a)
    MG.set arr a
    MG.read arr ix

replicateRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
replicateRead _ _ = testProperty "Replicate-Read" $ \dir (a :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    arr <- replicate dir (ix + excess) a :: ST s (MRotcev v s a)
    MG.read arr ix

-------------------------------------------------------------------------------
-- Overlaps

sliceOverlaps :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
sliceOverlaps _ _ = testProperty "Slice-Overlaps" $ \dir (NonNegative i) (NonNegative ij) (NonNegative jk) (NonNegative kl) (NonNegative lm) -> do
  let j = i + ij
      k = j + jk
      l = k + kl
      m = l + lm
  property $ runST $ do
    arr <- new dir (m + 1) :: ST s (MRotcev v s a)
    let slice1 = MG.slice i (k - i + 1) arr
        slice2 = MG.slice j (l - j + 1) arr
    pure $ MG.overlaps slice1 slice2

sliceCopy :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
sliceCopy _ _ = testProperty "Slice-Copy" $ \dir (a :: a) (NonNegative i) (NonNegative ix) (Positive excess) (NonNegative ij) (NonNegative jk) -> do
  let j = i + ix + excess + ij
      k = j + ix + excess + jk
  runST $ do
    arr <- new dir k :: ST s (MRotcev v s a)
    let src = MG.slice i (ix + excess) arr
        dst = MG.slice j (ix + excess) arr
    if MG.overlaps src dst then pure (property True) else do
      MG.write src ix a
      MG.copy dst src
      valSrc <- MG.read src ix
      valDst <- MG.read dst ix
      pure (valSrc === a .&&. valDst === a)

sliceMove :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
sliceMove _ _ = testProperty "Slice-Move" $ \dir (a :: a) (NonNegative i) (NonNegative ix) (Positive excess) (NonNegative ij) (NonNegative jk) -> do
  let j = i + ix + excess + ij
      k = j + ix + excess + jk
  (=== a) $ runST $ do
    arr <- new dir k :: ST s (MRotcev v s a)
    let src = MG.slice i (ix + excess) arr
        dst = MG.slice j (ix + excess) arr
    MG.write src ix a
    MG.move dst src
    MG.read dst ix

-------------------------------------------------------------------------------
-- Write + copy/move/grow

writeCopyRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeCopyRead _ _ = testProperty "Write-Copy-Read" $ \dir1 dir2 (a :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    src <- new dir1 (ix + excess) :: ST s (MRotcev v s a)
    MG.write src ix a
    dst <- new dir2 (ix + excess)
    MG.copy dst src
    MG.clear src
    MG.read dst ix

writeMoveRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeMoveRead _ _ = testProperty "Write-Move-Read" $ \dir1 dir2 (a :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    src <- new dir1 (ix + excess) :: ST s (MRotcev v s a)
    MG.write src ix a
    dst <- new dir2 (ix + excess)
    MG.move dst src
    MG.clear src
    MG.read dst ix

writeGrowRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeGrowRead _ _ = testProperty "Write-Grow-Read" $ \dir (a :: a) (NonNegative ix) (Positive excess) (Positive by) ->
  (=== a) $ runST $ do
    src <- new dir (ix + excess) :: ST s (MRotcev v s a)
    MG.write src ix a
    dst <- MG.grow src by
    MG.clear src
    MG.read dst ix

slicedWriteCopyRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
slicedWriteCopyRead _ _ = testProperty "Sliced-Write-Copy-Read" $ \dir1 dir2 (a :: a) (NonNegative ix) (Positive excess) beforeSrc afterSrc beforeDst afterDst ->
  (=== a) $ runST $ do
    src <- newSlice dir1 beforeSrc afterSrc (ix + excess) :: ST s (MRotcev v s a)
    MG.write src ix a
    dst <- newSlice dir2 beforeDst afterDst (ix + excess)
    MG.copy dst src
    MG.clear src
    MG.read dst ix

slicedWriteMoveRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
slicedWriteMoveRead _ _ = testProperty "Sliced-Write-Move-Read" $ \dir1 dir2 (a :: a) (NonNegative ix) (Positive excess) beforeSrc afterSrc beforeDst afterDst ->
  (=== a) $ runST $ do
    src <- newSlice dir1 beforeSrc afterSrc (ix + excess) :: ST s (MRotcev v s a)
    MG.write src ix a
    dst <- newSlice dir2 beforeDst afterDst (ix + excess)
    MG.move dst src
    MG.clear src
    MG.read dst ix

slicedWriteGrowRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
slicedWriteGrowRead _ _ = testProperty "Sliced-Write-Grow-Read" $ \dir (a :: a) (NonNegative ix) (Positive excess) (Positive by) beforeSrc afterSrc ->
  (=== a) $ runST $ do
    src <- newSlice dir beforeSrc afterSrc (ix + excess) :: ST s (MRotcev v s a)
    MG.write src ix a
    dst <- MG.grow src by
    MG.clear src
    MG.read dst ix

-------------------------------------------------------------------------------
-- Write + overwrite around

writeInitializeAroundRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeInitializeAroundRead _ _ = testProperty "Write-InitializeAround-Read" $ \dir (a :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    arr <- new dir (ix + excess) :: ST s (MRotcev v s a)
    MG.write arr ix a
    when (ix > 0) $
      MG.basicInitialize (MG.slice 0 ix arr)
    when (excess > 1) $
      MG.basicInitialize (MG.slice (ix + 1) (excess - 1) arr)
    MG.read arr ix

writeClearAroundRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeClearAroundRead _ _ = testProperty "Write-ClearAround-Read" $ \dir (a :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    arr <- new dir (ix + excess) :: ST s (MRotcev v s a)
    MG.write arr ix a
    when (ix > 0) $
      MG.clear (MG.slice 0 ix arr)
    when (excess > 1) $
      MG.clear (MG.slice (ix + 1) (excess - 1) arr)
    MG.read arr ix

writeSetAroundRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeSetAroundRead _ _ = testProperty "Write-SetAround-Read" $ \dir (a :: a) (b :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    arr <- new dir (ix + excess) :: ST s (MRotcev v s a)
    MG.write arr ix a
    when (ix > 0) $
      MG.set (MG.slice 0 ix arr) b
    when (excess > 1) $
      MG.set (MG.slice (ix + 1) (excess - 1) arr) b
    MG.read arr ix

writeWriteAroundRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeWriteAroundRead _ _ = testProperty "Write-WriteAround-Read" $ \dir (a :: a) (b :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    arr <- new dir (ix + excess) :: ST s (MRotcev v s a)
    MG.write arr ix a
    when (ix > 0) $
      MG.write arr (ix - 1) b
    when (excess > 1) $
      MG.write arr (ix + 1) b
    MG.read arr ix

writeCopyAroundRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeCopyAroundRead _ _ = testProperty "Write-CopyAround-Read" $ \dir1 dir2 (a :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    src <- new dir1 (ix + excess) :: ST s (MRotcev v s a)
    dst <- new dir2 (ix + excess)
    MG.write dst ix a
    when (ix > 0) $
      MG.copy (MG.slice 0 ix dst) (MG.slice 0 ix src)
    when (excess > 1) $
      MG.copy (MG.slice (ix + 1) (excess - 1) dst) (MG.slice (ix + 1) (excess - 1) src)
    MG.read dst ix

writeMoveAroundRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeMoveAroundRead _ _ = testProperty "Write-MoveAround-Read" $ \dir1 dir2 (a :: a) (NonNegative ix) (Positive excess) ->
  (=== a) $ runST $ do
    src <- new dir1 (ix + excess) :: ST s (MRotcev v s a)
    dst <- new dir2 (ix + excess)
    MG.write dst ix a
    when (ix > 0) $
      MG.move (MG.slice 0 ix dst) (MG.slice 0 ix src)
    when (excess > 1) $
      MG.move (MG.slice (ix + 1) (excess - 1) dst) (MG.slice (ix + 1) (excess - 1) src)
    MG.read dst ix

-------------------------------------------------------------------------------
-- Two writes + overwrite in between

writeInitializeBetweenRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeInitializeBetweenRead _ _ = testProperty "Write-InitializeBetween-Read" $ \dir (a :: a) (b :: a) (NonNegative ix) (Positive dix) (Positive excess) ->
  (=== (a, b)) $ runST $ do
    arr <- new dir (ix + dix + excess) :: ST s (MRotcev v s a)
    MG.write arr ix a
    MG.write arr (ix + dix) b
    MG.basicInitialize (MG.slice (ix + 1) (dix - 1) arr)
    (,) <$> MG.read arr ix <*> MG.read arr (ix + dix)

writeClearBetweenRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeClearBetweenRead _ _ = testProperty "Write-ClearBetween-Read" $ \dir (a :: a) (b :: a) (NonNegative ix) (Positive dix) (Positive excess) ->
  (=== (a, b)) $ runST $ do
    arr <- new dir (ix + dix + excess) :: ST s (MRotcev v s a)
    MG.write arr ix a
    MG.write arr (ix + dix) b
    MG.clear (MG.slice (ix + 1) (dix - 1) arr)
    (,) <$> MG.read arr ix <*> MG.read arr (ix + dix)

writeSetBetweenRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeSetBetweenRead _ _ = testProperty "Write-SetBetween-Read" $ \dir (a :: a) (b :: a) (c :: a) (NonNegative ix) (Positive dix) (Positive excess) ->
  (=== (a, b)) $ runST $ do
    arr <- new dir (ix + dix + excess) :: ST s (MRotcev v s a)
    MG.write arr ix a
    MG.write arr (ix + dix) b
    MG.set (MG.slice (ix + 1) (dix - 1) arr) c
    (,) <$> MG.read arr ix <*> MG.read arr (ix + dix)

writeCopyBetweenRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeCopyBetweenRead _ _ = testProperty "Write-CopyBetween-Read" $ \dir1 dir2 (a :: a) (b :: a) (NonNegative ix) (Positive dix) (Positive excess) ->
  (=== (a, b)) $ runST $ do
    src <- new dir1 (ix + dix + excess) :: ST s (MRotcev v s a)
    dst <- new dir2 (ix + dix + excess)
    MG.write dst ix a
    MG.write dst (ix + dix) b
    MG.copy (MG.slice (ix + 1) (dix - 1) dst) (MG.slice (ix + 1) (dix - 1) src)
    (,) <$> MG.read dst ix <*> MG.read dst (ix + dix)

writeMoveBetweenRead :: forall (v :: Type -> Type) (a :: Type). (G.Vector v a, Arbitrary a, Show a, Eq a) => Proxy v -> Proxy a -> TestTree
writeMoveBetweenRead _ _ = testProperty "Write-MoveBetween-Read" $ \dir1 dir2 (a :: a) (b :: a) (NonNegative ix) (Positive dix) (Positive excess) ->
  (=== (a, b)) $ runST $ do
    src <- new dir1 (ix + dix + excess) :: ST s (MRotcev v s a)
    dst <- new dir2 (ix + dix + excess)
    MG.write dst ix a
    MG.write dst (ix + dix) b
    MG.move (MG.slice (ix + 1) (dix - 1) dst) (MG.slice (ix + 1) (dix - 1) src)
    (,) <$> MG.read dst ix <*> MG.read dst (ix + dix)

-------------------------------------------------------------------------------
-- Utils

newSlice :: G.Vector v a => Bool -> NonNegative Int -> NonNegative Int -> Int -> ST s (MRotcev v s a)
newSlice dir (NonNegative before) (NonNegative after) len = do
  arr <- new dir (before + len + after)
  pure $ MG.slice before len arr

new :: G.Vector v a => Bool -> Int -> ST s (MRotcev v s a)
new False n = MG.new n
new True  n = mreverse <$> MG.new n

replicate :: G.Vector v a => Bool -> Int -> a -> ST s (MRotcev v s a)
replicate False n a = MG.replicate n a
replicate True  n a = mreverse <$> MG.replicate n a
