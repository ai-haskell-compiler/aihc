{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 610
{-# OPTIONS_GHC -frewrite-rules #-}
#else
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
#endif
#endif
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.CArray.Base
-- Copyright   : (c) 2001 The University of Glasgow
--               (c) 2008 Jed Brown
-- License     : BSD-style
-- 
-- Maintainer  : jed@59A2.org
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides both the immutable 'CArray' and mutable 'IOCArray'.  The
-- underlying storage is exactly the same - pinned memory on the GC'd heap.
-- Elements are stored according to the class 'Storable'.  You can obtain a
-- pointer to the array contents to manipulate elements from languages like C.
--
-- 'CArray' is 16-byte aligned by default.  If you create a 'CArray' with
-- 'unsafeForeignPtrToCArray' then it may not be aligned.  This will be an issue
-- if you intend to use SIMD instructions.
--
-- 'CArray' is similar to 'Data.Array.Unboxed.UArray' but slower if you stay
-- within Haskell.  'CArray' can handle more types and can be used by external
-- libraries.
--
-- 'IOCArray' is equivalent to 'Data.Array.Storable.StorableArray' and similar
-- to 'Data.Array.IO.IOUArray' but slower.  'IOCArray' has O(1) versions of
-- 'unsafeFreeze' and 'unsafeThaw' when converting to/from 'CArray'.
-----------------------------------------------------------------------------

module Data.Array.CArray.Base where

import Data.Array.Base
            (bounds, elems, listArray, unsafeAt, assocs, (!),
             numElements, getNumElements, showsIArray,
             unsafeFreeze, unsafeThaw,
             unsafeRead, unsafeWrite,
             unsafeArray, unsafeReplace, unsafeAccum,
             unsafeAccumArray, unsafeNewArray_)
import Data.Array.MArray
            (MArray(getBounds, newArray, newArray_), freeze, thaw)
import Data.Array.IArray (IArray)
import Data.Ix.Shapable (Shapable, shape, sShape, shapeToStride, size)
import Data.Ix (Ix, rangeSize, range, inRange, index)

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, arbitrary, CoArbitrary, coarbitrary,)
import qualified Data.ByteString.Internal as S
import Data.Binary (Binary, get, put)

import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import Foreign.Storable
            (Storable, sizeOf, alignment, peek, peekElemOff, pokeElemOff)
import Foreign.ForeignPtr
            (ForeignPtr, withForeignPtr, castForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (copyArray, withArray)
import Foreign.Ptr (plusPtr, alignPtr, castPtr)

import Data.Word                (Word8, Word)
import Data.Generics            (Data(..), Typeable)
import GHC.Ptr                  (Ptr(..))
import GHC.ForeignPtr           (ForeignPtr(..), mallocPlainForeignPtrBytes)

import Data.Complex (Complex, magnitude)
import Data.List (zipWith4, foldl')
import Control.Monad (replicateM, forM_, (>=>))
import Control.Applicative (liftA2)


-- | The immutable array type.
data CArray i e = CArray !i !i Int !(ForeignPtr e)
    deriving (Data, Typeable)

-- | Absolutely equivalent representation, but used for the mutable interface.
data IOCArray i e = IOCArray !i !i Int !(ForeignPtr e)
    deriving (Data, Typeable)

instance Storable e => MArray IOCArray e IO where
    getBounds (IOCArray l u _ _) = return (l,u)

    getNumElements (IOCArray _ _ n _) = return n

    newArray (l,u) e0 = do
        fp <- mallocForeignPtrArrayAligned n
        withForeignPtr fp $ \a ->
            forM_ [0 .. n - 1] $ \i -> pokeElemOff a i e0
        return (IOCArray l u n fp)
        where n = rangeSize (l,u)

    unsafeNewArray_ (l,u) = do
        let n = rangeSize (l,u)
        fp <- mallocForeignPtrArrayAligned n
        return (IOCArray l u n fp)

    newArray_ = unsafeNewArray_

    unsafeRead (IOCArray _ _ _ fp) i =
        withForeignPtr fp $ \a -> peekElemOff a i

    unsafeWrite (IOCArray _ _ _ fp) i e =
        withForeignPtr fp $ \a -> pokeElemOff a i e

-- | The pointer to the array contents is obtained by 'withCArray'.
-- The idea is similar to 'ForeignPtr' (used internally here).
-- The pointer should be used only during execution of the 'IO' action
-- retured by the function passed as argument to 'withCArray'.
withCArray :: CArray i e -> (Ptr e -> IO a) -> IO a
withCArray (CArray _ _ _ fp) f = withForeignPtr fp f

withIOCArray :: IOCArray i e -> (Ptr e -> IO a) -> IO a
withIOCArray (IOCArray _ _ _ fp) f = withForeignPtr fp f

-- | If you want to use it afterwards, ensure that you
-- 'touchCArray' after the last use of the pointer,
-- so the array is not freed too early.
touchIOCArray :: IOCArray i e -> IO ()
touchIOCArray (IOCArray _ _ _ fp) = touchForeignPtr fp

-- | /O(1)/ Construct a 'CArray' from an arbitrary 'ForeignPtr'.  It is
-- the caller's responsibility to ensure that the 'ForeignPtr' points to
-- an area of memory sufficient for the specified bounds.
unsafeForeignPtrToCArray
   :: Ix i => ForeignPtr e -> (i,i) -> IO (CArray i e)
unsafeForeignPtrToCArray p (l,u) =
   return (CArray l u (rangeSize (l,u)) p)

-- | /O(1)/ Construct a 'CArray' from an arbitrary 'ForeignPtr'.  It is
-- the caller's responsibility to ensure that the 'ForeignPtr' points to
-- an area of memory sufficient for the specified bounds.
unsafeForeignPtrToIOCArray
   :: Ix i => ForeignPtr e -> (i,i) -> IO (IOCArray i e)
unsafeForeignPtrToIOCArray p (l,u) =
   return (IOCArray l u (rangeSize (l,u)) p)

-- | /O(1)/ Extract ForeignPtr from a CArray.
toForeignPtr :: CArray i e -> (Int, ForeignPtr e)
toForeignPtr (CArray _ _ n fp) = (n, fp)

-- | /O(1)/ Turn a CArray into a ByteString.  Unsafe because it uses
-- 'castForeignPtr' and thus is not platform independent.
unsafeCArrayToByteString :: (Storable e) => CArray i e -> S.ByteString
unsafeCArrayToByteString (CArray _ _ l fp) = go undefined fp
    where go :: (Storable e) => e -> ForeignPtr e -> S.ByteString
          go dummy fp' = S.fromForeignPtr (castForeignPtr fp') 0 (l * sizeOf dummy)

-- | /O(1)/ Turn a ByteString into a CArray.  Unsafe because it uses
-- 'castForeignPtr' and thus is not platform independent.  Returns 'Nothing' if
-- the range specified is larger than the size of the ByteString or the start of
-- the ByteString does not fulfil the alignment requirement of the resulting
-- CArray (as specified by the Storable instance).
unsafeByteStringToCArray :: (Ix i, Storable e)
                            => (i,i) -> S.ByteString -> Maybe (CArray i e)
unsafeByteStringToCArray lu bs = go undefined lu
    where go :: (Ix i, Storable e) => e -> (i,i) -> Maybe (CArray i e)
          go dummy (l,u) | safe = Just (CArray l u n fp)
                         | otherwise = Nothing
              where n = rangeSize (l,u)
                    !(fp0, off, len) = S.toForeignPtr bs
                    fp = mapPtr (flip plusPtr off) fp0
                    p = unsafeForeignPtrToPtr fp
                    safe =
                        sizeOf dummy * n <= len &&
                        p == p `alignPtr` alignment dummy
          mapPtr :: (Ptr a -> Ptr b) -> ForeignPtr a -> ForeignPtr b
          mapPtr f (ForeignPtr addr contents) =
              case f $ Ptr addr of
                  Ptr addr' -> ForeignPtr addr' contents

copy :: (Ix i, Storable e) => CArray i e -> IO (CArray i e)
copy ain@(CArray l u n _) =
    createCArray (l,u) $ \op ->
        withCArray ain $ \ip ->
            copyArray op ip n

freezeIOCArray :: (Ix i, Storable e) => IOCArray i e -> IO (CArray i e)
freezeIOCArray = unsafeFreezeIOCArray >=> copy

unsafeFreezeIOCArray :: (Ix i) => IOCArray i e -> IO (CArray i e)
unsafeFreezeIOCArray (IOCArray l u n fp) = return (CArray l u n fp)

thawIOCArray :: (Ix i, Storable e) => CArray i e -> IO (IOCArray i e)
thawIOCArray = copy >=> unsafeThawIOCArray

unsafeThawIOCArray :: (Ix i) => CArray i e -> IO (IOCArray i e)
unsafeThawIOCArray (CArray l u n fp) = return (IOCArray l u n fp)

-- Since we can remove the (Storable e) restriction for these, the rules are
-- compact and general.
{-# RULES
"unsafeFreeze/IOCArray" unsafeFreeze = unsafeFreezeIOCArray
"unsafeThaw/IOCArray" unsafeThaw = unsafeThawIOCArray
  #-}

-- Since we can't parameterize the rules with the (Storable e) constraint, we
-- have to specialize manually.  This is unfortunate since it is less general.
{-# RULES
"freeze/IOCArray/Int"    freeze      = freezeIOCArray :: (Ix i) => IOCArray i Int -> IO (CArray i Int)
"freeze/IOCArray/Float"  freeze      = freezeIOCArray :: (Ix i) => IOCArray i Float -> IO (CArray i Float)
"freeze/IOCArray/Double" freeze      = freezeIOCArray :: (Ix i) => IOCArray i Double -> IO (CArray i Double)
"thaw/IOCArray/Int"      thaw        = thawIOCArray   :: (Ix i) => CArray i Int -> IO (IOCArray i Int)
"thaw/IOCArray/Float"    thaw        = thawIOCArray   :: (Ix i) => CArray i Float -> IO (IOCArray i Float)
"thaw/IOCArray/Double"   thaw        = thawIOCArray   :: (Ix i) => CArray i Double -> IO (IOCArray i Double)
  #-}


instance Storable e => IArray CArray e where
    {-# INLINE bounds #-}
    bounds (CArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (CArray _ _ n _) = n
    {-# NOINLINE unsafeArray #-}
    unsafeArray lu ies = unsafePerformIO $ unsafeArrayCArray lu ies (zeroElem (undefined :: e))
    {-# INLINE unsafeAt #-}
    unsafeAt (CArray _ _ _ fp) i = unsafeDupablePerformIO $
                                   withForeignPtr fp $ \a -> peekElemOff a i
    {-# NOINLINE unsafeReplace #-}
    unsafeReplace arr ies = unsafePerformIO $ unsafeReplaceCArray arr ies
    {-# NOINLINE unsafeAccum #-}
    unsafeAccum f arr ies = unsafePerformIO $ unsafeAccumCArray f arr ies
    {-# NOINLINE unsafeAccumArray #-}
    unsafeAccumArray f e0 lu ies = unsafePerformIO $ unsafeAccumArrayCArray f e0 lu ies


-- | Hackish way to get the zero element for a Storable type.
{-# NOINLINE zeroElem #-}
zeroElem :: Storable a => a -> a
zeroElem u = unsafePerformIO $
    withArray (replicate (sizeOf u) (0 :: Word8)) $ peek . castPtr

{-# INLINE unsafeArrayCArray #-}
unsafeArrayCArray :: (Storable e, Ix i)
                  => (i,i) -> [(Int, e)] -> e -> IO (CArray i e)
unsafeArrayCArray lu ies default_elem = do
    marr <- newArray lu default_elem
    mapM_ (uncurry $ unsafeWrite marr) ies
    unsafeFreezeIOCArray marr

{-# INLINE unsafeReplaceCArray #-}
unsafeReplaceCArray :: (Storable e, Ix i)
                       => CArray i e -> [(Int, e)] -> IO (CArray i e)
unsafeReplaceCArray arr ies = do
    marr <- thawIOCArray arr
    mapM_ (uncurry $ unsafeWrite marr) ies
    unsafeFreezeIOCArray marr

{-# INLINE unsafeAccumCArray #-}
unsafeAccumCArray :: (Storable e, Ix i)
                            => (e -> e' -> e) -> CArray i e -> [(Int, e')]
                                              -> IO (CArray i e)
unsafeAccumCArray f arr ies = do
    marr <- thawIOCArray arr
    forM_ ies $ \(i, new) -> do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
    unsafeFreezeIOCArray marr

{-# INLINE unsafeAccumArrayCArray #-}
unsafeAccumArrayCArray :: (Storable e, Ix i)
                          => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')]
                                            -> IO (CArray i e)
unsafeAccumArrayCArray f e0 lu ies = do
    marr <- newArray lu e0
    forM_ ies $ \(i, new) -> do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
    unsafeFreezeIOCArray marr

{-# INLINE eqCArray #-}
eqCArray :: (Storable e, Ix i, Eq e)
                   => CArray i e -> CArray i e -> Bool
eqCArray arr1@(CArray l1 u1 n1 _) arr2@(CArray l2 u2 n2 _) =
    if n1 == 0 then n2 == 0 else
        l1 == l2 && u1 == u2 &&
           and [unsafeAt arr1 i == unsafeAt arr2 i | i <- [0 .. n1 - 1]]

{-# INLINE [1] cmpCArray #-}
cmpCArray :: (Storable e, Ix i, Ord e)
                    => CArray i e -> CArray i e -> Ordering
cmpCArray arr1 arr2 = compare (assocs arr1) (assocs arr2)

{-# INLINE cmpIntCArray #-}
cmpIntCArray :: (Storable e, Ord e)
                       => CArray Int e -> CArray Int e -> Ordering
cmpIntCArray arr1@(CArray l1 u1 n1 _) arr2@(CArray l2 u2 n2 _) =
    if n1 == 0 then if n2 == 0 then EQ else LT else
    if n2 == 0 then GT else
    case compare l1 l2 of
        EQ    -> foldr cmp (compare u1 u2) [0 .. (n1 `min` n2) - 1]
        other -> other
    where
    cmp i rest = case compare (unsafeAt arr1 i) (unsafeAt arr2 i) of
        EQ    -> rest
        other -> other

{-# RULES "cmpCArray/Int" cmpCArray = cmpIntCArray #-}

instance (Ix ix, Eq e, Storable e) => Eq (CArray ix e) where
    (==) = eqCArray

instance (Ix ix, Ord e, Storable e) => Ord (CArray ix e) where
    compare = cmpCArray

instance (Ix ix, Show ix, Show e, Storable e) => Show (CArray ix e) where
    showsPrec = showsIArray

--
-- General purpose array operations which happen to be very fast for CArray.
--

-- | O(1) reshape an array.  The number of elements in the new shape must not
-- exceed the number in the old shape.  The elements are in C-style ordering.
reshape :: (Ix i, Ix j) => (j,j) -> CArray i e -> CArray j e
reshape (l',u') (CArray _ _ n fp) | n' > n = error "reshape: new size too large"
                                  | otherwise = CArray l' u' n' fp
    where n' = rangeSize (l', u')

-- | O(1) make a rank 1 array from an arbitrary shape.
-- It has the property that 'reshape (0, size a - 1) a == flatten a'.
flatten :: Ix i => CArray i e -> CArray Int e
flatten (CArray _ _ n fp) = CArray 0 (n - 1) n fp

--
-- None of the following are specific to CArray.  Some could have slightly
-- faster versions specialized to CArray.  In general, slicing is expensive
-- because the slice is not contiguous in memory, so must be copied.  There are
-- many specialized versions.
--

-- | Generic slice and map.  This takes the new range, the inverse map on
-- indices, and function to produce the next element.  It is the most general
-- operation in its class.
ixmapWithIndP :: (Ix i, Ix i', IArray a e, IArray a' e')
                 => (i',i') -> (i' -> i) -> (i -> e -> i' -> e') -> a i e -> a' i' e'
ixmapWithIndP lu f g arr = listArray lu
                           [ let i = f i' in g i (arr ! i) i' | i' <- range lu ]

-- | Less polymorphic version.
ixmapWithInd :: (Ix i, Ix i', IArray a e, IArray a e')
                => (i',i') -> (i' -> i) -> (i -> e -> i' -> e') -> a i e -> a i' e'
ixmapWithInd = ixmapWithIndP

-- | Perform an operation on the elements, independent of their location.
ixmapWithP  :: (Ix i, Ix i', IArray a e, IArray a' e')
               => (i',i') -> (i' -> i) -> (e -> e') -> a i e -> a' i' e'
ixmapWithP lu f g arr = listArray lu
                        [ g (arr ! f i') | i' <- range lu ]

-- | Less polymorphic version.
ixmapWith  :: (Ix i, Ix i', IArray a e, IArray a e')
              => (i',i') -> (i' -> i) -> (e -> e') -> a i e -> a i' e'
ixmapWith = ixmapWithP

-- | More polymorphic version of 'ixmap'.
ixmapP :: (Ix i, Ix i', IArray a e, IArray a' e)
          => (i',i') -> (i' -> i) -> a i e -> a' i' e
ixmapP lu f arr = ixmapWithP lu f id arr

-- | More friendly sub-arrays with element mapping.
sliceStrideWithP :: (Ix i, Shapable i, Ix i', IArray a e, IArray a' e')
                    => (i',i') -> (i,i,i) -> (e -> e') -> a i e -> a' i' e'
sliceStrideWithP lu (start,next,end) f arr
    | all (inRange (bounds arr)) [start,next,end] = listArray lu es
    | otherwise = error "sliceStrideWith: out of bounds"
    where is = offsetShapeFromThenTo (shape arr) (index' start) (index' next) (index' end)
          es = map (f . (unsafeAt arr)) is
          index' = indexes arr

-- | Less polymorphic version.
sliceStrideWith :: (Ix i, Shapable i, Ix i', IArray a e, IArray a e')
                   => (i',i') -> (i,i,i) -> (e -> e') -> a i e -> a i' e'
sliceStrideWith = sliceStrideWithP

-- | Strided sub-array without element mapping.
sliceStrideP :: (Ix i, Shapable i, Ix i', IArray a e, IArray a' e)
                => (i',i') -> (i,i,i) -> a i e -> a' i' e
sliceStrideP lu sne = sliceStrideWithP lu sne id

-- | Less polymorphic version.
sliceStride :: (Ix i, Shapable i, Ix i', IArray a e)
               => (i',i') -> (i,i,i) -> a i e -> a i' e
sliceStride = sliceStrideP

-- | Contiguous sub-array with element mapping.
sliceWithP :: (Ix i, Shapable i, Ix i', IArray a e, IArray a' e')
              => (i',i') -> (i,i) -> (e -> e') -> a i e -> a' i' e'
sliceWithP lu (start,end) f arr
    | all (inRange (bounds arr)) [start,end] = listArray lu es
    | otherwise = error "sliceWith: out of bounds"
    where is = offsetShapeFromTo (shape arr) (index' start) (index' end)
          es = map (f . (unsafeAt arr)) is
          index' = indexes arr

-- | Less polymorphic version.
sliceWith :: (Ix i, Shapable i, Ix i', IArray a e, IArray a e')
             => (i',i') -> (i,i) -> (e -> e') -> a i e -> a i' e'
sliceWith = sliceWithP

-- | Contiguous sub-array without element mapping.
sliceP :: (Ix i, Shapable i, Ix i', IArray a e, IArray a' e)
          => (i',i') -> (i,i) -> a i e -> a' i' e
sliceP lu se = sliceWithP lu se id

-- | Less polymorphic version.
slice ::  (Ix i, Shapable i, Ix i', IArray a e)
          => (i',i') -> (i,i) -> a i e -> a i' e
slice = sliceP

-- | In-place map on CArray.  Note that this is /IN PLACE/ so you should not
-- retain any reference to the original.  It flagrantly breaks referential
-- transparency!
{-# INLINE mapCArrayInPlace #-}
mapCArrayInPlace :: (Ix i, Storable e) => (e -> e) -> CArray i e -> CArray i e
mapCArrayInPlace f a = unsafeDupablePerformIO $ do
    withCArray a $ \p ->
        forM_ [0 .. size a - 1] $ \i ->
            peekElemOff p i >>= pokeElemOff p i . f
    return a

-----------------------------------------
-- These are meant to be internal only
indexes :: (Ix i, Shapable i, IArray a e) => a i e -> i -> [Int]
indexes a i = map pred $ (sShape . fst . bounds) a i

offsetShapeFromThenTo :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
offsetShapeFromThenTo s a b c = foldr (liftA2 (+)) [0] (ilists stride a b c)
    where ilists = zipWith4 (\s' a' b' c' -> map (*s') $ enumFromThenTo a' b' c')
          stride = shapeToStride s

offsetShapeFromTo :: [Int] -> [Int] -> [Int] -> [Int]
offsetShapeFromTo = offsetShapeFromTo' id

offsetShapeFromTo' :: ([[Int]] -> [[Int]]) -> [Int] -> [Int] -> [Int] -> [Int]
offsetShapeFromTo' f s a b = foldr (liftA2 (+)) [0] (f $ ilists stride a b)
    where ilists = zipWith3 (\s' a' b' -> map (*s') $ enumFromTo a' b')
          stride = shapeToStride s

offsets :: (Ix a, Shapable a) => (a, a) -> a -> [Int]
offsets lu i = reverse . osets (index lu i) . reverse . scanl1 (*) . uncurry sShape $ lu
    where osets 0 [] = []
          osets i' (b:bs) = r : osets d bs
              where (d,r) = i' `divMod` b
          osets _ _ = error "osets"
-----------------------------------------

-- | p-norm on the array taken as a vector
normp :: (Ix i, RealFloat e', Abs e e', IArray a e) => e' -> a i e -> e'
normp p a | 1 <= p && not (isInfinite p) = (** (1/p)) $ foldl' (\z e -> z + (abs_ e) ** p) 0 (elems a)
          | otherwise = error "normp: p < 1"

-- | 2-norm on the array taken as a vector (Frobenius norm for matrices)
norm2 :: (Ix i, Floating e', Abs e e', IArray a e) => a i e -> e'
norm2 a = sqrt $ foldl' (\z e -> z + abs_ e ^ (2 :: Int)) 0 (elems a)

-- | Sup norm on the array taken as a vector
normSup :: (Ix i, Num e', Ord e', Abs e e', IArray a e) => a i e -> e'
normSup a = foldl' (\z e -> z `max` abs_ e) 0 (elems a)

-- | Polymorphic version of amap.
liftArrayP :: (Ix i, IArray a e, IArray a1 e1)
              => (e -> e1) -> a i e -> a1 i e1
liftArrayP f a = listArray (bounds a) (map f (elems a))

-- | Equivalent to amap.  Here for consistency only.
liftArray :: (Ix i, IArray a e, IArray a e1)
              => (e -> e1) -> a i e -> a i e1
liftArray = liftArrayP

-- | Polymorphic 2-array lift.
liftArray2P :: (Ix i, IArray a e, IArray a1 e1, IArray a2 e2)
              => (e -> e1 -> e2) -> a i e -> a1 i e1 -> a2 i e2
liftArray2P f a b | aBounds == bounds b =
                     listArray aBounds (zipWith f (elems a) (elems b))
                 | otherwise = error "liftArray2: array bounds must match"
    where aBounds = bounds a

-- | Less polymorphic version.
liftArray2 :: (Ix i, IArray a e, IArray a e1, IArray a e2)
              => (e -> e1 -> e2) -> a i e -> a i e1 -> a i e2
liftArray2 = liftArray2P

-- | Polymorphic 3-array lift.
liftArray3P :: (Ix i, IArray a e, IArray a1 e1, IArray a2 e2, IArray a3 e3)
               => (e -> e1 -> e2 -> e3) -> a i e -> a1 i e1 -> a2 i e2 -> a3 i e3
liftArray3P f a b c | aBounds == bounds b && aBounds == bounds c =
                       listArray aBounds (zipWith3 f (elems a) (elems b) (elems c))
                   | otherwise = error "liftArray2: array bounds must match"
    where aBounds = bounds a

-- | Less polymorphic version.
liftArray3 :: (Ix i, IArray a e, IArray a e1, IArray a e2, IArray a e3)
              => (e -> e1 -> e2 -> e3) -> a i e -> a i e1 -> a i e2 -> a i e3
liftArray3 = liftArray3P


-- | Hack so that norms have a sensible type.
class Abs a b | a -> b where
    abs_ :: a -> b
instance Abs (Complex Double) Double where
    abs_ = magnitude
instance Abs (Complex Float) Float where
    abs_ = magnitude
instance Abs Double Double where
    abs_ = abs
instance Abs Float Float where
    abs_ = abs


-- | Allocate an array which is 16-byte aligned.  Essential for SIMD instructions.
mallocForeignPtrArrayAligned :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArrayAligned n = doMalloc undefined
  where
    doMalloc :: Storable b => b -> IO (ForeignPtr b)
    doMalloc dummy = mallocForeignPtrBytesAligned (n * sizeOf dummy)

-- | Allocate memory which is 16-byte aligned.  This is essential for SIMD
-- instructions.  We know that mallocPlainForeignPtrBytes will give word-aligned
-- memory, so we pad enough to be able to return the desired amount of memory
-- after aligning our pointer.
mallocForeignPtrBytesAligned :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytesAligned n = do
    (ForeignPtr addr contents) <- mallocPlainForeignPtrBytes (n + pad)
    let !(Ptr addr') = alignPtr (Ptr addr) 16
    return (ForeignPtr addr' contents)
    where pad = 16 - sizeOf (undefined :: Word)

-- | Make a new CArray with an IO action.
createCArray :: (Ix i, Storable e) => (i,i) -> (Ptr e -> IO ()) -> IO (CArray i e)
createCArray lu f = do
    fp <- mallocForeignPtrArrayAligned (rangeSize lu)
    withForeignPtr fp f
    unsafeForeignPtrToCArray fp lu

unsafeCreateCArray :: (Ix i, Storable e) => (i,i) -> (Ptr e -> IO ()) -> CArray i e
unsafeCreateCArray lu =  unsafePerformIO . createCArray lu


instance (Ix i, Binary i, Binary e, Storable e) => Binary (CArray i e) where
    put a = do
        put (bounds a)
        mapM_ put (elems a)
    get = do
        lu <- get
        es <- replicateM (rangeSize lu) get
        return $ listArray lu es


instance
    (Ix i, Arbitrary i, Storable e, Arbitrary e) =>
        Arbitrary (CArray i e) where
    arbitrary = do
        a <- QC.arbitrary
        b <- QC.arbitrary
        let rng = (min a b, max a b)
        fmap (listArray rng) $ QC.vector (rangeSize rng)

instance
    (Ix i, CoArbitrary i, Storable e, CoArbitrary e) =>
        CoArbitrary (CArray i e) where
    coarbitrary a = coarbitrary (assocs a)
