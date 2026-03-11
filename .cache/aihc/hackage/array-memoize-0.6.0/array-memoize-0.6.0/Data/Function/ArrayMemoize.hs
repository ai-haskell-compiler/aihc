{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Data.Function.ArrayMemoize where

import qualified Data.Array.MArray as MArray
import Data.Array.Unboxed
import Data.Array.IO (IOUArray)
import Data.Array.ST (STArray, STUArray, runSTArray)
import Control.Monad.ST


{-| Memoize a function over a finite (sub)domain, using an array (boxed), e.g., 
    @
     arrayMemo (0, 20) f
    @
    memoizes f between from 0 to 20. -}
{-# INLINE arrayMemo #-}
arrayMemo :: (Ix a, ArrayMemoizable b) => (a, a) -> (a -> b) -> (a -> b)
arrayMemo (l, u) f = 
    let cache = runSTArray (do cache <- newArray_ (l, u)
                               mapM_ (\x -> writeArray cache x (f x)) (range (l, u))
                               return cache)
    in \x -> cache ! x


{-| Memoize a fixed point of a function over a sub domain.
    Similar to 'fix', but over 'arrayMemo', passing a function a memoized 
    version of itself. -}
{-# INLINE arrayMemoFix #-}
arrayMemoFix :: (Ix a, ArrayMemoizable b) => (a, a) -> ((a -> b) -> (a -> b)) -> a -> b
arrayMemoFix (l, u) f = memo_f where memo_f = arrayMemo (l, u) (f memo_f) 


{-| Memoize a mutual fixed point for two functions (over sub domains of these functions). -}
{-# INLINE arrayMemoFixMutual #-}
arrayMemoFixMutual :: (ArrayMemoizable b, ArrayMemoizable d, Ix a, Ix c) => (a, a) -> (c, c) -> ((a -> b) -> (c -> d) -> (a -> b)) -> ((a -> b) -> (c -> d) -> (c -> d)) -> (a -> b)
arrayMemoFixMutual (l, u) (l', u') f g =
    memo_f where memo_f = arrayMemo (l, u) (f memo_f memo_g) 
                 memo_g = arrayMemo (l', u') (g memo_f memo_g) 


{-| Memoize a mutual fixed point for three functions (over sub domains of these functions). -}

{-# INLINE arrayMemoFixMutual3 #-}
arrayMemoFixMutual3 :: (ArrayMemoizable b, ArrayMemoizable d, ArrayMemoizable f, Ix a, Ix c, Ix e) => (a, a) -> (c, c) -> (e, e) -> ((a -> b) -> (c -> d) -> (e -> f) -> (a -> b)) -> ((a -> b) -> (c -> d) -> (e -> f) -> (c -> d)) ->
 ((a -> b) -> (c -> d) -> (e -> f) -> (e -> f)) -> (a -> b)
arrayMemoFixMutual3 (l, u) (l', u') (l'', u'') f g h =
    memo_f where memo_f = arrayMemo (l, u) (f memo_f memo_g memo_h) 
                 memo_g = arrayMemo (l', u') (g memo_f memo_g memo_h) 
                 memo_h = arrayMemo (l'', u'') (h memo_f memo_g memo_h) 


{-| Memoize a function over a finite (sub)domain, using an unboxed 'IO' array. 
    This requires the incoming function to return results in the 'IO' monad, but should 
    preferable be pure. -}
{-# INLINE uarrayMemoFixIO #-}
uarrayMemoFixIO :: (Ix a, UArrayMemoizable b) => (a, a) -> ((a -> IO b) -> (a -> IO b)) ->  a -> IO b
uarrayMemoFixIO (l, u) f =
    \i -> do cache <- newUArray_ (l, u)
             let f' = f (\x -> readUArray cache x)
             mapM_ (\x -> (f' x) >>= (\val -> writeUArray cache x val)) (range (l, u))            
             f' i


{-| Memoize and discretize a function over a finite (sub)domain, using an array.  e.g.
    @
     discretemMemo (0.0, 10.0) 2.0 f
    @
    returns a discretized version of f (with the discrete type defined by 'Discrete') 
    in the range 0.0 to 10.0 with step size 2.0 (i.e., the resulting discrete domain is 
    of size 5). -}
{-# INLINE discreteMemo #-}
discreteMemo :: (ArrayMemoizable b, Discretize a) => (a, a) -> a -> (a -> b) -> (Discrete a -> b)
discreteMemo (l, u) delta f =
     let disc  = discretize delta
         cache = runSTArray (do cache <- newArray_ (disc l, disc u)
                                mapM_ (\x -> writeArray cache x (f (continuize delta x))) (enumFromTo (disc l) (disc u))
                                return cache)
         
     in (\x -> cache ! x)

{-| Memoize and discretize a fixed point of a function over a subdomain with discretization step -}
{-# INLINE discreteMemoFix #-}
discreteMemoFix :: (ArrayMemoizable b, Discretize a) => (a, a) -> a -> ((a -> b) -> (a -> b)) ->(Discrete a -> b)
discreteMemoFix (l, u) delta f =
     let disc  = discretize delta
         cache' = runSTArray (do cache <- newArray_ (disc l, disc u)
                                 mapM_ (\x -> writeArray cache x (f (\x -> cache' ! (disc x)) (continuize delta x))) (enumFromTo (disc l) (disc u))
                                 return cache)
     in (\x -> cache' ! x)


{-| Memoize and quantize a function over a finite (sub)domain, using a boxed array, e.g, 
    @
     quantizedMemo (0.0, 10.0) 2.0 f
    @
    memoizes f between 0.0 and 10.0 with step size 2.0 (i.e. the function is quantized into 
    5 parts, memoized into an array of size 5).
-}
{-# INLINE quantizedMemo #-}
quantizedMemo :: (ArrayMemoizable b, Discretize a) => (a, a) -> a -> (a -> b) -> (a -> b)
quantizedMemo (l, u) delta f =
     let disc  = discretize delta
         cache = runSTArray (do cache <- newArray_ (disc l, succ (disc u))
                                mapM_ (\x -> writeArray cache x (f (continuize delta x))) (enumFromTo (disc l) (succ (disc u)))
                                return cache)
     in (\x -> cache ! disc x)


{-| Memoize and quantize a fixed point of a function. Similar to 'fix', but using 'quantizedMemo' 
to pass the fixed function a quantized memoized version of itself,
 therefore memoizing any recursive calls. -}
{-# INLINE quantizedMemoFix #-} 
quantizedMemoFix :: (ArrayMemoizable b, Discretize a) => (a, a) -> a -> ((a -> b) -> (a -> b)) -> (a -> b)
quantizedMemoFix (l, u) delta f = memo_f where memo_f = quantizedMemo (l, u) delta (f memo_f) 


{-| Memoize and quantize a mutually recursive fixed point of two functions. -}
{-# INLINE quantizedMemoFixMutual #-}
quantizedMemoFixMutual :: (ArrayMemoizable b, ArrayMemoizable d, Discretize a, Discretize c) => (a, a) -> a -> (c, c) -> c -> ((a -> b) -> (c -> d) -> (a -> b)) -> ((a -> b) -> (c -> d) -> (c -> d)) -> (a -> b)
quantizedMemoFixMutual (l, u) delta (l', u') delta' f g =
    memo_f where memo_f = quantizedMemo (l, u) delta (f memo_f memo_g) 
                 memo_g = quantizedMemo (l', u') delta' (g memo_f memo_g) 


{-| Memoize and quantize a mutually recursive fixed point of three functions. -}
{-# INLINE quantizedMemoFixMutual3 #-}
quantizedMemoFixMutual3 :: (ArrayMemoizable b, ArrayMemoizable d, ArrayMemoizable f, Discretize a, Discretize c, Discretize e) => (a, a) -> a -> (c, c) -> c -> (e, e) -> e -> ((a -> b) -> (c -> d) -> (e -> f) -> (a -> b)) -> ((a -> b) -> (c -> d) -> (e -> f) -> (c -> d)) -> ((a -> b) -> (c -> d) -> (e -> f) -> (e -> f)) -> (a -> b)
quantizedMemoFixMutual3 (l, u) delta (l', u') delta' (l'', u'') delta'' f g h =
    memo_f where memo_f = quantizedMemo (l, u) delta (f memo_f memo_g memo_h) 
                 memo_g = quantizedMemo (l', u') delta' (g memo_f memo_g memo_h) 
                 memo_h = quantizedMemo (l'', u'') delta'' (h memo_f memo_g memo_h) 


{-| 'ArrayMemoizable' defines the subset of types for which we can do array 
    memoization -}
 
class ArrayMemoizable a where
    newArray_ :: (Ix i) => (i, i) -> ST s (STArray s i a)
    writeArray :: (Ix i) => STArray s i a -> i -> a -> ST s ()

instance ArrayMemoizable Float where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

instance ArrayMemoizable Double where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

instance ArrayMemoizable Integer where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

instance ArrayMemoizable Int where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

instance ArrayMemoizable Bool where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

instance ArrayMemoizable Char where
    newArray_ = MArray.newArray_
    writeArray = MArray.writeArray

{-| 'UArrayMemoizable' defines the subset of types for which we can 
do unboxed IOUArray memoization -}

class IArray UArray a => UArrayMemoizable a where
    newUArray_ :: (Ix i) => (i, i) -> IO (IOUArray i a)
    writeUArray :: (Ix i) => IOUArray i a -> i -> a -> IO ()
    readUArray :: (Ix i) => IOUArray i a -> i -> IO a

instance UArrayMemoizable Float where
    newUArray_ = MArray.newArray_
    readUArray = MArray.readArray
    writeUArray = MArray.writeArray

instance UArrayMemoizable Double where
    newUArray_ = MArray.newArray_
    readUArray = MArray.readArray
    writeUArray = MArray.writeArray

instance UArrayMemoizable Int where
    newUArray_ = MArray.newArray_
    readUArray = MArray.readArray
    writeUArray = MArray.writeArray

instance UArrayMemoizable Char where
    newUArray_ = MArray.newArray_
    readUArray = MArray.readArray
    writeUArray = MArray.writeArray

{- Num and Enum classes for working with tuple domains -}

instance (Enum a, Enum b) => Enum (a, b) where
    toEnum = undefined
    succ (a, b) = (succ a, succ b)
    fromEnum (a, b) = fromEnum a * fromEnum b

    enumFromTo (lx, ly) (ux, uy) = 
        [ly..uy] >>= (\y -> [lx..ux] >>= (\x -> return (x, y)))

instance (Enum a, Enum b, Enum c) => Enum (a, b, c) where
    toEnum = undefined
    succ (a, b, c) = (succ a, succ b, succ c)
    fromEnum (a, b, c) = fromEnum a * fromEnum b * fromEnum c

    enumFromThenTo (lx, ly, lz) (nx, ny, nz) (ux, uy, uz) = 
        [lz,nz..uz] >>= (\z -> [ly,ny..uy] >>= (\y -> [lx,nx..ux] >>= (\x -> return (x, y, z))))

instance (Num a, Num b) => Num (a, b) where
    (a1, b1) + (a2, b2) = (a1 + a2, b1 + b2)
    (a1, b1) * (a2, b2) = (a1 * a2, b1 * b2)
    negate (a, b) = (negate a, negate b)
    abs (a, b) = (abs a, abs b)
    signum (a, b) = (signum a, signum b)
    fromInteger i = (0, fromInteger i)

instance (Num a, Num b, Num c) => Num (a, b, c) where
    (a1, b1, c1) + (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)
    (a1, b1, c1) * (a2, b2, c2) = (a1 * a2, b1 * b2, c1 * c2)
    negate (a, b, c) = (negate a, negate b, negate c)
    abs (a, b, c) = (abs a, abs b, abs c)
    signum (a, b, c) = (signum a, signum b, signum c)
    fromInteger i = (0, 0, fromInteger i)

{-|  Discretization of float/double values and tuples -}

class (Ix (Discrete t), Enum (Discrete t)) => Discretize t where
    type Discrete t
    discretize :: t -> t -> Discrete t
    continuize :: t -> Discrete t -> t

instance Discretize Float where
    type Discrete Float = Int
    discretize delta x = round' (x / delta) 
                           where round' x = let (n,r) = properFraction x in n + (round r)
    continuize delta x = (fromIntegral x) * delta

instance Discretize Double where
    type Discrete Double = Int
    discretize delta x = round' (x / delta) 
                           where round' x = let (n,r) = properFraction x in n + (round r)
    continuize delta x = (fromIntegral x) * delta

instance (Discretize a, Discretize b) => Discretize (a, b) where
    type Discrete (a, b) = (Discrete a, Discrete b)
    discretize (dx, dy) (x, y) = (discretize dx x, discretize dy y)
    continuize (dx, dy) (x, y) = (continuize dx x, continuize dy y)

instance (Discretize a, Discretize b, Discretize c) => Discretize (a, b, c) where
    type Discrete (a, b, c) = (Discrete a, Discrete b, Discrete c)
    discretize (dx, dy, dz) (x, y, z) = (discretize dx x, discretize dy y, discretize dz z)
    continuize (dx, dy, dz) (x, y, z) = (continuize dx x, continuize dy y, continuize dz z)

{-| Discretized a function function. The second parameter is the discretisation step. -}
discrete :: Discretize a => (a -> b) -> a -> (Discrete a -> b)
discrete f delta = f . (continuize delta)