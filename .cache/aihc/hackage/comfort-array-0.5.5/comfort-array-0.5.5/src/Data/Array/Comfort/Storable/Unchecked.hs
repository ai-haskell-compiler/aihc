{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
The functions in this module miss any bound checking.
-}
module Data.Array.Comfort.Storable.Unchecked (
   Priv.Array(Array, shape, buffer),
   Priv.reshape,
   mapShape,

   (Priv.!),
   unsafeCreate,
   unsafeCreateWithSize,
   unsafeCreateWithSizes,
   unsafeCreateWithAutoSizes,
   unsafeCreateWithSizeAndResult,
   Priv.toList,
   Priv.fromList,
   Priv.vectorFromList,
   fromStorableVector,
   toStorableVector,

   map,
   mapWithIndex,
   zipWith,
   (Priv.//),
   Priv.accumulate,
   Priv.fromAssociations,

   singleton,
   append,
   take, drop,
   takeLeft, takeRight, split,
   takeCenter,

   sum, product,
   foldl,
   ) where

import qualified Data.Array.Comfort.Storable.Unchecked.Monadic as Monadic
import qualified Data.Array.Comfort.Storable.Private as Priv
import qualified Data.Array.Comfort.Storable.Memory as Memory
import qualified Data.Array.Comfort.Shape.SubSize as SubSize
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Private (Array(Array), mapShape)
import Data.Array.Comfort.Shape ((::+)((::+)))

import qualified Data.StorableVector.Base as SVB

import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Array (copyArray, advancePtr)
import Foreign.Storable (Storable, poke, peek)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)

import Control.Monad.ST (runST)
import Control.Applicative (liftA2)

import qualified Data.List as List

import Prelude hiding (map, zipWith, foldl, take, drop, sum, product)


{- $setup
>>> import DocTest.Data.Array.Comfort.Storable (ShapeInt, genArray)
>>>
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import Data.Array.Comfort.Storable (Array, (!))
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>> import Data.Word (Word16)
>>>
>>> newtype Array16 = Array16 (Array ShapeInt Word16)
>>>    deriving (Show)
>>>
>>> instance QC.Arbitrary Array16 where
>>>    arbitrary = fmap Array16 genArray
-}


unsafeCreate ::
   (Shape.C sh, Storable a) =>
   sh -> (Ptr a -> IO ()) -> Array sh a
unsafeCreate sh arr = runST (Monadic.unsafeCreate sh arr)

unsafeCreateWithSize ::
   (Shape.C sh, Storable a) =>
   sh -> (Int -> Ptr a -> IO ()) -> Array sh a
unsafeCreateWithSize sh arr = runST (Monadic.unsafeCreateWithSize sh arr)

unsafeCreateWithSizes ::
   (Shape.C sh, Storable a) =>
   SubSize.T sh nsize -> sh -> (nsize -> Ptr a -> IO ()) -> Array sh a
unsafeCreateWithSizes sub sh arr =
   runST (Monadic.unsafeCreateWithSizes sub sh arr)

unsafeCreateWithAutoSizes ::
   (Shape.C sh, sh ~ SubSize.ToShape nsize, SubSize.C nsize,
    Storable a) =>
   sh -> (nsize -> Ptr a -> IO ()) -> Array sh a
unsafeCreateWithAutoSizes = unsafeCreateWithSizes SubSize.auto

unsafeCreateWithSizeAndResult ::
   (Shape.C sh, Storable a) =>
   sh -> (Int -> Ptr a -> IO b) -> (Array sh a, b)
unsafeCreateWithSizeAndResult sh arr =
   runST (Monadic.unsafeCreateWithSizeAndResult sh arr)


fromStorableVector ::
   (Storable a) => SVB.Vector a -> Array (Shape.ZeroBased Int) a
fromStorableVector xs =
   case SVB.toForeignPtr xs of
      (fptr,0,n) -> Array (Shape.ZeroBased n) fptr
      (fptr,s,n) ->
         takeRight $ Array (Shape.ZeroBased s ::+ Shape.ZeroBased n) fptr

toStorableVector :: (Shape.C sh, Storable a) => Array sh a -> SVB.Vector a
toStorableVector (Array sh fptr) =
   SVB.fromForeignPtr fptr $ Shape.size sh


map ::
   (Shape.C sh, Storable a, Storable b) =>
   (a -> b) -> Array sh a -> Array sh b
map f (Array sh a) =
   unsafeCreateWithSize sh $ \n dstPtr ->
   withForeignPtr a $ \srcPtr ->
   sequence_ $ List.take n $
      List.zipWith
         (\src dst -> poke dst . f =<< peek src)
         (iterate (flip advancePtr 1) srcPtr)
         (iterate (flip advancePtr 1) dstPtr)

mapWithIndex ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Storable a, Storable b) =>
   (ix -> a -> b) -> Array sh a -> Array sh b
mapWithIndex f (Array sh a) =
   unsafeCreate sh $ \dstPtr ->
   withForeignPtr a $ \srcPtr ->
   sequence_ $
      List.zipWith3
         (\ix src dst -> poke dst . f ix =<< peek src)
         (Shape.indices sh)
         (iterate (flip advancePtr 1) srcPtr)
         (iterate (flip advancePtr 1) dstPtr)

zipWith ::
   (Shape.C sh, Storable a, Storable b, Storable c) =>
   (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWith f (Array _sh a) (Array sh b) =
   unsafeCreateWithSize sh $ \n dstPtr ->
   withForeignPtr a $ \srcAPtr ->
   withForeignPtr b $ \srcBPtr ->
   sequence_ $ List.take n $
      List.zipWith3
         (\srcA srcB dst -> poke dst =<< liftA2 f (peek srcA) (peek srcB))
         (iterate (flip advancePtr 1) srcAPtr)
         (iterate (flip advancePtr 1) srcBPtr)
         (iterate (flip advancePtr 1) dstPtr)


{- |
prop> \x  ->  Array.singleton x ! () == (x::Word16)
-}
singleton :: (Storable a) => a -> Array () a
singleton a = unsafeCreate () $ flip poke a


infixr 5 `append`

append ::
   (Shape.C shx, Shape.C shy, Storable a) =>
   Array shx a -> Array shy a -> Array (shx::+shy) a
append = Priv.append (::+)

{- |
prop> \(QC.NonNegative n) (Array16 x)  ->  x == Array.mapShape (Shape.ZeroBased . Shape.size) (Array.append (Array.take n x) (Array.drop n x))
-}
take, drop ::
   (Integral n, Storable a) =>
   n -> Array (Shape.ZeroBased n) a -> Array (Shape.ZeroBased n) a
take n = takeLeft . splitN n
drop n = takeRight . splitN n

splitN ::
   (Integral n, Storable a) =>
   n -> Array (Shape.ZeroBased n) a ->
   Array (Shape.ZeroBased n ::+ Shape.ZeroBased n) a
splitN n = mapShape (Shape.zeroBasedSplit n)

{- |
prop> \(Array16 x) (Array16 y) -> let xy = Array.append x y in x == Array.takeLeft xy  &&  y == Array.takeRight xy
-}
takeLeft ::
   (Shape.C sh0, Shape.C sh1, Storable a) =>
   Array (sh0::+sh1) a -> Array sh0 a
takeLeft =
   takeCenter . mapShape (\(sh0 ::+ sh1) -> (Shape.Zero ::+ sh0 ::+ sh1))

takeRight ::
   (Shape.C sh0, Shape.C sh1, Storable a) =>
   Array (sh0::+sh1) a -> Array sh1 a
takeRight =
   takeCenter . mapShape (\(sh0 ::+ sh1) -> (sh0 ::+ sh1 ::+ Shape.Zero))

split ::
   (Shape.C sh0, Shape.C sh1, Storable a) =>
   Array (sh0::+sh1) a -> (Array sh0 a, Array sh1 a)
split x = (takeLeft x, takeRight x)

{- |
prop> \(Array16 x) (Array16 y) (Array16 z) -> let xyz = Array.append x $ Array.append y z in y == Array.takeCenter xyz
-}
takeCenter ::
   (Shape.C sh0, Shape.C sh1, Shape.C sh2, Storable a) =>
   Array (sh0::+sh1::+sh2) a -> Array sh1 a
takeCenter (Array (sh0::+sh1::+_sh2) x) =
   unsafeCreateWithSize sh1 $ \k yPtr ->
   withForeignPtr x $ \xPtr ->
      copyArray yPtr (advancePtr xPtr (Shape.size sh0)) k



{- |
prop> \(Array16 xs)  ->  Array.sum xs == sum (Array.toList xs)
-}
sum :: (Shape.C sh, Storable a, Num a) => Array sh a -> a
sum = foldl (+) 0

{- |
prop> \(Array16 xs)  ->  Array.product xs == product (Array.toList xs)
-}
product :: (Shape.C sh, Storable a, Num a) => Array sh a -> a
product = foldl (*) 1

{-# INLINE foldl #-}
foldl :: (Shape.C sh, Storable a) => (b -> a -> b) -> b -> Array sh a -> b
foldl op a (Array sh x) = unsafePerformIO $
   withForeignPtr x $ \xPtr ->
      Memory.foldl (const op) a (Shape.size sh) xPtr 1
