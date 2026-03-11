{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Array.Comfort.Boxed.Unchecked (
   Array(..),
   reshape,
   mapShape,
   (!),
   toList,
   fromList,
   vectorFromList,
   replicate,
   map,
   zipWith,

   append,
   take, drop,
   takeLeft, takeRight, split,
   takeCenter,
   ) where

import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.Primitive.Array as Prim
import Data.Array.Comfort.Shape ((::+)((::+)))

-- FixMe: In GHC-7.4.2 there is no instance PrimMonad (Lazy.ST s)
-- import qualified Control.Monad.ST.Lazy as ST
import qualified Control.Monad.ST.Strict as ST
import Control.Monad (liftM)
import Control.Applicative (Applicative, pure, (<*>), (<$>))
import Control.DeepSeq (NFData, rnf)

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.List as List
import Prelude hiding (map, zipWith, replicate, take, drop)


{- $setup
>>> import qualified Data.Array.Comfort.Boxed as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import Data.Array.Comfort.Boxed (Array, (!))
>>> import Data.Tuple.HT (swap)
>>> import Control.Applicative ((<$>))
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>> type ShapeInt = Shape.ZeroBased Int
>>>
>>> genArray :: QC.Gen (Array ShapeInt Char)
>>> genArray = Array.vectorFromList <$> QC.arbitrary
>>>
>>> newtype ArrayChar = ArrayChar (Array ShapeInt Char)
>>>    deriving (Show)
>>>
>>> instance QC.Arbitrary ArrayChar where
>>>    arbitrary = fmap ArrayChar genArray
>>>
>>>
>>> transpose ::
>>>    (Shape.Indexed sh0, Shape.Indexed sh1) =>
>>>    Array (sh0,sh1) a -> Array (sh1,sh0) a
>>> transpose a =
>>>    fmap (\(i,j) -> a!(j,i)) $ Array.indices $ swap $ Array.shape a
-}


data Array sh a =
   Array {
      shape :: sh,
      buffer :: Prim.Array a
   } deriving (Eq)

instance (Shape.C sh, Show sh, Show a) => Show (Array sh a) where
   showsPrec p arr =
      showParen (p>10) $
         showString "BoxedArray.fromList " .
         showsPrec 11 (shape arr) .
         showChar ' ' .
         shows (toList arr)


instance (Shape.C sh, NFData sh, NFData a) => NFData (Array sh a) where
   rnf a@(Array sh _arr) = rnf (sh, toList a)

instance (Shape.C sh) => Functor (Array sh) where
   fmap = map

{- |
We must restrict 'Applicative' to 'Shape.Static' because of 'pure'.
Because the shape is static, we do not need a size check in '(<*>)'.
-}
instance (Shape.Static sh) => Applicative (Array sh) where
   pure = replicate Shape.static
   (<*>) = zipWith ($)

instance (Shape.C sh) => Fold.Foldable (Array sh) where
   fold = Fold.fold . buffer
   foldMap f = Fold.foldMap f . buffer
   foldl f a = Fold.foldl f a . buffer
   foldr f a = Fold.foldr f a . buffer
   foldl1 f = Fold.foldl1 f . buffer
   foldr1 f = Fold.foldr1 f . buffer

instance (Shape.C sh) => Trav.Traversable (Array sh) where
   traverse f (Array sh arr) = Array sh <$> Trav.traverse f arr
   sequenceA (Array sh arr) = Array sh <$> Trav.sequenceA arr
   mapM f (Array sh arr) = liftM (Array sh) $ Trav.mapM f arr
   sequence (Array sh arr) = liftM (Array sh) $ Trav.sequence arr


reshape :: sh1 -> Array sh0 a -> Array sh1 a
reshape sh (Array _ arr) = Array sh arr

mapShape :: (sh0 -> sh1) -> Array sh0 a -> Array sh1 a
mapShape f (Array sh arr) = Array (f sh) arr


infixl 9 !

(!) :: (Shape.Indexed sh) => Array sh a -> Shape.Index sh -> a
(!) (Array sh arr) ix = Prim.indexArray arr $ Shape.uncheckedOffset sh ix

toList :: (Shape.C sh) => Array sh a -> [a]
toList (Array sh arr) =
   List.map (Prim.indexArray arr) $ List.take (Shape.size sh) [0..]

fromList :: (Shape.C sh) => sh -> [a] -> Array sh a
fromList sh xs = Array sh $ Prim.arrayFromListN (Shape.size sh) xs

vectorFromList :: [a] -> Array (Shape.ZeroBased Int) a
vectorFromList xs =
   let arr = Prim.arrayFromList xs
   in Array (Shape.ZeroBased $ Prim.sizeofArray arr) arr

replicate :: (Shape.C sh) => sh -> a -> Array sh a
replicate sh a =
   Array sh $
   ST.runST (Prim.unsafeFreezeArray  =<< Prim.newArray (Shape.size sh) a)

map :: (Shape.C sh) => (a -> b) -> Array sh a -> Array sh b
map f (Array sh arr) = Array sh $
   let n = Shape.size sh
   in Prim.arrayFromListN n $ List.map (f . Prim.indexArray arr) $ List.take n [0..]

zipWith ::
   (Shape.C sh) => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWith f (Array sha arra) (Array _shb arrb) = Array sha $
   let n = Shape.size sha
   in Prim.arrayFromListN n $
      List.map (\k -> f (Prim.indexArray arra k) (Prim.indexArray arrb k)) $
      List.take n [0..]



infixr 5 `append`

append ::
   (Shape.C shx, Shape.C shy) =>
   Array shx a -> Array shy a -> Array (shx::+shy) a
append (Array shX x) (Array shY y) =
   let sizeX = Shape.size shX in
   let sizeY = Shape.size shY in
   Array (shX::+shY) $
   ST.runST (do
      arr <-
         Prim.newArray (sizeX+sizeY)
            (error "Boxed.append: uninitialized element")
      Prim.copyArray arr 0 x 0 sizeX
      Prim.copyArray arr sizeX y 0 sizeY
      Prim.unsafeFreezeArray arr)

{- |
prop> \(QC.NonNegative n) (ArrayChar x)  ->  x == Array.mapShape (Shape.ZeroBased . Shape.size) (Array.append (Array.take n x) (Array.drop n x))
-}
take, drop ::
   (Integral n) =>
   n -> Array (Shape.ZeroBased n) a -> Array (Shape.ZeroBased n) a
take n = takeLeft . splitN n
drop n = takeRight . splitN n

splitN ::
   (Integral n) =>
   n -> Array (Shape.ZeroBased n) a ->
   Array (Shape.ZeroBased n ::+ Shape.ZeroBased n) a
splitN n = mapShape (Shape.zeroBasedSplit n)

{- |
prop> \(ArrayChar x) (ArrayChar y) -> let xy = Array.append x y in x == Array.takeLeft xy  &&  y == Array.takeRight xy
-}
takeLeft ::
   (Shape.C sh0, Shape.C sh1) =>
   Array (sh0::+sh1) a -> Array sh0 a
takeLeft =
   takeCenter . mapShape (\(sh0 ::+ sh1) -> (Shape.Zero ::+ sh0 ::+ sh1))

takeRight ::
   (Shape.C sh0, Shape.C sh1) =>
   Array (sh0::+sh1) a -> Array sh1 a
takeRight =
   takeCenter . mapShape (\(sh0 ::+ sh1) -> (sh0 ::+ sh1 ::+ Shape.Zero))

split ::
   (Shape.C sh0, Shape.C sh1) =>
   Array (sh0::+sh1) a -> (Array sh0 a, Array sh1 a)
split x = (takeLeft x, takeRight x)

{- |
prop> \(ArrayChar x) (ArrayChar y) (ArrayChar z) -> let xyz = Array.append x $ Array.append y z in y == Array.takeCenter xyz
-}
takeCenter ::
   (Shape.C sh0, Shape.C sh1, Shape.C sh2) =>
   Array (sh0::+sh1::+sh2) a -> Array sh1 a
takeCenter (Array (sh0::+sh1::+_sh2) x) =
   Array sh1 $ Prim.cloneArray x (Shape.size sh0) (Shape.size sh1)
