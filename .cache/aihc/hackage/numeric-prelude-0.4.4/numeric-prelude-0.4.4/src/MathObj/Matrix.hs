{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright    :   (c) Henning Thielemann 2009, Mikael Johansson 2006
Maintainer   :   numericprelude@henning-thielemann.de
Stability    :   provisional
Portability  :   requires multi-parameter type classes

Routines and abstractions for Matrices and
basic linear algebra over fields or rings.

We stick to simple Int indices.
Although advanced indices would be nice
e.g. for matrices with sub-matrices,
this is not easily implemented since arrays
do only support a lower and an upper bound
but no additional parameters.

ToDo:
 - Matrix inverse, determinant (see htam:Matrix)
-}

module MathObj.Matrix (
   T, Dimension,
   format,
   transpose,
   rows,
   columns,
   index,
   fromRows,
   fromColumns,
   fromList,
   dimension,
   numRows,
   numColumns,
   zipWith,
   zero,
   one,
   diagonal,
   scale,
   random,
   randomR,
   ) where

import qualified Algebra.Module   as Module
import qualified Algebra.Vector   as Vector
import qualified Algebra.Ring     as Ring
import qualified Algebra.Additive as Additive

import Algebra.Module((*>), )
import Algebra.Ring((*), fromInteger, scalarProduct, )
import Algebra.Additive((+), (-), subtract, )

import qualified System.Random as Rnd
import Data.Array (Array, array, listArray, accumArray, elems, bounds, (!), ixmap, range, )
import qualified Data.List as List

import Control.Monad (liftM2, )
import Control.Exception (assert, )

import Data.Function.HT (powerAssociative, )
import Data.Tuple.HT (swap, mapFst, )
import Data.List.HT (outerProduct, )
import Text.Show.HT (concatS, )

import NumericPrelude.Numeric (Int, )
import NumericPrelude.Base hiding (zipWith, )


{- $setup
>>> import qualified MathObj.Matrix as Matrix
>>> import qualified Algebra.Ring as Ring
>>> import qualified Algebra.Laws as Laws
>>> import Test.NumericPrelude.Utility ((/\))
>>> import qualified Test.QuickCheck as QC
>>> import NumericPrelude.Numeric as NP
>>> import NumericPrelude.Base as P
>>> import Prelude ()
>>>
>>> import Control.Monad (replicateM, join)
>>> import Control.Applicative (liftA2)
>>> import Data.Function.HT (nest)
>>>
>>> genDimension :: QC.Gen Int
>>> genDimension = QC.choose (0,20)
>>>
>>> genMatrixFor :: (QC.Arbitrary a) => Int -> Int -> QC.Gen (Matrix.T a)
>>> genMatrixFor m n =
>>>    fmap (Matrix.fromList m n) $ replicateM (m*n) QC.arbitrary
>>>
>>> genMatrix :: (QC.Arbitrary a) => QC.Gen (Matrix.T a)
>>> genMatrix = join $ liftA2 genMatrixFor genDimension genDimension
>>>
>>> genIntMatrix :: QC.Gen (Matrix.T Integer)
>>> genIntMatrix = genMatrix
>>>
>>> genFactorMatrix :: (QC.Arbitrary a) => Matrix.T a -> QC.Gen (Matrix.T a)
>>> genFactorMatrix a = genMatrixFor (Matrix.numColumns a) =<< genDimension
>>>
>>> genSameMatrix :: (QC.Arbitrary a) => Matrix.T a -> QC.Gen (Matrix.T a)
>>> genSameMatrix = uncurry genMatrixFor . Matrix.dimension
-}


{- |
A matrix is a twodimensional array, indexed by integers.
-}
newtype T a =
   Cons (Array (Dimension, Dimension) a)
      deriving (Eq, Ord, Read)

type Dimension = Int

{- |
Transposition of matrices is just transposition in the sense of Data.List.

prop> genIntMatrix /\ \a -> Matrix.rows a == Matrix.columns (Matrix.transpose a)
prop> genIntMatrix /\ \a -> Matrix.columns a == Matrix.rows (Matrix.transpose a)
prop> genIntMatrix /\ \a -> genSameMatrix a /\ \b -> Laws.homomorphism Matrix.transpose (+) (+) a b
-}
transpose :: T a -> T a
transpose (Cons m) =
   let (lower,upper) = bounds m
   in  Cons (ixmap (swap lower, swap upper) swap m)

rows :: T a -> [[a]]
rows mM@(Cons m) =
   let ((lr,lc), (ur,uc)) = bounds m
   in  outerProduct (index mM) (range (lr,ur)) (range (lc,uc))

columns :: T a -> [[a]]
columns mM@(Cons m) =
   let ((lr,lc), (ur,uc)) = bounds m
   in  outerProduct (flip (index mM)) (range (lc,uc)) (range (lr,ur))

index :: T a -> Dimension -> Dimension -> a
index (Cons m) i j = m ! (i,j)

{- |
prop> genIntMatrix /\ \a -> a == uncurry Matrix.fromRows (Matrix.dimension a) (Matrix.rows a)
-}
fromRows :: Dimension -> Dimension -> [[a]] -> T a
fromRows m n =
   Cons .
   array (indexBounds m n) .
   concat .
   List.zipWith (\r -> map (\(c,x) -> ((r,c),x))) allIndices .
   map (zip allIndices)

{- |
prop> genIntMatrix /\ \a -> a == uncurry Matrix.fromColumns (Matrix.dimension a) (Matrix.columns a)
-}
fromColumns :: Dimension -> Dimension -> [[a]] -> T a
fromColumns m n =
   Cons .
   array (indexBounds m n) .
   concat .
   List.zipWith (\r -> map (\(c,x) -> ((c,r),x))) allIndices .
   map (zip allIndices)

fromList :: Dimension -> Dimension -> [a] -> T a
fromList m n xs = Cons (listArray (indexBounds m n) xs)

appPrec :: Int
appPrec = 10

instance (Show a) => Show (T a) where
   showsPrec p m =
      showParen (p >= appPrec)
         (showString "Matrix.fromRows " . showsPrec appPrec (rows m))

format :: (Show a) => T a -> String
format m = formatS m ""

formatS :: (Show a) => T a -> ShowS
formatS =
   concatS .
   map (\r -> showString "(" . concatS r . showString ")\n") .
   map (List.intersperse (' ':) . map (showsPrec 11)) .
   rows

dimension :: T a -> (Dimension,Dimension)
dimension (Cons m) = uncurry subtract (bounds m) + (1,1)

numRows :: T a -> Dimension
numRows = fst . dimension

numColumns :: T a -> Dimension
numColumns = snd . dimension

-- These implementations may benefit from a better exception than
-- just assertions to validate dimensionalities
{- |
prop> genIntMatrix /\ \a -> genSameMatrix a /\ \b -> Laws.commutative (+) a b
prop> genIntMatrix /\ \a -> genSameMatrix a /\ \b -> genSameMatrix b /\ \c -> Laws.associative (+) a b c
-}
instance (Additive.C a) => Additive.C (T a) where
   (+) = zipWith (+)
   (-) = zipWith (-)
   zero = zero 1 1

zipWith :: (a -> b -> c) -> T a -> T b -> T c
zipWith op mM@(Cons m) nM@(Cons n) =
   let d = dimension mM
       em = elems m
       en = elems n
   in  assert (d == dimension nM) $
         uncurry fromList d (List.zipWith op em en)

{- |
prop> genIntMatrix /\ \a -> Laws.identity (+) (uncurry Matrix.zero $ Matrix.dimension a) a
-}
zero :: (Additive.C a) => Dimension -> Dimension -> T a
zero m n =
   fromList m n $
   List.repeat Additive.zero
--    List.replicate (fromInteger (m*n)) zero

one :: (Ring.C a) => Dimension -> T a
one n =
   Cons $
   accumArray (flip const) Additive.zero
      (indexBounds n n)
      (map (\i -> ((i,i), Ring.one)) (indexRange n))

{- |
prop> genDimension /\ \n -> Matrix.one n == Matrix.diagonal (replicate n Ring.one :: [Integer])
-}
diagonal :: (Additive.C a) => [a] -> T a
diagonal xs =
   let n = List.length xs
   in  Cons $
       accumArray (flip const) Additive.zero
          (indexBounds n n)
          (zip (map (\i -> (i,i)) allIndices) xs)

scale :: (Ring.C a) => a -> T a -> T a
scale s = Vector.functorScale s

{- |
prop> genIntMatrix /\ \a -> Laws.leftIdentity  (*) (Matrix.one (Matrix.numRows a)) a
prop> genIntMatrix /\ \a -> Laws.rightIdentity (*) (Matrix.one (Matrix.numColumns a)) a
prop> genIntMatrix /\ \a -> genFactorMatrix a /\ \b -> Laws.homomorphism Matrix.transpose (*) (flip (*)) a b
prop> genIntMatrix /\ \a -> genFactorMatrix a /\ \b -> genFactorMatrix b /\ \c -> Laws.associative (*) a b c
prop> genIntMatrix /\ \b -> genSameMatrix b /\ \c -> genFactorMatrix b /\ \a -> Laws.leftDistributive (*) (+) a b c
prop> genIntMatrix /\ \a -> genFactorMatrix a /\ \b -> genSameMatrix b /\ \c -> Laws.rightDistributive (*) (+) a b c
prop> QC.choose (0,10) /\ \k -> genDimension /\ \n -> genMatrixFor n n /\ \a -> a^k == nest (fromInteger k) ((a::Matrix.T Integer)*) (Matrix.one n)
-}
instance (Ring.C a) => Ring.C (T a) where
   mM * nM =
      assert (numColumns mM == numRows nM) $
      fromList (numRows mM) (numColumns nM) $
      liftM2 scalarProduct (rows mM) (columns nM)
   fromInteger n = fromList 1 1 [fromInteger n]
   mM ^ n =
      assert (numColumns mM == numRows mM) $
      assert (n >= Additive.zero) $
      powerAssociative (*) (one (numColumns mM)) mM n

instance Functor T where
   fmap f (Cons m) = Cons (fmap f m)

instance Vector.C T where
   zero  = Additive.zero
   (<+>) = (+)
   (*>)  = scale

instance Module.C a b => Module.C a (T b) where
   x *> m = fmap (x*>) m


random :: (Rnd.RandomGen g, Rnd.Random a) =>
   Dimension -> Dimension -> g -> (T a, g)
random =
   randomAux Rnd.random

randomR :: (Rnd.RandomGen g, Rnd.Random a) =>
   Dimension -> Dimension -> (a,a) -> g -> (T a, g)
randomR m n rng =
   randomAux (Rnd.randomR rng) m n

{-
could be made nicer with the State monad,
but I like to keep dependencies minimal
-}
randomAux :: (Rnd.RandomGen g, Rnd.Random a) =>
   (g -> (a,g)) -> Dimension -> Dimension -> g -> (T a, g)
randomAux rnd m n g0 =
   mapFst (fromList m n) $ swap $
   List.mapAccumL (\g _i -> swap $ rnd g) g0 (indexRange (m*n))

{-
What more do we need from our matrix type? We have addition,
subtraction and multiplication, and thus composition of generic
free-module-maps. We're going to want to solve linear equations with
or without fields underneath, so we're going to want an implementation
of the Gaussian algorithm as well as most probably Smith normal
form. Determinants are cool, and these are to be calculated either
with the Gaussian algorithm or some other goodish method.
-}

{-
{- |
 We'll want generic linear equation solving, returning one solution,
any solution really, or nothing. Basically, this is asking for the
preimage of a given vector over the given map, so

a_11 x_1 + .. + a_1n x_n = y_1
 ...
a_m1 x_1 + .. + a_mn a_n = y_m

has really x_1,...,x_n as a preimage of the vector y_1,..,y_m under
the map (a_ij), since obviously y_1,..,y_m = (a_ij) x_1,..,x_n

So, generic linear equation solving boils down to the function
-}
preimage :: (Ring.C a) => T a -> T a -> Maybe (T a)
preimage a y = assert
        (numRows a == numRows y &&     -- they match
         numColumns y == 1)               -- and y is a column vector
                Nothing
-}

{-
Cf. /usr/lib/hugs/demos/Matrix.hs
-}


-- these functions control whether we use 0 or 1 based indices

indexRange :: Dimension -> [Dimension]
indexRange n = [0..(n-1)]

indexBounds ::
   Dimension -> Dimension ->
   ((Dimension,Dimension), (Dimension,Dimension))
indexBounds m n =
   ((0,0), (m-1,n-1))

allIndices :: [Dimension]
allIndices = [0..]
