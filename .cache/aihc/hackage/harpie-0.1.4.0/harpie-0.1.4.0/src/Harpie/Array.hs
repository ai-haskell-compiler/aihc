{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | Arrays with shape information and computations at a value-level.
module Harpie.Array
  ( -- * Usage
    -- $usage

    -- * Harpie Arrays
    Array (..),
    array,
    (><),
    validate,
    safeArray,
    unsafeModifyShape,
    unsafeModifyVector,

    -- * Dimensions
    Dim,
    Dims,

    -- * Conversion
    FromVector (..),
    FromArray (..),

    -- * Shape Access
    shape,
    rank,
    size,
    length,
    isNull,

    -- * Indexing
    index,
    (!),
    (!?),
    tabulate,
    backpermute,

    -- * Scalars
    fromScalar,
    toScalar,
    isScalar,
    asSingleton,
    asScalar,

    -- * Array Creation
    empty,
    range,
    corange,
    indices,
    ident,
    konst,
    singleton,
    diag,
    undiag,

    -- * Element-level functions
    zipWith,
    zipWithSafe,
    modify,
    imap,

    -- * Function generalisers
    rowWise,
    colWise,
    dimsWise,

    -- * Single-dimension functions
    take,
    drop,
    select,
    insert,
    delete,
    append,
    prepend,
    concatenate,
    couple,
    slice,
    rotate,

    -- * Multi-dimension functions
    takes,
    drops,
    indexes,
    slices,
    heads,
    lasts,
    tails,
    inits,

    -- * Function application
    extracts,
    reduces,
    joins,
    joinsSafe,
    join,
    joinSafe,
    traverses,
    maps,
    filters,
    zips,
    zipsSafe,
    modifies,
    diffs,

    -- * Array expansion & contraction
    expand,
    coexpand,
    contract,
    prod,
    dot,
    mult,
    windows,

    -- * Search
    find,
    findNoOverlap,
    findIndices,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,

    -- * Shape manipulations
    fill,
    cut,
    cutSuffix,
    pad,
    lpad,
    reshape,
    flat,
    repeat,
    cycle,
    rerank,
    reorder,
    squeeze,
    elongate,
    transpose,
    inflate,
    intercalate,
    intersperse,
    concats,
    reverses,
    rotates,

    -- * Sorting
    sorts,
    sortsBy,
    orders,
    ordersBy,

    -- * Transmission
    transmit,
    transmitSafe,
    transmitOp,
    telecasts,
    telecastsSafe,

    -- * Row specializations
    pattern (:<),
    cons,
    uncons,
    pattern (:>),
    snoc,
    unsnoc,

    -- * Shape specializations
    iota,

    -- * Math
    uniform,
    invtri,
    inverse,
    chol,
  )
where

import Control.Monad hiding (join)
import Data.Bool
import Data.Foldable hiding (find, length, minimum)
import Data.Function
import Data.List qualified as List
import Data.Vector qualified as V
import GHC.Generics
import Harpie.Shape hiding (asScalar, asSingleton, concatenate, range, rank, reorder, rerank, rotate, size, squeeze)
import Harpie.Shape qualified as S
import Harpie.Sort
import Prettyprinter hiding (dot, fill)
import System.Random hiding (uniform)
import System.Random.Stateful hiding (uniform)
import Prelude as P hiding (cycle, drop, length, repeat, take, zip, zipWith)

-- $setup
-- >>> :m -Prelude
-- >>> import Prelude hiding (take, drop, zipWith, length, cycle, repeat)
-- >>> import Harpie.Array as A
-- >>> import Harpie.Shape qualified as S
-- >>> import Data.Vector qualified as V
-- >>> import Prettyprinter hiding (dot, fill)
-- >>> import Data.List qualified as List
-- >>> let s = 1 :: Array Int
-- >>> s
-- UnsafeArray [] [1]
-- >>> pretty s
-- 1
-- >>> let v = range [3]
-- >>> v
-- UnsafeArray [3] [0,1,2]
-- >>> let m = range [2,3]
-- >>> pretty m
-- [[0,1,2],
--  [3,4,5]]
-- >>> let a = range [2,3,4]
-- >>> a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]

-- $usage
--
-- Several names used in @harpie@ conflict with [Prelude](https://hackage.haskell.org/package/base/docs/Prelude.html):
--
-- >>> import Prelude hiding (cycle, repeat, take, drop, zipWith, length)
--
-- In general, 'Array' functionality is contained in @Harpie.Array@ and shape  functionality is contained in @Harpie.Shape@. These two modules also have name clashes and at least one needs to be qualified:
--
-- >>> import Harpie.Array as A
-- >>> import Harpie.Shape qualified as S
--
-- [@prettyprinter@](https://hackage.haskell.org/package/prettyprinter) is used to prettily render arrays to better visualise shape.
--
-- >>> import Prettyprinter hiding (dot,fill)
--
-- Examples of arrays:
--
-- An array with no dimensions (a scalar).
--
-- >>> s = 1 :: Array Int
-- >>> s
-- UnsafeArray [] [1]
-- >>> shape s
-- []
-- >>> pretty s
-- 1
--
-- A single-dimension array (a vector).
--
-- >>> let v = range [3]
-- >>> pretty v
-- [0,1,2]
--
-- A two-dimensional array (a matrix).
--
-- >>> let m = range [2,3]
-- >>> pretty m
-- [[0,1,2],
--  [3,4,5]]
--
-- An n-dimensional array (n should be finite).
--
-- >>> a = range [2,3,4]
-- >>> a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]

-- | A hyperrectangular (or multidimensional) array with a value-level shape.
--
-- >>> let a = array [2,3,4] [1..24] :: Array Int
-- >>> a
-- UnsafeArray [2,3,4] [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
--
-- >>> pretty a
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
data Array a = UnsafeArray [Int] (V.Vector a)
  deriving stock (Generic)
  deriving stock (Eq, Ord, Show)

type role Array representational

instance Functor Array where
  fmap f = unsafeModifyVector (V.map f)

instance Foldable Array where
  foldr f x0 a = V.foldr f x0 (asVector a)

instance Traversable Array where
  traverse f (UnsafeArray s v) =
    array s <$> traverse f v

instance (Show a) => Pretty (Array a) where
  pretty a@(UnsafeArray _ v) = case rank a of
    0 -> viaShow (V.head v)
    1 -> viaShow v
    _ ->
      pretty "["
        <> indent
          0
          ( vsep
              ( punctuate comma $
                  pretty
                    <$> toList (extracts [0] a)
              )
          )
        <> pretty "]"

-- * conversions

instance (Num a) => Num (Array a) where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = error "multiplication not defined"
  abs = fmap abs
  signum = fmap signum
  fromInteger x = toScalar (fromInteger x)

-- | Conversion to and from a `V.Vector`
--
-- Note that conversion of an 'Array' to a vector drops shape information, so that:
--
-- > vectorAs . asVector == id
-- > asVector . vectorAs == flat
--
-- >>> asVector (range [2,3])
-- [0,1,2,3,4,5]
--
-- >>> vectorAs (V.fromList [0..5]) :: Array Int
-- UnsafeArray [6] [0,1,2,3,4,5]
class FromVector t a | t -> a where
  asVector :: t -> V.Vector a
  vectorAs :: V.Vector a -> t

instance FromVector (V.Vector a) a where
  asVector = id
  vectorAs = id

instance FromVector [a] a where
  asVector = V.fromList
  vectorAs = V.toList

instance FromVector (Array a) a where
  asVector (UnsafeArray _ v) = v
  vectorAs v = UnsafeArray [V.length v] v

-- | Conversion to and from an `Array`
--
-- Note that conversion of an 'Array' to a `FromArray` likely drops shape information, so that:
--
-- > arrayAs . asArray == flat
-- > asArray . arrayAs == id
--
-- >>> asArray ([0..5::Int])
-- UnsafeArray [6] [0,1,2,3,4,5]
--
-- >>> arrayAs (range [2,3]) :: [Int]
-- [0,1,2,3,4,5]
class FromArray t a | t -> a where
  asArray :: t -> Array a
  arrayAs :: Array a -> t

instance FromArray (Array a) a where
  asArray = id
  arrayAs = id

instance FromArray [a] a where
  asArray l = UnsafeArray [S.rank l] (V.fromList l)
  arrayAs (UnsafeArray _ v) = V.toList v

instance FromArray (V.Vector a) a where
  asArray v = UnsafeArray [V.length v] v
  arrayAs (UnsafeArray _ v) = v

-- | Construct an array from a shape and a value without any shape validation.
--
-- >>> array [2,3] [0..5]
-- UnsafeArray [2,3] [0,1,2,3,4,5]
array :: (FromVector t a) => [Int] -> t -> Array a
array s (asVector -> v) = UnsafeArray s v

infixl 4 ><

-- | Construct an Array.
--
-- >>> pretty $ [2,3] >< [0..5]
-- [[0,1,2],
--  [3,4,5]]
(><) :: (FromVector t a) => [Int] -> t -> Array a
(><) = array

-- | Validate the size and shape of an array.
--
-- >>> validate (array [2,3,4] [1..23] :: Array Int)
-- False
validate :: Array a -> Bool
validate a = size a == V.length (asVector a)

-- | Construct an Array, checking shape.
--
-- >>> safeArray [2,3,4] [0..23] == Just a
-- True
safeArray :: (FromVector t a) => [Int] -> t -> Maybe (Array a)
safeArray s v =
  bool Nothing (Just a) (validate a)
  where
    a = UnsafeArray s (asVector v)

-- | Unsafely modify an array shape.
--
-- >>> unsafeModifyShape (fmap (+1) :: [Int] -> [Int]) (array [2,3] [0..5])
-- UnsafeArray [3,4] [0,1,2,3,4,5]
unsafeModifyShape :: ([Int] -> [Int]) -> Array a -> Array a
unsafeModifyShape f (UnsafeArray s v) = UnsafeArray (f s) v

-- | Unsafely modify an array vector.
--
-- >>> unsafeModifyVector (V.map (+1)) (array [2,3] [0..5])
-- UnsafeArray [2,3] [1,2,3,4,5,6]
unsafeModifyVector :: (FromVector u a) => (FromVector v b) => (u -> v) -> Array a -> Array b
unsafeModifyVector f (UnsafeArray s v) = UnsafeArray s (asVector (f (vectorAs v)))

-- | Representation of an index into a shape (an [Int]). The index is a dimension of the shape.
type Dim = Int

-- | Representation of indexes into a shape (an [Int]). The indexes are dimensions of the shape.
type Dims = [Int]

-- | shape of an Array
--
-- >>> shape a
-- [2,3,4]
shape :: Array a -> [Int]
shape (UnsafeArray s _) = s

-- | rank of an Array
--
-- >>> rank a
-- 3
rank :: Array a -> Int
rank = List.length . shape

-- | size of an Array, which is the total number of elements, if the Array is valid.
--
-- >>> size a
-- 24
size :: Array a -> Int
size = S.size . shape

-- | Number of rows (first dimension size) in an Array. As a convention, a scalar value is still a single row.
--
-- >>> length a
-- 2
-- >>> length (toScalar 0)
-- 1
length :: Array a -> Int
length a = case shape a of
  [] -> 1
  (x : _) -> x

-- | Is the Array empty (has zero number of elements).
--
-- >>> isNull ([2,0] >< [] :: Array ())
-- True
-- >>> isNull ([] >< [4] :: Array Int)
-- False
isNull :: Array a -> Bool
isNull = (0 ==) . size

-- | Extract an element at an index, unsafely.
--
-- >>> index a [1,2,3]
-- 23
index :: Array a -> [Int] -> a
index (UnsafeArray s v) i = V.unsafeIndex v (flatten s i)

infixl 9 !

-- | Extract an element at an index, unsafely.
--
-- >>> a ! [1,2,3]
-- 23
(!) :: Array a -> [Int] -> a
(!) = index

-- | Extract an element at an index, safely.
--
-- >>> a !? [1,2,3]
-- Just 23
-- >>> a !? [2,3,1]
-- Nothing
(!?) :: Array a -> [Int] -> Maybe a
(!?) a xs = bool Nothing (Just (a ! xs)) (xs `isFins` shape a)

-- | Tabulate an array supplying a shape and a tabulation function.
--
-- >>> tabulate [2,3,4] (S.flatten [2,3,4]) == a
-- True
tabulate :: [Int] -> ([Int] -> a) -> Array a
tabulate ds f =
  UnsafeArray ds (V.generate (V.product (asVector ds)) (f . shapen ds))

-- | @backpermute@ is a tabulation where the contents of an array do not need to be accessed, and is thus a fulcrum for leveraging laziness and fusion via the rule:
--
-- > backpermute f g (backpermute f' g' a) == backpermute (f . f') (g . g') a
--
-- Many functions in this module are examples of backpermute usage.
--
-- >>> pretty $ backpermute List.reverse List.reverse a
-- [[[0,12],
--   [4,16],
--   [8,20]],
--  [[1,13],
--   [5,17],
--   [9,21]],
--  [[2,14],
--   [6,18],
--   [10,22]],
--  [[3,15],
--   [7,19],
--   [11,23]]]
backpermute :: ([Int] -> [Int]) -> ([Int] -> [Int]) -> Array a -> Array a
backpermute f g a = tabulate (f (shape a)) (index a . g)
{-# INLINEABLE backpermute #-}

{- RULES
   "backpermute/backpermute" forall f f' g g' (a :: forall a. Array a)). backpermute f g (backpermute f' g' a) == backpermute (f . f') (g . g') a

-}

-- | Unwrap a scalar.
--
-- >>> let s = array [] [3] :: Array Int
-- >>> fromScalar s
-- 3
fromScalar :: Array a -> a
fromScalar a = index a ([] :: [Int])

-- | Wrap a scalar.
--
-- >>> :t toScalar 2
-- toScalar 2 :: Num a => Array a
toScalar :: a -> Array a
toScalar a = tabulate [] (const a)

-- | Is an array a Scalar?
--
-- >>> isScalar (toScalar (2::Int))
-- True
isScalar :: Array a -> Bool
isScalar a = rank a == 0

-- | Convert a scalar to being a dimensioned array. Do nothing if not a scalar.
--
-- >>> asSingleton (toScalar 4)
-- UnsafeArray [1] [4]
asSingleton :: Array a -> Array a
asSingleton = unsafeModifyShape S.asSingleton

-- | Convert an array with shape [1] to being a scalar (Do nothing if not a shape [1] array).
--
-- >>> asScalar (singleton 3)
-- UnsafeArray [] [3]
asScalar :: Array a -> Array a
asScalar = unsafeModifyShape S.asScalar

-- * Creation

-- | An array with no elements.
--
-- >>> empty
-- UnsafeArray [0] []
empty :: Array a
empty = array [0] []

-- | An enumeration of row-major or [lexicographic](https://en.wikipedia.org/wiki/Lexicographic_order) order.
--
-- >>> pretty $ range [2,3]
-- [[0,1,2],
--  [3,4,5]]
range :: [Int] -> Array Int
range xs = tabulate xs (flatten xs)

-- | An enumeration of col-major or [colexicographic](https://en.wikipedia.org/wiki/Lexicographic_order) order.
--
-- >>> pretty (corange [2,3,4])
-- [[[0,6,12,18],
--   [2,8,14,20],
--   [4,10,16,22]],
--  [[1,7,13,19],
--   [3,9,15,21],
--   [5,11,17,23]]]
corange :: [Int] -> Array Int
corange xs = tabulate xs (flatten (List.reverse xs) . List.reverse)

-- | Indices of an array shape.
--
-- >>> pretty $ indices [3,3]
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
indices :: [Int] -> Array [Int]
indices ds = tabulate ds id

-- | The identity array.
--
-- >>> pretty $ ident [3,3]
-- [[1,0,0],
--  [0,1,0],
--  [0,0,1]]
ident :: (Num a) => [Int] -> Array a
ident ds = tabulate ds (bool 0 1 . isDiag)

-- | Create an array composed of a single value.
--
-- >>> pretty $ konst [3,2] 1
-- [[1,1],
--  [1,1],
--  [1,1]]
konst :: [Int] -> a -> Array a
konst ds a = tabulate ds (const a)

-- | Create an array of shape [1].
--
-- >>> pretty $ singleton 1
-- [1]
-- >>> singleton 3 == toScalar 3
-- False
--
-- >>> asVector (singleton 3) == asVector (toScalar 3)
-- True
singleton :: a -> Array a
singleton a = UnsafeArray [1] (V.singleton a)

-- | Extract the diagonal of an array.
--
-- >>> pretty $ diag (ident [3,3])
-- [1,1,1]
diag ::
  Array a ->
  Array a
diag a = backpermute minDim (replicate (rank a) . getDim 0) a

-- | Expand the array to form a diagonal array.
--
-- >>> pretty $ undiag (range [3])
-- [[0,0,0],
--  [0,1,0],
--  [0,0,2]]
undiag ::
  (Num a) =>
  Array a ->
  Array a
undiag a = tabulate (shape a <> shape a) (\xs -> bool 0 (index a xs) (isDiag xs))

-- | Zip two arrays at an element level.
--
-- >>> zipWith (-) v v
-- UnsafeArray [3] [0,0,0]
zipWith :: (a -> b -> c) -> Array a -> Array b -> Array c
zipWith f (UnsafeArray s v) (UnsafeArray _ v') = UnsafeArray s (V.zipWith f v v')

-- | Zip two arrays at an element level, checking for shape consistency.
--
-- >>> zipWithSafe (-) (range [3]) (range [4])
-- Nothing
zipWithSafe :: (a -> b -> c) -> Array a -> Array b -> Maybe (Array c)
zipWithSafe f (UnsafeArray s v) (UnsafeArray s' v') = bool Nothing (Just $ UnsafeArray s (V.zipWith f v v')) (s == s')

-- | Modify a single value at an index.
--
-- >>> pretty $ modify [0,0] (const 100) (range [3,2])
-- [[100,1],
--  [2,3],
--  [4,5]]
modify :: [Int] -> (a -> a) -> Array a -> Array a
modify ds f a = tabulate (shape a) (\s -> bool id f (s == ds) (index a s))

-- | Maps an index function at element-level.
--
-- >>> pretty $ imap (\xs x -> x - sum xs) a
-- [[[0,0,0,0],
--   [3,3,3,3],
--   [6,6,6,6]],
--  [[11,11,11,11],
--   [14,14,14,14],
--   [17,17,17,17]]]
imap ::
  ([Int] -> a -> b) ->
  Array a ->
  Array b
imap f a = zipWith f (indices (shape a)) a

-- | With a function that takes dimensions and (type-level) parameters, apply the parameters to the initial dimensions. ie
--
-- > rowWise f xs = f [0..] xs
--
-- >>> rowWise indexes [1,0] a
-- UnsafeArray [4] [12,13,14,15]
rowWise :: (Dims -> [x] -> Array a -> Array a) -> [x] -> Array a -> Array a
rowWise f xs a = f [0 .. (S.rank xs - 1)] xs a

-- | With a function that takes dimensions and (type-level) parameters, apply the parameters to the the last dimensions. ie
--
-- > colWise f xs = f (List.reverse [0 .. (rank a - 1)]) xs
--
-- >>> colWise indexes [1,0] a
-- UnsafeArray [2] [1,13]
colWise :: (Dims -> [x] -> Array a -> Array a) -> [x] -> Array a -> Array a
colWise f xs a = f (List.reverse [(rank a - S.rank xs) .. (rank a - 1)]) xs a

-- | With a function that takes a dimension and a parameter, fold dimensions and parameters using the function.
--
-- >>> dimsWise take [0,2] [1,2] a
-- UnsafeArray [1,3,2] [0,1,4,5,8,9]
dimsWise :: (Dim -> x -> Array a -> Array a) -> Dims -> [x] -> Array a -> Array a
dimsWise f ds xs a = foldl' (\a' (d, x) -> f d x a') a (List.zip ds xs)

-- | Take the top-most elements across the specified dimension. Negative values take the bottom-most. No index check is performed.
--
-- > take d x == takes [(d,x)]
--
-- >>> pretty $ take 2 1 a
-- [[[0],
--   [4],
--   [8]],
--  [[12],
--   [16],
--   [20]]]
-- >>> pretty $ take 2 (-1) a
-- [[[3],
--   [7],
--   [11]],
--  [[15],
--   [19],
--   [23]]]
take ::
  Dim ->
  Int ->
  Array a ->
  Array a
take d t a = backpermute dsNew (modifyDim d (\x -> x + bool 0 (getDim d (shape a) + t) (t < 0))) a
  where
    dsNew = takeDim d (abs t)

-- | Drop the top-most elements across the specified dimension. Negative values take the bottom-most.
--
-- >>> pretty $ drop 2 1 a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]],
--  [[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
-- >>> pretty $ drop 2 (-1) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
drop ::
  Dim ->
  Int ->
  Array a ->
  Array a
drop d t a = backpermute dsNew (modifyDim d (\x -> x + bool t 0 (t < 0))) a
  where
    dsNew = dropDim d (abs t)

-- | Select an index along a dimension.
--
-- >>> let s = select 2 3 a
-- >>> pretty s
-- [[3,7,11],
--  [15,19,23]]
select ::
  Dim ->
  Int ->
  Array a ->
  Array a
select d x a = backpermute (deleteDim d) (insertDim d x) a

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert 2 0 a (konst [2,3] 0)
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
-- >>> insert 0 0 (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [2,1]
insert ::
  Dim ->
  Int ->
  Array a ->
  Array a ->
  Array a
insert d i a b = tabulate (incAt d (shape a)) go
  where
    go s
      | getDim d s == i = index b (deleteDim d s)
      | getDim d s < i = index a s
      | otherwise = index a (decAt d s)

-- | Delete along a dimension at a position.
--
-- >>> pretty $ delete 2 0 a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]],
--  [[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
delete ::
  Dim ->
  Int ->
  Array a ->
  Array a
delete d i a = backpermute (decAt d) (\s -> bool (incAt d s) s (getDim d s < i)) a

-- | Insert along a dimension at the end.
--
-- >>> pretty $ append 2 a (konst [2,3] 0)
-- [[[0,1,2,3,0],
--   [4,5,6,7,0],
--   [8,9,10,11,0]],
--  [[12,13,14,15,0],
--   [16,17,18,19,0],
--   [20,21,22,23,0]]]
append ::
  Dim ->
  Array a ->
  Array a ->
  Array a
append d a b = insert d (getDim d (shape a)) a b

-- | Insert along a dimension at the beginning.
--
-- >>> pretty $ prepend 2 (konst [2,3] 0) a
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
prepend ::
  Dim ->
  Array a ->
  Array a ->
  Array a
prepend d a b = insert d 0 b a

-- | Concatenate along a dimension.
--
-- >>> shape $ concatenate 1 a a
-- [2,6,4]
-- >>> concatenate 0 (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [1,2]
-- >>> concatenate 0 (toScalar 0) (asArray [1..3])
-- UnsafeArray [4] [0,1,2,3]
concatenate ::
  Dim ->
  Array a ->
  Array a ->
  Array a
concatenate d a0 a1 = tabulate (S.concatenate d (shape a0) (shape a1)) go
  where
    go s =
      bool
        (index a0 s)
        ( index
            a1
            ( insertDim
                d
                (getDim d s - getDim d ds0)
                (deleteDim d s)
            )
        )
        (getDim d s >= getDim d ds0)
    ds0 = shape a0

-- | Combine two arrays as a new dimension of a new array.
--
-- >>> pretty $ couple 0 (asArray [1,2,3]) (asArray [4,5,6::Int])
-- [[1,2,3],
--  [4,5,6]]
couple :: Int -> Array a -> Array a -> Array a
couple d a a' = concatenate d (elongate d a) (elongate d a')

-- | Slice along a dimension with the supplied offset & length.
--
-- >>> let s = slice 2 1 2 a
-- >>> pretty s
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slice ::
  Dim ->
  Int ->
  Int ->
  Array a ->
  Array a
slice d o l a = backpermute (setDim d l) (modifyDim d (+ o)) a

-- | Rotate an array along a dimension.
--
-- >>> pretty $ rotate 1 2 a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotate ::
  Dim ->
  Int ->
  Array a ->
  Array a
rotate d r a = backpermute id (rotateIndex d r (shape a)) a

-- * multi-dimension operators

-- | Takes the top-most elements across the supplied dimension,n tuples. Negative values take the bottom-most.
--
-- > takes == dimsWise take
--
-- >>> pretty $ takes [0,2] [1,-3] a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]]]
takes ::
  Dims ->
  [Int] ->
  Array a ->
  Array a
takes ds xs a = backpermute dsNew (List.zipWith (+) start) a
  where
    dsNew = setDims ds xsAbs
    start = List.zipWith (\x s -> bool 0 (s + x) (x < 0)) (setDims ds xs (replicate (rank a) 0)) (shape a)
    xsAbs = fmap abs xs

-- | Drops the top-most elements. Negative values drop the bottom-most.
--
-- >>> pretty $ drops [0,1,2] [1,2,-3] a
-- [[[20]]]
drops ::
  Dims ->
  [Int] ->
  Array a ->
  Array a
drops ds xs a = backpermute dsNew (List.zipWith (\d' s' -> bool (d' + s') s' (d' < 0)) xsNew) a
  where
    dsNew = dropDims ds xsAbs
    xsNew = setDims ds xs (replicate (rank a) 0)
    xsAbs = fmap abs xs

-- | Select by dimensions and indexes.
--
-- >>> let s = indexes [0,1] [1,1] a
-- >>> pretty s
-- [16,17,18,19]
indexes :: Dims -> [Int] -> Array a -> Array a
indexes ds xs a = backpermute (deleteDims ds) (insertDims ds xs) a

-- | Slice along dimensions with the supplied offsets and lengths.
--
-- >>> let s = slices [2,0] [1,1] [2,1] a
-- >>> pretty s
-- [[[13,14],
--   [17,18],
--   [21,22]]]
slices :: Dims -> [Int] -> [Int] -> Array a -> Array a
slices ds os ls a = dimsWise (\d (o, l) -> slice d o l) ds (List.zip os ls) a

-- | Select the first element along the supplied dimensions.
--
-- >>> pretty $ heads [0,2] a
-- [0,4,8]
heads :: Dims -> Array a -> Array a
heads ds a = indexes ds (List.replicate (S.rank ds) 0) a

-- | Select the last element along the supplied dimensions.
--
-- >>> pretty $ lasts [0,2] a
-- [15,19,23]
lasts :: Dims -> Array a -> Array a
lasts ds a = indexes ds lastds a
  where
    lastds = (\i -> getDim i (shape a) - 1) <$> ds

-- | Select the tail elements along the supplied dimensions.
--
-- >>> pretty $ tails [0,2] a
-- [[[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
tails :: Dims -> Array a -> Array a
tails ds a = slices ds os ls a
  where
    os = List.replicate (S.rank ds) 1
    ls = getLastPositions ds (shape a)

-- | Select the init elements along the supplied dimensions.
--
-- >>> pretty $ inits [0,2] a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]]]
inits :: Dims -> Array a -> Array a
inits ds a = slices ds os ls a
  where
    os = List.replicate (S.rank ds) 0
    ls = getLastPositions ds (shape a)

-- | Extracts dimensions to an outer layer.
--
-- >>> pretty $ shape <$> extracts [0] a
-- [[3,4],[3,4]]
extracts ::
  Dims ->
  Array a ->
  Array (Array a)
extracts ds a = tabulate (getDims ds (shape a)) go
  where
    go s = indexes ds s a

-- | Reduce along specified dimensions, using the supplied fold.
--
-- >>> pretty $ reduces [0] sum a
-- [66,210]
-- >>> pretty $ reduces [0,2] sum a
-- [[12,15,18,21],
--  [48,51,54,57]]
reduces ::
  Dims ->
  (Array a -> b) ->
  Array a ->
  Array b
reduces ds f a = fmap f (extracts ds a)

-- | Join inner and outer dimension layers by supplied dimensions. No checks on shape.
--
-- >>> let e = extracts [1,0] a
-- >>> let j = joins [1,0] e
-- >>> a == j
-- True
joins ::
  Dims ->
  Array (Array a) ->
  Array a
joins ds a = tabulate (insertDims ds so si) go
  where
    go s = index (index a (getDims ds s)) (deleteDims ds s)
    so = shape a
    si = shape (index a (replicate (rank a) 0))

-- | Join inner and outer dimension layers by supplied dimensions. Check inner layer shape.
--
-- >>> let e = extracts [1,0] a
-- >>> (Just j) = joinsSafe [1,0] e
-- >>> a == j
-- True
joinsSafe ::
  Dims ->
  Array (Array a) ->
  Maybe (Array a)
joinsSafe ds a =
  bool
    Nothing
    (Just $ joins ds a)
    (allEqual (fmap shape a))

-- | Join inner and outer dimension layers in outer dimension order.
--
-- >>> a == join (extracts [0,1] a)
-- True
join ::
  Array (Array a) ->
  Array a
join a = joins (S.dimsOf (shape a)) a

-- | Join inner and outer dimension layers in outer dimension order, checking for consistent inner dimension shape.
--
-- >>> joinSafe (extracts [0,1] a)
-- Just (UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23])
joinSafe ::
  Array (Array a) ->
  Maybe (Array a)
joinSafe a =
  bool
    Nothing
    (Just $ join a)
    (allEqual (fmap shape a))

-- | Satisfy a predicate across all elements
allEqual :: (Eq a) => Array a -> Bool
allEqual a = case arrayAs a of
  [] -> True
  (x : xs) -> all (== x) xs

-- | Traverse along specified dimensions.
--
-- traverses [1] print (range [2,3])
-- 0
-- 3
-- 1
-- 4
-- 2
-- 5
-- UnsafeArray [2,3] [(),(),(),(),(),()]
traverses ::
  (Applicative f) =>
  Dims ->
  (a -> f b) ->
  Array a ->
  f (Array b)
traverses ds f a = joins ds <$> traverse (traverse f) (extracts ds a)

-- | Maps a function along specified dimensions.
--
-- >>> pretty $ maps [1] transpose a
-- [[[0,12],
--   [4,16],
--   [8,20]],
--  [[1,13],
--   [5,17],
--   [9,21]],
--  [[2,14],
--   [6,18],
--   [10,22]],
--  [[3,15],
--   [7,19],
--   [11,23]]]
maps ::
  Dims ->
  (Array a -> Array b) ->
  Array a ->
  Array b
maps ds f a = joins ds (fmap f (extracts ds a))

-- | Filters along specified dimensions (which are flattened).
--
-- >>> pretty $ filters [0,1] (any ((==0) . (`mod` 7))) a
-- [[0,1,2,3],
--  [4,5,6,7],
--  [12,13,14,15],
--  [20,21,22,23]]
filters ::
  Dims ->
  (Array a -> Bool) ->
  Array a ->
  Array a
filters ds p a = join (asArray $ V.filter p $ asVector (extracts ds a))

-- | Zips two arrays with a function along specified dimensions.
--
-- >>> pretty $ zips [0,1] (zipWith (,)) a (reverses [0] a)
-- [[[(0,12),(1,13),(2,14),(3,15)],
--   [(4,16),(5,17),(6,18),(7,19)],
--   [(8,20),(9,21),(10,22),(11,23)]],
--  [[(12,0),(13,1),(14,2),(15,3)],
--   [(16,4),(17,5),(18,6),(19,7)],
--   [(20,8),(21,9),(22,10),(23,11)]]]
zips ::
  Dims ->
  (Array a -> Array b -> Array c) ->
  Array a ->
  Array b ->
  Array c
zips ds f a b = joins ds (zipWith f (extracts ds a) (extracts ds b))

-- | Zips two arrays with a function along specified dimensions, checking shapes.
--
-- >>> zipsSafe [0] (zipWith (,)) (asArray [1::Int]) (asArray [1,2::Int])
-- Nothing
zipsSafe ::
  Dims ->
  (Array a -> Array b -> Array c) ->
  Array a ->
  Array b ->
  Maybe (Array c)
zipsSafe ds f a b =
  bool
    (Just $ joins ds (zipWith f (extracts ds a) (extracts ds b)))
    Nothing
    (shape a /= (shape b :: [Int]))

-- | Modify using the supplied function along dimensions & positions.
--
-- >>> pretty $ modifies (fmap (100+)) [2] [0] a
-- [[[100,1,2,3],
--   [104,5,6,7],
--   [108,9,10,11]],
--  [[112,13,14,15],
--   [116,17,18,19],
--   [120,21,22,23]]]
modifies ::
  (Array a -> Array a) ->
  Dims ->
  [Int] ->
  Array a ->
  Array a
modifies f ds ps a = joins ds $ modify ps f (extracts ds a)

-- | Apply a binary function between successive slices, across dimensions and lags.
--
-- >>> pretty $ diffs [1] [1] (zipWith (-)) a
-- [[[4,4,4,4],
--   [4,4,4,4]],
--  [[4,4,4,4],
--   [4,4,4,4]]]
diffs :: Dims -> [Int] -> (Array a -> Array a -> Array b) -> Array a -> Array b
diffs ds xs f a = zips ds f (drops ds xs a) (drops ds (fmap P.negate xs) a)

-- | Product two arrays using the supplied binary function.
--
-- For context, if the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a [tensor product](https://en.wikipedia.org/wiki/Tensor_product).
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote the wiki article:
--
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
--
-- >>> x = array [3] [1,2,3]
-- >>> pretty $ expand (*) x x
-- [[1,2,3],
--  [2,4,6],
--  [3,6,9]]
--
-- Alternatively, expand can be understood as representing the permutation of element pairs of two arrays, so like the Applicative List instance.
--
-- >>> i2 = indices [2,2]
-- >>> pretty $ expand (,) i2 i2
-- [[[[([0,0],[0,0]),([0,0],[0,1])],
--    [([0,0],[1,0]),([0,0],[1,1])]],
--   [[([0,1],[0,0]),([0,1],[0,1])],
--    [([0,1],[1,0]),([0,1],[1,1])]]],
--  [[[([1,0],[0,0]),([1,0],[0,1])],
--    [([1,0],[1,0]),([1,0],[1,1])]],
--   [[([1,1],[0,0]),([1,1],[0,1])],
--    [([1,1],[1,0]),([1,1],[1,1])]]]]
expand ::
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array c
expand f a b = tabulate (shape a <> shape b) (\i -> f (index a (List.take r i)) (index b (List.drop r i)))
  where
    r = rank a

-- | Like expand, but permutes the first array first, rather than the second.
--
-- >>> pretty $ expand (,) v (fmap (+3) v)
-- [[(0,3),(0,4),(0,5)],
--  [(1,3),(1,4),(1,5)],
--  [(2,3),(2,4),(2,5)]]
--
-- >>> pretty $ coexpand (,) v (fmap (+3) v)
-- [[(0,3),(1,3),(2,3)],
--  [(0,4),(1,4),(2,4)],
--  [(0,5),(1,5),(2,5)]]
coexpand ::
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array c
coexpand f a b = tabulate (shape a <> shape b) (\i -> f (index a (List.drop r i)) (index b (List.take r i)))
  where
    r = rank a

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2.
--
--
-- >>> pretty $ contract [1,2] sum (expand (*) m (transpose m))
-- [[5,14],
--  [14,50]]
contract ::
  Dims ->
  (Array a -> b) ->
  Array a ->
  Array b
contract ds f a = f . diag <$> extracts (exceptDims ds (shape a)) a

-- | Product two arrays using the supplied function and then contract the result using the supplied matching dimensions and function.
--
-- >>> pretty $ prod [1] [0] sum (*) (range [2,3]) (range [3,2])
-- [[10,13],
--  [28,40]]
--
-- With full laziness, this computation would be equivalent to:
--
-- > f . diag <$> extracts ds' (expand g a b)
prod ::
  Dims ->
  Dims ->
  (Array c -> d) ->
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array d
prod ds0 ds1 g f a b = tabulate (S.deleteDims ds0 (shape a) <> S.deleteDims ds1 (shape b)) (\so -> g $ tabulate (S.getDims ds0 (shape a)) (\si -> f (index a (S.insertDims ds0 si (List.take sp so))) (index b (S.insertDims ds1 si (List.drop sp so)))))
  where
    sp = rank a - S.rank ds0

-- | A generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
--
-- matrix multiplication
--
-- >>> pretty $ dot sum (*) m (transpose m)
-- [[5,14],
--  [14,50]]
--
-- inner product
--
-- >>> pretty $ dot sum (*) v v
-- 5
--
-- matrix-vector multiplication
-- Note that an Array with shape [3] is neither a row vector nor column vector.
--
-- >>> pretty $ dot sum (*) v (transpose m)
-- [5,14]
--
-- >>> pretty $ dot sum (*) m v
-- [5,14]
dot ::
  (Array c -> d) ->
  (a -> b -> c) ->
  Array a ->
  Array b ->
  Array d
dot f g a b = contract [r - 1, r] f (expand g a b)
  where
    r = rank a

-- | Array multiplication.
--
-- matrix multiplication
--
-- >>> pretty $ mult m (transpose m)
-- [[5,14],
--  [14,50]]
--
-- inner product
--
-- >>> pretty $ mult v v
-- 5
--
-- matrix-vector multiplication
--
-- >>> pretty $ mult v (transpose m)
-- [5,14]
--
-- >>> pretty $ mult m v
-- [5,14]
mult ::
  (Num a) =>
  Array a ->
  Array a ->
  Array a
mult = dot sum (*)

-- | @windows xs@ are xs-sized windows of an array
--
-- >>> shape $ windows [2,2] (range [4,3,2])
-- [3,2,2,2,2]
windows :: [Int] -> Array a -> Array a
windows xs a = backpermute (expandWindows xs) (indexWindows (S.rank xs)) a

-- | Find the starting positions of occurences of one array in another.
--
-- >>> a = cycle [4,4] (range [3]) :: Array Int
-- >>> i = array [2,2] [1,2,2,0] :: Array Int
-- >>> pretty $ find i a
-- [[False,True,False],
--  [True,False,False],
--  [False,False,True]]
find :: (Eq a) => Array a -> Array a -> Array Bool
find i a = xs
  where
    i' = rerank (rank a) i
    ws = windows (shape i') a
    xs = fmap (== i') (extracts (dimWindows (expandWindows (shape i') (shape a)) (shape a)) ws)

-- | Find the ending positions of one array in another except where the array overlaps with another copy.
--
-- >>> a = konst [5,5] 1 :: Array Int
-- >>> i = konst [2,2] 1 :: Array Int
-- >>> pretty $ findNoOverlap i a
-- [[True,False,True,False],
--  [False,False,False,False],
--  [True,False,True,False],
--  [False,False,False,False]]
findNoOverlap :: (Eq a) => Array a -> Array a -> Array Bool
findNoOverlap i a = r
  where
    f :: Array Bool
    f = find i a

    cl :: [Int] -> [[Int]]
    cl sh = List.filter (P.not . any (> 0) . List.init) $ List.filter (P.not . all (>= 0)) $ arrayAs $ tabulate ((\x -> 2 * x - 1) <$> sh) (\s -> List.zipWith (\x x0 -> x - x0 + 1) s sh)
    go r' s = index f s && not (any (index r') (List.filter (\x -> isFins x (shape f)) $ fmap (List.zipWith (+) s) (cl (shape i))))
    r = tabulate (shape f) (go r)

-- | Find the indices of the starting location of one array in another.
--
-- >>> b = cycle [4,4] (range [3]) :: Array Int
-- >>> i = array [2,2] [1,2,2,0] :: Array Int
-- >>> pretty $ findIndices i b
-- [[0,1],[1,0],[2,2]]
findIndices :: (Eq a) => Array a -> Array a -> Array [Int]
findIndices i a = fmap fst $ vectorAs $ V.filter snd $ asVector $ imap (,) b
  where
    b = find i a

-- | Check if the first array is a prefix of the second
--
-- >>> isPrefixOf (array [2,2] [0,1,4,5]) a
-- True
isPrefixOf :: (Eq a) => Array a -> Array a -> Bool
isPrefixOf p a = p == cut (shape p) a

-- | Check if the first array is a suffix of the second
--
-- >>> isSuffixOf (array [2,2] [18,19,22,23]) a
-- True
isSuffixOf :: (Eq a) => Array a -> Array a -> Bool
isSuffixOf p a = p == cutSuffix (shape p) a

-- | Check if the first array is an infix of the second
--
-- >>> isInfixOf (array [2,2] [18,19,22,23]) a
-- True
isInfixOf :: (Eq a) => Array a -> Array a -> Bool
isInfixOf p a = or $ find p a

-- * shape manipulation

-- | Fill an array with the supplied value without regard to the original shape or cut the array values to match array size.
--
-- > validate (def x a) == True
--
-- >>> pretty $ fill 0 (array [3] [])
-- [0,0,0]
-- >>> pretty $ fill 0 (array [3] [1..4])
-- [1,2,3]
fill :: a -> Array a -> Array a
fill x (UnsafeArray s v) = UnsafeArray s (V.take (S.size s) (v <> V.replicate (S.size s - V.length v) x))

-- | Cut an array to form a new (smaller) shape. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> cut [2] (array [4] [0..3] :: Array Int)
-- UnsafeArray [2] [0,1]
cut ::
  [Int] ->
  Array a ->
  Array a
cut s' a = bool (error "bad cut") (tabulate s' (index a')) (isSubset s' (shape a))
  where
    a' = rerank (S.rank s') a

-- | Cut an array to form a new (smaller) shape, using suffix elements. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> cutSuffix [2,2] a
-- UnsafeArray [2,2] [18,19,22,23]
cutSuffix ::
  [Int] ->
  Array a ->
  Array a
cutSuffix s' a = bool (error "bad cut") (tabulate s' (index a' . List.zipWith (+) diffDim)) (isSubset s' (shape a))
  where
    a' = rerank (S.rank s') a
    diffDim = List.zipWith (-) (shape a') s'

-- | Pad an array to form a new shape, supplying a default value for elements outside the shape of the old array. The old array is reranked to the rank of the new shape first.
--
-- >>> pad 0 [5] (array [4] [0..3] :: Array Int)
-- UnsafeArray [5] [0,1,2,3,0]
pad ::
  a ->
  [Int] ->
  Array a ->
  Array a
pad d s' a = tabulate s' (\s -> bool d (index a' s) (s `isFins` shape a'))
  where
    a' = rerank (S.rank s') a

-- | Left pad an array to form a new shape, supplying a default value for elements outside the shape of the old array.
--
-- >>> lpad 0 [5] (array [4] [0..3] :: Array Int)
-- UnsafeArray [5] [0,0,1,2,3]
-- >>> pretty $ lpad 0 [3,3] (range [2,2] :: Array Int)
-- [[0,0,0],
--  [0,0,1],
--  [0,2,3]]
lpad ::
  a ->
  [Int] ->
  Array a ->
  Array a
lpad d s' a = tabulate s' (\s -> bool d (index a' (olds s)) (olds s `S.isFins` shape a'))
  where
    a' = rerank (S.rank s') a
    gap = List.zipWith (-) s' (shape a')
    olds s = List.zipWith (-) s gap

-- | Reshape an array (with the same or less number of elements).
--
-- >>> pretty $ reshape [4,3,2] a
-- [[[0,1],
--   [2,3],
--   [4,5]],
--  [[6,7],
--   [8,9],
--   [10,11]],
--  [[12,13],
--   [14,15],
--   [16,17]],
--  [[18,19],
--   [20,21],
--   [22,23]]]
reshape ::
  [Int] ->
  Array a ->
  Array a
reshape s a = backpermute (const s) (shapen (shape a) . flatten s) a

-- | Make an Array single dimensional.
--
-- >>> pretty $ flat (range [2,2])
-- [0,1,2,3]
-- >>> pretty (flat $ toScalar 0)
-- [0]
flat :: Array a -> Array a
flat a = unsafeModifyShape (pure . S.size) a

-- | Reshape an array, repeating the original array. The shape of the array should be a suffix of the new shape.
--
-- >>> pretty $ repeat [2,2,2] (array [2] [1,2])
-- [[[1,2],
--   [1,2]],
--  [[1,2],
--   [1,2]]]
--
-- > repeat ds (toScalar x) == konst ds x
repeat ::
  [Int] ->
  Array a ->
  Array a
repeat s a = backpermute (const s) (List.drop (S.rank s - rank a)) a

-- | Reshape an array, cycling through the elements without regard to the original shape.
--
-- >>> pretty $ cycle [2,2,2] (array [3] [1,2,3])
-- [[[1,2],
--   [3,1]],
--  [[2,3],
--   [1,2]]]
cycle ::
  [Int] ->
  Array a ->
  Array a
cycle s a = backpermute (const s) (shapen (shape a) . (`mod` size a) . flatten s) a

-- | Change rank by adding new dimensions at the front, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> shape (rerank 4 a)
-- [1,2,3,4]
-- >>> shape (rerank 2 a)
-- [6,4]
--
-- > flat == rerank 1
rerank :: Int -> Array a -> Array a
rerank r a = unsafeModifyShape (S.rerank r) a

-- | Change the order of dimensions.
--
-- >>> pretty $ reorder [2,0,1] a
-- [[[0,4,8],
--   [12,16,20]],
--  [[1,5,9],
--   [13,17,21]],
--  [[2,6,10],
--   [14,18,22]],
--  [[3,7,11],
--   [15,19,23]]]
reorder ::
  Dims ->
  Array a ->
  Array a
reorder ds a = backpermute (`S.reorder` ds) (\s -> insertDims ds s []) a

-- | Remove single dimensions.
--
-- >>> let sq = array [2,1,3,4,1] [1..24] :: Array Int
-- >>> shape $ squeeze sq
-- [2,3,4]
--
-- >>> shape $ squeeze (singleton 0)
-- []
squeeze ::
  Array a ->
  Array a
squeeze a = unsafeModifyShape S.squeeze a

-- | Insert a single dimension at the supplied position.
--
-- >>> shape $ elongate 1 a
-- [2,1,3,4]
-- >>> elongate 0 (toScalar 1)
-- UnsafeArray [1] [1]
elongate ::
  Dim ->
  Array a ->
  Array a
elongate d a = unsafeModifyShape (insertDim d 1) a

-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
-- >>> pretty $ transpose (array [2,2,2] [1..8])
-- [[[1,5],
--   [3,7]],
--  [[2,6],
--   [4,8]]]
transpose :: Array a -> Array a
transpose a = backpermute List.reverse List.reverse a

-- | Inflate an array by inserting a new dimension given a supplied dimension and size.
--
-- alt name: replicate
--
-- >>> pretty $ inflate 0 2 (array [3] [0,1,2])
-- [[0,1,2],
--  [0,1,2]]
inflate ::
  Dim ->
  Int ->
  Array a ->
  Array a
inflate d n a = backpermute (insertDim d n) (deleteDim d) a

-- | Intercalate an array along dimensions.
--
-- >>> pretty $ intercalate 2 (konst [2,3] 0) a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intercalate :: Dim -> Array a -> Array a -> Array a
intercalate d i a = joins [d] $ asArray (List.intersperse i (arrayAs (extracts [d] a)))

-- | Intersperse an element along dimensions.
--
-- >>> pretty $ intersperse 2 0 a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intersperse :: Dim -> a -> Array a -> Array a
intersperse d i a = intercalate d (konst (deleteDim d (shape a)) i) a

-- | Concatenate and replace dimensions, creating a new dimension at the supplied postion.
--
-- >>> pretty $ concats [0,1] 1 a
-- [[0,4,8,12,16,20],
--  [1,5,9,13,17,21],
--  [2,6,10,14,18,22],
--  [3,7,11,15,19,23]]
concats ::
  Dims ->
  Int ->
  Array a ->
  Array a
concats ds n a = backpermute (concatDims ds n) (unconcatDimsIndex ds n (shape a)) a

-- | Reverses element order along specified dimensions.
--
-- >>> pretty $ reverses [0,1] a
-- [[[20,21,22,23],
--   [16,17,18,19],
--   [12,13,14,15]],
--  [[8,9,10,11],
--   [4,5,6,7],
--   [0,1,2,3]]]
reverses ::
  Dims ->
  Array a ->
  Array a
reverses ds a = backpermute id (reverseIndex ds (shape a)) a

-- | Rotate an array by/along dimensions & offsets.
--
-- >>> pretty $ rotates [1] [2] a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotates ::
  Dims ->
  [Int] ->
  Array a ->
  Array a
rotates ds rs a = backpermute id (rotatesIndex ds rs (shape a)) a

-- * sorting

-- | Sort an array along the supplied dimensions.
--
-- >>> sorts [0] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [1,4,2,3]
-- >>> sorts [1] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
-- >>> sorts [0,1] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [1,2,3,4]
sorts :: (Ord a) => Dims -> Array a -> Array a
sorts ds a = joins ds $ unsafeModifyVector sortV (extracts ds a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> sortsBy [0] (fmap Down) (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
sortsBy :: (Ord b) => Dims -> (Array a -> Array b) -> Array a -> Array a
sortsBy ds c a = joins ds $ unsafeModifyVector (sortByV c) (extracts ds a)

-- | The indices into the array if it were sorted along the dimensions supplied.
--
-- >>> orders [0] (array [2,2] [2,3,1,4])
-- UnsafeArray [2] [1,0]
orders :: (Ord a) => Dims -> Array a -> Array Int
orders ds a = unsafeModifyVector orderV (extracts ds a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> ordersBy [0] (fmap Down) (array [2,2] [2,3,1,4])
-- UnsafeArray [2] [0,1]
ordersBy :: (Ord b) => Dims -> (Array a -> Array b) -> Array a -> Array Int
ordersBy ds c a = unsafeModifyVector (orderByV c) (extracts ds a)

-- * transmission

-- | Apply a binary array function to two arrays with matching shapes across the supplied dimensions. No check on shapes.
--
-- >>> a = array [2,3] [0..5]
-- >>> b = array [3] [0..2]
-- >>> pretty $ telecasts [1] [0] (concatenate 0) a b
-- [[0,1,2],
--  [3,4,5],
--  [0,1,2]]
telecasts :: Dims -> Dims -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
telecasts dsa dsb f a b = zipWith f (extracts dsa a) (extracts dsb b) & joins dsa

-- | Apply a binary array function to two arrays with matching shapes across the supplied dimensions. Checks shape.
--
-- >>> a = array [2,3] [0..5]
-- >>> b = array [1] [1]
-- >>> telecastsSafe [0] [0] (zipWith (+)) a b
-- Nothing
telecastsSafe :: Dims -> Dims -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Maybe (Array c)
telecastsSafe dsa dsb f a b =
  bool
    (Just $ telecasts dsa dsb f a b)
    Nothing
    (shape (extracts dsa a) /= (shape (extracts dsb b) :: [Int]))

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array. No checks on shape.
--
-- >>> a = array [2,3] [0..5]
-- >>> pretty $ transmit (zipWith (+)) (toScalar 1) a
-- [[1,2,3],
--  [4,5,6]]
transmit :: (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
transmit f a b = maps ds (f a) b
  where
    ds = [(rank a) .. (rank b - 1)]

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array. Checks shape.
--
-- >>> a = array [2,3] [0..5]
-- >>> transmitSafe (zipWith (+)) (array [3] [1,2,3]) a
-- Nothing
transmitSafe :: (Array a -> Array b -> Array c) -> Array a -> Array b -> Maybe (Array c)
transmitSafe f a b = bool Nothing (Just $ transmit f a b) (shape a `List.isPrefixOf` shape b)

-- | Transmit an operation if the first array is a prefix of the second or vice versa.
--
-- >>> pretty $ transmitOp (*) a (asArray [1,2])
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[24,26,28,30],
--   [32,34,36,38],
--   [40,42,44,46]]]
transmitOp :: (a -> b -> c) -> Array a -> Array b -> Array c
transmitOp f a b
  | shape a == shape b = zipWith f a b
  | shape a `List.isPrefixOf` shape b = transmit (zipWith f) a b
  | shape b `List.isPrefixOf` shape a = transmit (zipWith (flip f)) b a
  | otherwise = error "bad shapes"

-- | Vector specialisation of 'range'
--
-- >>> iota 5
-- UnsafeArray [5] [0,1,2,3,4]
iota :: Int -> Array Int
iota n = range [n]

-- * row (first dimension) specializations

-- | Add a new row
--
-- >>> pretty $ cons (array [2] [0,1]) (array [2,2] [2,3,4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
cons :: Array a -> Array a -> Array a
cons = prepend 0

-- | split an array into the first row and the remaining rows.
--
-- >>> uncons (array [3,2] [0..5])
-- (UnsafeArray [2] [0,1],UnsafeArray [2,2] [2,3,4,5])
uncons :: Array a -> (Array a, Array a)
uncons a = (heads [0] a', tails [0] a')
  where
    a' = asSingleton a

-- | Convenience pattern for row extraction and consolidation at the beginning of an Array.
--
-- >>> (x:<xs) = array [4] [0..3]
-- >>> x
-- UnsafeArray [] [0]
-- >>> xs
-- UnsafeArray [3] [1,2,3]
-- >>> (x:<xs)
-- UnsafeArray [4] [0,1,2,3]
pattern (:<) :: Array a -> Array a -> Array a
pattern x :< xs <- (uncons -> (x, xs))
  where
    x :< xs = cons x xs

infix 5 :<

{-# COMPLETE (:<) :: Array #-}

-- | Add a new row at the end
--
-- >>> pretty $ snoc (array [2,2] [0,1,2,3]) (array [2] [4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
snoc :: Array a -> Array a -> Array a
snoc = append 0

-- | split an array into the initial rows and the last row.
--
-- >>> unsnoc (array [3,2] [0..5])
-- (UnsafeArray [2,2] [0,1,2,3],UnsafeArray [2] [4,5])
unsnoc :: Array a -> (Array a, Array a)
unsnoc a = (inits [0] a', lasts [0] a')
  where
    a' = asSingleton a

-- | Convenience pattern for row extraction and consolidation at the end of an Array.
--
-- >>> (xs:>x) = array [4] [0..3]
-- >>> x
-- UnsafeArray [] [3]
-- >>> xs
-- UnsafeArray [3] [0,1,2]
-- >>> (xs:>x)
-- UnsafeArray [4] [0,1,2,3]
pattern (:>) :: Array a -> Array a -> Array a
pattern xs :> x <- (unsnoc -> (xs, x))
  where
    xs :> x = snoc xs x

infix 5 :>

{-# COMPLETE (:>) :: Array #-}

-- * Math

-- | Generate an array of uniform random variates between a range.
--
-- >>> import System.Random.Stateful hiding (uniform)
-- >>> g <- newIOGenM (mkStdGen 42)
-- >>> u <- uniform g [2,3,4] (0,9 :: Int)
-- >>> pretty u
-- [[[0,7,0,2],
--   [1,7,4,2],
--   [5,9,8,2]],
--  [[9,8,1,0],
--   [2,2,8,2],
--   [2,8,0,6]]]
uniform :: (StatefulGen g m, UniformRange a) => g -> [Int] -> (a, a) -> m (Array a)
uniform g ds r = do
  v <- V.replicateM (S.size ds) (uniformRM r g)
  pure $ UnsafeArray ds v

-- | Inverse of a square matrix.
--
-- >>> e = array [3,3] [4,12,-16,12,37,-43,-16,-43,98] :: Array Double
-- >>> pretty (inverse e)
-- [[49.36111111111111,-13.555555555555554,2.1111111111111107],
--  [-13.555555555555554,3.7777777777777772,-0.5555555555555555],
--  [2.1111111111111107,-0.5555555555555555,0.1111111111111111]]
--
-- > mult (inverse a) a == a
inverse :: (Floating a) => Array a -> Array a
inverse a = mult (invtri (transpose (chol a))) (invtri (chol a))

-- | [Inversion of a Triangular Matrix](https://math.stackexchange.com/questions/1003801/inverse-of-an-invertible-upper-triangular-matrix-of-order-3)
--
-- >>> t = array [3,3] ([1,0,1,0,1,2,0,0,1] :: [Double]) :: Array Double
-- >>> pretty (invtri t)
-- [[1.0,0.0,-1.0],
--  [0.0,1.0,-2.0],
--  [0.0,0.0,1.0]]
-- >>> ident (shape t) == mult t (invtri t)
-- True
invtri :: (Fractional a) => Array a -> Array a
invtri a = i
  where
    ti = undiag (fmap recip (diag a))
    tl = zipWith (-) a (undiag (diag a))
    l = fmap negate (dot sum (*) ti tl)
    pow xs x = foldr ($) (ident (shape xs)) (replicate x (mult xs))
    zero' = konst (shape a) 0
    add = zipWith (+)
    sum' = foldl' add zero'
    i = mult (sum' (fmap (pow l) (range [n]))) ti
    n = S.getDim 0 (shape a)

-- | Cholesky decomposition using the <https://en.wikipedia.org/wiki/Cholesky_decomposition#The_Cholesky_algorithm Cholesky-Crout> algorithm.
--
-- >>> e = array [3,3] [4,12,-16,12,37,-43,-16,-43,98] :: Array Double
-- >>> pretty (chol e)
-- [[2.0,0.0,0.0],
--  [6.0,1.0,0.0],
--  [-8.0,5.0,3.0]]
-- >>> mult (chol e) (transpose (chol e)) == e
-- True
chol :: (Floating a) => Array a -> Array a
chol a =
  let l =
        tabulate
          (shape a)
          ( \[i, j] ->
              bool
                ( 1
                    / index l [j, j]
                    * ( index a [i, j]
                          - sum
                            ( (\k -> index l [i, k] * index l [j, k])
                                <$> [0 .. (j - 1)]
                            )
                      )
                )
                ( sqrt
                    ( index a [i, i]
                        - sum
                          ( (\k -> index l [j, k] ^ (2 :: Int))
                              <$> [0 .. (j - 1)]
                          )
                    )
                )
                (i == j)
          )
   in l
