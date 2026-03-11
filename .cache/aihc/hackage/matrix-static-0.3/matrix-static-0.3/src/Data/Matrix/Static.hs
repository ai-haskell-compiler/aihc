{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-|
Module      : Data.Matrix.Static
Description : Wrapper around matrix that adds matrix sizes to the type-level
Copyright   : (c) Wanja Chresta, 2018
License     : BSD-3
Maintainer  : wanja dot hs at chrummibei dot ch
Stability   : experimental
Portability : POSIX

Data.Matrix.Static wraps @matrix@'s Data.Matrix functions and adds size
information on the type level. The name of the functions are mostly the same as
in @Data.Matrix@. Exceptions are, when there is a safer version of a function
due to the additional type-level information. In that case, there may be an
unsafe variant of the function with the postfix @Unsafe@.
-}

module Data.Matrix.Static (
    -- * Matrix type
    Matrix, prettyMatrix
  , nrows, ncols
  , forceMatrix
    -- * Builders
  , matrix
  , RowVector
  , rowVector
  , ColumnVector
  , colVector
    -- ** Special matrices
  , zero
  , identity
  , diagonal, diagonalUnsafe
  , permMatrix, permMatrixUnsafe
    -- * List conversions
  , fromList, fromListUnsafe, fromLists, fromListsUnsafe
  , toList, toLists
    -- * Accessing
  , getElem, (!), unsafeGet, (!.), safeGet, safeSet
  , getRow, getCol
#if MIN_VERSION_matrix(0,3,6)
  , safeGetRow, safeGetCol
#endif
  , getDiag
  , getMatrixAsVector
    -- * Manipulating matrices
  , (.*), (^*)
  , setElem
  , unsafeSet
  , transpose, setSize, extendTo
  , inverse, rref
  , mapRow, mapRowUnsafe, mapCol, mapColUnsafe
#if MIN_VERSION_matrix(0,3,6)
  , mapPos
#endif
    -- * Submatrices
    -- ** Splitting blocks
  , submatrix, submatrixUnsafe
  , minorMatrix, minorMatrixUnsafe
  , splitBlocks
   -- ** Joining blocks
  , (<|>) , (<->)
  , joinBlocks
    -- * Matrix operations
  , elementwise
    -- * Matrix multiplication
    -- ** About matrix multiplication
    -- $mult

    -- ** Functions
  , multStd
  , multStd2
  , multStrassen
  , multStrassenMixed
    -- * Linear transformations
  , scaleMatrix
  , scaleRow, scaleRowUnsafe
  , combineRows, combineRowsUnsafe
  , switchRows, switchRowsUnsafe
  , switchCols, switchColsUnsafe
    -- * Decompositions
  , luDecomp, luDecompUnsafe
  , luDecomp', luDecompUnsafe'
  , cholDecomp
    -- * Properties
  , trace, diagProd
    -- ** Determinants
  , detLaplace
  , detLU
  , flatten
    -- ** Helper functions
  , applyUnary, applyBinary, unpackStatic

  ) where

import Control.Applicative (Applicative(..), liftA2)
import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits
  ( Nat, KnownNat, natVal
  , type (*), type (+), type (-), type (<=))

import qualified Data.Matrix as M
import qualified Data.Semigroup as S
import qualified Data.Vector as V


-- | A matrix over the type @f@ with @m@ rows and @n@ columns. This just wraps
--   the 'Data.Matrix.Static.Matrix' constructor and adds size information to
--   the type
newtype Matrix (m :: Nat) (n :: Nat) (a :: Type) = Matrix (M.Matrix a)
  deriving ( Eq, Functor, Foldable, Traversable , NFData )

#if MIN_VERSION_base(4,10,0)
instance (KnownNat m, KnownNat n, Monoid a) => S.Semigroup (Matrix m n a) where
    (<>) = mappend
#endif

instance (KnownNat m, KnownNat n) => Applicative (Matrix m n) where
  pure a = matrix (const a)
  m <*> n = matrix (\ij -> (m!ij) (n!ij))

instance (KnownNat m, KnownNat n, Monoid a) => Monoid (Matrix m n a) where
  mempty = pure mempty
  mappend = liftA2 mappend

nrows :: forall m n a. KnownNat m => Matrix m n a -> Int
nrows = const m
  where m = fromInteger $ natVal @m Proxy

ncols :: forall m n a. KnownNat n => Matrix m n a -> Int
ncols = const n
  where n = fromInteger $ natVal @n Proxy

instance forall m n f. Show f => Show (Matrix m n f) where
    show (Matrix mat) = M.prettyMatrix mat

instance forall m n f. Ord f => Ord (Matrix m n f) where
    compare x y = toList x `compare` toList y -- TODO: Do not use `toList`?

instance forall f m n. Num f => Num (Matrix m n f) where
    -- Addition of matrices.
    {-# SPECIALIZE (+) :: Matrix m n Double
                       -> Matrix m n Double -> Matrix m n Double #-}
    {-# SPECIALIZE (+) :: Matrix m n Int
                       -> Matrix m n Int -> Matrix m n Int #-}
    {-# SPECIALIZE (+) :: Matrix m n Rational
                       -> Matrix m n Rational -> Matrix m n Rational #-}
    (+) = applyBinary (+)

    -- Substraction of matrices.
    {-# SPECIALIZE (-) :: Matrix m n Double
                       -> Matrix m n Double -> Matrix m n Double #-}
    {-# SPECIALIZE (-) :: Matrix m n Int
                       -> Matrix m n Int -> Matrix m n Int #-}
    {-# SPECIALIZE (-) :: Matrix m n Rational
                       -> Matrix m n Rational -> Matrix m n Rational #-}
    (-) = applyBinary (-)

    (*) = applyBinary (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Matrix . fromInteger

-- | Apply a map function to the unsafe inner matrix type.
applyUnary :: forall m n m' n' a b.
              (M.Matrix a -> M.Matrix b)
           -> Matrix m n a -> Matrix m' n' b
{-# INLINE applyUnary #-}
applyUnary f  = \(Matrix a) -> Matrix $ f a


-- | Transform a binary unstatic function to a binary static function.
applyBinary :: forall m n m' n' m'' n'' a b.
               (M.Matrix a -> M.Matrix a -> M.Matrix b)
            -> Matrix m n a -> Matrix m' n' a -> Matrix m'' n'' b
{-# INLINE applyBinary #-}
applyBinary f = \(Matrix a) -> applyUnary (f a)



-- | Forget static information about a matrix. This converts
--   this converts the 'Matrix' type to @Data.Matrix.Matrix@
unpackStatic :: forall m n a. Matrix m n a -> M.Matrix a
{-# INLINE unpackStatic #-}
unpackStatic (Matrix mat) = mat

-- * Wrapper around @Data.Matrix@'s functions

-- | Display a matrix as a 'String' using the 'Show' instance of its elements.
prettyMatrix :: forall m n a. Show a => Matrix m n a -> String
{-# INLINE prettyMatrix #-}
prettyMatrix = M.prettyMatrix . unpackStatic


-- | /O(rows*cols)/. Similar to 'V.force'. It copies the matrix content
--   dropping any extra memory.
--
--   Useful when using 'submatrix' from a big matrix.
forceMatrix :: forall m n a. Matrix m n a -> Matrix m n a
{-# INLINE forceMatrix #-}
forceMatrix = applyUnary M.forceMatrix


-- | Flatten a matrix of matrices.
flatten :: forall m' n' m n a.
    Matrix m' n' (Matrix m n a) -> Matrix (m'*m) (n'*n) a
{-# INLINE flatten #-}
flatten (Matrix mat) = Matrix $ M.flatten uMat
    where uMat :: M.Matrix (M.Matrix a)
          uMat = fmap unpackStatic mat


-- | /O(rows*cols)/. Map a function over a row.
--   The bounds of the row parameter is not checked and might throw an error.
--   Example:
--
-- >                                ( 1 2 3 )   ( 1 2 3 )
-- >                                ( 4 5 6 )   ( 5 6 7 )
-- > mapRowUnsafe (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 8 9 )
--
mapRowUnsafe :: forall m n a.
                (Int -> a -> a)
             -- ^ Function takes the current column as additional argument.
             -> Int
             -- ^ Row to map.
             -> Matrix m n a -> Matrix m n a
{-# INLINE mapRowUnsafe #-}
mapRowUnsafe f i = applyUnary $ M.mapRow f i


-- | /O(rows*cols)/. Map a function over a row.
--   The row to map is given by a TypeLevel Nat. To use this, use @-XDataKinds@
--   and @-XTypeApplications@.
--   Example:
--
-- >                           ( 1 2 3 )   ( 1 2 3 )
-- >                           ( 4 5 6 )   ( 5 6 7 )
-- > mapRow @2 (\_ x -> x + 1) ( 7 8 9 ) = ( 7 8 9 )
--
mapRow :: forall i m n a. (KnownNat i, KnownNat m, 1 <= i, i <= m)
       => (Int -> a -> a)
       -- ^ Function takes the current column as additional argument.
       -> Matrix m n a -> Matrix m n a
{-# INLINE mapRow #-}
mapRow f = applyUnary $ M.mapRow f i
    where i = fromInteger . natVal $ Proxy @i


-- | /O(rows*cols)/. Map a function over a column.
--   The bounds of the row parameter is not checked and might throw an error.
--   Example:
--
-- >                                ( 1 2 3 )   ( 1 3 3 )
-- >                                ( 4 5 6 )   ( 4 6 6 )
-- > mapColUnsafe (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 9 9 )
--
mapColUnsafe :: forall m n a.
                (Int -> a -> a)
             -- ^ Function takes the current column as additional argument.
             -> Int
             -- ^ Row to map.
             -> Matrix m n a -> Matrix m n a
{-# INLINE mapColUnsafe #-}
mapColUnsafe f j = applyUnary $ M.mapCol f j


-- | /O(rows*cols)/. Map a function over a column.
--   The row to map is given by a TypeLevel Nat. To use this, use @-XDataKinds@
--   and @-XTypeApplications@.
--   Example:
--
-- >                           ( 1 2 3 )   ( 1 3 3 )
-- >                           ( 4 5 6 )   ( 4 6 6 )
-- > mapCol @2 (\_ x -> x + 1) ( 7 8 9 ) = ( 7 9 9 )
--
mapCol :: forall j m n a. (KnownNat j, KnownNat m, 1 <= j, j <= n)
       => (Int -> a -> a)
       -- ^ Function takes the current column as additional argument.
       -> Matrix m n a -> Matrix m n a
{-# INLINE mapCol #-}
mapCol f = applyUnary $ M.mapCol f j
    where j = fromInteger . natVal $ Proxy @j


#if MIN_VERSION_matrix(0,3,6)
-- | /O(rows*cols)/. Map a function over elements.
--   Example:
--
-- >                            ( 1 2 3 )   ( 0 -1 -2 )
-- >                            ( 4 5 6 )   ( 1  0 -1 )
-- > mapPos (\(r,c) _ -> r - c) ( 7 8 9 ) = ( 2  1  0 )
--
--   Only available when used with @matrix >= 0.3.6@!
mapPos :: ((Int, Int) -> a -> b)
          -- ^ Function takes the current Position as additional argument.
       -> Matrix m n a
       -> Matrix m n b
{-# INLINE mapPos #-}
mapPos f = applyUnary (M.mapPos f)
#endif


-- BUILDERS

-- | /O(rows*cols)/. The zero matrix
--   This produces a zero matrix of the size given by the type. Often, the
--   correct dimensions can be inferred by the compiler.
--   If you want a specific size, give a type.
--
--   > zero :: Matrix 2 2 Int
--   > ( 0 0 )
--   > ( 0 0 )
zero :: forall m n a. (Num a, KnownNat n, KnownNat m) => Matrix m n a
{-# INLINE zero #-}
zero = Matrix $ M.zero m n
    where n = fromInteger $ natVal @n Proxy
          m = fromInteger $ natVal @m Proxy


-- | /O(rows*cols)/. Generate a matrix from a generator function.
-- | The elements are 1-indexed, i.e. top-left element is @(1,1)@.
--   Example of usage:
--
-- > matrix (\(i,j) -> 2*i - j) :: Matrix 2 4 Int
-- > ( 1  0 -1 -2 )
-- > ( 3  2  1  0 )
matrix :: forall m n a. (KnownNat m, KnownNat n)
       => ((Int,Int) -> a) -- ^ Generator function
       -> Matrix m n a
{-# INLINE matrix #-}
matrix = Matrix . M.matrix m n
  where
    n = fromInteger $ natVal @n Proxy
    m = fromInteger $ natVal @m Proxy
-- | /O(rows*cols)/. Identity matrix
--
-- > identitiy @n =
-- > ( 1 0 0 ... 0 0 )
-- > ( 0 1 0 ... 0 0 )
-- > (       ...     )
-- > ( 0 0 0 ... 1 0 )
-- > ( 0 0 0 ... 0 1 )
identity :: forall n a. (Num a, KnownNat n) => Matrix n n a
{-# INLINE identity #-}
identity = Matrix $ M.identity n'
    where n' = fromInteger $ natVal @n Proxy

-- | Similar to 'diagonalList', but using 'V.Vector', which
--   should be more efficient.
--   The size of the vector is /not/ checked and will lead to an exception
--   if it's not of size n.
diagonalUnsafe :: forall n a.
            a -- ^ Default element
         -> V.Vector a  -- ^ Diagonal vector
         -> Matrix n n a
{-# INLINE diagonalUnsafe #-}
diagonalUnsafe e = Matrix . M.diagonal e


-- | Similar to 'diagonalList', but using 'V.Vector', which
--   should be more efficient.
--   The size of the vector is /not/ checked and will lead to an exception
--   if it's not of size n.
diagonal :: forall n a. KnownNat n =>
            a -- ^ Default element
         -> V.Vector a  -- ^ Diagonal vector
         -> Maybe (Matrix n n a)
{-# INLINE diagonal  #-}
diagonal e v = if n == length v
                  then Just $ diagonalUnsafe e v
                  else Nothing
  where
    n = fromIntegral $ natVal @n Proxy


-- | Create a matrix from a list of elements.
--   The list must have exactly length @n*m@ or this returns Nothing.
--   An example:
--
-- > fromList [1..9] :: Maybe (Matrix 3 3 Int)
-- > Just ( 1 2 3 )
-- >      ( 4 5 6 )
-- >      ( 7 8 9 )
fromList :: forall m n a. (KnownNat m, KnownNat n)
         => [a] -> Maybe (Matrix m n a)
{-# INLINE fromList #-}
fromList as = if length as == n*m
                 then Just $ fromListUnsafe as
                 else Nothing
  where n = fromInteger $ natVal @n Proxy
        m = fromInteger $ natVal @m Proxy


-- | Create a matrix from a non-empty list given the desired size.
--   The list must have at least /rows*cols/ elements.
--   An example:
--
-- > fromListUnsafe [1..9] :: Matrix 3 3 Int
-- > ( 1 2 3 )
-- > ( 4 5 6 )
-- > ( 7 8 9 )
fromListUnsafe :: forall m n a. (KnownNat m, KnownNat n)
               => [a] -- ^ List of elements
               -> Matrix m n a
{-# INLINE fromListUnsafe #-}
fromListUnsafe = Matrix . M.fromList m n
  where
    n = fromIntegral $ natVal @n Proxy
    m = fromIntegral $ natVal @m Proxy


-- | Create a matrix from a list of rows. The list must have exactly @m@
--   lists of length @n@. Nothing is returned otherwise
--   Example:
--
-- > fromLists [ [1,2,3]      ( 1 2 3 )
-- >           , [4,5,6]      ( 4 5 6 )
-- >           , [7,8,9] ] =  ( 7 8 9 )
fromLists :: forall m n a. (KnownNat m, KnownNat n)
          => [[a]] -> Maybe (Matrix m n a)
{-# INLINE fromLists #-}
fromLists as = if length as == m && all (\row -> length row == n) as
                  then Just $ fromListsUnsafe as
                  else Nothing
    where n = fromInteger $ natVal @n Proxy
          m = fromInteger $ natVal @m Proxy


-- | Create a matrix from a list of rows. The list must have exactly @m@
--   lists of length @n@. If this does not hold, the resulting Matrix will have
--   different static dimensions that the runtime dimension and will result
--   in hard to debug errors. Use 'fromLists' whenever you're unsure.
--   Example:
--
-- > fromListsUnsafe [ [1,2,3]      ( 1 2 3 )
-- >                 , [4,5,6]      ( 4 5 6 )
-- >                 , [7,8,9] ] =  ( 7 8 9 )
fromListsUnsafe :: [[a]] -> Matrix m n a
{-# INLINE fromListsUnsafe #-}
fromListsUnsafe = Matrix . M.fromLists


-- | Get the elements of a matrix stored in a list.
--
-- >        ( 1 2 3 )
-- >        ( 4 5 6 )
-- > toList ( 7 8 9 ) = [1..9]
toList :: forall m n a. Matrix m n a -> [a]
{-# INLINE toList #-}
toList = M.toList . unpackStatic


-- | Get the elements of a matrix stored in a list of lists,
--   where each list contains the elements of a single row.
--
-- >         ( 1 2 3 )   [ [1,2,3]
-- >         ( 4 5 6 )   , [4,5,6]
-- > toLists ( 7 8 9 ) = , [7,8,9] ]
toLists :: forall m n a. Matrix m n a -> [[a]]
{-# INLINE toLists #-}
toLists = M.toLists . unpackStatic


-- | /O(1)/. Represent a vector as a one row matrix.
rowVector :: forall m a. KnownNat m => V.Vector a -> Maybe (RowVector m a)
rowVector v = if m == V.length v
                 then Just $ rowVectorUnsafe v
                 else Nothing
  where
    m = fromInteger $ natVal @m Proxy


-- | /O(1)/. Represent a vector as a one row matrix.
rowVectorUnsafe :: forall m a. V.Vector a -> RowVector m a
rowVectorUnsafe = Matrix . M.rowVector


-- | /O(1)/. Represent a vector as a one row matrix.
colVector :: forall n a. KnownNat n => V.Vector a -> Maybe (ColumnVector n a)
colVector v = if n == V.length v
                 then Just $ colVectorUnsafe v
                 else Nothing
  where
    n = fromInteger $ natVal @n Proxy


-- | /O(1)/. Represent a vector as a one row matrix.
colVectorUnsafe :: forall n a. V.Vector a -> ColumnVector n a
colVectorUnsafe = Matrix . M.colVector


-- | /O(rows*cols)/. Permutation matrix.
--   The parameters are given as type level Nats. To use this, use @-XDataKinds@
--   and @-XTypeApplications@.
--   The first type parameter gives the matrix' size, the two following
--   give the rows (or columns) to permute.
--
-- > permMatrix @n @i @j =
-- >               i     j       n
-- >   1 ( 1 0 ... 0 ... 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   i ( 0 0 ... 0 ... 1 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   j ( 0 0 ... 1 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >     ( 0 0 ... 0 ... 0 ... 1 0 )
-- >   n ( 0 0 ... 0 ... 0 ... 0 1 )
--
-- When @i == j@ it reduces to 'identity' @n@.
--
permMatrix :: forall n i j a.
  (Num a, KnownNat n, KnownNat i, KnownNat j, 1 <= i, i <= n, 1 <= j, j <= n)
    => Matrix n n a
{-# INLINE permMatrix #-}
permMatrix = permMatrixUnsafe @n i j
  where
    i = fromInteger $ natVal @i Proxy
    j = fromInteger $ natVal @j Proxy

-- | /O(rows*cols)/. Permutation matrix.
--   The values of the row and column identifiers are not checked and if
--   they are out of range (not between 1 and n) an exception will be thrown.
--
-- > permMatrixUnsafe @n i j =
-- >               i     j       n
-- >   1 ( 1 0 ... 0 ... 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   i ( 0 0 ... 0 ... 1 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   j ( 0 0 ... 1 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >     ( 0 0 ... 0 ... 0 ... 1 0 )
-- >   n ( 0 0 ... 0 ... 0 ... 0 1 )
--
-- When @i == j@ it reduces to 'identity' @n@.
--
permMatrixUnsafe :: forall n a. (Num a, KnownNat n)
           => Int -- ^ Permuted row 1.
           -> Int -- ^ Permuted row 2.
           -> Matrix n n a -- ^ Permutation matrix.
{-# INLINE permMatrixUnsafe #-}
permMatrixUnsafe i = Matrix . M.permMatrix n i
  where
    n = fromInteger $ natVal @n Proxy


-------------------------------------------------------
---- ACCESSING
-- | /O(1)/. Get an element of a matrix. Indices range from /(1,1)/ to /(m,n)/.
--   The parameters are given as type level Nats. To use this, use @-XDataKinds@
--   and @-XTypeApplications@.
--
--   The type parameters are: row, column
--
--   Example:
--
-- >               ( 1 2 )
-- > getElem @2 @1 ( 3 4 ) = 3
--
getElem :: forall i j m n a.
  (KnownNat i, KnownNat j, 1 <= i, i <= m, 1 <= j, j <= n)
      => Matrix m n a -- ^ Matrix
      -> a
{-# INLINE getElem #-}
getElem = M.unsafeGet i j . unpackStatic
  where
    i = fromInteger $ natVal @i Proxy
    j = fromInteger $ natVal @j Proxy


-- | /O(1)/. Unsafe variant of 'getElem'. This will do no bounds checking
unsafeGet :: Int      -- ^ Row
          -> Int      -- ^ Column
          -> Matrix m n a -- ^ Matrix
          -> a
{-# INLINE unsafeGet #-}
unsafeGet i j = M.unsafeGet i j . unpackStatic


-- | Short alias for 'unsafeGet'. Careful: This has no bounds checking
--   This deviates from @Data.Matrix@, where (!) does check bounds on runtime.
(!) :: Matrix m n a -> (Int,Int) -> a
{-# INLINE (!) #-}
m ! (i,j) = unsafeGet i j m


-- | Alias for '(!)'. This exists to keep the interface similar to @Data.Matrix@
--   but serves no other purpose. Use '(!)' (or even better 'getElem') instead.
(!.) :: Matrix m n a -> (Int,Int) -> a
{-# INLINE (!.) #-}
m !. (i,j) = unsafeGet i j m


-- | Variant of 'unsafeGet' that returns Maybe instead of an error.
safeGet :: forall m n a. (KnownNat n, KnownNat m)
        => Int -> Int -> Matrix m n a -> Maybe a
{-# INLINE safeGet #-}
safeGet i j mat = if 0 < i && i <= m && 0 < j && j <= n
                   then Just $ unsafeGet i j mat
                   else Nothing
  where
    n = fromInteger $ natVal @n Proxy
    m = fromInteger $ natVal @m Proxy


-- | Variant of 'setElem' that returns Maybe instead of an error.
safeSet :: forall m n a. a -> (Int, Int) -> Matrix m n a -> Maybe (Matrix m n a)
{-# INLINE safeSet #-}
safeSet x ij m = Matrix <$> M.safeSet x ij (unpackStatic m)


-- | /O(1)/. Get a row of a matrix as a vector.
--   The range of the input is not checked and must be between 1 and m
getRow :: Int -> Matrix m n a -> V.Vector a
{-# INLINE getRow #-}
getRow i = M.getRow i . unpackStatic


-- | /O(1)/. Get a column of a matrix as a vector.
--   The range of the input is not checked and must be between 1 and n
getCol :: Int -> Matrix m n a -> V.Vector a
{-# INLINE getCol #-}
getCol i = M.getCol i . unpackStatic


#if MIN_VERSION_matrix(0,3,6)
-- | Varian of 'getRow' that returns a maybe instead of an error
--   Only available when used with @matrix >= 0.3.6@!
safeGetRow :: Int -> Matrix m n a -> Maybe (V.Vector a)
{-# INLINE safeGetRow #-}
safeGetRow i = M.safeGetRow i . unpackStatic


-- | Variant of 'getCol' that returns a maybe instead of an error
--   Only available when used with @matrix >= 0.3.6@!
safeGetCol :: Int -> Matrix m n a -> Maybe (V.Vector a)
{-# INLINE safeGetCol #-}
safeGetCol i = M.safeGetCol i . unpackStatic
#endif


-- | /O(min rows cols)/. Diagonal of a /not necessarily square/ matrix.
getDiag :: Matrix m n a -> V.Vector a
{-# INLINE getDiag #-}
getDiag = M.getDiag . unpackStatic


-- | /O(rows*cols)/. Transform a 'Matrix' to a 'V.Vector' of size /rows*cols/.
--  This is equivalent to get all the rows of the matrix using 'getRow'
--  and then append them, but far more efficient.
getMatrixAsVector :: Matrix m n a -> V.Vector a
{-# INLINE getMatrixAsVector #-}
getMatrixAsVector = M.getMatrixAsVector . unpackStatic


-------------------------------------------------------
---- MANIPULATING MATRICES



-- | Replace the value of a cell in a matrix.
--   The position to be replaced is given by TypeLevel Nats. To use this, use
--   @-XDataKinds@ and @-XTypeApplications@.
--
--   Example:
--   setElem @1 @2 0 (1 2 3) = (1 0 3)
setElem :: forall i j m n a.
  ( KnownNat i, KnownNat j, 1 <= i, i <= m, 1 <= j, j <= n)
        => a -- ^ New value.
        -> Matrix m n a -- ^ Original matrix.
        -> Matrix m n a
           -- ^ Matrix with the given position replaced with the given value.
{-# INLINE setElem #-}
setElem x = applyUnary $ M.setElem x (i,j)
  where
    i = fromInteger $ natVal @i Proxy
    j = fromInteger $ natVal @j Proxy


-- | Unsafe variant of 'setElem', without bounds checking.
unsafeSet :: a -- ^ New value.
        -> (Int,Int) -- ^ Position to replace.
        -> Matrix m n a -- ^ Original matrix.
        -> Matrix m n a 
           -- ^ Matrix with the given position replaced with the given value.
{-# INLINE unsafeSet #-}
unsafeSet x ij = applyUnary $ M.unsafeSet x ij


-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: forall m n a. Matrix m n a -> Matrix n m a
{-# INLINE transpose #-}
transpose = applyUnary M.transpose


-- | /O(rows^4)/. The inverse of a square matrix
--   Uses naive Gaussian elimination formula.
inverse :: forall n a. (Fractional a, Eq a)
        => Matrix n n a -> Either String (Matrix n n a)
{-# INLINE inverse #-}
inverse m = Matrix <$> M.inverse (unpackStatic m)


-- | /O(rows*rows*cols*cols)/. Converts a matrix to reduced row echelon form,
--  thus solving a linear system of equations. This requires that (cols > rows)
--  if cols < rows, then there are fewer variables than equations and the
--  problem cannot be solved consistently. If rows = cols, then it is
--  basically a homogenous system of equations, so it will be reduced to
--  identity or an error depending on whether the marix is invertible
--  (this case is allowed for robustness).
rref :: (Fractional a, Eq a) => Matrix m n a -> Either String (Matrix m n a)
{-# INLINE rref #-}
rref = fmap Matrix . M.rref . unpackStatic


-- | Extend a matrix to the expected size adding a default element.
--   If the matrix already has the required size, nothing happens.
--   Example:
--
-- >                              ( 1 2 3 0 0 )
-- >                  ( 1 2 3 )   ( 4 5 6 0 0 )
-- >                  ( 4 5 6 )   ( 7 8 9 0 0 )
-- > extendTo @4 @5 0 ( 7 8 9 ) = ( 0 0 0 0 0 )
extendTo :: forall newM newN m n a.
  (KnownNat newM, KnownNat newN, n <= newN, m <= newM)
     => a  -- ^ Element to add when extending.
     -> Matrix m n a -> Matrix newM newN a
{-# INLINE extendTo #-}
extendTo = \e -> applyUnary $ M.extendTo e newM newN
  where
    newM = fromInteger $ natVal @newM Proxy
    newN = fromInteger $ natVal @newN Proxy


-- | Set the size of a matrix to given parameters. Use a default element
--   for undefined entries if the matrix has been extended.
setSize :: forall newM newN m n a.
  (KnownNat newM, KnownNat newN, 1 <= newM, 1 <= newN)
    => a   -- ^ Default element.
    -> Matrix m n a
    -> Matrix newM newN a
{-# INLINE setSize #-}
setSize = \e -> applyUnary $ M.setSize e newM newN
  where
    newM = fromInteger $ natVal @newM Proxy
    newN = fromInteger $ natVal @newN Proxy


-- | /O(1)/. Extract a submatrix from the given position.
--   The type parameters expected are the starting and ending indices
--   of row and column elements.
submatrix :: forall iFrom jFrom iTo jTo m n a.
  ( KnownNat iFrom, KnownNat iTo, KnownNat jFrom, KnownNat jTo
  , 1 <= iFrom, 1 <= iTo - iFrom + 1, iTo - iFrom + 1 <= m
  , 1 <= jFrom, 1 <= jTo - jFrom + 1, jTo - jFrom + 1 <= n
  )
    => Matrix m n a -> Matrix (iTo-iFrom+1) (jTo-jFrom+1) a
{-# INLINE submatrix #-}
submatrix = applyUnary $ M.submatrix iFrom iTo jFrom jTo
  where
    iFrom = fromInteger $ natVal @iFrom Proxy
    iTo = fromInteger $ natVal @iTo Proxy
    jFrom = fromInteger $ natVal @jFrom Proxy
    jTo = fromInteger $ natVal @jTo Proxy


-- | /O(1)/. Extract a submatrix from the given position.
--   The type parameters are the dimension of the returned matrix, the run-time
--   indices are the indiced of the top-left element of the new matrix.
--   Example:
--
-- >                           ( 1 2 3 )
-- >                           ( 4 5 6 )   ( 2 3 )
-- > submatrixUnsafe @2 @2 1 2 ( 7 8 9 ) = ( 5 6 )
submatrixUnsafe :: forall rows cols m n a.
  ( KnownNat rows, KnownNat cols
  , 1 <= rows, rows <= m, 1 <= cols, cols <= n )
    => Int -- ^ Starting row
    -> Int -- ^ Starting column
    -> Matrix m n a -> Matrix rows cols a
{-# INLINE submatrixUnsafe #-}
submatrixUnsafe iFrom jFrom =
  applyUnary $ M.submatrix iFrom (iFrom+rows-1) jFrom (jFrom+cols-1)
    where
      rows = fromInteger $ natVal @rows Proxy
      cols = fromInteger $ natVal @cols Proxy


-- | /O(rows*cols)/. Remove a row and a column from a matrix.
--   Example:
--
-- >                       ( 1 2 3 )
-- >                       ( 4 5 6 )   ( 1 3 )
-- > minorMatrixUnsafe 2 2 ( 7 8 9 ) = ( 7 9 )
minorMatrixUnsafe :: (2 <= n, 2 <= m)
                  => Int -- ^ Row @r@ to remove.
                  -> Int -- ^ Column @c@ to remove.
                  -> Matrix m n a -- ^ Original matrix.
                  -> Matrix (m-1) (n-1) a
                     -- ^ Matrix with row @r@ and column @c@ removed.
{-# INLINE minorMatrixUnsafe #-}
minorMatrixUnsafe i j = applyUnary $ M.minorMatrix i j


-- | /O(rows*cols)/. Remove a row and a column from a matrix.
--   Example:
--
-- >                   ( 1 2 3 )
-- >                   ( 4 5 6 )   ( 1 3 )
-- > minorMatrix @2 @2 ( 7 8 9 ) = ( 7 9 )
minorMatrix :: forall delRow delCol m n a.
  ( KnownNat delRow, KnownNat delCol
  , 1 <= delRow, 1 <= delCol, delRow <= m, delCol <= n, 2 <= n, 2 <= m)
    => Matrix m n a -- ^ Original matrix.
    -> Matrix (m-1) (n-1) a
       -- ^ Matrix with row @r@ and column @c@ removed.
{-# INLINE minorMatrix #-}
minorMatrix = applyUnary $ M.minorMatrix delCol delRow
  where
    delCol = fromInteger $ natVal @delCol Proxy
    delRow = fromInteger $ natVal @delRow Proxy


-- | /O(1)/. Make a block-partition of a matrix using a given element as
--   reference. The element will stay in the bottom-right corner of the
--   top-left corner matrix.
--   This means, the ranges of the pivot elements positions are
--   \[ i <- [1..m-1], j <- [1..n-1] \]
--
-- >                   (             )   ( TR   | TL   )
-- >                   (             )   ( ...  | ...  )
-- >                   (    x        )   (    x |      )
-- > splitBlocks @i @j (             ) = (-------------) , where x = a_{i,j}
-- >                   (             )   ( BL   | BR   )
-- >                   (             )   ( ...  | ...  )
-- >                   (             )   (      |      )
--
--   Note that contrary to the @matrix@ version of this function, blocks will
--   never be empty.
--   Also, because of TypeLits not providing proper dependent types, there is
--   no way to have a type safe variant of this functon where the pivot element
--   is given at run-time.
--
splitBlocks :: forall mt nl mb nr a.
  (KnownNat mt, KnownNat nl, 1 <= mt, 1 <= mb, 1 <= nl, 1 <= nr)
    => Matrix (mt+mb) (nl+nr) a -- ^ Matrix to split.
    -> ( Matrix mt nl a, Matrix mt nr a
       , Matrix mb nl a, Matrix mb nr a
       ) -- ^ (TL,TR,BL,BR)
{-# INLINE[1] splitBlocks #-}
splitBlocks mat =
  let mt = fromInteger $ natVal @mt Proxy
      nl = fromInteger $ natVal @nl Proxy
      (x,y,z,w) = M.splitBlocks mt nl $ unpackStatic mat
   in (Matrix x, Matrix y, Matrix z, Matrix w)


-- | Join blocks of the form detailed in 'splitBlocks'. Precisely:
--
-- > joinBlocks (tl,tr,bl,br) =
-- >   (tl <|> tr)
-- >       <->
-- >   (bl <|> br)
joinBlocks :: forall mt mb nl nr a.
  (1 <= mt, 1 <= mb, 1 <= nl, 1 <= nr) =>
     ( Matrix mt nl a, Matrix mt nr a
     , Matrix mb nl a, Matrix mb nr a)
  -> Matrix (mt + mb) (nl + nr) a
{-# INLINE[1] joinBlocks #-}
joinBlocks (Matrix tl, Matrix tr, Matrix bl, Matrix br) =
  Matrix $ M.joinBlocks (tl, tr, bl, br)

{-# RULES
"matrix-static/splitAndJoin"
   forall m. joinBlocks (splitBlocks m) = m
  #-}


-- | Horizontally join two matrices. Visually:
--
-- > ( A ) <|> ( B ) = ( A | B )
(<|>) :: forall m n k a. Matrix m n a -> Matrix m k a -> Matrix m (k+n) a
Matrix x <|> Matrix y = Matrix $ x M.<|> y


-- | Horizontally join two matrices. Visually:
--
-- >                   ( A )
-- > ( A ) <-> ( B ) = ( - )
-- >                   ( B )
(<->) :: forall m k n a. Matrix m n a -> Matrix k n a -> Matrix (m+k) n a
Matrix x <-> Matrix y = Matrix $ x M.<-> y



-- | Type safe matrix multiplication
--   This is called @(*)@ in @matrix@. Since the dimensions of the input
--   matrices differ, they are not the same type and we cannot use @Num@'s @(*)@
(.*) :: forall m k n a. Num a => Matrix m k a -> Matrix k n a -> Matrix m n a
{-# INLINE[1] (.*) #-}
(.*) = applyBinary (*)



-------------------------------------------------------
---- MATRIX OPERATIONS

-- | Type safe scalar multiplication
(^*) :: forall m n a. Num a => a -> Matrix m n a -> Matrix m n a
{-# INLINE (^*) #-}
(^*) x = applyUnary $ M.scaleMatrix x

-- | A row vector (a matrix with one row).
type RowVector = Matrix 1

-- | A column vector (a matrix with one column).
type ColumnVector m = Matrix m 1



-- | Perform an operation element-wise.
--   This uses @matrix@'s 'elementwiseUnsafe' since we can guarantee proper
--   dimensions at compile time.
elementwise :: forall m n a b c.
    (a -> b -> c) -> (Matrix m n a -> Matrix m n b -> Matrix m n c)
elementwise f (Matrix mat) (Matrix mat') =
    Matrix $ M.elementwiseUnsafe f mat mat'

-------------------------------------------------------
---- MATRIX MULTIPLICATION

{- $mult

Four methods are provided for matrix multiplication.

* 'multStd':
     Matrix multiplication following directly the definition.
     This is the best choice when you know for sure that your
     matrices are small.

* 'multStd2':
     Matrix multiplication following directly the definition.
     However, using a different definition from 'multStd'.
     According to our benchmarks with this version, 'multStd2' is
     around 3 times faster than 'multStd'.

* 'multStrassen':
     Matrix multiplication following the Strassen's algorithm.
     Complexity grows slower but also some work is added
     partitioning the matrix. Also, it only works on square
     matrices of order @2^n@, so if this condition is not a)
     met, it is zero-padded until this is accomplished.
     Therefore, its use is not recommended.

* 'multStrassenMixed':
     This function mixes the previous methods.
     It provides a better performance in general. Method @(@'*'@)@
     of the 'Num' class uses this function because it gives the best
     average performance. However, if you know for sure that your matrices are
     small (size less than 500x500), you should use 'multStd' or 'multStd2' instead,
     since 'multStrassenMixed' is going to switch to those functions anyway.

We keep researching how to get better performance for matrix multiplication.
If you want to be on the safe side, use ('*').

-}

-- | Standard matrix multiplication by definition.
multStd :: forall m k n a.
    Num a => Matrix m k a -> Matrix k n a -> Matrix m n a
{-# INLINE multStd #-}
multStd (Matrix a) = applyUnary (M.multStd a)


-- | Standard matrix multiplication by definition.
multStd2 :: forall m k n a.
    Num a => Matrix m k a -> Matrix k n a -> Matrix m n a
{-# INLINE multStd2 #-}
multStd2 (Matrix a) = applyUnary (M.multStd2 a)


-- | Strassen's matrix multiplication.
multStrassen :: forall m k n a.
    Num a => Matrix m k a -> Matrix k n a -> Matrix m n a
{-# INLINE multStrassen #-}
multStrassen (Matrix a) = applyUnary (M.multStrassen a)


-- | Mixed Strassen's matrix multiplication.
multStrassenMixed :: forall m k n a.
    Num a => Matrix m k a -> Matrix k n a -> Matrix m n a
{-# INLINE multStrassenMixed #-}
multStrassenMixed (Matrix a) = applyUnary (M.multStrassenMixed a)


-------------------------------------------------------
---- TRANSFORMATIONS

-- | Scale a matrix by a given factor.
--   Example:
--
-- >               ( 1 2 3 )   (  2  4  6 )
-- >               ( 4 5 6 )   (  8 10 12 )
-- > scaleMatrix 2 ( 7 8 9 ) = ( 14 16 18 )
scaleMatrix :: Num a => a -> Matrix m n a -> Matrix m n a
{-# INLINE[1] scaleMatrix #-}
scaleMatrix a = applyUnary $ M.scaleMatrix a


-- | Scale a row by a given factor. The input row is not checked for validity.
--   Example:
--
-- >                    ( 1 2 3 )   (  1  2  3 )
-- >                    ( 4 5 6 )   ( 12 15 18 )
-- > scaleRowUnsafe 3 2 ( 7 8 9 ) = (  7  8  9 )
scaleRowUnsafe :: Num a => a -> Int -> Matrix m n a -> Matrix m n a
{-# INLINE scaleRowUnsafe #-}
scaleRowUnsafe a i = applyUnary (M.scaleRow a i)


-- | Scale a row by a given factor. The input row is not checked for validity.
--   Example:
--
-- >               ( 1 2 3 )   (  1  2  3 )
-- >               ( 4 5 6 )   ( 12 15 18 )
-- > scaleRow @2 3 ( 7 8 9 ) = (  7  8  9 )
scaleRow :: forall i m n a. (KnownNat i, Num a)
         => a -> Matrix m n a -> Matrix m n a
{-# INLINE scaleRow #-}
scaleRow a = applyUnary (M.scaleRow a i)
  where i = fromInteger $ natVal @i Proxy


-- | Add to one row a scalar multiple of another row.
--   Example:
--
-- >                         ( 1 2 3 )   (  1  2  3 )
-- >                         ( 4 5 6 )   (  6  9 12 )
-- > combineRowsUnsafe 2 2 1 ( 7 8 9 ) = (  7  8  9 )
combineRowsUnsafe :: Num a => Int -> a -> Int -> Matrix m n a -> Matrix m n a
{-# INLINE combineRowsUnsafe #-}
combineRowsUnsafe i a k = applyUnary (M.combineRows i a k)

-- | Add to one row a scalar multiple of another row.
--   Example:
--
-- >                     ( 1 2 3 )   (  1  2  3 )
-- >                     ( 4 5 6 )   (  6  9 12 )
-- > combineRows @2 @1 2 ( 7 8 9 ) = (  7  8  9 )
combineRows :: forall i k m n a. (KnownNat i, KnownNat k, Num a)
            => a -> Matrix m n a -> Matrix m n a
{-# INLINE combineRows #-}
combineRows a = combineRowsUnsafe i a k
  where
    i = fromInteger $ natVal @i Proxy
    k = fromInteger $ natVal @k Proxy

-- | Switch two rows of a matrix.
--   The validity of the input row numbers is not checked
--   Example:
--
-- >                      ( 1 2 3 )   ( 4 5 6 )
-- >                      ( 4 5 6 )   ( 1 2 3 )
-- > switchRowsUnsafe 1 2 ( 7 8 9 ) = ( 7 8 9 )
switchRowsUnsafe :: Int -- ^ Row 1.
                 -> Int -- ^ Row 2.
                 -> Matrix m n a -- ^ Original matrix.
                 -> Matrix m n a -- ^ Matrix with rows 1 and 2 switched.
{-# INLINE switchRowsUnsafe #-}
switchRowsUnsafe i k = applyUnary (M.switchRows i k)


-- | Switch two rows of a matrix.
--   Example:
--
-- >                  ( 1 2 3 )   ( 4 5 6 )
-- >                  ( 4 5 6 )   ( 1 2 3 )
-- > switchRows @1 @2 ( 7 8 9 ) = ( 7 8 9 )
switchRows :: forall i k m n a.
  (KnownNat i, KnownNat k, 1 <= i, i <= m, 1 <= k, k <= m)
    => Matrix m n a -- ^ Original matrix.
    -> Matrix m n a -- ^ Matrix with rows 1 and 2 switched.
{-# INLINE switchRows #-}
switchRows = applyUnary (M.switchRows i k)
  where
    i = fromInteger $ natVal @i Proxy
    k = fromInteger $ natVal @k Proxy


-- | Switch two coumns of a matrix.
--   The validity of the input column numbers is not checked.
--   Example:
--
-- >                      ( 1 2 3 )   ( 2 1 3 )
-- >                      ( 4 5 6 )   ( 5 4 6 )
-- > switchColsUnsafe 1 2 ( 7 8 9 ) = ( 8 7 9 )
switchColsUnsafe :: Int -- ^ Col 1.
                 -> Int -- ^ Col 2.
                 -> Matrix m n a -- ^ Original matrix.
                 -> Matrix m n a -- ^ Matrix with cols 1 and 2 switched.
{-# INLINE switchColsUnsafe #-}
switchColsUnsafe k l = applyUnary (M.switchCols k l)


-- | Switch two coumns of a matrix.
--   Example:
--
-- >                  ( 1 2 3 )   ( 2 1 3 )
-- >                  ( 4 5 6 )   ( 5 4 6 )
-- > switchCols @1 @2 ( 7 8 9 ) = ( 8 7 9 )
switchCols :: forall i k m n a.
  (KnownNat i, KnownNat k, 1 <= i, i <= n, 1 <= k, k <= n)
           => Matrix m n a -- ^ Original matrix.
           -> Matrix m n a -- ^ Matrix with cols 1 and 2 switched.
{-# INLINE switchCols #-}
switchCols = applyUnary (M.switchCols i k)
  where
    i = fromInteger $ natVal @i Proxy
    k = fromInteger $ natVal @k Proxy

-------------------------------------------------------
---- DECOMPOSITIONS

-- LU DECOMPOSITION

-- | Matrix LU decomposition with /partial pivoting/.
--   The result for a matrix /M/ is given in the format /(U,L,P,d)/ where:
--
--   * /U/ is an upper triangular matrix.
--
--   * /L/ is an /unit/ lower triangular matrix.
--
--   * /P/ is a permutation matrix.
--
--   * /d/ is the determinant of /P/.
--
--   * /PM = LU/.
--
--   These properties are only guaranteed when the input matrix is invertible.
--   An additional property matches thanks to the strategy followed for 
--   pivoting:
--
--   * /L_(i,j)/ <= 1, for all /i,j/.
--
--   This follows from the maximal property of the selected pivots, which also
--   leads to a better numerical stability of the algorithm.
--
--   Example:
--
-- >          ( 1 2 0 )     ( 2 0  2 )   (   1 0 0 )   ( 0 0 1 )
-- >          ( 0 2 1 )     ( 0 2 -1 )   ( 1/2 1 0 )   ( 1 0 0 )
-- > luDecomp ( 2 0 2 ) = ( ( 0 0  2 ) , (   0 1 1 ) , ( 0 1 0 ) , 1 )
--
--   'Nothing' is returned if no LU decomposition exists.
luDecomp :: (Ord a, Fractional a)
         => Matrix m n a
         -> Maybe (Matrix m n a, Matrix m n a, Matrix m n a, a)
{-# INLINE luDecomp #-}
luDecomp = fmap packDecomp . M.luDecomp . unpackStatic
  where
    packDecomp (u, l, p, d) = (Matrix u, Matrix l, Matrix p, d)


-- | Unsafe version of 'luDecomp'. It fails when the input matrix is singular.
luDecompUnsafe :: (Ord a, Fractional a)
         => Matrix m n a
         -> (Matrix m n a, Matrix m n a, Matrix m n a, a)
{-# INLINE luDecompUnsafe #-}
luDecompUnsafe =
    fromMaybe (error "luDecompUnsafe of singular matrix") . luDecomp



-- | Matrix LU decomposition with /complete pivoting/.
--   The result for a matrix /M/ is given in the format /(U,L,P,Q,d,e)/ where:
--
--   * /U/ is an upper triangular matrix.
--
--   * /L/ is an /unit/ lower triangular matrix.
--
--   * /P,Q/ are permutation matrices.
--
--   * /d,e/ are the determinants of /P/ and /Q/ respectively.
--
--   * /PMQ = LU/.
--
--   These properties are only guaranteed when the input matrix is invertible.
--   An additional property matches thanks to the strategy followed for
--   pivoting:
--
--   * /L_(i,j)/ <= 1, for all /i,j/.
--
--   This follows from the maximal property of the selected pivots, which also
--   leads to a better numerical stability of the algorithm.
--
--   Example:
--
-- >           ( 1 0 )    ( 2 1 )  (   1    0 0 )  ( 0 0 1 )
-- >           ( 0 2 )    ( 0 2 )  (   0    1 0 )  ( 0 1 0 )  ( 1 0 )
-- > luDecomp' ( 2 1 ) = (( 0 0 ), ( 1/2 -1/4 1 ), ( 1 0 0 ), ( 0 1 ), -1 , 1 )
--
--   'Nothing' is returned if no LU decomposition exists.
luDecomp' :: (Ord a, Fractional a)
         => Matrix m n a
         -> Maybe ( Matrix m n a
                  , Matrix m m a
                  , Matrix m m a
                  , Matrix n n a
                  , a
                  , a)
{-# INLINE luDecomp' #-}
luDecomp' = fmap packDecomp . M.luDecomp' . unpackStatic
  where
    packDecomp (u,l,p,q,d,e) = (Matrix u, Matrix l, Matrix p, Matrix q, d, e)


-- | Unsafe version of 'luDecomp''. It fails when the input matrix is singular.
luDecompUnsafe' :: (Ord a, Fractional a)
    => Matrix m n a
    -> ( Matrix m n a
       , Matrix m m a
       , Matrix m m a
       , Matrix n n a
       , a
       , a)
{-# INLINE luDecompUnsafe' #-}
luDecompUnsafe' =
    fromMaybe (error "luDecompUnsafe of singular matrix") . luDecomp'


-- CHOLESKY DECOMPOSITION

-- | Simple Cholesky decomposition of a symmetric, positive definite matrix.
--   The result for a matrix /M/ is a lower triangular matrix /L/ such that:
--
--   * /M = LL^T/.
--
--   Example:
--
-- >            (  2 -1  0 )   (  1.41  0     0    )
-- >            ( -1  2 -1 )   ( -0.70  1.22  0    )
-- > cholDecomp (  0 -1  2 ) = (  0.00 -0.81  1.15 )
cholDecomp :: (Floating a) => Matrix n n a -> Matrix n n a
{-# INLINE cholDecomp #-}
cholDecomp = applyUnary M.cholDecomp


-------------------------------------------------------
---- PROPERTIES

{-# RULES
"matrix/traceOfScale"
    forall k a. trace (scaleMatrix k a) = k * trace a
  #-}

-- | Sum of the elements in the diagonal. See also 'getDiag'.
--   Example:
--
-- >       ( 1 2 3 )
-- >       ( 4 5 6 )
-- > trace ( 7 8 9 ) = 15
trace :: Num a => Matrix m n a -> a
{-# INLINE[1] trace #-}
trace = M.trace . unpackStatic

-- | Product of the elements in the diagonal. See also 'getDiag'.
--   Example:
--
-- >          ( 1 2 3 )
-- >          ( 4 5 6 )
-- > diagProd ( 7 8 9 ) = 45
diagProd :: Num a => Matrix m n a -> a
{-# INLINE diagProd #-}
diagProd = M.diagProd . unpackStatic

-- DETERMINANT

{-# RULES
"matrix/detLaplaceProduct"
    forall a b. detLaplace (a.*b) = detLaplace a * detLaplace b

"matrix/detLUProduct"
    forall a b. detLU (a.*b) = detLU a * detLU b
  #-}

-- | Matrix determinant using Laplace expansion.
--   If the elements of the 'Matrix' are instance of 'Ord' and 'Fractional'
--   consider to use 'detLU' in order to obtain better performance.
--   Function 'detLaplace' is /extremely/ slow.
detLaplace :: Num a => Matrix n n a -> a
{-# INLINE[1] detLaplace #-}
detLaplace = M.detLaplace . unpackStatic

-- | Matrix determinant using LU decomposition.
--   It works even when the input matrix is singular.
detLU :: (Ord a, Fractional a) => Matrix n n a -> a
{-# INLINE[1] detLU #-}
detLU = M.detLU . unpackStatic

