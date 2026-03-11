{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Matrix.Generic
    (
    -- * Immutable Matrix
      Matrix(..)

    -- * Accessors
    -- ** length information
    , MG.dim
    , MG.rows
    , MG.cols

    -- ** Indexing
    , MG.unsafeIndex
    , (MG.!)
    , MG.takeRow
    , MG.takeColumn
    , MG.takeDiag

    -- * Construction
    , MG.unsafeFromVector
    , MG.fromVector
    , MG.matrix
    , MG.fromList
    , MG.fromLists
    , MG.fromRows
    , fromColumns
    , MG.empty

    -- * Conversions
    , MG.flatten
    , MG.toRows
    , MG.toColumns
    , MG.toList
    , MG.toLists

    -- * Different matrix types
    , convert

    , tr
    , subMatrix
    , ident
    , diag
    , diagRect
    , fromBlocks
    , isSymmetric
    , force

    , Data.Matrix.Generic.foldl

    -- * Mapping
    , Data.Matrix.Generic.map
    , imap

    -- * Monadic mapping
    , mapM
    , imapM
    , mapM_
    , imapM_
    , forM
    , forM_

    -- * Zipping
    , Data.Matrix.Generic.zipWith
    , Data.Matrix.Generic.zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , izipWith
    , izipWith3
    , izipWith4
    , izipWith5
    , izipWith6
    , Data.Matrix.Generic.zip
    , Data.Matrix.Generic.zip3
    , zip4
    , zip5
    , zip6

    -- * Monadic Zipping
    , zipWithM
    , zipWithM_

    -- * Unzipping
    , Data.Matrix.Generic.unzip
    , Data.Matrix.Generic.unzip3
    , unzip4
    , unzip5
    , unzip6

    -- * Monadic sequencing
    , Data.Matrix.Generic.sequence
    , Data.Matrix.Generic.sequence_

    , generate

    -- * Mutable matrix
    , MG.thaw
    , MG.unsafeThaw
    , MG.freeze
    , MG.unsafeFreeze
    , MG.create
    ) where

import           Control.Arrow                     ((&&&), (***))
import           Control.DeepSeq                   hiding (force)
import           Control.Monad                     (foldM, foldM_, liftM)
import qualified Data.Foldable                     as F
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import           Prelude                           hiding (mapM, mapM_)

import           Data.Matrix.Generic.Mutable (MMatrix (..))
import qualified Data.Matrix.Class as MG
import           GHC.Generics                      (Generic)

type instance MG.Mutable Matrix = MMatrix

-- | Row-major matrix supporting efficient slice.
data Matrix v a = Matrix !Int    -- number of rows
                         !Int    -- number of cols
                         !Int    -- physical row dimension
                         !Int    -- offset
                         !(v a)  -- flat matrix
    deriving (Show, Read, Generic)

instance (G.Vector v a, Eq (v a)) => Eq (Matrix v a) where
    (==) m1 m2 = MG.flatten m1 == MG.flatten m2

instance NFData (v a) => NFData (Matrix v a) where
    rnf (Matrix _ _ _ _ vec) = rnf vec

instance G.Vector v a => MG.Matrix Matrix v a where
    -- | O(1) Return the size of matrix.
    dim (Matrix r c _ _ _) = (r,c)
    {-# INLINE dim #-}

    -- | O(1) Unsafe indexing without bound check.
    unsafeIndex (Matrix _ _ tda offset vec) (i,j) = vec `G.unsafeIndex` idx
      where
        idx = offset + i * tda + j
    {-# INLINE unsafeIndex #-}

    -- | O(1) Create matrix from vector.
    unsafeFromVector (r,c) = Matrix r c c 0
    {-# INLINE unsafeFromVector #-}

    -- | O(1) Extract a row.
    unsafeTakeRow (Matrix _ c tda offset vec) i = G.slice i' c vec
      where
        i' = offset + i * tda
    {-# INLINE unsafeTakeRow #-}

    -- | Create a vector by concatenating rows.
    flatten (Matrix r c tda offset vec)
        | c == tda = G.slice offset (r*c) vec
        | otherwise = G.generate (r*c) $ \i ->
            vec `G.unsafeIndex` (offset + (i `div` c) * tda + (i `mod` c))
    {-# INLINE flatten #-}

    thaw (Matrix r c tda offset v) = MMatrix r c tda offset `liftM` G.thaw v
    {-# INLINE thaw #-}

    unsafeThaw (Matrix r c tda offset v) = MMatrix r c tda offset `liftM` G.unsafeThaw v
    {-# INLINE unsafeThaw #-}

    freeze (MMatrix r c tda offset v) = Matrix r c tda offset `liftM` G.freeze v
    {-# INLINE freeze #-}

    unsafeFreeze (MMatrix r c tda offset v) = Matrix r c tda offset `liftM` G.unsafeFreeze v
    {-# INLINE unsafeFreeze #-}

--reshape :: G.Vector v a => Matrix v a -> (Int, Int) -> Matrix v a

-- | O(m*n) Create matrix from columns
fromColumns :: G.Vector v a => [v a] -> Matrix v a
fromColumns = tr . MG.fromRows
{-# INLINE fromColumns #-}

---- | construct upper triangular matrix from vector
--upperTriangular :: (Num a, G.Vector v a) => Int -> v a -> Matrix v a
--upperTriangular n vec =

-- | O(m*n) Convert different matrix type
convert :: (G.Vector v a, G.Vector w a) => Matrix v a -> Matrix w a
convert (Matrix r c tda offset vec) = Matrix r c tda offset . G.convert $ vec
{-# INLINE convert #-}

-- | O(1) Extract sub matrix
subMatrix :: G.Vector v a
          => (Int, Int)  -- ^ upper left corner of the submatrix
          -> (Int, Int)  -- ^ bottom right corner of the submatrix
          -> Matrix v a -> Matrix v a
subMatrix (i,j) (i',j') (Matrix _ _ tda offset vec)
    | m' <= 0 || n' <= 0 = MG.empty
    | otherwise = Matrix m' n' tda offset' vec
  where
    m' = i' - i + 1
    n' = j' - j + 1
    offset' = offset + i * tda + j
{-# INLINE subMatrix #-}

-- | O(m*n) Matrix transpose
tr :: G.Vector v a => Matrix v a -> Matrix v a
tr (Matrix r c tda offset vec) = MG.fromVector (c,r) $ G.generate (r*c) f
  where
    f i = vec G.! (offset + i `mod` r * tda + i `div` r)
{-# INLINE tr #-}

-- | O(m*n) Create an identity matrix
ident :: (Num a, G.Vector v a) => Int -> Matrix v a
ident n = diagRect 0 (n,n) $ replicate n 1
{-# INLINE ident #-}

-- | O(m*n) Create a square matrix with given diagonal, other entries default to 0
diag :: (Num a, G.Vector v a, F.Foldable t)
     => t a  -- ^ diagonal
     -> Matrix v a
diag d = diagRect 0 (n,n) d
  where n = length . F.toList $ d
{-# INLINE diag #-}

-- | O(m*n) Create a rectangular matrix with default values and given diagonal
diagRect :: (G.Vector v a, F.Foldable t)
         => a         -- ^ default value
         -> (Int, Int)
         -> t a       -- ^ diagonal
         -> Matrix v a
diagRect z0 (r,c) d = MG.fromVector (r,c) $ G.create $ GM.replicate n z0 >>= go d c
  where
    go xs c' v = F.foldlM f 0 xs >> return v
      where
        f !i x = GM.unsafeWrite v (i*(c'+1)) x >> return (i+1)
    n = r * c
{-# INLINE diagRect #-}

fromBlocks :: G.Vector v a
           => a               -- ^ default value
           -> [[Matrix v a]]
           -> Matrix v a
fromBlocks d ms = MG.fromVector (m,n) $ G.create $ GM.replicate (m*n) d >>= go n ms
  where
    go n' xss v = foldM_ f 0 xss >> return v
      where
        f !cr xs = do (r', _) <- foldM g (0, 0) xs
                      return $ cr + r'
          where
            g (!maxR, !cc) x = do
                let (r,c) = MG.dim x
                    vec = MG.flatten x
                    step i u = do
                        GM.unsafeWrite v ((cr + i `div` c) * n' + i `mod` c + cc) u
                        return (i+1)
                G.foldM'_ step (0::Int) vec
                return (max maxR r, cc + c)
    -- figure out the dimension of the new matrix
    (m, n) = (sum *** maximum) . Prelude.unzip . Prelude.map ((maximum *** sum) .
                Prelude.unzip . Prelude.map (MG.rows &&& MG.cols)) $ ms
{-# INLINE fromBlocks #-}

isSymmetric :: (Eq a, G.Vector v a) => Matrix v a -> Bool
isSymmetric m@(Matrix r c _ _ _) | r /= c = False
                                 | otherwise = all f [0 .. r-1]
  where
    f i = all g [i + 1 .. c-1]
      where g j = m MG.! (i,j) == m MG.! (j,i)
{-# INLINE isSymmetric #-}

force :: G.Vector v a => Matrix v a -> Matrix v a
force m@(Matrix r c _ _ _) = MG.fromVector (r,c) . G.force . MG.flatten $ m
{-# INLINE force #-}

map :: (G.Vector v a, G.Vector v b) => (a -> b) -> Matrix v a -> Matrix v b
map f m@(Matrix r c _ _ _) = MG.fromVector (r,c) $ G.map f . MG.flatten $ m
{-# INLINE map #-}

imap :: (G.Vector v a, G.Vector v b) => ((Int, Int) -> a -> b) -> Matrix v a -> Matrix v b
imap f m@(Matrix r c _ _ _) = MG.fromVector (r,c) $ G.imap f' . MG.flatten $ m
  where
    f' i = f (i `divMod` c)
{-# INLINE imap #-}

foldl :: G.Vector v b => (a -> b -> a) -> a -> Matrix v b -> a
foldl f acc m = G.foldl f acc . MG.flatten $ m
{-# INLINE foldl #-}

mapM :: (G.Vector v a, G.Vector v b, Monad m)
     => (a -> m b) -> Matrix v a -> m (Matrix v b)
mapM f m@(Matrix r c _ _ _) = liftM (MG.fromVector (r,c)) $ G.mapM f $ MG.flatten m
{-# INLINE mapM #-}

-- | O(m*n) Apply the monadic action to every element and its index,
-- yielding a matrix of results.
imapM :: (G.Vector v a, G.Vector v b, Monad m)
      => ((Int, Int) -> a -> m b) -> Matrix v a -> m (Matrix v b)
imapM f m@(Matrix r c _ _ _) = fmap (MG.fromVector (r,c)) $ G.imapM f' $
    MG.flatten m
  where
    f' i = f (i `divMod` c)
{-# INLINE imapM #-}

mapM_ :: (G.Vector v a, Monad m) => (a -> m b) -> Matrix v a -> m ()
mapM_ f = G.mapM_ f . MG.flatten
{-# INLINE mapM_ #-}

-- | O(m*n) Apply the monadic action to every element and its index,
-- ignoring the results.
imapM_ :: (G.Vector v a, Monad m)
       => ((Int, Int) -> a -> m b) -> Matrix v a -> m ()
imapM_ f m@(Matrix _ c _ _ _) = G.imapM_ f' $ MG.flatten m
  where
    f' i = f (i `divMod` c)
{-# INLINE imapM_ #-}

forM :: (G.Vector v a, G.Vector v b, Monad m)
     => Matrix v a -> (a -> m b) -> m (Matrix v b)
forM = flip mapM
{-# INLINE forM #-}

forM_ :: (G.Vector v a, Monad m) => Matrix v a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

zipWith :: (G.Vector v a, G.Vector v b, G.Vector v c)
        => (a -> b -> c) -> Matrix v a -> Matrix v b -> Matrix v c
zipWith f m1 m2
    | MG.dim m1 /= MG.dim m2 = error "zipWith: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zipWith f (MG.flatten m1) $ MG.flatten m2
{-# INLINE zipWith #-}

zipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d)
         => (a -> b -> c -> d) -> Matrix v a -> Matrix v b -> Matrix v c
         -> Matrix v d
zipWith3 f m1 m2 m3
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 = error "zipWith3: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zipWith3 f (MG.flatten m1) (MG.flatten m2) $ MG.flatten m3
{-# INLINE zipWith3 #-}

zipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e)
         => (a -> b -> c -> d -> e) -> Matrix v a -> Matrix v b -> Matrix v c
         -> Matrix v d -> Matrix v e
zipWith4 f m1 m2 m3 m4
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 = error "zipWith4: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zipWith4 f (MG.flatten m1) (MG.flatten m2)
                  (MG.flatten m3) $ MG.flatten m4
{-# INLINE zipWith4 #-}

zipWith5 :: ( G.Vector v a, G.Vector v b, G.Vector v c,G.Vector v d
            , G.Vector v e, G.Vector v f )
         => (a -> b -> c -> d -> e -> f) -> Matrix v a -> Matrix v b
         -> Matrix v c -> Matrix v d -> Matrix v e -> Matrix v f
zipWith5 f m1 m2 m3 m4 m5
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 ||
      MG.dim m4 /= MG.dim m5 = error "zipWith5: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zipWith5 f (MG.flatten m1) (MG.flatten m2)
                  (MG.flatten m3) (MG.flatten m4) $ MG.flatten m5
{-# INLINE zipWith5 #-}

zipWith6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
            , G.Vector v e, G.Vector v f, G.Vector v g )
         => (a -> b -> c -> d -> e -> f -> g) -> Matrix v a -> Matrix v b
         -> Matrix v c -> Matrix v d -> Matrix v e -> Matrix v f -> Matrix v g
zipWith6 f m1 m2 m3 m4 m5 m6
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 ||
      MG.dim m4 /= MG.dim m5 ||
      MG.dim m5 /= MG.dim m6 = error "zipWith6: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zipWith6 f (MG.flatten m1) (MG.flatten m2) (MG.flatten m3)
                  (MG.flatten m4) (MG.flatten m5) $ MG.flatten m6
{-# INLINE zipWith6 #-}

izipWith :: (G.Vector v a, G.Vector v b, G.Vector v c)
         => ((Int, Int) -> a -> b -> c) -> Matrix v a -> Matrix v b -> Matrix v c
izipWith f m1 m2
    | MG.dim m1 /= MG.dim m2 = error "izipWith: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.izipWith f' (MG.flatten m1) $ MG.flatten m2
  where
    c = MG.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith #-}

izipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d)
          => ((Int, Int) -> a -> b -> c -> d) -> Matrix v a -> Matrix v b
          -> Matrix v c -> Matrix v d
izipWith3 f m1 m2 m3
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 = error "izipWith3: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.izipWith3 f' (MG.flatten m1) (MG.flatten m2) $ MG.flatten m3
  where
    c = MG.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith3 #-}

izipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e)
          => ((Int, Int) -> a -> b -> c -> d -> e) -> Matrix v a -> Matrix v b
          -> Matrix v c -> Matrix v d -> Matrix v e
izipWith4 f m1 m2 m3 m4
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 = error "izipWith4: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.izipWith4 f' (MG.flatten m1) (MG.flatten m2)
                  (MG.flatten m3) $ MG.flatten m4
  where
    c = MG.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith4 #-}

izipWith5 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
             , G.Vector v e, G.Vector v f )
          => ((Int, Int) -> a -> b -> c -> d -> e -> f) -> Matrix v a
          -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v e -> Matrix v f
izipWith5 f m1 m2 m3 m4 m5
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 ||
      MG.dim m4 /= MG.dim m5 = error "izipWith5: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.izipWith5 f' (MG.flatten m1) (MG.flatten m2)
                  (MG.flatten m3) (MG.flatten m4) $ MG.flatten m5
  where
    c = MG.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith5 #-}

izipWith6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
             , G.Vector v e, G.Vector v f, G.Vector v g )
          => ((Int, Int) -> a -> b -> c -> d -> e -> f -> g) -> Matrix v a
          -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v e -> Matrix v f
          -> Matrix v g
izipWith6 f m1 m2 m3 m4 m5 m6
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 ||
      MG.dim m4 /= MG.dim m5 ||
      MG.dim m5 /= MG.dim m6 = error "izipWith6: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.izipWith6 f' (MG.flatten m1) (MG.flatten m2) (MG.flatten m3)
                  (MG.flatten m4) (MG.flatten m5) $ MG.flatten m6
  where
    c = MG.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith6 #-}


zip :: (G.Vector v a, G.Vector v b, G.Vector v (a,b))
    => Matrix v a -> Matrix v b -> Matrix v (a,b)
zip m1 m2
    | MG.dim m1 /= MG.dim m2 = error "zip: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zip (MG.flatten m1) $ MG.flatten m2
{-# INLINE zip #-}

zip3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a,b,c))
     => Matrix v a -> Matrix v b -> Matrix v c -> Matrix v (a,b,c)
zip3 m1 m2 m3
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 = error "zip3: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zip3 (MG.flatten m1) (MG.flatten m2) $ MG.flatten m3
{-# INLINE zip3 #-}

zip4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v (a,b,c,d))
     => Matrix v a -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v (a,b,c,d)
zip4 m1 m2 m3 m4
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 = error "zip4: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zip4 (MG.flatten m1) (MG.flatten m2)
                  (MG.flatten m3) $ MG.flatten m4
{-# INLINE zip4 #-}

zip5 :: ( G.Vector v a, G.Vector v b, G.Vector v c
        , G.Vector v d, G.Vector v e, G.Vector v (a,b,c,d,e) )
     => Matrix v a -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v e
     -> Matrix v (a,b,c,d,e)
zip5 m1 m2 m3 m4 m5
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 ||
      MG.dim m4 /= MG.dim m5 = error "zip5: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zip5 (MG.flatten m1) (MG.flatten m2)
                  (MG.flatten m3) (MG.flatten m4) $ MG.flatten m5
{-# INLINE zip5 #-}

zip6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e
        , G.Vector v f, G.Vector v (a,b,c,d,e,f) )
     => Matrix v a -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v e
     -> Matrix v f -> Matrix v (a,b,c,d,e,f)
zip6 m1 m2 m3 m4 m5 m6
    | MG.dim m1 /= MG.dim m2 ||
      MG.dim m2 /= MG.dim m3 ||
      MG.dim m3 /= MG.dim m4 ||
      MG.dim m4 /= MG.dim m5 ||
      MG.dim m5 /= MG.dim m6 = error "zip6: Dimensions don't match."
    | otherwise = MG.fromVector (MG.dim m1) $
                  G.zip6 (MG.flatten m1) (MG.flatten m2) (MG.flatten m3)
                  (MG.flatten m4) (MG.flatten m5) $ MG.flatten m6
{-# INLINE zip6 #-}

zipWithM :: (Monad m, G.Vector v a, G.Vector v b, G.Vector v c)
         => (a -> b -> m c) -> Matrix v a -> Matrix v b -> m (Matrix v c)
zipWithM f m1 m2
    | MG.dim m1 /= MG.dim m2 = error "zipWithM: Dimensions don't match."
    | otherwise = liftM (MG.fromVector $ MG.dim m1) $
                  G.zipWithM f (MG.flatten m1) $ MG.flatten m2
{-# INLINE zipWithM #-}

zipWithM_ :: (Monad m, G.Vector v a, G.Vector v b)
          => (a -> b -> m c) -> Matrix v a -> Matrix v b -> m ()
zipWithM_ f m1 m2
    | MG.dim m1 /= MG.dim m2 = error "zipWithM_: Dimensions don't match."
    | otherwise = G.zipWithM_ f (MG.flatten m1) $ MG.flatten m2
{-# INLINE zipWithM_ #-}

unzip :: (G.Vector v a, G.Vector v b, G.Vector v (a,b))
      => Matrix v (a,b) -> (Matrix v a, Matrix v b )
unzip m = (MG.fromVector d v1, MG.fromVector d v2)
  where
    d = MG.dim m
    (v1, v2) = G.unzip $ MG.flatten m
{-# INLINE unzip #-}

unzip3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a,b,c))
       => Matrix v (a,b, c) -> (Matrix v a, Matrix v b, Matrix v c)
unzip3 m = (MG.fromVector d v1, MG.fromVector d v2, MG.fromVector d v3)
  where
    d = MG.dim m
    (v1, v2, v3) = G.unzip3 $ MG.flatten m
{-# INLINE unzip3 #-}

unzip4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v (a,b,c,d))
       => Matrix v (a,b,c,d) -> (Matrix v a, Matrix v b, Matrix v c, Matrix v d)
unzip4 m = ( MG.fromVector d v1
           , MG.fromVector d v2
           , MG.fromVector d v3
           , MG.fromVector d v4
           )
  where
    d = MG.dim m
    (v1, v2, v3, v4) = G.unzip4 $ MG.flatten m
{-# INLINE unzip4 #-}

unzip5 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
          , G.Vector v e, G.Vector v (a,b,c,d,e) )
       => Matrix v (a,b,c,d,e)
       -> (Matrix v a, Matrix v b, Matrix v c, Matrix v d, Matrix v e)
unzip5 m = ( MG.fromVector d v1
           , MG.fromVector d v2
           , MG.fromVector d v3
           , MG.fromVector d v4
           , MG.fromVector d v5
           )
  where
    d = MG.dim m
    (v1, v2, v3, v4, v5) = G.unzip5 $ MG.flatten m
{-# INLINE unzip5 #-}

unzip6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
          , G.Vector v e, G.Vector v f, G.Vector v (a,b,c,d,e,f) )
       => Matrix v (a,b,c,d,e,f)
       -> (Matrix v a, Matrix v b, Matrix v c, Matrix v d, Matrix v e, Matrix v f)
unzip6 m = ( MG.fromVector d v1
           , MG.fromVector d v2
           , MG.fromVector d v3
           , MG.fromVector d v4
           , MG.fromVector d v5
           , MG.fromVector d v6
           )
  where
    d = MG.dim m
    (v1, v2, v3, v4, v5, v6) = G.unzip6 $ MG.flatten m
{-# INLINE unzip6 #-}

sequence :: (G.Vector v a, G.Vector v (m a), Monad m)
         => Matrix v (m a) -> m (Matrix v a)
sequence (Matrix r c tda offset vec) = liftM (Matrix r c tda offset) . G.sequence $ vec
{-# INLINE sequence #-}

sequence_ :: (G.Vector v (m a), Monad m)
          => Matrix v (m a) -> m ()
sequence_ (Matrix _ _ _ _ vec) = G.sequence_ vec
{-# INLINE sequence_ #-}

generate :: G.Vector v a => (Int, Int) -> ((Int, Int) -> a) -> Matrix v a
generate (r,c) f = MG.fromVector (r,c) . G.generate (r*c) $ \i -> f (i `divMod` c)
{-# INLINE generate #-}
