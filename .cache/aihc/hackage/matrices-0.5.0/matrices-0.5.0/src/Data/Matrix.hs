{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.Matrix
    ( Matrix

    -- * Accessors
    -- ** length information
    , dim
    , rows
    , cols

    -- ** Indexing
    , unsafeIndex
    , (!)
    , takeRow
    , takeColumn
    , takeDiag

    -- * Construction
    , unsafeFromVector
    , fromVector
    , matrix
    , fromList
    , fromLists
    , fromRows
    , fromColumns
    , empty

    -- * Conversions
    , flatten
    , toRows
    , toColumns
    , toList
    , toLists

    , tr
    , subMatrix
    , ident
    , diag
    , diagRect
    , fromBlocks
    , isSymmetric
    , force

    , foldl

    -- * Mapping
    , map
    , imap

    -- * Monadic mapping
    , mapM
    , imapM
    , mapM_
    , imapM_
    , forM
    , forM_

    -- * Zipping
    , zipWith
    , zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , izipWith
    , izipWith3
    , izipWith4
    , izipWith5
    , izipWith6
    , zip
    , zip3
    , zip4
    , zip5
    , zip6

    -- * Monadic Zipping
    , zipWithM
    , zipWithM_

    -- * Unzipping
    , unzip
    , unzip3
    , unzip4
    , unzip5
    , unzip6

    -- * Monadic sequencing
    , sequence
    , sequence_

    , generate

    -- * Mutable matrix
    , thaw
    , unsafeThaw
    , freeze
    , unsafeFreeze
    , create
    ) where

import GHC.Exts (Constraint)
import Prelude hiding (sequence, sequence_, mapM_, zip, zip, zip3, zipWith, zipWith3, foldl, unzip, map, mapM, unzip3)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST            (ST)
import Data.Foldable (Foldable)
import Data.Vector (Vector)

import qualified Data.Matrix.Generic as MG
import Data.Matrix.Mutable (MMatrix)

type Matrix = MG.Matrix Vector
type Context x = (() :: Constraint)

dim :: Context a => Matrix a -> (Int, Int)
dim = MG.dim

rows :: Context a => Matrix a -> Int
rows = MG.rows

cols :: Context a => Matrix a -> Int
cols = MG.cols

unsafeIndex :: Context a => Matrix a -> (Int, Int) -> a
unsafeIndex = MG.unsafeIndex

(!) :: Context a => Matrix a -> (Int, Int) -> a
(!) = (MG.!)

takeRow :: Context a => Matrix a -> Int -> Vector a
takeRow = MG.takeRow

takeColumn :: Context a => Matrix a -> Int -> Vector a
takeColumn = MG.takeColumn

takeDiag :: Context a => Matrix a -> Vector a
takeDiag = MG.takeDiag

unsafeFromVector :: Context a => (Int, Int) -> Vector a -> Matrix a
unsafeFromVector = MG.unsafeFromVector

fromVector :: Context a => (Int, Int) -> Vector a -> Matrix a
fromVector = MG.fromVector

-- | O(m*n) Matrix construction
matrix :: Context a => Int -> [a] -> Matrix a
matrix = MG.matrix

fromList :: Context a => (Int, Int) -> [a] -> Matrix a
fromList = MG.fromList

-- | O(m*n) Create matrix from list of lists, it doesn't check if the list of
-- list is a valid matrix
fromLists :: Context a => [[a]] -> Matrix a
fromLists = MG.fromLists

-- | O(m*n) Create matrix from rows
fromRows :: Context a => [Vector a] -> Matrix a
fromRows = MG.fromRows

-- | O(m*n) Create matrix from columns
fromColumns :: Context a => [Vector a] -> Matrix a
fromColumns = MG.fromColumns

empty :: Context a => Matrix a
empty = MG.empty

flatten :: Context a => Matrix a -> Vector a
flatten = MG.flatten

-- | O(m) Return the rows
toRows :: Context a => Matrix a -> [Vector a]
toRows = MG.toRows

toColumns :: Context a => Matrix a -> [Vector a]
toColumns = MG.toColumns

-- | O(m*n) Create a list by concatenating rows
toList :: Context a => Matrix a -> [a]
toList = MG.toList

-- | O(m*n) List of lists
toLists :: Context a => Matrix a -> [[a]]
toLists = MG.toLists

-- | O(m*n) Matrix transpose
tr :: Context a => Matrix a -> Matrix a
tr = MG.tr

-- | O(1) Extract sub matrix
subMatrix :: Context a
          => (Int, Int)  -- ^ upper left corner of the submatrix
          -> (Int, Int)  -- ^ bottom right corner of the submatrix
          -> Matrix a -> Matrix a
subMatrix = MG.subMatrix

-- | O(m*n) Create an identity matrix
ident :: (Context a, Num a) => Int -> Matrix a
ident = MG.ident

-- | O(m*n) Create a square matrix with given diagonal, other entries default to 0
diag :: (Context a, Num a, Foldable t)
     => t a  -- ^ diagonal
     -> Matrix a
diag = MG.diag

-- | O(m*n) Create a rectangular matrix with default values and given diagonal
diagRect :: (Context a, Foldable t)
         => a         -- ^ default value
         -> (Int, Int)
         -> t a       -- ^ diagonal
         -> Matrix a
diagRect = MG.diagRect

fromBlocks :: Context a
           => a    -- ^ default value
           -> [[Matrix a]]
           -> Matrix a
fromBlocks = MG.fromBlocks

isSymmetric :: (Context a, Eq a) => Matrix a -> Bool
isSymmetric = MG.isSymmetric

force :: Context a => Matrix a -> Matrix a
force = MG.force

foldl :: Context b => (a -> b -> a) -> a -> Matrix b -> a
foldl = MG.foldl

map :: (Context a, Context b) => (a -> b) -> Matrix a -> Matrix b
map = MG.map

imap :: (Context a, Context b) => ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
imap = MG.imap

mapM :: (Context a, Context b, Monad m) => (a -> m b) -> Matrix a -> m (Matrix b)
mapM = MG.mapM

-- | O(m*n) Apply the monadic action to every element and its index,
-- yielding a matrix of results.
imapM :: (Context a, Context b, Monad m) => ((Int, Int) -> a -> m b) -> Matrix a -> m (Matrix b)
imapM = MG.imapM

mapM_ :: (Context a, Monad m) => (a -> m b) -> Matrix a -> m ()
mapM_ = MG.mapM_

-- | O(m*n) Apply the monadic action to every element and its index,
-- ignoring the results.
imapM_ :: (Context a, Monad m) => ((Int, Int) -> a -> m b) -> Matrix a -> m ()
imapM_ = MG.imapM_

forM :: (Context a, Context b, Monad m) => Matrix a -> (a -> m b) -> m (Matrix b)
forM = MG.forM

forM_ :: (Context a, Monad m) => Matrix a -> (a -> m b) -> m ()
forM_ = MG.forM_

zipWith :: ( Context a, Context b, Context c)
        => (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipWith = MG.zipWith

zipWith3 :: ( Context a, Context b, Context c, Context d)
         => (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c
         -> Matrix d
zipWith3 = MG.zipWith3

zipWith4 :: ( Context a, Context b, Context c, Context d, Context e)
         => (a -> b -> c -> d -> e) -> Matrix a -> Matrix b -> Matrix c
         -> Matrix d -> Matrix e
zipWith4 = MG.zipWith4

zipWith5 :: ( Context a, Context b, Context c, Context d, Context e, Context f)
         => (a -> b -> c -> d -> e -> f) -> Matrix a -> Matrix b
         -> Matrix c -> Matrix d -> Matrix e -> Matrix f
zipWith5 = MG.zipWith5

zipWith6 :: ( Context a, Context b, Context c, Context d, Context e, Context f
            , Context g )
         => (a -> b -> c -> d -> e -> f -> g) -> Matrix a -> Matrix b
         -> Matrix c -> Matrix d -> Matrix e -> Matrix f -> Matrix g
zipWith6 = MG.zipWith6

izipWith :: ( Context a, Context b, Context c)
         => ((Int, Int) -> a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
izipWith = MG.izipWith

izipWith3 :: ( Context a, Context b, Context c, Context d)
          => ((Int, Int) -> a -> b -> c -> d) -> Matrix a -> Matrix b
          -> Matrix c -> Matrix d
izipWith3 = MG.izipWith3

izipWith4 :: ( Context a, Context b, Context c, Context d, Context e)
          => ((Int, Int) -> a -> b -> c -> d -> e) -> Matrix a -> Matrix b
          -> Matrix c -> Matrix d -> Matrix e
izipWith4 = MG.izipWith4

izipWith5 :: ( Context a, Context b, Context c, Context d, Context e, Context f)
          => ((Int, Int) -> a -> b -> c -> d -> e -> f) -> Matrix a
          -> Matrix b -> Matrix c -> Matrix d -> Matrix e -> Matrix f
izipWith5 = MG.izipWith5

izipWith6 :: ( Context a, Context b, Context c, Context d, Context e, Context f
             , Context g )
          => ((Int, Int) -> a -> b -> c -> d -> e -> f -> g) -> Matrix a
          -> Matrix b -> Matrix c -> Matrix d -> Matrix e -> Matrix f
          -> Matrix g
izipWith6 = MG.izipWith6

zip :: ( Context a, Context b
       , Context (a,b) )
    => Matrix a -> Matrix b -> Matrix (a,b)
zip = MG.zip

zip3 :: ( Context a, Context b, Context c
        , Context (a,b,c) )
     => Matrix a -> Matrix b -> Matrix c -> Matrix (a,b,c)
zip3 = MG.zip3

zip4 :: ( Context a, Context b, Context c, Context d
        , Context (a,b,c,d) )
     => Matrix a -> Matrix b -> Matrix c -> Matrix d -> Matrix (a,b,c,d)
zip4 = MG.zip4

zip5 :: ( Context a, Context b, Context c, Context d, Context e
        , Context (a,b,c,d,e) )
     => Matrix a -> Matrix b -> Matrix c -> Matrix d -> Matrix e
     -> Matrix (a,b,c,d,e)
zip5 = MG.zip5

zip6 :: ( Context a, Context b, Context c, Context d, Context e, Context f
        , Context (a,b,c,d,e,f) )
     => Matrix a -> Matrix b -> Matrix c -> Matrix d -> Matrix e
     -> Matrix f -> Matrix (a,b,c,d,e,f)
zip6 = MG.zip6

zipWithM :: (Context a, Context b, Context c, Monad m)
         => (a -> b -> m c) -> Matrix a -> Matrix b -> m (Matrix c)
zipWithM = MG.zipWithM

zipWithM_ :: (Context a, Context b, Monad m)
          => (a -> b -> m c) -> Matrix a -> Matrix b -> m ()
zipWithM_ = MG.zipWithM_

unzip :: (Context a, Context b, Context (a,b))
      => Matrix (a,b) -> (Matrix a, Matrix b )
unzip = MG.unzip

unzip3 :: ( Context a, Context b, Context c
          , Context (a,b,c) )
       => Matrix (a,b,c) -> (Matrix a, Matrix b, Matrix c)
unzip3 = MG.unzip3

unzip4 :: ( Context a, Context b, Context c, Context d
          , Context (a,b,c,d) )
       => Matrix (a,b,c,d) -> (Matrix a, Matrix b, Matrix c, Matrix d)
unzip4 = MG.unzip4

unzip5 :: ( Context a, Context b, Context c, Context d, Context e
          , Context (a,b,c,d,e) )
       => Matrix (a,b,c,d,e)
       -> (Matrix a, Matrix b, Matrix c, Matrix d, Matrix e)
unzip5 = MG.unzip5

unzip6 :: ( Context a, Context b, Context c, Context d, Context e, Context f
          , Context (a,b,c,d,e,f) )
       => Matrix (a,b,c,d,e,f)
       -> (Matrix a, Matrix b, Matrix c, Matrix d, Matrix e, Matrix f)
unzip6 = MG.unzip6

sequence :: Monad m => Matrix (m a) -> m (Matrix a)
sequence = MG.sequence

sequence_ :: Monad m
          => Matrix (m a) -> m ()
sequence_ = MG.sequence_

generate :: Context a => (Int, Int) -> ((Int, Int) -> a) -> Matrix a
generate = MG.generate

thaw :: (Context a, PrimMonad s) => Matrix a -> s (MMatrix (PrimState s) a)
thaw = MG.thaw

unsafeThaw :: (Context a, PrimMonad s) => Matrix a -> s (MMatrix (PrimState s) a)
unsafeThaw = MG.unsafeThaw

freeze :: (Context a, PrimMonad s) => MMatrix (PrimState s) a -> s (Matrix a)
freeze = MG.freeze

unsafeFreeze :: (Context a, PrimMonad s) => MMatrix (PrimState s) a -> s (Matrix a)
unsafeFreeze = MG.unsafeFreeze

create :: Context a => (forall s . ST s (MMatrix s a)) -> Matrix a
create = MG.create
