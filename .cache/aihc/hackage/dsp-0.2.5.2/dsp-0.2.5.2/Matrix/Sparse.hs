module Matrix.Sparse (
   Matrix,
   bounds,
   fromMap,
   fromRows,
   fromColumns,
   fromDense,
   toRows,
   toColumns,
   toDense,
   getRow,
   getColumn,
   mulVector,
   ) where

import qualified Matrix.Vector as Vector
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Map (Map)
import Data.Array (Array, Ix, accumArray, (!))


data Matrix i j a = Matrix ((i,j), (i,j)) (Map i (Map j a))
   deriving Show

instance Functor (Matrix i j) where
   fmap f (Matrix bnds m) = Matrix bnds $ fmap (fmap f) m


bounds :: Matrix i j a -> ((i,j), (i,j))
bounds (Matrix bnds _) = bnds

fromMap :: (Ord i, Ord j) => ((i,j), (i,j)) -> Map (i,j) a -> Matrix i j a
fromMap bnds =
   Matrix bnds . Map.fromListWith Map.union .
   map (\((i,j),a) -> (i, Map.singleton j a)) . Map.toList

fromRows ::
   (Ord i, Ord j) => ((i,j), (i,j)) -> Map i (Map j a) -> Matrix i j a
fromRows = Matrix

fromColumns ::
   (Ord i, Ord j) => ((i,j), (i,j)) -> Map j (Map i a) -> Matrix i j a
fromColumns bnds = Matrix bnds . flipMap

fromDense :: (Ix i, Ix j) => Array (i,j) a -> Matrix i j a
fromDense a = fromMap (Array.bounds a) $ Map.fromList $ Array.assocs a


toRows :: (Ord i, Ord j) => Matrix i j a -> Map i (Map j a)
toRows (Matrix _bnds rows) = rows

toColumns :: (Ord i, Ord j) => Matrix i j a -> Map j (Map i a)
toColumns (Matrix _bnds rows) = flipMap rows

toDense :: (Ix i, Ix j, Num a) => Matrix i j a -> Array (i,j) a
toDense (Matrix bnds a) =
   accumArray (const id) 0 bnds $ Fold.fold $
   Map.mapWithKey (\i -> map (\(j,e) -> ((i,j),e)) .  Map.toList) a


-- cf. comfort-graph:Graph.Comfort.Map.flip
flipMap :: (Ord i, Ord j) => Map i (Map j a) -> Map j (Map i a)
flipMap =
   Map.unionsWith (Map.unionWith (error $ "Map.flip: duplicate key")) .
   Map.elems . Map.mapWithKey (fmap . Map.singleton)


getRow :: (Ord i, Ord j) => i -> Matrix i j a -> Map j a
getRow i (Matrix _ rows) = Map.findWithDefault Map.empty i rows

getColumn :: (Ord i, Ord j) => j -> Matrix i j a -> Map i a
getColumn j (Matrix _ rows) = Map.mapMaybe (Map.lookup j) rows


mulVector :: (Ix i, Ix j, Num a) => Matrix i j a -> Array j a -> Array i a
mulVector a@(Matrix ((m0,n0), (m1,n1)) _) v =
   if (n0,n1) == Array.bounds v
     then Vector.generate (m0,m1) $ flip mulRowVector v . flip getRow a
     else error "Sparse.mulVector: dimensions mismatch"

mulRowVector :: (Ix j, Num a) => Map j a -> Array j a -> a
mulRowVector row v = Fold.sum $ Map.mapWithKey (\j x -> x * v!j) row
