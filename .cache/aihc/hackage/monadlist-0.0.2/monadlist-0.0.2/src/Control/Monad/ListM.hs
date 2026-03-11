-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.ListM
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Control.Monad.ListM (
  mapMP
, filterMP
, intersperseM
, intercalateM
, foldM1
, joinMap
, joinMapM
, anyM
, allM
, scanM
, mapAccumM
, iterateM
, takeM
, dropM
, splitAtM
, takeWhileM
, dropWhileM
, spanM
, breakM
, elemM
, notElemM
, lookupM
, findM
, partitionM
, elemIndexM
, elemIndicesM
, findIndexM
, findIndicesM
, zipWithM3
, zipWithM4
, zipWithM5
, zipWithM6
, nubM
, nubByM
, deleteM
, deleteByM
, deleteFirstsM
, deleteFirstsByM
, unionM
, unionByM
, intersectM
, intersectByM
, groupM
, groupByM
, sortM
, sortByM
, insertM
, insertByM
, maximumM
, maximumByM
, minimumM
, minimumByM
) where

import qualified Prelude
import Prelude hiding (error, mapM, sequence, and, or)

import Control.Monad hiding (mapM, sequence)
import Data.Foldable (or, and)
import Data.List (zip4, zip5, zip6)
import Data.Maybe (isJust)
import Data.Traversable (Traversable, mapM, sequence)


infixr 5 !, !!!


error :: String -> String -> a
error func msg = Prelude.error $ "Control.Monad.ListM." ++ func ++ ": " ++ msg

notM :: (Monad m) => Bool -> m Bool
notM = return . not

eqM :: (Eq a, Monad m) => a -> a -> m Bool
eqM x y = return $ x == y

compareM :: (Ord a, Monad m) => a -> a -> m Ordering
compareM x y = return $ compare x y

(!) :: (MonadPlus p) => a -> p a -> p a
x ! y = return x `mplus` y

(!!!) :: (MonadPlus p) => [a] -> p a -> p a
(!!!) = flip $ foldr (!)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (x, y, z, w) = f x y z w

uncurry5 :: (a -> b -> c -> d -> e -> f) -> ((a, b, c, d, e) -> f)
uncurry5 f (x, y, z, w, s) = f x y z w s

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> ((a, b, c, d, e, f) -> g)
uncurry6 f (x, y, z, w, s, t) = f x y z w s t

mapMP :: (Monad m, MonadPlus p) => (a -> m b) -> [a] -> m (p b)
mapMP _ [] = return mzero
mapMP f (x:xs) = do
  y <- f x
  liftM (y!) $ mapMP f xs

filterMP :: (Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p a)
filterMP _ [] = return mzero
filterMP p (x:xs) = do
  bool <- p x
  if bool
    then liftM (x!) $ filterMP p xs
    else filterMP p xs

intersperseM :: (Monad m, MonadPlus p) => m a -> [a] -> m (p a)
intersperseM _ [] = return mzero
intersperseM _ [x] = return $ return x
intersperseM m (x:ys) = do
  z <- m
  liftM ([x, z] !!!) $ intersperseM m ys

intercalateM :: (Monad m, MonadPlus p) => m (p a) -> [p a] -> m (p a)
intercalateM m = liftM join . intersperseM m

foldM1 :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error "foldM1" "empty list"
foldM1 f (x:xs) = foldM f x xs

joinMap :: (Monad m) => (a -> m b) -> m a -> m b
joinMap f = join . liftM f

joinMapM :: (Monad m, MonadPlus p) => (a -> m (p b)) -> [a] -> m (p b)
joinMapM f = liftM join . mapMP f

anyM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
anyM p = liftM or . mapM p

allM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
allM p = liftM and . mapM p

scanM :: (Monad m, MonadPlus p) => (a -> b -> m a) -> a -> [b] -> m (p a)
scanM _ z [] =  return $ return z
scanM f z (x:xs) = do
  z' <- f z x
  liftM (z!) $ scanM f z' xs

mapAccumM :: (Monad m, MonadPlus p) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, p y)
mapAccumM _ z [] = return (z, mzero)
mapAccumM f z (x:xs) = do
  (z', y) <- f z x
  (z'', ys) <- mapAccumM f z' xs
  return (z'', y!ys)

iterateM :: (Monad m, MonadPlus p) => (a -> m a) -> a -> m (p a)
iterateM f x = do
  x' <- f x
  liftM (x!) $ iterateM f x'

takeM :: (Integral i, Monad m, MonadPlus p) => i -> [m a] -> m (p a)
takeM _ [] = return mzero
takeM n (m:ms)
  | n <= 0 = return mzero
  | otherwise = m >>= \x -> liftM (x!) $ takeM (n-1) ms

dropM :: (Integral i, Monad m) => i -> [m a] -> m [a]
dropM _ [] = return []
dropM n (m:ms)
  | n <= 0 = sequence $ m:ms
  | otherwise = m >> dropM (n-1) ms

splitAtM :: (Integral i, Monad m, MonadPlus p) => i -> [m a] -> m (p a, [a])
splitAtM _ [] = return (mzero, [])
splitAtM n (m:ms)
  | n <= 0 = do
      ys <- sequence $ m:ms
      return (mzero, ys)
  | otherwise = do
      x <- m
      (xs, ys) <- splitAtM (n-1) ms
      return (x!xs, ys)

takeWhileM :: (Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p a)
takeWhileM _ [] = return mzero
takeWhileM p (x:xs) = do
  bool <- p x
  if bool
    then liftM (x!) $ takeWhileM p xs
    else return mzero

dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM p (x:xs) = do
  bool <- p x
  if bool
    then dropWhileM p xs
    else return $ x:xs

spanM :: (Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p a, [a])
spanM _ [] = return (mzero, [])
spanM p (x:xs) = do
  bool <- p x
  if bool
    then do
      (ys, zs) <- spanM p xs
      return (x!ys, zs)
    else return (mzero, x:xs)

breakM :: (Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p a, [a])
breakM p = spanM $ notM <=< p

elemM :: (Eq a, Monad m) => a -> [a] -> m Bool
elemM x xs = do
  idx <- elemIndexM x xs
  let _ = idx :: Maybe Int
  return $ isJust idx

notElemM :: (Eq a, Monad m) => a -> [a] -> m Bool
notElemM x = notM <=< elemM x

lookupM :: (Eq a, Monad m, MonadPlus p) => a -> [m (a, b)] -> m (p b)
lookupM _ [] = return mzero
lookupM x (m:ms) = do
  (k, v) <- m
  if x == k
    then return $ return v
    else lookupM x ms

findM :: (Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p a)
findM _ [] = return mzero
findM p (x:xs) = do
  bool <- p x
  if bool
    then return $ return x
    else findM p xs

partitionM :: (Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p a, [a])
partitionM _ [] = return (mzero, [])
partitionM p (x:xs) = do
  bool <- p x
  if bool
    then do
      (ys, zs) <- partitionM p xs
      return (x!ys, zs)
    else return (mzero, x:xs)

elemIndexM :: (Eq a, Integral i, Monad m, MonadPlus p) => a -> [a] -> m (p i)
elemIndexM x = findIndexM $ eqM x

elemIndicesM :: (Eq a, Integral i, Monad m, MonadPlus p) => a -> [a] -> m (p i)
elemIndicesM x = findIndicesM $ eqM x

findIndexM :: (Integral i, Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p i)
findIndexM p = liftM (liftM fst) . findM (p . snd) . zip [0..]

findIndicesM :: (Integral i, Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p i)
findIndicesM p = liftM (liftM fst) . filterMP (p . snd) . zip [0..]

zipWithM3 :: (Monad m, MonadPlus p) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m (p d)
zipWithM3 f xs ys = mapMP (uncurry3 f) . zip3 xs ys

zipWithM4 :: (Monad m, MonadPlus p) => (a -> b -> c -> d -> m e) -> [a] -> [b] -> [c] -> [d] -> m (p e)
zipWithM4 f xs ys zs = mapMP (uncurry4 f) . zip4 xs ys zs

zipWithM5 :: (Monad m, MonadPlus p) => (a -> b -> c -> d -> e -> m f) -> [a] -> [b] -> [c] -> [d] -> [e] -> m (p f)
zipWithM5 f xs ys zs ws = mapMP (uncurry5 f) . zip5 xs ys zs ws

zipWithM6 :: (Monad m, MonadPlus p) => (a -> b -> c -> d -> e -> f -> m g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> m (p g)
zipWithM6 f xs ys zs ws ss = mapMP (uncurry6 f) . zip6 xs ys zs ws ss

nubM :: (Eq a, Monad m, MonadPlus p) => [a] -> m (p a)
nubM = nubByM eqM

nubByM :: (Monad m, MonadPlus p) => (a -> a -> m Bool) -> [a] -> m (p a)
nubByM _ [] = return mzero
nubByM eq (x:xs) = liftM (x!) $ filterM (notM <=< eq x) xs >>= nubByM eq

deleteM :: (Eq a, Monad m) => a -> [a] -> m [a]
deleteM = deleteByM eqM

deleteByM :: (Monad m) => (a -> a -> m Bool) -> a -> [a] -> m [a]
deleteByM _ _ [] = return []
deleteByM eq x (y:ys) = do
  bool <- eq x y
  if bool
    then return ys
    else liftM (y:) $ deleteByM eq x ys

deleteFirstsM :: (Eq a, Monad m) => [a] -> [a] -> m [a]
deleteFirstsM = deleteFirstsByM eqM

deleteFirstsByM :: (Monad m) => (a -> a -> m Bool) -> [a] -> [a] -> m [a]
deleteFirstsByM _ xs [] = return xs
deleteFirstsByM eq xs (y:ys) = do
  xs' <- deleteByM eq y xs
  deleteFirstsByM eq xs' ys

unionM :: (Eq a, Monad m) => [a] -> [a] -> m [a]
unionM = unionByM eqM

unionByM :: (Monad m) => (a -> a -> m Bool) -> [a] -> [a] -> m [a]
unionByM eq ys xs = do
  ys' <- nubByM eq ys
  ys'' <- foldM (flip $ deleteByM eq) ys' xs
  return $ xs ++ ys''

intersectM :: (Eq a, Monad m, MonadPlus p) => [a] -> [a] -> m (p a)
intersectM = intersectByM eqM

intersectByM :: (Monad m, MonadPlus p) => (a -> a -> m Bool) -> [a] -> [a] -> m (p a)
intersectByM _ [] _ = return mzero
intersectByM _ _ [] = return mzero
intersectByM eq (x:xs) ys = do
  bool <- anyM (eq x) ys
  if bool
    then liftM (x!) $ intersectByM eq xs ys
    else intersectByM eq xs ys

groupM :: (Eq a, Monad m, MonadPlus p, MonadPlus q) => [a] -> m (p (q a))
groupM = groupByM eqM

groupByM :: (Monad m, MonadPlus p, MonadPlus q) => (a -> a -> m Bool) -> [a] -> m (p (q a))
groupByM _ [] = return mzero
groupByM eq (x:xs) = do
  (ys, zs) <- spanM (eq x) xs
  liftM ((x!ys)!) $ groupByM eq zs

sortM :: (Ord a, Monad m) => [a] -> m [a]
sortM = sortByM compareM

sortByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM cmp = mergeAll <=< sequences
  where
    sequences (a:b:xs) = do
      ord <- cmp a b
      case ord of
        GT -> descending b [a] xs
        _ -> ascending b (a:) xs
    sequences xs = return [xs]

    descending a as cs@(b:bs) = do
      ord <- cmp a b
      case ord of
        GT -> descending b (a:as) bs
        _ -> liftM ((a:as) :) $ sequences cs
    descending a as bs = liftM ((a:as) :) $ sequences bs

    ascending a as cs@(b:bs) = do
      ord <- cmp a b
      case ord of
        GT -> liftM (as [a] :) $ sequences cs
        _ -> ascending b (\ys -> as (a:ys)) bs
    ascending a as bs = liftM (as [a] :) $ sequences bs

    mergeAll [x] = return x
    mergeAll xs = mergeAll =<< (mergePairs xs)

    mergePairs (a:b:xs) = liftM2 (:) (merge a b) $ mergePairs xs
    mergePairs xs = return xs

    merge as@(a:as') bs@(b:bs') = do
      ord <- cmp a b
      case ord of
        GT -> liftM (b :) $ merge as  bs'
        _ -> liftM (a :) $ merge as' bs
    merge [] bs = return bs
    merge as [] = return as

insertM :: (Ord a, Monad m) => a -> [a] -> m [a]
insertM = insertByM compareM

insertByM :: (Monad m) => (a -> a -> m Ordering) -> a -> [a] -> m [a]
insertByM _ x [] = return [x]
insertByM cmp x (y:ys) = do
  ordering <- cmp x y
  case ordering of
    GT -> liftM (y:) $ insertByM cmp x ys
    _ -> return $ x:y:ys

maximumM :: (Ord a, Monad m) => [a] -> m a
maximumM [] = error "maximumM" "empty list"
maximumM xs = maximumByM compareM xs

maximumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
maximumByM _ [] = error "maximumByM" "empty list"
maximumByM cmp xs = foldM1 maxByM xs
  where
    maxByM x y = do
      ordering <- cmp x y
      return $ case ordering of
        GT -> x
        _ -> y

minimumM :: (Ord a, Monad m) => [a] -> m a
minimumM [] = error "minimumM" "empty list"
minimumM xs = minimumByM compareM xs

minimumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
minimumByM _ [] = error "minimumByM" "empty list"
minimumByM cmp xs = foldM1 minByM xs
  where
    minByM x y = do
      ordering <- cmp x y
      return $ case ordering of
        GT -> y
        _ -> x

