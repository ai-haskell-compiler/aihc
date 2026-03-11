{-# LANGUAGE RankNTypes #-}
module Data.Vector.Unboxing
  (Vector
  ,Unboxable(Rep)
  ,Generics(..)
  ,Enum(..)
  ,EnumRep(..)
  -- * Accessors
  -- ** Length information
  ,length,null
  -- ** Indexing
  ,(!),(!?),head,last,unsafeIndex,unsafeHead,unsafeLast
  -- ** Monadic indexing
  ,indexM,headM,lastM,unsafeIndexM,unsafeHeadM,unsafeLastM
  -- ** Extracting subvectors (slicing)
  ,slice,init,tail,take,drop,splitAt,unsafeSlice,unsafeInit,unsafeTail
  ,unsafeTake,unsafeDrop
  -- * Construction
  -- ** Initialisation
  ,empty,singleton,replicate,generate,iterateN
  -- ** Monadic initialisation
  ,replicateM,generateM,iterateNM,create,createT
  -- ** Unfolding
  ,unfoldr,unfoldrN,unfoldrM,unfoldrNM,constructN,constructrN
  -- ** Enumeration
  ,enumFromN,enumFromStepN,enumFromTo,enumFromThenTo
  -- ** Concatenation
  ,cons,snoc,(++),concat
  -- ** Restricting memory usage
  ,force
  -- * Modifying vectors
  -- ** Bulk updates
  ,(//),update,update_,unsafeUpd,unsafeUpdate,unsafeUpdate_
  -- ** Accumulations
  ,accum,accumulate,accumulate_,unsafeAccum,unsafeAccumulate,unsafeAccumulate_
  -- ** Permutations
  ,reverse,backpermute,unsafeBackpermute
  -- ** Safe destructive updates
  ,modify
  -- * Elementwise operations
  -- ** Indexing
  ,indexed
  -- ** Mapping
  ,map,imap,concatMap
  -- ** Monadic mapping
  ,mapM,imapM,mapM_,imapM_,forM,forM_
  -- ** Zipping
  ,zipWith,zipWith3,zipWith4,zipWith5,zipWith6,izipWith,izipWith3,izipWith4
  ,izipWith5,izipWith6,zip,zip3,zip4,zip5,zip6
  -- ** Monadic zipping
  ,zipWithM,izipWithM,zipWithM_,izipWithM_
  -- ** Unzipping
  ,unzip,unzip3,unzip4,unzip5,unzip6
  -- * Working with predicates
  -- ** Filtering
  ,filter,ifilter,uniq,mapMaybe,imapMaybe,filterM,takeWhile,dropWhile
  -- ** Partitioning
  ,partition,unstablePartition,span,break
  -- ** Searching
  ,elem,notElem,find,findIndex,findIndices,elemIndex,elemIndices
  -- * Folding
  ,foldl,foldl1,foldl',foldl1',foldr,foldr1,foldr',foldr1',ifoldl,ifoldl'
  ,ifoldr,ifoldr'
  -- ** Specialised folds
  ,all,any,and,or,sum,product,maximum,maximumBy,minimum,minimumBy,minIndex
  ,minIndexBy,maxIndex,maxIndexBy
  -- ** Monadic folds
  ,foldM,ifoldM,foldM',ifoldM',fold1M,fold1M',foldM_,ifoldM_,foldM'_,ifoldM'_
  ,fold1M_,fold1M'_
  -- * Prefix sums (scans)
  ,prescanl,prescanl',postscanl,postscanl',scanl,scanl',scanl1,scanl1',iscanl
  ,iscanl',prescanr,prescanr',postscanr,postscanr',scanr,scanr',scanr1,scanr1'
  ,iscanr,iscanr'
  -- * Conversions
  -- ** Lists
  ,toList,fromList,fromListN
  -- ** Other vector types
  ,convert -- from Data.Vector.Generic
  ,toUnboxedVector
  ,fromUnboxedVector
  ,coercionWithUnboxedVector
  ,coerceVector
  ,liftCoercion
  ,vectorCoercion
  -- ** Mutable vectors
  ,freeze,thaw,copy,unsafeFreeze,unsafeThaw,unsafeCopy
  ) where

import Prelude (Monad,Int,Bool,Maybe,Traversable,Eq,Num,Ord,Ordering)
import qualified Prelude
import qualified Data.Vector.Generic as G
import Data.Vector.Generic (convert)
import Data.Vector.Unboxing.Internal
import Data.Vector.Unboxing.Instances ()
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad,PrimState)

length :: (Unboxable a) => Vector a -> Int
length = G.length
{-# INLINE length #-}

null :: (Unboxable a) => Vector a -> Bool
null = G.null
{-# INLINE null #-}

(!) :: (Unboxable a) => Vector a -> Int -> a
(!) = (G.!)
{-# INLINE (!) #-}

(!?) :: (Unboxable a) => Vector a -> Int -> Maybe a
(!?) = (G.!?)
{-# INLINE (!?) #-}

head :: (Unboxable a) => Vector a -> a
head = G.head
{-# INLINE head #-}

last :: (Unboxable a) => Vector a -> a
last = G.last
{-# INLINE last #-}

unsafeIndex :: (Unboxable a) => Vector a -> Int -> a
unsafeIndex = G.unsafeIndex
{-# INLINE unsafeIndex #-}

unsafeHead :: (Unboxable a) => Vector a -> a
unsafeHead = G.unsafeHead
{-# INLINE unsafeHead #-}

unsafeLast :: (Unboxable a) => Vector a -> a
unsafeLast = G.unsafeLast
{-# INLINE unsafeLast #-}

indexM :: (Monad m, Unboxable a) => Vector a -> Int -> m a
indexM = G.indexM
{-# INLINE indexM #-}

headM :: (Monad m, Unboxable a) => Vector a -> m a
headM = G.headM
{-# INLINE headM #-}

lastM :: (Monad m, Unboxable a) => Vector a -> m a
lastM = G.lastM
{-# INLINE lastM #-}

unsafeIndexM :: (Monad m, Unboxable a) => Vector a -> Int -> m a
unsafeIndexM = G.unsafeIndexM
{-# INLINE unsafeIndexM #-}

unsafeHeadM :: (Monad m, Unboxable a) => Vector a -> m a
unsafeHeadM = G.unsafeHeadM
{-# INLINE unsafeHeadM #-}

unsafeLastM :: (Monad m, Unboxable a) => Vector a -> m a
unsafeLastM = G.unsafeLastM
{-# INLINE unsafeLastM #-}

slice :: (Unboxable a) => Int -> Int -> Vector a -> Vector a
slice = G.slice
{-# INLINE slice #-}

init :: (Unboxable a) => Vector a -> Vector a
init = G.init
{-# INLINE init #-}

tail :: (Unboxable a) => Vector a -> Vector a
tail = G.tail
{-# INLINE tail #-}

take :: (Unboxable a) => Int -> Vector a -> Vector a
take = G.take
{-# INLINE take #-}

drop :: (Unboxable a) => Int -> Vector a -> Vector a
drop = G.drop
{-# INLINE drop #-}

splitAt :: (Unboxable a) => Int -> Vector a -> (Vector a, Vector a)
splitAt = G.splitAt
{-# INLINE splitAt #-}

unsafeSlice :: (Unboxable a) => Int -> Int -> Vector a -> Vector a
unsafeSlice = G.unsafeSlice
{-# INLINE unsafeSlice #-}

unsafeInit :: (Unboxable a) => Vector a -> Vector a
unsafeInit = G.unsafeInit
{-# INLINE unsafeInit #-}

unsafeTail :: (Unboxable a) => Vector a -> Vector a
unsafeTail = G.unsafeTail
{-# INLINE unsafeTail #-}

unsafeTake :: (Unboxable a) => Int -> Vector a -> Vector a
unsafeTake = G.unsafeTake
{-# INLINE unsafeTake #-}

unsafeDrop :: (Unboxable a) => Int -> Vector a -> Vector a
unsafeDrop = G.unsafeDrop
{-# INLINE unsafeDrop #-}

empty :: (Unboxable a) => Vector a
empty = G.empty
{-# INLINE empty #-}

singleton :: (Unboxable a) => a -> Vector a
singleton = G.singleton
{-# INLINE singleton #-}

replicate :: (Unboxable a) => Int -> a -> Vector a
replicate = G.replicate
{-# INLINE replicate #-}

generate :: (Unboxable a) => Int -> (Int -> a) -> Vector a
generate = G.generate
{-# INLINE generate #-}

iterateN :: (Unboxable a) => Int -> (a -> a) -> a -> Vector a
iterateN = G.iterateN
{-# INLINE iterateN #-}

replicateM :: (Monad m, Unboxable a) => Int -> m a -> m (Vector a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

generateM :: (Monad m, Unboxable a) => Int -> (Int -> m a) -> m (Vector a)
generateM = G.generateM
{-# INLINE generateM #-}

iterateNM :: (Monad m, Unboxable a) => Int -> (a -> m a) -> a -> m (Vector a)
iterateNM = G.iterateNM
{-# INLINE iterateNM #-}

create :: (Unboxable a) => (forall s. ST s (MVector s a)) -> Vector a
create = G.create
{-# INLINE create #-}

createT :: (Traversable f, Unboxable a) => (forall s. ST s (f (MVector s a))) -> f (Vector a)
createT = G.createT
{-# INLINE createT #-}

unfoldr :: (Unboxable a) => (b -> Maybe (a, b)) -> b -> Vector a
unfoldr = G.unfoldr
{-# INLINE unfoldr #-}

unfoldrN :: (Unboxable a) => Int -> (b -> Maybe (a, b)) -> b -> Vector a
unfoldrN = G.unfoldrN
{-# INLINE unfoldrN #-}

unfoldrM :: (Monad m, Unboxable a) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrM = G.unfoldrM
{-# INLINE unfoldrM #-}

unfoldrNM :: (Monad m, Unboxable a) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrNM = G.unfoldrNM
{-# INLINE unfoldrNM #-}

constructN :: (Unboxable a) => Int -> (Vector a -> a) -> Vector a
constructN = G.constructN
{-# INLINE constructN #-}

constructrN :: (Unboxable a) => Int -> (Vector a -> a) -> Vector a
constructrN = G.constructrN
{-# INLINE constructrN #-}

enumFromN :: (Num a, Unboxable a) => a -> Int -> Vector a
enumFromN = G.enumFromN
{-# INLINE enumFromN #-}

enumFromStepN :: (Num a, Unboxable a) => a -> a -> Int -> Vector a
enumFromStepN = G.enumFromStepN
{-# INLINE enumFromStepN #-}

enumFromTo :: (Prelude.Enum a, Unboxable a) => a -> a -> Vector a
enumFromTo = G.enumFromTo
{-# INLINE enumFromTo #-}

enumFromThenTo :: (Prelude.Enum a, Unboxable a) => a -> a -> a -> Vector a
enumFromThenTo = G.enumFromThenTo
{-# INLINE enumFromThenTo #-}

cons :: (Unboxable a) => a -> Vector a -> Vector a
cons = G.cons
{-# INLINE cons #-}

snoc :: (Unboxable a) => Vector a -> a -> Vector a
snoc = G.snoc
{-# INLINE snoc #-}

(++) :: (Unboxable a) => Vector a -> Vector a -> Vector a
(++) = (G.++)
{-# INLINE (++) #-}

concat :: (Unboxable a) => [Vector a] -> Vector a
concat = G.concat
{-# INLINE concat #-}

force :: (Unboxable a) => Vector a -> Vector a
force = G.force
{-# INLINE force #-}

(//) :: (Unboxable a) => Vector a -> [(Int, a)] -> Vector a
(//) = (G.//)
{-# INLINE (//) #-}

update :: (Unboxable a) => Vector a -> Vector (Int, a) -> Vector a
update = G.update
{-# INLINE update #-}

update_ :: (Unboxable a) => Vector a -> Vector Int -> Vector a -> Vector a
update_ = G.update_
{-# INLINE update_ #-}

unsafeUpd :: (Unboxable a) => Vector a -> [(Int, a)] -> Vector a
unsafeUpd = G.unsafeUpd
{-# INLINE unsafeUpd #-}

unsafeUpdate :: (Unboxable a) => Vector a -> Vector (Int, a) -> Vector a
unsafeUpdate = G.unsafeUpdate
{-# INLINE unsafeUpdate #-}

unsafeUpdate_ :: (Unboxable a) => Vector a -> Vector Int -> Vector a -> Vector a
unsafeUpdate_ = G.unsafeUpdate_
{-# INLINE unsafeUpdate_ #-}

accum :: (Unboxable a) => (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
accum = G.accum
{-# INLINE accum #-}

accumulate :: (Unboxable a, Unboxable b) => (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
accumulate = G.accumulate
{-# INLINE accumulate #-}

accumulate_ :: (Unboxable a, Unboxable b) => (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
accumulate_ = G.accumulate_
{-# INLINE accumulate_ #-}

unsafeAccum :: (Unboxable a) => (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
unsafeAccum = G.unsafeAccum
{-# INLINE unsafeAccum #-}

unsafeAccumulate :: (Unboxable a, Unboxable b) => (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
unsafeAccumulate = G.unsafeAccumulate
{-# INLINE unsafeAccumulate #-}

unsafeAccumulate_ :: (Unboxable a, Unboxable b) => (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
unsafeAccumulate_ = G.unsafeAccumulate_
{-# INLINE unsafeAccumulate_ #-}

reverse :: (Unboxable a) => Vector a -> Vector a
reverse = G.reverse
{-# INLINE reverse #-}

backpermute :: (Unboxable a) => Vector a -> Vector Int -> Vector a
backpermute = G.backpermute
{-# INLINE backpermute #-}

unsafeBackpermute :: (Unboxable a) => Vector a -> Vector Int -> Vector a
unsafeBackpermute = G.unsafeBackpermute
{-# INLINE unsafeBackpermute #-}

modify :: (Unboxable a) => (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
modify = G.modify
{-# INLINE modify #-}

indexed :: (Unboxable a) => Vector a -> Vector (Int, a)
indexed = G.indexed
{-# INLINE indexed #-}

map :: (Unboxable a, Unboxable b) => (a -> b) -> Vector a -> Vector b
map = G.map
{-# INLINE map #-}

imap :: (Unboxable a, Unboxable b) => (Int -> a -> b) -> Vector a -> Vector b
imap = G.imap
{-# INLINE imap #-}

concatMap :: (Unboxable a, Unboxable b) => (a -> Vector b) -> Vector a -> Vector b
concatMap = G.concatMap
{-# INLINE concatMap #-}

mapM :: (Monad m, Unboxable a, Unboxable b) => (a -> m b) -> Vector a -> m (Vector b)
mapM = G.mapM
{-# INLINE mapM #-}

imapM :: (Monad m, Unboxable a, Unboxable b) => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM = G.imapM
{-# INLINE imapM #-}

mapM_ :: (Monad m, Unboxable a) => (a -> m b) -> Vector a -> m ()
mapM_ = G.mapM_
{-# INLINE mapM_ #-}

imapM_ :: (Monad m, Unboxable a) => (Int -> a -> m b) -> Vector a -> m ()
imapM_ = G.imapM_
{-# INLINE imapM_ #-}

forM :: (Monad m, Unboxable a, Unboxable b) => Vector a -> (a -> m b) -> m (Vector b)
forM = G.forM
{-# INLINE forM #-}

forM_ :: (Monad m, Unboxable a) => Vector a -> (a -> m b) -> m ()
forM_ = G.forM_
{-# INLINE forM_ #-}

zipWith :: (Unboxable a, Unboxable b, Unboxable c) => (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith = G.zipWith
{-# INLINE zipWith #-}

zipWith3 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 = G.zipWith3
{-# INLINE zipWith3 #-}

zipWith4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
zipWith4 = G.zipWith4
{-# INLINE zipWith4 #-}

zipWith5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
zipWith5 = G.zipWith5
{-# INLINE zipWith5 #-}

zipWith6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f, Unboxable g) => (a -> b -> c -> d -> e -> f -> g) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector g
zipWith6 = G.zipWith6
{-# INLINE zipWith6 #-}

izipWith :: (Unboxable a, Unboxable b, Unboxable c) => (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
izipWith = G.izipWith
{-# INLINE izipWith #-}

izipWith3 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => (Int -> a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
izipWith3 = G.izipWith3
{-# INLINE izipWith3 #-}

izipWith4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => (Int -> a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
izipWith4 = G.izipWith4
{-# INLINE izipWith4 #-}

izipWith5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => (Int -> a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
izipWith5 = G.izipWith5
{-# INLINE izipWith5 #-}

izipWith6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f, Unboxable g) => (Int -> a -> b -> c -> d -> e -> f -> g) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector g
izipWith6 = G.izipWith6
{-# INLINE izipWith6 #-}

zip :: (Unboxable a, Unboxable b) => Vector a -> Vector b -> Vector (a, b)
zip = G.zip
{-# INLINE zip #-}

zip3 :: (Unboxable a, Unboxable b, Unboxable c) => Vector a -> Vector b -> Vector c -> Vector (a, b, c)
zip3 = G.zip3
{-# INLINE zip3 #-}

zip4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => Vector a -> Vector b -> Vector c -> Vector d -> Vector (a, b, c, d)
zip4 = G.zip4
{-# INLINE zip4 #-}

zip5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a, b, c, d, e)
zip5 = G.zip5
{-# INLINE zip5 #-}

zip6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a, b, c, d, e, f)
zip6 = G.zip6
{-# INLINE zip6 #-}

zipWithM :: (Monad m, Unboxable a, Unboxable b, Unboxable c) => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
zipWithM = G.zipWithM
{-# INLINE zipWithM #-}

izipWithM :: (Monad m, Unboxable a, Unboxable b, Unboxable c) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
izipWithM = G.izipWithM
{-# INLINE izipWithM #-}

zipWithM_ :: (Monad m, Unboxable a, Unboxable b) => (a -> b -> m c) -> Vector a -> Vector b -> m ()
zipWithM_ = G.zipWithM_
{-# INLINE zipWithM_ #-}

izipWithM_ :: (Monad m, Unboxable a, Unboxable b) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
izipWithM_ = G.izipWithM_
{-# INLINE izipWithM_ #-}

unzip :: (Unboxable a, Unboxable b) => Vector (a, b) -> (Vector a, Vector b)
unzip = G.unzip
{-# INLINE unzip #-}

unzip3 :: (Unboxable a, Unboxable b, Unboxable c) => Vector (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 = G.unzip3
{-# INLINE unzip3 #-}

unzip4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
unzip4 = G.unzip4
{-# INLINE unzip4 #-}

unzip5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => Vector (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
unzip5 = G.unzip5
{-# INLINE unzip5 #-}

unzip6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => Vector (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
unzip6 = G.unzip6
{-# INLINE unzip6 #-}

filter :: (Unboxable a) => (a -> Bool) -> Vector a -> Vector a
filter = G.filter
{-# INLINE filter #-}

ifilter :: (Unboxable a) => (Int -> a -> Bool) -> Vector a -> Vector a
ifilter = G.ifilter
{-# INLINE ifilter #-}

uniq :: (Eq a, Unboxable a) => Vector a -> Vector a
uniq = G.uniq
{-# INLINE uniq #-}

mapMaybe :: (Unboxable a, Unboxable b) => (a -> Maybe b) -> Vector a -> Vector b
mapMaybe = G.mapMaybe
{-# INLINE mapMaybe #-}

imapMaybe :: (Unboxable a, Unboxable b) => (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe = G.imapMaybe
{-# INLINE imapMaybe #-}

filterM :: (Monad m, Unboxable a) => (a -> m Bool) -> Vector a -> m (Vector a)
filterM = G.filterM
{-# INLINE filterM #-}

takeWhile :: (Unboxable a) => (a -> Bool) -> Vector a -> Vector a
takeWhile = G.takeWhile
{-# INLINE takeWhile #-}

dropWhile :: (Unboxable a) => (a -> Bool) -> Vector a -> Vector a
dropWhile = G.dropWhile
{-# INLINE dropWhile #-}

partition :: (Unboxable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition = G.partition
{-# INLINE partition #-}

unstablePartition :: (Unboxable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
unstablePartition = G.unstablePartition
{-# INLINE unstablePartition #-}

span :: (Unboxable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
span = G.span
{-# INLINE span #-}

break :: (Unboxable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)
break = G.break
{-# INLINE break #-}

elem :: (Eq a, Unboxable a) => a -> Vector a -> Bool
elem = G.elem
{-# INLINE elem #-}

notElem :: (Eq a, Unboxable a) => a -> Vector a -> Bool
notElem = G.notElem
{-# INLINE notElem #-}

find :: (Unboxable a) => (a -> Bool) -> Vector a -> Maybe a
find = G.find
{-# INLINE find #-}

findIndex :: (Unboxable a) => (a -> Bool) -> Vector a -> Maybe Int
findIndex = G.findIndex
{-# INLINE findIndex #-}

findIndices :: (Unboxable a) => (a -> Bool) -> Vector a -> Vector Int
findIndices = G.findIndices
{-# INLINE findIndices #-}

elemIndex :: (Eq a, Unboxable a) => a -> Vector a -> Maybe Int
elemIndex = G.elemIndex
{-# INLINE elemIndex #-}

elemIndices :: (Eq a, Unboxable a) => a -> Vector a -> Vector Int
elemIndices = G.elemIndices
{-# INLINE elemIndices #-}

foldl :: (Unboxable b) => (a -> b -> a) -> a -> Vector b -> a
foldl = G.foldl
{-# INLINE foldl #-}

foldl1 :: (Unboxable a) => (a -> a -> a) -> Vector a -> a
foldl1 = G.foldl1
{-# INLINE foldl1 #-}

foldl' :: (Unboxable b) => (a -> b -> a) -> a -> Vector b -> a
foldl' = G.foldl'
{-# INLINE foldl' #-}

foldl1' :: (Unboxable a) => (a -> a -> a) -> Vector a -> a
foldl1' = G.foldl1'
{-# INLINE foldl1' #-}

foldr :: (Unboxable a) => (a -> b -> b) -> b -> Vector a -> b
foldr = G.foldr
{-# INLINE foldr #-}

foldr1 :: (Unboxable a) => (a -> a -> a) -> Vector a -> a
foldr1 = G.foldr1
{-# INLINE foldr1 #-}

foldr' :: (Unboxable a) => (a -> b -> b) -> b -> Vector a -> b
foldr' = G.foldr'
{-# INLINE foldr' #-}

foldr1' :: (Unboxable a) => (a -> a -> a) -> Vector a -> a
foldr1' = G.foldr1'
{-# INLINE foldr1' #-}

ifoldl :: (Unboxable b) => (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl = G.ifoldl
{-# INLINE ifoldl #-}

ifoldl' :: (Unboxable b) => (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl' = G.ifoldl'
{-# INLINE ifoldl' #-}

ifoldr :: (Unboxable a) => (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr = G.ifoldr
{-# INLINE ifoldr #-}

ifoldr' :: (Unboxable a) => (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr' = G.ifoldr'
{-# INLINE ifoldr' #-}

all :: (Unboxable a) => (a -> Bool) -> Vector a -> Bool
all = G.all
{-# INLINE all #-}

any :: (Unboxable a) => (a -> Bool) -> Vector a -> Bool
any = G.any
{-# INLINE any #-}

and :: Vector Bool -> Bool
and = G.and
{-# INLINE and #-}

or :: Vector Bool -> Bool
or = G.or
{-# INLINE or #-}

sum :: (Num a, Unboxable a) => Vector a -> a
sum = G.sum
{-# INLINE sum #-}

product :: (Num a, Unboxable a) => Vector a -> a
product = G.product
{-# INLINE product #-}

maximum :: (Ord a, Unboxable a) => Vector a -> a
maximum = G.maximum
{-# INLINE maximum #-}

maximumBy :: (Unboxable a) => (a -> a -> Ordering) -> Vector a -> a
maximumBy = G.maximumBy
{-# INLINE maximumBy #-}

minimum :: (Ord a, Unboxable a) => Vector a -> a
minimum = G.minimum
{-# INLINE minimum #-}

minimumBy :: (Unboxable a) => (a -> a -> Ordering) -> Vector a -> a
minimumBy = G.minimumBy
{-# INLINE minimumBy #-}

minIndex :: (Ord a, Unboxable a) => Vector a -> Int
minIndex = G.minIndex
{-# INLINE minIndex #-}

minIndexBy :: (Unboxable a) => (a -> a -> Ordering) -> Vector a -> Int
minIndexBy = G.minIndexBy
{-# INLINE minIndexBy #-}

maxIndex :: (Ord a, Unboxable a) => Vector a -> Int
maxIndex = G.maxIndex
{-# INLINE maxIndex #-}

maxIndexBy :: (Unboxable a) => (a -> a -> Ordering) -> Vector a -> Int
maxIndexBy = G.maxIndexBy
{-# INLINE maxIndexBy #-}

foldM :: (Monad m, Unboxable b) => (a -> b -> m a) -> a -> Vector b -> m a
foldM = G.foldM
{-# INLINE foldM #-}

ifoldM :: (Monad m, Unboxable b) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM = G.ifoldM
{-# INLINE ifoldM #-}

foldM' :: (Monad m, Unboxable b) => (a -> b -> m a) -> a -> Vector b -> m a
foldM' = G.foldM'
{-# INLINE foldM' #-}

ifoldM' :: (Monad m, Unboxable b) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM' = G.ifoldM'
{-# INLINE ifoldM' #-}

fold1M :: (Monad m, Unboxable a) => (a -> a -> m a) -> Vector a -> m a
fold1M = G.fold1M
{-# INLINE fold1M #-}

fold1M' :: (Monad m, Unboxable a) => (a -> a -> m a) -> Vector a -> m a
fold1M' = G.fold1M'
{-# INLINE fold1M' #-}

foldM_ :: (Monad m, Unboxable b) => (a -> b -> m a) -> a -> Vector b -> m ()
foldM_ = G.foldM_
{-# INLINE foldM_ #-}

ifoldM_ :: (Monad m, Unboxable b) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM_ = G.ifoldM_
{-# INLINE ifoldM_ #-}

foldM'_ :: (Monad m, Unboxable b) => (a -> b -> m a) -> a -> Vector b -> m ()
foldM'_ = G.foldM'_
{-# INLINE foldM'_ #-}

ifoldM'_ :: (Monad m, Unboxable b) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM'_ = G.ifoldM'_
{-# INLINE ifoldM'_ #-}

fold1M_ :: (Monad m, Unboxable a) => (a -> a -> m a) -> Vector a -> m ()
fold1M_ = G.fold1M_
{-# INLINE fold1M_ #-}

fold1M'_ :: (Monad m, Unboxable a) => (a -> a -> m a) -> Vector a -> m ()
fold1M'_ = G.fold1M'_
{-# INLINE fold1M'_ #-}

prescanl :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a
prescanl = G.prescanl
{-# INLINE prescanl #-}

prescanl' :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a
prescanl' = G.prescanl'
{-# INLINE prescanl' #-}

postscanl :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a
postscanl = G.postscanl
{-# INLINE postscanl #-}

postscanl' :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a
postscanl' = G.postscanl'
{-# INLINE postscanl' #-}

scanl :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a
scanl = G.scanl
{-# INLINE scanl #-}

scanl' :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a
scanl' = G.scanl'
{-# INLINE scanl' #-}

scanl1 :: (Unboxable a) => (a -> a -> a) -> Vector a -> Vector a
scanl1 = G.scanl1
{-# INLINE scanl1 #-}

scanl1' :: (Unboxable a) => (a -> a -> a) -> Vector a -> Vector a
scanl1' = G.scanl1'
{-# INLINE scanl1' #-}

iscanl :: (Unboxable a, Unboxable b) => (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl = G.iscanl
{-# INLINE iscanl #-}

iscanl' :: (Unboxable a, Unboxable b) => (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl' = G.iscanl'
{-# INLINE iscanl' #-}

prescanr :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b
prescanr = G.prescanr
{-# INLINE prescanr #-}

prescanr' :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b
prescanr' = G.prescanr'
{-# INLINE prescanr' #-}

postscanr :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b
postscanr = G.postscanr
{-# INLINE postscanr #-}

postscanr' :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b
postscanr' = G.postscanr'
{-# INLINE postscanr' #-}

scanr :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b
scanr = G.scanr
{-# INLINE scanr #-}

scanr' :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b
scanr' = G.scanr'
{-# INLINE scanr' #-}

scanr1 :: (Unboxable a) => (a -> a -> a) -> Vector a -> Vector a
scanr1 = G.scanr1
{-# INLINE scanr1 #-}

scanr1' :: (Unboxable a) => (a -> a -> a) -> Vector a -> Vector a
scanr1' = G.scanr1'
{-# INLINE scanr1' #-}

iscanr :: (Unboxable a, Unboxable b) => (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr = G.iscanr
{-# INLINE iscanr #-}

iscanr' :: (Unboxable a, Unboxable b) => (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr' = G.iscanr'
{-# INLINE iscanr' #-}

toList :: (Unboxable a) => Vector a -> [a]
toList = G.toList
{-# INLINE toList #-}

fromList :: (Unboxable a) => [a] -> Vector a
fromList = G.fromList
{-# INLINE fromList #-}

fromListN :: (Unboxable a) => Int -> [a] -> Vector a
fromListN = G.fromListN
{-# INLINE fromListN #-}

freeze :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> m (Vector a)
freeze = G.freeze
{-# INLINE freeze #-}

thaw :: (PrimMonad m, Unboxable a) => Vector a -> m (MVector (PrimState m) a)
thaw = G.thaw
{-# INLINE thaw #-}

copy :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Vector a -> m ()
copy = G.copy
{-# INLINE copy #-}

unsafeFreeze :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> m (Vector a)
unsafeFreeze = G.unsafeFreeze
{-# INLINE unsafeFreeze #-}

unsafeThaw :: (PrimMonad m, Unboxable a) => Vector a -> m (MVector (PrimState m) a)
unsafeThaw = G.unsafeThaw
{-# INLINE unsafeThaw #-}

unsafeCopy :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Vector a -> m ()
unsafeCopy = G.unsafeCopy
{-# INLINE unsafeCopy #-}
