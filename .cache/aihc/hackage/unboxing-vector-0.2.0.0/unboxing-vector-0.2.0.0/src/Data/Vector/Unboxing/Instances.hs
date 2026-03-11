{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Vector.Unboxing.Instances () where
import Data.Vector.Unboxing.Internal
import qualified Data.Vector.Generic as G

#if defined(ENABLE_MONO_TRAVERSABLE)

import qualified Data.MonoTraversable -- from mono-traversable
import qualified Data.Sequences       -- from mono-traversable

--
-- Classes from mono-traversable
--

type instance Data.MonoTraversable.Element (Vector a) = a

instance (Unboxable a) => Data.MonoTraversable.MonoFunctor (Vector a) where
  omap = G.map
  {-# INLINE omap #-}

instance (Unboxable a) => Data.MonoTraversable.MonoFoldable (Vector a) where
  ofoldMap f = G.foldr (mappend . f) mempty
  ofoldr = G.foldr
  ofoldl' = G.foldl'
  otoList = G.toList
  oall = G.all
  oany = G.any
  onull = G.null
  olength = G.length
  olength64 = fromIntegral . G.length
  -- ocompareLength : use default
  -- otraverse_ : use default
  -- ofor_ : use default
  -- omapM_ : use default (G.mapM_ requires a Monad, unfortunately)
  -- oforM_ : use default (G.forM_ requires a Monad, unfortunately)
  ofoldlM = G.foldM
  -- ofoldMap1Ex : use default
  ofoldr1Ex = G.foldr1
  ofoldl1Ex' = G.foldl1'
  headEx = G.head
  lastEx = G.last
  unsafeHead = G.unsafeHead
  unsafeLast = G.unsafeLast
  maximumByEx = G.maximumBy
  minimumByEx = G.minimumBy
  oelem = G.elem
  onotElem = G.notElem
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE otoList #-}
  {-# INLINE oall #-}
  {-# INLINE oany #-}
  {-# INLINE onull #-}
  {-# INLINE olength #-}
  {-# INLINE olength64 #-}
  {-# INLINE ofoldlM #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  {-# INLINE headEx #-}
  {-# INLINE lastEx #-}
  {-# INLINE unsafeHead #-}
  {-# INLINE unsafeLast #-}
  {-# INLINE maximumByEx #-}
  {-# INLINE minimumByEx #-}
  {-# INLINE oelem #-}
  {-# INLINE onotElem #-}

instance (Unboxable a) => Data.MonoTraversable.MonoTraversable (Vector a) where
  otraverse f v = let !n = G.length v
                  in G.fromListN n <$> traverse f (G.toList v)
  omapM = Data.MonoTraversable.otraverse
  {-# INLINE otraverse #-}
  {-# INLINE omapM #-}

instance (Unboxable a) => Data.MonoTraversable.MonoPointed (Vector a) where
  opoint = G.singleton
  {-# INLINE opoint #-}

instance (Unboxable a) => Data.MonoTraversable.GrowingAppend (Vector a)

instance (Unboxable a) => Data.Sequences.SemiSequence (Vector a) where
  type Index (Vector a) = Int
  intersperse = Data.Sequences.defaultIntersperse
  reverse = G.reverse
  find = G.find
  sortBy = Data.Sequences.vectorSortBy
  cons = G.cons
  snoc = G.snoc
  {-# INLINE intersperse #-}
  {-# INLINE reverse #-}
  {-# INLINE find #-}
  {-# INLINE sortBy #-}
  {-# INLINE cons #-}
  {-# INLINE snoc #-}

instance (Unboxable a) => Data.Sequences.IsSequence (Vector a) where
  fromList = G.fromList
  lengthIndex = G.length
  break = G.break
  span = G.span
  dropWhile = G.dropWhile
  takeWhile = G.takeWhile
  splitAt = G.splitAt
  -- unsafeSplitAt : use default
  take = G.take
  unsafeTake = G.unsafeTake
  drop = G.drop
  unsafeDrop = G.unsafeDrop
  -- dropEnd : use default
  partition = G.partition
  uncons v | G.null v = Nothing
           | otherwise = Just (G.head v, G.tail v)
  unsnoc v | G.null v = Nothing
           | otherwise = Just (G.init v, G.last v)
  filter = G.filter
  filterM = G.filterM
  replicate = G.replicate
  replicateM = G.replicateM
  -- groupBy : use default
  -- groupAllOn : use default
  -- subsequences : use default
  -- permutations : use default
  tailEx = G.tail
  -- tailMay : use default
  initEx = G.init
  -- initMay : use default
  unsafeTail = G.unsafeTail
  unsafeInit = G.unsafeInit
  index = (G.!?)
  indexEx = (G.!)
  unsafeIndex = G.unsafeIndex
  -- splitWhen : use default
  {-# INLINE fromList #-}
  {-# INLINE lengthIndex #-}
  {-# INLINE break #-}
  {-# INLINE span #-}
  {-# INLINE dropWhile #-}
  {-# INLINE takeWhile #-}
  {-# INLINE splitAt #-}
  {-# INLINE take #-}
  {-# INLINE unsafeTake #-}
  {-# INLINE drop #-}
  {-# INLINE unsafeDrop #-}
  {-# INLINE partition #-}
  {-# INLINE uncons #-}
  {-# INLINE unsnoc #-}
  {-# INLINE filter #-}
  {-# INLINE filterM #-}
  {-# INLINE replicate #-}
  {-# INLINE replicateM #-}
  {-# INLINE tailEx #-}
  {-# INLINE initEx #-}
  {-# INLINE unsafeTail #-}
  {-# INLINE unsafeInit #-}
  {-# INLINE index #-}
  {-# INLINE indexEx #-}
  {-# INLINE unsafeIndex #-}

#endif
