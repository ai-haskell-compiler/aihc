{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Chunks
  ( Chunks (..)
  , null
  , reverse
  , reverseOnto
  , copy
  , copyReverse
  , concat
  , concatReverse

    -- * Indexing
  , index

    -- * Traversals
  , map'

    -- * Construction
  , singleton
  , doubleton
  , tripleton
  , quadrupleton
  , quintupleton
  , sextupleton
  , septupleton
  , octupleton
  , nonupleton
  , decupleton
  , undecupleton
  , duodecupleton

    -- * Construction Alternate
  , construct1
  , construct2
  , construct3
  , construct4
  , construct5
  , construct6
  , construct7
  , construct8
  , construct9
  , construct10
  , construct11
  , construct12
  ) where

import Prelude hiding (concat, null, reverse)

import Control.Monad.ST.Run (runSmallArrayST)
import Data.Primitive (SmallArray (..), SmallMutableArray (..))
import GHC.Exts (Int (I#), Int#, IsList, SmallArray#, SmallMutableArray#, State#, (+#), (-#))
import GHC.ST (ST (..))

import qualified Data.Foldable as F
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

{- | A list of chunks. This is a foundation on top of
which efficient builder-like abstractions can be
implemented. There are no restrictions on the number
of elements in each chunk, although extremely small
chunks (singleton or doubleton chunks) may lead to
poor performance.
-}
data Chunks a
  = ChunksCons !(SmallArray a) !(Chunks a)
  | ChunksNil
  deriving stock (Show)

instance (Eq a) => Eq (Chunks a) where
  (==) = eqChunks

instance IsList (Chunks a) where
  type Item (Chunks a) = SmallArray a
  toList = chunksToSmallArrayList
  fromList xs = F.foldr ChunksCons ChunksNil xs

-- | Are there any elements in the chunked list?
null :: Chunks a -> Bool
null = go
 where
  go ChunksNil = True
  go (ChunksCons x xs) = case PM.sizeofSmallArray x of
    0 -> go xs
    _ -> False

{- | Indexing into the chunked list, returning @Nothing@ if there
are not enough elements.
-}
index :: Chunks a -> Int -> Maybe a
index cs0 !ix0 = go cs0 ix0
 where
  go ChunksNil !_ = Nothing
  go (ChunksCons x xs) !ix =
    let !len = PM.sizeofSmallArray x
     in if ix < len
          then case PM.indexSmallArray## x ix of
            (# v #) -> Just v
          else go xs (ix - len)

chunksToSmallArrayList :: Chunks a -> [SmallArray a]
chunksToSmallArrayList ChunksNil = []
chunksToSmallArrayList (ChunksCons x xs) =
  x : chunksToSmallArrayList xs

eqChunks :: (Eq a) => Chunks a -> Chunks a -> Bool
eqChunks ChunksNil cs = allEmpty cs
eqChunks (ChunksCons x xs) cs = eqChunksCons x 0 (PM.sizeofSmallArray x) xs cs

-- The first argument chunk belongs to the first argument chunks.
-- It is its head.
eqChunksCons :: (Eq a) => SmallArray a -> Int -> Int -> Chunks a -> Chunks a -> Bool
eqChunksCons !_ !_ !len xs ChunksNil = case len of
  0 -> allEmpty xs
  _ -> False
eqChunksCons x !off !len xs (ChunksCons y ys) =
  eqChunksConsBoth x off len y 0 (PM.sizeofSmallArray y) xs ys

eqChunksConsBoth :: (Eq a) => SmallArray a -> Int -> Int -> SmallArray a -> Int -> Int -> Chunks a -> Chunks a -> Bool
eqChunksConsBoth !xh !xoff !xlen !yh !yoff !ylen !xt !yt = case compare xlen ylen of
  LT -> eqRange xh xoff yh yoff xlen && eqChunksCons yh (yoff + xlen) (ylen - xlen) yt xt
  GT -> eqRange xh xoff yh yoff ylen && eqChunksCons xh (xoff + ylen) (xlen - ylen) xt yt
  EQ -> eqRange xh xoff yh yoff xlen && eqChunks xt yt

eqRange :: (Eq a) => SmallArray a -> Int -> SmallArray a -> Int -> Int -> Bool
eqRange !xs !xoff !ys !yoff !len
  | len == 0 = True
  | otherwise =
      PM.indexSmallArray xs xoff == PM.indexSmallArray ys yoff
        && eqRange xs (xoff + 1) ys (yoff + 1) (len - 1)

allEmpty :: Chunks a -> Bool
allEmpty ChunksNil = True
allEmpty (ChunksCons x xs) = case PM.sizeofSmallArray x of
  0 -> allEmpty xs
  _ -> False

instance Semigroup (Chunks a) where
  ChunksNil <> a = a
  cs@(ChunksCons _ _) <> ChunksNil = cs
  as@(ChunksCons _ _) <> bs@(ChunksCons _ _) =
    reverseOnto bs (reverse as)

instance Monoid (Chunks a) where
  mempty = ChunksNil

instance Foldable Chunks where
  {-# INLINE foldl' #-}
  {-# INLINE foldr #-}
  {-# INLINE length #-}
  foldl' = chunksFoldl'
  foldr = chunksFoldr
  length = chunksLength

chunksFoldl' :: (b -> a -> b) -> b -> Chunks a -> b
{-# INLINE chunksFoldl' #-}
chunksFoldl' f = go
 where
  go !acc ChunksNil = acc
  go !acc (ChunksCons x cs) = go (F.foldl' f acc x) cs

chunksFoldr :: (a -> b -> b) -> b -> Chunks a -> b
{-# INLINE chunksFoldr #-}
chunksFoldr f z0 = go
 where
  go ChunksNil = z0
  go (ChunksCons x cs) = F.foldr f (go cs) x

chunksLength :: Chunks a -> Int
{-# INLINE chunksLength #-}
chunksLength = chunksLengthGo 0

chunksLengthGo :: Int -> Chunks a -> Int
chunksLengthGo !n ChunksNil = n
chunksLengthGo !n (ChunksCons c cs) =
  chunksLengthGo (n + PM.sizeofSmallArray c) cs

{- | Reverse chunks but not the elements within each
chunk.

>>> reverse [[42,17,94],[6,12],[3,14]]
[[3,14],[6,12],[42,17,94]]
-}
reverse :: Chunks a -> Chunks a
reverse = reverseOnto ChunksNil

{- | Variant of 'reverse' that allows the caller to provide
an initial list of chunks that the reversed chunks will
be pushed onto.

>>> reverseOnto [[15],[12,4]] [[42,17,94],[6,12],[3,14]]
[[3,14],[6,12],[42,17,94],[15],[12,4]]
-}
reverseOnto :: Chunks a -> Chunks a -> Chunks a
reverseOnto !x ChunksNil = x
reverseOnto !x (ChunksCons y ys) =
  reverseOnto (ChunksCons y x) ys

{- | Copy the contents of the chunks into a mutable array.
Precondition: The destination must have enough space to
house the contents. This is not checked.

> dest (before): [x,x,x,x,x,x,x,x,x,x,x,x]
> copy dest 2 [[X,Y,Z],[A,B],[C,D]] (returns 9)
> dest (after):  [x,x,X,Y,Z,A,B,C,D,x,x,x]
-}
copy ::
  -- | Destination
  SmallMutableArray s a ->
  -- | Destination offset
  Int ->
  -- | Source
  Chunks a ->
  -- | Returns the next index into the destination after the payload
  ST s Int
{-# INLINE copy #-}
copy (SmallMutableArray dst) (I# off) cs =
  ST
    ( \s0 -> case copy# dst off cs s0 of
        (# s1, nextOff #) -> (# s1, I# nextOff #)
    )

copy# :: SmallMutableArray# s a -> Int# -> Chunks a -> State# s -> (# State# s, Int# #)
copy# _ off ChunksNil s0 = (# s0, off #)
copy# marr off (ChunksCons (SmallArray c) cs) s0 =
  let !sz = Exts.sizeofSmallArray# c
   in case Exts.copySmallArray# c 0# marr off sz s0 of
        s1 -> copy# marr (off +# sz) cs s1

{- | Copy the contents of the chunks into a mutable array,
reversing the order of the chunks. Precondition: The
destination must have enough space to house the contents.
This is not checked.

> dest (before): [x,x,x,x,x,x,x,x,x,x,x,x]
> copyReverse dest 10 [[X,Y,Z],[A,B],[C,D]] (returns 3)
> dest (after):  [x,x,x,C,D,A,B,X,Y,Z,x,x]
-}
copyReverse ::
  -- | Destination
  SmallMutableArray s a ->
  -- | Destination range successor
  Int ->
  -- | Source
  Chunks a ->
  -- | Returns the next index into the destination after the payload
  ST s Int
{-# INLINE copyReverse #-}
copyReverse (SmallMutableArray dst) (I# off) cs =
  ST
    ( \s0 -> case copyReverse# dst off cs s0 of
        (# s1, nextOff #) -> (# s1, I# nextOff #)
    )

copyReverse# :: SmallMutableArray# s a -> Int# -> Chunks a -> State# s -> (# State# s, Int# #)
copyReverse# _ off ChunksNil s0 = (# s0, off #)
copyReverse# marr prevOff (ChunksCons (SmallArray c) cs) s0 =
  let !sz = Exts.sizeofSmallArray# c
      !off = prevOff -# sz
   in case Exts.copySmallArray# c 0# marr off sz s0 of
        s1 -> copyReverse# marr off cs s1

concat :: Chunks a -> SmallArray a
{-# INLINE concat #-}
concat x = SmallArray (concat# x)

concat# :: Chunks a -> SmallArray# a
{-# NOINLINE concat# #-}
concat# ChunksNil = case mempty of SmallArray x -> x
concat# (ChunksCons c cs) = case cs of
  ChunksNil -> case c of SmallArray x -> x
  ChunksCons d ds -> unSmallArray $ runSmallArrayST $ do
    let szc = PM.sizeofSmallArray c
        szd = PM.sizeofSmallArray d
        szboth = szc + szd
        len = chunksLengthGo szboth ds
    dst <- PM.newSmallArray len errorThunk
    PM.copySmallArray dst 0 c 0 szc
    PM.copySmallArray dst szc d 0 szd
    _ <- copy dst szboth ds
    PM.unsafeFreezeSmallArray dst

concatReverse :: Chunks a -> SmallArray a
{-# INLINE concatReverse #-}
concatReverse x = SmallArray (concatReverse# x)

concatReverse# :: Chunks a -> SmallArray# a
{-# NOINLINE concatReverse# #-}
concatReverse# ChunksNil = case mempty of SmallArray x -> x
concatReverse# (ChunksCons c cs) = case cs of
  ChunksNil -> case c of SmallArray x -> x
  ChunksCons d ds -> unSmallArray $ runSmallArrayST $ do
    let szc = PM.sizeofSmallArray c
        szd = PM.sizeofSmallArray d
        szboth = szc + szd
        len = chunksLengthGo szboth ds
    dst <- PM.newSmallArray len errorThunk
    PM.copySmallArray dst (len - szc) c 0 szc
    PM.copySmallArray dst (len - (szc + szd)) d 0 szd
    _ <- copyReverse dst (len - (szc + szd)) ds
    PM.unsafeFreezeSmallArray dst

unSmallArray :: SmallArray a -> SmallArray# a
unSmallArray (SmallArray x) = x

errorThunk :: a
{-# NOINLINE errorThunk #-}
errorThunk = error "Data.Chunks: mistake"

{- | Mapping over chunks is a little unusual in that the result
is just a 'SmallArray'.
-}
map' :: (a -> b) -> Chunks a -> SmallArray b
{-# INLINE map' #-}
map' f cs = runSmallArrayST $ do
  dst <- PM.newSmallArray len errorThunk
  !_ <-
    F.foldlM
      ( \ !ix a -> do
          let !b = f a
          PM.writeSmallArray dst ix b
          pure (ix + 1)
      )
      0
      cs
  PM.unsafeFreezeSmallArray dst
 where
  !len = chunksLength cs

construct1 :: a -> Chunks a
{-# INLINE construct1 #-}
construct1 = singleton

construct2 :: a -> a -> Chunks a
{-# INLINE construct2 #-}
construct2 = doubleton

construct3 :: a -> a -> a -> Chunks a
{-# INLINE construct3 #-}
construct3 = tripleton

construct4 :: a -> a -> a -> a -> Chunks a
{-# INLINE construct4 #-}
construct4 = quadrupleton

construct5 :: a -> a -> a -> a -> a -> Chunks a
{-# INLINE construct5 #-}
construct5 = quintupleton

construct6 :: a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE construct6 #-}
construct6 = sextupleton

construct7 :: a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE construct7 #-}
construct7 = septupleton

construct8 :: a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE construct8 #-}
construct8 = octupleton

construct9 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE construct9 #-}
construct9 = nonupleton

construct10 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE construct10 #-}
construct10 = decupleton

construct11 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE construct11 #-}
construct11 = undecupleton

construct12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE construct12 #-}
construct12 = duodecupleton

-- | Create chunks with 1 element.
singleton :: a -> Chunks a
{-# INLINE singleton #-}
singleton a =
  ChunksCons
    ( runSmallArrayST (PM.newSmallArray 1 a >>= PM.unsafeFreezeSmallArray)
    )
    ChunksNil

-- | Create chunks with 2 elements.
doubleton :: a -> a -> Chunks a
{-# INLINE doubleton #-}
doubleton a b =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 2 a
        PM.writeSmallArray dst 1 b
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 3 elements.
tripleton :: a -> a -> a -> Chunks a
{-# INLINE tripleton #-}
tripleton a b c =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 3 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 4 elements.
quadrupleton :: a -> a -> a -> a -> Chunks a
{-# INLINE quadrupleton #-}
quadrupleton a b c d =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 4 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 5 elements.
quintupleton :: a -> a -> a -> a -> a -> Chunks a
{-# INLINE quintupleton #-}
quintupleton a b c d e =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 5 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.writeSmallArray dst 4 e
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 6 elements.
sextupleton :: a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE sextupleton #-}
sextupleton a b c d e f =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 6 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.writeSmallArray dst 4 e
        PM.writeSmallArray dst 5 f
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 7 elements.
septupleton :: a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE septupleton #-}
septupleton a b c d e f g =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 7 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.writeSmallArray dst 4 e
        PM.writeSmallArray dst 5 f
        PM.writeSmallArray dst 6 g
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 8 elements.
octupleton :: a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE octupleton #-}
octupleton a b c d e f g h =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 8 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.writeSmallArray dst 4 e
        PM.writeSmallArray dst 5 f
        PM.writeSmallArray dst 6 g
        PM.writeSmallArray dst 7 h
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 9 elements.
nonupleton :: a -> a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE nonupleton #-}
nonupleton a b c d e f g h i =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 9 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.writeSmallArray dst 4 e
        PM.writeSmallArray dst 5 f
        PM.writeSmallArray dst 6 g
        PM.writeSmallArray dst 7 h
        PM.writeSmallArray dst 8 i
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 10 elements.
decupleton :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE decupleton #-}
decupleton a b c d e f g h i j =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 10 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.writeSmallArray dst 4 e
        PM.writeSmallArray dst 5 f
        PM.writeSmallArray dst 6 g
        PM.writeSmallArray dst 7 h
        PM.writeSmallArray dst 8 i
        PM.writeSmallArray dst 9 j
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 11 elements.
undecupleton :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE undecupleton #-}
undecupleton a b c d e f g h i j k =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 11 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.writeSmallArray dst 4 e
        PM.writeSmallArray dst 5 f
        PM.writeSmallArray dst 6 g
        PM.writeSmallArray dst 7 h
        PM.writeSmallArray dst 8 i
        PM.writeSmallArray dst 9 j
        PM.writeSmallArray dst 10 k
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil

-- | Create chunks with 12 elements.
duodecupleton :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Chunks a
{-# INLINE duodecupleton #-}
duodecupleton a b c d e f g h i j k l =
  ChunksCons
    ( runSmallArrayST $ do
        dst <- PM.newSmallArray 12 a
        PM.writeSmallArray dst 1 b
        PM.writeSmallArray dst 2 c
        PM.writeSmallArray dst 3 d
        PM.writeSmallArray dst 4 e
        PM.writeSmallArray dst 5 f
        PM.writeSmallArray dst 6 g
        PM.writeSmallArray dst 7 h
        PM.writeSmallArray dst 8 i
        PM.writeSmallArray dst 9 j
        PM.writeSmallArray dst 10 k
        PM.writeSmallArray dst 11 l
        PM.unsafeFreezeSmallArray dst
    )
    ChunksNil
