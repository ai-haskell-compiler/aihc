{-# OPTIONS_GHC -Wno-orphans #-}

module PrimitiveExtras.Bitmap
  ( Bitmap (..),
    empty,
    singleton,
    insert,
    invert,
    indexList,
    boolList,
    pair,
    populatedIndex,
    isPopulated,
    population,
    null,
    bits,
    populatedIndicesList,
    int64,
    allBitsList,
    allBitsUnfoldl,
    populatedBitsUnfoldl,
    indicesAmongstPopulatedBitsUnfoldl,
  )
where

import qualified DeferredFolds.Unfoldl as Unfoldl
import PrimitiveExtras.Prelude hiding (empty, insert, null, singleton, traverse_)
import PrimitiveExtras.Types

deriving instance Eq Bitmap

{-# NOINLINE maxSize #-}
maxSize :: Int
maxSize = finiteBitSize (undefined :: Int)

{-# NOINLINE maxBit #-}
maxBit :: Int
maxBit = pred maxSize

{-# NOINLINE allBitsList #-}
allBitsList :: [Int]
allBitsList = [0 .. maxBit]

-- * Constructors

-------------------------

{-# INLINE empty #-}
empty :: Bitmap
empty = Bitmap 0

{-# INLINE singleton #-}
singleton :: Int -> Bitmap
singleton = Bitmap . bit

{-# INLINE insert #-}
insert :: Int -> Bitmap -> Bitmap
insert i = Bitmap . (bit i .|.) . int64

{-# INLINE invert #-}
invert :: Int -> Bitmap -> Bitmap
invert i = Bitmap . (bit i `xor`) . int64

{-# INLINE indexList #-}
indexList :: [Int] -> Bitmap
indexList = Bitmap . foldr (.|.) 0 . map bit

{-# INLINE boolList #-}
boolList :: [Bool] -> Bitmap
boolList = Bitmap . foldr (.|.) 0 . zipWith (\index -> bool 0 (bit index)) allBitsList

{-# INLINE pair #-}
pair :: Int -> Int -> Bitmap
pair i1 i2 = Bitmap (bit i1 .|. bit i2)

-- * Accessors

-------------------------

-- |
-- A number of non-zero bits, preceding this one.
{-# INLINE populatedIndex #-}
populatedIndex :: Int -> Bitmap -> Int
populatedIndex i (Bitmap int) = popCount (int .&. (bit i - 1))

{-# INLINE isPopulated #-}
isPopulated :: Int -> Bitmap -> Bool
isPopulated index (Bitmap int) = testBit int index

{-# INLINE population #-}
population :: Bitmap -> Int
population (Bitmap int) = popCount int

{-# INLINE null #-}
null :: Bitmap -> Bool
null (Bitmap int) = int == 0

{-# INLINE bits #-}
bits :: Bitmap -> [Int]
bits (Bitmap int) = filter (testBit int) allBitsList

{-# INLINE populatedIndicesList #-}
populatedIndicesList :: Bitmap -> [Int]
populatedIndicesList = enumFromTo 0 . pred . population

{-# INLINE int64 #-}
int64 :: Bitmap -> Int64
int64 (Bitmap int) = int

{-# NOINLINE allBitsUnfoldl #-}
allBitsUnfoldl :: Unfoldl Int
allBitsUnfoldl = Unfoldl.intsInRange 0 maxBit

populatedBitsUnfoldl :: Bitmap -> Unfoldl Int
populatedBitsUnfoldl bitmap = Unfoldl.filter (flip isPopulated bitmap) allBitsUnfoldl

indicesAmongstPopulatedBitsUnfoldl :: Bitmap -> Unfoldl Int
indicesAmongstPopulatedBitsUnfoldl bitmap = Unfoldl.intsInRange 0 (pred (population bitmap))
