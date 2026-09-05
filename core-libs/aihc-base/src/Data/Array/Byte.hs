{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Array.Byte
  ( ByteArray (..),
    MutableByteArray (..),
  )
where

import Data.Data (Data (..), mkNoRepType)
import GHC.Classes (Eq (..), Ord (..))
import GHC.Err (errorWithoutStackTrace)
import GHC.IsList (IsList (..))
import GHC.Num (Num (..))
import GHC.Prim (ByteArray#, MutableByteArray#, compareByteArrays#, indexWord8Array#, newByteArray#, sizeofByteArray#, unsafeFreezeByteArray#, writeWord8Array#, (+#), (-#), (<#), (==#))
import GHC.ST (ST (..), runST)
import GHC.Types (Bool (..), Int (..), Ordering (..), isTrue#)
import GHC.Word (Word8 (..))

-- | Lifted wrapper for an immutable primitive byte array.
--
-- The primitive 'ByteArray#' is unlifted, so it cannot be stored directly in
-- ordinary lifted data structures.  This wrapper is the public representation
-- used by @base@ and packages such as @deepseq@.
data ByteArray = ByteArray ByteArray#

-- | Lifted wrapper for a mutable primitive byte array in state thread @s@.
--
-- Keeping @s@ on both layers prevents a mutable array from escaping the state
-- thread that owns it.
data MutableByteArray s = MutableByteArray (MutableByteArray# s)

-- | The length of a byte array in bytes.
sizeofByteArray :: ByteArray -> Int
sizeofByteArray (ByteArray bytes#) = I# (sizeofByteArray# bytes#)

-- | Compare two byte arrays lexicographically by their bytes. A shorter
-- prefix is less than the longer array.
compareByteArrays :: ByteArray -> ByteArray -> Ordering
compareByteArrays left@(ByteArray left#) right@(ByteArray right#) =
  case (sizeofByteArray left, sizeofByteArray right) of
    (I# leftLength#, I# rightLength#) ->
      let common# = if isTrue# (leftLength# <# rightLength#) then leftLength# else rightLength#
          result# = compareByteArrays# left# 0# right# 0# common#
       in if isTrue# (result# <# 0#)
            then LT
            else
              if isTrue# (result# ==# 0#)
                then compare (I# leftLength#) (I# rightLength#)
                else GT

instance Eq ByteArray where
  left == right = case compareByteArrays left right of
    EQ -> True
    _ -> False

instance Ord ByteArray where
  compare = compareByteArrays

instance IsList ByteArray where
  type Item ByteArray = Word8
  fromList bytes = fromListN (lengthList bytes) bytes
  fromListN (I# n#) bytes =
    runST
      ( ST
          ( \s0 ->
              case newByteArray# n# s0 of
                (# s1, marr# #) ->
                  let fill _ [] s = s
                      fill i# (W8# byte# : rest) s = fill (i# +# 1#) rest (writeWord8Array# marr# i# byte# s)
                   in case fill 0# bytes s1 of
                        s2 -> case unsafeFreezeByteArray# marr# s2 of
                          (# s3, arr# #) -> (# s3, ByteArray arr# #)
          )
      )
  toList bytes@(ByteArray arr#) = go 0#
    where
      !(I# n#) = sizeofByteArray bytes
      go i# =
        if isTrue# (i# <# n#)
          then W8# (indexWord8Array# arr# i#) : go (i# +# 1#)
          else []

lengthList :: [a] -> Int
lengthList = go 0
  where
    go :: Int -> [b] -> Int
    go n [] = n
    go n (_ : rest) = go (n + 1) rest

-- | A byte array has no generic representation.
instance Data ByteArray where
  toConstr _ = errorWithoutStackTrace "Data.Array.Byte.toConstr"
  gunfold _ _ _ = errorWithoutStackTrace "Data.Array.Byte.gunfold"
  dataTypeOf _ = mkNoRepType "Data.Array.Byte.ByteArray"
