{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language DerivingStrategies #-}

module Data.Tuple.Types
  ( -- * Pairs
    DoublePair(..)
  , IntPair(..)
  , WordPair(..)
  , ByteArrayPair(..)
    -- * Triples
  , DoubleTriple(..)
  , IntTriple(..)
  , WordTriple(..)
  , ByteArrayTriple(..)
  ) where

import Data.Primitive (ByteArray)
import Data.Primitive.Types (Prim)
import GHC.Exts ((+#),(*#))

import qualified Data.Primitive.Types as P

data DoublePair = DoublePair
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
  deriving stock (Eq,Ord,Show)
data DoubleTriple = DoubleTriple
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
  deriving stock (Eq,Ord,Show)

data IntPair = IntPair
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  deriving stock (Eq,Ord,Show)
data IntTriple = IntTriple
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  deriving stock (Eq,Ord,Show)

data WordPair = WordPair
  {-# UNPACK #-} !Word
  {-# UNPACK #-} !Word
  deriving stock (Eq,Ord,Show)
data WordTriple = WordTriple
  {-# UNPACK #-} !Word
  {-# UNPACK #-} !Word
  {-# UNPACK #-} !Word
  deriving stock (Eq,Ord,Show)

data ByteArrayPair = ByteArrayPair
  {-# UNPACK #-} !ByteArray
  {-# UNPACK #-} !ByteArray
  deriving stock (Eq,Ord,Show)
data ByteArrayTriple = ByteArrayTriple
  {-# UNPACK #-} !ByteArray
  {-# UNPACK #-} !ByteArray
  {-# UNPACK #-} !ByteArray
  deriving stock (Eq,Ord,Show)

instance Prim IntTriple where
  {-# inline sizeOf# #-}
  {-# inline alignment# #-}
  {-# inline indexByteArray# #-}
  {-# inline readByteArray# #-}
  {-# inline writeByteArray# #-}
  {-# inline setByteArray# #-}
  {-# inline indexOffAddr# #-}
  {-# inline readOffAddr# #-}
  {-# inline writeOffAddr# #-}
  {-# inline setOffAddr# #-}
  sizeOf# _ = 3# *# P.sizeOf# (undefined :: Int)
  alignment# _ = P.alignment# (undefined :: Int)
  indexByteArray# arr# i# = IntTriple
    (P.indexByteArray# arr# (3# *# i#))
    (P.indexByteArray# arr# ((3# *# i#) +# 1#))
    (P.indexByteArray# arr# ((3# *# i#) +# 2#))
  readByteArray# arr# i# =
    \s0 -> case P.readByteArray# arr# (3# *# i#) s0 of
      (# s1, i0 #) -> case P.readByteArray# arr# ((3# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> case P.readByteArray# arr# ((3# *# i#) +# 2#) s2 of
          (# s3, i2 #) -> (# s3, IntTriple i0 i1 i2 #)
  writeByteArray# arr# i# (IntTriple a b c) =
    \s0 -> case P.writeByteArray# arr# (3# *# i#) a s0 of
       s1 -> case P.writeByteArray# arr# ((3# *# i#) +# 1#) b s1 of
         s2 -> case P.writeByteArray# arr# ((3# *# i#) +# 2# ) c s2 of
           s3 -> s3
  setByteArray# = P.defaultSetByteArray#
  indexOffAddr# arr# i# = IntTriple
    (P.indexOffAddr# arr# (3# *# i#))
    (P.indexOffAddr# arr# ((3# *# i#) +# 1#))
    (P.indexOffAddr# arr# ((3# *# i#) +# 2#))
  readOffAddr# arr# i# =
    \s0 -> case P.readOffAddr# arr# (3# *# i#) s0 of
      (# s1, i0 #) -> case P.readOffAddr# arr# ((3# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> case P.readOffAddr# arr# ((3# *# i#) +# 2#) s2 of
          (# s3, i2 #) -> (# s3, IntTriple i0 i1 i2 #)
  writeOffAddr# addr# i# (IntTriple a b c) =
    \s0 -> case P.writeOffAddr# addr# (3# *# i#) a s0 of
       s1 -> case P.writeOffAddr# addr# ((3# *# i#) +# 1#) b s1 of
         s2 -> case P.writeOffAddr# addr# ((3# *# i#) +# 2# ) c s2 of
           s3 -> s3
  setOffAddr# = P.defaultSetOffAddr#

instance Prim WordTriple where
  {-# inline sizeOf# #-}
  {-# inline alignment# #-}
  {-# inline indexByteArray# #-}
  {-# inline readByteArray# #-}
  {-# inline writeByteArray# #-}
  {-# inline setByteArray# #-}
  {-# inline indexOffAddr# #-}
  {-# inline readOffAddr# #-}
  {-# inline writeOffAddr# #-}
  {-# inline setOffAddr# #-}
  sizeOf# _ = 3# *# P.sizeOf# (undefined :: Word)
  alignment# _ = P.alignment# (undefined :: Word)
  indexByteArray# arr# i# = WordTriple
    (P.indexByteArray# arr# (3# *# i#))
    (P.indexByteArray# arr# ((3# *# i#) +# 1#))
    (P.indexByteArray# arr# ((3# *# i#) +# 2#))
  readByteArray# arr# i# =
    \s0 -> case P.readByteArray# arr# (3# *# i#) s0 of
      (# s1, i0 #) -> case P.readByteArray# arr# ((3# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> case P.readByteArray# arr# ((3# *# i#) +# 2#) s2 of
          (# s3, i2 #) -> (# s3, WordTriple i0 i1 i2 #)
  writeByteArray# arr# i# (WordTriple a b c) =
    \s0 -> case P.writeByteArray# arr# (3# *# i#) a s0 of
       s1 -> case P.writeByteArray# arr# ((3# *# i#) +# 1#) b s1 of
         s2 -> case P.writeByteArray# arr# ((3# *# i#) +# 2# ) c s2 of
           s3 -> s3
  setByteArray# = P.defaultSetByteArray#
  indexOffAddr# arr# i# = WordTriple
    (P.indexOffAddr# arr# (3# *# i#))
    (P.indexOffAddr# arr# ((3# *# i#) +# 1#))
    (P.indexOffAddr# arr# ((3# *# i#) +# 2#))
  readOffAddr# arr# i# =
    \s0 -> case P.readOffAddr# arr# (3# *# i#) s0 of
      (# s1, i0 #) -> case P.readOffAddr# arr# ((3# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> case P.readOffAddr# arr# ((3# *# i#) +# 2#) s2 of
          (# s3, i2 #) -> (# s3, WordTriple i0 i1 i2 #)
  writeOffAddr# addr# i# (WordTriple a b c) =
    \s0 -> case P.writeOffAddr# addr# (3# *# i#) a s0 of
       s1 -> case P.writeOffAddr# addr# ((3# *# i#) +# 1#) b s1 of
         s2 -> case P.writeOffAddr# addr# ((3# *# i#) +# 2# ) c s2 of
           s3 -> s3
  setOffAddr# = P.defaultSetOffAddr#

instance Prim DoubleTriple where
  {-# inline sizeOf# #-}
  {-# inline alignment# #-}
  {-# inline indexByteArray# #-}
  {-# inline readByteArray# #-}
  {-# inline writeByteArray# #-}
  {-# inline setByteArray# #-}
  {-# inline indexOffAddr# #-}
  {-# inline readOffAddr# #-}
  {-# inline writeOffAddr# #-}
  {-# inline setOffAddr# #-}
  sizeOf# _ = 3# *# P.sizeOf# (undefined :: Double)
  alignment# _ = P.alignment# (undefined :: Double)
  indexByteArray# arr# i# = DoubleTriple
    (P.indexByteArray# arr# (3# *# i#))
    (P.indexByteArray# arr# ((3# *# i#) +# 1#))
    (P.indexByteArray# arr# ((3# *# i#) +# 2#))
  readByteArray# arr# i# =
    \s0 -> case P.readByteArray# arr# (3# *# i#) s0 of
      (# s1, i0 #) -> case P.readByteArray# arr# ((3# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> case P.readByteArray# arr# ((3# *# i#) +# 2#) s2 of
          (# s3, i2 #) -> (# s3, DoubleTriple i0 i1 i2 #)
  writeByteArray# arr# i# (DoubleTriple a b c) =
    \s0 -> case P.writeByteArray# arr# (3# *# i#) a s0 of
       s1 -> case P.writeByteArray# arr# ((3# *# i#) +# 1#) b s1 of
         s2 -> case P.writeByteArray# arr# ((3# *# i#) +# 2# ) c s2 of
           s3 -> s3
  setByteArray# = P.defaultSetByteArray#
  indexOffAddr# arr# i# = DoubleTriple
    (P.indexOffAddr# arr# (3# *# i#))
    (P.indexOffAddr# arr# ((3# *# i#) +# 1#))
    (P.indexOffAddr# arr# ((3# *# i#) +# 2#))
  readOffAddr# arr# i# =
    \s0 -> case P.readOffAddr# arr# (3# *# i#) s0 of
      (# s1, i0 #) -> case P.readOffAddr# arr# ((3# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> case P.readOffAddr# arr# ((3# *# i#) +# 2#) s2 of
          (# s3, i2 #) -> (# s3, DoubleTriple i0 i1 i2 #)
  writeOffAddr# addr# i# (DoubleTriple a b c) =
    \s0 -> case P.writeOffAddr# addr# (3# *# i#) a s0 of
       s1 -> case P.writeOffAddr# addr# ((3# *# i#) +# 1#) b s1 of
         s2 -> case P.writeOffAddr# addr# ((3# *# i#) +# 2# ) c s2 of
           s3 -> s3
  setOffAddr# = P.defaultSetOffAddr#

instance Prim DoublePair where
  {-# inline sizeOf# #-}
  {-# inline alignment# #-}
  {-# inline indexByteArray# #-}
  {-# inline readByteArray# #-}
  {-# inline writeByteArray# #-}
  {-# inline setByteArray# #-}
  {-# inline indexOffAddr# #-}
  {-# inline readOffAddr# #-}
  {-# inline writeOffAddr# #-}
  {-# inline setOffAddr# #-}
  sizeOf# _ = 2# *# P.sizeOf# (undefined :: Double)
  alignment# _ = P.alignment# (undefined :: Double)
  indexByteArray# arr# i# = DoublePair
    (P.indexByteArray# arr# (2# *# i#))
    (P.indexByteArray# arr# ((2# *# i#) +# 1#))
  readByteArray# arr# i# =
    \s0 -> case P.readByteArray# arr# (2# *# i#) s0 of
      (# s1, i0 #) -> case P.readByteArray# arr# ((2# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> (# s2, DoublePair i0 i1 #)
  writeByteArray# arr# i# (DoublePair a b) =
    \s0 -> case P.writeByteArray# arr# (2# *# i#) a s0 of
       s1 -> case P.writeByteArray# arr# ((2# *# i#) +# 1#) b s1 of
         s2 -> s2
  setByteArray# = P.defaultSetByteArray#
  indexOffAddr# arr# i# = DoublePair
    (P.indexOffAddr# arr# (2# *# i#))
    (P.indexOffAddr# arr# ((2# *# i#) +# 1#))
  readOffAddr# arr# i# =
    \s0 -> case P.readOffAddr# arr# (2# *# i#) s0 of
      (# s1, i0 #) -> case P.readOffAddr# arr# ((2# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> (# s2, DoublePair i0 i1 #)
  writeOffAddr# addr# i# (DoublePair a b) =
    \s0 -> case P.writeOffAddr# addr# (2# *# i#) a s0 of
       s1 -> case P.writeOffAddr# addr# ((2# *# i#) +# 1#) b s1 of
         s2 -> s2
  setOffAddr# = P.defaultSetOffAddr#

instance Prim IntPair where
  {-# inline sizeOf# #-}
  {-# inline alignment# #-}
  {-# inline indexByteArray# #-}
  {-# inline readByteArray# #-}
  {-# inline writeByteArray# #-}
  {-# inline setByteArray# #-}
  {-# inline indexOffAddr# #-}
  {-# inline readOffAddr# #-}
  {-# inline writeOffAddr# #-}
  {-# inline setOffAddr# #-}
  sizeOf# _ = 2# *# P.sizeOf# (undefined :: Int)
  alignment# _ = P.alignment# (undefined :: Int)
  indexByteArray# arr# i# = IntPair
    (P.indexByteArray# arr# (2# *# i#))
    (P.indexByteArray# arr# ((2# *# i#) +# 1#))
  readByteArray# arr# i# =
    \s0 -> case P.readByteArray# arr# (2# *# i#) s0 of
      (# s1, i0 #) -> case P.readByteArray# arr# ((2# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> (# s2, IntPair i0 i1 #)
  writeByteArray# arr# i# (IntPair a b) =
    \s0 -> case P.writeByteArray# arr# (2# *# i#) a s0 of
       s1 -> case P.writeByteArray# arr# ((2# *# i#) +# 1#) b s1 of
         s2 -> s2
  setByteArray# = P.defaultSetByteArray#
  indexOffAddr# arr# i# = IntPair
    (P.indexOffAddr# arr# (2# *# i#))
    (P.indexOffAddr# arr# ((2# *# i#) +# 1#))
  readOffAddr# arr# i# =
    \s0 -> case P.readOffAddr# arr# (2# *# i#) s0 of
      (# s1, i0 #) -> case P.readOffAddr# arr# ((2# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> (# s2, IntPair i0 i1 #)
  writeOffAddr# addr# i# (IntPair a b) =
    \s0 -> case P.writeOffAddr# addr# (2# *# i#) a s0 of
       s1 -> case P.writeOffAddr# addr# ((2# *# i#) +# 1#) b s1 of
         s2 -> s2
  setOffAddr# = P.defaultSetOffAddr#

instance Prim WordPair where
  {-# inline sizeOf# #-}
  {-# inline alignment# #-}
  {-# inline indexByteArray# #-}
  {-# inline readByteArray# #-}
  {-# inline writeByteArray# #-}
  {-# inline setByteArray# #-}
  {-# inline indexOffAddr# #-}
  {-# inline readOffAddr# #-}
  {-# inline writeOffAddr# #-}
  {-# inline setOffAddr# #-}
  sizeOf# _ = 2# *# P.sizeOf# (undefined :: Int)
  alignment# _ = P.alignment# (undefined :: Int)
  indexByteArray# arr# i# = WordPair
    (P.indexByteArray# arr# (2# *# i#))
    (P.indexByteArray# arr# ((2# *# i#) +# 1#))
  readByteArray# arr# i# =
    \s0 -> case P.readByteArray# arr# (2# *# i#) s0 of
      (# s1, i0 #) -> case P.readByteArray# arr# ((2# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> (# s2, WordPair i0 i1 #)
  writeByteArray# arr# i# (WordPair a b) =
    \s0 -> case P.writeByteArray# arr# (2# *# i#) a s0 of
       s1 -> case P.writeByteArray# arr# ((2# *# i#) +# 1#) b s1 of
         s2 -> s2
  setByteArray# = P.defaultSetByteArray#
  indexOffAddr# arr# i# = WordPair
    (P.indexOffAddr# arr# (2# *# i#))
    (P.indexOffAddr# arr# ((2# *# i#) +# 1#))
  readOffAddr# arr# i# =
    \s0 -> case P.readOffAddr# arr# (2# *# i#) s0 of
      (# s1, i0 #) -> case P.readOffAddr# arr# ((2# *# i#) +# 1#) s1 of
        (# s2, i1 #) -> (# s2, WordPair i0 i1 #)
  writeOffAddr# addr# i# (WordPair a b) =
    \s0 -> case P.writeOffAddr# addr# (2# *# i#) a s0 of
       s1 -> case P.writeOffAddr# addr# ((2# *# i#) +# 1#) b s1 of
         s2 -> s2
  setOffAddr# = P.defaultSetOffAddr#
