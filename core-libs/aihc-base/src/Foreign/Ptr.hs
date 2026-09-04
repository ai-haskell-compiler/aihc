{-# LANGUAGE MagicHash #-}

module Foreign.Ptr
  ( Ptr (..),
    FunPtr (..),
    nullPtr,
    nullFunPtr,
    castPtr,
    plusPtr,
    minusPtr,
    alignPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtrToFunPtr,
    freeHaskellFunPtr,
    IntPtr (..),
    WordPtr (..),
    ptrToIntPtr,
    intPtrToPtr,
    ptrToWordPtr,
    wordPtrToPtr,
  )
where

import GHC.Enum (Bounded (..), Enum (..))
import GHC.Int (Int (..))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Prim (addr2Int#, int2Addr#, int2Word#, word2Int#)
import GHC.Ptr
  ( FunPtr (..),
    Ptr (..),
    alignPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtr,
    castPtrToFunPtr,
    minusPtr,
    nullFunPtr,
    nullPtr,
    plusPtr,
  )
import GHC.Real (Integral (..), Real (..))
import GHC.Word (Word (..))
import Prelude (IO, return)

freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr _ = return ()

newtype IntPtr = IntPtr Int

newtype WordPtr = WordPtr Word

ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr address) = IntPtr (I# (addr2Int# address))

intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr (I# value)) = Ptr (int2Addr# value)

ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr address) = WordPtr (W# (int2Word# (addr2Int# address)))

wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr (W# value)) = Ptr (int2Addr# (word2Int# value))

-- | A local list map keeps this module free of a Data.List import.
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList convert (value : values) = convert value : mapList convert values

-- The pointer-sized integer types wrap 'Int' and 'Word', so every class
-- below forwards to the wrapped type.

instance Eq IntPtr where
  IntPtr left == IntPtr right = left == right

instance Ord IntPtr where
  compare (IntPtr left) (IntPtr right) = compare left right

instance Enum IntPtr where
  succ (IntPtr value) = IntPtr (succ value)
  pred (IntPtr value) = IntPtr (pred value)
  toEnum value = IntPtr (toEnum value)
  fromEnum (IntPtr value) = fromEnum value
  enumFrom (IntPtr value) = mapList IntPtr (enumFrom value)
  enumFromThen (IntPtr first) (IntPtr second) = mapList IntPtr (enumFromThen first second)
  enumFromTo (IntPtr first) (IntPtr final) = mapList IntPtr (enumFromTo first final)
  enumFromThenTo (IntPtr first) (IntPtr second) (IntPtr final) = mapList IntPtr (enumFromThenTo first second final)

instance Bounded IntPtr where
  minBound = IntPtr minBound
  maxBound = IntPtr maxBound

instance Num IntPtr where
  IntPtr left + IntPtr right = IntPtr (left + right)
  IntPtr left - IntPtr right = IntPtr (left - right)
  IntPtr left * IntPtr right = IntPtr (left * right)
  negate (IntPtr value) = IntPtr (negate value)
  abs (IntPtr value) = IntPtr (abs value)
  signum (IntPtr value) = IntPtr (signum value)
  fromInteger value = IntPtr (fromInteger value)

instance Real IntPtr where
  toRational (IntPtr value) = toRational value

instance Integral IntPtr where
  quot (IntPtr left) (IntPtr right) = IntPtr (quot left right)
  rem (IntPtr left) (IntPtr right) = IntPtr (rem left right)
  div (IntPtr left) (IntPtr right) = IntPtr (div left right)
  mod (IntPtr left) (IntPtr right) = IntPtr (mod left right)
  quotRem (IntPtr left) (IntPtr right) = case quotRem left right of
    (quotient, remainder) -> (IntPtr quotient, IntPtr remainder)
  divMod (IntPtr left) (IntPtr right) = case divMod left right of
    (quotient, modulus) -> (IntPtr quotient, IntPtr modulus)
  toInteger (IntPtr value) = toInteger value

instance Eq WordPtr where
  WordPtr left == WordPtr right = left == right

instance Ord WordPtr where
  compare (WordPtr left) (WordPtr right) = compare left right

instance Enum WordPtr where
  succ (WordPtr value) = WordPtr (succ value)
  pred (WordPtr value) = WordPtr (pred value)
  toEnum value = WordPtr (toEnum value)
  fromEnum (WordPtr value) = fromEnum value
  enumFrom (WordPtr value) = mapList WordPtr (enumFrom value)
  enumFromThen (WordPtr first) (WordPtr second) = mapList WordPtr (enumFromThen first second)
  enumFromTo (WordPtr first) (WordPtr final) = mapList WordPtr (enumFromTo first final)
  enumFromThenTo (WordPtr first) (WordPtr second) (WordPtr final) = mapList WordPtr (enumFromThenTo first second final)

instance Bounded WordPtr where
  minBound = WordPtr minBound
  maxBound = WordPtr maxBound

instance Num WordPtr where
  WordPtr left + WordPtr right = WordPtr (left + right)
  WordPtr left - WordPtr right = WordPtr (left - right)
  WordPtr left * WordPtr right = WordPtr (left * right)
  negate (WordPtr value) = WordPtr (negate value)
  abs (WordPtr value) = WordPtr (abs value)
  signum (WordPtr value) = WordPtr (signum value)
  fromInteger value = WordPtr (fromInteger value)

instance Real WordPtr where
  toRational (WordPtr value) = toRational value

instance Integral WordPtr where
  quot (WordPtr left) (WordPtr right) = WordPtr (quot left right)
  rem (WordPtr left) (WordPtr right) = WordPtr (rem left right)
  div (WordPtr left) (WordPtr right) = WordPtr (div left right)
  mod (WordPtr left) (WordPtr right) = WordPtr (mod left right)
  quotRem (WordPtr left) (WordPtr right) = case quotRem left right of
    (quotient, remainder) -> (WordPtr quotient, WordPtr remainder)
  divMod (WordPtr left) (WordPtr right) = case divMod left right of
    (quotient, modulus) -> (WordPtr quotient, WordPtr modulus)
  toInteger (WordPtr value) = toInteger value
