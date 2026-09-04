{-# LANGUAGE MagicHash #-}

module GHC.Ptr
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
  )
where

import GHC.Classes (Eq (..), Ord (..))
import GHC.Int (Int (..))
import GHC.Prim (Addr#, addr2Int#, eqAddr#, int2Word#, ltAddr#, minusAddr#, nullAddr#, plusAddr#, remWord#, word2Int#, (-#))
import GHC.Types (Bool (..), Ordering (..))

data Ptr a = Ptr Addr#

data FunPtr a = FunPtr Addr#

nullPtr :: Ptr a
nullPtr = Ptr nullAddr#

nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullAddr#

castPtr :: Ptr a -> Ptr b
castPtr (Ptr address) = Ptr address

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr address) (I# offset) = Ptr (plusAddr# address offset)

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr (Ptr left) (Ptr right) = I# (minusAddr# left right)

-- | Round a pointer up to the next multiple of the alignment.
alignPtr :: Ptr a -> Int -> Ptr a
alignPtr pointer@(Ptr address) (I# alignment) =
  case word2Int# (remWord# (int2Word# (addr2Int# address)) (int2Word# alignment)) of
    0# -> pointer
    remainder -> Ptr (plusAddr# address (alignment -# remainder))

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr address) = FunPtr address

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr address) = Ptr address

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr address) = FunPtr address

-- | Pointers compare by the address they hold; the phantom element type plays
-- no part.
instance Eq (Ptr a) where
  Ptr left == Ptr right = addressEquals left right

instance Ord (Ptr a) where
  compare (Ptr left) (Ptr right) = compareAddress left right

instance Eq (FunPtr a) where
  FunPtr left == FunPtr right = addressEquals left right

instance Ord (FunPtr a) where
  compare (FunPtr left) (FunPtr right) = compareAddress left right

addressEquals :: Addr# -> Addr# -> Bool
addressEquals left right =
  case eqAddr# left right of
    0# -> False
    _ -> True

compareAddress :: Addr# -> Addr# -> Ordering
compareAddress left right =
  case eqAddr# left right of
    0# -> case ltAddr# left right of
      0# -> GT
      _ -> LT
    _ -> EQ
