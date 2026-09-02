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

import GHC.Base (String)
import GHC.Int (Int)
import GHC.Prim (Addr#, nullAddr#, raise#)

data Ptr a = Ptr Addr#

data FunPtr a = FunPtr Addr#

nullPtr :: Ptr a
nullPtr = Ptr nullAddr#

nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullAddr#

castPtr :: Ptr a -> Ptr b
castPtr (Ptr address) = Ptr address

-- | Address arithmetic needs the plusAddr# primitive, which is not available.
plusPtr :: Ptr a -> Int -> Ptr b
plusPtr _ _ = pointerError "GHC.Ptr.plusPtr: address arithmetic is not available"

-- | Address arithmetic needs the minusAddr# primitive, which is not available.
minusPtr :: Ptr a -> Ptr b -> Int
minusPtr _ _ = pointerError "GHC.Ptr.minusPtr: address arithmetic is not available"

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr _ _ = pointerError "GHC.Ptr.alignPtr: address arithmetic is not available"

pointerError :: String -> a
pointerError = raise#

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr address) = FunPtr address

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr address) = Ptr address

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr address) = FunPtr address
