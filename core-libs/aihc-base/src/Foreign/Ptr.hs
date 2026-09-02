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

import GHC.Int (Int (..))
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
