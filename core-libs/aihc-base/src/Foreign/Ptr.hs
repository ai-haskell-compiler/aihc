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
import Prelude (IO, Int, Word, error, return)

freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr _ = return ()

newtype IntPtr = IntPtr Int

newtype WordPtr = WordPtr Word

ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr _ = error "Foreign.Ptr.ptrToIntPtr: address conversion is not available"

intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr _ = error "Foreign.Ptr.intPtrToPtr: address conversion is not available"

ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr _ = error "Foreign.Ptr.ptrToWordPtr: address conversion is not available"

wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr _ = error "Foreign.Ptr.wordPtrToPtr: address conversion is not available"
