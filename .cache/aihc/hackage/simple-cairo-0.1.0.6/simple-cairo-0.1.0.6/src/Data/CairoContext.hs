{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoContext where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

newtype CairoT r s = CairoT (ForeignPtr (CairoT r s)) deriving Show

type CairoTIO r = CairoT r RealWorld

withCairoT :: PrimMonad m => CairoT r (PrimState m) -> (Ptr (CairoT r (PrimState m)) -> IO a) -> m a
withCairoT (CairoT fcr) = unsafeIOToPrim . withForeignPtr fcr
