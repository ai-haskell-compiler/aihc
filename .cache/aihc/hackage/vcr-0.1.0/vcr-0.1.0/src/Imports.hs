{-# LANGUAGE CPP #-}
module Imports (module Imports) where

import Prelude as Imports
import Control.Arrow as Imports
import Control.Monad as Imports
import Data.Maybe as Imports
import Data.Functor as Imports
#if MIN_VERSION_base(4,19,0)
  hiding (unzip)
#endif
import Data.IORef as Imports (IORef, newIORef, atomicWriteIORef, atomicModifyIORef')
import Data.ByteString as Imports (ByteString)
import Data.ByteString.Lazy as Imports (LazyByteString)

atomicReadIORef :: IORef a -> IO a
atomicReadIORef ref = atomicModifyIORef' ref (id &&& id)

pass :: Applicative m => m ()
pass = pure ()
