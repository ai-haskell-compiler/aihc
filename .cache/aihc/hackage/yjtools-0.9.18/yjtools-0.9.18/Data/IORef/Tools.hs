module Data.IORef.Tools(
	atomicModifyIORef_
) where

import Data.IORef(IORef, atomicModifyIORef)

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())
