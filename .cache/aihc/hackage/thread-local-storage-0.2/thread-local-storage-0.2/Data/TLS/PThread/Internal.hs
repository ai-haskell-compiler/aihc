{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Like "Data.TLS.PThread", but this also exports internal functionality
-- not exposed in the public interface.
--
-- There are no API guaranteees whatsoever for this module, so use it with
-- with caution.
module Data.TLS.PThread.Internal where

import Control.Concurrent (isCurrentThreadBound, rtsSupportsBoundThreads)
import Control.Monad
import Control.Exception
import Data.IORef
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable(Storable(sizeOf))

#if !(MIN_VERSION_base(4,8,0))
import Data.Word (Word)
#endif

#include "../TLS_Sig.hs"
--------------------------------------------------------------------------------

type Key = Word

foreign import ccall unsafe
   get_pthread_key_size :: Int

foreign import ccall unsafe
   pthread_key_create :: Ptr Key -> Ptr () -> IO Int

foreign import ccall unsafe
   easy_make_pthread_key :: IO Key

foreign import ccall unsafe
   pthread_getspecific :: Key -> IO (StablePtr a)

foreign import ccall unsafe
   pthread_setspecific :: Key -> StablePtr a -> IO Int

foreign import ccall unsafe
   pthread_key_delete :: Key -> IO Int

check_error :: ()
check_error =
--  if get_pthread_key_size == sizeOf(0::Word)
 if get_pthread_key_size <= sizeOf(0::Word)
 then ()
 else error $ "Data.TLS.PThread: internal invariant broken!  Expected pthread_key_t to be word-sized!\n"
             ++"Instead it was: "++show get_pthread_key_size


{-# INLINE setspecific #-}
setspecific :: Key -> StablePtr a -> IO ()
setspecific k p = do
    code <- pthread_setspecific k p
    unless (code == 0) (error $ "pthread_setspecific returned error code: "++show code)

{-# INLINE delete #-}
delete :: Key -> IO ()
delete k = do
    code <- pthread_key_delete k
--    putStrLn $ "KEY DELETED: "++show k
    unless (code == 0) (error $ "pthread_key_delete returned error code: "++show code)
    return ()


--------------------------------------------------------------------------------

-- | A thread-local variable of type `a`.
data TLS a = TLS { key       :: {-# UNPACK #-} !Key
                 , mknew     :: !(IO a)
                 , allCopies :: {-# UNPACK #-} !(IORef [StablePtr a]) }

mkTLS new = do
  evaluate check_error
  key  <- easy_make_pthread_key
--   putStrLn $ "KEY CREATED: "++show key
  allC <- newIORef []
  return $! TLS key new allC

-- | Fetches the copy of the TLS variable of the running OS thread.
--
-- The returned value is reliable only if the current thread is bound.
-- For this reason, this function calls 'error' if the current thread is
-- unbound.
getTLS TLS{key,mknew,allCopies} = do
  p <- pthread_getspecific key
  checkBoundness
  if castStablePtrToPtr p == nullPtr then do
    a <- mknew
    sp <- newStablePtr a
    setspecific key sp
    atomicModifyIORef' allCopies (\l -> (sp:l,()))
    return a
   else
    deRefStablePtr p
 where
  checkBoundness = when rtsSupportsBoundThreads $ do
    b <- isCurrentThreadBound
    when (not b) $
      fail "thread-local-storage: PThread.getTLS used from an unbound thread."

allTLS TLS{allCopies} = do
    ls <- readIORef allCopies
    mapM deRefStablePtr ls

forEachTLS_ tls fn = do
  ls <- allTLS tls
  forM_ ls fn

freeAllTLS TLS{key,allCopies} = do
    ls <- readIORef allCopies
    delete key
    mapM_ freeStablePtr ls
