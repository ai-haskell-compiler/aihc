{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP     #-}

module Network.DNS.FFI where

import           Control.Concurrent.MVar
import           Control.Monad           (unless)
import           Control.Exception       (bracket)
import           Foreign.C
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           System.IO.Unsafe        (unsafePerformIO)

#if !defined(USE_RES_NQUERY)
# error USE_RES_NQUERY not defined
#endif

{-# INLINE resIsReentrant #-}
-- | Whether the reentrant DNS resolver C API (e.g. @res_nquery(3)@, @res_nsend(3)@) is being used.
--
-- If this this 'False', then as a fall-back
-- @res_query(3)@/@res_send(3)@ are used, protected by a global mutex.
--
-- @since 0.1.1.0
resIsReentrant :: Bool
#if USE_RES_NQUERY
resIsReentrant = True
#else
resIsReentrant = False
#endif

#if !defined(SIZEOF_RES_STATE)
# error SIZEOF_RES_STATE not defined
#endif

#if USE_RES_NQUERY && (SIZEOF_RES_STATE <= 0)
# error broken invariant
#endif

{-# INLINE sizeOfResState #-}
sizeOfResState :: CSize
sizeOfResState = SIZEOF_RES_STATE

data CResState

{-# NOINLINE resolvLock #-}
resolvLock :: MVar ()
resolvLock = unsafePerformIO $ newMVar ()

withCResState :: (Ptr CResState -> IO a) -> IO a
withCResState act
  | resIsReentrant = allocaBytes (fromIntegral sizeOfResState) $ \ptr -> do
                         _ <- c_memset ptr 0 sizeOfResState
                         act ptr
  | otherwise = withMVar resolvLock $ \() -> act nullPtr

withCResInit :: Ptr CResState -> IO a -> IO a
withCResInit stptr act = bracket initialize finalize $ const act
  where
    initialize = do
        rc1 <- c_res_opt_set_use_dnssec stptr
        unless (rc1 == 0) $
            fail "res_init(3) failed"
        resetErrno
    finalize _ = c_res_close stptr

-- void *memset(void *s, int c, size_t n);
foreign import capi unsafe "string.h memset" c_memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)

-- int res_query(void *, const char *dname, int class, int type, unsigned char *answer, int anslen);
foreign import capi safe "hs_resolv.h hs_res_query" c_res_query :: Ptr CResState -> CString -> CInt -> CInt -> Ptr CChar -> CInt -> IO CInt

-- int res_send(void *, const unsigned char *msg, int msglen, unsigned char *answer, int anslen);
foreign import capi safe "hs_resolv.h hs_res_send" c_res_send :: Ptr CResState -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> IO CInt

-- int res_opt_set_use_dnssec(void *);
foreign import capi safe "hs_resolv.h res_opt_set_use_dnssec" c_res_opt_set_use_dnssec :: Ptr CResState -> IO CInt

-- int hs_res_mkquery(void *, const char *dname, int class, int type, unsigned char *req, int reqlen0);
foreign import capi safe "hs_resolv.h hs_res_mkquery" c_res_mkquery :: Ptr CResState -> CString -> CInt -> CInt -> Ptr CChar -> CInt -> IO CInt

-- void hs_res_close(void *);
foreign import capi safe "hs_resolv.h hs_res_close" c_res_close :: Ptr CResState -> IO ()

-- void *hs_get_h_errno(void *);
foreign import capi unsafe "hs_resolv.h hs_get_h_errno" c_get_h_errno :: Ptr CResState -> IO CInt
