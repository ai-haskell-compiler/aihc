-- | Interface to membuf.c
module Exomizer.Extern.Membuf
    ( Membuf
    , withMembuf
    , membufAppend
    , membufGet
    ) where

import           Control.Monad
import           Data.ByteString
import           Data.ByteString.Internal
import           Foreign                  hiding (void)
import           Foreign.C

#import "membuf.h"

-- | @since 1.0.0
data Membuf

foreign import ccall safe "membuf_init"
  c_membuf_init
    :: Ptr Membuf
    -> IO ()

foreign import ccall safe "membuf_free"
  c_membuf_free
    :: Ptr Membuf
    -> IO ()

-- | @since 1.0.0
withMembuf :: (Ptr Membuf -> IO a) -> IO a
{-# INLINE withMembuf #-}
withMembuf action = do
  allocaBytesAligned
    {#sizeof membuf#}
    {#alignof membuf#}
    $ \membufPtr -> do
      c_membuf_init membufPtr
      result <- action membufPtr
      c_membuf_free membufPtr
      return result

foreign import ccall safe "membuf_append"
  c_membuf_append
    :: Ptr Membuf
    -> Ptr CChar
    -> CInt
    -> IO (Ptr CChar)

-- | @since 1.0.0
membufAppend :: Ptr Membuf -> ByteString -> IO ()
{-# INLINE membufAppend #-}
membufAppend membufPtr (PS fp o l) =
  withForeignPtr fp $ \p ->
    void $ c_membuf_append membufPtr (p `plusPtr` o) (fromIntegral l)

-- | @since 1.0.0
membufGet :: Ptr Membuf -> IO ByteString
{-# INLINE membufGet #-}
membufGet membufPtr = do
  p <- {#get membuf->buf#} membufPtr
  l <- {#get membuf->len#} membufPtr
  packCStringLen (castPtr p, fromIntegral l)
