-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.ByteCoordinates (
  -- * Extension Support
    gl_OES_byte_coordinates

  -- * GL_OES_byte_coordinates
  , glMultiTexCoord1bOES
  , glMultiTexCoord1bvOES
  , glMultiTexCoord2bOES
  , glMultiTexCoord2bvOES
  , glMultiTexCoord3bOES
  , glMultiTexCoord3bvOES
  , glMultiTexCoord4bOES
  , glMultiTexCoord4bvOES
  , glTexCoord1bOES
  , glTexCoord1bvOES
  , glTexCoord2bOES
  , glTexCoord2bvOES
  , glTexCoord3bOES
  , glTexCoord3bvOES
  , glTexCoord4bOES
  , glTexCoord4bvOES
  , glVertex2bOES
  , glVertex2bvOES
  , glVertex3bOES
  , glVertex3bvOES
  , glVertex4bOES
  , glVertex4bvOES
  , pattern GL_BYTE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_byte_coordinates.txt GL_OES_byte_coordinates> extension is available.

gl_OES_byte_coordinates :: Bool
gl_OES_byte_coordinates = member "GL_OES_byte_coordinates" extensions
{-# NOINLINE gl_OES_byte_coordinates #-}

-- | Usage: @'glMultiTexCoord1bOES' texture s@


glMultiTexCoord1bOES :: MonadIO m => GLenum -> GLbyte -> m ()
glMultiTexCoord1bOES = ffienumbyteIOV glMultiTexCoord1bOESFunPtr

glMultiTexCoord1bOESFunPtr :: FunPtr (GLenum -> GLbyte -> IO ())
glMultiTexCoord1bOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1bOES")

{-# NOINLINE glMultiTexCoord1bOESFunPtr #-}

-- | Usage: @'glMultiTexCoord1bvOES' texture coords@
--
-- The length of @coords@ should be @1@.


glMultiTexCoord1bvOES :: MonadIO m => GLenum -> Ptr GLbyte -> m ()
glMultiTexCoord1bvOES = ffienumPtrbyteIOV glMultiTexCoord1bvOESFunPtr

glMultiTexCoord1bvOESFunPtr :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
glMultiTexCoord1bvOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1bvOES")

{-# NOINLINE glMultiTexCoord1bvOESFunPtr #-}

-- | Usage: @'glMultiTexCoord2bOES' texture s t@


glMultiTexCoord2bOES :: MonadIO m => GLenum -> GLbyte -> GLbyte -> m ()
glMultiTexCoord2bOES = ffienumbytebyteIOV glMultiTexCoord2bOESFunPtr

glMultiTexCoord2bOESFunPtr :: FunPtr (GLenum -> GLbyte -> GLbyte -> IO ())
glMultiTexCoord2bOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2bOES")

{-# NOINLINE glMultiTexCoord2bOESFunPtr #-}

-- | Usage: @'glMultiTexCoord2bvOES' texture coords@
--
-- The length of @coords@ should be @2@.


glMultiTexCoord2bvOES :: MonadIO m => GLenum -> Ptr GLbyte -> m ()
glMultiTexCoord2bvOES = ffienumPtrbyteIOV glMultiTexCoord2bvOESFunPtr

glMultiTexCoord2bvOESFunPtr :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
glMultiTexCoord2bvOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2bvOES")

{-# NOINLINE glMultiTexCoord2bvOESFunPtr #-}

-- | Usage: @'glMultiTexCoord3bOES' texture s t r@


glMultiTexCoord3bOES :: MonadIO m => GLenum -> GLbyte -> GLbyte -> GLbyte -> m ()
glMultiTexCoord3bOES = ffienumbytebytebyteIOV glMultiTexCoord3bOESFunPtr

glMultiTexCoord3bOESFunPtr :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ())
glMultiTexCoord3bOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3bOES")

{-# NOINLINE glMultiTexCoord3bOESFunPtr #-}

-- | Usage: @'glMultiTexCoord3bvOES' texture coords@
--
-- The length of @coords@ should be @3@.


glMultiTexCoord3bvOES :: MonadIO m => GLenum -> Ptr GLbyte -> m ()
glMultiTexCoord3bvOES = ffienumPtrbyteIOV glMultiTexCoord3bvOESFunPtr

glMultiTexCoord3bvOESFunPtr :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
glMultiTexCoord3bvOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3bvOES")

{-# NOINLINE glMultiTexCoord3bvOESFunPtr #-}

-- | Usage: @'glMultiTexCoord4bOES' texture s t r q@


glMultiTexCoord4bOES :: MonadIO m => GLenum -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> m ()
glMultiTexCoord4bOES = ffienumbytebytebytebyteIOV glMultiTexCoord4bOESFunPtr

glMultiTexCoord4bOESFunPtr :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
glMultiTexCoord4bOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4bOES")

{-# NOINLINE glMultiTexCoord4bOESFunPtr #-}

-- | Usage: @'glMultiTexCoord4bvOES' texture coords@
--
-- The length of @coords@ should be @4@.


glMultiTexCoord4bvOES :: MonadIO m => GLenum -> Ptr GLbyte -> m ()
glMultiTexCoord4bvOES = ffienumPtrbyteIOV glMultiTexCoord4bvOESFunPtr

glMultiTexCoord4bvOESFunPtr :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
glMultiTexCoord4bvOESFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4bvOES")

{-# NOINLINE glMultiTexCoord4bvOESFunPtr #-}

-- | Usage: @'glTexCoord1bOES' s@


glTexCoord1bOES :: MonadIO m => GLbyte -> m ()
glTexCoord1bOES = ffibyteIOV glTexCoord1bOESFunPtr

glTexCoord1bOESFunPtr :: FunPtr (GLbyte -> IO ())
glTexCoord1bOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord1bOES")

{-# NOINLINE glTexCoord1bOESFunPtr #-}

-- | Usage: @'glTexCoord1bvOES' coords@
--
-- The length of @coords@ should be @1@.


glTexCoord1bvOES :: MonadIO m => Ptr GLbyte -> m ()
glTexCoord1bvOES = ffiPtrbyteIOV glTexCoord1bvOESFunPtr

glTexCoord1bvOESFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glTexCoord1bvOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord1bvOES")

{-# NOINLINE glTexCoord1bvOESFunPtr #-}

-- | Usage: @'glTexCoord2bOES' s t@


glTexCoord2bOES :: MonadIO m => GLbyte -> GLbyte -> m ()
glTexCoord2bOES = ffibytebyteIOV glTexCoord2bOESFunPtr

glTexCoord2bOESFunPtr :: FunPtr (GLbyte -> GLbyte -> IO ())
glTexCoord2bOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord2bOES")

{-# NOINLINE glTexCoord2bOESFunPtr #-}

-- | Usage: @'glTexCoord2bvOES' coords@
--
-- The length of @coords@ should be @2@.


glTexCoord2bvOES :: MonadIO m => Ptr GLbyte -> m ()
glTexCoord2bvOES = ffiPtrbyteIOV glTexCoord2bvOESFunPtr

glTexCoord2bvOESFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glTexCoord2bvOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord2bvOES")

{-# NOINLINE glTexCoord2bvOESFunPtr #-}

-- | Usage: @'glTexCoord3bOES' s t r@


glTexCoord3bOES :: MonadIO m => GLbyte -> GLbyte -> GLbyte -> m ()
glTexCoord3bOES = ffibytebytebyteIOV glTexCoord3bOESFunPtr

glTexCoord3bOESFunPtr :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
glTexCoord3bOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord3bOES")

{-# NOINLINE glTexCoord3bOESFunPtr #-}

-- | Usage: @'glTexCoord3bvOES' coords@
--
-- The length of @coords@ should be @3@.


glTexCoord3bvOES :: MonadIO m => Ptr GLbyte -> m ()
glTexCoord3bvOES = ffiPtrbyteIOV glTexCoord3bvOESFunPtr

glTexCoord3bvOESFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glTexCoord3bvOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord3bvOES")

{-# NOINLINE glTexCoord3bvOESFunPtr #-}

-- | Usage: @'glTexCoord4bOES' s t r q@


glTexCoord4bOES :: MonadIO m => GLbyte -> GLbyte -> GLbyte -> GLbyte -> m ()
glTexCoord4bOES = ffibytebytebytebyteIOV glTexCoord4bOESFunPtr

glTexCoord4bOESFunPtr :: FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
glTexCoord4bOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord4bOES")

{-# NOINLINE glTexCoord4bOESFunPtr #-}

-- | Usage: @'glTexCoord4bvOES' coords@
--
-- The length of @coords@ should be @4@.


glTexCoord4bvOES :: MonadIO m => Ptr GLbyte -> m ()
glTexCoord4bvOES = ffiPtrbyteIOV glTexCoord4bvOESFunPtr

glTexCoord4bvOESFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glTexCoord4bvOESFunPtr = unsafePerformIO (getProcAddress "glTexCoord4bvOES")

{-# NOINLINE glTexCoord4bvOESFunPtr #-}

-- | Usage: @'glVertex2bOES' x y@


glVertex2bOES :: MonadIO m => GLbyte -> GLbyte -> m ()
glVertex2bOES = ffibytebyteIOV glVertex2bOESFunPtr

glVertex2bOESFunPtr :: FunPtr (GLbyte -> GLbyte -> IO ())
glVertex2bOESFunPtr = unsafePerformIO (getProcAddress "glVertex2bOES")

{-# NOINLINE glVertex2bOESFunPtr #-}

-- | Usage: @'glVertex2bvOES' coords@
--
-- The length of @coords@ should be @2@.


glVertex2bvOES :: MonadIO m => Ptr GLbyte -> m ()
glVertex2bvOES = ffiPtrbyteIOV glVertex2bvOESFunPtr

glVertex2bvOESFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glVertex2bvOESFunPtr = unsafePerformIO (getProcAddress "glVertex2bvOES")

{-# NOINLINE glVertex2bvOESFunPtr #-}

-- | Usage: @'glVertex3bOES' x y z@


glVertex3bOES :: MonadIO m => GLbyte -> GLbyte -> GLbyte -> m ()
glVertex3bOES = ffibytebytebyteIOV glVertex3bOESFunPtr

glVertex3bOESFunPtr :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
glVertex3bOESFunPtr = unsafePerformIO (getProcAddress "glVertex3bOES")

{-# NOINLINE glVertex3bOESFunPtr #-}

-- | Usage: @'glVertex3bvOES' coords@
--
-- The length of @coords@ should be @3@.


glVertex3bvOES :: MonadIO m => Ptr GLbyte -> m ()
glVertex3bvOES = ffiPtrbyteIOV glVertex3bvOESFunPtr

glVertex3bvOESFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glVertex3bvOESFunPtr = unsafePerformIO (getProcAddress "glVertex3bvOES")

{-# NOINLINE glVertex3bvOESFunPtr #-}

-- | Usage: @'glVertex4bOES' x y z w@


glVertex4bOES :: MonadIO m => GLbyte -> GLbyte -> GLbyte -> GLbyte -> m ()
glVertex4bOES = ffibytebytebytebyteIOV glVertex4bOESFunPtr

glVertex4bOESFunPtr :: FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
glVertex4bOESFunPtr = unsafePerformIO (getProcAddress "glVertex4bOES")

{-# NOINLINE glVertex4bOESFunPtr #-}

-- | Usage: @'glVertex4bvOES' coords@
--
-- The length of @coords@ should be @4@.


glVertex4bvOES :: MonadIO m => Ptr GLbyte -> m ()
glVertex4bvOES = ffiPtrbyteIOV glVertex4bvOESFunPtr

glVertex4bvOESFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glVertex4bvOESFunPtr = unsafePerformIO (getProcAddress "glVertex4bvOES")

{-# NOINLINE glVertex4bvOESFunPtr #-}