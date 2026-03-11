-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.DrawTexture (
  -- * Extension Support
    gl_OES_draw_texture

  -- * GL_OES_draw_texture
  , glDrawTexfOES
  , glDrawTexfvOES
  , glDrawTexiOES
  , glDrawTexivOES
  , glDrawTexsOES
  , glDrawTexsvOES
  , glDrawTexxOES
  , glDrawTexxvOES
  , pattern GL_TEXTURE_CROP_RECT_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_draw_texture.txt GL_OES_draw_texture> extension is available.

gl_OES_draw_texture :: Bool
gl_OES_draw_texture = member "GL_OES_draw_texture" extensions
{-# NOINLINE gl_OES_draw_texture #-}

-- | Usage: @'glDrawTexfOES' x y z width height@


glDrawTexfOES :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glDrawTexfOES = ffifloatfloatfloatfloatfloatIOV glDrawTexfOESFunPtr

glDrawTexfOESFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glDrawTexfOESFunPtr = unsafePerformIO (getProcAddress "glDrawTexfOES")

{-# NOINLINE glDrawTexfOESFunPtr #-}

-- | Usage: @'glDrawTexfvOES' coords@


glDrawTexfvOES :: MonadIO m => Ptr GLfloat -> m ()
glDrawTexfvOES = ffiPtrfloatIOV glDrawTexfvOESFunPtr

glDrawTexfvOESFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glDrawTexfvOESFunPtr = unsafePerformIO (getProcAddress "glDrawTexfvOES")

{-# NOINLINE glDrawTexfvOESFunPtr #-}

-- | Usage: @'glDrawTexiOES' x y z width height@


glDrawTexiOES :: MonadIO m => GLint -> GLint -> GLint -> GLint -> GLint -> m ()
glDrawTexiOES = ffiintintintintintIOV glDrawTexiOESFunPtr

glDrawTexiOESFunPtr :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
glDrawTexiOESFunPtr = unsafePerformIO (getProcAddress "glDrawTexiOES")

{-# NOINLINE glDrawTexiOESFunPtr #-}

-- | Usage: @'glDrawTexivOES' coords@


glDrawTexivOES :: MonadIO m => Ptr GLint -> m ()
glDrawTexivOES = ffiPtrintIOV glDrawTexivOESFunPtr

glDrawTexivOESFunPtr :: FunPtr (Ptr GLint -> IO ())
glDrawTexivOESFunPtr = unsafePerformIO (getProcAddress "glDrawTexivOES")

{-# NOINLINE glDrawTexivOESFunPtr #-}

-- | Usage: @'glDrawTexsOES' x y z width height@


glDrawTexsOES :: MonadIO m => GLshort -> GLshort -> GLshort -> GLshort -> GLshort -> m ()
glDrawTexsOES = ffishortshortshortshortshortIOV glDrawTexsOESFunPtr

glDrawTexsOESFunPtr :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
glDrawTexsOESFunPtr = unsafePerformIO (getProcAddress "glDrawTexsOES")

{-# NOINLINE glDrawTexsOESFunPtr #-}

-- | Usage: @'glDrawTexsvOES' coords@


glDrawTexsvOES :: MonadIO m => Ptr GLshort -> m ()
glDrawTexsvOES = ffiPtrshortIOV glDrawTexsvOESFunPtr

glDrawTexsvOESFunPtr :: FunPtr (Ptr GLshort -> IO ())
glDrawTexsvOESFunPtr = unsafePerformIO (getProcAddress "glDrawTexsvOES")

{-# NOINLINE glDrawTexsvOESFunPtr #-}

-- | Usage: @'glDrawTexxOES' x y z width height@


glDrawTexxOES :: MonadIO m => GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
glDrawTexxOES = ffifixedfixedfixedfixedfixedIOV glDrawTexxOESFunPtr

glDrawTexxOESFunPtr :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
glDrawTexxOESFunPtr = unsafePerformIO (getProcAddress "glDrawTexxOES")

{-# NOINLINE glDrawTexxOESFunPtr #-}

-- | Usage: @'glDrawTexxvOES' coords@


glDrawTexxvOES :: MonadIO m => Ptr GLfixed -> m ()
glDrawTexxvOES = ffiPtrfixedIOV glDrawTexxvOESFunPtr

glDrawTexxvOESFunPtr :: FunPtr (Ptr GLfixed -> IO ())
glDrawTexxvOESFunPtr = unsafePerformIO (getProcAddress "glDrawTexxvOES")

{-# NOINLINE glDrawTexxvOESFunPtr #-}

pattern GL_TEXTURE_CROP_RECT_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CROP_RECT_OES = 0x8B9D