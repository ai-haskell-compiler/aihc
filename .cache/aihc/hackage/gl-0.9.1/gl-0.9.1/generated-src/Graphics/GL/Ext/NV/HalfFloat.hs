-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.HalfFloat (
  -- * Extension Support
    gl_NV_half_float

  -- * GL_NV_half_float
  , glColor3hNV
  , glColor3hvNV
  , glColor4hNV
  , glColor4hvNV
  , glFogCoordhNV
  , glFogCoordhvNV
  , glMultiTexCoord1hNV
  , glMultiTexCoord1hvNV
  , glMultiTexCoord2hNV
  , glMultiTexCoord2hvNV
  , glMultiTexCoord3hNV
  , glMultiTexCoord3hvNV
  , glMultiTexCoord4hNV
  , glMultiTexCoord4hvNV
  , glNormal3hNV
  , glNormal3hvNV
  , glSecondaryColor3hNV
  , glSecondaryColor3hvNV
  , glTexCoord1hNV
  , glTexCoord1hvNV
  , glTexCoord2hNV
  , glTexCoord2hvNV
  , glTexCoord3hNV
  , glTexCoord3hvNV
  , glTexCoord4hNV
  , glTexCoord4hvNV
  , glVertex2hNV
  , glVertex2hvNV
  , glVertex3hNV
  , glVertex3hvNV
  , glVertex4hNV
  , glVertex4hvNV
  , glVertexAttrib1hNV
  , glVertexAttrib1hvNV
  , glVertexAttrib2hNV
  , glVertexAttrib2hvNV
  , glVertexAttrib3hNV
  , glVertexAttrib3hvNV
  , glVertexAttrib4hNV
  , glVertexAttrib4hvNV
  , glVertexAttribs1hvNV
  , glVertexAttribs2hvNV
  , glVertexAttribs3hvNV
  , glVertexAttribs4hvNV
  , glVertexWeighthNV
  , glVertexWeighthvNV
  , pattern GL_HALF_FLOAT_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/half_float.txt GL_NV_half_float> extension is available.

gl_NV_half_float :: Bool
gl_NV_half_float = member "GL_NV_half_float" extensions
{-# NOINLINE gl_NV_half_float #-}

-- | Usage: @'glColor3hNV' red green blue@
--
-- The parameter @red@ is a @Half16NV@.
--
-- The parameter @green@ is a @Half16NV@.
--
-- The parameter @blue@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glColor3hvNV'.


glColor3hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glColor3hNV = ffihalfNVhalfNVhalfNVIOV glColor3hNVFunPtr

glColor3hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glColor3hNVFunPtr = unsafePerformIO (getProcAddress "glColor3hNV")

{-# NOINLINE glColor3hNVFunPtr #-}

-- | Usage: @'glColor3hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @3@.


glColor3hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glColor3hvNV = ffiPtrhalfNVIOV glColor3hvNVFunPtr

glColor3hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glColor3hvNVFunPtr = unsafePerformIO (getProcAddress "glColor3hvNV")

{-# NOINLINE glColor3hvNVFunPtr #-}

-- | Usage: @'glColor4hNV' red green blue alpha@
--
-- The parameter @red@ is a @Half16NV@.
--
-- The parameter @green@ is a @Half16NV@.
--
-- The parameter @blue@ is a @Half16NV@.
--
-- The parameter @alpha@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glColor4hvNV'.


glColor4hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glColor4hNV = ffihalfNVhalfNVhalfNVhalfNVIOV glColor4hNVFunPtr

glColor4hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glColor4hNVFunPtr = unsafePerformIO (getProcAddress "glColor4hNV")

{-# NOINLINE glColor4hNVFunPtr #-}

-- | Usage: @'glColor4hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @4@.


glColor4hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glColor4hvNV = ffiPtrhalfNVIOV glColor4hvNVFunPtr

glColor4hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glColor4hvNVFunPtr = unsafePerformIO (getProcAddress "glColor4hvNV")

{-# NOINLINE glColor4hvNVFunPtr #-}

-- | Usage: @'glFogCoordhNV' fog@
--
-- The parameter @fog@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glFogCoordhvNV'.


glFogCoordhNV :: MonadIO m => GLhalfNV -> m ()
glFogCoordhNV = ffihalfNVIOV glFogCoordhNVFunPtr

glFogCoordhNVFunPtr :: FunPtr (GLhalfNV -> IO ())
glFogCoordhNVFunPtr = unsafePerformIO (getProcAddress "glFogCoordhNV")

{-# NOINLINE glFogCoordhNVFunPtr #-}

-- | Usage: @'glFogCoordhvNV' fog@
--
-- The parameter @fog@ is a @Half16NV@.
--
-- The length of @fog@ should be @1@.


glFogCoordhvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glFogCoordhvNV = ffiPtrhalfNVIOV glFogCoordhvNVFunPtr

glFogCoordhvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glFogCoordhvNVFunPtr = unsafePerformIO (getProcAddress "glFogCoordhvNV")

{-# NOINLINE glFogCoordhvNVFunPtr #-}

-- | Usage: @'glMultiTexCoord1hNV' target s@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glMultiTexCoord1hvNV'.


glMultiTexCoord1hNV :: MonadIO m => GLenum -> GLhalfNV -> m ()
glMultiTexCoord1hNV = ffienumhalfNVIOV glMultiTexCoord1hNVFunPtr

glMultiTexCoord1hNVFunPtr :: FunPtr (GLenum -> GLhalfNV -> IO ())
glMultiTexCoord1hNVFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1hNV")

{-# NOINLINE glMultiTexCoord1hNVFunPtr #-}

-- | Usage: @'glMultiTexCoord1hvNV' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @1@.


glMultiTexCoord1hvNV :: MonadIO m => GLenum -> Ptr GLhalfNV -> m ()
glMultiTexCoord1hvNV = ffienumPtrhalfNVIOV glMultiTexCoord1hvNVFunPtr

glMultiTexCoord1hvNVFunPtr :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
glMultiTexCoord1hvNVFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1hvNV")

{-# NOINLINE glMultiTexCoord1hvNVFunPtr #-}

-- | Usage: @'glMultiTexCoord2hNV' target s t@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @Half16NV@.
--
-- The parameter @t@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glMultiTexCoord2hvNV'.


glMultiTexCoord2hNV :: MonadIO m => GLenum -> GLhalfNV -> GLhalfNV -> m ()
glMultiTexCoord2hNV = ffienumhalfNVhalfNVIOV glMultiTexCoord2hNVFunPtr

glMultiTexCoord2hNVFunPtr :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> IO ())
glMultiTexCoord2hNVFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2hNV")

{-# NOINLINE glMultiTexCoord2hNVFunPtr #-}

-- | Usage: @'glMultiTexCoord2hvNV' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @2@.


glMultiTexCoord2hvNV :: MonadIO m => GLenum -> Ptr GLhalfNV -> m ()
glMultiTexCoord2hvNV = ffienumPtrhalfNVIOV glMultiTexCoord2hvNVFunPtr

glMultiTexCoord2hvNVFunPtr :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
glMultiTexCoord2hvNVFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2hvNV")

{-# NOINLINE glMultiTexCoord2hvNVFunPtr #-}

-- | Usage: @'glMultiTexCoord3hNV' target s t r@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @Half16NV@.
--
-- The parameter @t@ is a @Half16NV@.
--
-- The parameter @r@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glMultiTexCoord3hvNV'.


glMultiTexCoord3hNV :: MonadIO m => GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glMultiTexCoord3hNV = ffienumhalfNVhalfNVhalfNVIOV glMultiTexCoord3hNVFunPtr

glMultiTexCoord3hNVFunPtr :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glMultiTexCoord3hNVFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3hNV")

{-# NOINLINE glMultiTexCoord3hNVFunPtr #-}

-- | Usage: @'glMultiTexCoord3hvNV' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @3@.


glMultiTexCoord3hvNV :: MonadIO m => GLenum -> Ptr GLhalfNV -> m ()
glMultiTexCoord3hvNV = ffienumPtrhalfNVIOV glMultiTexCoord3hvNVFunPtr

glMultiTexCoord3hvNVFunPtr :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
glMultiTexCoord3hvNVFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3hvNV")

{-# NOINLINE glMultiTexCoord3hvNVFunPtr #-}

-- | Usage: @'glMultiTexCoord4hNV' target s t r q@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @Half16NV@.
--
-- The parameter @t@ is a @Half16NV@.
--
-- The parameter @r@ is a @Half16NV@.
--
-- The parameter @q@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glMultiTexCoord4hvNV'.


glMultiTexCoord4hNV :: MonadIO m => GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glMultiTexCoord4hNV = ffienumhalfNVhalfNVhalfNVhalfNVIOV glMultiTexCoord4hNVFunPtr

glMultiTexCoord4hNVFunPtr :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glMultiTexCoord4hNVFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4hNV")

{-# NOINLINE glMultiTexCoord4hNVFunPtr #-}

-- | Usage: @'glMultiTexCoord4hvNV' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @4@.


glMultiTexCoord4hvNV :: MonadIO m => GLenum -> Ptr GLhalfNV -> m ()
glMultiTexCoord4hvNV = ffienumPtrhalfNVIOV glMultiTexCoord4hvNVFunPtr

glMultiTexCoord4hvNVFunPtr :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
glMultiTexCoord4hvNVFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4hvNV")

{-# NOINLINE glMultiTexCoord4hvNVFunPtr #-}

-- | Usage: @'glNormal3hNV' nx ny nz@
--
-- The parameter @nx@ is a @Half16NV@.
--
-- The parameter @ny@ is a @Half16NV@.
--
-- The parameter @nz@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glNormal3hvNV'.


glNormal3hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glNormal3hNV = ffihalfNVhalfNVhalfNVIOV glNormal3hNVFunPtr

glNormal3hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glNormal3hNVFunPtr = unsafePerformIO (getProcAddress "glNormal3hNV")

{-# NOINLINE glNormal3hNVFunPtr #-}

-- | Usage: @'glNormal3hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @3@.


glNormal3hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glNormal3hvNV = ffiPtrhalfNVIOV glNormal3hvNVFunPtr

glNormal3hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glNormal3hvNVFunPtr = unsafePerformIO (getProcAddress "glNormal3hvNV")

{-# NOINLINE glNormal3hvNVFunPtr #-}

-- | Usage: @'glSecondaryColor3hNV' red green blue@
--
-- The parameter @red@ is a @Half16NV@.
--
-- The parameter @green@ is a @Half16NV@.
--
-- The parameter @blue@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glSecondaryColor3hvNV'.


glSecondaryColor3hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glSecondaryColor3hNV = ffihalfNVhalfNVhalfNVIOV glSecondaryColor3hNVFunPtr

glSecondaryColor3hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glSecondaryColor3hNVFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3hNV")

{-# NOINLINE glSecondaryColor3hNVFunPtr #-}

-- | Usage: @'glSecondaryColor3hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @3@.


glSecondaryColor3hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glSecondaryColor3hvNV = ffiPtrhalfNVIOV glSecondaryColor3hvNVFunPtr

glSecondaryColor3hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glSecondaryColor3hvNVFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3hvNV")

{-# NOINLINE glSecondaryColor3hvNVFunPtr #-}

-- | Usage: @'glTexCoord1hNV' s@
--
-- The parameter @s@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glTexCoord1hvNV'.


glTexCoord1hNV :: MonadIO m => GLhalfNV -> m ()
glTexCoord1hNV = ffihalfNVIOV glTexCoord1hNVFunPtr

glTexCoord1hNVFunPtr :: FunPtr (GLhalfNV -> IO ())
glTexCoord1hNVFunPtr = unsafePerformIO (getProcAddress "glTexCoord1hNV")

{-# NOINLINE glTexCoord1hNVFunPtr #-}

-- | Usage: @'glTexCoord1hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @1@.


glTexCoord1hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glTexCoord1hvNV = ffiPtrhalfNVIOV glTexCoord1hvNVFunPtr

glTexCoord1hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glTexCoord1hvNVFunPtr = unsafePerformIO (getProcAddress "glTexCoord1hvNV")

{-# NOINLINE glTexCoord1hvNVFunPtr #-}

-- | Usage: @'glTexCoord2hNV' s t@
--
-- The parameter @s@ is a @Half16NV@.
--
-- The parameter @t@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glTexCoord2hvNV'.


glTexCoord2hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> m ()
glTexCoord2hNV = ffihalfNVhalfNVIOV glTexCoord2hNVFunPtr

glTexCoord2hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> IO ())
glTexCoord2hNVFunPtr = unsafePerformIO (getProcAddress "glTexCoord2hNV")

{-# NOINLINE glTexCoord2hNVFunPtr #-}

-- | Usage: @'glTexCoord2hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @2@.


glTexCoord2hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glTexCoord2hvNV = ffiPtrhalfNVIOV glTexCoord2hvNVFunPtr

glTexCoord2hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glTexCoord2hvNVFunPtr = unsafePerformIO (getProcAddress "glTexCoord2hvNV")

{-# NOINLINE glTexCoord2hvNVFunPtr #-}

-- | Usage: @'glTexCoord3hNV' s t r@
--
-- The parameter @s@ is a @Half16NV@.
--
-- The parameter @t@ is a @Half16NV@.
--
-- The parameter @r@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glTexCoord3hvNV'.


glTexCoord3hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glTexCoord3hNV = ffihalfNVhalfNVhalfNVIOV glTexCoord3hNVFunPtr

glTexCoord3hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glTexCoord3hNVFunPtr = unsafePerformIO (getProcAddress "glTexCoord3hNV")

{-# NOINLINE glTexCoord3hNVFunPtr #-}

-- | Usage: @'glTexCoord3hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @3@.


glTexCoord3hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glTexCoord3hvNV = ffiPtrhalfNVIOV glTexCoord3hvNVFunPtr

glTexCoord3hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glTexCoord3hvNVFunPtr = unsafePerformIO (getProcAddress "glTexCoord3hvNV")

{-# NOINLINE glTexCoord3hvNVFunPtr #-}

-- | Usage: @'glTexCoord4hNV' s t r q@
--
-- The parameter @s@ is a @Half16NV@.
--
-- The parameter @t@ is a @Half16NV@.
--
-- The parameter @r@ is a @Half16NV@.
--
-- The parameter @q@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glTexCoord4hvNV'.


glTexCoord4hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glTexCoord4hNV = ffihalfNVhalfNVhalfNVhalfNVIOV glTexCoord4hNVFunPtr

glTexCoord4hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glTexCoord4hNVFunPtr = unsafePerformIO (getProcAddress "glTexCoord4hNV")

{-# NOINLINE glTexCoord4hNVFunPtr #-}

-- | Usage: @'glTexCoord4hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @4@.


glTexCoord4hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glTexCoord4hvNV = ffiPtrhalfNVIOV glTexCoord4hvNVFunPtr

glTexCoord4hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glTexCoord4hvNVFunPtr = unsafePerformIO (getProcAddress "glTexCoord4hvNV")

{-# NOINLINE glTexCoord4hvNVFunPtr #-}

-- | Usage: @'glVertex2hNV' x y@
--
-- The parameter @x@ is a @Half16NV@.
--
-- The parameter @y@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glVertex2hvNV'.


glVertex2hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> m ()
glVertex2hNV = ffihalfNVhalfNVIOV glVertex2hNVFunPtr

glVertex2hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> IO ())
glVertex2hNVFunPtr = unsafePerformIO (getProcAddress "glVertex2hNV")

{-# NOINLINE glVertex2hNVFunPtr #-}

-- | Usage: @'glVertex2hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @2@.


glVertex2hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glVertex2hvNV = ffiPtrhalfNVIOV glVertex2hvNVFunPtr

glVertex2hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glVertex2hvNVFunPtr = unsafePerformIO (getProcAddress "glVertex2hvNV")

{-# NOINLINE glVertex2hvNVFunPtr #-}

-- | Usage: @'glVertex3hNV' x y z@
--
-- The parameter @x@ is a @Half16NV@.
--
-- The parameter @y@ is a @Half16NV@.
--
-- The parameter @z@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glVertex3hvNV'.


glVertex3hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glVertex3hNV = ffihalfNVhalfNVhalfNVIOV glVertex3hNVFunPtr

glVertex3hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glVertex3hNVFunPtr = unsafePerformIO (getProcAddress "glVertex3hNV")

{-# NOINLINE glVertex3hNVFunPtr #-}

-- | Usage: @'glVertex3hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @3@.


glVertex3hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glVertex3hvNV = ffiPtrhalfNVIOV glVertex3hvNVFunPtr

glVertex3hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glVertex3hvNVFunPtr = unsafePerformIO (getProcAddress "glVertex3hvNV")

{-# NOINLINE glVertex3hvNVFunPtr #-}

-- | Usage: @'glVertex4hNV' x y z w@
--
-- The parameter @x@ is a @Half16NV@.
--
-- The parameter @y@ is a @Half16NV@.
--
-- The parameter @z@ is a @Half16NV@.
--
-- The parameter @w@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glVertex4hvNV'.


glVertex4hNV :: MonadIO m => GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glVertex4hNV = ffihalfNVhalfNVhalfNVhalfNVIOV glVertex4hNVFunPtr

glVertex4hNVFunPtr :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glVertex4hNVFunPtr = unsafePerformIO (getProcAddress "glVertex4hNV")

{-# NOINLINE glVertex4hNVFunPtr #-}

-- | Usage: @'glVertex4hvNV' v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @4@.


glVertex4hvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glVertex4hvNV = ffiPtrhalfNVIOV glVertex4hvNVFunPtr

glVertex4hvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glVertex4hvNVFunPtr = unsafePerformIO (getProcAddress "glVertex4hvNV")

{-# NOINLINE glVertex4hvNVFunPtr #-}

-- | Usage: @'glVertexAttrib1hNV' index x@
--
-- The parameter @x@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glVertexAttrib1hvNV'.


glVertexAttrib1hNV :: MonadIO m => GLuint -> GLhalfNV -> m ()
glVertexAttrib1hNV = ffiuinthalfNVIOV glVertexAttrib1hNVFunPtr

glVertexAttrib1hNVFunPtr :: FunPtr (GLuint -> GLhalfNV -> IO ())
glVertexAttrib1hNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib1hNV")

{-# NOINLINE glVertexAttrib1hNVFunPtr #-}

-- | Usage: @'glVertexAttrib1hvNV' index v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @1@.


glVertexAttrib1hvNV :: MonadIO m => GLuint -> Ptr GLhalfNV -> m ()
glVertexAttrib1hvNV = ffiuintPtrhalfNVIOV glVertexAttrib1hvNVFunPtr

glVertexAttrib1hvNVFunPtr :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
glVertexAttrib1hvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib1hvNV")

{-# NOINLINE glVertexAttrib1hvNVFunPtr #-}

-- | Usage: @'glVertexAttrib2hNV' index x y@
--
-- The parameter @x@ is a @Half16NV@.
--
-- The parameter @y@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glVertexAttrib2hvNV'.


glVertexAttrib2hNV :: MonadIO m => GLuint -> GLhalfNV -> GLhalfNV -> m ()
glVertexAttrib2hNV = ffiuinthalfNVhalfNVIOV glVertexAttrib2hNVFunPtr

glVertexAttrib2hNVFunPtr :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> IO ())
glVertexAttrib2hNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib2hNV")

{-# NOINLINE glVertexAttrib2hNVFunPtr #-}

-- | Usage: @'glVertexAttrib2hvNV' index v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @2@.


glVertexAttrib2hvNV :: MonadIO m => GLuint -> Ptr GLhalfNV -> m ()
glVertexAttrib2hvNV = ffiuintPtrhalfNVIOV glVertexAttrib2hvNVFunPtr

glVertexAttrib2hvNVFunPtr :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
glVertexAttrib2hvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib2hvNV")

{-# NOINLINE glVertexAttrib2hvNVFunPtr #-}

-- | Usage: @'glVertexAttrib3hNV' index x y z@
--
-- The parameter @x@ is a @Half16NV@.
--
-- The parameter @y@ is a @Half16NV@.
--
-- The parameter @z@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glVertexAttrib3hvNV'.


glVertexAttrib3hNV :: MonadIO m => GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glVertexAttrib3hNV = ffiuinthalfNVhalfNVhalfNVIOV glVertexAttrib3hNVFunPtr

glVertexAttrib3hNVFunPtr :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glVertexAttrib3hNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib3hNV")

{-# NOINLINE glVertexAttrib3hNVFunPtr #-}

-- | Usage: @'glVertexAttrib3hvNV' index v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @3@.


glVertexAttrib3hvNV :: MonadIO m => GLuint -> Ptr GLhalfNV -> m ()
glVertexAttrib3hvNV = ffiuintPtrhalfNVIOV glVertexAttrib3hvNVFunPtr

glVertexAttrib3hvNVFunPtr :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
glVertexAttrib3hvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib3hvNV")

{-# NOINLINE glVertexAttrib3hvNVFunPtr #-}

-- | Usage: @'glVertexAttrib4hNV' index x y z w@
--
-- The parameter @x@ is a @Half16NV@.
--
-- The parameter @y@ is a @Half16NV@.
--
-- The parameter @z@ is a @Half16NV@.
--
-- The parameter @w@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glVertexAttrib4hvNV'.


glVertexAttrib4hNV :: MonadIO m => GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
glVertexAttrib4hNV = ffiuinthalfNVhalfNVhalfNVhalfNVIOV glVertexAttrib4hNVFunPtr

glVertexAttrib4hNVFunPtr :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
glVertexAttrib4hNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4hNV")

{-# NOINLINE glVertexAttrib4hNVFunPtr #-}

-- | Usage: @'glVertexAttrib4hvNV' index v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @4@.


glVertexAttrib4hvNV :: MonadIO m => GLuint -> Ptr GLhalfNV -> m ()
glVertexAttrib4hvNV = ffiuintPtrhalfNVIOV glVertexAttrib4hvNVFunPtr

glVertexAttrib4hvNVFunPtr :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
glVertexAttrib4hvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttrib4hvNV")

{-# NOINLINE glVertexAttrib4hvNVFunPtr #-}

-- | Usage: @'glVertexAttribs1hvNV' index n v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @n@.


glVertexAttribs1hvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLhalfNV -> m ()
glVertexAttribs1hvNV = ffiuintsizeiPtrhalfNVIOV glVertexAttribs1hvNVFunPtr

glVertexAttribs1hvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
glVertexAttribs1hvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs1hvNV")

{-# NOINLINE glVertexAttribs1hvNVFunPtr #-}

-- | Usage: @'glVertexAttribs2hvNV' index n v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @n@.


glVertexAttribs2hvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLhalfNV -> m ()
glVertexAttribs2hvNV = ffiuintsizeiPtrhalfNVIOV glVertexAttribs2hvNVFunPtr

glVertexAttribs2hvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
glVertexAttribs2hvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs2hvNV")

{-# NOINLINE glVertexAttribs2hvNVFunPtr #-}

-- | Usage: @'glVertexAttribs3hvNV' index n v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @n@.


glVertexAttribs3hvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLhalfNV -> m ()
glVertexAttribs3hvNV = ffiuintsizeiPtrhalfNVIOV glVertexAttribs3hvNVFunPtr

glVertexAttribs3hvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
glVertexAttribs3hvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs3hvNV")

{-# NOINLINE glVertexAttribs3hvNVFunPtr #-}

-- | Usage: @'glVertexAttribs4hvNV' index n v@
--
-- The parameter @v@ is a @Half16NV@.
--
-- The length of @v@ should be @n@.


glVertexAttribs4hvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLhalfNV -> m ()
glVertexAttribs4hvNV = ffiuintsizeiPtrhalfNVIOV glVertexAttribs4hvNVFunPtr

glVertexAttribs4hvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
glVertexAttribs4hvNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribs4hvNV")

{-# NOINLINE glVertexAttribs4hvNVFunPtr #-}

-- | Usage: @'glVertexWeighthNV' weight@
--
-- The parameter @weight@ is a @Half16NV@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.NV.HalfFloat.glVertexWeighthvNV'.


glVertexWeighthNV :: MonadIO m => GLhalfNV -> m ()
glVertexWeighthNV = ffihalfNVIOV glVertexWeighthNVFunPtr

glVertexWeighthNVFunPtr :: FunPtr (GLhalfNV -> IO ())
glVertexWeighthNVFunPtr = unsafePerformIO (getProcAddress "glVertexWeighthNV")

{-# NOINLINE glVertexWeighthNVFunPtr #-}

-- | Usage: @'glVertexWeighthvNV' weight@
--
-- The parameter @weight@ is a @Half16NV@.
--
-- The length of @weight@ should be @1@.


glVertexWeighthvNV :: MonadIO m => Ptr GLhalfNV -> m ()
glVertexWeighthvNV = ffiPtrhalfNVIOV glVertexWeighthvNVFunPtr

glVertexWeighthvNVFunPtr :: FunPtr (Ptr GLhalfNV -> IO ())
glVertexWeighthvNVFunPtr = unsafePerformIO (getProcAddress "glVertexWeighthvNV")

{-# NOINLINE glVertexWeighthvNVFunPtr #-}

pattern GL_HALF_FLOAT_NV  :: (Eq a, Num a) => a

pattern GL_HALF_FLOAT_NV = 0x140B