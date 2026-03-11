-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.Multitexture (
  -- * Extension Support
    gl_ARB_multitexture

  -- * GL_ARB_multitexture
  , glActiveTextureARB
  , glClientActiveTextureARB
  , glMultiTexCoord1dARB
  , glMultiTexCoord1dvARB
  , glMultiTexCoord1fARB
  , glMultiTexCoord1fvARB
  , glMultiTexCoord1iARB
  , glMultiTexCoord1ivARB
  , glMultiTexCoord1sARB
  , glMultiTexCoord1svARB
  , glMultiTexCoord2dARB
  , glMultiTexCoord2dvARB
  , glMultiTexCoord2fARB
  , glMultiTexCoord2fvARB
  , glMultiTexCoord2iARB
  , glMultiTexCoord2ivARB
  , glMultiTexCoord2sARB
  , glMultiTexCoord2svARB
  , glMultiTexCoord3dARB
  , glMultiTexCoord3dvARB
  , glMultiTexCoord3fARB
  , glMultiTexCoord3fvARB
  , glMultiTexCoord3iARB
  , glMultiTexCoord3ivARB
  , glMultiTexCoord3sARB
  , glMultiTexCoord3svARB
  , glMultiTexCoord4dARB
  , glMultiTexCoord4dvARB
  , glMultiTexCoord4fARB
  , glMultiTexCoord4fvARB
  , glMultiTexCoord4iARB
  , glMultiTexCoord4ivARB
  , glMultiTexCoord4sARB
  , glMultiTexCoord4svARB
  , pattern GL_ACTIVE_TEXTURE_ARB
  , pattern GL_CLIENT_ACTIVE_TEXTURE_ARB
  , pattern GL_MAX_TEXTURE_UNITS_ARB
  , pattern GL_TEXTURE0_ARB
  , pattern GL_TEXTURE10_ARB
  , pattern GL_TEXTURE11_ARB
  , pattern GL_TEXTURE12_ARB
  , pattern GL_TEXTURE13_ARB
  , pattern GL_TEXTURE14_ARB
  , pattern GL_TEXTURE15_ARB
  , pattern GL_TEXTURE16_ARB
  , pattern GL_TEXTURE17_ARB
  , pattern GL_TEXTURE18_ARB
  , pattern GL_TEXTURE19_ARB
  , pattern GL_TEXTURE1_ARB
  , pattern GL_TEXTURE20_ARB
  , pattern GL_TEXTURE21_ARB
  , pattern GL_TEXTURE22_ARB
  , pattern GL_TEXTURE23_ARB
  , pattern GL_TEXTURE24_ARB
  , pattern GL_TEXTURE25_ARB
  , pattern GL_TEXTURE26_ARB
  , pattern GL_TEXTURE27_ARB
  , pattern GL_TEXTURE28_ARB
  , pattern GL_TEXTURE29_ARB
  , pattern GL_TEXTURE2_ARB
  , pattern GL_TEXTURE30_ARB
  , pattern GL_TEXTURE31_ARB
  , pattern GL_TEXTURE3_ARB
  , pattern GL_TEXTURE4_ARB
  , pattern GL_TEXTURE5_ARB
  , pattern GL_TEXTURE6_ARB
  , pattern GL_TEXTURE7_ARB
  , pattern GL_TEXTURE8_ARB
  , pattern GL_TEXTURE9_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/multitexture.txt GL_ARB_multitexture> extension is available.

gl_ARB_multitexture :: Bool
gl_ARB_multitexture = member "GL_ARB_multitexture" extensions
{-# NOINLINE gl_ARB_multitexture #-}

-- | Usage: @'glActiveTextureARB' texture@
--
-- The parameter @texture@ is a @TextureUnit@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glActiveTexture'.


glActiveTextureARB :: MonadIO m => GLenum -> m ()
glActiveTextureARB = ffienumIOV glActiveTextureARBFunPtr

glActiveTextureARBFunPtr :: FunPtr (GLenum -> IO ())
glActiveTextureARBFunPtr = unsafePerformIO (getProcAddress "glActiveTextureARB")

{-# NOINLINE glActiveTextureARBFunPtr #-}

-- | Usage: @'glClientActiveTextureARB' texture@
--
-- The parameter @texture@ is a @TextureUnit@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glClientActiveTexture'.


glClientActiveTextureARB :: MonadIO m => GLenum -> m ()
glClientActiveTextureARB = ffienumIOV glClientActiveTextureARBFunPtr

glClientActiveTextureARBFunPtr :: FunPtr (GLenum -> IO ())
glClientActiveTextureARBFunPtr = unsafePerformIO (getProcAddress "glClientActiveTextureARB")

{-# NOINLINE glClientActiveTextureARBFunPtr #-}

-- | Usage: @'glMultiTexCoord1dARB' target s@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord1d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord1dv'.


glMultiTexCoord1dARB :: MonadIO m => GLenum -> GLdouble -> m ()
glMultiTexCoord1dARB = ffienumdoubleIOV glMultiTexCoord1dARBFunPtr

glMultiTexCoord1dARBFunPtr :: FunPtr (GLenum -> GLdouble -> IO ())
glMultiTexCoord1dARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1dARB")

{-# NOINLINE glMultiTexCoord1dARBFunPtr #-}

-- | Usage: @'glMultiTexCoord1dvARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord1dv'.


glMultiTexCoord1dvARB :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glMultiTexCoord1dvARB = ffienumPtrdoubleIOV glMultiTexCoord1dvARBFunPtr

glMultiTexCoord1dvARBFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glMultiTexCoord1dvARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1dvARB")

{-# NOINLINE glMultiTexCoord1dvARBFunPtr #-}

-- | Usage: @'glMultiTexCoord1fARB' target s@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord1f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord1fv'.


glMultiTexCoord1fARB :: MonadIO m => GLenum -> GLfloat -> m ()
glMultiTexCoord1fARB = ffienumfloatIOV glMultiTexCoord1fARBFunPtr

glMultiTexCoord1fARBFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glMultiTexCoord1fARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1fARB")

{-# NOINLINE glMultiTexCoord1fARBFunPtr #-}

-- | Usage: @'glMultiTexCoord1fvARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord1fv'.


glMultiTexCoord1fvARB :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMultiTexCoord1fvARB = ffienumPtrfloatIOV glMultiTexCoord1fvARBFunPtr

glMultiTexCoord1fvARBFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMultiTexCoord1fvARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1fvARB")

{-# NOINLINE glMultiTexCoord1fvARBFunPtr #-}

-- | Usage: @'glMultiTexCoord1iARB' target s@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord1i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord1iv'.


glMultiTexCoord1iARB :: MonadIO m => GLenum -> GLint -> m ()
glMultiTexCoord1iARB = ffienumintIOV glMultiTexCoord1iARBFunPtr

glMultiTexCoord1iARBFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glMultiTexCoord1iARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1iARB")

{-# NOINLINE glMultiTexCoord1iARBFunPtr #-}

-- | Usage: @'glMultiTexCoord1ivARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord1iv'.


glMultiTexCoord1ivARB :: MonadIO m => GLenum -> Ptr GLint -> m ()
glMultiTexCoord1ivARB = ffienumPtrintIOV glMultiTexCoord1ivARBFunPtr

glMultiTexCoord1ivARBFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glMultiTexCoord1ivARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1ivARB")

{-# NOINLINE glMultiTexCoord1ivARBFunPtr #-}

-- | Usage: @'glMultiTexCoord1sARB' target s@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord1s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord1sv'.


glMultiTexCoord1sARB :: MonadIO m => GLenum -> GLshort -> m ()
glMultiTexCoord1sARB = ffienumshortIOV glMultiTexCoord1sARBFunPtr

glMultiTexCoord1sARBFunPtr :: FunPtr (GLenum -> GLshort -> IO ())
glMultiTexCoord1sARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1sARB")

{-# NOINLINE glMultiTexCoord1sARBFunPtr #-}

-- | Usage: @'glMultiTexCoord1svARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord1sv'.


glMultiTexCoord1svARB :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glMultiTexCoord1svARB = ffienumPtrshortIOV glMultiTexCoord1svARBFunPtr

glMultiTexCoord1svARBFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glMultiTexCoord1svARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord1svARB")

{-# NOINLINE glMultiTexCoord1svARBFunPtr #-}

-- | Usage: @'glMultiTexCoord2dARB' target s t@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordD@.
--
-- The parameter @t@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord2d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord2dv'.


glMultiTexCoord2dARB :: MonadIO m => GLenum -> GLdouble -> GLdouble -> m ()
glMultiTexCoord2dARB = ffienumdoubledoubleIOV glMultiTexCoord2dARBFunPtr

glMultiTexCoord2dARBFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ())
glMultiTexCoord2dARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2dARB")

{-# NOINLINE glMultiTexCoord2dARBFunPtr #-}

-- | Usage: @'glMultiTexCoord2dvARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord2dv'.


glMultiTexCoord2dvARB :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glMultiTexCoord2dvARB = ffienumPtrdoubleIOV glMultiTexCoord2dvARBFunPtr

glMultiTexCoord2dvARBFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glMultiTexCoord2dvARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2dvARB")

{-# NOINLINE glMultiTexCoord2dvARBFunPtr #-}

-- | Usage: @'glMultiTexCoord2fARB' target s t@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordF@.
--
-- The parameter @t@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord2f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord2fv'.


glMultiTexCoord2fARB :: MonadIO m => GLenum -> GLfloat -> GLfloat -> m ()
glMultiTexCoord2fARB = ffienumfloatfloatIOV glMultiTexCoord2fARBFunPtr

glMultiTexCoord2fARBFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ())
glMultiTexCoord2fARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2fARB")

{-# NOINLINE glMultiTexCoord2fARBFunPtr #-}

-- | Usage: @'glMultiTexCoord2fvARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord2fv'.


glMultiTexCoord2fvARB :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMultiTexCoord2fvARB = ffienumPtrfloatIOV glMultiTexCoord2fvARBFunPtr

glMultiTexCoord2fvARBFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMultiTexCoord2fvARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2fvARB")

{-# NOINLINE glMultiTexCoord2fvARBFunPtr #-}

-- | Usage: @'glMultiTexCoord2iARB' target s t@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordI@.
--
-- The parameter @t@ is a @CoordI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord2i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord2iv'.


glMultiTexCoord2iARB :: MonadIO m => GLenum -> GLint -> GLint -> m ()
glMultiTexCoord2iARB = ffienumintintIOV glMultiTexCoord2iARBFunPtr

glMultiTexCoord2iARBFunPtr :: FunPtr (GLenum -> GLint -> GLint -> IO ())
glMultiTexCoord2iARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2iARB")

{-# NOINLINE glMultiTexCoord2iARBFunPtr #-}

-- | Usage: @'glMultiTexCoord2ivARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord2iv'.


glMultiTexCoord2ivARB :: MonadIO m => GLenum -> Ptr GLint -> m ()
glMultiTexCoord2ivARB = ffienumPtrintIOV glMultiTexCoord2ivARBFunPtr

glMultiTexCoord2ivARBFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glMultiTexCoord2ivARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2ivARB")

{-# NOINLINE glMultiTexCoord2ivARBFunPtr #-}

-- | Usage: @'glMultiTexCoord2sARB' target s t@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordS@.
--
-- The parameter @t@ is a @CoordS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord2s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord2sv'.


glMultiTexCoord2sARB :: MonadIO m => GLenum -> GLshort -> GLshort -> m ()
glMultiTexCoord2sARB = ffienumshortshortIOV glMultiTexCoord2sARBFunPtr

glMultiTexCoord2sARBFunPtr :: FunPtr (GLenum -> GLshort -> GLshort -> IO ())
glMultiTexCoord2sARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2sARB")

{-# NOINLINE glMultiTexCoord2sARBFunPtr #-}

-- | Usage: @'glMultiTexCoord2svARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord2sv'.


glMultiTexCoord2svARB :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glMultiTexCoord2svARB = ffienumPtrshortIOV glMultiTexCoord2svARBFunPtr

glMultiTexCoord2svARBFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glMultiTexCoord2svARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord2svARB")

{-# NOINLINE glMultiTexCoord2svARBFunPtr #-}

-- | Usage: @'glMultiTexCoord3dARB' target s t r@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordD@.
--
-- The parameter @t@ is a @CoordD@.
--
-- The parameter @r@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord3d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord3dv'.


glMultiTexCoord3dARB :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> m ()
glMultiTexCoord3dARB = ffienumdoubledoubledoubleIOV glMultiTexCoord3dARBFunPtr

glMultiTexCoord3dARBFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
glMultiTexCoord3dARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3dARB")

{-# NOINLINE glMultiTexCoord3dARBFunPtr #-}

-- | Usage: @'glMultiTexCoord3dvARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord3dv'.


glMultiTexCoord3dvARB :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glMultiTexCoord3dvARB = ffienumPtrdoubleIOV glMultiTexCoord3dvARBFunPtr

glMultiTexCoord3dvARBFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glMultiTexCoord3dvARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3dvARB")

{-# NOINLINE glMultiTexCoord3dvARBFunPtr #-}

-- | Usage: @'glMultiTexCoord3fARB' target s t r@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordF@.
--
-- The parameter @t@ is a @CoordF@.
--
-- The parameter @r@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord3f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord3fv'.


glMultiTexCoord3fARB :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLfloat -> m ()
glMultiTexCoord3fARB = ffienumfloatfloatfloatIOV glMultiTexCoord3fARBFunPtr

glMultiTexCoord3fARBFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
glMultiTexCoord3fARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3fARB")

{-# NOINLINE glMultiTexCoord3fARBFunPtr #-}

-- | Usage: @'glMultiTexCoord3fvARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord3fv'.


glMultiTexCoord3fvARB :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMultiTexCoord3fvARB = ffienumPtrfloatIOV glMultiTexCoord3fvARBFunPtr

glMultiTexCoord3fvARBFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMultiTexCoord3fvARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3fvARB")

{-# NOINLINE glMultiTexCoord3fvARBFunPtr #-}

-- | Usage: @'glMultiTexCoord3iARB' target s t r@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordI@.
--
-- The parameter @t@ is a @CoordI@.
--
-- The parameter @r@ is a @CoordI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord3i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord3iv'.


glMultiTexCoord3iARB :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> m ()
glMultiTexCoord3iARB = ffienumintintintIOV glMultiTexCoord3iARBFunPtr

glMultiTexCoord3iARBFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
glMultiTexCoord3iARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3iARB")

{-# NOINLINE glMultiTexCoord3iARBFunPtr #-}

-- | Usage: @'glMultiTexCoord3ivARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord3iv'.


glMultiTexCoord3ivARB :: MonadIO m => GLenum -> Ptr GLint -> m ()
glMultiTexCoord3ivARB = ffienumPtrintIOV glMultiTexCoord3ivARBFunPtr

glMultiTexCoord3ivARBFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glMultiTexCoord3ivARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3ivARB")

{-# NOINLINE glMultiTexCoord3ivARBFunPtr #-}

-- | Usage: @'glMultiTexCoord3sARB' target s t r@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordS@.
--
-- The parameter @t@ is a @CoordS@.
--
-- The parameter @r@ is a @CoordS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord3s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord3sv'.


glMultiTexCoord3sARB :: MonadIO m => GLenum -> GLshort -> GLshort -> GLshort -> m ()
glMultiTexCoord3sARB = ffienumshortshortshortIOV glMultiTexCoord3sARBFunPtr

glMultiTexCoord3sARBFunPtr :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
glMultiTexCoord3sARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3sARB")

{-# NOINLINE glMultiTexCoord3sARBFunPtr #-}

-- | Usage: @'glMultiTexCoord3svARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord3sv'.


glMultiTexCoord3svARB :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glMultiTexCoord3svARB = ffienumPtrshortIOV glMultiTexCoord3svARBFunPtr

glMultiTexCoord3svARBFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glMultiTexCoord3svARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord3svARB")

{-# NOINLINE glMultiTexCoord3svARBFunPtr #-}

-- | Usage: @'glMultiTexCoord4dARB' target s t r q@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordD@.
--
-- The parameter @t@ is a @CoordD@.
--
-- The parameter @r@ is a @CoordD@.
--
-- The parameter @q@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord4d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord4dv'.


glMultiTexCoord4dARB :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glMultiTexCoord4dARB = ffienumdoubledoubledoubledoubleIOV glMultiTexCoord4dARBFunPtr

glMultiTexCoord4dARBFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glMultiTexCoord4dARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4dARB")

{-# NOINLINE glMultiTexCoord4dARBFunPtr #-}

-- | Usage: @'glMultiTexCoord4dvARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord4dv'.


glMultiTexCoord4dvARB :: MonadIO m => GLenum -> Ptr GLdouble -> m ()
glMultiTexCoord4dvARB = ffienumPtrdoubleIOV glMultiTexCoord4dvARBFunPtr

glMultiTexCoord4dvARBFunPtr :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
glMultiTexCoord4dvARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4dvARB")

{-# NOINLINE glMultiTexCoord4dvARBFunPtr #-}

-- | Usage: @'glMultiTexCoord4fARB' target s t r q@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordF@.
--
-- The parameter @t@ is a @CoordF@.
--
-- The parameter @r@ is a @CoordF@.
--
-- The parameter @q@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord4f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord4fv'.


glMultiTexCoord4fARB :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glMultiTexCoord4fARB = ffienumfloatfloatfloatfloatIOV glMultiTexCoord4fARBFunPtr

glMultiTexCoord4fARBFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glMultiTexCoord4fARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4fARB")

{-# NOINLINE glMultiTexCoord4fARBFunPtr #-}

-- | Usage: @'glMultiTexCoord4fvARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord4fv'.


glMultiTexCoord4fvARB :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glMultiTexCoord4fvARB = ffienumPtrfloatIOV glMultiTexCoord4fvARBFunPtr

glMultiTexCoord4fvARBFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glMultiTexCoord4fvARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4fvARB")

{-# NOINLINE glMultiTexCoord4fvARBFunPtr #-}

-- | Usage: @'glMultiTexCoord4iARB' target s t r q@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordI@.
--
-- The parameter @t@ is a @CoordI@.
--
-- The parameter @r@ is a @CoordI@.
--
-- The parameter @q@ is a @CoordI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord4i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord4iv'.


glMultiTexCoord4iARB :: MonadIO m => GLenum -> GLint -> GLint -> GLint -> GLint -> m ()
glMultiTexCoord4iARB = ffienumintintintintIOV glMultiTexCoord4iARBFunPtr

glMultiTexCoord4iARBFunPtr :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
glMultiTexCoord4iARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4iARB")

{-# NOINLINE glMultiTexCoord4iARBFunPtr #-}

-- | Usage: @'glMultiTexCoord4ivARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord4iv'.


glMultiTexCoord4ivARB :: MonadIO m => GLenum -> Ptr GLint -> m ()
glMultiTexCoord4ivARB = ffienumPtrintIOV glMultiTexCoord4ivARBFunPtr

glMultiTexCoord4ivARBFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glMultiTexCoord4ivARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4ivARB")

{-# NOINLINE glMultiTexCoord4ivARBFunPtr #-}

-- | Usage: @'glMultiTexCoord4sARB' target s t r q@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @s@ is a @CoordS@.
--
-- The parameter @t@ is a @CoordS@.
--
-- The parameter @r@ is a @CoordS@.
--
-- The parameter @q@ is a @CoordS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord4s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Internal.Shared.glMultiTexCoord4sv'.


glMultiTexCoord4sARB :: MonadIO m => GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> m ()
glMultiTexCoord4sARB = ffienumshortshortshortshortIOV glMultiTexCoord4sARBFunPtr

glMultiTexCoord4sARBFunPtr :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
glMultiTexCoord4sARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4sARB")

{-# NOINLINE glMultiTexCoord4sARBFunPtr #-}

-- | Usage: @'glMultiTexCoord4svARB' target v@
--
-- The parameter @target@ is a @TextureUnit@.
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiTexCoord4sv'.


glMultiTexCoord4svARB :: MonadIO m => GLenum -> Ptr GLshort -> m ()
glMultiTexCoord4svARB = ffienumPtrshortIOV glMultiTexCoord4svARBFunPtr

glMultiTexCoord4svARBFunPtr :: FunPtr (GLenum -> Ptr GLshort -> IO ())
glMultiTexCoord4svARBFunPtr = unsafePerformIO (getProcAddress "glMultiTexCoord4svARB")

{-# NOINLINE glMultiTexCoord4svARBFunPtr #-}

pattern GL_ACTIVE_TEXTURE_ARB  :: (Eq a, Num a) => a

pattern GL_ACTIVE_TEXTURE_ARB = 0x84E0

pattern GL_CLIENT_ACTIVE_TEXTURE_ARB  :: (Eq a, Num a) => a

pattern GL_CLIENT_ACTIVE_TEXTURE_ARB = 0x84E1

pattern GL_MAX_TEXTURE_UNITS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_TEXTURE_UNITS_ARB = 0x84E2

pattern GL_TEXTURE10_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE10_ARB = 0x84CA

pattern GL_TEXTURE11_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE11_ARB = 0x84CB

pattern GL_TEXTURE12_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE12_ARB = 0x84CC

pattern GL_TEXTURE13_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE13_ARB = 0x84CD

pattern GL_TEXTURE14_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE14_ARB = 0x84CE

pattern GL_TEXTURE15_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE15_ARB = 0x84CF

pattern GL_TEXTURE16_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE16_ARB = 0x84D0

pattern GL_TEXTURE17_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE17_ARB = 0x84D1

pattern GL_TEXTURE18_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE18_ARB = 0x84D2

pattern GL_TEXTURE19_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE19_ARB = 0x84D3

pattern GL_TEXTURE20_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE20_ARB = 0x84D4

pattern GL_TEXTURE21_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE21_ARB = 0x84D5

pattern GL_TEXTURE22_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE22_ARB = 0x84D6

pattern GL_TEXTURE23_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE23_ARB = 0x84D7

pattern GL_TEXTURE24_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE24_ARB = 0x84D8

pattern GL_TEXTURE25_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE25_ARB = 0x84D9

pattern GL_TEXTURE26_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE26_ARB = 0x84DA

pattern GL_TEXTURE27_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE27_ARB = 0x84DB

pattern GL_TEXTURE28_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE28_ARB = 0x84DC

pattern GL_TEXTURE29_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE29_ARB = 0x84DD

pattern GL_TEXTURE2_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE2_ARB = 0x84C2

pattern GL_TEXTURE30_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE30_ARB = 0x84DE

pattern GL_TEXTURE31_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE31_ARB = 0x84DF

pattern GL_TEXTURE3_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE3_ARB = 0x84C3

pattern GL_TEXTURE4_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE4_ARB = 0x84C4

pattern GL_TEXTURE5_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE5_ARB = 0x84C5

pattern GL_TEXTURE6_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE6_ARB = 0x84C6

pattern GL_TEXTURE7_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE7_ARB = 0x84C7

pattern GL_TEXTURE8_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE8_ARB = 0x84C8

pattern GL_TEXTURE9_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE9_ARB = 0x84C9