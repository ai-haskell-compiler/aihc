-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.EnvmapBumpmap (
  -- * Extension Support
    gl_ATI_envmap_bumpmap

  -- * GL_ATI_envmap_bumpmap
  , glGetTexBumpParameterfvATI
  , glGetTexBumpParameterivATI
  , glTexBumpParameterfvATI
  , glTexBumpParameterivATI
  , pattern GL_BUMP_ENVMAP_ATI
  , pattern GL_BUMP_NUM_TEX_UNITS_ATI
  , pattern GL_BUMP_ROT_MATRIX_ATI
  , pattern GL_BUMP_ROT_MATRIX_SIZE_ATI
  , pattern GL_BUMP_TARGET_ATI
  , pattern GL_BUMP_TEX_UNITS_ATI
  , pattern GL_DU8DV8_ATI
  , pattern GL_DUDV_ATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/envmap_bumpmap.txt GL_ATI_envmap_bumpmap> extension is available.

gl_ATI_envmap_bumpmap :: Bool
gl_ATI_envmap_bumpmap = member "GL_ATI_envmap_bumpmap" extensions
{-# NOINLINE gl_ATI_envmap_bumpmap #-}

-- | Usage: @'glGetTexBumpParameterfvATI' pname param@
--
-- The parameter @pname@ is a @GetTexBumpParameterATI@.
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glGetTexBumpParameterfvATI :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glGetTexBumpParameterfvATI = ffienumPtrfloatIOV glGetTexBumpParameterfvATIFunPtr

glGetTexBumpParameterfvATIFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glGetTexBumpParameterfvATIFunPtr = unsafePerformIO (getProcAddress "glGetTexBumpParameterfvATI")

{-# NOINLINE glGetTexBumpParameterfvATIFunPtr #-}

-- | Usage: @'glGetTexBumpParameterivATI' pname param@
--
-- The parameter @pname@ is a @GetTexBumpParameterATI@.
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glGetTexBumpParameterivATI :: MonadIO m => GLenum -> Ptr GLint -> m ()
glGetTexBumpParameterivATI = ffienumPtrintIOV glGetTexBumpParameterivATIFunPtr

glGetTexBumpParameterivATIFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glGetTexBumpParameterivATIFunPtr = unsafePerformIO (getProcAddress "glGetTexBumpParameterivATI")

{-# NOINLINE glGetTexBumpParameterivATIFunPtr #-}

-- | Usage: @'glTexBumpParameterfvATI' pname param@
--
-- The parameter @pname@ is a @TexBumpParameterATI@.
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glTexBumpParameterfvATI :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glTexBumpParameterfvATI = ffienumPtrfloatIOV glTexBumpParameterfvATIFunPtr

glTexBumpParameterfvATIFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glTexBumpParameterfvATIFunPtr = unsafePerformIO (getProcAddress "glTexBumpParameterfvATI")

{-# NOINLINE glTexBumpParameterfvATIFunPtr #-}

-- | Usage: @'glTexBumpParameterivATI' pname param@
--
-- The parameter @pname@ is a @TexBumpParameterATI@.
--
-- The length of @param@ should be @COMPSIZE(pname)@.


glTexBumpParameterivATI :: MonadIO m => GLenum -> Ptr GLint -> m ()
glTexBumpParameterivATI = ffienumPtrintIOV glTexBumpParameterivATIFunPtr

glTexBumpParameterivATIFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glTexBumpParameterivATIFunPtr = unsafePerformIO (getProcAddress "glTexBumpParameterivATI")

{-# NOINLINE glTexBumpParameterivATIFunPtr #-}

pattern GL_BUMP_ENVMAP_ATI  :: (Eq a, Num a) => a

pattern GL_BUMP_ENVMAP_ATI = 0x877B

pattern GL_BUMP_NUM_TEX_UNITS_ATI  :: (Eq a, Num a) => a

pattern GL_BUMP_NUM_TEX_UNITS_ATI = 0x8777

pattern GL_BUMP_ROT_MATRIX_ATI  :: (Eq a, Num a) => a

pattern GL_BUMP_ROT_MATRIX_ATI = 0x8775

pattern GL_BUMP_ROT_MATRIX_SIZE_ATI  :: (Eq a, Num a) => a

pattern GL_BUMP_ROT_MATRIX_SIZE_ATI = 0x8776

pattern GL_BUMP_TARGET_ATI  :: (Eq a, Num a) => a

pattern GL_BUMP_TARGET_ATI = 0x877C

pattern GL_BUMP_TEX_UNITS_ATI  :: (Eq a, Num a) => a

pattern GL_BUMP_TEX_UNITS_ATI = 0x8778

pattern GL_DU8DV8_ATI  :: (Eq a, Num a) => a

pattern GL_DU8DV8_ATI = 0x877A

pattern GL_DUDV_ATI  :: (Eq a, Num a) => a

pattern GL_DUDV_ATI = 0x8779