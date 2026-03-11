-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Sprite (
  -- * Extension Support
    gl_SGIX_sprite

  -- * GL_SGIX_sprite
  , glSpriteParameterfSGIX
  , glSpriteParameterfvSGIX
  , glSpriteParameteriSGIX
  , glSpriteParameterivSGIX
  , pattern GL_SPRITE_AXIAL_SGIX
  , pattern GL_SPRITE_AXIS_SGIX
  , pattern GL_SPRITE_EYE_ALIGNED_SGIX
  , pattern GL_SPRITE_MODE_SGIX
  , pattern GL_SPRITE_OBJECT_ALIGNED_SGIX
  , pattern GL_SPRITE_SGIX
  , pattern GL_SPRITE_TRANSLATION_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/sprite.txt GL_SGIX_sprite> extension is available.

gl_SGIX_sprite :: Bool
gl_SGIX_sprite = member "GL_SGIX_sprite" extensions
{-# NOINLINE gl_SGIX_sprite #-}

-- | Usage: @'glSpriteParameterfSGIX' pname param@
--
-- The parameter @pname@ is a @SpriteParameterNameSGIX@.
--
-- The parameter @param@ is a @CheckedFloat32@.


glSpriteParameterfSGIX :: MonadIO m => GLenum -> GLfloat -> m ()
glSpriteParameterfSGIX = ffienumfloatIOV glSpriteParameterfSGIXFunPtr

glSpriteParameterfSGIXFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glSpriteParameterfSGIXFunPtr = unsafePerformIO (getProcAddress "glSpriteParameterfSGIX")

{-# NOINLINE glSpriteParameterfSGIXFunPtr #-}

-- | Usage: @'glSpriteParameterfvSGIX' pname params@
--
-- The parameter @pname@ is a @SpriteParameterNameSGIX@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glSpriteParameterfvSGIX :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glSpriteParameterfvSGIX = ffienumPtrfloatIOV glSpriteParameterfvSGIXFunPtr

glSpriteParameterfvSGIXFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glSpriteParameterfvSGIXFunPtr = unsafePerformIO (getProcAddress "glSpriteParameterfvSGIX")

{-# NOINLINE glSpriteParameterfvSGIXFunPtr #-}

-- | Usage: @'glSpriteParameteriSGIX' pname param@
--
-- The parameter @pname@ is a @SpriteParameterNameSGIX@.
--
-- The parameter @param@ is a @CheckedInt32@.


glSpriteParameteriSGIX :: MonadIO m => GLenum -> GLint -> m ()
glSpriteParameteriSGIX = ffienumintIOV glSpriteParameteriSGIXFunPtr

glSpriteParameteriSGIXFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glSpriteParameteriSGIXFunPtr = unsafePerformIO (getProcAddress "glSpriteParameteriSGIX")

{-# NOINLINE glSpriteParameteriSGIXFunPtr #-}

-- | Usage: @'glSpriteParameterivSGIX' pname params@
--
-- The parameter @pname@ is a @SpriteParameterNameSGIX@.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glSpriteParameterivSGIX :: MonadIO m => GLenum -> Ptr GLint -> m ()
glSpriteParameterivSGIX = ffienumPtrintIOV glSpriteParameterivSGIXFunPtr

glSpriteParameterivSGIXFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glSpriteParameterivSGIXFunPtr = unsafePerformIO (getProcAddress "glSpriteParameterivSGIX")

{-# NOINLINE glSpriteParameterivSGIXFunPtr #-}

pattern GL_SPRITE_AXIAL_SGIX  :: (Eq a, Num a) => a

pattern GL_SPRITE_AXIAL_SGIX = 0x814C

pattern GL_SPRITE_AXIS_SGIX  :: (Eq a, Num a) => a

pattern GL_SPRITE_AXIS_SGIX = 0x814A

pattern GL_SPRITE_EYE_ALIGNED_SGIX  :: (Eq a, Num a) => a

pattern GL_SPRITE_EYE_ALIGNED_SGIX = 0x814E

pattern GL_SPRITE_MODE_SGIX  :: (Eq a, Num a) => a

pattern GL_SPRITE_MODE_SGIX = 0x8149

pattern GL_SPRITE_OBJECT_ALIGNED_SGIX  :: (Eq a, Num a) => a

pattern GL_SPRITE_OBJECT_ALIGNED_SGIX = 0x814D

pattern GL_SPRITE_SGIX  :: (Eq a, Num a) => a

pattern GL_SPRITE_SGIX = 0x8148

pattern GL_SPRITE_TRANSLATION_SGIX  :: (Eq a, Num a) => a

pattern GL_SPRITE_TRANSLATION_SGIX = 0x814B