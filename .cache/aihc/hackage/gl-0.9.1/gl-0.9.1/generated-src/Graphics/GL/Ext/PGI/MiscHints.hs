-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.PGI.MiscHints (
  -- * Extension Support
    gl_PGI_misc_hints

  -- * GL_PGI_misc_hints
  , glHintPGI
  , pattern GL_ALLOW_DRAW_FRG_HINT_PGI
  , pattern GL_ALLOW_DRAW_MEM_HINT_PGI
  , pattern GL_ALLOW_DRAW_OBJ_HINT_PGI
  , pattern GL_ALLOW_DRAW_WIN_HINT_PGI
  , pattern GL_ALWAYS_FAST_HINT_PGI
  , pattern GL_ALWAYS_SOFT_HINT_PGI
  , pattern GL_BACK_NORMALS_HINT_PGI
  , pattern GL_CLIP_FAR_HINT_PGI
  , pattern GL_CLIP_NEAR_HINT_PGI
  , pattern GL_CONSERVE_MEMORY_HINT_PGI
  , pattern GL_FULL_STIPPLE_HINT_PGI
  , pattern GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI
  , pattern GL_NATIVE_GRAPHICS_END_HINT_PGI
  , pattern GL_NATIVE_GRAPHICS_HANDLE_PGI
  , pattern GL_PREFER_DOUBLEBUFFER_HINT_PGI
  , pattern GL_RECLAIM_MEMORY_HINT_PGI
  , pattern GL_STRICT_DEPTHFUNC_HINT_PGI
  , pattern GL_STRICT_LIGHTING_HINT_PGI
  , pattern GL_STRICT_SCISSOR_HINT_PGI
  , pattern GL_WIDE_LINE_HINT_PGI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/PGI/misc_hints.txt GL_PGI_misc_hints> extension is available.

gl_PGI_misc_hints :: Bool
gl_PGI_misc_hints = member "GL_PGI_misc_hints" extensions
{-# NOINLINE gl_PGI_misc_hints #-}

-- | Usage: @'glHintPGI' target mode@
--
-- The parameter @target@ is a @HintTargetPGI@.


glHintPGI :: MonadIO m => GLenum -> GLint -> m ()
glHintPGI = ffienumintIOV glHintPGIFunPtr

glHintPGIFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glHintPGIFunPtr = unsafePerformIO (getProcAddress "glHintPGI")

{-# NOINLINE glHintPGIFunPtr #-}

pattern GL_ALLOW_DRAW_FRG_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_ALLOW_DRAW_FRG_HINT_PGI = 0x1A210

pattern GL_ALLOW_DRAW_MEM_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_ALLOW_DRAW_MEM_HINT_PGI = 0x1A211

pattern GL_ALLOW_DRAW_OBJ_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_ALLOW_DRAW_OBJ_HINT_PGI = 0x1A20E

pattern GL_ALLOW_DRAW_WIN_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_ALLOW_DRAW_WIN_HINT_PGI = 0x1A20F

pattern GL_ALWAYS_FAST_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_ALWAYS_FAST_HINT_PGI = 0x1A20C

pattern GL_ALWAYS_SOFT_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_ALWAYS_SOFT_HINT_PGI = 0x1A20D

pattern GL_BACK_NORMALS_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_BACK_NORMALS_HINT_PGI = 0x1A223

pattern GL_CLIP_FAR_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_CLIP_FAR_HINT_PGI = 0x1A221

pattern GL_CLIP_NEAR_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_CLIP_NEAR_HINT_PGI = 0x1A220

pattern GL_CONSERVE_MEMORY_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_CONSERVE_MEMORY_HINT_PGI = 0x1A1FD

pattern GL_FULL_STIPPLE_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_FULL_STIPPLE_HINT_PGI = 0x1A219

pattern GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI = 0x1A203

pattern GL_NATIVE_GRAPHICS_END_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_NATIVE_GRAPHICS_END_HINT_PGI = 0x1A204

pattern GL_NATIVE_GRAPHICS_HANDLE_PGI  :: (Eq a, Num a) => a

pattern GL_NATIVE_GRAPHICS_HANDLE_PGI = 0x1A202

pattern GL_PREFER_DOUBLEBUFFER_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_PREFER_DOUBLEBUFFER_HINT_PGI = 0x1A1F8

pattern GL_RECLAIM_MEMORY_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_RECLAIM_MEMORY_HINT_PGI = 0x1A1FE

pattern GL_STRICT_DEPTHFUNC_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_STRICT_DEPTHFUNC_HINT_PGI = 0x1A216

pattern GL_STRICT_LIGHTING_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_STRICT_LIGHTING_HINT_PGI = 0x1A217

pattern GL_STRICT_SCISSOR_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_STRICT_SCISSOR_HINT_PGI = 0x1A218

pattern GL_WIDE_LINE_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_WIDE_LINE_HINT_PGI = 0x1A222