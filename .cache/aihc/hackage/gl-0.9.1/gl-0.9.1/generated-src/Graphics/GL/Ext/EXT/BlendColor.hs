-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BlendColor (
  -- * Extension Support
    gl_EXT_blend_color

  -- * GL_EXT_blend_color
  , glBlendColorEXT
  , pattern GL_BLEND_COLOR_EXT
  , pattern GL_CONSTANT_ALPHA_EXT
  , pattern GL_CONSTANT_COLOR_EXT
  , pattern GL_ONE_MINUS_CONSTANT_ALPHA_EXT
  , pattern GL_ONE_MINUS_CONSTANT_COLOR_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/blend_color.txt GL_EXT_blend_color> extension is available.

gl_EXT_blend_color :: Bool
gl_EXT_blend_color = member "GL_EXT_blend_color" extensions
{-# NOINLINE gl_EXT_blend_color #-}

-- | Usage: @'glBlendColorEXT' red green blue alpha@
--
-- The parameter @red@ is a @ColorF@.
--
-- The parameter @green@ is a @ColorF@.
--
-- The parameter @blue@ is a @ColorF@.
--
-- The parameter @alpha@ is a @ColorF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendColor'.


glBlendColorEXT :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glBlendColorEXT = ffifloatfloatfloatfloatIOV glBlendColorEXTFunPtr

glBlendColorEXTFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glBlendColorEXTFunPtr = unsafePerformIO (getProcAddress "glBlendColorEXT")

{-# NOINLINE glBlendColorEXTFunPtr #-}

pattern GL_BLEND_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_BLEND_COLOR_EXT = 0x8005

pattern GL_CONSTANT_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_CONSTANT_ALPHA_EXT = 0x8003

pattern GL_CONSTANT_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_CONSTANT_COLOR_EXT = 0x8001

pattern GL_ONE_MINUS_CONSTANT_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_ONE_MINUS_CONSTANT_ALPHA_EXT = 0x8004

pattern GL_ONE_MINUS_CONSTANT_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_ONE_MINUS_CONSTANT_COLOR_EXT = 0x8002