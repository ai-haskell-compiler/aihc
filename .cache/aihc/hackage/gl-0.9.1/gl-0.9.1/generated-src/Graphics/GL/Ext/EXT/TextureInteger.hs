-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureInteger (
  -- * Extension Support
    gl_EXT_texture_integer

  -- * GL_EXT_texture_integer
  , glClearColorIiEXT
  , glClearColorIuiEXT
  , glGetTexParameterIivEXT
  , glGetTexParameterIuivEXT
  , glTexParameterIivEXT
  , glTexParameterIuivEXT
  , pattern GL_ALPHA16I_EXT
  , pattern GL_ALPHA16UI_EXT
  , pattern GL_ALPHA32I_EXT
  , pattern GL_ALPHA32UI_EXT
  , pattern GL_ALPHA8I_EXT
  , pattern GL_ALPHA8UI_EXT
  , pattern GL_ALPHA_INTEGER_EXT
  , pattern GL_BGRA_INTEGER_EXT
  , pattern GL_BGR_INTEGER_EXT
  , pattern GL_BLUE_INTEGER_EXT
  , pattern GL_GREEN_INTEGER_EXT
  , pattern GL_INTENSITY16I_EXT
  , pattern GL_INTENSITY16UI_EXT
  , pattern GL_INTENSITY32I_EXT
  , pattern GL_INTENSITY32UI_EXT
  , pattern GL_INTENSITY8I_EXT
  , pattern GL_INTENSITY8UI_EXT
  , pattern GL_LUMINANCE16I_EXT
  , pattern GL_LUMINANCE16UI_EXT
  , pattern GL_LUMINANCE32I_EXT
  , pattern GL_LUMINANCE32UI_EXT
  , pattern GL_LUMINANCE8I_EXT
  , pattern GL_LUMINANCE8UI_EXT
  , pattern GL_LUMINANCE_ALPHA16I_EXT
  , pattern GL_LUMINANCE_ALPHA16UI_EXT
  , pattern GL_LUMINANCE_ALPHA32I_EXT
  , pattern GL_LUMINANCE_ALPHA32UI_EXT
  , pattern GL_LUMINANCE_ALPHA8I_EXT
  , pattern GL_LUMINANCE_ALPHA8UI_EXT
  , pattern GL_LUMINANCE_ALPHA_INTEGER_EXT
  , pattern GL_LUMINANCE_INTEGER_EXT
  , pattern GL_RED_INTEGER_EXT
  , pattern GL_RGB16I_EXT
  , pattern GL_RGB16UI_EXT
  , pattern GL_RGB32I_EXT
  , pattern GL_RGB32UI_EXT
  , pattern GL_RGB8I_EXT
  , pattern GL_RGB8UI_EXT
  , pattern GL_RGBA16I_EXT
  , pattern GL_RGBA16UI_EXT
  , pattern GL_RGBA32I_EXT
  , pattern GL_RGBA32UI_EXT
  , pattern GL_RGBA8I_EXT
  , pattern GL_RGBA8UI_EXT
  , pattern GL_RGBA_INTEGER_EXT
  , pattern GL_RGBA_INTEGER_MODE_EXT
  , pattern GL_RGB_INTEGER_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_integer.txt GL_EXT_texture_integer> extension is available.

gl_EXT_texture_integer :: Bool
gl_EXT_texture_integer = member "GL_EXT_texture_integer" extensions
{-# NOINLINE gl_EXT_texture_integer #-}

-- | Usage: @'glClearColorIiEXT' red green blue alpha@


glClearColorIiEXT :: MonadIO m => GLint -> GLint -> GLint -> GLint -> m ()
glClearColorIiEXT = ffiintintintintIOV glClearColorIiEXTFunPtr

glClearColorIiEXTFunPtr :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
glClearColorIiEXTFunPtr = unsafePerformIO (getProcAddress "glClearColorIiEXT")

{-# NOINLINE glClearColorIiEXTFunPtr #-}

-- | Usage: @'glClearColorIuiEXT' red green blue alpha@


glClearColorIuiEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> GLuint -> m ()
glClearColorIuiEXT = ffiuintuintuintuintIOV glClearColorIuiEXTFunPtr

glClearColorIuiEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glClearColorIuiEXTFunPtr = unsafePerformIO (getProcAddress "glClearColorIuiEXT")

{-# NOINLINE glClearColorIuiEXTFunPtr #-}

pattern GL_ALPHA16I_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA16I_EXT = 0x8D8A

pattern GL_ALPHA16UI_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA16UI_EXT = 0x8D78

pattern GL_ALPHA32I_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA32I_EXT = 0x8D84

pattern GL_ALPHA32UI_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA32UI_EXT = 0x8D72

pattern GL_ALPHA8I_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA8I_EXT = 0x8D90

pattern GL_ALPHA8UI_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA8UI_EXT = 0x8D7E

pattern GL_ALPHA_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA_INTEGER_EXT = 0x8D97

pattern GL_BGRA_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_BGRA_INTEGER_EXT = 0x8D9B

pattern GL_BGR_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_BGR_INTEGER_EXT = 0x8D9A

pattern GL_BLUE_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_BLUE_INTEGER_EXT = 0x8D96

pattern GL_GREEN_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_GREEN_INTEGER_EXT = 0x8D95

pattern GL_INTENSITY16I_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY16I_EXT = 0x8D8B

pattern GL_INTENSITY16UI_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY16UI_EXT = 0x8D79

pattern GL_INTENSITY32I_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY32I_EXT = 0x8D85

pattern GL_INTENSITY32UI_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY32UI_EXT = 0x8D73

pattern GL_INTENSITY8I_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY8I_EXT = 0x8D91

pattern GL_INTENSITY8UI_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY8UI_EXT = 0x8D7F

pattern GL_LUMINANCE16I_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE16I_EXT = 0x8D8C

pattern GL_LUMINANCE16UI_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE16UI_EXT = 0x8D7A

pattern GL_LUMINANCE32I_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE32I_EXT = 0x8D86

pattern GL_LUMINANCE32UI_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE32UI_EXT = 0x8D74

pattern GL_LUMINANCE8I_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE8I_EXT = 0x8D92

pattern GL_LUMINANCE8UI_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE8UI_EXT = 0x8D80

pattern GL_LUMINANCE_ALPHA16I_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA16I_EXT = 0x8D8D

pattern GL_LUMINANCE_ALPHA16UI_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA16UI_EXT = 0x8D7B

pattern GL_LUMINANCE_ALPHA32I_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA32I_EXT = 0x8D87

pattern GL_LUMINANCE_ALPHA32UI_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA32UI_EXT = 0x8D75

pattern GL_LUMINANCE_ALPHA8I_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA8I_EXT = 0x8D93

pattern GL_LUMINANCE_ALPHA8UI_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA8UI_EXT = 0x8D81

pattern GL_LUMINANCE_ALPHA_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA_INTEGER_EXT = 0x8D9D

pattern GL_LUMINANCE_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_INTEGER_EXT = 0x8D9C

pattern GL_RED_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_RED_INTEGER_EXT = 0x8D94

pattern GL_RGB16I_EXT  :: (Eq a, Num a) => a

pattern GL_RGB16I_EXT = 0x8D89

pattern GL_RGB16UI_EXT  :: (Eq a, Num a) => a

pattern GL_RGB16UI_EXT = 0x8D77

pattern GL_RGB32I_EXT  :: (Eq a, Num a) => a

pattern GL_RGB32I_EXT = 0x8D83

pattern GL_RGB32UI_EXT  :: (Eq a, Num a) => a

pattern GL_RGB32UI_EXT = 0x8D71

pattern GL_RGB8I_EXT  :: (Eq a, Num a) => a

pattern GL_RGB8I_EXT = 0x8D8F

pattern GL_RGB8UI_EXT  :: (Eq a, Num a) => a

pattern GL_RGB8UI_EXT = 0x8D7D

pattern GL_RGBA16I_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA16I_EXT = 0x8D88

pattern GL_RGBA16UI_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA16UI_EXT = 0x8D76

pattern GL_RGBA32I_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA32I_EXT = 0x8D82

pattern GL_RGBA32UI_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA32UI_EXT = 0x8D70

pattern GL_RGBA8I_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA8I_EXT = 0x8D8E

pattern GL_RGBA8UI_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA8UI_EXT = 0x8D7C

pattern GL_RGBA_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA_INTEGER_EXT = 0x8D99

pattern GL_RGBA_INTEGER_MODE_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA_INTEGER_MODE_EXT = 0x8D9E

pattern GL_RGB_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_RGB_INTEGER_EXT = 0x8D98