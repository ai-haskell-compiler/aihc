-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.FogFunction (
  -- * Extension Support
    gl_SGIS_fog_function

  -- * GL_SGIS_fog_function
  , glFogFuncSGIS
  , glGetFogFuncSGIS
  , pattern GL_FOG_FUNC_POINTS_SGIS
  , pattern GL_FOG_FUNC_SGIS
  , pattern GL_MAX_FOG_FUNC_POINTS_SGIS
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_SGIS_fog_function extension is available.

gl_SGIS_fog_function :: Bool
gl_SGIS_fog_function = member "GL_SGIS_fog_function" extensions
{-# NOINLINE gl_SGIS_fog_function #-}

-- | Usage: @'glFogFuncSGIS' n points@
--
-- The length of @points@ should be @n*2@.


glFogFuncSGIS :: MonadIO m => GLsizei -> Ptr GLfloat -> m ()
glFogFuncSGIS = ffisizeiPtrfloatIOV glFogFuncSGISFunPtr

glFogFuncSGISFunPtr :: FunPtr (GLsizei -> Ptr GLfloat -> IO ())
glFogFuncSGISFunPtr = unsafePerformIO (getProcAddress "glFogFuncSGIS")

{-# NOINLINE glFogFuncSGISFunPtr #-}

-- | Usage: @'glGetFogFuncSGIS' points@
--
-- The length of @points@ should be @COMPSIZE()@.


glGetFogFuncSGIS :: MonadIO m => Ptr GLfloat -> m ()
glGetFogFuncSGIS = ffiPtrfloatIOV glGetFogFuncSGISFunPtr

glGetFogFuncSGISFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glGetFogFuncSGISFunPtr = unsafePerformIO (getProcAddress "glGetFogFuncSGIS")

{-# NOINLINE glGetFogFuncSGISFunPtr #-}

pattern GL_FOG_FUNC_POINTS_SGIS  :: (Eq a, Num a) => a

pattern GL_FOG_FUNC_POINTS_SGIS = 0x812B

pattern GL_FOG_FUNC_SGIS  :: (Eq a, Num a) => a

pattern GL_FOG_FUNC_SGIS = 0x812A

pattern GL_MAX_FOG_FUNC_POINTS_SGIS  :: (Eq a, Num a) => a

pattern GL_MAX_FOG_FUNC_POINTS_SGIS = 0x812C