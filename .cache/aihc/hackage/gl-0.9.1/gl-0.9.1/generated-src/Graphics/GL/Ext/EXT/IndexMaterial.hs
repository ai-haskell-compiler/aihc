-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.IndexMaterial (
  -- * Extension Support
    gl_EXT_index_material

  -- * GL_EXT_index_material
  , glIndexMaterialEXT
  , pattern GL_INDEX_MATERIAL_EXT
  , pattern GL_INDEX_MATERIAL_FACE_EXT
  , pattern GL_INDEX_MATERIAL_PARAMETER_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/index_material.txt GL_EXT_index_material> extension is available.

gl_EXT_index_material :: Bool
gl_EXT_index_material = member "GL_EXT_index_material" extensions
{-# NOINLINE gl_EXT_index_material #-}

-- | Usage: @'glIndexMaterialEXT' face mode@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @mode@ is a @IndexMaterialParameterEXT@.


glIndexMaterialEXT :: MonadIO m => GLenum -> GLenum -> m ()
glIndexMaterialEXT = ffienumenumIOV glIndexMaterialEXTFunPtr

glIndexMaterialEXTFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glIndexMaterialEXTFunPtr = unsafePerformIO (getProcAddress "glIndexMaterialEXT")

{-# NOINLINE glIndexMaterialEXTFunPtr #-}

pattern GL_INDEX_MATERIAL_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_MATERIAL_EXT = 0x81B8

pattern GL_INDEX_MATERIAL_FACE_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_MATERIAL_FACE_EXT = 0x81BA

pattern GL_INDEX_MATERIAL_PARAMETER_EXT  :: (Eq a, Num a) => a

pattern GL_INDEX_MATERIAL_PARAMETER_EXT = 0x81B9