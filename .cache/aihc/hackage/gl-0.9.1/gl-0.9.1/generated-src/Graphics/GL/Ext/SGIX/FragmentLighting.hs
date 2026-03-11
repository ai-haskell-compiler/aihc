-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.FragmentLighting (
  -- * Extension Support
    gl_SGIX_fragment_lighting

  -- * GL_SGIX_fragment_lighting
  , glFragmentColorMaterialSGIX
  , glFragmentLightModelfSGIX
  , glFragmentLightModelfvSGIX
  , glFragmentLightModeliSGIX
  , glFragmentLightModelivSGIX
  , glFragmentLightfSGIX
  , glFragmentLightfvSGIX
  , glFragmentLightiSGIX
  , glFragmentLightivSGIX
  , glFragmentMaterialfSGIX
  , glFragmentMaterialfvSGIX
  , glFragmentMaterialiSGIX
  , glFragmentMaterialivSGIX
  , glGetFragmentLightfvSGIX
  , glGetFragmentLightivSGIX
  , glGetFragmentMaterialfvSGIX
  , glGetFragmentMaterialivSGIX
  , glLightEnviSGIX
  , pattern GL_CURRENT_RASTER_NORMAL_SGIX
  , pattern GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX
  , pattern GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX
  , pattern GL_FRAGMENT_COLOR_MATERIAL_SGIX
  , pattern GL_FRAGMENT_LIGHT0_SGIX
  , pattern GL_FRAGMENT_LIGHT1_SGIX
  , pattern GL_FRAGMENT_LIGHT2_SGIX
  , pattern GL_FRAGMENT_LIGHT3_SGIX
  , pattern GL_FRAGMENT_LIGHT4_SGIX
  , pattern GL_FRAGMENT_LIGHT5_SGIX
  , pattern GL_FRAGMENT_LIGHT6_SGIX
  , pattern GL_FRAGMENT_LIGHT7_SGIX
  , pattern GL_FRAGMENT_LIGHTING_SGIX
  , pattern GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX
  , pattern GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX
  , pattern GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX
  , pattern GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX
  , pattern GL_LIGHT_ENV_MODE_SGIX
  , pattern GL_MAX_ACTIVE_LIGHTS_SGIX
  , pattern GL_MAX_FRAGMENT_LIGHTS_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_SGIX_fragment_lighting extension is available.

gl_SGIX_fragment_lighting :: Bool
gl_SGIX_fragment_lighting = member "GL_SGIX_fragment_lighting" extensions
{-# NOINLINE gl_SGIX_fragment_lighting #-}

-- | Usage: @'glFragmentColorMaterialSGIX' face mode@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @mode@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.


glFragmentColorMaterialSGIX :: MonadIO m => GLenum -> GLenum -> m ()
glFragmentColorMaterialSGIX = ffienumenumIOV glFragmentColorMaterialSGIXFunPtr

glFragmentColorMaterialSGIXFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glFragmentColorMaterialSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentColorMaterialSGIX")

{-# NOINLINE glFragmentColorMaterialSGIXFunPtr #-}

-- | Usage: @'glFragmentLightModelfSGIX' pname param@
--
-- The parameter @pname@ is a @FragmentLightModelParameterSGIX@, one of: 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX'.
--
-- The parameter @param@ is a @CheckedFloat32@.


glFragmentLightModelfSGIX :: MonadIO m => GLenum -> GLfloat -> m ()
glFragmentLightModelfSGIX = ffienumfloatIOV glFragmentLightModelfSGIXFunPtr

glFragmentLightModelfSGIXFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glFragmentLightModelfSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentLightModelfSGIX")

{-# NOINLINE glFragmentLightModelfSGIXFunPtr #-}

-- | Usage: @'glFragmentLightModelfvSGIX' pname params@
--
-- The parameter @pname@ is a @FragmentLightModelParameterSGIX@, one of: 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glFragmentLightModelfvSGIX :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glFragmentLightModelfvSGIX = ffienumPtrfloatIOV glFragmentLightModelfvSGIXFunPtr

glFragmentLightModelfvSGIXFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glFragmentLightModelfvSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentLightModelfvSGIX")

{-# NOINLINE glFragmentLightModelfvSGIXFunPtr #-}

-- | Usage: @'glFragmentLightModeliSGIX' pname param@
--
-- The parameter @pname@ is a @FragmentLightModelParameterSGIX@, one of: 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX'.
--
-- The parameter @param@ is a @CheckedInt32@.


glFragmentLightModeliSGIX :: MonadIO m => GLenum -> GLint -> m ()
glFragmentLightModeliSGIX = ffienumintIOV glFragmentLightModeliSGIXFunPtr

glFragmentLightModeliSGIXFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glFragmentLightModeliSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentLightModeliSGIX")

{-# NOINLINE glFragmentLightModeliSGIXFunPtr #-}

-- | Usage: @'glFragmentLightModelivSGIX' pname params@
--
-- The parameter @pname@ is a @FragmentLightModelParameterSGIX@, one of: 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glFragmentLightModelivSGIX :: MonadIO m => GLenum -> Ptr GLint -> m ()
glFragmentLightModelivSGIX = ffienumPtrintIOV glFragmentLightModelivSGIXFunPtr

glFragmentLightModelivSGIXFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glFragmentLightModelivSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentLightModelivSGIX")

{-# NOINLINE glFragmentLightModelivSGIXFunPtr #-}

-- | Usage: @'glFragmentLightfSGIX' light pname param@
--
-- The parameter @light@ is a @FragmentLightNameSGIX@.
--
-- The parameter @pname@ is a @FragmentLightParameterSGIX@.
--
-- The parameter @param@ is a @CheckedFloat32@.


glFragmentLightfSGIX :: MonadIO m => GLenum -> GLenum -> GLfloat -> m ()
glFragmentLightfSGIX = ffienumenumfloatIOV glFragmentLightfSGIXFunPtr

glFragmentLightfSGIXFunPtr :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
glFragmentLightfSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentLightfSGIX")

{-# NOINLINE glFragmentLightfSGIXFunPtr #-}

-- | Usage: @'glFragmentLightfvSGIX' light pname params@
--
-- The parameter @light@ is a @FragmentLightNameSGIX@.
--
-- The parameter @pname@ is a @FragmentLightParameterSGIX@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glFragmentLightfvSGIX :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glFragmentLightfvSGIX = ffienumenumPtrfloatIOV glFragmentLightfvSGIXFunPtr

glFragmentLightfvSGIXFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glFragmentLightfvSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentLightfvSGIX")

{-# NOINLINE glFragmentLightfvSGIXFunPtr #-}

-- | Usage: @'glFragmentLightiSGIX' light pname param@
--
-- The parameter @light@ is a @FragmentLightNameSGIX@.
--
-- The parameter @pname@ is a @FragmentLightParameterSGIX@.
--
-- The parameter @param@ is a @CheckedInt32@.


glFragmentLightiSGIX :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glFragmentLightiSGIX = ffienumenumintIOV glFragmentLightiSGIXFunPtr

glFragmentLightiSGIXFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glFragmentLightiSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentLightiSGIX")

{-# NOINLINE glFragmentLightiSGIXFunPtr #-}

-- | Usage: @'glFragmentLightivSGIX' light pname params@
--
-- The parameter @light@ is a @FragmentLightNameSGIX@.
--
-- The parameter @pname@ is a @FragmentLightParameterSGIX@.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glFragmentLightivSGIX :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glFragmentLightivSGIX = ffienumenumPtrintIOV glFragmentLightivSGIXFunPtr

glFragmentLightivSGIXFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glFragmentLightivSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentLightivSGIX")

{-# NOINLINE glFragmentLightivSGIXFunPtr #-}

-- | Usage: @'glFragmentMaterialfSGIX' face pname param@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @pname@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.
--
-- The parameter @param@ is a @CheckedFloat32@.


glFragmentMaterialfSGIX :: MonadIO m => GLenum -> GLenum -> GLfloat -> m ()
glFragmentMaterialfSGIX = ffienumenumfloatIOV glFragmentMaterialfSGIXFunPtr

glFragmentMaterialfSGIXFunPtr :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
glFragmentMaterialfSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentMaterialfSGIX")

{-# NOINLINE glFragmentMaterialfSGIXFunPtr #-}

-- | Usage: @'glFragmentMaterialfvSGIX' face pname params@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @pname@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glFragmentMaterialfvSGIX :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glFragmentMaterialfvSGIX = ffienumenumPtrfloatIOV glFragmentMaterialfvSGIXFunPtr

glFragmentMaterialfvSGIXFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glFragmentMaterialfvSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentMaterialfvSGIX")

{-# NOINLINE glFragmentMaterialfvSGIXFunPtr #-}

-- | Usage: @'glFragmentMaterialiSGIX' face pname param@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @pname@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.
--
-- The parameter @param@ is a @CheckedInt32@.


glFragmentMaterialiSGIX :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
glFragmentMaterialiSGIX = ffienumenumintIOV glFragmentMaterialiSGIXFunPtr

glFragmentMaterialiSGIXFunPtr :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
glFragmentMaterialiSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentMaterialiSGIX")

{-# NOINLINE glFragmentMaterialiSGIXFunPtr #-}

-- | Usage: @'glFragmentMaterialivSGIX' face pname params@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @pname@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glFragmentMaterialivSGIX :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glFragmentMaterialivSGIX = ffienumenumPtrintIOV glFragmentMaterialivSGIXFunPtr

glFragmentMaterialivSGIXFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glFragmentMaterialivSGIXFunPtr = unsafePerformIO (getProcAddress "glFragmentMaterialivSGIX")

{-# NOINLINE glFragmentMaterialivSGIXFunPtr #-}

-- | Usage: @'glGetFragmentLightfvSGIX' light pname params@
--
-- The parameter @light@ is a @FragmentLightNameSGIX@.
--
-- The parameter @pname@ is a @FragmentLightParameterSGIX@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFragmentLightfvSGIX :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetFragmentLightfvSGIX = ffienumenumPtrfloatIOV glGetFragmentLightfvSGIXFunPtr

glGetFragmentLightfvSGIXFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetFragmentLightfvSGIXFunPtr = unsafePerformIO (getProcAddress "glGetFragmentLightfvSGIX")

{-# NOINLINE glGetFragmentLightfvSGIXFunPtr #-}

-- | Usage: @'glGetFragmentLightivSGIX' light pname params@
--
-- The parameter @light@ is a @FragmentLightNameSGIX@.
--
-- The parameter @pname@ is a @FragmentLightParameterSGIX@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFragmentLightivSGIX :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetFragmentLightivSGIX = ffienumenumPtrintIOV glGetFragmentLightivSGIXFunPtr

glGetFragmentLightivSGIXFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetFragmentLightivSGIXFunPtr = unsafePerformIO (getProcAddress "glGetFragmentLightivSGIX")

{-# NOINLINE glGetFragmentLightivSGIXFunPtr #-}

-- | Usage: @'glGetFragmentMaterialfvSGIX' face pname params@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @pname@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFragmentMaterialfvSGIX :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetFragmentMaterialfvSGIX = ffienumenumPtrfloatIOV glGetFragmentMaterialfvSGIXFunPtr

glGetFragmentMaterialfvSGIXFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetFragmentMaterialfvSGIXFunPtr = unsafePerformIO (getProcAddress "glGetFragmentMaterialfvSGIX")

{-# NOINLINE glGetFragmentMaterialfvSGIXFunPtr #-}

-- | Usage: @'glGetFragmentMaterialivSGIX' face pname params@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @pname@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFragmentMaterialivSGIX :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetFragmentMaterialivSGIX = ffienumenumPtrintIOV glGetFragmentMaterialivSGIXFunPtr

glGetFragmentMaterialivSGIXFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetFragmentMaterialivSGIXFunPtr = unsafePerformIO (getProcAddress "glGetFragmentMaterialivSGIX")

{-# NOINLINE glGetFragmentMaterialivSGIXFunPtr #-}

-- | Usage: @'glLightEnviSGIX' pname param@
--
-- The parameter @pname@ is a @LightEnvParameterSGIX@, one of: 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_LIGHT_ENV_MODE_SGIX'.
--
-- The parameter @param@ is a @CheckedInt32@.


glLightEnviSGIX :: MonadIO m => GLenum -> GLint -> m ()
glLightEnviSGIX = ffienumintIOV glLightEnviSGIXFunPtr

glLightEnviSGIXFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glLightEnviSGIXFunPtr = unsafePerformIO (getProcAddress "glLightEnviSGIX")

{-# NOINLINE glLightEnviSGIXFunPtr #-}

pattern GL_CURRENT_RASTER_NORMAL_SGIX  :: (Eq a, Num a) => a

pattern GL_CURRENT_RASTER_NORMAL_SGIX = 0x8406

pattern GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX = 0x8402

pattern GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX = 0x8403

pattern GL_FRAGMENT_COLOR_MATERIAL_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_COLOR_MATERIAL_SGIX = 0x8401

pattern GL_FRAGMENT_LIGHT0_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT0_SGIX = 0x840C

pattern GL_FRAGMENT_LIGHT1_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT1_SGIX = 0x840D

pattern GL_FRAGMENT_LIGHT2_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT2_SGIX = 0x840E

pattern GL_FRAGMENT_LIGHT3_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT3_SGIX = 0x840F

pattern GL_FRAGMENT_LIGHT4_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT4_SGIX = 0x8410

pattern GL_FRAGMENT_LIGHT5_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT5_SGIX = 0x8411

pattern GL_FRAGMENT_LIGHT6_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT6_SGIX = 0x8412

pattern GL_FRAGMENT_LIGHT7_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT7_SGIX = 0x8413

pattern GL_FRAGMENT_LIGHTING_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHTING_SGIX = 0x8400

pattern GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX = 0x840A

pattern GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX = 0x8408

pattern GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX = 0x840B

pattern GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX = 0x8409

pattern GL_LIGHT_ENV_MODE_SGIX  :: (Eq a, Num a) => a

pattern GL_LIGHT_ENV_MODE_SGIX = 0x8407

pattern GL_MAX_ACTIVE_LIGHTS_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_ACTIVE_LIGHTS_SGIX = 0x8405

pattern GL_MAX_FRAGMENT_LIGHTS_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_FRAGMENT_LIGHTS_SGIX = 0x8404