-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.PolynomialFfd (
  -- * Extension Support
    gl_SGIX_polynomial_ffd

  -- * GL_SGIX_polynomial_ffd
  , glDeformSGIX
  , glDeformationMap3dSGIX
  , glDeformationMap3fSGIX
  , glLoadIdentityDeformationMapSGIX
  , pattern GL_DEFORMATIONS_MASK_SGIX
  , pattern GL_GEOMETRY_DEFORMATION_BIT_SGIX
  , pattern GL_GEOMETRY_DEFORMATION_SGIX
  , pattern GL_MAX_DEFORMATION_ORDER_SGIX
  , pattern GL_TEXTURE_DEFORMATION_BIT_SGIX
  , pattern GL_TEXTURE_DEFORMATION_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_SGIX_polynomial_ffd extension is available.

gl_SGIX_polynomial_ffd :: Bool
gl_SGIX_polynomial_ffd = member "GL_SGIX_polynomial_ffd" extensions
{-# NOINLINE gl_SGIX_polynomial_ffd #-}

-- | Usage: @'glDeformSGIX' mask@
--
-- The parameter @mask@ is a @FfdMaskSGIX@, one of: .


glDeformSGIX :: MonadIO m => GLbitfield -> m ()
glDeformSGIX = ffibitfieldIOV glDeformSGIXFunPtr

glDeformSGIXFunPtr :: FunPtr (GLbitfield -> IO ())
glDeformSGIXFunPtr = unsafePerformIO (getProcAddress "glDeformSGIX")

{-# NOINLINE glDeformSGIXFunPtr #-}

-- | Usage: @'glDeformationMap3dSGIX' target u1 u2 ustride uorder v1 v2 vstride vorder w1 w2 wstride worder points@
--
-- The parameter @target@ is a @FfdTargetSGIX@, one of: 'Graphics.GL.Ext.SGIX.PolynomialFfd.GL_GEOMETRY_DEFORMATION_SGIX', 'Graphics.GL.Ext.SGIX.PolynomialFfd.GL_TEXTURE_DEFORMATION_SGIX'.
--
-- The parameter @u1@ is a @CoordD@.
--
-- The parameter @u2@ is a @CoordD@.
--
-- The parameter @uorder@ is a @CheckedInt32@.
--
-- The parameter @v1@ is a @CoordD@.
--
-- The parameter @v2@ is a @CoordD@.
--
-- The parameter @vorder@ is a @CheckedInt32@.
--
-- The parameter @w1@ is a @CoordD@.
--
-- The parameter @w2@ is a @CoordD@.
--
-- The parameter @worder@ is a @CheckedInt32@.
--
-- The parameter @points@ is a @CoordD@.
--
-- The length of @points@ should be @COMPSIZE(target,ustride,uorder,vstride,vorder,wstride,worder)@.


glDeformationMap3dSGIX :: MonadIO m => GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> m ()
glDeformationMap3dSGIX = ffienumdoubledoubleintintdoubledoubleintintdoubledoubleintintPtrdoubleIOV glDeformationMap3dSGIXFunPtr

glDeformationMap3dSGIXFunPtr :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
glDeformationMap3dSGIXFunPtr = unsafePerformIO (getProcAddress "glDeformationMap3dSGIX")

{-# NOINLINE glDeformationMap3dSGIXFunPtr #-}

-- | Usage: @'glDeformationMap3fSGIX' target u1 u2 ustride uorder v1 v2 vstride vorder w1 w2 wstride worder points@
--
-- The parameter @target@ is a @FfdTargetSGIX@, one of: 'Graphics.GL.Ext.SGIX.PolynomialFfd.GL_GEOMETRY_DEFORMATION_SGIX', 'Graphics.GL.Ext.SGIX.PolynomialFfd.GL_TEXTURE_DEFORMATION_SGIX'.
--
-- The parameter @u1@ is a @CoordF@.
--
-- The parameter @u2@ is a @CoordF@.
--
-- The parameter @uorder@ is a @CheckedInt32@.
--
-- The parameter @v1@ is a @CoordF@.
--
-- The parameter @v2@ is a @CoordF@.
--
-- The parameter @vorder@ is a @CheckedInt32@.
--
-- The parameter @w1@ is a @CoordF@.
--
-- The parameter @w2@ is a @CoordF@.
--
-- The parameter @worder@ is a @CheckedInt32@.
--
-- The parameter @points@ is a @CoordF@.
--
-- The length of @points@ should be @COMPSIZE(target,ustride,uorder,vstride,vorder,wstride,worder)@.


glDeformationMap3fSGIX :: MonadIO m => GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> m ()
glDeformationMap3fSGIX = ffienumfloatfloatintintfloatfloatintintfloatfloatintintPtrfloatIOV glDeformationMap3fSGIXFunPtr

glDeformationMap3fSGIXFunPtr :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
glDeformationMap3fSGIXFunPtr = unsafePerformIO (getProcAddress "glDeformationMap3fSGIX")

{-# NOINLINE glDeformationMap3fSGIXFunPtr #-}

-- | Usage: @'glLoadIdentityDeformationMapSGIX' mask@
--
-- The parameter @mask@ is a @FfdMaskSGIX@, one of: .


glLoadIdentityDeformationMapSGIX :: MonadIO m => GLbitfield -> m ()
glLoadIdentityDeformationMapSGIX = ffibitfieldIOV glLoadIdentityDeformationMapSGIXFunPtr

glLoadIdentityDeformationMapSGIXFunPtr :: FunPtr (GLbitfield -> IO ())
glLoadIdentityDeformationMapSGIXFunPtr = unsafePerformIO (getProcAddress "glLoadIdentityDeformationMapSGIX")

{-# NOINLINE glLoadIdentityDeformationMapSGIXFunPtr #-}

pattern GL_DEFORMATIONS_MASK_SGIX  :: (Eq a, Num a) => a

pattern GL_DEFORMATIONS_MASK_SGIX = 0x8196

pattern GL_GEOMETRY_DEFORMATION_BIT_SGIX  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_DEFORMATION_BIT_SGIX = 0x00000002

pattern GL_GEOMETRY_DEFORMATION_SGIX  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_DEFORMATION_SGIX = 0x8194

pattern GL_MAX_DEFORMATION_ORDER_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_DEFORMATION_ORDER_SGIX = 0x8197

pattern GL_TEXTURE_DEFORMATION_BIT_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_DEFORMATION_BIT_SGIX = 0x00000001

pattern GL_TEXTURE_DEFORMATION_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_DEFORMATION_SGIX = 0x8195