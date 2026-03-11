-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Standard14 (
  -- * Graphics.GL.Standard14
    module Graphics.GL.Standard13
  , glBlendColor
  , glBlendEquation
  , glBlendFuncSeparate
  , glFogCoordPointer
  , glFogCoordd
  , glFogCoorddv
  , glFogCoordf
  , glFogCoordfv
  , glMultiDrawArrays
  , glMultiDrawElements
  , glPointParameterf
  , glPointParameterfv
  , glPointParameteri
  , glPointParameteriv
  , glSecondaryColor3b
  , glSecondaryColor3bv
  , glSecondaryColor3d
  , glSecondaryColor3dv
  , glSecondaryColor3f
  , glSecondaryColor3fv
  , glSecondaryColor3i
  , glSecondaryColor3iv
  , glSecondaryColor3s
  , glSecondaryColor3sv
  , glSecondaryColor3ub
  , glSecondaryColor3ubv
  , glSecondaryColor3ui
  , glSecondaryColor3uiv
  , glSecondaryColor3us
  , glSecondaryColor3usv
  , glSecondaryColorPointer
  , glWindowPos2d
  , glWindowPos2dv
  , glWindowPos2f
  , glWindowPos2fv
  , glWindowPos2i
  , glWindowPos2iv
  , glWindowPos2s
  , glWindowPos2sv
  , glWindowPos3d
  , glWindowPos3dv
  , glWindowPos3f
  , glWindowPos3fv
  , glWindowPos3i
  , glWindowPos3iv
  , glWindowPos3s
  , glWindowPos3sv
  , pattern GL_BLEND_DST_ALPHA
  , pattern GL_BLEND_DST_RGB
  , pattern GL_BLEND_SRC_ALPHA
  , pattern GL_BLEND_SRC_RGB
  , pattern GL_COLOR_SUM
  , pattern GL_COMPARE_R_TO_TEXTURE
  , pattern GL_CONSTANT_ALPHA
  , pattern GL_CONSTANT_COLOR
  , pattern GL_CURRENT_FOG_COORDINATE
  , pattern GL_CURRENT_SECONDARY_COLOR
  , pattern GL_DECR_WRAP
  , pattern GL_DEPTH_COMPONENT16
  , pattern GL_DEPTH_COMPONENT24
  , pattern GL_DEPTH_COMPONENT32
  , pattern GL_DEPTH_TEXTURE_MODE
  , pattern GL_FOG_COORDINATE
  , pattern GL_FOG_COORDINATE_ARRAY
  , pattern GL_FOG_COORDINATE_ARRAY_POINTER
  , pattern GL_FOG_COORDINATE_ARRAY_STRIDE
  , pattern GL_FOG_COORDINATE_ARRAY_TYPE
  , pattern GL_FOG_COORDINATE_SOURCE
  , pattern GL_FRAGMENT_DEPTH
  , pattern GL_FUNC_ADD
  , pattern GL_FUNC_REVERSE_SUBTRACT
  , pattern GL_FUNC_SUBTRACT
  , pattern GL_GENERATE_MIPMAP
  , pattern GL_GENERATE_MIPMAP_HINT
  , pattern GL_INCR_WRAP
  , pattern GL_MAX
  , pattern GL_MAX_TEXTURE_LOD_BIAS
  , pattern GL_MIN
  , pattern GL_MIRRORED_REPEAT
  , pattern GL_ONE_MINUS_CONSTANT_ALPHA
  , pattern GL_ONE_MINUS_CONSTANT_COLOR
  , pattern GL_POINT_DISTANCE_ATTENUATION
  , pattern GL_POINT_FADE_THRESHOLD_SIZE
  , pattern GL_POINT_SIZE_MAX
  , pattern GL_POINT_SIZE_MIN
  , pattern GL_SECONDARY_COLOR_ARRAY
  , pattern GL_SECONDARY_COLOR_ARRAY_POINTER
  , pattern GL_SECONDARY_COLOR_ARRAY_SIZE
  , pattern GL_SECONDARY_COLOR_ARRAY_STRIDE
  , pattern GL_SECONDARY_COLOR_ARRAY_TYPE
  , pattern GL_TEXTURE_COMPARE_FUNC
  , pattern GL_TEXTURE_COMPARE_MODE
  , pattern GL_TEXTURE_DEPTH_SIZE
  , pattern GL_TEXTURE_FILTER_CONTROL
  , pattern GL_TEXTURE_LOD_BIAS
) where

import Graphics.GL.Internal.Shared

import Graphics.GL.Standard13