-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Compatibility32 (
  -- * Graphics.GL.Compatibility32
    module Graphics.GL.Core32
  , glAccum
  , glAlphaFunc
  , glAreTexturesResident
  , glArrayElement
  , glBegin
  , glBitmap
  , glCallList
  , glCallLists
  , glClearAccum
  , glClearIndex
  , glClientActiveTexture
  , glClipPlane
  , glColor3b
  , glColor3bv
  , glColor3d
  , glColor3dv
  , glColor3f
  , glColor3fv
  , glColor3i
  , glColor3iv
  , glColor3s
  , glColor3sv
  , glColor3ub
  , glColor3ubv
  , glColor3ui
  , glColor3uiv
  , glColor3us
  , glColor3usv
  , glColor4b
  , glColor4bv
  , glColor4d
  , glColor4dv
  , glColor4f
  , glColor4fv
  , glColor4i
  , glColor4iv
  , glColor4s
  , glColor4sv
  , glColor4ub
  , glColor4ubv
  , glColor4ui
  , glColor4uiv
  , glColor4us
  , glColor4usv
  , glColorMaterial
  , glColorPointer
  , glCopyPixels
  , glDeleteLists
  , glDisableClientState
  , glDrawPixels
  , glEdgeFlag
  , glEdgeFlagPointer
  , glEdgeFlagv
  , glEnableClientState
  , glEnd
  , glEndList
  , glEvalCoord1d
  , glEvalCoord1dv
  , glEvalCoord1f
  , glEvalCoord1fv
  , glEvalCoord2d
  , glEvalCoord2dv
  , glEvalCoord2f
  , glEvalCoord2fv
  , glEvalMesh1
  , glEvalMesh2
  , glEvalPoint1
  , glEvalPoint2
  , glFeedbackBuffer
  , glFogCoordPointer
  , glFogCoordd
  , glFogCoorddv
  , glFogCoordf
  , glFogCoordfv
  , glFogf
  , glFogfv
  , glFogi
  , glFogiv
  , glFrustum
  , glGenLists
  , glGetClipPlane
  , glGetLightfv
  , glGetLightiv
  , glGetMapdv
  , glGetMapfv
  , glGetMapiv
  , glGetMaterialfv
  , glGetMaterialiv
  , glGetPixelMapfv
  , glGetPixelMapuiv
  , glGetPixelMapusv
  , glGetPointerv
  , glGetPolygonStipple
  , glGetTexEnvfv
  , glGetTexEnviv
  , glGetTexGendv
  , glGetTexGenfv
  , glGetTexGeniv
  , glIndexMask
  , glIndexPointer
  , glIndexd
  , glIndexdv
  , glIndexf
  , glIndexfv
  , glIndexi
  , glIndexiv
  , glIndexs
  , glIndexsv
  , glIndexub
  , glIndexubv
  , glInitNames
  , glInterleavedArrays
  , glIsList
  , glLightModelf
  , glLightModelfv
  , glLightModeli
  , glLightModeliv
  , glLightf
  , glLightfv
  , glLighti
  , glLightiv
  , glLineStipple
  , glListBase
  , glLoadIdentity
  , glLoadMatrixd
  , glLoadMatrixf
  , glLoadName
  , glLoadTransposeMatrixd
  , glLoadTransposeMatrixf
  , glMap1d
  , glMap1f
  , glMap2d
  , glMap2f
  , glMapGrid1d
  , glMapGrid1f
  , glMapGrid2d
  , glMapGrid2f
  , glMaterialf
  , glMaterialfv
  , glMateriali
  , glMaterialiv
  , glMatrixMode
  , glMultMatrixd
  , glMultMatrixf
  , glMultTransposeMatrixd
  , glMultTransposeMatrixf
  , glMultiTexCoord1d
  , glMultiTexCoord1dv
  , glMultiTexCoord1f
  , glMultiTexCoord1fv
  , glMultiTexCoord1i
  , glMultiTexCoord1iv
  , glMultiTexCoord1s
  , glMultiTexCoord1sv
  , glMultiTexCoord2d
  , glMultiTexCoord2dv
  , glMultiTexCoord2f
  , glMultiTexCoord2fv
  , glMultiTexCoord2i
  , glMultiTexCoord2iv
  , glMultiTexCoord2s
  , glMultiTexCoord2sv
  , glMultiTexCoord3d
  , glMultiTexCoord3dv
  , glMultiTexCoord3f
  , glMultiTexCoord3fv
  , glMultiTexCoord3i
  , glMultiTexCoord3iv
  , glMultiTexCoord3s
  , glMultiTexCoord3sv
  , glMultiTexCoord4d
  , glMultiTexCoord4dv
  , glMultiTexCoord4f
  , glMultiTexCoord4fv
  , glMultiTexCoord4i
  , glMultiTexCoord4iv
  , glMultiTexCoord4s
  , glMultiTexCoord4sv
  , glNewList
  , glNormal3b
  , glNormal3bv
  , glNormal3d
  , glNormal3dv
  , glNormal3f
  , glNormal3fv
  , glNormal3i
  , glNormal3iv
  , glNormal3s
  , glNormal3sv
  , glNormalPointer
  , glOrtho
  , glPassThrough
  , glPixelMapfv
  , glPixelMapuiv
  , glPixelMapusv
  , glPixelTransferf
  , glPixelTransferi
  , glPixelZoom
  , glPolygonStipple
  , glPopAttrib
  , glPopClientAttrib
  , glPopMatrix
  , glPopName
  , glPrioritizeTextures
  , glPushAttrib
  , glPushClientAttrib
  , glPushMatrix
  , glPushName
  , glRasterPos2d
  , glRasterPos2dv
  , glRasterPos2f
  , glRasterPos2fv
  , glRasterPos2i
  , glRasterPos2iv
  , glRasterPos2s
  , glRasterPos2sv
  , glRasterPos3d
  , glRasterPos3dv
  , glRasterPos3f
  , glRasterPos3fv
  , glRasterPos3i
  , glRasterPos3iv
  , glRasterPos3s
  , glRasterPos3sv
  , glRasterPos4d
  , glRasterPos4dv
  , glRasterPos4f
  , glRasterPos4fv
  , glRasterPos4i
  , glRasterPos4iv
  , glRasterPos4s
  , glRasterPos4sv
  , glRectd
  , glRectdv
  , glRectf
  , glRectfv
  , glRecti
  , glRectiv
  , glRects
  , glRectsv
  , glRenderMode
  , glRotated
  , glRotatef
  , glScaled
  , glScalef
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
  , glSelectBuffer
  , glShadeModel
  , glTexCoord1d
  , glTexCoord1dv
  , glTexCoord1f
  , glTexCoord1fv
  , glTexCoord1i
  , glTexCoord1iv
  , glTexCoord1s
  , glTexCoord1sv
  , glTexCoord2d
  , glTexCoord2dv
  , glTexCoord2f
  , glTexCoord2fv
  , glTexCoord2i
  , glTexCoord2iv
  , glTexCoord2s
  , glTexCoord2sv
  , glTexCoord3d
  , glTexCoord3dv
  , glTexCoord3f
  , glTexCoord3fv
  , glTexCoord3i
  , glTexCoord3iv
  , glTexCoord3s
  , glTexCoord3sv
  , glTexCoord4d
  , glTexCoord4dv
  , glTexCoord4f
  , glTexCoord4fv
  , glTexCoord4i
  , glTexCoord4iv
  , glTexCoord4s
  , glTexCoord4sv
  , glTexCoordPointer
  , glTexEnvf
  , glTexEnvfv
  , glTexEnvi
  , glTexEnviv
  , glTexGend
  , glTexGendv
  , glTexGenf
  , glTexGenfv
  , glTexGeni
  , glTexGeniv
  , glTranslated
  , glTranslatef
  , glVertex2d
  , glVertex2dv
  , glVertex2f
  , glVertex2fv
  , glVertex2i
  , glVertex2iv
  , glVertex2s
  , glVertex2sv
  , glVertex3d
  , glVertex3dv
  , glVertex3f
  , glVertex3fv
  , glVertex3i
  , glVertex3iv
  , glVertex3s
  , glVertex3sv
  , glVertex4d
  , glVertex4dv
  , glVertex4f
  , glVertex4fv
  , glVertex4i
  , glVertex4iv
  , glVertex4s
  , glVertex4sv
  , glVertexPointer
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
  , pattern GL_2D
  , pattern GL_2_BYTES
  , pattern GL_3D
  , pattern GL_3D_COLOR
  , pattern GL_3D_COLOR_TEXTURE
  , pattern GL_3_BYTES
  , pattern GL_4D_COLOR_TEXTURE
  , pattern GL_4_BYTES
  , pattern GL_ACCUM
  , pattern GL_ACCUM_ALPHA_BITS
  , pattern GL_ACCUM_BLUE_BITS
  , pattern GL_ACCUM_BUFFER_BIT
  , pattern GL_ACCUM_CLEAR_VALUE
  , pattern GL_ACCUM_GREEN_BITS
  , pattern GL_ACCUM_RED_BITS
  , pattern GL_ADD
  , pattern GL_ADD_SIGNED
  , pattern GL_ALIASED_POINT_SIZE_RANGE
  , pattern GL_ALL_ATTRIB_BITS
  , pattern GL_ALPHA12
  , pattern GL_ALPHA16
  , pattern GL_ALPHA4
  , pattern GL_ALPHA8
  , pattern GL_ALPHA_BIAS
  , pattern GL_ALPHA_BITS
  , pattern GL_ALPHA_INTEGER
  , pattern GL_ALPHA_SCALE
  , pattern GL_ALPHA_TEST
  , pattern GL_ALPHA_TEST_FUNC
  , pattern GL_ALPHA_TEST_REF
  , pattern GL_AMBIENT
  , pattern GL_AMBIENT_AND_DIFFUSE
  , pattern GL_ATTRIB_STACK_DEPTH
  , pattern GL_AUTO_NORMAL
  , pattern GL_AUX0
  , pattern GL_AUX1
  , pattern GL_AUX2
  , pattern GL_AUX3
  , pattern GL_AUX_BUFFERS
  , pattern GL_BITMAP
  , pattern GL_BITMAP_TOKEN
  , pattern GL_BLUE_BIAS
  , pattern GL_BLUE_BITS
  , pattern GL_BLUE_SCALE
  , pattern GL_C3F_V3F
  , pattern GL_C4F_N3F_V3F
  , pattern GL_C4UB_V2F
  , pattern GL_C4UB_V3F
  , pattern GL_CLAMP
  , pattern GL_CLAMP_FRAGMENT_COLOR
  , pattern GL_CLAMP_VERTEX_COLOR
  , pattern GL_CLIENT_ACTIVE_TEXTURE
  , pattern GL_CLIENT_ALL_ATTRIB_BITS
  , pattern GL_CLIENT_ATTRIB_STACK_DEPTH
  , pattern GL_CLIENT_PIXEL_STORE_BIT
  , pattern GL_CLIENT_VERTEX_ARRAY_BIT
  , pattern GL_CLIP_PLANE0
  , pattern GL_CLIP_PLANE1
  , pattern GL_CLIP_PLANE2
  , pattern GL_CLIP_PLANE3
  , pattern GL_CLIP_PLANE4
  , pattern GL_CLIP_PLANE5
  , pattern GL_COEFF
  , pattern GL_COLOR_ARRAY
  , pattern GL_COLOR_ARRAY_BUFFER_BINDING
  , pattern GL_COLOR_ARRAY_POINTER
  , pattern GL_COLOR_ARRAY_SIZE
  , pattern GL_COLOR_ARRAY_STRIDE
  , pattern GL_COLOR_ARRAY_TYPE
  , pattern GL_COLOR_INDEX
  , pattern GL_COLOR_INDEXES
  , pattern GL_COLOR_MATERIAL
  , pattern GL_COLOR_MATERIAL_FACE
  , pattern GL_COLOR_MATERIAL_PARAMETER
  , pattern GL_COLOR_SUM
  , pattern GL_COMBINE
  , pattern GL_COMBINE_ALPHA
  , pattern GL_COMBINE_RGB
  , pattern GL_COMPARE_R_TO_TEXTURE
  , pattern GL_COMPILE
  , pattern GL_COMPILE_AND_EXECUTE
  , pattern GL_COMPRESSED_ALPHA
  , pattern GL_COMPRESSED_INTENSITY
  , pattern GL_COMPRESSED_LUMINANCE
  , pattern GL_COMPRESSED_LUMINANCE_ALPHA
  , pattern GL_COMPRESSED_SLUMINANCE
  , pattern GL_COMPRESSED_SLUMINANCE_ALPHA
  , pattern GL_CONSTANT
  , pattern GL_CONSTANT_ATTENUATION
  , pattern GL_COORD_REPLACE
  , pattern GL_COPY_PIXEL_TOKEN
  , pattern GL_CURRENT_BIT
  , pattern GL_CURRENT_COLOR
  , pattern GL_CURRENT_FOG_COORD
  , pattern GL_CURRENT_FOG_COORDINATE
  , pattern GL_CURRENT_INDEX
  , pattern GL_CURRENT_NORMAL
  , pattern GL_CURRENT_RASTER_COLOR
  , pattern GL_CURRENT_RASTER_DISTANCE
  , pattern GL_CURRENT_RASTER_INDEX
  , pattern GL_CURRENT_RASTER_POSITION
  , pattern GL_CURRENT_RASTER_POSITION_VALID
  , pattern GL_CURRENT_RASTER_SECONDARY_COLOR
  , pattern GL_CURRENT_RASTER_TEXTURE_COORDS
  , pattern GL_CURRENT_SECONDARY_COLOR
  , pattern GL_CURRENT_TEXTURE_COORDS
  , pattern GL_DECAL
  , pattern GL_DEPTH_BIAS
  , pattern GL_DEPTH_BITS
  , pattern GL_DEPTH_SCALE
  , pattern GL_DEPTH_TEXTURE_MODE
  , pattern GL_DIFFUSE
  , pattern GL_DOMAIN
  , pattern GL_DOT3_RGB
  , pattern GL_DOT3_RGBA
  , pattern GL_DRAW_PIXEL_TOKEN
  , pattern GL_EDGE_FLAG
  , pattern GL_EDGE_FLAG_ARRAY
  , pattern GL_EDGE_FLAG_ARRAY_BUFFER_BINDING
  , pattern GL_EDGE_FLAG_ARRAY_POINTER
  , pattern GL_EDGE_FLAG_ARRAY_STRIDE
  , pattern GL_EMISSION
  , pattern GL_ENABLE_BIT
  , pattern GL_EVAL_BIT
  , pattern GL_EXP
  , pattern GL_EXP2
  , pattern GL_EYE_LINEAR
  , pattern GL_EYE_PLANE
  , pattern GL_FEEDBACK
  , pattern GL_FEEDBACK_BUFFER_POINTER
  , pattern GL_FEEDBACK_BUFFER_SIZE
  , pattern GL_FEEDBACK_BUFFER_TYPE
  , pattern GL_FLAT
  , pattern GL_FOG
  , pattern GL_FOG_BIT
  , pattern GL_FOG_COLOR
  , pattern GL_FOG_COORD
  , pattern GL_FOG_COORDINATE
  , pattern GL_FOG_COORDINATE_ARRAY
  , pattern GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING
  , pattern GL_FOG_COORDINATE_ARRAY_POINTER
  , pattern GL_FOG_COORDINATE_ARRAY_STRIDE
  , pattern GL_FOG_COORDINATE_ARRAY_TYPE
  , pattern GL_FOG_COORDINATE_SOURCE
  , pattern GL_FOG_COORD_ARRAY
  , pattern GL_FOG_COORD_ARRAY_BUFFER_BINDING
  , pattern GL_FOG_COORD_ARRAY_POINTER
  , pattern GL_FOG_COORD_ARRAY_STRIDE
  , pattern GL_FOG_COORD_ARRAY_TYPE
  , pattern GL_FOG_COORD_SRC
  , pattern GL_FOG_DENSITY
  , pattern GL_FOG_END
  , pattern GL_FOG_HINT
  , pattern GL_FOG_INDEX
  , pattern GL_FOG_MODE
  , pattern GL_FOG_START
  , pattern GL_FRAGMENT_DEPTH
  , pattern GL_GENERATE_MIPMAP
  , pattern GL_GENERATE_MIPMAP_HINT
  , pattern GL_GREEN_BIAS
  , pattern GL_GREEN_BITS
  , pattern GL_GREEN_SCALE
  , pattern GL_HINT_BIT
  , pattern GL_INDEX_ARRAY
  , pattern GL_INDEX_ARRAY_BUFFER_BINDING
  , pattern GL_INDEX_ARRAY_POINTER
  , pattern GL_INDEX_ARRAY_STRIDE
  , pattern GL_INDEX_ARRAY_TYPE
  , pattern GL_INDEX_BITS
  , pattern GL_INDEX_CLEAR_VALUE
  , pattern GL_INDEX_LOGIC_OP
  , pattern GL_INDEX_MODE
  , pattern GL_INDEX_OFFSET
  , pattern GL_INDEX_SHIFT
  , pattern GL_INDEX_WRITEMASK
  , pattern GL_INTENSITY
  , pattern GL_INTENSITY12
  , pattern GL_INTENSITY16
  , pattern GL_INTENSITY4
  , pattern GL_INTENSITY8
  , pattern GL_INTERPOLATE
  , pattern GL_LIGHT0
  , pattern GL_LIGHT1
  , pattern GL_LIGHT2
  , pattern GL_LIGHT3
  , pattern GL_LIGHT4
  , pattern GL_LIGHT5
  , pattern GL_LIGHT6
  , pattern GL_LIGHT7
  , pattern GL_LIGHTING
  , pattern GL_LIGHTING_BIT
  , pattern GL_LIGHT_MODEL_AMBIENT
  , pattern GL_LIGHT_MODEL_COLOR_CONTROL
  , pattern GL_LIGHT_MODEL_LOCAL_VIEWER
  , pattern GL_LIGHT_MODEL_TWO_SIDE
  , pattern GL_LINEAR_ATTENUATION
  , pattern GL_LINE_BIT
  , pattern GL_LINE_RESET_TOKEN
  , pattern GL_LINE_STIPPLE
  , pattern GL_LINE_STIPPLE_PATTERN
  , pattern GL_LINE_STIPPLE_REPEAT
  , pattern GL_LINE_TOKEN
  , pattern GL_LIST_BASE
  , pattern GL_LIST_BIT
  , pattern GL_LIST_INDEX
  , pattern GL_LIST_MODE
  , pattern GL_LOAD
  , pattern GL_LOGIC_OP
  , pattern GL_LUMINANCE
  , pattern GL_LUMINANCE12
  , pattern GL_LUMINANCE12_ALPHA12
  , pattern GL_LUMINANCE12_ALPHA4
  , pattern GL_LUMINANCE16
  , pattern GL_LUMINANCE16_ALPHA16
  , pattern GL_LUMINANCE4
  , pattern GL_LUMINANCE4_ALPHA4
  , pattern GL_LUMINANCE6_ALPHA2
  , pattern GL_LUMINANCE8
  , pattern GL_LUMINANCE8_ALPHA8
  , pattern GL_LUMINANCE_ALPHA
  , pattern GL_MAP1_COLOR_4
  , pattern GL_MAP1_GRID_DOMAIN
  , pattern GL_MAP1_GRID_SEGMENTS
  , pattern GL_MAP1_INDEX
  , pattern GL_MAP1_NORMAL
  , pattern GL_MAP1_TEXTURE_COORD_1
  , pattern GL_MAP1_TEXTURE_COORD_2
  , pattern GL_MAP1_TEXTURE_COORD_3
  , pattern GL_MAP1_TEXTURE_COORD_4
  , pattern GL_MAP1_VERTEX_3
  , pattern GL_MAP1_VERTEX_4
  , pattern GL_MAP2_COLOR_4
  , pattern GL_MAP2_GRID_DOMAIN
  , pattern GL_MAP2_GRID_SEGMENTS
  , pattern GL_MAP2_INDEX
  , pattern GL_MAP2_NORMAL
  , pattern GL_MAP2_TEXTURE_COORD_1
  , pattern GL_MAP2_TEXTURE_COORD_2
  , pattern GL_MAP2_TEXTURE_COORD_3
  , pattern GL_MAP2_TEXTURE_COORD_4
  , pattern GL_MAP2_VERTEX_3
  , pattern GL_MAP2_VERTEX_4
  , pattern GL_MAP_COLOR
  , pattern GL_MAP_STENCIL
  , pattern GL_MATRIX_MODE
  , pattern GL_MAX_ATTRIB_STACK_DEPTH
  , pattern GL_MAX_CLIENT_ATTRIB_STACK_DEPTH
  , pattern GL_MAX_CLIP_PLANES
  , pattern GL_MAX_EVAL_ORDER
  , pattern GL_MAX_LIGHTS
  , pattern GL_MAX_LIST_NESTING
  , pattern GL_MAX_MODELVIEW_STACK_DEPTH
  , pattern GL_MAX_NAME_STACK_DEPTH
  , pattern GL_MAX_PIXEL_MAP_TABLE
  , pattern GL_MAX_PROJECTION_STACK_DEPTH
  , pattern GL_MAX_TEXTURE_COORDS
  , pattern GL_MAX_TEXTURE_STACK_DEPTH
  , pattern GL_MAX_TEXTURE_UNITS
  , pattern GL_MODELVIEW
  , pattern GL_MODELVIEW_MATRIX
  , pattern GL_MODELVIEW_STACK_DEPTH
  , pattern GL_MODULATE
  , pattern GL_MULT
  , pattern GL_MULTISAMPLE_BIT
  , pattern GL_N3F_V3F
  , pattern GL_NAME_STACK_DEPTH
  , pattern GL_NORMALIZE
  , pattern GL_NORMAL_ARRAY
  , pattern GL_NORMAL_ARRAY_BUFFER_BINDING
  , pattern GL_NORMAL_ARRAY_POINTER
  , pattern GL_NORMAL_ARRAY_STRIDE
  , pattern GL_NORMAL_ARRAY_TYPE
  , pattern GL_NORMAL_MAP
  , pattern GL_OBJECT_LINEAR
  , pattern GL_OBJECT_PLANE
  , pattern GL_OPERAND0_ALPHA
  , pattern GL_OPERAND0_RGB
  , pattern GL_OPERAND1_ALPHA
  , pattern GL_OPERAND1_RGB
  , pattern GL_OPERAND2_ALPHA
  , pattern GL_OPERAND2_RGB
  , pattern GL_ORDER
  , pattern GL_PASS_THROUGH_TOKEN
  , pattern GL_PERSPECTIVE_CORRECTION_HINT
  , pattern GL_PIXEL_MAP_A_TO_A
  , pattern GL_PIXEL_MAP_A_TO_A_SIZE
  , pattern GL_PIXEL_MAP_B_TO_B
  , pattern GL_PIXEL_MAP_B_TO_B_SIZE
  , pattern GL_PIXEL_MAP_G_TO_G
  , pattern GL_PIXEL_MAP_G_TO_G_SIZE
  , pattern GL_PIXEL_MAP_I_TO_A
  , pattern GL_PIXEL_MAP_I_TO_A_SIZE
  , pattern GL_PIXEL_MAP_I_TO_B
  , pattern GL_PIXEL_MAP_I_TO_B_SIZE
  , pattern GL_PIXEL_MAP_I_TO_G
  , pattern GL_PIXEL_MAP_I_TO_G_SIZE
  , pattern GL_PIXEL_MAP_I_TO_I
  , pattern GL_PIXEL_MAP_I_TO_I_SIZE
  , pattern GL_PIXEL_MAP_I_TO_R
  , pattern GL_PIXEL_MAP_I_TO_R_SIZE
  , pattern GL_PIXEL_MAP_R_TO_R
  , pattern GL_PIXEL_MAP_R_TO_R_SIZE
  , pattern GL_PIXEL_MAP_S_TO_S
  , pattern GL_PIXEL_MAP_S_TO_S_SIZE
  , pattern GL_PIXEL_MODE_BIT
  , pattern GL_POINT_BIT
  , pattern GL_POINT_DISTANCE_ATTENUATION
  , pattern GL_POINT_SIZE_MAX
  , pattern GL_POINT_SIZE_MIN
  , pattern GL_POINT_SMOOTH
  , pattern GL_POINT_SMOOTH_HINT
  , pattern GL_POINT_SPRITE
  , pattern GL_POINT_TOKEN
  , pattern GL_POLYGON
  , pattern GL_POLYGON_BIT
  , pattern GL_POLYGON_STIPPLE
  , pattern GL_POLYGON_STIPPLE_BIT
  , pattern GL_POLYGON_TOKEN
  , pattern GL_POSITION
  , pattern GL_PREVIOUS
  , pattern GL_PRIMARY_COLOR
  , pattern GL_PROJECTION
  , pattern GL_PROJECTION_MATRIX
  , pattern GL_PROJECTION_STACK_DEPTH
  , pattern GL_Q
  , pattern GL_QUADRATIC_ATTENUATION
  , pattern GL_QUADS
  , pattern GL_QUAD_STRIP
  , pattern GL_R
  , pattern GL_RED_BIAS
  , pattern GL_RED_BITS
  , pattern GL_RED_SCALE
  , pattern GL_REFLECTION_MAP
  , pattern GL_RENDER
  , pattern GL_RENDER_MODE
  , pattern GL_RESCALE_NORMAL
  , pattern GL_RETURN
  , pattern GL_RGBA_MODE
  , pattern GL_RGB_SCALE
  , pattern GL_S
  , pattern GL_SCISSOR_BIT
  , pattern GL_SECONDARY_COLOR_ARRAY
  , pattern GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING
  , pattern GL_SECONDARY_COLOR_ARRAY_POINTER
  , pattern GL_SECONDARY_COLOR_ARRAY_SIZE
  , pattern GL_SECONDARY_COLOR_ARRAY_STRIDE
  , pattern GL_SECONDARY_COLOR_ARRAY_TYPE
  , pattern GL_SELECT
  , pattern GL_SELECTION_BUFFER_POINTER
  , pattern GL_SELECTION_BUFFER_SIZE
  , pattern GL_SEPARATE_SPECULAR_COLOR
  , pattern GL_SHADE_MODEL
  , pattern GL_SHININESS
  , pattern GL_SINGLE_COLOR
  , pattern GL_SLUMINANCE
  , pattern GL_SLUMINANCE8
  , pattern GL_SLUMINANCE8_ALPHA8
  , pattern GL_SLUMINANCE_ALPHA
  , pattern GL_SMOOTH
  , pattern GL_SOURCE0_ALPHA
  , pattern GL_SOURCE0_RGB
  , pattern GL_SOURCE1_ALPHA
  , pattern GL_SOURCE1_RGB
  , pattern GL_SOURCE2_ALPHA
  , pattern GL_SOURCE2_RGB
  , pattern GL_SPECULAR
  , pattern GL_SPHERE_MAP
  , pattern GL_SPOT_CUTOFF
  , pattern GL_SPOT_DIRECTION
  , pattern GL_SPOT_EXPONENT
  , pattern GL_SRC0_ALPHA
  , pattern GL_SRC0_RGB
  , pattern GL_SRC1_RGB
  , pattern GL_SRC2_ALPHA
  , pattern GL_SRC2_RGB
  , pattern GL_STACK_OVERFLOW
  , pattern GL_STACK_UNDERFLOW
  , pattern GL_STENCIL_BITS
  , pattern GL_SUBTRACT
  , pattern GL_T
  , pattern GL_T2F_C3F_V3F
  , pattern GL_T2F_C4F_N3F_V3F
  , pattern GL_T2F_C4UB_V3F
  , pattern GL_T2F_N3F_V3F
  , pattern GL_T2F_V3F
  , pattern GL_T4F_C4F_N3F_V4F
  , pattern GL_T4F_V4F
  , pattern GL_TEXTURE_BIT
  , pattern GL_TEXTURE_BORDER
  , pattern GL_TEXTURE_COMPONENTS
  , pattern GL_TEXTURE_COORD_ARRAY
  , pattern GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING
  , pattern GL_TEXTURE_COORD_ARRAY_POINTER
  , pattern GL_TEXTURE_COORD_ARRAY_SIZE
  , pattern GL_TEXTURE_COORD_ARRAY_STRIDE
  , pattern GL_TEXTURE_COORD_ARRAY_TYPE
  , pattern GL_TEXTURE_ENV
  , pattern GL_TEXTURE_ENV_COLOR
  , pattern GL_TEXTURE_ENV_MODE
  , pattern GL_TEXTURE_FILTER_CONTROL
  , pattern GL_TEXTURE_GEN_MODE
  , pattern GL_TEXTURE_GEN_Q
  , pattern GL_TEXTURE_GEN_R
  , pattern GL_TEXTURE_GEN_S
  , pattern GL_TEXTURE_GEN_T
  , pattern GL_TEXTURE_INTENSITY_SIZE
  , pattern GL_TEXTURE_INTENSITY_TYPE
  , pattern GL_TEXTURE_LUMINANCE_SIZE
  , pattern GL_TEXTURE_LUMINANCE_TYPE
  , pattern GL_TEXTURE_MATRIX
  , pattern GL_TEXTURE_PRIORITY
  , pattern GL_TEXTURE_RESIDENT
  , pattern GL_TEXTURE_STACK_DEPTH
  , pattern GL_TRANSFORM_BIT
  , pattern GL_TRANSPOSE_COLOR_MATRIX
  , pattern GL_TRANSPOSE_MODELVIEW_MATRIX
  , pattern GL_TRANSPOSE_PROJECTION_MATRIX
  , pattern GL_TRANSPOSE_TEXTURE_MATRIX
  , pattern GL_V2F
  , pattern GL_V3F
  , pattern GL_VERTEX_ARRAY
  , pattern GL_VERTEX_ARRAY_BUFFER_BINDING
  , pattern GL_VERTEX_ARRAY_POINTER
  , pattern GL_VERTEX_ARRAY_SIZE
  , pattern GL_VERTEX_ARRAY_STRIDE
  , pattern GL_VERTEX_ARRAY_TYPE
  , pattern GL_VERTEX_PROGRAM_TWO_SIDE
  , pattern GL_VIEWPORT_BIT
  , pattern GL_WEIGHT_ARRAY_BUFFER_BINDING
  , pattern GL_ZOOM_X
  , pattern GL_ZOOM_Y
) where

import Graphics.GL.Internal.Shared

import Graphics.GL.Core32