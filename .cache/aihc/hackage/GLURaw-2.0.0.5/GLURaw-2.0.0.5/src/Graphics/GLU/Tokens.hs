{-# LANGUAGE CPP, PatternSynonyms, ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GLU.Tokens
-- Copyright   :  (c) Sven Panne 2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- All tokens from GLU 1.3, see <http://www.opengl.org/>.
--
--------------------------------------------------------------------------------

module Graphics.GLU.Tokens where

import Graphics.GL.Core31

--------------------------------------------------------------------------------

-- Boolean
pattern GLU_FALSE = 0 :: GLenum
pattern GLU_TRUE = 1 :: GLenum

-- StringName
pattern GLU_VERSION = 100800 :: GLenum
pattern GLU_EXTENSIONS = 100801 :: GLenum

-- ErrorCode
pattern GLU_INVALID_ENUM = 100900 :: GLenum
pattern GLU_INVALID_VALUE = 100901 :: GLenum
pattern GLU_OUT_OF_MEMORY = 100902 :: GLenum
pattern GLU_INCOMPATIBLE_GL_VERSION = 100903 :: GLenum
pattern GLU_INVALID_OPERATION = 100904 :: GLenum

-- NurbsDisplay
--      GLU_FILL
pattern GLU_OUTLINE_POLYGON = 100240 :: GLenum
pattern GLU_OUTLINE_PATCH = 100241 :: GLenum

-- NurbsCallback
pattern GLU_NURBS_ERROR = 100103 :: GLenum
pattern GLU_ERROR = 100103 :: GLenum
pattern GLU_NURBS_BEGIN = 100164 :: GLenum
pattern GLU_NURBS_BEGIN_EXT = 100164 :: GLenum
pattern GLU_NURBS_VERTEX = 100165 :: GLenum
pattern GLU_NURBS_VERTEX_EXT = 100165 :: GLenum
pattern GLU_NURBS_NORMAL = 100166 :: GLenum
pattern GLU_NURBS_NORMAL_EXT = 100166 :: GLenum
pattern GLU_NURBS_COLOR = 100167 :: GLenum
pattern GLU_NURBS_COLOR_EXT = 100167 :: GLenum
pattern GLU_NURBS_TEXTURE_COORD = 100168 :: GLenum
pattern GLU_NURBS_TEX_COORD_EXT = 100168 :: GLenum
pattern GLU_NURBS_END = 100169 :: GLenum
pattern GLU_NURBS_END_EXT = 100169 :: GLenum
pattern GLU_NURBS_BEGIN_DATA = 100170 :: GLenum
pattern GLU_NURBS_BEGIN_DATA_EXT = 100170 :: GLenum
pattern GLU_NURBS_VERTEX_DATA = 100171 :: GLenum
pattern GLU_NURBS_VERTEX_DATA_EXT = 100171 :: GLenum
pattern GLU_NURBS_NORMAL_DATA = 100172 :: GLenum
pattern GLU_NURBS_NORMAL_DATA_EXT = 100172 :: GLenum
pattern GLU_NURBS_COLOR_DATA = 100173 :: GLenum
pattern GLU_NURBS_COLOR_DATA_EXT = 100173 :: GLenum
pattern GLU_NURBS_TEXTURE_COORD_DATA = 100174 :: GLenum
pattern GLU_NURBS_TEX_COORD_DATA_EXT = 100174 :: GLenum
pattern GLU_NURBS_END_DATA = 100175 :: GLenum
pattern GLU_NURBS_END_DATA_EXT = 100175 :: GLenum

-- NurbsError
pattern GLU_NURBS_ERROR1 = 100251 :: GLenum
pattern GLU_NURBS_ERROR2 = 100252 :: GLenum
pattern GLU_NURBS_ERROR3 = 100253 :: GLenum
pattern GLU_NURBS_ERROR4 = 100254 :: GLenum
pattern GLU_NURBS_ERROR5 = 100255 :: GLenum
pattern GLU_NURBS_ERROR6 = 100256 :: GLenum
pattern GLU_NURBS_ERROR7 = 100257 :: GLenum
pattern GLU_NURBS_ERROR8 = 100258 :: GLenum
pattern GLU_NURBS_ERROR9 = 100259 :: GLenum
pattern GLU_NURBS_ERROR10 = 100260 :: GLenum
pattern GLU_NURBS_ERROR11 = 100261 :: GLenum
pattern GLU_NURBS_ERROR12 = 100262 :: GLenum
pattern GLU_NURBS_ERROR13 = 100263 :: GLenum
pattern GLU_NURBS_ERROR14 = 100264 :: GLenum
pattern GLU_NURBS_ERROR15 = 100265 :: GLenum
pattern GLU_NURBS_ERROR16 = 100266 :: GLenum
pattern GLU_NURBS_ERROR17 = 100267 :: GLenum
pattern GLU_NURBS_ERROR18 = 100268 :: GLenum
pattern GLU_NURBS_ERROR19 = 100269 :: GLenum
pattern GLU_NURBS_ERROR20 = 100270 :: GLenum
pattern GLU_NURBS_ERROR21 = 100271 :: GLenum
pattern GLU_NURBS_ERROR22 = 100272 :: GLenum
pattern GLU_NURBS_ERROR23 = 100273 :: GLenum
pattern GLU_NURBS_ERROR24 = 100274 :: GLenum
pattern GLU_NURBS_ERROR25 = 100275 :: GLenum
pattern GLU_NURBS_ERROR26 = 100276 :: GLenum
pattern GLU_NURBS_ERROR27 = 100277 :: GLenum
pattern GLU_NURBS_ERROR28 = 100278 :: GLenum
pattern GLU_NURBS_ERROR29 = 100279 :: GLenum
pattern GLU_NURBS_ERROR30 = 100280 :: GLenum
pattern GLU_NURBS_ERROR31 = 100281 :: GLenum
pattern GLU_NURBS_ERROR32 = 100282 :: GLenum
pattern GLU_NURBS_ERROR33 = 100283 :: GLenum
pattern GLU_NURBS_ERROR34 = 100284 :: GLenum
pattern GLU_NURBS_ERROR35 = 100285 :: GLenum
pattern GLU_NURBS_ERROR36 = 100286 :: GLenum
pattern GLU_NURBS_ERROR37 = 100287 :: GLenum

-- NurbsProperty
pattern GLU_AUTO_LOAD_MATRIX = 100200 :: GLenum
pattern GLU_CULLING = 100201 :: GLenum
pattern GLU_SAMPLING_TOLERANCE = 100203 :: GLenum
pattern GLU_DISPLAY_MODE = 100204 :: GLenum
pattern GLU_PARAMETRIC_TOLERANCE = 100202 :: GLenum
pattern GLU_SAMPLING_METHOD = 100205 :: GLenum
pattern GLU_U_STEP = 100206 :: GLenum
pattern GLU_V_STEP = 100207 :: GLenum
pattern GLU_NURBS_MODE = 100160 :: GLenum
pattern GLU_NURBS_MODE_EXT = 100160 :: GLenum
pattern GLU_NURBS_TESSELLATOR = 100161 :: GLenum
pattern GLU_NURBS_TESSELLATOR_EXT = 100161 :: GLenum
pattern GLU_NURBS_RENDERER = 100162 :: GLenum
pattern GLU_NURBS_RENDERER_EXT = 100162 :: GLenum

-- NurbsSampling
pattern GLU_OBJECT_PARAMETRIC_ERROR = 100208 :: GLenum
pattern GLU_OBJECT_PARAMETRIC_ERROR_EXT = 100208 :: GLenum
pattern GLU_OBJECT_PATH_LENGTH = 100209 :: GLenum
pattern GLU_OBJECT_PATH_LENGTH_EXT = 100209 :: GLenum
pattern GLU_PATH_LENGTH = 100215 :: GLenum
pattern GLU_PARAMETRIC_ERROR = 100216 :: GLenum
pattern GLU_DOMAIN_DISTANCE = 100217 :: GLenum

-- NurbsTrim
pattern GLU_MAP1_TRIM_2 = 100210 :: GLenum
pattern GLU_MAP1_TRIM_3 = 100211 :: GLenum

-- QuadricDrawStyle
pattern GLU_POINT = 100010 :: GLenum
pattern GLU_LINE = 100011 :: GLenum
pattern GLU_FILL = 100012 :: GLenum
pattern GLU_SILHOUETTE = 100013 :: GLenum

-- QuadricCallback
--      GLU_ERROR

-- QuadricNormal
pattern GLU_SMOOTH = 100000 :: GLenum
pattern GLU_FLAT = 100001 :: GLenum
pattern GLU_NONE = 100002 :: GLenum

-- QuadricOrientation
pattern GLU_OUTSIDE = 100020 :: GLenum
pattern GLU_INSIDE = 100021 :: GLenum

-- TessCallback
pattern GLU_TESS_BEGIN = 100100 :: GLenum
pattern GLU_BEGIN = 100100 :: GLenum
pattern GLU_TESS_VERTEX = 100101 :: GLenum
pattern GLU_VERTEX = 100101 :: GLenum
pattern GLU_TESS_END = 100102 :: GLenum
pattern GLU_END = 100102 :: GLenum
pattern GLU_TESS_ERROR = 100103 :: GLenum
pattern GLU_TESS_EDGE_FLAG = 100104 :: GLenum
pattern GLU_EDGE_FLAG = 100104 :: GLenum
pattern GLU_TESS_COMBINE = 100105 :: GLenum
pattern GLU_TESS_BEGIN_DATA = 100106 :: GLenum
pattern GLU_TESS_VERTEX_DATA = 100107 :: GLenum
pattern GLU_TESS_END_DATA = 100108 :: GLenum
pattern GLU_TESS_ERROR_DATA = 100109 :: GLenum
pattern GLU_TESS_EDGE_FLAG_DATA = 100110 :: GLenum
pattern GLU_TESS_COMBINE_DATA = 100111 :: GLenum

-- TessContour
pattern GLU_CW = 100120 :: GLenum
pattern GLU_CCW = 100121 :: GLenum
pattern GLU_INTERIOR = 100122 :: GLenum
pattern GLU_EXTERIOR = 100123 :: GLenum
pattern GLU_UNKNOWN = 100124 :: GLenum

-- TessProperty
pattern GLU_TESS_WINDING_RULE = 100140 :: GLenum
pattern GLU_TESS_BOUNDARY_ONLY = 100141 :: GLenum
pattern GLU_TESS_TOLERANCE = 100142 :: GLenum

-- TessError
pattern GLU_TESS_ERROR1 = 100151 :: GLenum
pattern GLU_TESS_ERROR2 = 100152 :: GLenum
pattern GLU_TESS_ERROR3 = 100153 :: GLenum
pattern GLU_TESS_ERROR4 = 100154 :: GLenum
pattern GLU_TESS_ERROR5 = 100155 :: GLenum
pattern GLU_TESS_ERROR6 = 100156 :: GLenum
pattern GLU_TESS_ERROR7 = 100157 :: GLenum
pattern GLU_TESS_ERROR8 = 100158 :: GLenum
pattern GLU_TESS_MISSING_BEGIN_POLYGON = 100151 :: GLenum
pattern GLU_TESS_MISSING_BEGIN_CONTOUR = 100152 :: GLenum
pattern GLU_TESS_MISSING_END_POLYGON = 100153 :: GLenum
pattern GLU_TESS_MISSING_END_CONTOUR = 100154 :: GLenum
pattern GLU_TESS_COORD_TOO_LARGE = 100155 :: GLenum
pattern GLU_TESS_NEED_COMBINE_CALLBACK = 100156 :: GLenum

-- TessWinding
pattern GLU_TESS_WINDING_ODD = 100130 :: GLenum
pattern GLU_TESS_WINDING_NONZERO = 100131 :: GLenum
pattern GLU_TESS_WINDING_POSITIVE = 100132 :: GLenum
pattern GLU_TESS_WINDING_NEGATIVE = 100133 :: GLenum
pattern GLU_TESS_WINDING_ABS_GEQ_TWO = 100134 :: GLenum
