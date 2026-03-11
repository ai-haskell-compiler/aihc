{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GLU.Callbacks
-- Copyright   :  (c) Sven Panne 2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- All callbacks from GLU 1.3, see <http://www.opengl.org/>.
--
--------------------------------------------------------------------------------

module Graphics.GLU.Callbacks (
   -- * Tessellator Callbacks
   -- ** Tessellator Callbacks without Polygon Data
   TessBeginCallback, makeTessBeginCallback,
   TessEdgeFlagCallback, makeTessEdgeFlagCallback,
   TessVertexCallback, makeTessVertexCallback,
   TessEndCallback, makeTessEndCallback,
   TessErrorCallback, makeTessErrorCallback,
   TessCombineCallback, makeTessCombineCallback,
   -- ** Tessellator Callbacks with Polygon Data
   TessBeginDataCallback, makeTessBeginDataCallback,
   TessEdgeFlagDataCallback, makeTessEdgeFlagDataCallback,
   TessVertexDataCallback, makeTessVertexDataCallback,
   TessEndDataCallback, makeTessEndDataCallback,
   TessErrorDataCallback, makeTessErrorDataCallback,
   TessCombineDataCallback, makeTessCombineDataCallback,
   -- * Quadrics Callbacks
   QuadricCallback, makeQuadricCallback,
   -- * NURBS Callbacks
   -- ** NURBS Callbacks without User Data
   NURBSBeginCallback, makeNURBSBeginCallback,
   NURBSVertexCallback, makeNURBSVertexCallback,
   NURBSNormalCallback, makeNURBSNormalCallback,
   NURBSColorCallback, makeNURBSColorCallback,
   NURBSTexCoordCallback, makeNURBSTexCoordCallback,
   NURBSEndCallback, makeNURBSEndCallback,
   NURBSErrorCallback, makeNURBSErrorCallback,
   -- ** NURBS Callbacks with User Data
   NURBSBeginDataCallback, makeNURBSBeginDataCallback,
   NURBSVertexDataCallback, makeNURBSVertexDataCallback,
   NURBSNormalDataCallback, makeNURBSNormalDataCallback,
   NURBSColorDataCallback, makeNURBSColorDataCallback,
   NURBSTexCoordDataCallback, makeNURBSTexCoordDataCallback,
   NURBSEndDataCallback, makeNURBSEndDataCallback
) where

import Foreign.Ptr
import Graphics.GL

--------------------------------------------------------------------------------

type TessBeginCallback = GLenum -> IO ()

foreign import CALLCONV "wrapper"
   makeTessBeginCallback :: TessBeginCallback -> IO (FunPtr TessBeginCallback)

type TessEdgeFlagCallback = GLboolean -> IO ()

foreign import CALLCONV "wrapper"
   makeTessEdgeFlagCallback :: TessEdgeFlagCallback -> IO (FunPtr TessEdgeFlagCallback)

type TessVertexCallback v = Ptr v -> IO ()

foreign import CALLCONV "wrapper"
   makeTessVertexCallback :: TessVertexCallback v -> IO (FunPtr (TessVertexCallback v))

type TessEndCallback = IO ()

foreign import CALLCONV "wrapper"
   makeTessEndCallback :: TessEndCallback -> IO (FunPtr TessEndCallback)

type TessErrorCallback = GLenum -> IO ()

foreign import CALLCONV "wrapper"
   makeTessErrorCallback :: TessErrorCallback -> IO (FunPtr TessErrorCallback)

type TessCombineCallback v = Ptr GLdouble -> Ptr (Ptr v) -> Ptr GLfloat -> Ptr (Ptr v) -> IO ()

foreign import CALLCONV "wrapper"
   makeTessCombineCallback :: TessCombineCallback v -> IO (FunPtr (TessCombineCallback v))

type TessBeginDataCallback p = GLenum -> Ptr p -> IO ()

foreign import CALLCONV "wrapper"
   makeTessBeginDataCallback :: TessBeginDataCallback p -> IO (FunPtr (TessBeginDataCallback p))

type TessEdgeFlagDataCallback p = GLboolean -> Ptr p -> IO ()

foreign import CALLCONV "wrapper"
   makeTessEdgeFlagDataCallback :: TessEdgeFlagDataCallback p -> IO (FunPtr (TessEdgeFlagDataCallback p))

type TessVertexDataCallback v p = Ptr v -> Ptr p -> IO ()

foreign import CALLCONV "wrapper"
   makeTessVertexDataCallback :: TessVertexDataCallback v p -> IO (FunPtr (TessVertexDataCallback v p))

type TessEndDataCallback p = Ptr p -> IO ()

foreign import CALLCONV "wrapper"
   makeTessEndDataCallback :: TessEndDataCallback p -> IO (FunPtr (TessEndDataCallback p))

type TessErrorDataCallback p = GLenum -> Ptr p -> IO ()

foreign import CALLCONV "wrapper"
   makeTessErrorDataCallback :: TessErrorDataCallback p -> IO (FunPtr (TessErrorDataCallback p))

type TessCombineDataCallback v p = Ptr GLdouble -> Ptr (Ptr v) -> Ptr GLfloat -> Ptr (Ptr v) -> Ptr p -> IO ()

foreign import CALLCONV "wrapper"
   makeTessCombineDataCallback :: TessCombineDataCallback v p -> IO (FunPtr (TessCombineDataCallback v p))

--------------------------------------------------------------------------------

type QuadricCallback = GLenum -> IO ()

foreign import CALLCONV "wrapper"
   makeQuadricCallback :: QuadricCallback -> IO (FunPtr QuadricCallback)

--------------------------------------------------------------------------------

type NURBSBeginCallback = GLenum -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSBeginCallback :: NURBSBeginCallback -> IO (FunPtr NURBSBeginCallback)

type NURBSVertexCallback = Ptr GLfloat -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSVertexCallback :: NURBSVertexCallback -> IO (FunPtr NURBSVertexCallback)

type NURBSNormalCallback = Ptr GLfloat -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSNormalCallback :: NURBSNormalCallback -> IO (FunPtr NURBSNormalCallback)

type NURBSColorCallback = Ptr GLfloat -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSColorCallback :: NURBSColorCallback -> IO (FunPtr NURBSColorCallback)

type NURBSTexCoordCallback = Ptr GLfloat -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSTexCoordCallback :: NURBSTexCoordCallback -> IO (FunPtr NURBSTexCoordCallback)

type NURBSEndCallback = IO ()

foreign import CALLCONV "wrapper"
   makeNURBSEndCallback :: NURBSEndCallback -> IO (FunPtr NURBSEndCallback)

type NURBSErrorCallback = GLenum -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSErrorCallback :: NURBSErrorCallback -> IO (FunPtr NURBSErrorCallback)

type NURBSBeginDataCallback u = GLenum -> Ptr u -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSBeginDataCallback :: NURBSBeginDataCallback u -> IO (FunPtr (NURBSBeginDataCallback u))

type NURBSVertexDataCallback u = Ptr GLfloat -> Ptr u -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSVertexDataCallback :: NURBSVertexDataCallback u -> IO (FunPtr (NURBSVertexDataCallback u))

type NURBSNormalDataCallback u = Ptr GLfloat -> Ptr u -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSNormalDataCallback :: NURBSNormalDataCallback u -> IO (FunPtr (NURBSNormalDataCallback u))

type NURBSColorDataCallback u = Ptr GLfloat -> Ptr u -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSColorDataCallback :: NURBSColorDataCallback u -> IO (FunPtr (NURBSColorDataCallback u))

type NURBSTexCoordDataCallback u = Ptr GLfloat -> Ptr u -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSTexCoordDataCallback :: NURBSTexCoordDataCallback u -> IO (FunPtr (NURBSTexCoordDataCallback u))

type NURBSEndDataCallback u = Ptr u -> IO ()

foreign import CALLCONV "wrapper"
   makeNURBSEndDataCallback :: NURBSEndDataCallback u -> IO (FunPtr (NURBSEndDataCallback u))

