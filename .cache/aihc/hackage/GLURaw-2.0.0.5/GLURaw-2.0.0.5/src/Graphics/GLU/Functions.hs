{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GLU.Functions
-- Copyright   :  (c) Sven Panne 2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- All functions from GLU 1.3, see <http://www.opengl.org/>.
--
--------------------------------------------------------------------------------

module Graphics.GLU.Functions (
   gluBeginCurve,
   gluBeginPolygon,
   gluBeginSurface,
   gluBeginTrim,
   gluBuild1DMipmapLevels,
   gluBuild1DMipmaps,
   gluBuild2DMipmapLevels,
   gluBuild2DMipmaps,
   gluBuild3DMipmapLevels,
   gluBuild3DMipmaps,
   gluCheckExtension,
   gluCylinder,
   gluDeleteNurbsRenderer,
   gluDeleteQuadric,
   gluDeleteTess,
   gluDisk,
   gluEndCurve,
   gluEndPolygon,
   gluEndSurface,
   gluEndTrim,
   gluErrorString,
   gluGetNurbsProperty,
   gluGetString,
   gluGetTessProperty,
   gluLoadSamplingMatrices,
   gluLookAt,
   gluNewNurbsRenderer,
   gluNewQuadric,
   gluNewTess,
   gluNextContour,
   gluNurbsCallback,
   gluNurbsCallbackData,
   gluNurbsCallbackDataEXT,
   gluNurbsCurve,
   gluNurbsProperty,
   gluNurbsSurface,
   gluOrtho2D,
   gluPartialDisk,
   gluPerspective,
   gluPickMatrix,
   gluProject,
   gluPwlCurve,
   gluQuadricCallback,
   gluQuadricDrawStyle,
   gluQuadricNormals,
   gluQuadricOrientation,
   gluQuadricTexture,
   gluScaleImage,
   gluSphere,
   gluTessBeginContour,
   gluTessBeginPolygon,
   gluTessCallback,
   gluTessEndContour,
   gluTessEndPolygon,
   gluTessNormal,
   gluTessProperty,
   gluTessVertex,
   gluUnProject,
   gluUnProject4,
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.C.String ( withCString, CString )
import Foreign.Marshal.Error ( throwIf )
import Foreign.Ptr ( Ptr, FunPtr, nullFunPtr )
import System.IO.Unsafe ( unsafePerformIO )

import Graphics.GLU.Types
import Graphics.GL.Types

--------------------------------------------------------------------------------

-- | Retrieve a GLU API entry by name. Throws a userError when no entry with the
-- given name was found.
getAPIEntry :: String -> IO (FunPtr a)
getAPIEntry extensionEntry =
   throwIfNullFunPtr ("unknown GLU entry " ++ extensionEntry) $
      withCString extensionEntry hs_GLU_getProcAddress

throwIfNullFunPtr :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNullFunPtr = throwIf (== nullFunPtr) . const

foreign import ccall unsafe "hs_GLU_getProcAddress"
   hs_GLU_getProcAddress :: CString -> IO (FunPtr a)

-- gluBeginCurve ----------------------------------------------------------------------

gluBeginCurve :: MonadIO m => Ptr GLUnurbs -> m ()
gluBeginCurve v1 = liftIO $ dyn_gluBeginCurve ptr_gluBeginCurve v1

foreign import CALLCONV "dynamic" dyn_gluBeginCurve
  :: FunPtr (Ptr GLUnurbs -> IO ())
  ->         Ptr GLUnurbs -> IO ()

{-# NOINLINE ptr_gluBeginCurve #-}
ptr_gluBeginCurve :: FunPtr a
ptr_gluBeginCurve = unsafePerformIO $ getAPIEntry "gluBeginCurve"

-- gluBeginPolygon ----------------------------------------------------------------------

gluBeginPolygon :: MonadIO m => Ptr GLUtesselator -> m ()
gluBeginPolygon v1 = liftIO $ dyn_gluBeginPolygon ptr_gluBeginPolygon v1

foreign import CALLCONV "dynamic" dyn_gluBeginPolygon
  :: FunPtr (Ptr GLUtesselator -> IO ())
  ->         Ptr GLUtesselator -> IO ()

{-# NOINLINE ptr_gluBeginPolygon #-}
ptr_gluBeginPolygon :: FunPtr a
ptr_gluBeginPolygon = unsafePerformIO $ getAPIEntry "gluBeginPolygon"

-- gluBeginSurface ----------------------------------------------------------------------

gluBeginSurface :: MonadIO m => Ptr GLUnurbs -> m ()
gluBeginSurface v1 = liftIO $ dyn_gluBeginSurface ptr_gluBeginSurface v1

foreign import CALLCONV "dynamic" dyn_gluBeginSurface
  :: FunPtr (Ptr GLUnurbs -> IO ())
  ->         Ptr GLUnurbs -> IO ()

{-# NOINLINE ptr_gluBeginSurface #-}
ptr_gluBeginSurface :: FunPtr a
ptr_gluBeginSurface = unsafePerformIO $ getAPIEntry "gluBeginSurface"

-- gluBeginTrim ----------------------------------------------------------------------

gluBeginTrim :: MonadIO m => Ptr GLUnurbs -> m ()
gluBeginTrim v1 = liftIO $ dyn_gluBeginTrim ptr_gluBeginTrim v1

foreign import CALLCONV "dynamic" dyn_gluBeginTrim
  :: FunPtr (Ptr GLUnurbs -> IO ())
  ->         Ptr GLUnurbs -> IO ()

{-# NOINLINE ptr_gluBeginTrim #-}
ptr_gluBeginTrim :: FunPtr a
ptr_gluBeginTrim = unsafePerformIO $ getAPIEntry "gluBeginTrim"

-- gluBuild1DMipmapLevels ----------------------------------------------------------------------

gluBuild1DMipmapLevels :: MonadIO m => GLenum -> GLint -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> m GLint
gluBuild1DMipmapLevels v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn_gluBuild1DMipmapLevels ptr_gluBuild1DMipmapLevels v1 v2 v3 v4 v5 v6 v7 v8 v9

foreign import CALLCONV "dynamic" dyn_gluBuild1DMipmapLevels
  :: FunPtr (GLenum -> GLint -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> IO GLint)
  ->         GLenum -> GLint -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> IO GLint

{-# NOINLINE ptr_gluBuild1DMipmapLevels #-}
ptr_gluBuild1DMipmapLevels :: FunPtr a
ptr_gluBuild1DMipmapLevels = unsafePerformIO $ getAPIEntry "gluBuild1DMipmapLevels"

-- gluBuild1DMipmaps ----------------------------------------------------------------------

gluBuild1DMipmaps :: MonadIO m => GLenum -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> m GLint
gluBuild1DMipmaps v1 v2 v3 v4 v5 v6 = liftIO $ dyn_gluBuild1DMipmaps ptr_gluBuild1DMipmaps v1 v2 v3 v4 v5 v6

foreign import CALLCONV "dynamic" dyn_gluBuild1DMipmaps
  :: FunPtr (GLenum -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO GLint)
  ->         GLenum -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO GLint

{-# NOINLINE ptr_gluBuild1DMipmaps #-}
ptr_gluBuild1DMipmaps :: FunPtr a
ptr_gluBuild1DMipmaps = unsafePerformIO $ getAPIEntry "gluBuild1DMipmaps"

-- gluBuild2DMipmapLevels ----------------------------------------------------------------------

gluBuild2DMipmapLevels :: MonadIO m => GLenum -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> m GLint
gluBuild2DMipmapLevels v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn_gluBuild2DMipmapLevels ptr_gluBuild2DMipmapLevels v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

foreign import CALLCONV "dynamic" dyn_gluBuild2DMipmapLevels
  :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> IO GLint)
  ->         GLenum -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> IO GLint

{-# NOINLINE ptr_gluBuild2DMipmapLevels #-}
ptr_gluBuild2DMipmapLevels :: FunPtr a
ptr_gluBuild2DMipmapLevels = unsafePerformIO $ getAPIEntry "gluBuild2DMipmapLevels"

-- gluBuild2DMipmaps ----------------------------------------------------------------------

gluBuild2DMipmaps :: MonadIO m => GLenum -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> m GLint
gluBuild2DMipmaps v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn_gluBuild2DMipmaps ptr_gluBuild2DMipmaps v1 v2 v3 v4 v5 v6 v7

foreign import CALLCONV "dynamic" dyn_gluBuild2DMipmaps
  :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO GLint)
  ->         GLenum -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO GLint

{-# NOINLINE ptr_gluBuild2DMipmaps #-}
ptr_gluBuild2DMipmaps :: FunPtr a
ptr_gluBuild2DMipmaps = unsafePerformIO $ getAPIEntry "gluBuild2DMipmaps"

-- gluBuild3DMipmapLevels ----------------------------------------------------------------------

gluBuild3DMipmapLevels :: MonadIO m => GLenum -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> m GLint
gluBuild3DMipmapLevels v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11= liftIO $ dyn_gluBuild3DMipmapLevels ptr_gluBuild3DMipmapLevels v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

foreign import CALLCONV "dynamic" dyn_gluBuild3DMipmapLevels
  :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> IO GLint)
  ->         GLenum -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLint -> GLint -> GLint -> Ptr a -> IO GLint

{-# NOINLINE ptr_gluBuild3DMipmapLevels #-}
ptr_gluBuild3DMipmapLevels :: FunPtr a
ptr_gluBuild3DMipmapLevels = unsafePerformIO $ getAPIEntry "gluBuild3DMipmapLevels"

-- gluBuild3DMipmaps ----------------------------------------------------------------------

gluBuild3DMipmaps :: MonadIO m => GLenum -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> m GLint
gluBuild3DMipmaps v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn_gluBuild3DMipmaps ptr_gluBuild3DMipmaps v1 v2 v3 v4 v5 v6 v7 v8

foreign import CALLCONV "dynamic" dyn_gluBuild3DMipmaps
  :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO GLint)
  ->         GLenum -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO GLint

{-# NOINLINE ptr_gluBuild3DMipmaps #-}
ptr_gluBuild3DMipmaps :: FunPtr a
ptr_gluBuild3DMipmaps = unsafePerformIO $ getAPIEntry "gluBuild3DMipmaps"

-- gluCheckExtension ----------------------------------------------------------------------

gluCheckExtension :: MonadIO m => Ptr GLubyte -> Ptr GLubyte -> m GLboolean
gluCheckExtension v1 v2 = liftIO $ dyn_gluCheckExtension ptr_gluCheckExtension v1 v2

foreign import CALLCONV "dynamic" dyn_gluCheckExtension
  :: FunPtr (Ptr GLubyte -> Ptr GLubyte -> IO GLboolean)
  ->         Ptr GLubyte -> Ptr GLubyte -> IO GLboolean

{-# NOINLINE ptr_gluCheckExtension #-}
ptr_gluCheckExtension :: FunPtr a
ptr_gluCheckExtension = unsafePerformIO $ getAPIEntry "gluCheckExtension"

-- gluCylinder ----------------------------------------------------------------------

gluCylinder :: MonadIO m => Ptr GLUquadric -> GLdouble -> GLdouble -> GLdouble -> GLint -> GLint -> m ()
gluCylinder v1 v2 v3 v4 v5 v6 = liftIO $ dyn_gluCylinder ptr_gluCylinder v1 v2 v3 v4 v5 v6

foreign import CALLCONV "dynamic" dyn_gluCylinder
  :: FunPtr (Ptr GLUquadric -> GLdouble -> GLdouble -> GLdouble -> GLint -> GLint -> IO ())
  ->         Ptr GLUquadric -> GLdouble -> GLdouble -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_gluCylinder #-}
ptr_gluCylinder :: FunPtr a
ptr_gluCylinder = unsafePerformIO $ getAPIEntry "gluCylinder"

-- gluDeleteNurbsRenderer ----------------------------------------------------------------------

gluDeleteNurbsRenderer :: MonadIO m => Ptr GLUnurbs -> m ()
gluDeleteNurbsRenderer v1 = liftIO $ dyn_gluDeleteNurbsRenderer ptr_gluDeleteNurbsRenderer v1

foreign import CALLCONV "dynamic" dyn_gluDeleteNurbsRenderer
  :: FunPtr (Ptr GLUnurbs -> IO ())
  ->         Ptr GLUnurbs -> IO ()

{-# NOINLINE ptr_gluDeleteNurbsRenderer #-}
ptr_gluDeleteNurbsRenderer :: FunPtr a
ptr_gluDeleteNurbsRenderer = unsafePerformIO $ getAPIEntry "gluDeleteNurbsRenderer"

-- gluDeleteQuadric ----------------------------------------------------------------------

gluDeleteQuadric :: MonadIO m => Ptr GLUquadric -> m ()
gluDeleteQuadric v1 = liftIO $ dyn_gluDeleteQuadric ptr_gluDeleteQuadric v1

foreign import CALLCONV "dynamic" dyn_gluDeleteQuadric
  :: FunPtr (Ptr GLUquadric -> IO ())
  ->         Ptr GLUquadric -> IO ()

{-# NOINLINE ptr_gluDeleteQuadric #-}
ptr_gluDeleteQuadric :: FunPtr a
ptr_gluDeleteQuadric = unsafePerformIO $ getAPIEntry "gluDeleteQuadric"

-- gluDeleteTess ----------------------------------------------------------------------

gluDeleteTess :: MonadIO m => Ptr GLUtesselator -> m ()
gluDeleteTess v1 = liftIO $ dyn_gluDeleteTess ptr_gluDeleteTess v1

foreign import CALLCONV "dynamic" dyn_gluDeleteTess
  :: FunPtr (Ptr GLUtesselator -> IO ())
  ->         Ptr GLUtesselator -> IO ()

{-# NOINLINE ptr_gluDeleteTess #-}
ptr_gluDeleteTess :: FunPtr a
ptr_gluDeleteTess = unsafePerformIO $ getAPIEntry "gluDeleteTess"

-- gluDisk ----------------------------------------------------------------------

gluDisk :: MonadIO m => Ptr GLUquadric -> GLdouble -> GLdouble -> GLint -> GLint -> m ()
gluDisk v1 v2 v3 v4 v5 = liftIO $ dyn_gluDisk ptr_gluDisk v1 v2 v3 v4 v5

foreign import CALLCONV "dynamic" dyn_gluDisk
  :: FunPtr (Ptr GLUquadric -> GLdouble -> GLdouble -> GLint -> GLint -> IO ())
  ->         Ptr GLUquadric -> GLdouble -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_gluDisk #-}
ptr_gluDisk :: FunPtr a
ptr_gluDisk = unsafePerformIO $ getAPIEntry "gluDisk"

-- gluEndCurve ----------------------------------------------------------------------

gluEndCurve :: MonadIO m => Ptr GLUnurbs -> m ()
gluEndCurve v1 = liftIO $ dyn_gluEndCurve ptr_gluEndCurve v1

foreign import CALLCONV "dynamic" dyn_gluEndCurve
  :: FunPtr (Ptr GLUnurbs -> IO ())
  ->         Ptr GLUnurbs -> IO ()

{-# NOINLINE ptr_gluEndCurve #-}
ptr_gluEndCurve :: FunPtr a
ptr_gluEndCurve = unsafePerformIO $ getAPIEntry "gluEndCurve"

-- gluEndPolygon ----------------------------------------------------------------------

gluEndPolygon :: MonadIO m => Ptr GLUtesselator -> m ()
gluEndPolygon v1 = liftIO $ dyn_gluEndPolygon ptr_gluEndPolygon v1

foreign import CALLCONV "dynamic" dyn_gluEndPolygon
  :: FunPtr (Ptr GLUtesselator -> IO ())
  ->         Ptr GLUtesselator -> IO ()

{-# NOINLINE ptr_gluEndPolygon #-}
ptr_gluEndPolygon :: FunPtr a
ptr_gluEndPolygon = unsafePerformIO $ getAPIEntry "gluEndPolygon"

-- gluEndSurface ----------------------------------------------------------------------

gluEndSurface :: MonadIO m => Ptr GLUnurbs -> m ()
gluEndSurface v1 = liftIO $ dyn_gluEndSurface ptr_gluEndSurface v1

foreign import CALLCONV "dynamic" dyn_gluEndSurface
  :: FunPtr (Ptr GLUnurbs -> IO ())
  ->         Ptr GLUnurbs -> IO ()

{-# NOINLINE ptr_gluEndSurface #-}
ptr_gluEndSurface :: FunPtr a
ptr_gluEndSurface = unsafePerformIO $ getAPIEntry "gluEndSurface"

-- gluEndTrim ----------------------------------------------------------------------

gluEndTrim :: MonadIO m => Ptr GLUnurbs -> m ()
gluEndTrim v1 = liftIO $ dyn_gluEndTrim ptr_gluEndTrim v1

foreign import CALLCONV "dynamic" dyn_gluEndTrim
  :: FunPtr (Ptr GLUnurbs -> IO ())
  ->         Ptr GLUnurbs -> IO ()

{-# NOINLINE ptr_gluEndTrim #-}
ptr_gluEndTrim :: FunPtr a
ptr_gluEndTrim = unsafePerformIO $ getAPIEntry "gluEndTrim"

-- gluErrorString ----------------------------------------------------------------------

gluErrorString :: MonadIO m => GLenum -> m (Ptr GLubyte)
gluErrorString v1 = liftIO $ dyn_gluErrorString ptr_gluErrorString v1

foreign import CALLCONV "dynamic" dyn_gluErrorString
  :: FunPtr (GLenum -> IO (Ptr GLubyte))
  ->         GLenum -> IO (Ptr GLubyte)

{-# NOINLINE ptr_gluErrorString #-}
ptr_gluErrorString :: FunPtr a
ptr_gluErrorString = unsafePerformIO $ getAPIEntry "gluErrorString"

-- gluGetNurbsProperty ----------------------------------------------------------------------

gluGetNurbsProperty :: MonadIO m => Ptr GLUnurbs -> GLenum -> Ptr GLfloat -> m ()
gluGetNurbsProperty v1 v2 v3 = liftIO $ dyn_gluGetNurbsProperty ptr_gluGetNurbsProperty v1 v2 v3

foreign import CALLCONV "dynamic" dyn_gluGetNurbsProperty
  :: FunPtr (Ptr GLUnurbs -> GLenum -> Ptr GLfloat -> IO ())
  ->         Ptr GLUnurbs -> GLenum -> Ptr GLfloat -> IO ()

{-# NOINLINE ptr_gluGetNurbsProperty #-}
ptr_gluGetNurbsProperty :: FunPtr a
ptr_gluGetNurbsProperty = unsafePerformIO $ getAPIEntry "gluGetNurbsProperty"

-- gluGetString ----------------------------------------------------------------------

gluGetString :: MonadIO m => GLenum -> m (Ptr GLubyte)
gluGetString v1 = liftIO $ dyn_gluGetString ptr_gluGetString v1

foreign import CALLCONV "dynamic" dyn_gluGetString
  :: FunPtr (GLenum -> IO (Ptr GLubyte))
  ->         GLenum -> IO (Ptr GLubyte)

{-# NOINLINE ptr_gluGetString #-}
ptr_gluGetString :: FunPtr a
ptr_gluGetString = unsafePerformIO $ getAPIEntry "gluGetString"

-- gluGetTessProperty ----------------------------------------------------------------------

gluGetTessProperty :: MonadIO m => Ptr GLUtesselator -> GLenum -> Ptr GLdouble -> m ()
gluGetTessProperty v1 v2 v3 = liftIO $ dyn_gluGetTessProperty ptr_gluGetTessProperty v1 v2 v3

foreign import CALLCONV "dynamic" dyn_gluGetTessProperty
  :: FunPtr (Ptr GLUtesselator -> GLenum -> Ptr GLdouble -> IO ())
  ->         Ptr GLUtesselator -> GLenum -> Ptr GLdouble -> IO ()

{-# NOINLINE ptr_gluGetTessProperty #-}
ptr_gluGetTessProperty :: FunPtr a
ptr_gluGetTessProperty = unsafePerformIO $ getAPIEntry "gluGetTessProperty"

-- gluLoadSamplingMatrices ----------------------------------------------------------------------

gluLoadSamplingMatrices :: MonadIO m => Ptr GLUnurbs -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLint -> m ()
gluLoadSamplingMatrices v1 v2 v3 v4 = liftIO $ dyn_gluLoadSamplingMatrices ptr_gluLoadSamplingMatrices v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_gluLoadSamplingMatrices
  :: FunPtr (Ptr GLUnurbs -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLint -> IO ())
  ->         Ptr GLUnurbs -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLint -> IO ()

{-# NOINLINE ptr_gluLoadSamplingMatrices #-}
ptr_gluLoadSamplingMatrices :: FunPtr a
ptr_gluLoadSamplingMatrices = unsafePerformIO $ getAPIEntry "gluLoadSamplingMatrices"

-- gluLookAt ----------------------------------------------------------------------

gluLookAt :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
gluLookAt v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn_gluLookAt ptr_gluLookAt v1 v2 v3 v4 v5 v6 v7 v8 v9

foreign import CALLCONV "dynamic" dyn_gluLookAt
  :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
  ->         GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

{-# NOINLINE ptr_gluLookAt #-}
ptr_gluLookAt :: FunPtr a
ptr_gluLookAt = unsafePerformIO $ getAPIEntry "gluLookAt"

-- gluNewNurbsRenderer ----------------------------------------------------------------------

gluNewNurbsRenderer :: MonadIO m => m (Ptr GLUnurbs)
gluNewNurbsRenderer = liftIO $ dyn_gluNewNurbsRenderer ptr_gluNewNurbsRenderer

foreign import CALLCONV "dynamic" dyn_gluNewNurbsRenderer
  :: FunPtr (IO (Ptr GLUnurbs))
  ->         IO (Ptr GLUnurbs)

{-# NOINLINE ptr_gluNewNurbsRenderer #-}
ptr_gluNewNurbsRenderer :: FunPtr a
ptr_gluNewNurbsRenderer = unsafePerformIO $ getAPIEntry "gluNewNurbsRenderer"

-- gluNewQuadric ----------------------------------------------------------------------

gluNewQuadric :: MonadIO m => m (Ptr GLUquadric)
gluNewQuadric = liftIO $ dyn_gluNewQuadric ptr_gluNewQuadric

foreign import CALLCONV "dynamic" dyn_gluNewQuadric
  :: FunPtr (IO (Ptr GLUquadric))
  ->         IO (Ptr GLUquadric)

{-# NOINLINE ptr_gluNewQuadric #-}
ptr_gluNewQuadric :: FunPtr a
ptr_gluNewQuadric = unsafePerformIO $ getAPIEntry "gluNewQuadric"

-- gluNewTess ----------------------------------------------------------------------

gluNewTess :: MonadIO m => m (Ptr GLUtesselator)
gluNewTess = liftIO $ dyn_gluNewTess ptr_gluNewTess

foreign import CALLCONV "dynamic" dyn_gluNewTess
  :: FunPtr (IO (Ptr GLUtesselator))
  ->         IO (Ptr GLUtesselator)

{-# NOINLINE ptr_gluNewTess #-}
ptr_gluNewTess :: FunPtr a
ptr_gluNewTess = unsafePerformIO $ getAPIEntry "gluNewTess"

-- gluNextContour ----------------------------------------------------------------------

gluNextContour :: MonadIO m => Ptr GLUtesselator -> GLenum -> m ()
gluNextContour v1 v2 = liftIO $ dyn_gluNextContour ptr_gluNextContour v1 v2

foreign import CALLCONV "dynamic" dyn_gluNextContour
  :: FunPtr (Ptr GLUtesselator -> GLenum -> IO ())
  ->         Ptr GLUtesselator -> GLenum -> IO ()

{-# NOINLINE ptr_gluNextContour #-}
ptr_gluNextContour :: FunPtr a
ptr_gluNextContour = unsafePerformIO $ getAPIEntry "gluNextContour"

-- gluNurbsCallback ----------------------------------------------------------------------

gluNurbsCallback :: MonadIO m => Ptr GLUnurbs -> GLenum -> FunPtr a -> m ()
gluNurbsCallback v1 v2 v3 = liftIO $ dyn_gluNurbsCallback ptr_gluNurbsCallback v1 v2 v3

foreign import CALLCONV "dynamic" dyn_gluNurbsCallback
  :: FunPtr (Ptr GLUnurbs -> GLenum -> FunPtr a -> IO ())
  ->         Ptr GLUnurbs -> GLenum -> FunPtr a -> IO ()

{-# NOINLINE ptr_gluNurbsCallback #-}
ptr_gluNurbsCallback :: FunPtr a
ptr_gluNurbsCallback = unsafePerformIO $ getAPIEntry "gluNurbsCallback"

-- gluNurbsCallbackData ----------------------------------------------------------------------

gluNurbsCallbackData :: MonadIO m => Ptr GLUnurbs -> Ptr a -> m ()
gluNurbsCallbackData v1 v2 = liftIO $ dyn_gluNurbsCallbackData ptr_gluNurbsCallbackData v1 v2

foreign import CALLCONV "dynamic" dyn_gluNurbsCallbackData
  :: FunPtr (Ptr GLUnurbs -> Ptr a -> IO ())
  ->         Ptr GLUnurbs -> Ptr a -> IO ()

{-# NOINLINE ptr_gluNurbsCallbackData #-}
ptr_gluNurbsCallbackData :: FunPtr a
ptr_gluNurbsCallbackData = unsafePerformIO $ getAPIEntry "gluNurbsCallbackData"

-- gluNurbsCallbackDataEXT ----------------------------------------------------------------------

gluNurbsCallbackDataEXT :: MonadIO m => Ptr GLUnurbs -> Ptr a -> m ()
gluNurbsCallbackDataEXT v1 v2 = liftIO $ dyn_gluNurbsCallbackDataEXT ptr_gluNurbsCallbackDataEXT v1 v2

foreign import CALLCONV "dynamic" dyn_gluNurbsCallbackDataEXT
  :: FunPtr (Ptr GLUnurbs -> Ptr a -> IO ())
  ->         Ptr GLUnurbs -> Ptr a -> IO ()

{-# NOINLINE ptr_gluNurbsCallbackDataEXT #-}
ptr_gluNurbsCallbackDataEXT :: FunPtr a
ptr_gluNurbsCallbackDataEXT = unsafePerformIO $ getAPIEntry "gluNurbsCallbackDataEXT"

-- gluNurbsCurve ----------------------------------------------------------------------

gluNurbsCurve :: MonadIO m => Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLenum -> m ()
gluNurbsCurve v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn_gluNurbsCurve ptr_gluNurbsCurve v1 v2 v3 v4 v5 v6 v7

foreign import CALLCONV "dynamic" dyn_gluNurbsCurve
  :: FunPtr (Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLenum -> IO ())
  ->         Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLenum -> IO ()

{-# NOINLINE ptr_gluNurbsCurve #-}
ptr_gluNurbsCurve :: FunPtr a
ptr_gluNurbsCurve = unsafePerformIO $ getAPIEntry "gluNurbsCurve"

-- gluNurbsProperty ----------------------------------------------------------------------

gluNurbsProperty :: MonadIO m => Ptr GLUnurbs -> GLenum -> GLfloat -> m ()
gluNurbsProperty v1 v2 v3 = liftIO $ dyn_gluNurbsProperty ptr_gluNurbsProperty v1 v2 v3

foreign import CALLCONV "dynamic" dyn_gluNurbsProperty
  :: FunPtr (Ptr GLUnurbs -> GLenum -> GLfloat -> IO ())
  ->         Ptr GLUnurbs -> GLenum -> GLfloat -> IO ()

{-# NOINLINE ptr_gluNurbsProperty #-}
ptr_gluNurbsProperty :: FunPtr a
ptr_gluNurbsProperty = unsafePerformIO $ getAPIEntry "gluNurbsProperty"

-- gluNurbsSurface ----------------------------------------------------------------------

gluNurbsSurface :: MonadIO m => Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLint -> Ptr GLfloat -> GLint -> GLint -> GLenum -> m ()
gluNurbsSurface v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn_gluNurbsSurface ptr_gluNurbsSurface v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

foreign import CALLCONV "dynamic" dyn_gluNurbsSurface
  :: FunPtr (Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLint -> Ptr GLfloat -> GLint -> GLint -> GLenum -> IO ())
  ->         Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLint -> Ptr GLfloat -> GLint -> GLint -> GLenum -> IO ()

{-# NOINLINE ptr_gluNurbsSurface #-}
ptr_gluNurbsSurface :: FunPtr a
ptr_gluNurbsSurface = unsafePerformIO $ getAPIEntry "gluNurbsSurface"

-- gluOrtho2D ----------------------------------------------------------------------

gluOrtho2D :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
gluOrtho2D v1 v2 v3 v4 = liftIO $ dyn_gluOrtho2D ptr_gluOrtho2D v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_gluOrtho2D
  :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
  ->         GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

{-# NOINLINE ptr_gluOrtho2D #-}
ptr_gluOrtho2D :: FunPtr a
ptr_gluOrtho2D = unsafePerformIO $ getAPIEntry "gluOrtho2D"

-- gluPartialDisk ----------------------------------------------------------------------

gluPartialDisk :: MonadIO m => Ptr GLUquadric -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> m ()
gluPartialDisk v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn_gluPartialDisk ptr_gluPartialDisk v1 v2 v3 v4 v5 v6 v7

foreign import CALLCONV "dynamic" dyn_gluPartialDisk
  :: FunPtr (Ptr GLUquadric -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> IO ())
  ->         Ptr GLUquadric -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> IO ()

{-# NOINLINE ptr_gluPartialDisk #-}
ptr_gluPartialDisk :: FunPtr a
ptr_gluPartialDisk = unsafePerformIO $ getAPIEntry "gluPartialDisk"

-- gluPerspective ----------------------------------------------------------------------

gluPerspective :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
gluPerspective v1 v2 v3 v4 = liftIO $ dyn_gluPerspective ptr_gluPerspective v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_gluPerspective
  :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
  ->         GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

{-# NOINLINE ptr_gluPerspective #-}
ptr_gluPerspective :: FunPtr a
ptr_gluPerspective = unsafePerformIO $ getAPIEntry "gluPerspective"

-- gluPickMatrix ----------------------------------------------------------------------

gluPickMatrix :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLint -> m ()
gluPickMatrix v1 v2 v3 v4 v5 = liftIO $ dyn_gluPickMatrix ptr_gluPickMatrix v1 v2 v3 v4 v5

foreign import CALLCONV "dynamic" dyn_gluPickMatrix
  :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLint -> IO ())
  ->         GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLint -> IO ()

{-# NOINLINE ptr_gluPickMatrix #-}
ptr_gluPickMatrix :: FunPtr a
ptr_gluPickMatrix = unsafePerformIO $ getAPIEntry "gluPickMatrix"

-- gluProject ----------------------------------------------------------------------

gluProject :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> m GLint
gluProject v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn_gluProject ptr_gluProject v1 v2 v3 v4 v5 v6 v7 v8 v9

foreign import CALLCONV "dynamic" dyn_gluProject
  :: FunPtr (GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint)
  ->         GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

{-# NOINLINE ptr_gluProject #-}
ptr_gluProject :: FunPtr a
ptr_gluProject = unsafePerformIO $ getAPIEntry "gluProject"

-- gluPwlCurve ----------------------------------------------------------------------

gluPwlCurve :: MonadIO m => Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> GLenum -> m ()
gluPwlCurve v1 v2 v3 v4 v5 = liftIO $ dyn_gluPwlCurve ptr_gluPwlCurve v1 v2 v3 v4 v5

foreign import CALLCONV "dynamic" dyn_gluPwlCurve
  :: FunPtr (Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> GLenum -> IO ())
  ->         Ptr GLUnurbs -> GLint -> Ptr GLfloat -> GLint -> GLenum -> IO ()

{-# NOINLINE ptr_gluPwlCurve #-}
ptr_gluPwlCurve :: FunPtr a
ptr_gluPwlCurve = unsafePerformIO $ getAPIEntry "gluPwlCurve"

-- gluQuadricCallback ----------------------------------------------------------------------

gluQuadricCallback :: MonadIO m => Ptr GLUquadric -> GLenum -> FunPtr a -> m ()
gluQuadricCallback v1 v2 v3 = liftIO $ dyn_gluQuadricCallback ptr_gluQuadricCallback v1 v2 v3

foreign import CALLCONV "dynamic" dyn_gluQuadricCallback
  :: FunPtr (Ptr GLUquadric -> GLenum -> FunPtr a -> IO ())
  ->         Ptr GLUquadric -> GLenum -> FunPtr a -> IO ()

{-# NOINLINE ptr_gluQuadricCallback #-}
ptr_gluQuadricCallback :: FunPtr a
ptr_gluQuadricCallback = unsafePerformIO $ getAPIEntry "gluQuadricCallback"

-- gluQuadricDrawStyle ----------------------------------------------------------------------

gluQuadricDrawStyle :: MonadIO m => Ptr GLUquadric -> GLenum -> m ()
gluQuadricDrawStyle v1 v2 = liftIO $ dyn_gluQuadricDrawStyle ptr_gluQuadricDrawStyle v1 v2

foreign import CALLCONV "dynamic" dyn_gluQuadricDrawStyle
  :: FunPtr (Ptr GLUquadric -> GLenum -> IO ())
  ->         Ptr GLUquadric -> GLenum -> IO ()

{-# NOINLINE ptr_gluQuadricDrawStyle #-}
ptr_gluQuadricDrawStyle :: FunPtr a
ptr_gluQuadricDrawStyle = unsafePerformIO $ getAPIEntry "gluQuadricDrawStyle"

-- gluQuadricNormals ----------------------------------------------------------------------

gluQuadricNormals :: MonadIO m => Ptr GLUquadric -> GLenum -> m ()
gluQuadricNormals v1 v2 = liftIO $ dyn_gluQuadricNormals ptr_gluQuadricNormals v1 v2

foreign import CALLCONV "dynamic" dyn_gluQuadricNormals
  :: FunPtr (Ptr GLUquadric -> GLenum -> IO ())
  ->         Ptr GLUquadric -> GLenum -> IO ()

{-# NOINLINE ptr_gluQuadricNormals #-}
ptr_gluQuadricNormals :: FunPtr a
ptr_gluQuadricNormals = unsafePerformIO $ getAPIEntry "gluQuadricNormals"

-- gluQuadricOrientation ----------------------------------------------------------------------

gluQuadricOrientation :: MonadIO m => Ptr GLUquadric -> GLenum -> m ()
gluQuadricOrientation v1 v2 = liftIO $ dyn_gluQuadricOrientation ptr_gluQuadricOrientation v1 v2

foreign import CALLCONV "dynamic" dyn_gluQuadricOrientation
  :: FunPtr (Ptr GLUquadric -> GLenum -> IO ())
  ->         Ptr GLUquadric -> GLenum -> IO ()

{-# NOINLINE ptr_gluQuadricOrientation #-}
ptr_gluQuadricOrientation :: FunPtr a
ptr_gluQuadricOrientation = unsafePerformIO $ getAPIEntry "gluQuadricOrientation"

-- gluQuadricTexture ----------------------------------------------------------------------

gluQuadricTexture :: MonadIO m => Ptr GLUquadric -> GLboolean -> m ()
gluQuadricTexture v1 v2 = liftIO $ dyn_gluQuadricTexture ptr_gluQuadricTexture v1 v2

foreign import CALLCONV "dynamic" dyn_gluQuadricTexture
  :: FunPtr (Ptr GLUquadric -> GLboolean -> IO ())
  ->         Ptr GLUquadric -> GLboolean -> IO ()

{-# NOINLINE ptr_gluQuadricTexture #-}
ptr_gluQuadricTexture :: FunPtr a
ptr_gluQuadricTexture = unsafePerformIO $ getAPIEntry "gluQuadricTexture"

-- gluScaleImage ----------------------------------------------------------------------

gluScaleImage :: MonadIO m => GLenum -> GLsizei -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLsizei -> GLenum -> Ptr b -> m GLint
gluScaleImage v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn_gluScaleImage ptr_gluScaleImage v1 v2 v3 v4 v5 v6 v7 v8 v9

foreign import CALLCONV "dynamic" dyn_gluScaleImage
  :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLsizei -> GLenum -> Ptr b -> IO GLint)
  ->         GLenum -> GLsizei -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLsizei -> GLenum -> Ptr b -> IO GLint

{-# NOINLINE ptr_gluScaleImage #-}
ptr_gluScaleImage :: FunPtr a
ptr_gluScaleImage = unsafePerformIO $ getAPIEntry "gluScaleImage"

-- gluSphere ----------------------------------------------------------------------

gluSphere :: MonadIO m => Ptr GLUquadric -> GLdouble -> GLint -> GLint -> m ()
gluSphere v1 v2 v3 v4 = liftIO $ dyn_gluSphere ptr_gluSphere v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_gluSphere
  :: FunPtr (Ptr GLUquadric -> GLdouble -> GLint -> GLint -> IO ())
  ->         Ptr GLUquadric -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_gluSphere #-}
ptr_gluSphere :: FunPtr a
ptr_gluSphere = unsafePerformIO $ getAPIEntry "gluSphere"

-- gluTessBeginContour ----------------------------------------------------------------------

gluTessBeginContour :: MonadIO m => Ptr GLUtesselator -> m ()
gluTessBeginContour v1 = liftIO $ dyn_gluTessBeginContour ptr_gluTessBeginContour v1

foreign import CALLCONV "dynamic" dyn_gluTessBeginContour
  :: FunPtr (Ptr GLUtesselator -> IO ())
  ->         Ptr GLUtesselator -> IO ()

{-# NOINLINE ptr_gluTessBeginContour #-}
ptr_gluTessBeginContour :: FunPtr a
ptr_gluTessBeginContour = unsafePerformIO $ getAPIEntry "gluTessBeginContour"

-- gluTessBeginPolygon ----------------------------------------------------------------------

gluTessBeginPolygon :: MonadIO m => Ptr GLUtesselator -> Ptr a -> m ()
gluTessBeginPolygon v1 v2 = liftIO $ dyn_gluTessBeginPolygon ptr_gluTessBeginPolygon v1 v2

foreign import CALLCONV "dynamic" dyn_gluTessBeginPolygon
  :: FunPtr (Ptr GLUtesselator -> Ptr a -> IO ())
  ->         Ptr GLUtesselator -> Ptr a -> IO ()

{-# NOINLINE ptr_gluTessBeginPolygon #-}
ptr_gluTessBeginPolygon :: FunPtr a
ptr_gluTessBeginPolygon = unsafePerformIO $ getAPIEntry "gluTessBeginPolygon"

-- gluTessCallback ----------------------------------------------------------------------

gluTessCallback :: MonadIO m => Ptr GLUtesselator -> GLenum -> FunPtr a -> m ()
gluTessCallback v1 v2 v3 = liftIO $ dyn_gluTessCallback ptr_gluTessCallback v1 v2 v3

foreign import CALLCONV "dynamic" dyn_gluTessCallback
  :: FunPtr (Ptr GLUtesselator -> GLenum -> FunPtr a -> IO ())
  ->         Ptr GLUtesselator -> GLenum -> FunPtr a -> IO ()

{-# NOINLINE ptr_gluTessCallback #-}
ptr_gluTessCallback :: FunPtr a
ptr_gluTessCallback = unsafePerformIO $ getAPIEntry "gluTessCallback"

-- gluTessEndContour ----------------------------------------------------------------------

gluTessEndContour :: MonadIO m => Ptr GLUtesselator -> m ()
gluTessEndContour v1 = liftIO $ dyn_gluTessEndContour ptr_gluTessEndContour v1

foreign import CALLCONV "dynamic" dyn_gluTessEndContour
  :: FunPtr (Ptr GLUtesselator -> IO ())
  ->         Ptr GLUtesselator -> IO ()

{-# NOINLINE ptr_gluTessEndContour #-}
ptr_gluTessEndContour :: FunPtr a
ptr_gluTessEndContour = unsafePerformIO $ getAPIEntry "gluTessEndContour"

-- gluTessEndPolygon ----------------------------------------------------------------------

gluTessEndPolygon :: MonadIO m => Ptr GLUtesselator -> m ()
gluTessEndPolygon v1 = liftIO $ dyn_gluTessEndPolygon ptr_gluTessEndPolygon v1

foreign import CALLCONV "dynamic" dyn_gluTessEndPolygon
  :: FunPtr (Ptr GLUtesselator -> IO ())
  ->         Ptr GLUtesselator -> IO ()

{-# NOINLINE ptr_gluTessEndPolygon #-}
ptr_gluTessEndPolygon :: FunPtr a
ptr_gluTessEndPolygon = unsafePerformIO $ getAPIEntry "gluTessEndPolygon"

-- gluTessNormal ----------------------------------------------------------------------

gluTessNormal :: MonadIO m => Ptr GLUtesselator -> GLdouble -> GLdouble -> GLdouble -> m ()
gluTessNormal v1 v2 v3 v4 = liftIO $ dyn_gluTessNormal ptr_gluTessNormal v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_gluTessNormal
  :: FunPtr (Ptr GLUtesselator -> GLdouble -> GLdouble -> GLdouble -> IO ())
  ->         Ptr GLUtesselator -> GLdouble -> GLdouble -> GLdouble -> IO ()

{-# NOINLINE ptr_gluTessNormal #-}
ptr_gluTessNormal :: FunPtr a
ptr_gluTessNormal = unsafePerformIO $ getAPIEntry "gluTessNormal"

-- gluTessProperty ----------------------------------------------------------------------

gluTessProperty :: MonadIO m => Ptr GLUtesselator -> GLenum -> GLdouble -> m ()
gluTessProperty v1 v2 v3 = liftIO $ dyn_gluTessProperty ptr_gluTessProperty v1 v2 v3

foreign import CALLCONV "dynamic" dyn_gluTessProperty
  :: FunPtr (Ptr GLUtesselator -> GLenum -> GLdouble -> IO ())
  ->         Ptr GLUtesselator -> GLenum -> GLdouble -> IO ()

{-# NOINLINE ptr_gluTessProperty #-}
ptr_gluTessProperty :: FunPtr a
ptr_gluTessProperty = unsafePerformIO $ getAPIEntry "gluTessProperty"

-- gluTessVertex ----------------------------------------------------------------------

gluTessVertex :: MonadIO m => Ptr GLUtesselator -> Ptr GLdouble -> Ptr a -> m ()
gluTessVertex v1 v2 v3 = liftIO $ dyn_gluTessVertex ptr_gluTessVertex v1 v2 v3

foreign import CALLCONV "dynamic" dyn_gluTessVertex
  :: FunPtr (Ptr GLUtesselator -> Ptr GLdouble -> Ptr a -> IO ())
  ->         Ptr GLUtesselator -> Ptr GLdouble -> Ptr a -> IO ()

{-# NOINLINE ptr_gluTessVertex #-}
ptr_gluTessVertex :: FunPtr a
ptr_gluTessVertex = unsafePerformIO $ getAPIEntry "gluTessVertex"

-- gluUnProject ----------------------------------------------------------------------

gluUnProject :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> m GLint
gluUnProject v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn_gluUnProject ptr_gluUnProject v1 v2 v3 v4 v5 v6 v7 v8 v9

foreign import CALLCONV "dynamic" dyn_gluUnProject
  :: FunPtr (GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint)
  ->         GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

{-# NOINLINE ptr_gluUnProject #-}
ptr_gluUnProject :: FunPtr a
ptr_gluUnProject = unsafePerformIO $ getAPIEntry "gluUnProject"

-- gluUnProject4 ----------------------------------------------------------------------

gluUnProject4 :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> GLclampd -> GLclampd -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> m GLint
gluUnProject4 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn_gluUnProject4 ptr_gluUnProject4 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

foreign import CALLCONV "dynamic" dyn_gluUnProject4
  :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> GLclampd -> GLclampd -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint)
  ->         GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> GLclampd -> GLclampd -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

{-# NOINLINE ptr_gluUnProject4 #-}
ptr_gluUnProject4 :: FunPtr a
ptr_gluUnProject4 = unsafePerformIO $ getAPIEntry "gluUnProject4"
