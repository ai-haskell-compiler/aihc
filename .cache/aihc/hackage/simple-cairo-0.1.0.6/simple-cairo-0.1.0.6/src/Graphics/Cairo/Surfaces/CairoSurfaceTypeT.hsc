{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceTypeT (
	CairoSurfaceTypeT(..),
	module Graphics.Cairo.Surfaces.CairoSurfaceTypeT
	) where

import Graphics.Cairo.Surfaces.CairoSurfaceTypeT.Template

#include <cairo.h>

mkMember "CairoSurfaceTypeImage" #{const CAIRO_SURFACE_TYPE_IMAGE}
mkMember "CairoSurfaceTypePdf" #{const CAIRO_SURFACE_TYPE_PDF}
mkMember "CairoSurfaceTypePs" #{const CAIRO_SURFACE_TYPE_PS}
mkMember "CairoSurfaceTypeXlib" #{const CAIRO_SURFACE_TYPE_XLIB}
mkMember "CairoSurfaceTypeXcb" #{const CAIRO_SURFACE_TYPE_XCB}
mkMember "CairoSurfaceTypeGlitz" #{const CAIRO_SURFACE_TYPE_GLITZ}
mkMember "CairoSurfaceTypeQuartz" #{const CAIRO_SURFACE_TYPE_QUARTZ}
mkMember "CairoSurfaceTypeWin32" #{const CAIRO_SURFACE_TYPE_WIN32}
mkMember "CairoSurfaceTypeBeos" #{const CAIRO_SURFACE_TYPE_DIRECTFB}
mkMember "CairoSurfaceTypeSvg" #{const CAIRO_SURFACE_TYPE_SVG}
mkMember "CairoSurfaceTypeOs2" #{const CAIRO_SURFACE_TYPE_OS2}
mkMember "CairoSurfaceTypeWin32Printing"
	#{const CAIRO_SURFACE_TYPE_WIN32_PRINTING}
mkMember "CairoSurfaceTypeQuartzImage" #{const CAIRO_SURFACE_TYPE_QUARTZ_IMAGE}
mkMember "CairoSurfaceTypeScript" #{const CAIRO_SURFACE_TYPE_SCRIPT}
mkMember "CairoSurfaceTypeQt" #{const CAIRO_SURFACE_TYPE_QT}
mkMember "CairoSurfaceTypeRecording" #{const CAIRO_SURFACE_TYPE_RECORDING}
mkMember "CairoSurfaceTypeVg" #{const CAIRO_SURFACE_TYPE_VG}
mkMember "CairoSurfaceTypeGl" #{const CAIRO_SURFACE_TYPE_GL}
mkMember "CairoSurfaceTypeDrm" #{const CAIRO_SURFACE_TYPE_DRM}
mkMember "CairoSurfaceTypeTee" #{const CAIRO_SURFACE_TYPE_TEE}
mkMember "CairoSurfaceTypeXml" #{const CAIRO_SURFACE_TYPE_XML}
mkMember "CairoSurafceTypeSkia" #{const CAIRO_SURFACE_TYPE_SKIA}
mkMember "CairoSurfaceTypeSubsurface" #{const CAIRO_SURFACE_TYPE_SUBSURFACE}
mkMember "CairoSurfaceTypeCogl" #{const CAIRO_SURFACE_TYPE_COGL}
