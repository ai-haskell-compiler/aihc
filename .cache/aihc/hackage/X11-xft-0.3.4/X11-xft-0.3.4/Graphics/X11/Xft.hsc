{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- Module      :  Graphics.X11.Xft
-- Copyright   :  Clemens Fruhwirth <clemens@endorphin.org> 2007
--
-- Haskell bindings for the Xft library.
--
-----------------------------------------------------------------------------

module Graphics.X11.Xft ( XftColor
                        , xftcolor_pixel
                        , allocaXftColor
                        , withXftColorName
                        , withXftColorValue
                        , XftDraw
                        , withXftDraw
                        , xftDrawCreate
                        , xftDrawCreateBitmap
                        , xftDrawCreateAlpha
                        , xftDrawChange
                        , xftDrawDisplay
                        , xftDrawDrawable
                        , xftDrawColormap
                        , xftDrawVisual
                        , xftDrawDestroy
                        , XftFont
                        , xftfont_ascent
                        , xftfont_max_ascent
                        , xftfont_descent
                        , xftfont_max_descent
                        , xftfont_height
                        , xftfont_max_height
                        , xftfont_max_advance_width
                        , xftFontOpen
                        , xftFontOpenXlfd
                        , xftLockFace
                        , xftUnlockFace
                        , xftFontCopy
                        , xftFontClose
                        , xftDrawGlyphs
                        , xftDrawString
                        , xftDrawStringFallback
                        , xftTextExtents
                        , xftTextAccumExtents
                        , xftDrawRect
                        , xftDrawSetClipRectangles
                        , xftDrawSetSubwindowMode
                        , xftInitFtLibrary
                        )
 where

import Graphics.X11
import Graphics.X11.Xlib.Types
import Graphics.X11.Xrender

import Codec.Binary.UTF8.String as UTF8
import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.Char (ord)
import Data.Function (on)
import Data.List (groupBy, foldl')
import Data.List.NonEmpty (NonEmpty)
import Foreign hiding (void)
import Foreign.C.String
import Foreign.C.Types

#include <X11/Xft/Xft.h>

-----------------------
-- Color Handling    --
-----------------------

newtype XftColor = XftColor (Ptr XftColor)

xftcolor_pixel :: XftColor -> IO Int
xftcolor_pixel (XftColor p) = peekCUShort p #{offset XftColor, pixel}
-- missing xftcolor_color to get XRenderColor

foreign import ccall "XftColorAllocName"
    cXftColorAllocName :: Display -> Visual -> Colormap -> CString -> XftColor -> IO (#type Bool)

allocaXftColor :: (Ptr XftColor -> IO a) -> IO a
allocaXftColor = allocaBytes (#size XftColor)

withXftColorName :: Display -> Visual -> Colormap -> String -> (XftColor -> IO a) -> IO a
withXftColorName d v cm name f =
    allocaXftColor $ (\color -> do
                        withCAString name (\cstring -> do
                                             void $ cXftColorAllocName d v cm cstring color
                                             r <- f color
                                             cXftColorFree d v cm color
                                             return r)) . XftColor

foreign import ccall "XftColorAllocValue"
  cXftColorAllocValue :: Display -> Visual -> Colormap -> (Ptr XRenderColor) -> XftColor -> IO (#type Bool)

withXftColorValue :: Display -> Visual -> Colormap -> XRenderColor -> (XftColor -> IO a) -> IO a
withXftColorValue d v cm rc f =
    allocaXftColor $ (\color -> do
                        with rc (\rc_ptr -> do
                                   void $ cXftColorAllocValue d v cm rc_ptr color
                                   r <- f color
                                   cXftColorFree d v cm color
                                   return r)) . XftColor

foreign import ccall "XftColorFree"
  cXftColorFree :: Display -> Visual -> Colormap -> XftColor -> IO ()

-----------------------
-- Draw Handling    --
-----------------------

newtype XftDraw = XftDraw (Ptr XftDraw)

withXftDraw :: Display -> Drawable -> Visual -> Colormap -> (XftDraw -> IO a) -> IO a
withXftDraw d p v c act =
    do
      draw <- xftDrawCreate d p v c
      a <- act draw
      xftDrawDestroy draw
      return a

foreign import ccall "XftDrawCreate"
  xftDrawCreate :: Display -> Drawable -> Visual -> Colormap -> IO XftDraw

foreign import ccall "XftDrawCreateBitmap"
  xftDrawCreateBitmap :: Display -> Pixmap -> IO XftDraw

foreign import ccall "XftDrawCreateAlpha"
  cXftDrawCreateAlpha :: Display -> Pixmap -> CInt -> IO XftDraw

xftDrawCreateAlpha :: Integral a => Display -> Pixmap -> a -> IO XftDraw
xftDrawCreateAlpha d p i = cXftDrawCreateAlpha d p (fi i)

foreign import ccall "XftDrawChange"
  xftDrawChange :: XftDraw -> Drawable -> IO ()

foreign import ccall "XftDrawDisplay"
  xftDrawDisplay :: XftDraw -> IO Display -- FIXME correct? Is X11 giving us the underlying Display?

foreign import ccall "XftDrawDrawable"
  xftDrawDrawable :: XftDraw -> IO Drawable

foreign import ccall "XftDrawColormap"
  xftDrawColormap :: XftDraw -> IO Colormap

foreign import ccall "XftDrawVisual"
  xftDrawVisual :: XftDraw -> IO Visual

foreign import ccall "XftDrawDestroy"
  xftDrawDestroy :: XftDraw -> IO ()

--------------------
-- Font handling  --
--------------------

newtype XftFont = XftFont (Ptr XftFont)

xftfont_ascent, xftfont_descent, xftfont_height, xftfont_max_advance_width :: XftFont -> IO Int
xftfont_ascent (XftFont p)            = peekCUShort p #{offset XftFont, ascent}
xftfont_descent (XftFont p)           = peekCUShort p #{offset XftFont, descent}
xftfont_height (XftFont p)            = peekCUShort p #{offset XftFont, height}
xftfont_max_advance_width (XftFont p) = peekCUShort p #{offset XftFont, max_advance_width}
-- missing xftfont_charset
-- missing xftfont_pattern

foreign import ccall "XftFontOpenName"
  cXftFontOpen :: Display -> CInt -> CString -> IO XftFont

xftFontOpen :: Display -> Screen -> String -> IO XftFont
xftFontOpen dpy screen fontname =
    withCAString fontname $
      \cfontname -> cXftFontOpen dpy (fi (screenNumberOfScreen screen)) cfontname

foreign import ccall "XftFontOpenXlfd"
  cXftFontOpenXlfd :: Display -> CInt -> CString -> IO XftFont

xftFontOpenXlfd :: Display -> Screen -> String -> IO XftFont
xftFontOpenXlfd dpy screen fontname =
    withCAString fontname $ \cfontname -> cXftFontOpenXlfd dpy (fi (screenNumberOfScreen screen)) cfontname

foreign import ccall "XftLockFace"
  xftLockFace :: XftFont -> IO ()                  -- FIXME XftLockFace returns FT_face not void

foreign import ccall "XftUnlockFace"
  xftUnlockFace :: XftFont -> IO ()

foreign import ccall "XftFontCopy"
  xftFontCopy :: Display -> XftFont -> IO XftFont

foreign import ccall "XftFontClose"
  xftFontClose :: Display -> XftFont -> IO ()

-- Support for multiple fonts --

xftfont_max_ascent :: NonEmpty XftFont -> IO Int
xftfont_max_ascent = fmap maximum . mapM xftfont_ascent

xftfont_max_descent :: NonEmpty XftFont -> IO Int
xftfont_max_descent = fmap maximum . mapM xftfont_descent

xftfont_max_height :: NonEmpty XftFont -> IO Int
xftfont_max_height = fmap maximum . mapM xftfont_height

---------------------
-- Painting
---------------------

-- Drawing strings or glyphs --

foreign import ccall "XftCharExists"
  cXftCharExists :: Display -> XftFont -> (#type FcChar32) -> IO (#type FcBool)

xftCharExists :: Display -> XftFont -> Char -> IO Bool
xftCharExists d f c = bool <$> cXftCharExists d f (fi $ ord c)
  where
    bool 0 = False
    bool _ = True

foreign import ccall "XftDrawGlyphs"
  cXftDrawGlyphs :: XftDraw -> XftColor -> XftFont -> CInt -> CInt -> Ptr (#type FT_UInt) -> CInt -> IO ()

xftDrawGlyphs :: (Integral a, Integral b, Integral c)
              => XftDraw -> XftColor -> XftFont -> b -> c -> [a] -> IO ()
xftDrawGlyphs d c f x y glyphs =
    withArrayLen (map fi glyphs)
      (\len ptr -> cXftDrawGlyphs d c f (fi x) (fi y) ptr (fi len))

foreign import ccall "XftDrawStringUtf8"
  cXftDrawStringUtf8 :: XftDraw -> XftColor -> XftFont -> CInt -> CInt -> Ptr (#type FcChar8) -> CInt -> IO ()

xftDrawString :: (Integral a, Integral b)
              => XftDraw -> XftColor -> XftFont -> a -> b -> String -> IO ()
xftDrawString d c f x y string =
    withArrayLen (map fi (UTF8.encode string))
      (\len ptr -> cXftDrawStringUtf8 d c f (fi x) (fi y) ptr (fi len))

-- Querying text extends for strings or glyphs --

foreign import ccall "XftTextExtentsUtf8"
  cXftTextExtentsUtf8 :: Display -> XftFont -> CString -> CInt -> Ptr XGlyphInfo -> IO ()

xftTextExtents :: Display -> XftFont -> String -> IO XGlyphInfo
xftTextExtents d f string =
    withArrayLen (map fi (UTF8.encode string)) $
    \len str_ptr -> alloca $
    \cglyph -> do
      cXftTextExtentsUtf8 d f str_ptr (fi len) cglyph
      peek cglyph

-- Support for multiple fonts --

-- | Like 'xftDrawString', but fall back to another font in the given
-- list if necessary (i.e., should a character not be drawable with the
-- currently selected font).
xftDrawStringFallback :: XftDraw -> XftColor -> [XftFont] -> Int -> Int -> String -> IO ()
xftDrawStringFallback d c fs x y string = do
    display <- xftDrawDisplay d
    chunks <- getChunks display fs x y string
    mapM_ (\(f, s, (XGlyphInfo _  _ x' y' _ _)) -> xftDrawString d c f x' y' s) chunks

-- | Like 'xftTextExtents' but for multiple fonts. Return
-- accumulative extents using appropriate fonts for each part of
-- string.
xftTextAccumExtents :: Display -> [XftFont] -> String -> IO XGlyphInfo
xftTextAccumExtents disp fts string = do
  chunks <- map (\ (_, _, gi) -> gi) <$> getChunks disp fts 0 0 string
  return $ foldl' calcExtents (XGlyphInfo 0 0 0 0 0 0) chunks
  where
    calcExtents :: XGlyphInfo -> XGlyphInfo -> XGlyphInfo
    calcExtents (XGlyphInfo _ _ x y xo yo) (XGlyphInfo w' h' _ _ xo' yo')
      = XGlyphInfo (xo + w') (yo + h') x y (xo + xo') (yo + yo')

-- | Split string and determine fonts/offsets for individual parts
getChunks :: Display
          -> [XftFont]
          -> Int
          -> Int
          -> String
          -> IO [(XftFont, String, XGlyphInfo)]
getChunks disp fts xInit yInit str = do
    chunks <- getFonts fts str
    getChunksExtents xInit yInit chunks
  where
    -- Split string and determine fonts for individual parts
    getFonts :: [XftFont] -> String -> IO [(XftFont, String)]
    getFonts [] _ = return []
    getFonts [ft] s = return [(ft, s)]
    getFonts fonts@(ft:_) s = do
        -- Determine which glyph can be rendered by current font
        glyphs <- mapM (xftCharExists disp ft) s
        -- Split string into parts that return "can/cannot be rendered"
        let splits = map (fst . head &&& map snd)
                   . groupBy ((==) `on` fst)
                   $ zip glyphs s
        -- Determine which font to render each chunk with
        concat <$> mapM (getFont fonts) splits

    -- Determine fonts for substrings
    getFont :: [XftFont] -> (Bool, String) -> IO [(XftFont, String)]
    getFont [] _ = return []
    getFont [ft] (_, s) = return [(ft, s)]      -- Last font, use it
    getFont (ft:_) (True, s) = return [(ft, s)] -- Current font can render this substring
    getFont (_:fs) (False, s) = getFonts fs s   -- Fallback to next font

    -- Determine coordinates for chunks using extents
    getChunksExtents :: Int -> Int -> [(XftFont, String)] -> IO [(XftFont, String, XGlyphInfo)]
    getChunksExtents _ _ [] = return []
    getChunksExtents x y ((f, s) : chunks) = do
      (XGlyphInfo w h _ _ xo yo) <- xftTextExtents disp f s
      rest <- getChunksExtents (x + xo) (y + yo) chunks
      return $ (f, s, XGlyphInfo w h x y xo yo) : rest

-- Drawing auxilary --

foreign import ccall "XftDrawRect"
  cXftDrawRect :: XftDraw -> XftColor -> CInt -> CInt -> CUInt -> CUInt -> IO ()

xftDrawRect :: (Integral a, Integral b, Integral c, Integral d)
            => XftDraw -> XftColor -> a -> b -> c -> d -> IO ()
xftDrawRect draw color x y width height =
    cXftDrawRect draw color (fi x) (fi y) (fi width) (fi height)

foreign import ccall "XftDrawSetClip"
    cXftDrawSetClip :: XftDraw -> Ptr Region -> IO (#type Bool)

--xftDrawSetClip d (Region r) =
--    do
--      rv <- cXftDrawSetClip d r
--      return $ (fi rv) /= 0

foreign import ccall "XftDrawSetClipRectangles"
  cXftDrawSetClipRectangles :: XftDraw -> CInt -> CInt -> (Ptr Rectangle) -> CInt -> IO CInt

xftDrawSetClipRectangles :: XftDraw -> Int -> Int -> [Rectangle] -> IO Bool
xftDrawSetClipRectangles draw x y rectangles =
    withArrayLen rectangles
      (\len rects -> do
         r <- cXftDrawSetClipRectangles draw (fi x) (fi y) rects (fi len)
         return (toInteger r /= 0)) -- verify whether this is really the convention

foreign import ccall "XftDrawSetSubwindowMode"
  cXftDrawSetSubwindowMode :: XftDraw -> CInt -> IO ()

xftDrawSetSubwindowMode :: Integral a => XftDraw -> a -> IO ()
xftDrawSetSubwindowMode d i = cXftDrawSetSubwindowMode d (fi i)

--------------
-- Auxillary
--------------

foreign import ccall "XftInitFtLibrary"
  xftInitFtLibrary :: IO ()

{-
These functions minimize round-trip between the library and the using program (maybe also to the X server?)
but otherwise all the functions can be achieved by DrawGlyphs

void
XftDrawCharSpec (XftDraw                *draw,
                 _Xconst XftColor       *color,
                 XftFont                *pub,
                 _Xconst XftCharSpec    *chars,
                 int                    len);

void
XftDrawCharFontSpec (XftDraw                    *draw,
                     _Xconst XftColor           *color,
                     _Xconst XftCharFontSpec    *chars,
                     int                        len);

void
XftDrawGlyphSpec (XftDraw               *draw,
                  _Xconst XftColor      *color,
                  XftFont               *pub,
                  _Xconst XftGlyphSpec  *glyphs,
                  int                   len);

void
XftDrawGlyphFontSpec (XftDraw                   *draw,
                      _Xconst XftColor          *color,
                      _Xconst XftGlyphFontSpec  *glyphs,
                      int                       len);
------
Missing
void
XftGlyphExtents (Display            *dpy,
                 XftFont            *pub,
                 _Xconst FT_UInt    *glyphs,
                 int                nglyphs,
                 XGlyphInfo         *extents);

Intentionally Missing Bindings
xftDrawString8,xftDrawString16,xftDrawString32,xftDrawStringUtf16


--foreign import ccall "XftDrawSetClip"
-- cXftDrawSetClip :: XftDraw -> Ptr (??) Region -> IO (#type Bool)


Missing Bindings because of missing Freetype bindings

/* xftfreetype.c */

XftFontInfo *
XftFontInfoCreate (Display *dpy, _Xconst FcPattern *pattern);

void
XftFontInfoDestroy (Display *dpy, XftFontInfo *fi);

FcChar32
XftFontInfoHash (_Xconst XftFontInfo *fi);

FcBool
XftFontInfoEqual (_Xconst XftFontInfo *a, _Xconst XftFontInfo *b);

XftFont *
XftFontOpenInfo (Display        *dpy,
                 FcPattern      *pattern,
                 XftFontInfo    *fi);

XftFont *
XftFontOpenPattern (Display *dpy, FcPattern *pattern);

-- no Render bindings yet
--foreign import ccall "XftDrawPicture"
--  cXftDrawPicture :: XftDraw -> IO Picture
--foreign import ccall "XftDrawPicture"
--  cXftDrawSrcPicture :: XftDraw -> XftColor -> IO Picture
-}

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
