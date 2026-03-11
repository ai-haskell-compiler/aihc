-----------------------------------------------------------------------------
-- Module      :  Graphics.X11.Xrender
-- Copyright   :  Clemens Fruhwirth <clemens@endorphin.org> 2007
--
-- Haskell bindings for the Xrender extension.
--
-----------------------------------------------------------------------------

module Graphics.X11.Xrender (
  peekCUShort,
  pokeCUShort,
  peekCShort,
  pokeCShort,
  XRenderColor (..),
  XGlyphInfo (..),
  XRenderDirectFormat (..),
) where

import Foreign
import Foreign.C

#include <X11/extensions/Xrender.h>

peekCUShort :: Ptr a -> CInt -> IO Int
peekCUShort ptr off = do
        v <- peekByteOff ptr (fromIntegral off)
        return (fromIntegral (v::CUShort))

pokeCUShort :: Ptr a -> CInt -> Int -> IO ()
pokeCUShort ptr off v =
        pokeByteOff ptr (fromIntegral off) (fromIntegral v::CUShort)

peekCShort :: Ptr a -> CInt -> IO Int
peekCShort ptr off = do
        v <- peekByteOff ptr (fromIntegral off)
        return (fromIntegral (v::CShort))

pokeCShort :: Ptr a -> CInt -> Int -> IO ()
pokeCShort ptr off v =
        pokeByteOff ptr (fromIntegral off) (fromIntegral v::CShort)

data XRenderColor = XRenderColor {
      xrendercolor_red   :: Int,
      xrendercolor_green :: Int,
      xrendercolor_blue  :: Int,
      xrendercolor_alpha :: Int
}

instance Storable XRenderColor where
        sizeOf _ = #{size XRenderColor}
        alignment _ = alignment (undefined::CInt)
        peek p = do
                red   <- peekCUShort p #{offset XRenderColor, red}
                blue  <- peekCUShort p #{offset XRenderColor, blue}
                green <- peekCUShort p #{offset XRenderColor, green}
                alpha <- peekCUShort p #{offset XRenderColor, alpha}
                return (XRenderColor red green blue alpha)
        poke p (XRenderColor red green blue alpha) = do
                pokeCUShort p #{offset XRenderColor,red} red
                pokeCUShort p #{offset XRenderColor,blue} blue
                pokeCUShort p #{offset XRenderColor,green} green
                pokeCUShort p #{offset XRenderColor,alpha} alpha

data XGlyphInfo = XGlyphInfo {
      xglyphinfo_width  :: Int,
      xglyphinfo_height :: Int,
      xglyphinfo_x      :: Int,
      xglyphinfo_y      :: Int,
      xglyphinfo_xOff   :: Int,
      xglyphinfo_yOff   :: Int
}

instance Storable XGlyphInfo where
        sizeOf _ = #{size XGlyphInfo}
        alignment _ = alignment (undefined::CInt)
        peek p = do
                width  <- peekCUShort p #{offset XGlyphInfo, width}
                height <- peekCUShort p #{offset XGlyphInfo, height}
                x <- peekCShort p #{offset XGlyphInfo, x}
                y <- peekCShort p #{offset XGlyphInfo, y}
                xOff <- peekCShort p #{offset XGlyphInfo, xOff}
                yOff <- peekCShort p #{offset XGlyphInfo, yOff}
                return (XGlyphInfo width height x y xOff yOff)
        poke p (XGlyphInfo width height x y xOff yOff) = do
                pokeCUShort p #{offset XGlyphInfo,width} width
                pokeCUShort p #{offset XGlyphInfo,height} height
                pokeCShort p #{offset XGlyphInfo,x} x
                pokeCShort p #{offset XGlyphInfo,y} y
                pokeCShort p #{offset XGlyphInfo,xOff} xOff
                pokeCShort p #{offset XGlyphInfo,yOff} yOff


data XRenderDirectFormat = XRenderDirectFormat {
     xrenderdirectformat_red       :: Int,
     xrenderdirectformat_redMask   :: Int,
     xrenderdirectformat_green     :: Int,
     xrenderdirectformat_greenMask :: Int,
     xrenderdirectformat_blue      :: Int,
     xrenderdirectformat_blueMask  :: Int,
     xrenderdirectformat_alpha     :: Int,
     xrenderdirectformat_alphaMask :: Int
}

instance Storable XRenderDirectFormat where
        sizeOf _ = #{size XRenderDirectFormat}
        alignment _ = alignment (undefined::CInt)
        peek p = do
                red  <- peekCShort p #{offset XRenderDirectFormat, red}
                redMask  <- peekCShort p #{offset XRenderDirectFormat, redMask}
                green  <- peekCShort p #{offset XRenderDirectFormat, green}
                greenMask  <- peekCShort p #{offset XRenderDirectFormat, greenMask}
                blue  <- peekCShort p #{offset XRenderDirectFormat, blue}
                blueMask  <- peekCShort p #{offset XRenderDirectFormat, blueMask}
                alpha  <- peekCShort p #{offset XRenderDirectFormat, alpha}
                alphaMask  <- peekCShort p #{offset XRenderDirectFormat, alphaMask}
                return (XRenderDirectFormat red redMask green greenMask blue blueMask alpha alphaMask)
        poke p (XRenderDirectFormat red redMask green greenMask blue blueMask alpha alphaMask) = do
                pokeCShort p #{offset XRenderDirectFormat,red} red
                pokeCShort p #{offset XRenderDirectFormat,redMask} redMask
                pokeCShort p #{offset XRenderDirectFormat,blue} blue
                pokeCShort p #{offset XRenderDirectFormat,blueMask} blueMask
                pokeCShort p #{offset XRenderDirectFormat,green} green
                pokeCShort p #{offset XRenderDirectFormat,greenMask} greenMask
                pokeCShort p #{offset XRenderDirectFormat,alpha} alpha
                pokeCShort p #{offset XRenderDirectFormat,alphaMask} alphaMask
