module Graphics.GD.Internal where

import           Control.Exception (bracket)
import           Control.Monad     (liftM, unless)
import           Data.Bits
import           Foreign           (Ptr,FunPtr,ForeignPtr)
import           Foreign           (peek,peekByteOff)
import qualified Foreign           as F
import           Foreign.C         (CDouble,CInt,CString)
import qualified Foreign.C         as C

data CFILE = CFILE

foreign import ccall "stdio.h fopen" c_fopen
    :: CString -> CString -> IO (Ptr CFILE)
foreign import ccall "stdio.h fclose" c_fclose
    :: Ptr CFILE -> IO CInt

fopen :: FilePath -> String -> IO (Ptr CFILE)
fopen file mode = 
    C.throwErrnoIfNull file $ C.withCString file $ 
                         \f -> C.withCString mode $ \m -> c_fopen f m

fclose :: Ptr CFILE -> IO ()
fclose p = C.throwErrnoIf_ (== #{const EOF}) "fclose" $ c_fclose p

withCFILE :: FilePath -> String -> (Ptr CFILE -> IO a) -> IO a
withCFILE file mode = bracket (fopen file mode) fclose

#include <gd.h>
#include "gd-extras.h"

data GDImage = GDImage

-- JPEG format

foreign import ccall "gd.h gdImageCreateFromJpeg" gdImageCreateFromJpeg
    :: Ptr CFILE -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageCreateFromJpegPtr" gdImageCreateFromJpegPtr
    :: CInt -> Ptr a -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageJpeg" gdImageJpeg
    :: Ptr GDImage -> Ptr CFILE -> CInt -> IO ()

foreign import ccall "gd.h gdImageJpegPtr" gdImageJpegPtr
    :: Ptr GDImage -> Ptr CInt -> CInt -> IO (Ptr a)

-- PNG format

foreign import ccall "gd.h gdImageCreateFromPng" gdImageCreateFromPng
    :: Ptr CFILE -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageCreateFromPngPtr" gdImageCreateFromPngPtr
    :: CInt -> Ptr a -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImagePng" gdImagePng
    :: Ptr GDImage -> Ptr CFILE -> IO ()

foreign import ccall "gd.h gdImagePngPtr" gdImagePngPtr
    :: Ptr GDImage -> Ptr CInt -> IO (Ptr a)

-- GIF format

foreign import ccall "gd.h gdImageCreateFromGif" gdImageCreateFromGif
    :: Ptr CFILE -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageCreateFromGifPtr" gdImageCreateFromGifPtr
    :: CInt -> Ptr a -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageGif" gdImageGif
    :: Ptr GDImage -> Ptr CFILE -> IO ()

foreign import ccall "gd.h gdImageGifPtr" gdImageGifPtr
    :: Ptr GDImage -> Ptr CInt -> IO (Ptr a)

-- Creating and destroying images

foreign import ccall "gd.h gdImageCreateTrueColor" gdImageCreateTrueColor 
    :: CInt -> CInt -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageDestroy" gdImageDestroy
    :: Ptr GDImage -> IO ()

foreign import ccall "gd-extras.h &gdImagePtrDestroyIfNotNull"
    ptr_gdImagePtrDestroyIfNotNull 
    :: FunPtr (Ptr (Ptr GDImage) -> IO ())


-- Copying image parts

foreign import ccall "gd.h gdImageCopy" gdImageCopy
    :: Ptr GDImage -> Ptr GDImage 
    -> CInt -> CInt -> CInt -> CInt
    -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageCopyResampled" gdImageCopyResampled
    :: Ptr GDImage -> Ptr GDImage 
    -> CInt -> CInt -> CInt -> CInt
    -> CInt -> CInt -> CInt -> CInt -> IO ()

{-
foreign import ccall "gd.h gdImageCopyRotated" gdImageCopyRotated
    :: Ptr GDImage -> Ptr GDImage 
    -> CDouble -> CDouble -> CInt -> CInt
    -> CInt -> CInt -> CInt -> IO ()
-}

foreign import ccall "gd-extras.h gdImageCopyRotated90" gdImageCopyRotated90
    :: Ptr GDImage -> Ptr GDImage 
    -> CInt -> CInt -> CInt -> CInt
    -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageGetPixel" gdImageGetPixel
    :: Ptr GDImage -> CInt -> CInt -> IO CInt

-- Drawing functions

foreign import ccall "gd.h gdImageFilledRectangle" gdImageFilledRectangle
    :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageFilledEllipse" gdImageFilledEllipse
    :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageLine" gdImageLine
    :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageArc" gdImageArc
    :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt
    -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageSetAntiAliased" gdImageSetAntiAliased
    :: Ptr GDImage -> CInt -> IO ()

foreign import ccall "gd.h gdImageSetPixel" gdImageSetPixel
    :: Ptr GDImage -> CInt -> CInt -> CInt -> IO ()

-- Text functions

foreign import ccall "gd.h gdFTUseFontConfig" gdFTUseFontConfig
    :: CInt -> IO CInt

foreign import ccall "gd.h gdImageStringFT" gdImageStringFT
    :: Ptr GDImage -> Ptr CInt -> CInt -> CString -> CDouble -> CDouble -> CInt
       -> CInt -> CString -> IO CString

foreign import ccall "gd.h gdImageStringFTCircle" gdImageStringFTCircle
    :: Ptr GDImage -> CInt -> CInt -> CDouble -> CDouble -> CDouble -> CString
       -> CDouble -> CString -> CString -> CInt -> IO CString

-- Miscellaneous functions

foreign import ccall "gd.h &gdFree" gdFree
    :: FunPtr (Ptr a -> IO ())

-- We use a second level of indirection to allow storing a null pointer
-- when the image has already been freed. This allows 'withImage' to 
-- free the @gdImage@ early.
newtype Image = Image (ForeignPtr (Ptr GDImage))

type Size = (Int,Int)

type Point = (Int,Int)

type Color = CInt

mkImage :: Ptr GDImage -> IO Image
mkImage img = do fp <- F.mallocForeignPtr
                 F.withForeignPtr fp $ \p -> F.poke p img
                 F.addForeignPtrFinalizer ptr_gdImagePtrDestroyIfNotNull fp
                 return $ Image fp

-- | Creates an image, performs an operation on the image, and
-- frees it.
-- This function allows block scoped management of 'Image'
-- objects.  If you are handling large images, the delay before
-- the finalizer which frees the image runs may cause significant
-- temporary extra memory use.  Use this function to force the
-- image to be freed as soons as you are done with it.  Note that
-- it is unsafe to hold on to the 'Image' after the function is
-- done.
withImage :: IO Image        -- ^ Image creation action.
          -> (Image -> IO b) -- ^ Some operation on the image. The result should
                             -- not reference the 'Image'.
          -> IO b
withImage ini f = bracket ini freeImage f

-- | Overwrites the pointer with a null pointer, and frees the @gdImage@. 
-- Safe to call twice. Doesn't free the 'ForeignPtr', we rely on the
-- GC to do that.
freeImage :: Image -> IO ()
freeImage (Image fp) = F.withForeignPtr fp $ 
  \pp -> do p <- peek pp
            F.poke pp F.nullPtr
            unless (p == F.nullPtr) $ gdImageDestroy p

withImagePtr :: Image -> (Ptr GDImage -> IO a) -> IO a
withImagePtr (Image fp) f = F.withForeignPtr fp $
  \pp -> peek pp >>= \p ->
       if p == F.nullPtr then fail "Image has been freed." else f p

-- | Create a new empty image.
newImage :: Size -> IO Image
newImage (w,h) = newImage_ (int w) (int h)

newImage_  :: CInt -> CInt -> IO Image
newImage_ w h = do p <- F.throwIfNull "gdImageCreateTrueColor" $
                        gdImageCreateTrueColor w h
                   mkImage p

-- | Create a new empty image and apply a function to it.
onNewImage :: CInt -> CInt -> (Ptr GDImage -> IO a) -> IO Image
onNewImage w h f = newImage_ w h >>= \i -> withImagePtr i f >> return i

-- | Make a copy of an image.
copyImage :: Image -> IO Image
copyImage i = withImagePtr i f
  where f p = do (w,h) <- imageSize_ p
                 onNewImage w h (\p' -> gdImageCopy p' p 0 0 0 0 w h)

-- | Copy a region of one image into another
copyRegion :: Point    -- ^ Source upper left-hand corner
              -> Size  -- ^ Size of copied region
              -> Image -- ^ Source image
              -> Point -- ^ Destination upper left-hand corner
              -> Image -- ^ Destination image
              -> IO ()
copyRegion (srcX, srcY) (w, h) srcIPtr (dstX, dstY) dstIPtr
    = withImagePtr dstIPtr $
      \dstImg -> withImagePtr srcIPtr $
      \srcImg -> gdImageCopy dstImg srcImg (int dstX) (int dstY)
                 (int srcX) (int srcY) (int w) (int h)

-- | Copy a region of one image into another, rescaling the region 
copyRegionScaled :: Point -- ^ Source upper left-hand corner
                 -> Size  -- ^ Size of source region
                 -> Image -- ^ Source image
                 -> Point -- ^ Destination upper left-hand corner
                 -> Size  -- ^ Size of destination region
                 -> Image -- ^ Destination image
                 -> IO ()
copyRegionScaled (srcX, srcY) (srcW, srcH) srcIPtr (dstX, dstY) (dstW, dstH)
                 dstIPtr
    = withImagePtr dstIPtr $
      \dstImg -> withImagePtr srcIPtr $
      \srcImg -> gdImageCopyResampled dstImg srcImg (int dstX) (int dstY)
                 (int srcX) (int srcY) (int dstW) (int dstH)
                 (int srcW) (int srcH)

--
-- * Querying
--
                                       
-- | Retrieves the color index or the color values of a particular pixel.
getPixel :: (Int,Int) -> Image -> IO Color
getPixel (x,y) i = withImagePtr i f
    where f p' = gdImageGetPixel p' (int x) (int y)

--
-- * Getting information about images.
--

-- | Get the size of an image.
imageSize :: Image -> IO (Int,Int) -- ^ (width, height)
imageSize i = liftM f $ withImagePtr i imageSize_
    where f = (\ (w,h) -> (fromIntegral w, fromIntegral h))

imageSize_ :: Ptr GDImage -> IO (CInt,CInt)
imageSize_ p = do w <- #{peek gdImage, sx} p
                  h <- #{peek gdImage, sy} p
                  return (w, h)

--
-- * Transforming images.
--

-- | Resize an image to a give size.
resizeImage :: Int -- ^ width in pixels of output image
            -> Int -- ^ height in pixels of output image
            -> Image 
            -> IO Image
resizeImage w h i = withImagePtr i f
    where 
      f p = do let (outW,outH) = (fromIntegral w, fromIntegral h)
               (inW, inH) <- imageSize_ p
               onNewImage outW outH $ \p' -> 
                   gdImageCopyResampled p' p 0 0 0 0 outW outH inW inH

-- | Rotate an image by a multiple of 90 degrees counter-clockwise.
rotateImage :: Int -- ^ 1 for 90 degrees counter-clockwise, 
                   -- 2 for 180 degrees, etc.
            -> Image 
            -> IO Image
rotateImage r i = withImagePtr i f
    where f p = do (inW,inH) <- imageSize_ p
                   let q = fromIntegral (r `mod` 4)
                       (outW,outH) | r `mod` 2 == 0 = (inW,inH) 
                                   | otherwise      = (inH,inW)
                       srcX = if q == 1 || q == 2 then inW-1 else 0;
                       srcY = if q == 2 || q == 3 then inH-1 else 0;
                   onNewImage outW outH (\p' ->
                       gdImageCopyRotated90 p' p 0 0 srcX srcY inW inH q)

--
-- * Drawing
--

-- | Fill the entire image with the given color.
fillImage :: Color -> Image -> IO ()
fillImage c i = do sz <- imageSize i
                   drawFilledRectangle (0,0) sz c i

drawFilledRectangle :: Point -- ^ Upper left corner
                    -> Point -- ^ Lower right corner
                    -> Color -> Image -> IO ()
drawFilledRectangle (x1,y1) (x2,y2) c i =
    withImagePtr i $ \p -> 
        gdImageFilledRectangle p (int x1) (int y1) (int x2) (int y2) c

drawFilledEllipse :: Point -- ^ Center
                  -> Size  -- ^ Width and height
                  -> Color -> Image -> IO ()
drawFilledEllipse (cx,cy) (w,h) c i =
    withImagePtr i $ \p -> 
        gdImageFilledEllipse p (int cx) (int cy) (int w) (int h) c

drawLine :: Point -- ^ Start
         -> Point -- ^ End
         -> Color -> Image -> IO ()
drawLine (x1,y1) (x2,y2) c i =
    withImagePtr i $ \p ->
        gdImageLine p (int x1) (int y1) (int x2) (int y2) c

drawArc :: Point -- ^ Center
        -> Size  -- ^ Width and height
        -> Int   -- ^ Starting position (degrees)
        -> Int   -- ^ Ending position (degrees)
        -> Color -> Image -> IO ()
drawArc (cx,cy) (w,h) sp ep c i =
    withImagePtr i $ \p ->
        gdImageArc p (int cx) (int cy) (int w) (int h) (int sp) (int ep) c

-- | Use anti-aliasing when performing the given drawing function.
--   This can cause a segault with some gd versions.
antiAliased :: (Color -> Image -> IO a) -> Color -> Image -> IO a
antiAliased f c i = 
    do withImagePtr i (\p -> gdImageSetAntiAliased p c)
       f (#{const gdAntiAliased}) i

setPixel :: Point -> Color -> Image -> IO ()
setPixel (x,y) c i =
    withImagePtr i $ \p ->
        gdImageSetPixel p (int x) (int y) c

--
-- * Colors
--

rgb :: Int    -- ^ Red (0-255)
       -> Int -- ^ Green (0-255)
       -> Int -- ^ Blue (0-255)
       -> Color
rgb r g b = rgba r g b 0

rgba :: Int    -- ^ Red (0-255)
        -> Int -- ^ Green (0-255)
        -> Int -- ^ Blue (0-255)
        -> Int -- ^ Alpha (0-127), 0 is opaque, 127 is transparent
        -> Color
rgba r g b a = 
    (int a `F.shiftL` 24) .|.
    (int r `F.shiftL` 16) .|.
    (int g `F.shiftL` 8)  .|.
    int b
    
toRGBA :: Color -> (Int, Int, Int, Int) 
toRGBA c = (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)
 where
   b = c `mod` byte
   g = shiftR c 8 `mod` byte
   r = shiftR c 16 `mod` byte
   a = shiftR c 24 `mod` byte
   byte = 2 ^ (8::Int)

--
-- * Text
--

-- | Globally switch from using font file names to fontconfig paths
-- | for fonts in drawString (and measureString).
useFontConfig :: Bool -> IO Bool
useFontConfig use = liftM (/= 0) $ gdFTUseFontConfig $ if use then 1 else 0

--
-- * Utilities
--

int :: (Integral a, Num b) => a -> b
int = fromIntegral

double :: (Real a, Fractional b) => a -> b
double = realToFrac
