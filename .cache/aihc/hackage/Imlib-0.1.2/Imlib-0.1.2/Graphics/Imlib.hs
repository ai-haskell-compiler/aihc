{-# OPTIONS -fffi -fglasgow-exts #-}

{-
* Haskell Binding for Imlib 2
* Copyright (c) 2004-2008, Cale Gibbard
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of Cale Gibbard nor the
*       names of his contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY CALE GIBBARD ``AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL CALE GIBBARD BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-
New exported calls (not in Imlib 2):
withImageBits
withImage
imageWithData
colorToBits
createImageUsingList
createImageUsingArray
-}

{-
TODO: Convenience wrappers for reading in proper Haskell arrays/lists
      in the case of:
      image_get_data, image_get_data_for_reading_only,
      image_put_back_data and image_with_data

      This was really written for the most part back in 2004, and only
      lightly touched since then. It could be better integrated with
      the standard bindings to X now, and there are various design issues
      which might be considered. For example, Imlib is very stateful
      and it might be nice to try to capture that state in a particular
      monad, though there might be some tricky module-loading issues with
      that.
-}

{- -- List of what is not imported --

 -- This list contains anything that interfaces with X (i.e. uses Display/Visual/Drawable/Colormap/Pixmap)

 -- Apart from that, the only thing that is in this list is imlib_apply_filter (which is rather unfortunate,
 -- since it's a cool feature). It uses va_args.

int imlib_get_visual_depth(Display *display, Visual *visual);
Visual *imlib_get_best_visual(Display *display, int screen, int *depth_return);

void imlib_context_set_display(Display *display);
void imlib_context_set_visual(Visual *visual);
void imlib_context_set_colormap(Colormap colormap);
void imlib_context_set_drawable(Drawable drawable);
void imlib_context_set_mask(Pixmap mask);

Display *imlib_context_get_display(void);
Visual *imlib_context_get_visual(void);
Colormap imlib_context_get_colormap(void);
Drawable imlib_context_get_drawable(void);
Pixmap imlib_context_get_mask(void);

void imlib_render_pixmaps_for_whole_image(Pixmap *pixmap_return, Pixmap *mask_return);
void imlib_render_pixmaps_for_whole_image_at_size(Pixmap *pixmap_return, Pixmap *mask_return, int width, int height);
void imlib_free_pixmap_and_mask(Pixmap pixmap);
void imlib_render_image_on_drawable(int x, int y);
void imlib_render_image_on_drawable_at_size(int x, int y, int width, int height);
void imlib_render_image_part_on_drawable_at_size(int source_x, int source_y, int source_width, int source_height, int x, int y, int width, int height);

Imlib_Image imlib_create_image_from_drawable(Pixmap mask, int x, int y, int width, int height, char need_to_grab_x);
Imlib_Image imlib_create_scaled_image_from_drawable(Pixmap mask, int source_x, int source_y, int source_width, int source_height, int destination_width, int destination_height, char need_to_grab_x, char get_mask_from_shape);
char imlib_copy_drawable_to_image(Pixmap mask, int x, int y, int width, int height, int destination_x, int destination_y, char need_to_grab_x);

void imlib_render_image_updates_on_drawable(Imlib_updates updates, int x, int y);

void imlib_render_image_on_drawable_skewed(int source_x, int source_y, int source_width, int source_height, int destination_x, int destination_y, int h_angle_x, int h_angle_y, int v_angle_x, int v_angle_y);
void imlib_render_image_on_drawable_at_angle(int source_x, int source_y, int source_width, int source_height, int destination_x, int destination_y, int angle_x, int angle_y);

void imlib_apply_filter( char *script, ... );
-}

module Graphics.Imlib (
    ImlibProgressFunction, ImlibDataDestructorFunction,
    ImlibTTFEncoding (..), ImlibLoadError (..), ImlibTextDirection (..),
    ImlibOperation (..), ImlibColor (..), ImlibBorder (..), ImlibPolygon,
    ImlibFilter, ImlibColorRange, ImlibFont, ImlibUpdates,
    ImlibColorModifier, ImlibImage, ImlibContext,
    contextSetDitherMask, contextSetAntiAlias, contextSetDither,
    contextSetBlend, contextSetColorModifier, contextSetOperation,
    contextSetFont, contextSetDirection, contextSetAngle, contextSetColor,
    contextSetColorCmya, contextSetColorHsva, contextSetColorHlsa,
    contextSetColorRange, contextSetProgressFunction,
    contextSetProgressGranularity, contextSetFilter, contextSetImage,
    contextGetDitherMask, contextGetAntiAlias, contextGetDither,
    contextGetBlend, contextGetColorModifier, contextGetOperation,
    contextGetFont, contextGetAngle, contextGetDirection, contextGetColor,
    contextGetColorCmya, contextGetColorHsva, contextGetColorHlsa,
    contextGetImlibColor, contextGetColorRange, contextGetProgressFunction,
    contextGetProgressGranularity, contextGetImage, contextGetFilter,
    getCacheSize, setCacheSize, getColorUsage, setColorUsage,
    flushLoaders, loadImage, loadImageImmediately, loadImageWithoutCache,
    loadImageImmediatelyWithoutCache, loadImageWithErrorReturn, freeImage,
    freeImageAndDecache, imageGetWidth, imageGetHeight, imageGetFilename,
    imageGetData, imageGetDataForReadingOnly, imagePutBackData,
    imageWithData, imageHasAlpha, imageSetChangesOnDisk, imageGetBorder,
    imageSetBorder, imageSetFormat, imageSetIrrelevantFormat,
    imageSetIrrelevantBorder, imageSetIrrelevantAlpha, imageFormat,
    imageSetHasAlpha, blendImageOntoImage, createImage,
    createImageUsingData, createImageUsingCopiedData, cloneImage,
    createCroppedImage, createCroppedScaledImage, updatesClone,
    updateAppendRect, updatesMerge, updatesMergeForRendering,
    updatesFree, updatesGetNext, updatesGetCoordinates,
    updatesSetCoordinates, updatesInit, updatesAppendUpdates,
    imageFlipHorizontal, imageFlipVertical, imageFlipDiagonal,
    imageOrientate, imageBlur, imageSharpen, imageTileHorizontal,
    imageTileVertical, imageTile, loadFont, freeFont, textDraw,
    textDrawWithReturnMetrics, getTextSize, getTextAdvance, getTextInset,
    addPathToFontPath, removePathFromFontPath, listFontPath,
    textGetIndexAndLocation, textGetLocationAtIndex, listFonts,
    getFontCacheSize, setFontCacheSize, flushFontCache, getFontAscent,
    getFontDescent, getMaximumFontAscent, getMaximumFontDescent,
    createColorModifier, freeColorModifier, modifyColorModifierGamma,
    modifyColorModifierBrightness, modifyColorModifierContrast,
    setColorModifierTables, getColorModifierTables, resetColorModifier, 
    applyColorModifier, applyColorModifierToRectangle, imageDrawLine, 
    imageDrawRectangle, imageFillRectangle, imageCopyAlphaToImage, 
    imageCopyAlphaRectangleToImage, imageScrollRect, imageCopyRect, 
    createColorRange, freeColorRange, addColorToColorRange, 
    imageFillColorRangeRectangle, imageFillHsvaColorRangeRectangle, 
    imageQueryPixel, imageQueryPixelCmya, imageQueryPixelHsva, 
    imageQueryPixelHlsa, imageAttachDataValue, imageGetAttachedData, 
    imageGetAttachedValue, imageRemoveAttachedDataValue, 
    imageRemoveAndFreeAttachedDataValue, saveImage, 
    saveImageWithErrorReturn, createRotatedImage, 
    blendImageOntoImageAtAngle, blendImageOntoImageSkewed, 
    contextSetCliprect, polygonNew, polygonFree, 
    polygonAddPoint, imageDrawPolygon, imageFillPolygon, polygonGetBounds, 
    polygonContainsPoint, imageDrawEllipse, imageFillEllipse, imageFilter, 
    createFilter, freeFilter, filterSet, filterSetAlpha, filterSetRed, 
    filterSetGreen, filterSetBlue, filterConstants, filterDivisors,
    createImageUsingArray, withImageBits, withImage, colorFromBits, colorToBits
)
where

import Foreign
import Foreign.C
import Data.Bits
import Data.Array
import Control.Monad

import Graphics.X11.Types

{- Types from the Imlib 2 header
typedef void *ImlibContext;
typedef void *ImlibImage;
typedef void *ImlibColorModifier;
typedef void *ImlibUpdates;
typedef void *ImlibFont;
typedef void *ImlibColorRange;
typedef void *ImlibFilter;
typedef struct ImlibBorder ImlibBorder;
typedef struct ImlibColor ImlibColor;
typedef void *ImlibPolygon;
-}

----- Utility Calls

nmalloc :: Storable a => Int -> IO [Ptr a]
nmalloc n = replicateM n malloc

nmallocArray :: Storable a => Int -> Int -> IO [Ptr a]
nmallocArray n m = replicateM n (mallocArray m)

-----

newtype ImlibContext = ImlibContext (Ptr ImlibContext)
newtype ImlibImage = ImlibImage (Ptr ImlibImage)
newtype ImlibColorModifier = ImlibColorModifier (Ptr ImlibColorModifier)
newtype ImlibUpdates = ImlibUpdates (Ptr ImlibUpdates)
newtype ImlibFont = ImlibFont (Ptr ImlibFont)
newtype ImlibColorRange = ImlibColorRange (Ptr ImlibColorRange)
newtype ImlibFilter = ImlibFilter (Ptr ImlibFilter)
newtype ImlibPolygon = ImlibPolygon (Ptr ImlibPolygon)

data ImlibBorder = ImlibBorder CInt CInt CInt CInt deriving (Show, Eq) -- left, right, top, bottom
instance Storable ImlibBorder where
    sizeOf _ = 16
    alignment _ = 4
    peek p = do
        [a,b,c,d] <- peekArray 4 (castPtr p)
        return (ImlibBorder a b c d)
    poke p (ImlibBorder a b c d) =
        pokeArray (castPtr p) [a,b,c,d]

data ImlibColor = ImlibColor Word32 Word32 Word32 Word32 deriving (Show, Eq) -- alpha, red, green, blue
instance Storable ImlibColor where
    sizeOf _ = 16
    alignment _ = 4
    peek p = do
        [a,b,c,d] <- peekArray 4 (castPtr p)
        return (ImlibColor a b c d)
        where
    poke p (ImlibColor a b c d) = do
        pokeArray (castPtr p) [a,b,c,d]

data ImlibOperation = ImlibOpCopy     -- 0
                    | ImlibOpAdd      -- 1
                    | ImlibOpSubtract -- 2
                    | ImlibOpReshade  -- 3
    deriving (Enum, Show, Eq)

data ImlibTextDirection = ImlibTextToRight -- 0
                        | ImlibTextToLeft  -- 1
                        | ImlibTextToDown  -- 2
                        | ImlibTextToUp    -- 3
                        | ImlibTextToAngle -- 4
    deriving (Enum, Show, Eq)

data ImlibLoadError = ImlibLoadErrorNone                           -- 0
                    | ImlibLoadErrorFileDoesNotExist               -- 1
                    | ImlibLoadErrorFileIsDirectory                -- 2
                    | ImlibLoadErrorPermissionDeniedToRead         -- 3
                    | ImlibLoadErrorNoLoaderForFileFormat          -- 4
                    | ImlibLoadErrorPathTooLong                    -- 5
                    | ImlibLoadErrorPathComponentNonExistant       -- 6
                    | ImlibLoadErrorPathComponentNotDirectory      -- 7
                    | ImlibLoadErrorPathPointsOutsideAddressSpace  -- 8
                    | ImlibLoadErrorTooManySymbolicLinks           -- 9
                    | ImlibLoadErrorOutOfMemory                    -- 10
                    | ImlibLoadErrorOutOfFileDescriptors           -- 11
                    | ImlibLoadErrorPermissionDeniedToWrite        -- 12
                    | ImlibLoadErrorOutOfDiskSpace                 -- 13
                    | ImlibLoadErrorUnknown                        -- 14
    deriving (Show, Enum, Eq)

data ImlibTTFEncoding = ImlibTTFEncodingISO88591 -- 0
                      | ImlibTTFEncodingISO88592 -- 1
                      | ImlibTTFEncodingISO88593 -- 2
                      | ImlibTTFEncodingISO88594 -- 3
                      | ImlibTTFEncodingISO88595 -- 4
    deriving (Enum, Show, Eq)

type ImlibProgressFunction = ImlibImage -- image
                            -> Word8    -- percent
                            -> CInt      -- update x
                            -> CInt      -- update y
                            -> CInt      -- update w
                            -> CInt      -- update h
                            -> IO CInt   -- return value (not sure what this means)
foreign import ccall "wrapper" mkProgressFunction :: ImlibProgressFunction -> IO (FunPtr ImlibProgressFunction)

colorToBits :: ImlibColor -> Word32
colorToBits (ImlibColor a r g b) = fromIntegral (b + (g `shift` 8) + (r `shift` 16) + (a `shift` 24))

colorFromBits :: Word32 -> ImlibColor
colorFromBits n = ImlibColor a r g b
    where
        b = fromIntegral $ n                 `mod` 256
        g = fromIntegral $ (n `shift` (-8))  `mod` 256
        r = fromIntegral $ (n `shift` (-16)) `mod` 256
        a = fromIntegral $ (n `shift` (-24)) `mod` 256

--typedef void (*ImlibDataDestructorFunction) (ImlibImage im, void *data);
type ImlibDataDestructorFunction = ImlibImage -> Ptr () -> IO ()
foreign import ccall "wrapper" mkDestructorFunction :: ImlibDataDestructorFunction -> IO (FunPtr ImlibDataDestructorFunction)

--void imlib_context_set_dither_mask(char dither_mask);
foreign import ccall "static Imlib2.h imlib_context_set_dither_mask" imlibContextSetDitherMask :: Bool -> IO ()
contextSetDitherMask :: Bool -> IO ()
contextSetDitherMask = imlibContextSetDitherMask

--void imlib_context_set_anti_alias(char anti_alias);
foreign import ccall "static Imlib2.h imlib_context_set_anti_alias" imlibContextSetAntiAlias :: Bool -> IO ()
contextSetAntiAlias :: Bool -> IO ()
contextSetAntiAlias = imlibContextSetAntiAlias

--void imlib_context_set_dither(char dither);
foreign import ccall "static Imlib2.h imlib_context_set_dither" imlibContextSetDither :: Bool -> IO ()
contextSetDither :: Bool -> IO ()
contextSetDither = imlibContextSetDither

--void imlib_context_set_blend(char blend);
foreign import ccall "static Imlib2.h imlib_context_set_blend" imlibContextSetBlend :: Bool -> IO ()
contextSetBlend :: Bool -> IO ()
contextSetBlend = imlibContextSetBlend

--void imlib_context_set_color_modifier(Imlib_Color_Modifier color_modifier);
foreign import ccall "static Imlib2.h imlib_context_set_color_modifier" imlibContextSetColorModifier :: ImlibColorModifier -> IO ()
contextSetColorModifier :: ImlibColorModifier -> IO ()
contextSetColorModifier = imlibContextSetColorModifier

--void imlib_context_set_operation(Imlib_Operation operation);
foreign import ccall "static Imlib2.h imlib_context_set_operation" imlibContextSetOperation :: CInt -> IO ()
contextSetOperation :: ImlibOperation -> IO ()
contextSetOperation op = imlibContextSetOperation (fromIntegral $ fromEnum op)

--void imlib_context_set_font(Imlib_Font font);
foreign import ccall "static Imlib2.h imlib_context_set_font" imlibContextSetFont :: ImlibFont -> IO ()
contextSetFont :: ImlibFont -> IO ()
contextSetFont = imlibContextSetFont

--void imlib_context_set_direction(Imlib_Text_Direction direction);
foreign import ccall "static Imlib2.h imlib_context_set_direction" imlibContextSetDirection :: CInt -> IO ()
contextSetDirection :: ImlibTextDirection -> IO ()
contextSetDirection dir = imlibContextSetDirection (fromIntegral $ fromEnum dir)

--void imlib_context_set_angle(double angle);
foreign import ccall "static Imlib2.h imlib_context_set_angle" imlibContextSetAngle :: CDouble -> IO ()
contextSetAngle :: Double -> IO ()
contextSetAngle = imlibContextSetAngle . realToFrac

--void imlib_context_set_color(int red, int green, int blue, int alpha);
foreign import ccall "static Imlib2.h imlib_context_set_color" imlibContextSetColor :: CInt -> CInt -> CInt -> CInt -> IO ()
contextSetColor :: Int -> Int -> Int -> Int -> IO ()
contextSetColor r g b a = imlibContextSetColor (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

--void imlib_context_set_color_cmya(int cyan, magenta, int yellow, int alpha);
foreign import ccall "static Imlib2.h imlib_context_set_color_cmya" imlibContextSetColorCmya :: CInt -> CInt -> CInt -> CInt -> IO ()
contextSetColorCmya :: Int -> Int -> Int -> Int -> IO ()
contextSetColorCmya c m y a = imlibContextSetColorCmya (fromIntegral c) (fromIntegral m) (fromIntegral y) (fromIntegral a)

--void imlib_context_set_color_hsva(float hue, float saturation, float value, int alpha);
foreign import ccall "static Imlib2.h imlib_context_set_color_hsva" imlibContextSetColorHsva :: CInt -> CInt -> CInt -> CInt -> IO ()
contextSetColorHsva h s v a = imlibContextSetColorHsva (fromIntegral h) (fromIntegral s) (fromIntegral v) (fromIntegral a)

--void imlib_context_set_color_hlsa(float hue, float lightness, float saturation, int alpha);
foreign import ccall "static Imlib2.h imlib_context_set_color_hlsa" imlibContextSetColorHlsa :: CInt -> CInt -> CInt -> CInt -> IO ()
contextSetColorHlsa h l s a = imlibContextSetColorHlsa (fromIntegral h) (fromIntegral l) (fromIntegral s) (fromIntegral a)

--void imlib_context_set_color_range(Imlib_Color_Range color_range);
foreign import ccall "static Imlib2.h imlib_context_set_color_range" imlibContextSetColorRange :: ImlibColorRange -> IO ()
contextSetColorRange :: ImlibColorRange -> IO ()
contextSetColorRange = imlibContextSetColorRange

--void imlib_context_set_progress_function(Imlib_Progress_Function progress_function);
foreign import ccall "static Imlib2.h imlib_context_set_progress_function" imlibContextSetProgressFunction :: FunPtr(ImlibProgressFunction) -> IO ()
contextSetProgressFunction f = do
    fn <- mkProgressFunction f
    imlibContextSetProgressFunction fn

--void imlib_context_set_progress_granularity(char progress_granularity);
foreign import ccall "static Imlib2.h imlib_context_set_progress_granularity" imlibContextSetProgressGranularity :: Word8 -> IO ()
contextSetProgressGranularity :: Word8 -> IO ()
contextSetProgressGranularity = imlibContextSetProgressGranularity

--void imlib_context_set_filter(Imlib_Filter filter);
foreign import ccall "static Imlib2.h imlib_context_set_filter" imlibContextSetFilter :: ImlibFilter -> IO ()
contextSetFilter :: ImlibFilter -> IO ()
contextSetFilter = imlibContextSetFilter

-- void imlib_context_set_image(Imlib_Image image);
foreign import ccall "static Imlib2.h imlib_context_set_image" imlibContextSetImage :: ImlibImage -> IO ()
contextSetImage :: ImlibImage -> IO ()
contextSetImage = imlibContextSetImage

-- char imlib_context_get_dither_mask(void);
foreign import ccall "static Imlib2.h imlib_context_get_dither_mask" imlibContextGetDitherMask :: IO Bool
contextGetDitherMask :: IO Bool
contextGetDitherMask = imlibContextGetDitherMask

-- char imlib_context_get_anti_alias(void);
foreign import ccall "static Imlib2.h imlib_context_get_anti_alias" imlibContextGetAntiAlias :: IO Bool
contextGetAntiAlias :: IO Bool
contextGetAntiAlias = imlibContextGetAntiAlias

-- char imlib_context_get_dither(void);
foreign import ccall "static Imlib2.h imlib_context_get_dither" imlibContextGetDither :: IO Bool
contextGetDither :: IO Bool
contextGetDither = imlibContextGetDither

-- char imlib_context_get_blend(void);
foreign import ccall "static Imlib2.h imlib_context_get_blend" imlibContextGetBlend :: IO Bool
contextGetBlend :: IO Bool
contextGetBlend = imlibContextGetBlend

-- Imlib_Color_Modifier imlib_context_get_color_modifier(void);
foreign import ccall "static Imlib2.h imlib_context_get_color_modifier" imlibContextGetColorModifier :: IO ImlibColorModifier
contextGetColorModifier :: IO ImlibColorModifier
contextGetColorModifier = imlibContextGetColorModifier

-- Imlib_Operation imlib_context_get_operation(void);
foreign import ccall "static Imlib2.h imlib_context_get_operation" imlibContextGetOperation :: IO CInt
contextGetOperation :: IO ImlibOperation
contextGetOperation = do
    i <- imlibContextGetOperation
    return (toEnum $ fromIntegral i)

-- Imlib_Font imlib_context_get_font(void);
foreign import ccall "static Imlib2.h imlib_context_get_font" imlibContextGetFont :: IO ImlibFont
contextGetFont :: IO ImlibFont
contextGetFont = imlibContextGetFont

-- double imlib_context_get_angle(void);
foreign import ccall "static Imlib2.h imlib_context_get_angle" imlibContextGetAngle :: IO CDouble
contextGetAngle :: IO Double
contextGetAngle = imlibContextGetAngle >>= return . realToFrac

-- Imlib_Text_Direction imlib_context_get_direction(void);
foreign import ccall "static Imlib2.h imlib_context_get_direction" imlibContextGetDirection :: IO CInt
contextGetDirection :: IO ImlibTextDirection
contextGetDirection = do
    d <- imlibContextGetDirection
    return (toEnum $ fromIntegral d)

-- void imlib_context_get_color(int *red, int *green, int *blue, int *alpha);
foreign import ccall "static Imlib2.h imlib_context_get_color" imlibContextGetColor :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
contextGetColor :: IO (Int, Int, Int, Int)
contextGetColor = do
    [r,g,b,a] <- nmalloc 4
    imlibContextGetColor r g b a
    [rr,gg,bb,aa] <- liftM (map fromIntegral) $ mapM peek [r,g,b,a]
    mapM free [r,g,b,a]
    return (rr,gg,bb,aa)

-- void imlib_context_get_color_cmya(int *cyan, int *magenta, int *yellow, int *alpha);
foreign import ccall "static Imlib2.h imlib_context_get_color_cmya" imlibContextGetColorCmya :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
contextGetColorCmya :: IO (Int, Int, Int, Int)
contextGetColorCmya = do
    [c,m,y,a] <- nmalloc 4
    imlibContextGetColorCmya c m y a
    [cc,mm,yy,aa] <- liftM (map fromIntegral) $ mapM peek [c,m,y,a]
    mapM free [c,m,y,a]
    return (cc,mm,yy,aa)

-- void imlib_context_get_color_hsva(float *hue, float *saturation, float *value, int *alpha);
foreign import ccall "static Imlib2.h imlib_context_get_color_hsva" imlibContextGetColorHsva :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
contextGetColorHsva :: IO (Int, Int, Int, Int)
contextGetColorHsva = do
    [h,s,v,a] <- nmalloc 4
    imlibContextGetColorHsva h s v a
    [hh,ss,vv,aa] <- liftM (map fromIntegral) $ mapM peek [h,s,v,a]
    mapM free [h,s,v,a]
    return (hh,ss,vv,aa)

-- void imlib_context_get_color_hlsa(float *hue, float * lightness, float *saturation, int *alpha);
foreign import ccall "static Imlib2.h imlib_context_get_color_hlsa" imlibContextGetColorHlsa :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
contextGetColorHlsa :: IO (Int, Int, Int, Int)
contextGetColorHlsa = do
    [h,l,s,a] <- nmalloc 4
    imlibContextGetColorHlsa h l s a
    [hh,ll,ss,aa] <- liftM (map fromIntegral) $ mapM peek [h,l,s,a]
    mapM free [h,l,s,a]
    return (hh,ll,ss,aa)

-- Imlib_Color *imlib_context_get_imlib_color(void);
foreign import ccall "static Imlib2.h imlib_context_get_imlib_color" imlibContextGetImlibColor :: IO (Ptr ImlibColor)
contextGetImlibColor :: IO ImlibColor
contextGetImlibColor = do
    c <- imlibContextGetImlibColor
    cc <- peek c
    free c
    return cc

-- Imlib_Color_Range imlib_context_get_color_range(void);
foreign import ccall "static Imlib2.h imlib_context_get_color_range" imlibContextGetColorRange :: IO ImlibColorRange
contextGetColorRange :: IO ImlibColorRange
contextGetColorRange = imlibContextGetColorRange

-- Imlib_Progress_Function imlib_context_get_progress_function(void);
foreign import ccall "static Imlib2.h imlib_context_get_progress_function" imlibContextGetProgressFunction :: IO (FunPtr ImlibProgressFunction)
contextGetProgressFunction = imlibContextGetProgressFunction

-- char imlib_context_get_progress_granularity(void);
foreign import ccall "static Imlib2.h imlib_context_get_progress_granularity" imlibContextGetProgressGranularity :: IO Word8
contextGetProgressGranularity :: IO Word8
contextGetProgressGranularity = imlibContextGetProgressGranularity

-- Imlib_Image imlib_context_get_image(void);
foreign import ccall "static Imlib2.h imlib_context_get_image" imlibContextGetImage :: IO ImlibImage
contextGetImage :: IO ImlibImage
contextGetImage = imlibContextGetImage

-- Imlib_Filter imlib_context_get_filter(void);
foreign import ccall "static Imlib2.h imlib_context_get_filter" imlibContextGetFilter :: IO ImlibFilter
contextGetFilter :: IO ImlibFilter
contextGetFilter = imlibContextGetFilter

-- int imlib_get_cache_size(void);
foreign import ccall "static Imlib2.h imlib_get_cache_size" imlibGetCacheSize :: IO CInt
getCacheSize :: IO Int
getCacheSize = imlibGetCacheSize >>= return . fromIntegral

-- void imlib_set_cache_size(int bytes);
foreign import ccall "static Imlib2.h imlib_set_cache_size" imlibSetCacheSize :: Int -> IO ()
setCacheSize :: Int -> IO ()
setCacheSize = imlibSetCacheSize

-- int imlib_get_color_usage(void);
foreign import ccall "static Imlib2.h imlib_get_color_usage" imlibGetColorUsage :: IO CInt
getColorUsage :: IO Int
getColorUsage = imlibGetColorUsage >>= return . fromIntegral

-- void imlib_set_color_usage(int max);
foreign import ccall "static Imlib2.h imlib_set_color_usage" imlibSetColorUsage :: CInt -> IO ()
setColorUsage :: Int -> IO ()
setColorUsage = imlibSetColorUsage . fromIntegral

-- void imlib_flush_loaders(void);
foreign import ccall "static Imlib2.h imlib_flush_loaders" imlibFlushLoaders :: IO ()
flushLoaders :: IO ()
flushLoaders = imlibFlushLoaders

-- Imlib_Image imlib_load_image(const char *file);
foreign import ccall "static Imlib2.h imlib_load_image" imlibLoadImage :: CString -> IO ImlibImage
loadImage :: String -> IO ImlibImage
loadImage str = withCString str imlibLoadImage

--Imlib_Image imlib_load_image_immediately(const char *file);
foreign import ccall "static Imlib2.h imlib_load_image_immediately" imlibLoadImageImmediately :: CString -> IO ImlibImage
loadImageImmediately :: String -> IO ImlibImage
loadImageImmediately str = withCString str imlibLoadImageImmediately

--Imlib_Image imlib_load_image_without_cache(const char *file);
foreign import ccall "static Imlib2.h imlib_load_image_without_cache" imlibLoadImageWithoutCache :: CString -> IO ImlibImage
loadImageWithoutCache :: String -> IO ImlibImage
loadImageWithoutCache str = withCString str imlibLoadImageWithoutCache

--Imlib_Image imlib_load_image_immediately_without_cache(const char *file);
foreign import ccall "static Imlib2.h imlib_load_image_immediately_without_cache" imlibLoadImageImmediatelyWithoutCache :: CString -> IO ImlibImage
loadImageImmediatelyWithoutCache str = withCString str imlibLoadImageImmediatelyWithoutCache

--Imlib_Image imlib_load_image_with_error_return(const char *file, Imlib_Load_Error *error_return);
foreign import ccall "static Imlib2.h imlib_load_image_with_error_return" imlibLoadImageWithErrorReturn :: CString -> Ptr CInt -> IO ImlibImage
loadImageWithErrorReturn :: String -> IO (ImlibImage, ImlibLoadError)
loadImageWithErrorReturn str = do
    pe <- malloc
    im <- withCString str (\s -> imlibLoadImageWithErrorReturn s pe)
    e <- peek pe
    free pe
    return (im, toEnum $ fromIntegral e)

--void imlib_free_image(void);
foreign import ccall "static Imlib2.h imlib_free_image" imlibFreeImage :: IO ()
freeImage :: IO ()
freeImage = imlibFreeImage

--void imlib_free_image_and_decache(void);
foreign import ccall "static Imlib2.h imlib_free_image_and_decache" imlibFreeImageAndDecache :: IO ()
freeImageAndDecache :: IO ()
freeImageAndDecache = imlibFreeImageAndDecache

--int imlib_image_get_width(void);
foreign import ccall "static Imlib2.h imlib_image_get_width" imlibImageGetWidth :: IO CInt
imageGetWidth :: IO Int
imageGetWidth = imlibImageGetWidth >>= return . fromIntegral

--int imlib_image_get_height(void);
foreign import ccall "static Imlib2.h imlib_image_get_height" imlibImageGetHeight :: IO CInt
imageGetHeight :: IO Int
imageGetHeight = imlibImageGetHeight >>= return . fromIntegral

--const char *imlib_image_get_filename(void);
foreign import ccall "static Imlib2.h imlib_image_get_filename" imlibImageGetFilename :: IO CString
imageGetFilename :: IO String
imageGetFilename = do
    b <- imlibImageGetFilename
    peekCString b

--DATA32 *imlib_image_get_data(void);
foreign import ccall "static Imlib2.h imlib_image_get_data" imlibImageGetData :: IO (Ptr Word32)
imageGetData :: IO (Ptr Word32)
imageGetData = imlibImageGetData

--DATA32 *imlib_image_get_data_for_reading_only(void);
foreign import ccall "static Imlib2.h imlib_image_get_data_for_reading_only" imlibImageGetDataForReadingOnly :: IO (Ptr Word32)
imageGetDataForReadingOnly :: IO (Ptr Word32)
imageGetDataForReadingOnly = imlibImageGetDataForReadingOnly

--void imlib_image_put_back_data(DATA32 *data);
foreign import ccall "static Imlib2.h imlib_image_put_back_data" imlibImagePutBackData :: Ptr Word32 -> IO ()
imagePutBackData :: Ptr Word32 -> IO ()
imagePutBackData = imlibImagePutBackData

-- Convenience "control structure" for working with getting and putting image data back, since you must always return the data that you get.
imageWithData :: (Ptr Word32 -> IO a) -> IO a
imageWithData f = do
    p <- imageGetData
    r <- f p
    imagePutBackData p
    return r

withImageBits :: (Int -> Int -> [Word32] -> [Word32]) -> IO ()
withImageBits f = do
    w <- imageGetWidth
    h <- imageGetHeight
    p <- imageGetData
    arr <- peekArray (w*h) p
    -- turn the [Word32] into a [Imlib_Color] and pass it to the function along with the
    -- width and height, then poke the array with the new image returned.
    pokeArray p $ f w h arr
    imagePutBackData p

-- Takes a function which gets the image width and height and image data and transforms it,
-- and returns an action performing that transformation.
withImage :: (Int -> Int -> [ImlibColor] -> [ImlibColor]) -> IO ()
withImage f = do
    w <- imageGetWidth
    h <- imageGetHeight
    p <- imageGetData
    arr <- peekArray (w*h) p
    -- turn the [Word32] into a [Imlib_Color] and pass it to the function along with the
    -- width and height, then poke the array with the new image returned.
    pokeArray p . map colorToBits . f w h . map colorFromBits $ arr
    imagePutBackData p

--char imlib_image_has_alpha(void);
foreign import ccall "static Imlib2.h imlib_image_has_alpha" imlibImageHasAlpha :: IO Bool
imageHasAlpha :: IO Bool
imageHasAlpha = imlibImageHasAlpha

--void imlib_image_set_changes_on_disk(void);
foreign import ccall "static Imlib2.h imlib_image_set_changes_on_disk" imlibImageSetChangesOnDisk :: IO ()
imageSetChangesOnDisk :: IO ()
imageSetChangesOnDisk = imlibImageSetChangesOnDisk

--void imlib_image_get_border(Imlib_Border *border);
foreign import ccall "static Imlib2.h imlib_image_get_border" imlibImageGetBorder :: Ptr ImlibBorder -> IO ()
imageGetBorder :: IO ImlibBorder
imageGetBorder = do
    b <- malloc
    imlibImageGetBorder b
    bb <- peek b
    free b
    return bb

--void imlib_image_set_border(Imlib_Border *border);
foreign import ccall "static Imlib2.h imlib_image_set_border" imlibImageSetBorder :: Ptr ImlibBorder -> IO ()
imageSetBorder :: ImlibBorder -> IO ()
imageSetBorder b = do
    p <- malloc
    poke p b
    imlibImageSetBorder p
    free p

--void imlib_image_set_format(const char *format);
foreign import ccall "static Imlib2.h imlib_image_set_format" imlibImageSetFormat :: CString -> IO ()
imageSetFormat :: String -> IO ()
imageSetFormat str = withCString str imlibImageSetFormat

--void imlib_image_set_irrelevant_format(char irrelevant);
foreign import ccall "static Imlib2.h imlib_image_set_irrelevant_format" imlibImageSetIrrelevantFormat :: Bool -> IO ()
imageSetIrrelevantFormat :: Bool -> IO ()
imageSetIrrelevantFormat = imlibImageSetIrrelevantFormat

--void imlib_image_set_irrelevant_border(char irrelevant);
foreign import ccall "static Imlib2.h imlib_image_set_irrelevant_border" imlibImageSetIrrelevantBorder :: Bool -> IO ()
imageSetIrrelevantBorder :: Bool -> IO ()
imageSetIrrelevantBorder = imlibImageSetIrrelevantBorder

--void imlib_image_set_irrelevant_alpha(char irrelevant);
foreign import ccall "static Imlib2.h imlib_image_set_irrelevant_alpha" imlibImageSetIrrelevantAlpha :: Bool -> IO ()
imageSetIrrelevantAlpha :: Bool -> IO ()
imageSetIrrelevantAlpha = imlibImageSetIrrelevantAlpha

--char *imlib_image_format(void);
foreign import ccall "static Imlib2.h imlib_image_format" imlibImageFormat :: IO CString
imageFormat :: IO String
imageFormat = imlibImageFormat >>= peekCString

--void imlib_image_set_has_alpha(char has_alpha);
foreign import ccall "static Imlib2.h imlib_image_set_has_alpha" imlibImageSetHasAlpha :: Bool -> IO ()
imageSetHasAlpha :: Bool -> IO ()
imageSetHasAlpha = imlibImageSetHasAlpha


--void imlib_blend_image_onto_image(Imlib_Image source_image, char merge_alpha, int source_x, int source_y, int source_width, int source_height, int destination_x, int destination_y, int destination_width, int destination_height);
foreign import ccall "static Imlib2.h imlib_blend_image_onto_image" imlibBlendImageOntoImage
    :: ImlibImage
    -> Bool  -- merge alpha
    -> CInt   -- source x
    -> CInt   -- source y
    -> CInt   -- source width
    -> CInt   -- source height
    -> CInt   -- dest x
    -> CInt   -- dest y
    -> CInt   -- dest width
    -> CInt   -- dest height
    -> IO ()
blendImageOntoImage img mergeAlpha (sx, sy, sw, sh) (dx, dy, dw, dh) = imlibBlendImageOntoImage img mergeAlpha (fromIntegral sx) (fromIntegral sy) (fromIntegral sw) (fromIntegral sh) (fromIntegral dx) (fromIntegral dy) (fromIntegral dw) (fromIntegral dh)

--Imlib_Image imlib_create_image(int width, int height);
foreign import ccall "static Imlib2.h imlib_create_image" imlibCreateImage :: CInt -> CInt -> IO ImlibImage
createImage :: Int -> Int -> IO ImlibImage
createImage w h = imlibCreateImage (fromIntegral w) (fromIntegral h)

--Imlib_Image imlib_create_image_using_data(int width, int height, DATA32 *data);
foreign import ccall "static Imlib2.h imlib_create_image_using_data" imlibCreateImageUsingData :: CInt -> CInt -> Ptr Word32 -> IO ImlibImage
createImageUsingData :: Int -> Int -> Ptr Word32 -> IO ImlibImage
createImageUsingData w h ptr = imlibCreateImageUsingData (fromIntegral w) (fromIntegral h) ptr

--Imlib_Image imlib_create_image_using_copied_data(int width, int height, DATA32 *data);
foreign import ccall "static Imlib2.h imlib_create_image_using_copied_data" imlibCreateImageUsingCopiedData :: CInt -> CInt -> Ptr(Word32) -> IO ImlibImage
createImageUsingCopiedData w h ptr  = imlibCreateImageUsingCopiedData (fromIntegral w) (fromIntegral h) ptr

createImageUsingList :: Int -> Int -> [ImlibColor] -> IO ImlibImage
createImageUsingList w h xs = withArray (map colorToBits xs) (createImageUsingCopiedData w h)

createImageUsingArray :: Array (Int, Int) ImlibColor -> IO ImlibImage
createImageUsingArray a = createImageUsingList (b-t+1) (r-l+1) (elems a)
    where ((t,l),(b,r)) = bounds a

--Imlib_Image imlib_clone_image(void);
foreign import ccall "static Imlib2.h imlib_clone_image" imlibCloneImage :: IO ImlibImage
cloneImage :: IO ImlibImage
cloneImage = imlibCloneImage

--Imlib_Image imlib_create_cropped_image(int x, int y, int width, int height);
foreign import ccall "static Imlib2.h imlib_create_cropped_image" imlibCreateCroppedImage :: CInt -> CInt -> CInt -> CInt -> IO ImlibImage
createCroppedImage x y w h = imlibCreateCroppedImage (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

--Imlib_Image imlib_create_cropped_scaled_image(int source_x, int source_y, int source_width, int source_height, int destination_width, int destination_height);
foreign import ccall "static Imlib2.h imlib_create_cropped_scaled_image" imlibCreateCroppedScaledImage :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ImlibImage
createCroppedScaledImage sx sy sw sh dw dh = imlibCreateCroppedScaledImage (fromIntegral sx) (fromIntegral sy) (fromIntegral sw) (fromIntegral sh) (fromIntegral dw) (fromIntegral dh)

--Imlib_Updates imlib_updates_clone(Imlib_Updates updates);
foreign import ccall "static Imlib2.h imlib_updates_clone" imlibUpdatesClone :: ImlibUpdates -> IO ImlibUpdates
updatesClone :: ImlibUpdates -> IO ImlibUpdates
updatesClone = imlibUpdatesClone

--Imlib_Updates imlib_update_append_rect(Imlib_Updates updates, int x, int y, int w, int h);
foreign import ccall "static Imlib2.h imlib_update_append_rect" imlibUpdateAppendRect :: ImlibUpdates -> CInt -> CInt -> CInt -> CInt -> IO ImlibUpdates
updateAppendRect u x y w h = imlibUpdateAppendRect u (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

--Imlib_Updates imlib_updates_merge(Imlib_Updates updates, int w, int h);
foreign import ccall "static Imlib2.h imlib_updates_merge" imlibUpdatesMerge :: ImlibUpdates -> CInt -> CInt -> IO ImlibUpdates
updatesMerge u w h = imlibUpdatesMerge u (fromIntegral w) (fromIntegral h)

--Imlib_Updates imlib_updates_merge_for_rendering(Imlib_Updates updates, int w, int h);
foreign import ccall "static Imlib2.h imlib_updates_merge_for_rendering" imlibUpdatesMergeForRendering :: ImlibUpdates -> CInt -> CInt -> IO ImlibUpdates
updatesMergeForRendering u w h = imlibUpdatesMergeForRendering u (fromIntegral w) (fromIntegral h)

--void imlib_updates_free(Imlib_Updates updates);
foreign import ccall "static Imlib2.h imlib_updates_free" imlibUpdatesFree :: ImlibUpdates -> IO ()
updatesFree :: ImlibUpdates -> IO ()
updatesFree = imlibUpdatesFree

--Imlib_Updates imlib_updates_get_next(Imlib_Updates updates);
foreign import ccall "static Imlib2.h imlib_updates_get_next" imlibUpdatesGetNext :: ImlibUpdates -> IO ImlibUpdates
updatesGetNext :: ImlibUpdates -> IO ImlibUpdates
updatesGetNext = imlibUpdatesGetNext

--void imlib_updates_get_coordinates(Imlib_Updates updates, int *x_return, int *y_return, int *width_return, int *height_return);
foreign import ccall "static Imlib2.h imlib_updates_get_coordinates" imlibUpdatesGetCoordinates :: ImlibUpdates -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
updatesGetCoordinates :: ImlibUpdates -> IO (Int, Int, Int, Int)
updatesGetCoordinates u = do
    [x,y,w,h] <- nmalloc 4
    imlibUpdatesGetCoordinates u x y w h
    [xx,yy,ww,hh] <- liftM (map fromIntegral) $ mapM peek [x,y,w,h]
    mapM free [x,y,w,h]
    return (xx,yy,ww,hh)

--void imlib_updates_set_coordinates(Imlib_Updates updates, int x, int y, int width, int height);
foreign import ccall "static Imlib2.h imlib_updates_set_coordinates" imlibUpdatesSetCoordinates :: ImlibUpdates -> CInt -> CInt -> CInt -> CInt -> IO ()
updatesSetCoordinates u x y w h = imlibUpdatesSetCoordinates u (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

--Imlib_Updates imlib_updates_init(void);
foreign import ccall "static Imlib2.h imlib_updates_init" imlibUpdatesInit :: IO ImlibUpdates
updatesInit :: IO ImlibUpdates
updatesInit = imlibUpdatesInit

--Imlib_Updates imlib_updates_append_updates(Imlib_Updates updates, Imlib_Updates appended_updates);
foreign import ccall "static Imlib2.h imlib_updates_append_updates" imlibUpdatesAppendUpdates :: ImlibUpdates -> ImlibUpdates -> IO ImlibUpdates
updatesAppendUpdates = imlibUpdatesAppendUpdates

--void imlib_image_flip_horizontal(void);
foreign import ccall "static Imlib2.h imlib_image_flip_horizontal" imlibImageFlipHorizontal :: IO ()
imageFlipHorizontal :: IO ()
imageFlipHorizontal = imlibImageFlipHorizontal

--void imlib_image_flip_vertical(void);
foreign import ccall "static Imlib2.h imlib_image_flip_vertical" imlibImageFlipVertical :: IO ()
imageFlipVertical :: IO ()
imageFlipVertical = imlibImageFlipVertical

--void imlib_image_flip_diagonal(void);
foreign import ccall "static Imlib2.h imlib_image_flip_diagonal" imlibImageFlipDiagonal :: IO ()
imageFlipDiagonal :: IO ()
imageFlipDiagonal = imlibImageFlipDiagonal

--void imlib_image_orientate(int orientation);
foreign import ccall "static Imlib2.h imlib_image_orientate" imlibImageOrientate :: CInt -> IO ()
imageOrientate :: Int -> IO ()
imageOrientate = imlibImageOrientate . fromIntegral

--void imlib_image_blur(int radius);
foreign import ccall "static Imlib2.h imlib_image_blur" imlibImageBlur :: CInt -> IO ()
imageBlur :: Int -> IO ()
imageBlur = imlibImageBlur . fromIntegral

--void imlib_image_sharpen(int radius);
foreign import ccall "static Imlib2.h imlib_image_sharpen" imlibImageSharpen :: CInt -> IO ()
imageSharpen :: Int -> IO ()
imageSharpen = imlibImageSharpen . fromIntegral

--void imlib_image_tile_horizontal(void);
foreign import ccall "static Imlib2.h imlib_image_tile_horizontal" imlibImageTileHorizontal :: IO ()
imageTileHorizontal :: IO ()
imageTileHorizontal = imlibImageTileHorizontal

--void imlib_image_tile_vertical(void);
foreign import ccall "static Imlib2.h imlib_image_tile_vertical" imlibImageTileVertical :: IO ()
imageTileVertical :: IO ()
imageTileVertical = imlibImageTileVertical

--void imlib_image_tile(void);
foreign import ccall "static Imlib2.h imlib_image_tile" imlibImageTile :: IO ()
imageTile :: IO ()
imageTile = imlibImageTile

--Imlib_Font imlib_load_font(const char *font_name);
foreign import ccall "static Imlib2.h imlib_load_font" imlibLoadFont :: CString -> IO ImlibFont
loadFont :: String -> IO ImlibFont
loadFont str = withCString str imlibLoadFont

--void imlib_free_font(void);
foreign import ccall "static Imlib2.h imlib_free_font" imlibFreeFont :: IO ()
freeFont :: forall t. t -> IO ()
freeFont _ = imlibFreeFont


--void imlib_text_draw(int x, int y, const char *text);
foreign import ccall "static Imlib2.h imlib_text_draw" imlibTextDraw :: CInt -> CInt -> CString -> IO ()
textDraw :: Int -> Int -> String -> IO ()
textDraw x y str = withCString str (imlibTextDraw (fromIntegral x) (fromIntegral y))

--void imlib_text_draw_with_return_metrics(int x, int y, const char *text, int *width_return, int *height_return, int *horizontal_advance_return, int *vertical_advance_return);
foreign import ccall "static Imlib2.h imlib_text_draw_with_return_metrics" imlibTextDrawWithReturnMetrics :: CInt -> CInt -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
textDrawWithReturnMetrics x y str = do
    [w,h,ah,av] <- nmalloc 4
    withCString str (\s -> imlibTextDrawWithReturnMetrics (fromIntegral x) (fromIntegral y) s w h ah av)
    [rw,rh,rah,rav] <- liftM (map fromIntegral) $ mapM peek [w,h,ah,av]
    mapM free [w,h,ah,av]
    return (rw,rh,rah,rav)

--void imlib_get_text_size(const char *text, int *width_return, int *height_return);
foreign import ccall "static Imlib2.h imlib_get_text_size" imlibGetTextSize :: CString -> Ptr CInt -> Ptr CInt -> IO ()
getTextSize :: String -> IO (Int, Int)
getTextSize str = do
    [w,h] <- nmalloc 2
    withCString str (\s -> imlibGetTextSize s w h)
    [rw,rh] <- mapM peek [w,h]
    mapM free [w,h]
    return (fromIntegral rw,fromIntegral rh)

--void imlib_get_text_advance(const char *text, int *horizontal_advance_return, int *vertical_advance_return);
foreign import ccall "static Imlib2.h imlib_get_text_advance" imlibGetTextAdvance :: CString -> Ptr CInt -> Ptr CInt -> IO ()
getTextAdvance :: String -> IO (Int, Int)
getTextAdvance str = do
    [h,v] <- nmalloc 2
    withCString str (\s -> imlibGetTextAdvance s h v)
    [rh,rv] <- mapM peek [h,v]
    mapM free [h,v]
    return (fromIntegral rh,fromIntegral rv)

--int imlib_get_text_inset(const char *text);
foreign import ccall "static Imlib2.h imlib_get_text_inset" imlibGetTextInset :: CString -> IO CInt
getTextInset :: String -> IO Int
getTextInset str = withCString str $ liftM fromIntegral . imlibGetTextInset

--void imlib_add_path_to_font_path(const char *path);
foreign import ccall "static Imlib2.h imlib_add_path_to_font_path" imlibAddPathToFontPath :: CString -> IO ()
addPathToFontPath :: String -> IO ()
addPathToFontPath str = withCString str imlibAddPathToFontPath

--void imlib_remove_path_from_font_path(const char *path);
foreign import ccall "static Imlib2.h imlib_remove_path_from_font_path" imlibRemovePathFromFontPath :: CString -> IO ()
removePathFromFontPath :: String -> IO ()
removePathFromFontPath str = withCString str imlibRemovePathFromFontPath

--char **imlib_list_font_path(int *number_return);
foreign import ccall "static Imlib2.h imlib_list_font_path" imlibListFontPath :: Ptr CInt -> IO (Ptr CString)
listFontPath :: IO [String]
listFontPath = do
    pn <- malloc
    pxs <- imlibListFontPath pn
    n <- peek pn
    free pn
    xs <- peekArray (fromIntegral n) pxs
    ys <- mapM peekCString xs
    return ys

--int imlib_text_get_index_and_location(const char *text, int x, int y, int *char_x_return, int *char_y_return, int *char_width_return, int *char_height_return);
foreign import ccall "static Imlib2.h imlib_text_get_index_and_location" imlibTextGetIndexAndLocation :: CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
textGetIndexAndLocation str x y = do
    [xp,yp,wp,hp] <- nmalloc 4
    n <- withCString str (\s -> imlibTextGetIndexAndLocation s x y xp yp wp hp)
    [xr,yr,wr,hr] <- mapM peek [xp,yp,wp,hp]
    mapM free [xp,yp,wp,hp]
    return (fromIntegral n, fromIntegral xr, fromIntegral yr, fromIntegral wr, fromIntegral hr)


--void imlib_text_get_location_at_index(const char *text, int index, int *char_x_return, int *char_y_return, int *char_width_return, int *char_height_return);
foreign import ccall "static Imlib2.h imlib_text_get_location_at_index" imlibTextGetLocationAtIndex :: CString -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
textGetLocationAtIndex str k = do
    [xp,yp,wp,hp] <- nmalloc 4
    withCString str (\s -> imlibTextGetLocationAtIndex s k xp yp wp hp)
    [xr,yr,wr,hr] <- mapM peek [xp,yp,wp,hp]
    mapM free [xp,yp,wp,hp]
    return (fromIntegral xr, fromIntegral yr, fromIntegral wr, fromIntegral hr)

--char **imlib_list_fonts(int *number_return);
--void imlib_free_font_list(char **font_list, int number);
foreign import ccall "static Imlib2.h imlib_list_fonts" imlibListFonts :: Ptr CInt -> IO (Ptr CString)
foreign import ccall "static Imlib2.h imlib_free_font_list" imlibFreeFontList :: Ptr (CString) -> CInt -> IO ()
listFonts :: IO [String]
listFonts = do
    pn <- malloc
    pxs <- imlibListFonts pn
    n <- peek pn
    free pn
    xs <- peekArray (fromIntegral n) pxs
    ys <- mapM peekCString xs
    imlibFreeFontList pxs n
    return ys

--int imlib_get_font_cache_size(void);
foreign import ccall "static Imlib2.h imlib_get_font_cache_size" imlibGetFontCacheSize :: IO CInt
getFontCacheSize :: IO Int
getFontCacheSize = liftM fromIntegral imlibGetFontCacheSize

--void imlib_set_font_cache_size(int bytes);
foreign import ccall "static Imlib2.h imlib_set_font_cache_size" imlibSetFontCacheSize :: CInt -> IO ()
setFontCacheSize :: Int -> IO ()
setFontCacheSize = imlibSetFontCacheSize . fromIntegral

--void imlib_flush_font_cache(void);
foreign import ccall "static Imlib2.h imlib_flush_font_cache" imlibFlushFontCache :: IO ()
flushFontCache :: IO ()
flushFontCache = imlibFlushFontCache

--int imlib_get_font_ascent(void);
foreign import ccall "static Imlib2.h imlib_get_font_ascent" imlibGetFontAscent :: IO CInt
getFontAscent :: IO Int
getFontAscent = liftM fromIntegral imlibGetFontAscent

--int imlib_get_font_descent(void);
foreign import ccall "static Imlib2.h imlib_get_font_descent" imlibGetFontDescent :: IO CInt
getFontDescent :: IO Int
getFontDescent = liftM fromIntegral imlibGetFontDescent

--int imlib_get_maximum_font_ascent(void);
foreign import ccall "static Imlib2.h imlib_get_maximum_font_ascent" imlibGetMaximumFontAscent :: IO CInt
getMaximumFontAscent :: IO Int
getMaximumFontAscent = liftM fromIntegral imlibGetMaximumFontAscent

--int imlib_get_maximum_font_descent(void);
foreign import ccall "static Imlib2.h imlib_get_maximum_font_descent" imlibGetMaximumFontDescent :: IO CInt
getMaximumFontDescent :: IO Int
getMaximumFontDescent = liftM fromIntegral imlibGetMaximumFontDescent

--Imlib_Color_Modifier imlib_create_color_modifier(void);
foreign import ccall "static Imlib2.h imlib_create_color_modifier" imlibCreateColorModifier :: IO ImlibColorModifier
createColorModifier :: IO ImlibColorModifier
createColorModifier = imlibCreateColorModifier

--void imlib_free_color_modifier(void);
foreign import ccall "static Imlib2.h imlib_free_color_modifier" imlibFreeColorModifier :: IO ()
freeColorModifier :: IO ()
freeColorModifier = imlibFreeColorModifier

--void imlib_modify_color_modifier_gamma(double gamma_value);
foreign import ccall "static Imlib2.h imlib_modify_color_modifier_gamma" imlibModifyColorModifierGamma :: CDouble -> IO ()
modifyColorModifierGamma :: Double -> IO ()
modifyColorModifierGamma = imlibModifyColorModifierGamma . fromRational . toRational

--void imlib_modify_color_modifier_brightness(double brightness_value);
foreign import ccall "static Imlib2.h imlib_modify_color_modifier_brightness" imlibModifyColorModifierBrightness :: CDouble -> IO ()
modifyColorModifierBrightness :: Double -> IO ()
modifyColorModifierBrightness = imlibModifyColorModifierBrightness . fromRational . toRational

--void imlib_modify_color_modifier_contrast(double contrast_value);
foreign import ccall "static Imlib2.h imlib_modify_color_modifier_contrast" imlibModifyColorModifierContrast :: CDouble -> IO ()
modifyColorModifierContrast :: Double -> IO ()
modifyColorModifierContrast = imlibModifyColorModifierContrast . fromRational . toRational

--void imlib_set_color_modifier_tables(DATA8 *red_table, DATA8 *green_table, DATA8 *blue_table, DATA8 *alpha_table);
foreign import ccall "static Imlib2.h imlib_set_color_modifier_tables" imlibSetColorModifierTables :: (Ptr Word8) -> (Ptr Word8) -> (Ptr Word8) -> (Ptr Word8) -> IO ()
setColorModifierTables r g b a = do
    withArray rs (\ra ->
        withArray gs (\ga ->
            withArray bs (\ba ->
                withArray as (\aa -> imlibSetColorModifierTables ra ga ba aa))))
    where
        padLeft n b xs | l >  n = take n xs
                       | l <  n = (replicate (n-l) b) ++ xs
                       | otherwise = xs -- l == n
            where l = length xs
        rs = padLeft 256 0 r
        gs = padLeft 256 0 g
        bs = padLeft 256 0 b
        as = padLeft 256 0 a

--void imlib_get_color_modifier_tables(DATA8 *red_table, DATA8 *green_table, DATA8 *blue_table, DATA8 *alpha_table);
foreign import ccall "static Imlib2.h imlib_get_color_modifier_tables" imlibGetColorModifierTables :: (Ptr Word8) -> (Ptr Word8) -> (Ptr Word8) -> (Ptr Word8) -> IO ()
getColorModifierTables = do
    [ra,ga,ba,aa] <- nmallocArray 4 256
    imlibGetColorModifierTables ra ga ba aa
    [rs,gs,bs,as] <- mapM (peekArray 256) [ra,ga,ba,aa]
    mapM free [ra,ga,ba,aa]
    return (rs,gs,bs,as)

--void imlib_reset_color_modifier(void);
foreign import ccall "static Imlib2.h imlib_reset_color_modifier" imlibResetColorModifier :: IO ()
resetColorModifier :: IO ()
resetColorModifier = imlibResetColorModifier

--void imlib_apply_color_modifier(void);
foreign import ccall "static Imlib2.h imlib_apply_color_modifier" imlibApplyColorModifier :: IO ()
applyColorModifier :: IO ()
applyColorModifier = imlibApplyColorModifier

--void imlib_apply_color_modifier_to_rectangle(int x, int y, int width, int height);
foreign import ccall "static Imlib2.h imlib_apply_color_modifier_to_rectangle" imlibApplyColorModifierToRectangle :: CInt -> CInt -> CInt -> CInt -> IO ()
applyColorModifierToRectangle :: Int -> Int -> Int -> Int -> IO ()
applyColorModifierToRectangle x y w h = do
    imlibApplyColorModifierToRectangle (fromIntegral x) (fromIntegral x) (fromIntegral w) (fromIntegral h)

--Imlib_Updates imlib_image_draw_line(int x1, int y1, int x2, int y2, char make_updates);
foreign import ccall "static Imlib2.h imlib_image_draw_line" imlibImageDrawLine :: CInt -> CInt -> CInt -> CInt -> Bool -> IO ImlibUpdates
imageDrawLine :: Int -> Int -> Int -> Int -> Bool -> IO ImlibUpdates
imageDrawLine x1 y1 x2 y2 mu = do
    imlibImageDrawLine (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) (fromIntegral y2) mu

--void imlib_image_draw_rectangle(int x, int y, int width, int height);
foreign import ccall "static Imlib2.h imlib_image_draw_rectangle" imlibImageDrawRectangle :: CInt -> CInt -> CInt -> CInt -> IO ()
imageDrawRectangle :: Int -> Int -> Int -> Int -> IO ()
imageDrawRectangle x y w h = do
    imlibImageDrawRectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

--void imlib_image_fill_rectangle(int x, int y, int width, int height);
foreign import ccall "static Imlib2.h imlib_image_fill_rectangle" imlibImageFillRectangle :: CInt -> CInt -> CInt -> CInt -> IO ()
imageFillRectangle :: Int -> Int -> Int -> Int -> IO ()
imageFillRectangle x y w h = do
    imlibImageFillRectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

--void imlib_image_copy_alpha_to_image(Imlib_Image image_source, int x, int y);
foreign import ccall "static Imlib2.h imlib_image_copy_alpha_to_image" imlibImageCopyAlphaToImage :: ImlibImage -> CInt -> CInt -> IO ()
imageCopyAlphaToImage :: ImlibImage -> Int -> Int -> IO ()
imageCopyAlphaToImage im x y = do
    imlibImageCopyAlphaToImage im (fromIntegral x) (fromIntegral y)

--void imlib_image_copy_alpha_rectangle_to_image(Imlib_Image image_source, int x, int y, int width, int height, int destination_x, int destination_y);
foreign import ccall "static Imlib2.h imlib_image_copy_alpha_rectangle_to_image" imlibImageCopyAlphaRectangleToImage :: ImlibImage -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
imageCopyAlphaRectangleToImage img x y w h dx dy = imlibImageCopyAlphaRectangleToImage img (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (fromIntegral dx) (fromIntegral dy)

--void imlib_image_scroll_rect(int x, int y, int width, int height, int delta_x, int delta_y);
foreign import ccall "static Imlib2.h imlib_image_scroll_rect" imlibImageScrollRect :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
imageScrollRect x y w h dx dy = imlibImageScrollRect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (fromIntegral dx) (fromIntegral dy)

--void imlib_image_copy_rect(int x, int y, int width, int height, int new_x, int new_y);
foreign import ccall "static Imlib2.h imlib_image_copy_rect" imlibImageCopyRect :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
imageCopyRect x y w h dx dy = imlibImageCopyRect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (fromIntegral dx) (fromIntegral dy)

--Imlib_Color_Range imlib_create_color_range(void);
foreign import ccall "static Imlib2.h imlib_create_color_range" imlibCreateColorRange :: IO ImlibColorRange
createColorRange :: IO ImlibColorRange
createColorRange = imlibCreateColorRange

--void imlib_free_color_range(void);
foreign import ccall "static Imlib2.h imlib_free_color_range" imlibFreeColorRange :: IO ()
freeColorRange :: IO ()
freeColorRange = imlibFreeColorRange

--void imlib_add_color_to_color_range(int distance_away);
foreign import ccall "static Imlib2.h imlib_add_color_to_color_range" imlibAddColorToColorRange :: CInt -> IO ()
addColorToColorRange :: Int -> IO ()
addColorToColorRange = imlibAddColorToColorRange . fromIntegral

--void imlib_image_fill_color_range_rectangle(int x, int y, int width, int height, double angle);
foreign import ccall "static Imlib2.h imlib_image_fill_color_range_rectangle" imlibImageFillColorRangeRectangle :: CInt -> CInt -> CInt -> CInt -> CDouble -> IO ()
imageFillColorRangeRectangle x y w h a = imlibImageFillColorRangeRectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (realToFrac a)

--void imlib_image_fill_hsva_color_range_rectangle(int x, int y, int width, int height, double angle);
foreign import ccall "static Imlib2.h imlib_image_fill_hsva_color_range_rectangle" imlibImageFillHsvaColorRangeRectangle :: CInt -> CInt -> CInt -> CInt -> CDouble -> IO ()
imageFillHsvaColorRangeRectangle x y w h a = imlibImageFillHsvaColorRangeRectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (realToFrac a)

--void imlib_image_query_pixel(int x, int y, Imlib_Color *color_return);
foreign import ccall "static Imlib2.h imlib_image_query_pixel" imlibImageQueryPixel :: CInt -> CInt -> Ptr ImlibColor -> IO ()
imageQueryPixel :: Int -> Int -> IO ImlibColor
imageQueryPixel x y = do
    c <- malloc
    imlibImageQueryPixel (fromIntegral x) (fromIntegral y) c
    r <- peek c
    free c
    return r

--void imlib_image_query_pixel_cmya(int x, int y, int *cyan, int *magenta, int *yellow, int *alpha);
foreign import ccall "static Imlib2.h imlib_image_query_pixel_cmya" imlibImageQueryPixelCmya :: CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
imageQueryPixelCmya :: Int -> Int -> IO (Int, Int, Int, Int)
imageQueryPixelCmya cx cy = do
    [c,m,y,a] <- nmalloc 4
    imlibImageQueryPixelCmya (fromIntegral cx) (fromIntegral cy) c m y a
    [cc,mm,yy,aa] <- liftM (map fromIntegral) $ mapM peek [c,m,y,a]
    mapM free [c,m,y,a]
    return (cc,mm,yy,aa)

--void imlib_image_query_pixel_hsva(int x, int y, float *hue, float *saturation, float *value, int *alpha);
foreign import ccall "static Imlib2.h imlib_image_query_pixel_hsva" imlibImageQueryPixelHsva :: CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
imageQueryPixelHsva :: Int -> Int -> IO (Int, Int, Int, Int)
imageQueryPixelHsva cx cy = do
    [h,s,v,a] <- nmalloc 4
    imlibImageQueryPixelHsva (fromIntegral cx) (fromIntegral cy) h s v a
    [hh,ss,vv,aa] <- liftM (map fromIntegral) $ mapM peek [h,s,v,a]
    mapM free [h,s,v,a]
    return (hh,ss,vv,aa)

--void imlib_image_query_pixel_hlsa(int x, int y, float *hue, float *lightness, float *saturation, int *alpha);
foreign import ccall "static Imlib2.h imlib_image_query_pixel_hlsa" imlibImageQueryPixelHlsa :: CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
imageQueryPixelHlsa :: Int -> Int -> IO (Int, Int, Int, Int)
imageQueryPixelHlsa cx cy = do
    [h,l,s,a] <- nmalloc 4
    imlibImageQueryPixelHlsa (fromIntegral cx) (fromIntegral cy) h l s a
    [hh,ll,ss,aa] <- liftM (map fromIntegral) $ mapM peek [h,l,s,a]
    mapM free [h,l,s,a]
    return (hh,ll,ss,aa)

--void imlib_image_attach_data_value(const char *key, void *data, int value, Imlib_Data_Destructor_Function destructor_function);
foreign import ccall "static Imlib2.h imlib_image_attach_data_value" imlibImageAttachDataValue :: CString -> Ptr () -> Int -> FunPtr ImlibDataDestructorFunction -> IO ()
imageAttachDataValue key d v dest = do
    df <- mkDestructorFunction dest
    withCString key (\k -> imlibImageAttachDataValue k d v df)

--void *imlib_image_get_attached_data(const char *key);
foreign import ccall "static Imlib2.h imlib_image_get_attached_data" imlibImageGetAttachedData :: CString -> IO (Ptr ())
imageGetAttachedData :: String -> IO (Ptr ())
imageGetAttachedData key = withCString key imlibImageGetAttachedData

--int imlib_image_get_attached_value(const char *key);
foreign import ccall "static Imlib2.h imlib_image_get_attached_value" imlibImageGetAttachedValue :: CString -> IO CInt
imageGetAttachedValue :: String -> IO Int
imageGetAttachedValue key = withCString key imlibImageGetAttachedValue >>= return . fromIntegral

--void imlib_image_remove_attached_data_value(const char *key);
foreign import ccall "static Imlib2.h imlib_image_remove_attached_data_value" imlibImageRemoveAttachedDataValue :: CString -> IO ()
imageRemoveAttachedDataValue :: String -> IO ()
imageRemoveAttachedDataValue key = withCString key imlibImageRemoveAttachedDataValue

--void imlib_image_remove_and_free_attached_data_value(const char *key);
foreign import ccall "static Imlib2.h imlib_image_remove_and_free_attached_data_value" imlibImageRemoveAndFreeAttachedDataValue :: CString -> IO ()
imageRemoveAndFreeAttachedDataValue key = withCString key imlibImageRemoveAndFreeAttachedDataValue

--void imlib_save_image(const char *filename);
foreign import ccall "static Imlib2.h imlib_save_image" imlibSaveImage :: CString -> IO ()
saveImage :: String -> IO ()
saveImage str = withCString str imlibSaveImage

--void imlib_save_image_with_error_return(const char *filename, Imlib_Load_Error *error_return);
foreign import ccall "static Imlib2.h imlib_save_image_with_error_return" imlibSaveImageWithErrorReturn :: CString -> Ptr CInt -> IO ()
saveImageWithErrorReturn :: String -> IO ImlibLoadError
saveImageWithErrorReturn str = do
    pe <- malloc
    im <- withCString str (\s -> imlibSaveImageWithErrorReturn s pe) -- Unused?
    e <- peek pe
    free pe
    return (toEnum $ fromIntegral e)

--Imlib_Image imlib_create_rotated_image(double angle);
foreign import ccall "static Imlib2.h imlib_create_rotated_image" imlibCreateRotatedImage :: CDouble -> IO ImlibImage
createRotatedImage :: Double -> IO ImlibImage
createRotatedImage = imlibCreateRotatedImage . realToFrac

--void imlib_blend_image_onto_image_at_angle(Imlib_Image source_image, char merge_alpha, int source_x, int source_y, int source_width, int source_height, int destination_x, int destination_y, int angle_x, int angle_y);
foreign import ccall "static Imlib2.h imlib_blend_image_onto_image_at_angle" imlibBlendImageOntoImageAtAngle :: Ptr(ImlibImage)
                                                                             -> Bool  -- merge alpha
                                                                             -> CInt   -- source x
                                                                             -> CInt   -- source y
                                                                             -> CInt   -- source width
                                                                             -> CInt   -- source height
                                                                             -> CInt   -- dest x
                                                                             -> CInt   -- dest y
                                                                             -> CInt   -- angle x
                                                                             -> CInt   -- angle y
                                                                             -> IO ()
blendImageOntoImageAtAngle ptr mergeAlpha sx sy sw sh dx dy ax ay = imlibBlendImageOntoImageAtAngle ptr mergeAlpha (fromIntegral sx) (fromIntegral sy) (fromIntegral sw) (fromIntegral sh) (fromIntegral dx) (fromIntegral dy) (fromIntegral ax) (fromIntegral ay)

--void imlib_blend_image_onto_image_skewed(Imlib_Image source_image, char merge_alpha, int source_x, int source_y, int source_width, int source_height, int destination_x, int destination_y, int h_angle_x, int h_angle_y, int v_angle_x, int v_angle_y);
foreign import ccall "static Imlib2.h imlib_blend_image_onto_image_skewed" imlibBlendImageOntoImageSkewed :: Ptr(ImlibImage)
                                                                           -> Bool  -- merge alpha
                                                                           -> CInt   -- source x
                                                                           -> CInt   -- source y
                                                                           -> CInt   -- source width
                                                                           -> CInt   -- source height
                                                                           -> CInt   -- dest x
                                                                           -> CInt   -- dest y
                                                                           -> CInt   -- h angle x
                                                                           -> CInt   -- h angle y
                                                                           -> CInt   -- v angle x
                                                                           -> CInt   -- v angle y
                                                                           -> IO ()
blendImageOntoImageSkewed ptr mergeAlpha sx sy sw sh dx dy hx hy vx vy = imlibBlendImageOntoImageSkewed ptr mergeAlpha (fromIntegral sx) (fromIntegral sy) (fromIntegral sw) (fromIntegral sh) (fromIntegral dx) (fromIntegral dy) (fromIntegral hx) (fromIntegral hy) (fromIntegral vx) (fromIntegral vy)

--void imlib_context_set_cliprect(int x, int y, int w, int h);
foreign import ccall "static Imlib2.h imlib_context_set_cliprect" imlibContextSetCliprect :: Int -> Int -> Int -> Int -> IO ()
contextSetCliprect = imlibContextSetCliprect

--int imlib_clip_line(int x0, int y0, int x1, int y1, int xmin, int xmax, int ymin, int ymax, int *clip_x0, int *clip_y0, int *clip_x1, int *clip_y1);
{- -- Apparently this call no longer exists.
foreign import ccall "static Imlib2.h imlib_clip_line" imlibClipLine :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO Int
clipLine x0 y0 x1 y1 xmin xmax ymin ymax = do
    [cx0,cy0,cx1,cy1] <- nmalloc 4
    n <- imlibClipLine x0 y0 x1 y1 xmin xmax ymin ymax cx0 cy0 cx1 cy1
    [rcx0,rcy0,rcx1,rcy1] <- mapM peek [cx0,cy0,cx1,cy1]
    mapM free [cx0,cy0,cx1,cy1]
    return (n,rcx0,rcy0,rcx1,rcy1)
-}

--void imlib_polygon_new(void);
foreign import ccall "static Imlib2.h imlib_polygon_new" imlibPolygonNew :: IO ()
polygonNew :: IO ()
polygonNew = imlibPolygonNew

--void imlib_polygon_free(ImlibPolygon poly);
foreign import ccall "static Imlib2.h imlib_polygon_free" imlibPolygonFree :: ImlibPolygon -> IO ()
polygonFree :: ImlibPolygon -> IO ()
polygonFree = imlibPolygonFree

--void imlib_polygon_add_point(ImlibPolygon poly, int x, int y);
foreign import ccall "static Imlib2.h imlib_polygon_add_point" imlibPolygonAddPoint :: ImlibPolygon -> CInt -> CInt -> IO ()
polygonAddPoint poly x y = imlibPolygonAddPoint poly (fromIntegral x) (fromIntegral y)

--void imlib_image_draw_polygon(ImlibPolygon poly, unsigned char closed);
foreign import ccall "static Imlib2.h imlib_image_draw_polygon" imlibImageDrawPolygon :: ImlibPolygon -> Bool -> IO ()
imageDrawPolygon :: ImlibPolygon -> Bool -> IO ()
imageDrawPolygon = imlibImageDrawPolygon

--void imlib_image_fill_polygon(ImlibPolygon poly);
foreign import ccall "static Imlib2.h imlib_image_fill_polygon" imlibImageFillPolygon :: ImlibPolygon -> IO ()
imageFillPolygon :: ImlibPolygon -> IO ()
imageFillPolygon = imlibImageFillPolygon

--void imlib_polygon_get_bounds(ImlibPolygon poly, int *px1, int *py1, int *px2, int *py2);
foreign import ccall "static Imlib2.h imlib_polygon_get_bounds" imlibPolygonGetBounds :: ImlibPolygon -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
polygonGetBounds poly = do
    [x1,y1,x2,y2] <- nmalloc 4
    imlibPolygonGetBounds poly x1 y1 x2 y2
    [rx1,ry1,rx2,ry2] <- liftM (map fromIntegral) $ mapM peek [x1,y1,x2,y2]
    mapM free [x1,y1,x2,y2]
    return (rx1,ry1,rx2,ry2)

--unsigned char imlib_polygon_contains_point(ImlibPolygon poly, int x, int y);
foreign import ccall "static Imlib2.h imlib_polygon_contains_point" imlibPolygonContainsPoint :: ImlibPolygon -> CInt -> CInt -> IO Bool
polygonContainsPoint poly x y = imlibPolygonContainsPoint poly (fromIntegral x) (fromIntegral y)

--void imlib_image_draw_ellipse(int xc, int yc, int a, int b);
foreign import ccall "static Imlib2.h imlib_image_draw_ellipse" imlibImageDrawEllipse :: CInt -> CInt -> CInt -> CInt -> IO ()
imageDrawEllipse xc yc a b = imlibImageDrawEllipse (fromIntegral xc) (fromIntegral yc) (fromIntegral a) (fromIntegral b)

--void imlib_image_fill_ellipse(int xc, int yc, int a, int b);
foreign import ccall "static Imlib2.h imlib_image_fill_ellipse" imlibImageFillEllipse :: CInt -> CInt -> CInt -> CInt -> IO ()
imageFillEllipse xc yc a b = imlibImageFillEllipse (fromIntegral xc) (fromIntegral yc) (fromIntegral a) (fromIntegral b)

--void imlib_image_filter(void);
foreign import ccall "static Imlib2.h imlib_image_filter" imlibImageFilter :: IO ()
imageFilter :: IO ()
imageFilter = imlibImageFilter

--Imlib_Filter imlib_create_filter(int initsize);
foreign import ccall "static Imlib2.h imlib_create_filter" imlibCreateFilter :: CInt -> IO ImlibFilter
createFilter :: Int -> IO ImlibFilter
createFilter = imlibCreateFilter . fromIntegral

--void imlib_free_filter(void);
foreign import ccall "static Imlib2.h imlib_free_filter" imlibFreeFilter :: IO ()
freeFilter :: IO ()
freeFilter = imlibFreeFilter

--void imlib_filter_set(int xoff, int yoff, int a, int r, int g, int b);
foreign import ccall "static Imlib2.h imlib_filter_set" imlibFilterSet :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
filterSet xoff yoff a r g b = imlibFilterSet (fromIntegral xoff) (fromIntegral yoff) (fromIntegral a) (fromIntegral r) (fromIntegral g) (fromIntegral b)

--void imlib_filter_set_alpha(int xoff, int yoff, int a, int r, int g, int b);
foreign import ccall "static Imlib2.h imlib_filter_set_alpha" imlibFilterSetAlpha :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
filterSetAlpha xoff yoff a r g b = imlibFilterSetAlpha (fromIntegral xoff) (fromIntegral yoff) (fromIntegral a) (fromIntegral r) (fromIntegral g) (fromIntegral b)

--void imlib_filter_set_red(int xoff, int yoff, int a, int r, int g, int b);
foreign import ccall "static Imlib2.h imlib_filter_set_red" imlibFilterSetRed :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
filterSetRed xoff yoff a r g b = imlibFilterSetRed (fromIntegral xoff) (fromIntegral yoff) (fromIntegral a) (fromIntegral r) (fromIntegral g) (fromIntegral b)

--void imlib_filter_set_green(int xoff, int yoff, int a, int r, int g, int b);
foreign import ccall "static Imlib2.h imlib_filter_set_green" imlibFilterSetGreen :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
filterSetGreen xoff yoff a r g b = imlibFilterSetGreen (fromIntegral xoff) (fromIntegral yoff) (fromIntegral a) (fromIntegral r) (fromIntegral g) (fromIntegral b)

--void imlib_filter_set_blue(int xoff, int yoff, int a, int r, int g, int b);
foreign import ccall "static Imlib2.h imlib_filter_set_blue" imlibFilterSetBlue :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
filterSetBlue xoff yoff a r g b = imlibFilterSetBlue (fromIntegral xoff) (fromIntegral yoff) (fromIntegral a) (fromIntegral r) (fromIntegral g) (fromIntegral b)

--void imlib_filter_constants(int a, int r, int g, int b);
foreign import ccall "static Imlib2.h imlib_filter_constants" imlibFilterConstants :: CInt -> CInt -> CInt -> CInt -> IO ()
filterConstants :: Int -> Int -> Int -> Int -> IO ()
filterConstants a r g b = imlibFilterConstants (fromIntegral a) (fromIntegral r) (fromIntegral g) (fromIntegral b)

--void imlib_filter_divisors(int a, int r, int g, int b);
foreign import ccall "static Imlib2.h imlib_filter_divisors" imlibFilterDivisors :: CInt -> CInt -> CInt -> CInt -> IO ()
filterDivisors :: Int -> Int -> Int -> Int -> IO ()
filterDivisors a r g b = imlibFilterDivisors (fromIntegral a) (fromIntegral r) (fromIntegral g) (fromIntegral b)


