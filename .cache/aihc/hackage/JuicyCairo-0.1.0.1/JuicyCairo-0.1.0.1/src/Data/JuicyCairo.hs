{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.JuicyCairo (
	-- * Immutable
	-- ** Cairo Image =\> JuicyPixels Image
	cairoToJuicy,
	cairoArgb32ToJuicyRGBA8, cairoRgb24ToJuicyRGB8,
	cairoA8ToJuicyRGBA8, cairoA8ToJuicyYA8,
	cairoA1ToJuicyRGBA8, cairoA1ToJuicyYA8,
	cairoRgb16_565ToJuicyRGB8, cairoRgb30ToJuicyRGB16,
	-- ** JuicyPixels Image =\> Cairo Image
	juicyToCairo,
	juicyRGBA8ToCairoArgb32, juicyRGB8ToCairoRgb24,
	juicyRGBA8ToCairoA8, juicyYA8ToCairoA8,
	juicyRGBA8ToCairoA1, juicyYA8ToCairoA1,
	juicyRGB8ToCairoRgb16_565, juicyRGB16ToCairoRgb30,

	-- * Mutable
	-- ** Cairo Mutable Image =\> JuicyPixels Image
	cairoMutToJuicy,
	cairoArgb32MutToJuicyRGBA8, cairoRgb24MutToJuicyRGB8,
	cairoA8MutToJuicyRGBA8, cairoA8MutToJuicyYA8,
	cairoA1MutToJuicyRGBA8, cairoA1MutToJuicyYA8,
	cairoRgb16_565MutToJuicyRGB8, cairoRgb30MutToJuicyRGB16,
	-- ** JuicyPixels Image =\> Cairo Mutable Image
	juicyToCairoMut,
	juicyRGBA8ToCairoArgb32Mut, juicyRGB8ToCairoRgb24Mut,
	juicyRGBA8ToCairoA8Mut, juicyYA8ToCairoA8Mut,
	juicyRGBA8ToCairoA1Mut, juicyYA8ToCairoA1Mut,
	juicyRGB8ToCairoRgb16_565Mut, juicyRGB16ToCairoRgb30Mut ) where

import Control.Arrow ((***))
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (for_)
import Data.Bool (bool)
import Data.Maybe (fromJust)
import Data.Word (Word8)

import qualified Data.CairoImage as C
import qualified Codec.Picture as J

---------------------------------------------------------------------------

-- * CAIRO IMAGE => JUICY IMAGE
-- * JUICY IMAGE => CAIRO IMAGE
-- * CAIRO MUTABLE IMAGE => JUICY IMAGE
-- * JUICY IMAGE => CAIRO MUTABLE IMAGE
-- * CAIRO PIXEL => JUICY PIXEL
-- * JUICY PIXEL => CAIRO PIXEL

---------------------------------------------------------------------------
-- CAIRO IMAGE => JUICY IMAGE
---------------------------------------------------------------------------

cairoToJuicy :: (C.Image i, J.Pixel p) => (C.Pixel i -> p) -> i -> J.Image p
cairoToJuicy c i = (uncurry . J.generateImage)
	(\(fromIntegral -> x) (fromIntegral -> y) ->
		c . fromJust $ C.pixelAt i x y)
	(fromIntegral *** fromIntegral $ C.imageSize i)

cairoArgb32ToJuicyRGBA8 :: C.Argb32 -> J.Image J.PixelRGBA8
cairoArgb32ToJuicyRGBA8 = cairoToJuicy pixelArgb32ToPixelRGBA8

cairoRgb24ToJuicyRGB8 :: C.Rgb24 -> J.Image J.PixelRGB8
cairoRgb24ToJuicyRGB8 = cairoToJuicy pixelRgb24ToPixelRGB8

cairoA8ToJuicyRGBA8 :: Word8 -> Word8 -> Word8 -> C.A8 -> J.Image J.PixelRGBA8
cairoA8ToJuicyRGBA8 r g b = cairoToJuicy $ pixelA8ToPixelRGBA8 r g b

cairoA8ToJuicyYA8 :: Word8 -> C.A8 -> J.Image J.PixelYA8
cairoA8ToJuicyYA8 = cairoToJuicy . pixelA8ToPixelYA8

cairoA1ToJuicyRGBA8 :: Word8 -> Word8 -> Word8 -> C.A1 -> J.Image J.PixelRGBA8
cairoA1ToJuicyRGBA8 r g b = cairoToJuicy $ pixelA1ToPixelRGBA8 r g b

cairoA1ToJuicyYA8 :: Word8 -> C.A1 -> J.Image J.PixelYA8
cairoA1ToJuicyYA8 = cairoToJuicy . pixelA1ToPixelYA8

cairoRgb16_565ToJuicyRGB8 :: C.Rgb16_565 -> J.Image J.PixelRGB8
cairoRgb16_565ToJuicyRGB8 = cairoToJuicy pixelRgb16_565ToPixelRGB8

cairoRgb30ToJuicyRGB16 :: C.Rgb30 -> J.Image J.PixelRGB16
cairoRgb30ToJuicyRGB16 = cairoToJuicy pixelRgb30ToPixelRGB16

---------------------------------------------------------------------------
-- JUICY IMAGE => CAIRO IMAGE
---------------------------------------------------------------------------

juicyToCairo :: (J.Pixel p, C.Image i) => (p -> C.Pixel i) -> J.Image p -> i
juicyToCairo c i = C.generateImage
	(fromIntegral $ J.imageWidth i) (fromIntegral $ J.imageHeight i)
	\(fromIntegral -> x) (fromIntegral -> y) -> c $ J.pixelAt i x y

juicyRGBA8ToCairoArgb32 :: J.Image J.PixelRGBA8 -> C.Argb32
juicyRGBA8ToCairoArgb32 = juicyToCairo pixelRGBA8ToPixelArgb32

juicyRGB8ToCairoRgb24 :: J.Image J.PixelRGB8 -> C.Rgb24
juicyRGB8ToCairoRgb24 = juicyToCairo pixelRGB8ToPixelRgb24

juicyRGBA8ToCairoA8 :: J.Image J.PixelRGBA8 -> C.A8
juicyRGBA8ToCairoA8 = juicyToCairo pixelRGBA8ToPixelA8

juicyYA8ToCairoA8 :: J.Image J.PixelYA8 -> C.A8
juicyYA8ToCairoA8 = juicyToCairo pixelYA8ToPixelA8

juicyRGBA8ToCairoA1 :: Word8 -> J.Image J.PixelRGBA8 -> C.A1
juicyRGBA8ToCairoA1 = juicyToCairo . pixelRGBA8ToPixelA1

juicyYA8ToCairoA1 :: Word8 -> J.Image J.PixelYA8 -> C.A1
juicyYA8ToCairoA1 = juicyToCairo . pixelYA8ToPixelA1

juicyRGB8ToCairoRgb16_565 :: J.Image J.PixelRGB8 -> C.Rgb16_565
juicyRGB8ToCairoRgb16_565 = juicyToCairo pixelRGB8ToPixelRgb16_565

juicyRGB16ToCairoRgb30 :: J.Image J.PixelRGB16 -> C.Rgb30
juicyRGB16ToCairoRgb30 = juicyToCairo pixelRGB16ToPixelRgb30

---------------------------------------------------------------------------
-- CAIRO MUTABLE IMAGE => JUICY IMAGE
---------------------------------------------------------------------------

cairoMutToJuicy :: (PrimMonad m, C.ImageMut im, J.Pixel p) =>
	(C.PixelMut im -> p) -> im (PrimState m) -> m (J.Image p)
cairoMutToJuicy c i = uncurry J.withImage
	(fromIntegral *** fromIntegral $ C.imageMutSize i)
	\(fromIntegral -> x) (fromIntegral -> y) ->
		c . fromJust <$> C.getPixel i x y

cairoArgb32MutToJuicyRGBA8 :: PrimMonad m =>
	C.Argb32Mut (PrimState m) -> m (J.Image J.PixelRGBA8)
cairoArgb32MutToJuicyRGBA8 = cairoMutToJuicy pixelArgb32ToPixelRGBA8

cairoRgb24MutToJuicyRGB8 :: PrimMonad m =>
	C.Rgb24Mut (PrimState m) -> m (J.Image J.PixelRGB8)
cairoRgb24MutToJuicyRGB8 = cairoMutToJuicy pixelRgb24ToPixelRGB8

cairoA8MutToJuicyRGBA8 :: PrimMonad m => Word8 -> Word8 -> Word8 ->
	C.A8Mut (PrimState m) -> m (J.Image J.PixelRGBA8)
cairoA8MutToJuicyRGBA8 r g b = cairoMutToJuicy $ pixelA8ToPixelRGBA8 r g b

cairoA8MutToJuicyYA8 :: PrimMonad m =>
	Word8 -> C.A8Mut (PrimState m) -> m (J.Image J.PixelYA8)
cairoA8MutToJuicyYA8 = cairoMutToJuicy . pixelA8ToPixelYA8

cairoA1MutToJuicyRGBA8 :: PrimMonad m => Word8 -> Word8 -> Word8 ->
	C.A1Mut (PrimState m) -> m (J.Image J.PixelRGBA8)
cairoA1MutToJuicyRGBA8 r g b = cairoMutToJuicy $ pixelA1ToPixelRGBA8 r g b

cairoA1MutToJuicyYA8 :: PrimMonad m =>
	Word8 -> C.A1Mut (PrimState m) -> m (J.Image J.PixelYA8)
cairoA1MutToJuicyYA8 = cairoMutToJuicy . pixelA1ToPixelYA8

cairoRgb16_565MutToJuicyRGB8 :: PrimMonad m =>
	C.Rgb16_565Mut (PrimState m) -> m (J.Image J.PixelRGB8)
cairoRgb16_565MutToJuicyRGB8 = cairoMutToJuicy pixelRgb16_565ToPixelRGB8

cairoRgb30MutToJuicyRGB16 :: PrimMonad m =>
	C.Rgb30Mut (PrimState m) -> m (J.Image J.PixelRGB16)
cairoRgb30MutToJuicyRGB16 = cairoMutToJuicy pixelRgb30ToPixelRGB16

---------------------------------------------------------------------------
-- JUICY IMAGE => CAIRO MUTABLE IMAGE
---------------------------------------------------------------------------

juicyToCairoMut :: (PrimMonad m, J.Pixel p, C.ImageMut im) =>
	(p -> C.PixelMut im) -> J.Image p -> m (im (PrimState m))
juicyToCairoMut c i = C.newImageMut w' h' >>= \im ->
	im <$ for_ ys \(y, y') -> for_ xs \(x, x') ->
		C.putPixel im x' y' $ c (J.pixelAt i x y)
	where
	w = J.imageWidth i; h = J.imageHeight i
	w' = fromIntegral w; h' = fromIntegral h
	xs = zip [0 .. w] [0 .. w']; ys = zip [0 .. h] [0 .. h']

juicyRGBA8ToCairoArgb32Mut :: PrimMonad m =>
	J.Image J.PixelRGBA8 -> m (C.Argb32Mut (PrimState m))
juicyRGBA8ToCairoArgb32Mut = juicyToCairoMut pixelRGBA8ToPixelArgb32

juicyRGB8ToCairoRgb24Mut :: PrimMonad m =>
	J.Image J.PixelRGB8 -> m (C.Rgb24Mut (PrimState m))
juicyRGB8ToCairoRgb24Mut = juicyToCairoMut pixelRGB8ToPixelRgb24

juicyRGBA8ToCairoA8Mut :: PrimMonad m =>
	J.Image J.PixelRGBA8 -> m (C.A8Mut (PrimState m))
juicyRGBA8ToCairoA8Mut = juicyToCairoMut pixelRGBA8ToPixelA8

juicyYA8ToCairoA8Mut :: PrimMonad m =>
	J.Image J.PixelYA8 -> m (C.A8Mut (PrimState m))
juicyYA8ToCairoA8Mut = juicyToCairoMut pixelYA8ToPixelA8

juicyRGBA8ToCairoA1Mut :: PrimMonad m =>
	Word8 -> J.Image J.PixelRGBA8 -> m (C.A1Mut (PrimState m))
juicyRGBA8ToCairoA1Mut = juicyToCairoMut . pixelRGBA8ToPixelA1

juicyYA8ToCairoA1Mut :: PrimMonad m =>
	Word8 -> J.Image J.PixelYA8 -> m (C.A1Mut (PrimState m))
juicyYA8ToCairoA1Mut = juicyToCairoMut . pixelYA8ToPixelA1

juicyRGB8ToCairoRgb16_565Mut :: PrimMonad m =>
	J.Image J.PixelRGB8 -> m (C.Rgb16_565Mut (PrimState m))
juicyRGB8ToCairoRgb16_565Mut = juicyToCairoMut pixelRGB8ToPixelRgb16_565

juicyRGB16ToCairoRgb30Mut :: PrimMonad m =>
	J.Image J.PixelRGB16 -> m (C.Rgb30Mut (PrimState m))
juicyRGB16ToCairoRgb30Mut = juicyToCairoMut pixelRGB16ToPixelRgb30

---------------------------------------------------------------------------
-- CAIRO PIXEL => JUICY PIXEL
---------------------------------------------------------------------------

pixelArgb32ToPixelRGBA8 :: C.PixelArgb32 -> J.PixelRGBA8
pixelArgb32ToPixelRGBA8 (C.PixelArgb32Straight a r g b) = J.PixelRGBA8 r g b a

pixelRgb24ToPixelRGB8 :: C.PixelRgb24 -> J.PixelRGB8
pixelRgb24ToPixelRGB8 (C.PixelRgb24 r g b) = J.PixelRGB8 r g b

pixelA8ToPixelRGBA8 :: Word8 -> Word8 -> Word8 -> C.PixelA8 -> J.PixelRGBA8
pixelA8ToPixelRGBA8 r g b (C.PixelA8 a) = J.PixelRGBA8 r g b a

pixelA8ToPixelYA8 :: Word8 -> C.PixelA8 -> J.PixelYA8
pixelA8ToPixelYA8 y (C.PixelA8 a) = J.PixelYA8 y a

pixelA1ToPixelRGBA8 :: Word8 -> Word8 -> Word8 -> C.PixelA1 -> J.PixelRGBA8
pixelA1ToPixelRGBA8 r g b (C.PixelA1 a) = J.PixelRGBA8 r g b $ C.bit 0x00 0xff a

pixelA1ToPixelYA8 :: Word8 -> C.PixelA1 -> J.PixelYA8
pixelA1ToPixelYA8 y (C.PixelA1 a) = J.PixelYA8 y $ C.bit 0x00 0xff a

pixelRgb16_565ToPixelRGB8 :: C.PixelRgb16_565 -> J.PixelRGB8
pixelRgb16_565ToPixelRGB8 (C.PixelRgb16_565 r g b) = J.PixelRGB8 r g b

pixelRgb30ToPixelRGB16 :: C.PixelRgb30 -> J.PixelRGB16
pixelRgb30ToPixelRGB16 (C.PixelRgb30 r g b) = J.PixelRGB16 r g b

---------------------------------------------------------------------------
-- JUICY PIXEL => CAIRO PIXEL
---------------------------------------------------------------------------

pixelRGBA8ToPixelArgb32 :: J.PixelRGBA8 -> C.PixelArgb32
pixelRGBA8ToPixelArgb32 (J.PixelRGBA8 r g b a) = C.PixelArgb32Straight a r g b

pixelRGB8ToPixelRgb24 :: J.PixelRGB8 -> C.PixelRgb24
pixelRGB8ToPixelRgb24 (J.PixelRGB8 r g b) = C.PixelRgb24 r g b

pixelRGBA8ToPixelA8 :: J.PixelRGBA8 -> C.PixelA8
pixelRGBA8ToPixelA8 (J.PixelRGBA8 _ _ _ a) = C.PixelA8 a

pixelYA8ToPixelA8 :: J.PixelYA8 -> C.PixelA8
pixelYA8ToPixelA8 (J.PixelYA8 _ a) = C.PixelA8 a

pixelRGBA8ToPixelA1 :: Word8 -> J.PixelRGBA8 -> C.PixelA1
pixelRGBA8ToPixelA1 t (J.PixelRGBA8 _ _ _ a) = C.PixelA1 . bool C.O C.I $ t <= a

pixelYA8ToPixelA1 :: Word8 -> J.PixelYA8 -> C.PixelA1
pixelYA8ToPixelA1 t (J.PixelYA8 _ a) = C.PixelA1 . bool C.O C.I $ t <= a

pixelRGB8ToPixelRgb16_565 :: J.PixelRGB8 -> C.PixelRgb16_565
pixelRGB8ToPixelRgb16_565 (J.PixelRGB8 r g b) = C.PixelRgb16_565 r g b

pixelRGB16ToPixelRgb30 :: J.PixelRGB16 -> C.PixelRgb30
pixelRGB16ToPixelRgb30 (J.PixelRGB16 r g b) = C.PixelRgb30 r g b
