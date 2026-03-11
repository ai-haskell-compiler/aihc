{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage.Internal (
	-- * Class Image and ImageMut
	Image(..), ImageMut(..),
	-- * Type CairoImage and CairoImageMut
	CairoImage(..), CairoImageMut(..), cairoImageFreeze, cairoImageThaw,
	-- * Image Format
	-- ** ARGB 32
	PixelArgb32(..),
	pattern PixelArgb32Premultiplied, pixelArgb32Premultiplied,
	pattern PixelArgb32Straight,
	pattern CairoImageArgb32, Argb32,
	pattern CairoImageMutArgb32, Argb32Mut,
	-- ** RGB 24
	PixelRgb24(..), pattern PixelRgb24,
	pattern CairoImageRgb24, Rgb24,
	pattern CairoImageMutRgb24, Rgb24Mut,
	-- ** A 8
	PixelA8(..),
	pattern CairoImageA8, A8,
	pattern CairoImageMutA8, A8Mut,
	-- ** A 1
	PixelA1(..), Bit(..), bit,
	pattern CairoImageA1, A1,
	pattern CairoImageMutA1, A1Mut,
	-- ** RGB 16 565
	PixelRgb16_565(..), pattern PixelRgb16_565,
	pattern CairoImageRgb16_565, Rgb16_565,
	pattern CairoImageMutRgb16_565, Rgb16_565Mut,
	-- ** RGB 30
	PixelRgb30(..), pattern PixelRgb30,
	pattern CairoImageRgb30, Rgb30,
	pattern CairoImageMutRgb30, Rgb30Mut,

	-- * CairoFormatT
	CairoFormatT(..),
	pattern CairoFormatArgb32, pattern CairoFormatRgb24,
	pattern CairoFormatA8, pattern CairoFormatA1,
	pattern CairoFormatRgb16_565, pattern CairoFormatRgb30 ) where

import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal (mallocBytes, free, copyBytes)
import Foreign.Storable (Storable, peek, poke)
import Foreign.C.Types (CInt(..), CUChar)
import Control.Monad.Primitive (
	PrimMonad(..), PrimBase, unsafeIOToPrim, unsafePrimToIO )
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.List (foldl1')
import Data.Bool (bool)
import Data.Bits ((.|.), testBit, clearBit, setBit, shiftL, shiftR)
import Data.Word (Word8, Word16, Word32)
import Data.CairoImage.Parts
import System.IO.Unsafe (unsafePerformIO)
import System.TargetEndian (endian)

---------------------------------------------------------------------------

-- * CLASS IMAGE AND IMAGE MUTABLE
-- * TYPE CAIRO IMAGE AND CAIRO IMAGE MUTABLE
-- * ARGB 32
--	+ PIXEL
--	+ IMAGE
--	+ IMAGE MUTABLE
-- * RGB 24
--	+ PIXEL
--	+ IMAGE
--	+ IMAGE MUTABLE
-- * A 8
--	+ PIXEL
--	+ IMAGE
--	+ IMAGE MUTABLE
-- * A 1
--	+ PIXEL
--	+ IMAGE
--	+ IMAGE MUTABLE
-- * RGB 16 565
--	+ PIXEL
--	+ IMAGE
--	+ IMAGE MUTABLE
-- * RGB 30
--	+ PIXEL
--	+ IMAGE
--	+ IMAGE MUTABLE
-- * COMMON

---------------------------------------------------------------------------
-- CLASS IMAGE AND IMAGE MUTABLE
---------------------------------------------------------------------------

class Image i where
	type Pixel i
	imageSize :: i -> (CInt, CInt)
	pixelAt :: i -> CInt -> CInt -> Maybe (Pixel i)
	generateImage :: CInt -> CInt -> (CInt -> CInt -> Pixel i) -> i
	generateImagePrimM :: PrimBase m =>
		CInt -> CInt -> (CInt -> CInt -> m (Pixel i)) -> m i

	generateImage w h f =
		runST $ generateImagePrimM w h \x y -> pure $ f x y

class ImageMut im where
	type PixelMut im
	imageMutSize :: im s -> (CInt, CInt)
	getPixel :: PrimMonad m =>
		im (PrimState m) -> CInt -> CInt -> m (Maybe (PixelMut im))
	putPixel :: PrimMonad m =>
		im (PrimState m) -> CInt -> CInt -> PixelMut im -> m ()
	newImageMut :: PrimMonad m => CInt -> CInt -> m (im (PrimState m))

---------------------------------------------------------------------------
-- TYPE CAIRO IMAGE AND CAIRO IMAGE MUTABLE
---------------------------------------------------------------------------

data CairoImage = CairoImage {
	cairoImageFormat :: CairoFormatT,
	cairoImageWidth :: CInt, cairoImageHeight :: CInt,
	cairoImageStride :: CInt, cairoImageData :: ForeignPtr CUChar }
	deriving Show

instance Eq CairoImage where
	ci1 == ci2 = and [
		fmt1 == fmt2, w1 == w2, h1 == h2, str1 == str2,
		unsafePerformIO $ with fd1 \d1 -> with fd2 \d2 ->
			(EQ ==) <$> compareBytes d1 d2 (str1 * h1) ]
		where
		[fmt1, fmt2] = cairoImageFormat <$> [ci1, ci2]
		[w1, w2] = cairoImageWidth <$> [ci1, ci2]
		[h1, h2] = cairoImageHeight <$> [ci1, ci2]
		[str1, str2] = cairoImageStride <$> [ci1, ci2]
		[fd1, fd2] = cairoImageData <$> [ci1, ci2]

compareBytes :: (Ord n, Num n) => Ptr a -> Ptr a -> n -> IO Ordering
compareBytes _ _ n | n < 1 = pure EQ
compareBytes p1 p2 _ | p1 == p2 = pure EQ
compareBytes p1 p2 n = compare <$> peek pb1 <*> peek pb2 >>= \case
	EQ -> compareBytes p1 p2 (n - 1); o -> pure o
	where [pb1, pb2] = castPtr <$> [p1, p2] :: [Ptr Word8]

data CairoImageMut s = CairoImageMut {
	cairoImageMutFormat :: CairoFormatT,
	cairoImageMutWidth :: CInt, cairoImageMutHeight :: CInt,
	cairoImageMutStride :: CInt, cairoImageMutData :: ForeignPtr CUChar }
	deriving Show

cairoImageFreeze :: PrimMonad m => CairoImageMut (PrimState m) -> m CairoImage
cairoImageFreeze im = unsafeIOToPrim $ CairoImage f w h st <$> cidClone st h dt
	where
	f = cairoImageMutFormat im
	w = cairoImageMutWidth im; h = cairoImageMutHeight im
	st = cairoImageMutStride im; dt = cairoImageMutData im

cairoImageThaw :: PrimMonad m => CairoImage -> m (CairoImageMut (PrimState m))
cairoImageThaw i = unsafeIOToPrim $ CairoImageMut f w h st <$> cidClone st h dt
	where
	f = cairoImageFormat i
	w = cairoImageWidth i; h = cairoImageHeight i
	st = cairoImageStride i; dt = cairoImageData i

cidClone :: CInt -> CInt -> ForeignPtr CUChar -> IO (ForeignPtr CUChar)
cidClone st h fd = with fd \d -> mallocBytes n >>= \d' ->
	copyBytes d' d n >> newForeignPtr d' (free d')
	where n = fromIntegral $ st * h

---------------------------------------------------------------------------
-- ARGB 32
---------------------------------------------------------------------------

-- PIXEL

newtype PixelArgb32 = PixelArgb32Word32 Word32 deriving (Show, Storable)

{-# COMPLETE PixelArgb32Premultiplied #-}

pattern PixelArgb32Premultiplied ::
	Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pattern PixelArgb32Premultiplied a r g b <- (pixelArgb32ToArgb -> (a, r, g, b))

pixelArgb32Premultiplied ::
	Word8 -> Word8 -> Word8 -> Word8 -> Maybe PixelArgb32
pixelArgb32Premultiplied a r g b
	| r <= a, g <= a, b <= a = Just $ pixelArgb32FromArgb a r g b
	| otherwise = Nothing

pixelArgb32FromArgb :: Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pixelArgb32FromArgb
	(fromIntegral -> a) (fromIntegral -> r)
	(fromIntegral -> g) (fromIntegral -> b) = PixelArgb32Word32
	. foldl1' (.|.) $ zipWith shiftL [a, r, g, b] [24, 16, 8, 0]

pixelArgb32ToArgb :: PixelArgb32 -> (Word8, Word8, Word8, Word8)
pixelArgb32ToArgb (PixelArgb32Word32 w) = (
	fromIntegral $ w `shiftR` 24, fromIntegral $ w `shiftR` 16,
	fromIntegral $ w `shiftR` 8, fromIntegral w )

{-# COMPLETE PixelArgb32Straight #-}

pattern PixelArgb32Straight :: Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pattern PixelArgb32Straight a r g b <- (pixelArgb32ToArgbSt -> (a, r, g, b))
	where PixelArgb32Straight a r g b = pixelArgb32FromArgb
		a (r `unit` (a, 0xff)) (g `unit` (a, 0xff)) (b `unit` (a, 0xff))

pixelArgb32ToArgbSt :: PixelArgb32 -> (Word8, Word8, Word8, Word8)
pixelArgb32ToArgbSt p = let (a, r, g, b) = pixelArgb32ToArgb p in
	(a, r `unit` (0xff, a), g `unit` (0xff, a), b `unit` (0xff, a))

unit :: Word8 -> (Word8, Word8) -> Word8
(fromIntegral -> n) `unit` ((fromIntegral -> m), (fromIntegral -> d)) =
	fromIntegral @Word16 $ n * m `div'` d

infixl 7 `div'`

div' :: Integral n => n -> n -> n
div' n = \case 0 -> 0; m -> n `div` m

-- IMAGE

data Argb32 = Argb32 {
	argb32Width :: CInt, argb32Height :: CInt,
	argb32Stride :: CInt, argb32Data :: ForeignPtr PixelArgb32 }
	deriving Show

pattern CairoImageArgb32 :: Argb32 -> CairoImage
pattern CairoImageArgb32 a <- (cairoImageToArgb32 -> Just a) where
	CairoImageArgb32 (Argb32 w h s d) =
		CairoImage CairoFormatArgb32 w h s $ castForeignPtr d

cairoImageToArgb32 :: CairoImage -> Maybe Argb32
cairoImageToArgb32 = \case
	CairoImage CairoFormatArgb32 w h s d ->
		Just . Argb32 w h s $ castForeignPtr d
	_ -> Nothing

instance Image Argb32 where
	type Pixel Argb32 = PixelArgb32
	imageSize (Argb32 w h _ _) = (w, h)
	pixelAt (Argb32 w h s d) x y = unsafePerformIO $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	generateImagePrimM w h f = stride CairoFormatArgb32 w >>= \s ->
		Argb32 w h s <$> gen w h s f

-- IMAGE MUTABLE

data Argb32Mut s = Argb32Mut {
	argb32MutWidth :: CInt, argb32MutHeight :: CInt,
	argb32MutStride :: CInt, argb32MutData :: ForeignPtr PixelArgb32 }
	deriving Show

pattern CairoImageMutArgb32 :: Argb32Mut s -> CairoImageMut s
pattern CairoImageMutArgb32 a <- (cairoImageMutToArgb32 -> Just a) where
	CairoImageMutArgb32 (Argb32Mut w h s d) =
		CairoImageMut CairoFormatArgb32 w h s $ castForeignPtr d

cairoImageMutToArgb32 :: CairoImageMut s -> Maybe (Argb32Mut s)
cairoImageMutToArgb32 = \case
	CairoImageMut CairoFormatArgb32 w h s d ->
		Just . Argb32Mut w h s $ castForeignPtr d
	_ -> Nothing

instance ImageMut Argb32Mut where
	type PixelMut Argb32Mut = PixelArgb32
	imageMutSize (Argb32Mut w h _ _) = (w, h)
	getPixel (Argb32Mut w h s d) x y = unsafeIOToPrim $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	putPixel (Argb32Mut w h s d) x y px = unsafeIOToPrim $ with d \p ->
		maybe (pure ()) (`poke` px) $ ptr w h s p x y
	newImageMut w h =
		stride CairoFormatArgb32 w >>= \s -> Argb32Mut w h s <$> new s h

---------------------------------------------------------------------------
-- RGB 24
---------------------------------------------------------------------------

-- PIXEL

newtype PixelRgb24 = PixelRgb24Word32 Word32 deriving (Show, Storable)

{-# COMPLETE PixelRgb24 #-}

pattern PixelRgb24 :: Word8 -> Word8 -> Word8 -> PixelRgb24
pattern PixelRgb24 r g b <- (pixelRgb24ToRgb -> (r, g, b)) where
	PixelRgb24 (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) =
		PixelRgb24Word32
			. foldl1' (.|.) $ zipWith shiftL [r, g, b] [16, 8, 0]

pixelRgb24ToRgb :: PixelRgb24 -> (Word8, Word8, Word8)
pixelRgb24ToRgb (PixelRgb24Word32 w) = (
	fromIntegral $ w `shiftR` 16,
	fromIntegral $ w `shiftR` 8, fromIntegral w )

-- IMAGE

data Rgb24 = Rgb24 {
	rgb24Width :: CInt, rgb24Height :: CInt,
	rgb24Stride :: CInt, rgb24Data :: ForeignPtr PixelRgb24 }
	deriving Show

pattern CairoImageRgb24 :: Rgb24 -> CairoImage
pattern CairoImageRgb24 r <- (cairoImageToRgb24 -> Just r) where
	CairoImageRgb24 (Rgb24 w h s d) =
		CairoImage CairoFormatRgb24 w h s $ castForeignPtr d

cairoImageToRgb24 :: CairoImage -> Maybe Rgb24
cairoImageToRgb24 = \case
	CairoImage CairoFormatRgb24 w h s d ->
		Just . Rgb24 w h s $ castForeignPtr d
	_ -> Nothing

instance Image Rgb24 where
	type Pixel Rgb24 = PixelRgb24
	imageSize (Rgb24 w h _ _) = (w, h)
	pixelAt (Rgb24 w h s d) x y = unsafePerformIO $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	generateImagePrimM w h f =
		stride CairoFormatRgb24 w >>= \s -> Rgb24 w h s <$> gen w h s f

-- IMAGE MUTABLE

data Rgb24Mut s = Rgb24Mut {
	rgb24MutWidth :: CInt, rgb24MutHeight :: CInt,
	rgb24MutStride :: CInt, rgb24MutData :: ForeignPtr PixelRgb24 }
	deriving Show

pattern CairoImageMutRgb24 :: Rgb24Mut s -> CairoImageMut s
pattern CairoImageMutRgb24 r <- (cairoImageMutToRgb24 -> Just r) where
	CairoImageMutRgb24 (Rgb24Mut w h s d) =
		CairoImageMut CairoFormatRgb24 w h s $ castForeignPtr d

cairoImageMutToRgb24 :: CairoImageMut s -> Maybe (Rgb24Mut s)
cairoImageMutToRgb24 = \case
	CairoImageMut CairoFormatRgb24 w h s d ->
		Just . Rgb24Mut w h s $ castForeignPtr d
	_ -> Nothing

instance ImageMut Rgb24Mut where
	type PixelMut Rgb24Mut = PixelRgb24
	imageMutSize (Rgb24Mut w h _ _) = (w, h)
	getPixel (Rgb24Mut w h s d) x y = unsafeIOToPrim $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	putPixel (Rgb24Mut w h s d) x y px = unsafeIOToPrim $ with d \p ->
		maybe (pure ()) (`poke` px) $ ptr w h s p x y
	newImageMut w h =
		stride CairoFormatRgb24 w >>= \s -> Rgb24Mut w h s <$> new s h

---------------------------------------------------------------------------
-- A 8
---------------------------------------------------------------------------

-- PIXEL

newtype PixelA8 = PixelA8 Word8 deriving (Show, Storable)

-- IMAGE

data A8 = A8 {
	a8Width :: CInt, a8Height :: CInt,
	a8Stride :: CInt, a8Data :: ForeignPtr PixelA8 }
	deriving Show

pattern CairoImageA8 :: A8 -> CairoImage
pattern CairoImageA8 a <- (cairoImageToA8 -> Just a)
	where CairoImageA8 (A8 w h s d) =
		CairoImage CairoFormatA8 w h s $ castForeignPtr d

cairoImageToA8 :: CairoImage -> Maybe A8
cairoImageToA8 = \case
	CairoImage CairoFormatA8 w h s d -> Just . A8 w h s $ castForeignPtr d
	_ -> Nothing

instance Image A8 where
	type Pixel A8 = PixelA8
	imageSize (A8 w h _ _) = (w, h)
	pixelAt (A8 w h s d) x y = unsafePerformIO $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	generateImagePrimM w h f =
		stride CairoFormatA8 w >>= \s -> A8 w h s <$> gen w h s f

-- IMAGE MUTABLE

data A8Mut s = A8Mut {
	a8MutWidth :: CInt, a8MutHeight :: CInt,
	a8MutStride :: CInt, a8MutData :: ForeignPtr PixelA8 }
	deriving Show

pattern CairoImageMutA8 :: A8Mut s -> CairoImageMut s
pattern CairoImageMutA8 a <- (cairoImageMutToA8 -> Just a)
	where CairoImageMutA8 (A8Mut w h s d) =
		CairoImageMut CairoFormatA8 w h s $ castForeignPtr d

cairoImageMutToA8 :: CairoImageMut s -> Maybe (A8Mut s)
cairoImageMutToA8 = \case
	CairoImageMut CairoFormatA8 w h s d ->
		Just . A8Mut w h s $ castForeignPtr d
	_ -> Nothing

instance ImageMut A8Mut where
	type PixelMut A8Mut = PixelA8
	imageMutSize (A8Mut w h _ _) = (w, h)
	getPixel (A8Mut w h s d) x y = unsafeIOToPrim $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	putPixel (A8Mut w h s d) x y px = unsafeIOToPrim $ with d \p ->
		maybe (pure ()) (`poke` px) $ ptr w h s p x y
	newImageMut w h =
		stride CairoFormatA8 w >>= \s -> A8Mut w h s <$> new s h

---------------------------------------------------------------------------
-- A 1
---------------------------------------------------------------------------

-- PIXEL

newtype PixelA1 = PixelA1 Bit deriving Show

data Bit = O | I deriving (Show, Enum)

bit :: a -> a -> Bit -> a
bit x y = \case O -> x; I -> y

ptrA1 :: CInt -> CInt -> CInt ->
	Ptr PixelA1 -> CInt -> CInt -> Maybe (Ptr PixelA1, CInt)
ptrA1 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just
		(p `plusPtr` fromIntegral (y * s + x `div` 32 * 4), x `mod` 32)
	| otherwise = Nothing

peA1 :: (Ptr PixelA1, CInt) -> IO PixelA1
peA1 ((castPtr -> p), (fromIntegral . $(endian [e| id |] [e| (31 -) |]) -> i)) =
	PixelA1 . bool O I . (`testBit` i) <$> peek @Word32 p

poA1 :: (Ptr PixelA1, CInt) -> PixelA1 -> IO ()
poA1 ((castPtr -> p), (fromIntegral . $(endian [e| id |] [e| (31 -) |]) -> i))
	(PixelA1 b) = poke p . flip (bit clearBit setBit b) i =<< peek @Word32 p

-- IMAGE

data A1 = A1 {
	a1Width :: CInt, a1Height :: CInt,
	a1Stride :: CInt, a1Data :: ForeignPtr PixelA1 }
	deriving Show

pattern CairoImageA1 :: A1 -> CairoImage
pattern CairoImageA1 a <- (cairoImageToA1 -> Just a)
	where CairoImageA1 (A1 w h s d) =
		CairoImage CairoFormatA1 w h s $ castForeignPtr d

cairoImageToA1 :: CairoImage -> Maybe A1
cairoImageToA1 = \case
	CairoImage CairoFormatA1 w h s d -> Just . A1 w h s $ castForeignPtr d
	_ -> Nothing

instance Image A1 where
	type Pixel A1 = PixelA1
	imageSize (A1 w h _ _) = (w, h)
	pixelAt (A1 w h s d) x y = unsafePerformIO $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peA1) $ ptrA1 w h s p x y
	generateImagePrimM = genA1

genA1 :: PrimBase m => CInt -> CInt -> (CInt -> CInt -> m PixelA1) -> m A1
genA1 w h f = unsafeIOToPrim $ stride CairoFormatA1 w >>= \s -> do
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x ->
		unsafePrimToIO (f x y) >>= \px ->
			maybe (pure ()) (`poA1` px) $ ptrA1 w h s d x y
	A1 w h s <$> newForeignPtr d (free d)

-- IMAGE MUTABLE

data A1Mut s = A1Mut {
	a1MutWidth :: CInt, a1MutHeight :: CInt,
	a1MutStride :: CInt, a1MutData :: ForeignPtr PixelA1 }
	deriving Show

pattern CairoImageMutA1 :: A1Mut s -> CairoImageMut s
pattern CairoImageMutA1 a <- (cairoImageMutToA1 -> Just a)
	where CairoImageMutA1 (A1Mut w h s d) =
		CairoImageMut CairoFormatA1 w h s $ castForeignPtr d

cairoImageMutToA1 :: CairoImageMut s -> Maybe (A1Mut s)
cairoImageMutToA1 = \case
	CairoImageMut CairoFormatA1 w h s d ->
		Just . A1Mut w h s $ castForeignPtr d
	_ -> Nothing

instance ImageMut A1Mut where
	type PixelMut A1Mut = PixelA1
	imageMutSize (A1Mut w h _ _) = (w, h)
	getPixel (A1Mut w h s d) x y = unsafeIOToPrim $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peA1) $ ptrA1 w h s p x y
	putPixel (A1Mut w h s d) x y px = unsafeIOToPrim $ with d \p ->
		maybe (pure ()) (`poA1` px) $ ptrA1 w h s p x y
	newImageMut w h =
		stride CairoFormatA1 w >>= \s -> A1Mut w h s <$> new s h

---------------------------------------------------------------------------
-- RGB 16 565
---------------------------------------------------------------------------

-- PIXEL

newtype PixelRgb16_565 = PixelRgb16_565Word16 Word16 deriving (Show, Storable)

{-# COMPLETE PixelRgb16_565 #-}

pattern PixelRgb16_565 :: Word8 -> Word8 -> Word8 -> PixelRgb16_565
pattern PixelRgb16_565 r g b <- (pixelRgb16_565ToRgb -> (r, g, b))
	where PixelRgb16_565
		(fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) =
		PixelRgb16_565Word16 $ r' .|. g' .|. b'
		where
		r' = r `shiftR` 3 `shiftL` 11
		g' = g `shiftR` 2 `shiftL` 5
		b' = b `shiftR` 3

pixelRgb16_565ToRgb :: PixelRgb16_565 -> (Word8, Word8, Word8)
pixelRgb16_565ToRgb (PixelRgb16_565Word16 rgb) =
	(r .|. r `shiftR` 5, g .|. g `shiftR` 6, b .|. b `shiftR` 5)
	where
	r = fromIntegral $ rgb `shiftR` 11 `shiftL` 3
	g = fromIntegral $ rgb `shiftR` 5 `shiftL` 2
	b = fromIntegral $ rgb `shiftL` 3

-- IMAGE

data Rgb16_565 = Rgb16_565 {
	rgb16_565Width :: CInt, rgb16_565Height :: CInt,
	rgb16_565Stride :: CInt, rgb16_565Data :: ForeignPtr PixelRgb16_565 }
	deriving Show

pattern CairoImageRgb16_565 :: Rgb16_565 -> CairoImage
pattern CairoImageRgb16_565 r <- (cairoImageToRgb16_565 -> Just r)
	where CairoImageRgb16_565 (Rgb16_565 w h s d) =
		CairoImage CairoFormatRgb16_565 w h s $ castForeignPtr d

cairoImageToRgb16_565 :: CairoImage -> Maybe Rgb16_565
cairoImageToRgb16_565 = \case
	CairoImage CairoFormatRgb16_565 w h s d ->
		Just . Rgb16_565 w h s $ castForeignPtr d
	_ -> Nothing

instance Image Rgb16_565 where
	type Pixel Rgb16_565 = PixelRgb16_565
	imageSize (Rgb16_565 w h _ _) = (w, h)
	pixelAt (Rgb16_565 w h s d) x y = unsafePerformIO $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	generateImagePrimM w h f = stride CairoFormatRgb16_565 w >>= \s ->
		Rgb16_565 w h s <$> gen w h s f

-- IMAGE MUTABLE

data Rgb16_565Mut s = Rgb16_565Mut {
	rgb16_565MutWidth :: CInt, rgb16_565MutHeight :: CInt,
	rgb16_565MutStride :: CInt,
	rgb16_565MutData :: ForeignPtr PixelRgb16_565 }
	deriving Show

pattern CairoImageMutRgb16_565 :: Rgb16_565Mut s -> CairoImageMut s
pattern CairoImageMutRgb16_565 r <- (cairoImageMutToRgb16_565 -> Just r)
	where CairoImageMutRgb16_565 (Rgb16_565Mut w h s d) =
		CairoImageMut CairoFormatRgb16_565 w h s $ castForeignPtr d

cairoImageMutToRgb16_565 :: CairoImageMut s -> Maybe (Rgb16_565Mut s)
cairoImageMutToRgb16_565 = \case
	CairoImageMut CairoFormatRgb16_565 w h s d ->
		Just . Rgb16_565Mut w h s $ castForeignPtr d
	_ -> Nothing

instance ImageMut Rgb16_565Mut where
	type PixelMut Rgb16_565Mut = PixelRgb16_565
	imageMutSize (Rgb16_565Mut w h _ _) = (w, h)
	getPixel (Rgb16_565Mut w h s d) x y = unsafeIOToPrim $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	putPixel (Rgb16_565Mut w h s d) x y px = unsafeIOToPrim $ with d \p ->
		maybe (pure ()) (`poke` px) $ ptr w h s p x y
	newImageMut w h = stride CairoFormatRgb16_565 w >>= \s ->
		Rgb16_565Mut w h s <$> new s h

---------------------------------------------------------------------------
-- RGB 30
---------------------------------------------------------------------------

-- PIXEL

newtype PixelRgb30 = PixelRgb30Word32 Word32 deriving (Show, Storable)

{-# COMPLETE PixelRgb30 #-}

pattern PixelRgb30 :: Word16 -> Word16 -> Word16 -> PixelRgb30
pattern PixelRgb30 r g b <- (pixelRgb30ToRgb -> (r, g, b)) where
	PixelRgb30 (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) =
		PixelRgb30Word32 $ r' .|. g' .|. b'
		where
		r' = r `shiftR` 6 `shiftL` 20
		g' = g `shiftR` 6 `shiftL` 10
		b' = b `shiftR` 6

pixelRgb30ToRgb :: PixelRgb30 -> (Word16, Word16, Word16)
pixelRgb30ToRgb (PixelRgb30Word32 rgb) =
	(r .|. r `shiftR` 10, g .|. g `shiftR` 10, b .|. b `shiftR` 10)
	where
	r = fromIntegral $ rgb `shiftR` 20 `shiftL` 6
	g = fromIntegral $ rgb `shiftR` 10 `shiftL` 6
	b = fromIntegral $ rgb `shiftL` 6

-- IMAGE

data Rgb30 = Rgb30 {
	rgb30Width :: CInt, rgb30Height :: CInt,
	rgb30Stride :: CInt, rgb30Data :: ForeignPtr PixelRgb30 }
	deriving Show

pattern CairoImageRgb30 :: Rgb30 -> CairoImage
pattern CairoImageRgb30 r <- (cairoImageToRgb30 -> Just r)
	where CairoImageRgb30 (Rgb30 w h s d) =
		CairoImage CairoFormatRgb30 w h s $ castForeignPtr d

cairoImageToRgb30 :: CairoImage -> Maybe Rgb30
cairoImageToRgb30 = \case
	CairoImage CairoFormatRgb30 w h s d ->
		Just . Rgb30 w h s $ castForeignPtr d
	_ -> Nothing

instance Image Rgb30 where
	type Pixel Rgb30 = PixelRgb30
	imageSize (Rgb30 w h _ _) = (w, h)
	pixelAt (Rgb30 w h s d) x y = unsafePerformIO $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	generateImagePrimM w h f =
		stride CairoFormatRgb30 w >>= \s -> Rgb30 w h s <$> gen w h s f

-- IMAGE MUTABLE

data Rgb30Mut s = Rgb30Mut {
	rgb30MutWidth :: CInt, rgb30MutHeight :: CInt,
	rgb30MutStride :: CInt, rgb30MutData :: ForeignPtr PixelRgb30 }
	deriving Show

pattern CairoImageMutRgb30 :: Rgb30Mut s -> CairoImageMut s
pattern CairoImageMutRgb30 r <- (cairoImageMutToRgb30 -> Just r)
	where CairoImageMutRgb30 (Rgb30Mut w h s d) =
		CairoImageMut CairoFormatRgb30 w h s $ castForeignPtr d

cairoImageMutToRgb30 :: CairoImageMut s -> Maybe (Rgb30Mut s)
cairoImageMutToRgb30 = \case
	CairoImageMut CairoFormatRgb30 w h s d ->
		Just . Rgb30Mut w h s $ castForeignPtr d
	_ -> Nothing

instance ImageMut Rgb30Mut where
	type PixelMut Rgb30Mut = PixelRgb30
	imageMutSize (Rgb30Mut w h _ _) = (w, h)
	getPixel (Rgb30Mut w h s d) x y = unsafeIOToPrim $ with d \p ->
		maybe (pure Nothing) ((Just <$>) . peek) $ ptr w h s p x y
	putPixel (Rgb30Mut w h s d) x y px = unsafeIOToPrim $ with d \p ->
		maybe (pure ()) (`poke` px) $ ptr w h s p x y
	newImageMut w h =
		stride CairoFormatRgb30 w >>= \s -> Rgb30Mut w h s <$> new s h
