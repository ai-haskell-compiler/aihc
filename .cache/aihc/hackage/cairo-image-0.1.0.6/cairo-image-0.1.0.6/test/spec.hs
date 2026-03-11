{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word
import Data.CairoImage.Internal

main :: IO ()
main = main1 >> main2

main1, main2 :: IO ()
main1 = do
	let	rgb16max = PixelRgb16_565 0xff 0xff 0xff
		PixelRgb16_565 ra ga ba = rgb16max
		rgb16min = PixelRgb16_565 0x00 0x00 0x00
		PixelRgb16_565 ri gi bi = rgb16min
		rgb16foo = PixelRgb16_565 0x12 0x34 0x56
		PixelRgb16_565 rf gf bf = rgb16foo
	print rgb16max
	print (ra, ga, ba)
	print rgb16min
	print (ri, gi, bi)
	print rgb16foo
	print (rf, gf, bf)
	print (0x12 :: Word8, 0x34 :: Word8, 0x56 :: Word8)

main2 = do
	let	rgb30max = PixelRgb30 0xffff 0xffff 0xffff
		PixelRgb30 ra ga ba = rgb30max
		rgb30min = PixelRgb30 0x0000 0x0000 0x0000
		PixelRgb30 ri gi bi = rgb30min
		rgb30foo = PixelRgb30 0x1234 0x5678 0x9abc
		PixelRgb30 rf gf bf = rgb30foo
	print rgb30max
	print (ra, ga, ba)
	print rgb30min
	print (ri, gi, bi)
	print rgb30foo
	print (rf, gf, bf)
