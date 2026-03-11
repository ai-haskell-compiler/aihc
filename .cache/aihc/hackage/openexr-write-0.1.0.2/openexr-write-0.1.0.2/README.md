Haskell OpenEXR Library
=======================

This library can write colored images which use floating point values into
OpenEXR (v2) high-dynamic-range file format.

Supported features:

- scanline format
- compression (ZIPS, ZIP)
- pixel format (RGB float32)



Usage
-----

An image can be written by calling `writeFile` function. For example,
following code stores a compressed image consisting of one red pixel.

```
module Main where

import qualified Data.Vector      as V
import qualified Graphics.OpenEXR as EXR


main :: IO ()
main = do
        let image = EXR.ImageRGBF 1 1 (V.fromList [EXR.PixelRGBF 1.0 0.0 0.0])
        EXR.writeFile "image.exr" image EXR.ZipCompression
```
