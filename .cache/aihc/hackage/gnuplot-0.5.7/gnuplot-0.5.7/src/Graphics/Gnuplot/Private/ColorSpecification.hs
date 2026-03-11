module Graphics.Gnuplot.Private.ColorSpecification where

import Graphics.Gnuplot.Utility (quote, )
import Data.List.HT (padLeft, )
import Data.Word (Word8, )
import Numeric (showHex, )


data T =
     Name String
   | RGB8 {red, green, blue :: Word8}
   | PaletteFrac Double


toString :: T -> String
toString c =
   case c of
      Name name -> "rgbcolor " ++ quote name
      RGB8 r g b ->
         "rgbcolor #" ++
         concatMap (padLeft '0' 2 . flip showHex "") [r,g,b]
      PaletteFrac frac -> "palette frac " ++ show frac
