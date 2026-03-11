-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Text.PercentFormat.Spec

main :: IO ()
main = reportTests tests

tests :: [Bool]
tests =
  [ True
  , parseSpec "%"    ==- spec {ty = Percent}
  , parseSpec "r"    ==- spec {ty = ReprSpec}
  , parseSpec "s"    ==- spec {ty = StringSpec}
  , parseSpec "10s"  ==- spec {ty = StringSpec,  width = 10}
  , parseSpec "21s"  ==- spec {ty = StringSpec,  width = 21}
  , parseSpec "d"    ==- spec {ty = NumberSpec}
  , parseSpec "4d"   ==- spec {ty = NumberSpec, width =  4,  base = 10}
  , parseSpec "6d"   ==- spec {ty = NumberSpec, width =  6,  base = 10}
  , parseSpec "07d"  ==- spec {ty = NumberSpec, width =  7,  padWith = '0'}
  , parseSpec "073d" ==- spec {ty = NumberSpec, width =  73, padWith = '0'}
  , parseSpec "6i"   ==- spec {ty = NumberSpec, width =  6,  base = 10, precision = Just 0}
  , parseSpec "07i"  ==- spec {ty = NumberSpec, width =  7,  padWith = '0', precision = Just 0}
  , parseSpec "07.*i" ==- spec {ty = NumberSpec, width =  7,  padWith = '0'}
  , parseSpec "073i" ==- spec {ty = NumberSpec, width =  73, padWith = '0', precision = Just 0}
  , parseSpec "x"    ==- spec {ty = NumberSpec, base = 16}
  , parseSpec "X"    ==- spec {ty = NumberSpec, base = 16, capitalizeDigits = True}
  , parseSpec "o"    ==- spec {ty = NumberSpec, base =  8}
  , parseSpec "b"    ==- spec {ty = NumberSpec, base =  2}
  , parseSpec "d"    ==- spec {ty = NumberSpec, positivePrefix=""}
  , parseSpec "+d"   ==- spec {ty = NumberSpec, positivePrefix="+"}
  , parseSpec " d"   ==- spec {ty = NumberSpec, positivePrefix=" "}
  , parseSpec "i"    ==- spec {ty = NumberSpec, positivePrefix="", precision = Just 0}
  , parseSpec "+i"   ==- spec {ty = NumberSpec, positivePrefix="+", precision = Just 0}
  , parseSpec " i"   ==- spec {ty = NumberSpec, positivePrefix=" ", precision = Just 0}
  , parseSpec ".*i"  ==- spec {ty = NumberSpec, positivePrefix=""}
  , parseSpec "f"    ==- spec {ty = NumberSpec, minPrecision = 1}
  , parseSpec "3f"   ==- spec {ty = NumberSpec, minPrecision = 1, width = 3}
  , parseSpec "4.1f" ==- spec {ty = NumberSpec, minPrecision = 1, width = 4, precision = Just 1}
  , parseSpec "4.0f" ==- spec {ty = NumberSpec, minPrecision = 1, width = 4, precision = Just 0}
  , parseSpec "4.f"  ==- spec {ty = NumberSpec, minPrecision = 1, width = 4, precision = Just 0}
  , parseSpec "f"    ==- spec {ty = NumberSpec, minPrecision = 1, positivePrefix=""}
  , parseSpec "+f"   ==- spec {ty = NumberSpec, minPrecision = 1, positivePrefix="+"}
  , parseSpec " f"   ==- spec {ty = NumberSpec, minPrecision = 1, positivePrefix=" "}
    ]
  where
  x ==- y = x == (y,"")
