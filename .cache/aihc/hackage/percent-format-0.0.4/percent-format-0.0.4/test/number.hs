-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Text.PercentFormat
import Text.PercentFormat.Quotient (Quotient)
import Data.List (isInfixOf)

import qualified Data.Ratio as R

main :: IO ()
main = reportTests tests

tests :: [Bool]
tests =
  [ True

  -- single digits, positives
  , "%i" % 1 == "1"
  , "%d" % 2 == "2"
  , "%x" % 3 == "3"
  , "%o" % 4 == "4"
  , "%b" % 5 == "101"
  , "%f" % 6 == "6.0"

  -- double digits, positives
  , "%i" % 12 == "12"
  , "%d" % 23 == "23"
  , "%x" % 34 == "22"
  , "%o" % 45 == "55"
  , "%b" % 56 == "111000"
  , "%f" % 67 == "67.0"

  -- single digits, negatives
  , "%i" % (-1) == "-1"
  , "%d" % (-2) == "-2"
  , "%x" % (-3) == "-3"
  , "%o" % (-4) == "-4"
  , "%b" % (-5) == "-101"
  , "%f" % (-6) == "-6.0"

  -- double digits, negatives
  , "%i" % (-12) == "-12"
  , "%d" % (-23) == "-23"
  , "%x" % (-34) == "-22"
  , "%o" % (-45) == "-55"
  , "%b" % (-56) == "-111000"
  , "%f" % (-67) == "-67.0"

  -- Infinity
  , "%f" % (1.0 / 0.0) == "Infinity"
  , "%i" % (1.0 / 0.0) == "Infinity"
  , "%d" % (1.0 / 0.0) == "Infinity"
  , "%x" % (1.0 / 0.0) == "Infinity"
  , "%o" % (1.0 / 0.0) == "Infinity"
  , and $ [ "%%%i%c" -%% (n,c) -% (1.0 / 0.0) == "Infinity"
          | n <- [-7..7], c <- "idxobf" ]

  -- -Infinity
  , "%f" % (-1.0 / 0.0) == "-Infinity"
  , "%i" % (-1.0 / 0.0) == "-Infinity"
  , "%d" % (-1.0 / 0.0) == "-Infinity"
  , "%x" % (-1.0 / 0.0) == "-Infinity"
  , "%o" % (-1.0 / 0.0) == "-Infinity"

  -- NaN
  , "%f" % (0.0 / 0.0) == "NaN"
  , "%i" % (0.0 / 0.0) == "NaN"
  , "%d" % (0.0 / 0.0) == "NaN"
  , "%x" % (0.0 / 0.0) == "NaN"
  , "%o" % (0.0 / 0.0) == "NaN"
  , and $ [ "NaN" `isInfixOf` ("%%%i%c" -%% (n,c) -% (0.0 / 0.0))
          | n <- [-7..7], c <- "idxobf" ]

  -- decimal fractional digits
  , "%f" % 1 == "1.0"
  , "%f" % 1.1 == "1.1"
  , "%f" % (10.0 / 3.0) == show (10.0 / 3.0)
  , "%.3f" % (10.0 / 3.0) == "3.333"
  , "%1.2f" % 1.1 == "1.10"
  , "%06.3f" % 1.23 == "01.230"
  , "%.1f" % 1.14 == "1.1"
  , "%.3f" % (-1.2) == "-1.200"
  , "%.1f" % 1.16 == "1.2"
  , "%.1f" % 1.15 == "1.2"  -- round to the nearest even number
  , "%.1f" % 1.25 == "1.2"  -- round to the nearest even number
  , "%15.7f" % 1234567.1234567 == "1234567.1234567"
  , "%17.8f" % (12345678.12345678 :: Quotient) == "12345678.12345678"
--, "%17.8f" % 12345678.12345678 == "12345678.12345678"  -- GHC
--, "%17.8f" % 12345678.12345678 == "12345678.12345680"  -- Hugs

  -- fractional digits in other bases
  , "%.2x" % (1.0 / 200.0) == "0.01"
  , "%.2x" % (3.0 / 200.0) == "0.04"
  , "%.8b" % (1.0 / 200.0) == "0.00000001"
  , "%.8b" % (3.0 / 200.0) == "0.00000100"
  , "%.1x" % (3.0 / 2.0) == "1.8"
  , "%.1b" % (3.0 / 2.0) == "1.1"
  , "%.0b" % (3.0 / 2.0) == "10"
  , "%.0b" % (1.0 / 2.0) == "0"

  -- %i rounds to the nearest integer, unless precision is specified
  , "%i"   % 3.1 == "3"
  , "%.*i" % 3.1 == "3.1"
  , "%.2i" % 3.1 == "3.10"
  , "%.*i" % 3.141 == "3.141"
  , "%.2i" % 3.141 == "3.14"

  -- %f shows a minimum of one fractional digit
  , "%f" % 6 == "6.0"
  , "%f" % 12 == "12.0"

  -- positive/negative signal placement
  , "%+i" % 1 == "+1"
  , "% i" % 1 == " 1"
  , "%+f" % 1.2 == "+1.2"
  , "% f" % 1.2 == " 1.2"
  , "%3d" % (-1) == " -1"
  , "%4d" % (-1) == "  -1"
  , "%03d" % (-1) == "-01"
  , "%04d" % (-1) == "-001"
  , "%+3d" % 1 == " +1"
  , "%+4d" % 1 == "  +1"
  , "%+03d" % 1 == "+01"
  , "%+04d" % 1 == "+001"
  , "% 07.3f" %   3.3  == " 03.300"
  , "% 07.3f" % (-3.3) == "-03.300"
  , "% 7.3f"  %   3.3  == "  3.300"
  , "% 7.3f"  % (-3.3) == " -3.300"

  , "%07.3d" -% 10.5 == "010.500"
  , "%07.3x" -% 10.5 == "00a.800"
  , "%07.3b" -% 10.5 == "1010.100"

  -- Ratios
  , "asdf %r qwer" -% (10 R.% 3) == "asdf 10 % 3 qwer"
  , "asdf %07.3f qwer" -% (10 R.% 3) == "asdf 003.333 qwer"

  , "%x" -% 0xfedcba9876543210 == "fedcba9876543210"
  , "%X" -% 0xFEDCBA9876543210 == "FEDCBA9876543210"

  , "%i"    % pi == "3"
  , "%.8d"  % pi == "3.14159265"
--, "%d"    % pi == "3.14159265358979"   -- Hugs
--, "%d"    % pi == "3.141592653589793"  -- GHC
  , "%.8x" % pi == "3.243f6a89"
  , "%.12b" % pi == "11.001001000100"


-- TODO: in the future, also handle Ratios nicely:
-- NOTE: use 360 or 5040 as a default divisor (max precision)
-- '/' can be just an alias for '.'
--, "%q" % (2/3 :: Rational) == "2/3"
--, "%q" % (2/3 :: Rational) == "2/3"
--, "%Q" % (2/3 :: Rational) == "2 % 3"
--, "%q" % 0.333 == "1/3"
--, "%q" % 0 == "0/1"
--, "%q" % 1 == "1/1"
--, "%q" % 2 == "2/1"
--, "%/12q" % 0.333 == "1/3"
--, "%/3q" % (3/12) == "1/3"
--, "%4/3q" % (3/12) == " 1/3"
-- why not use /N for exact precision?
-- one can just format manually using %i/%i
-- or even use %/*q

  -- error handling
  , "%i"   % "a" == "!"
  , "%d"   % "a" == "!"
  , "%x"   % "a" == "!"
  , "%o"   % "a" == "!"
  , "%b"   % "a" == "!"
  , "%f"   % "a" == "!"
  , "%1i"  % "b" == "!"
  , "%2d"  % "c" == "!!"
  , "%3x"  % " " == "!!!"
  , "%4o"  % "!" == "!!!!"
  , "%5b"  % "." == "!!!!!"
  , "%6f"  % "-" == "!!!!!!"
  , "%01f" % "A" == "!"
  , "%02b" % "B" == "!!"
  , "%03o" % "C" == "!!!"
  , "%04x" % "%" == "!!!!"
  , "%05d" % "/" == "!!!!!"
  , "%06i" % "^" == "!!!!!!"
  ]
