-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Text.PercentFormat

main :: IO ()
main = reportTests tests

tests :: [Bool]
tests =
  [ True

  , "asdf qwer"      % "asdf" ==  "asdf qwer"
  , "zxcv qwer"      % "asdf" ==  "zxcv qwer"
  , "asdf %s qwer"   % "asdf" ==  "asdf asdf qwer"
  , "asdf %5s qwer"  % "asdf" ==  "asdf  asdf qwer"
  , "asdf %5-s qwer" % "asdf" ==  "asdf asdf  qwer"
  , "%s" % "1%" == "1%%"
  , "%s" -% "1%" == "1%"

  , "asdf %d qwer"   % 10     ==  "asdf 10 qwer"
  , "asdf %2d qwer"  %  9     ==  "asdf  9 qwer"
  , "asdf %02d qwer" %  9     ==  "asdf 09 qwer"
  , "asdf %012d qwer" % 123   ==  "asdf 000000000123 qwer"

  , "asdf %x qwer" % 10       ==  "asdf a qwer"
  , "asdf %04x qwer" % 10     ==  "asdf 000a qwer"
  , "asdf %o qwer"   % 10     ==  "asdf 12 qwer"
  , "asdf %03o qwer" % 10     ==  "asdf 012 qwer"
  , "%b" % 10 == "1010"
  , "%08b" % 10 == "00001010"
  , "asdf %X qwer" % 10       ==  "asdf A qwer"

  , "%c" % 'c' == "c"
  , "%r" % 'c' == "'c'"

  , "%s" % 10 == "10"

  , "%c"   % "a" == "!"
  , "%c"   % 3   == "!"
  , "%c"   % 3.1 == "!"

  -- not really error:
  , "%s"   % 0    == "0"
  , "%s"   % 3.14 == "3.14"
  , "%2s"  % 0    == " 0"
  , "%2s"  % 3.14 == "3.14"
  ]
