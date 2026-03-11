module YamlUnscrambler.AsciiAtto where

import Data.Attoparsec.ByteString.Char8
import YamlUnscrambler.Model
import YamlUnscrambler.Prelude

integralScalar :: (Integral a, Bits a) => Signed -> NumeralSystem -> Parser a
integralScalar (Signed isSigned) numeralSystem =
  if isSigned
    then signed numeralParser
    else numeralParser
  where
    numeralParser =
      case numeralSystem of
        DecimalNumeralSystem ->
          decimal
        HexadecimalNumeralSystem ->
          hexadecimal
