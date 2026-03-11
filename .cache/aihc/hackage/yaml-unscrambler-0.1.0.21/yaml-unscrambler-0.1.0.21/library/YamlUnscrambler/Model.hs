module YamlUnscrambler.Model where

import YamlUnscrambler.Prelude

-- |
-- Specification of the maximum allowed length for the input.
-- A safety measure to ensure that the parser doesn't exhaust memory
-- when parsing to unlimited datatypes.
newtype MaxInputSize
  = MaxInputSize Int

newtype Signed
  = Signed Bool

data NumeralSystem
  = DecimalNumeralSystem
  | HexadecimalNumeralSystem

newtype CaseSensitive
  = CaseSensitive Bool
